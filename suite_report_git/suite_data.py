#!/usr/bin/env python3
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
Class containing helper methods for gathering data needed for a SuiteReport object
"""

import sys

sys.path.append("../")
import subprocess
import sqlite3
import shutil
import yaml
import re

try:
    from bdiff.git_bdiff import GitBDiff, GitInfo
except ImportError:
    try:
        from git_bdiff import GitBDiff, GitInfo
    except ImportError:
        raise ImportError(
            "Unable to import from git_bdiff module. This is included in the same "
            "repository as this script and included with a relative import. Ensure "
            "this script is being called from the correct place."
        )
from typing import Union, Optional, List, Dict
from pathlib import Path
from collections import defaultdict


class SuiteData:
    """
    Class to gather info on a suite
    """

    pink_failures = (
        "_vs_",
        "lrun_crun_atmos",
        "proc",
        "atmos_omp",
        "atmos_nruncrun",
        "atmos_thread",
        "-v-",
    )

    def parse_tasks(self) -> Dict[str, List[str]]:
        """
        Read through the tasks run, sorting by state
        """

        data = defaultdict(list)

        for task, state in self.task_states.items():
            if state == "failed" and (
                task.startswith("rose_ana") or task.startswith("check_")
            ):
                for item in self.pink_failures:
                    if item in task:
                        state = "pink failure"
                        break
            data[state].append(task)
        return data

    def populate_gitbdiff(self):
        """
        Run GitBDiff on each copied source if the source isn't main-like, storing in the
        dependencies directory
        """

        for dependency, data in self.dependencies.items():
            if not data["gitinfo"].is_main():
                if dependency.lower() == "simsys_scripts":
                    parent = "main"
                else:
                    parent = "trunk"
                self.dependencies[dependency]["gitbdiff"] = GitBDiff(
                    repo=self.temp_directory / dependency, parent=parent
                ).files()

    def populate_gitinfo(self):
        """
        Run GitInfo on each copied source, storing in the dependencies directory
        """

        for dependency in self.dependencies:
            self.dependencies[dependency]["gitinfo"] = GitInfo(
                self.temp_directory / dependency
            )

    def clone_sources(self):
        """
        Clone the sources defined in the dependencies file, to allow reading of files
        and creation of diffs.
        If the source is not a github url, then copy it using rsync
        """

        for dependency, data in self.dependencies.items():
            loc = self.temp_directory / dependency
            if data["source"].endswith(".git"):
                commands = [
                    f"git clone {data['source']} {loc}",
                    f"git -C {loc} checkout {data['ref']}",
                ]
                for command in commands:
                    self.run_command(command)
            else:
                source = data["source"]
                if not source.endswith("/"):
                    source = source + "/"
                command = (
                    f'rsync -e "ssh -o StrictHostKeyChecking=no" -avl {source} {loc}'
                )
                self.run_command(command, shell=True)

    def determine_primary_source(self) -> str:
        """
        Work out what repo launched the suite based on the what sources are available in
        the dependencies. Return 'unknown' if unable to determine.
        """

        # If just 1 dependency, it must be that
        if len(self.dependencies) == 1:
            return list(self.dependencies.keys())[0]

        # If 2 dependencies, remove simsys_scripts
        if len(self.dependencies) == 2:
            for item in self.dependencies:
                if item.lower() != "simsys_scripts":
                    return item

        # If LFRic Apps in sources, that is the primary source
        if "lfric_apps" in self.dependencies.keys():
            return "lfric_apps"

        if "um" in self.dependencies.keys():
            return "um"

        return "unknown"

    def read_rose_conf(self) -> Dict[str, str]:
        """
        Read the suite rose-suite.conf file into a dictionary
        """

        path = self.suite_path / "log" / "config"
        for item in path.rglob("*-rose-suite.conf"):
            conf_file = item
            break

        rose_conf_text = conf_file.read_text().split("\n")
        rose_conf = {}
        for line in rose_conf_text:
            line = line.strip()
            if (
                not line
                or line.startswith("!")
                or line.startswith("[")
                or line.startswith("#")
            ):
                continue
            line = line.split("=")
            key = line[0].strip()
            value = line[1].strip()
            if (
                value.startswith("'")
                and value.endswith("'")
                or value.startswith('"')
                and value.endswith('"')
            ):
                value = value[1:-1]
            rose_conf[key] = value
        return rose_conf

    def find_unknown_dependency(self, dependency: str) -> str:
        """
        TEMPORARY
        The primary dependency may be unset in the dependencies file. In this case find
        it from the *_SOURCE variable in the rose-suite.conf.
        TODO: Once cylc provides the location of the source code itself, this method
        should be changed to use that instead, as then the _SOURCE variable will be
        removed
        """

        var = f"{dependency.upper()}_SOURCE".replace('"', "")
        if var not in self.rose_data:
            raise RuntimeError(f"Cant determine source for {dependency}")
        rval = self.rose_data[var]
        if "$ROSE_ORIG_HOST" in rval:
            rval = rval.replace("$ROSE_ORIG_HOST", self.rose_data["ROSE_ORIG_HOST"])
        return rval

    def read_dependencies(self) -> Dict[str, Dict]:
        """
        Read the suite dependencies from the dependencies.yaml file - this is assumed to
        have been copied to the suite_path directory
        """

        with open(self.suite_path / "dependencies.yaml", "r") as stream:
            dependencies = yaml.safe_load(stream)
        for dependency, data in dependencies.items():
            if data["source"] is None:
                dependencies[dependecy]["source"] = self.find_unknown_dependency(
                    dependecy
                )
        return dependencies

    def get_workflow_id(self) -> str:
        """
        Read cylc install log for workflow id
        """

        with open(self.suite_path / "log" / "scheduler" / "log", "r") as f:
            for line in f:
                match = re.search(r"INFO - Workflow: (\w+\/\w+)", line)
                try:
                    workflow_id = match.group(1)
                    return workflow_id
                except IndexError:
                    continue

    def get_suite_starttime(self) -> str:
        """
        Read the suite starttime from the suite database
        """

        starttime = "unknown"
        for row in self.query_suite_database(
            self.suite_path / "log" / "db", ["start_time"], "workflow_flows"
        ):
            starttime = row[0]
        return starttime.split("+")[0]

    def read_groups_run(self) -> str:
        """
        Read in groups run as part of suite from the cylc database file
        """

        groups = None
        for row in self.query_suite_database(
            self.suite_path / "log" / "db", ["key", "value"], "workflow_template_vars"
        ):
            if row[0] in ("g", "groups"):
                groups = row[1].strip("[]'\"").split(",")
                break
        return groups

    def get_task_states(self) -> Dict[str, str]:
        """
        Query the database and return a dictionary of states. This is assumed to be in
        suite_path/log/db
        """

        data = {}
        for row in self.query_suite_database(
            self.suite_path / "log" / "db", ["name", "status"], "task_states"
        ):
            data[row[0]] = row[1]
        return data

    def query_suite_database(
        self, database: Path, selections: List[str], source: str
    ) -> List[tuple]:
        """
        Create an sql statement annd query provided database. Return the result
        """

        sql_statement = f"select {', '.join(s for s in selections)} from {source};"

        database = sqlite3.connect(database)
        cursor = database.cursor()
        cursor.execute(sql_statement)
        data = []
        for row in cursor:
            data.append(row)
        database.close()
        return data

    def run_command(
        self, command: Union[str, List[str]], shell: bool = False, rval: bool = False
    ) -> Optional[subprocess.CompletedProcess]:
        """
        Run a subprocess command and return the result object
        Inputs:
            - command, str with command to run
        Outputs:
            - result object from subprocess.run
        """
        if not shell and type(command) != list:
            command = command.split()
        result = subprocess.run(
            command,
            capture_output=True,
            text=True,
            timeout=300,
            shell=shell,
            check=False,
        )
        if result.returncode:
            print(result.stdout, end="\n\n\n")
            raise RuntimeError(
                f"[FAIL] Issue found running command {command}\n\n{result.stderr}"
            )
        if rval:
            return result

    def cleanup(self):
        """
        Remove self.temp_directory
        """
        shutil.rmtree(self.temp_directory)
