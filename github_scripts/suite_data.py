#!/usr/bin/env python3
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
Class containing helper methods for gathering data needed for a SuiteReport object
"""

import re
import shutil
import sqlite3
import subprocess
import yaml
from collections import defaultdict
from pathlib import Path
from typing import Dict, List, Optional, Set, Union
from git_bdiff import GitBDiff, GitInfo
from get_git_sources import clone_repo, sync_repo


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

    def __init__(self) -> None:
        self.dependencies = {}
        self.rose_data = {}
        self.suite_path = None
        self.task_states = {}
        self.temp_directory = None

    def get_um_failed_configs(self) -> Set[str]:
        """
        Read through failed UM rose_ana tasks
        """

        failed_configs = set()
        for task, state in self.task_states.items():
            if task.startswith("rose_ana") and state == "failed":
                try:
                    config = task.split("-")[2]
                except IndexError:
                    if "mule" in task:
                        config = "mule"
                    else:
                        config = ""
                failed_configs.add(config)
        return failed_configs

    def read_um_section(self, change: str) -> str:
        """
        Read through a UM file searching for line
        """
        change = self.temp_directory / "um" / change
        lines = change.read_text()
        lines = lines.lower()
        try:
            section = re.search(r"this file belongs in section:\s*(\w+)", lines).group(
                1
            )
        except AttributeError:
            section = "Unknown"
        return section

    def get_changed_um_section(self) -> Set[str]:
        """
        Read through bdiff of UM source and find code owner section for each changed
        file
        """

        changed_sections = set()
        for change_path in self.dependencies["um"]["gitbdiff"]:
            change = change_path.lower()
            # First check files which will not have a Code Section Comment
            if "dependencies.yaml" in change:
                continue
            if "configowners.txt" in change or "codeowners.txt" in change:
                changed_sections.add(change)
            elif change.startswith("admin"):
                changed_sections.add("admin")
            elif change.startswith("bin"):
                changed_sections.add("bin")
            elif change.startswith("fcm-make"):
                changed_sections.add("fcm-make_um")
            elif change.startswith("fab"):
                changed_sections.add("fab")
            elif change.startswith("rose-stem"):
                changed_sections.add("rose_stem")
            elif change.startswith("rose-meta"):
                if "etc/stash" in change:
                    changed_sections.add("stash")
                elif "rose-meta.conf" in change:
                    changed_sections.add("rose-meta.conf")
                else:
                    changed_sections.add("upgrade_macros")
            else:
                # Read the section from the code comment
                changed_sections.add(self.read_um_section(change_path))

        return changed_sections

    def get_um_owners(self, filename: str) -> Dict:
        """
        Read UM Code Owners file and write to a dictionary
        """

        fpath = self.temp_directory / "um" / filename
        owners_text = fpath.read_text()

        in_owners = False
        owners = {}
        for line in owners_text.split("\n"):
            line = line.lower().strip()
            if (
                not line
                or line.startswith("#")
                or line.startswith("area")
                or line.startswith("configuration")
            ):
                continue
            if line.startswith("{{{"):
                in_owners = True
            elif line.startswith("}}}"):
                in_owners = False
            elif in_owners:
                line = line.split()
                while len(line) < 3:
                    line.append("--")
                owners[line[0]] = (line[1], line[2])

        # Hard Code some SSD sections
        if "Code" in filename:
            owners["admin"] = ("SSD Team", "--")
            owners["bin"] = ("SSD Team", "--")
            owners["codeowners.txt"] = ("SSD Team", "--")
            owners["configowners.txt"] = ("SSD Team", "--")

        return owners

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

    def populate_gitbdiff(self) -> None:
        """
        Run GitBDiff on each copied source if the source isn't main-like, storing in the
        dependencies directory
        """

        for dependency, data in self.dependencies.items():
            if not data["gitinfo"].is_main():
                parent = "main"
                self.dependencies[dependency]["gitbdiff"] = GitBDiff(
                    repo=self.temp_directory / dependency, parent=parent
                ).files()

    def populate_gitinfo(self) -> None:
        """
        Run GitInfo on each copied source, storing in the dependencies directory
        """

        for dependency in self.dependencies:
            self.dependencies[dependency]["gitinfo"] = GitInfo(
                self.temp_directory / dependency
            )

    def clone_sources(self) -> None:
        """
        Clone the sources defined in the dependencies file, to allow reading of files
        and creation of diffs.
        If the source is not a github url, then copy it using rsync
        """

        for dependency, data in self.dependencies.items():
            loc = self.temp_directory / dependency
            if data["source"].endswith(".git"):
                clone_repo(data["source"], data["ref"], loc)
            else:
                sync_repo(data["source"], data["ref"], loc)

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
        else:
            raise FileNotFoundError(
                "Couldn't find a *-rose-suite.conf file in the cylc-run log directory"
            )

        rose_conf_text = conf_file.read_text().split("\n")
        rose_conf = {}
        for line in rose_conf_text:
            line = line.strip()
            if (
                not line
                or "=" not in line
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
        The primary dependency may be unset in the dependencies file. In this case find
        it from the CYLC_WORKFLOW_SRC_DIR variable that gets set in the
        flow-processed.cylc file
        """

        pattern = re.compile(rf"{dependency.upper()} SOURCE CLONE=(\S+)")
        log_file = self.suite_path / "log" / "scheduler" / "log"
        with open(log_file, "r") as f:
            for line in f:
                if match := pattern.search(line):
                    return match.group(1).rstrip("/")
        raise RuntimeError(f"Unable to find source for dependency {dependency}")

    def read_dependencies(self) -> Dict[str, Dict]:
        """
        Read the suite dependencies from the dependencies.yaml file - this is assumed to
        have been copied to the suite_path directory
        """

        with open(self.suite_path / "dependencies.yaml", "r") as stream:
            dependencies = yaml.safe_load(stream)
        for dependency, data in dependencies.items():
            if data["source"] is None:
                dependencies[dependency]["source"] = self.find_unknown_dependency(
                    dependency
                )
        return dependencies

    def get_workflow_id(self) -> str:
        """
        Read cylc install log for workflow id
        """

        with open(self.suite_path / "log" / "scheduler" / "log", "r") as f:
            for line in f:
                match = re.search(r"INFO - Workflow: (\S+\/\w+)", line)
                try:
                    workflow_id = match.group(1)
                    return workflow_id
                except IndexError:
                    continue

        return "unknown_workflow_id"

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
            if row[0] in ("g", "group"):
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
        Create an sql statement and query provided database. Return the result
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
        if not shell and isinstance(command, str):
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

    def cleanup(self) -> None:
        """
        Remove self.temp_directory
        """
        shutil.rmtree(self.temp_directory)
