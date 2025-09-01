#!/usr/bin/env python3
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
Script to produce report on testing completed by a simulation-systems test
suite. Intended to be run at the end of a rose-stem run.
"""

import sys
import os
import argparse
from suite_data import SuiteData
from pathlib import Path
from tempfile import mkdtemp
from typing import Dict, List
from contextlib import contextmanager


def create_markdown_row(*columns: str, header=False) -> List[str]:
    """
    Join any number of columns into a markdown formatted table row
    Will attempt to format column entries as str
    """
    line = [f"| {' | '.join(str(c) for c in columns)} |"]
    if header:
        line.append(f"| {' | '.join(":---:" for _ in columns)} |")
    return line

@contextmanager
def file_or_stdout(file_name):
    if file_name is None:
        yield sys.stdout
    else:
        with open(file_name, 'w') as out_file:
            yield out_file

class SuiteReport(SuiteData):

    def __init__(self, suite_path: Path):
        self.suite_path: Path = suite_path
        self.suite_user = suite_path.owner()
        self.suite_starttime: str = self.get_suite_starttime()
        self.workflow_id: str = self.get_workflow_id()
        self.suite_database: Dict[str, str] = self.get_task_states()
        self.groups: str = self.read_groups_run()
        self.rose_data: Dict[str, str] = self.read_rose_conf()
        self.dependencies: Dict[str, Dict] = self.read_dependencies()
        self.primary_source: str = self.determine_primary_source()
        self.temp_directory = Path(mkdtemp())
        self.clone_sources()
        self.populate_gitinfo()
        self.populate_gitbdiff()
        self.trac_log = []

    def create_suite_info_table(self):
        """
        Create a table containing data on the suite run
        """

        self.trac_log.extend(create_markdown_row("Item", "Value", header=True))

        rows = (
            ("Suite Name", self.workflow_id),
            ("Suite User", self.suite_user),
            ("Workflow Start", self.suite_starttime),
            ("Groups Run", ",".join(g for g in self.groups)),
            ("ROSE_ORIG_HOST", self.rose_data["ROSE_ORIG_HOST"]),
        )

        for row in rows:
            self.trac_log.extend(create_markdown_row(*row))

        self.trac_log.append("")


    def create_dependency_table(self):
        """
        Create a table containing dependency information
        """

        self.trac_log.extend(create_markdown_row("Dependency", "Source", "Ref", "Main Like", header=True))

        for dependency, data in self.dependencies.items():
            self.trac_log.extend(create_markdown_row(dependency, data["source"], data["ref"], data["gitinfo"].is_main()))

        self.trac_log.append("")



    def create_log(self):
        """
        Create the trac.log file, writing each line as a str in self.trac_log
        """

        # Write Header
        self.trac_log.append(
            f"# Test Suite Results - {self.primary_source} - {self.workflow_id}"
        )
        self.trac_log.append("")

        # Write Suite Info
        self.trac_log.append("## Suite Information")
        self.trac_log.append("")
        self.create_suite_info_table()
        self.create_dependency_table()


    def write_log(self, log_path):
        """
        Write the log to provided output
        """

        if log_path:
            log_path = log_path / "trac_log"
        with file_or_stdout(log_path) as wfile:
            for line in self.trac_log:
                wfile.write(line + "\n")


def check_log_path(opt) -> Path:
    """
    Check that log_path is a valid directory, if it has been provided.
    """

    if opt is None:
        return opt

    opt = Path(opt).expanduser()

    if not opt.is_dir():
        raise argparse.ArgumentTypeError(
            "The provided log_path is not a valid directory"
        )
    return opt


def check_suite_path(opt) -> Path:
    """
    Check that the suite_path is a valid cylc-run directory
    """

    opt = Path(opt).expanduser()
    if not opt or not opt.is_dir() or "cylc-run" not in str(opt):
        raise argparse.ArgumentTypeError(
            f"The suite_path argument, {opt}, is not a valid directory in cylc-run"
        )
    return opt


def parse_args():
    """
    Parse command line arguments
    """

    suite_path = os.environ.get("CYLC_WORKFLOW_RUN_DIR", None)

    parser = argparse.ArgumentParser(description="Generate a suite report")
    parser.add_argument(
        "-L",
        "--log_path",
        default=suite_path,
        type=check_log_path,
        help="path to suite, including runN directory, "
        "defaults to finding the suite directory from Cylc Env variables. "
        "If not found, stdout will be used instead.",
    )
    parser.add_argument(
        "-S",
        "--suite_path",
        default=suite_path,
        type=check_suite_path,
        help="path to suite, including runN directory, "
        "defaults to finding the suite directory from Cylc Env variables.",
    )

    return parser.parse_args()


def main():
    """
    Main function for this program
    """

    args = parse_args()

    suite_report = SuiteReport(args.suite_path)

    suite_report.create_log()

    suite_report.write_log(args.log_path)

    suite_report.cleanup()


if __name__ == "__main__":
    main()
