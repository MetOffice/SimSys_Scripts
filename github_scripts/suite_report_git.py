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

import argparse
import os
import re
import sys
from collections import defaultdict
from contextlib import contextmanager
from pathlib import Path
from tempfile import mkdtemp
from typing import Dict, List, Set, Tuple

from suite_data import SuiteData


def create_markdown_row(*columns: str, header=False) -> List[str]:
    """
    Join any number of columns into a markdown formatted table row
    Will attempt to format column entries as str
    """
    line = [f"| {' | '.join(str(c) for c in columns)} |"]
    if header:
        line.append(f"| {' | '.join(':---' for _ in columns)} |")
    return line


@contextmanager
def file_or_stdout(file_name: str):
    """
    Yield file_name if writable, or stdout if not
    """
    if file_name is None:
        yield sys.stdout
    else:
        with open(file_name, "w") as out_file:
            yield out_file


def extract_org_repo(url: str) -> str:
    """
    Extract 'Org/repo' from a GitHub URL (SSH or HTTPS).
    """

    match = re.search(r"github\.com[:/](.*?)(?:\.git)?$", url)
    if match:
        return match.group(1)
    return ""


class SuiteReport(SuiteData):
    """
    Suite Report object to gather suite data and write out to markdown formatted file
    """

    # str to enable pink text colour
    pink_text = r"$${\color{magenta}failed}$$"

    # str's for collapsed sections in markdown
    open_collapsed = "<details>"
    close_collapsed = "</details>"

    def __init__(self, suite_path: Path) -> None:
        self.suite_path: Path = suite_path
        self.suite_user = suite_path.owner()
        self.suite_starttime: str = self.get_suite_starttime()
        self.workflow_id: str = self.get_workflow_id()
        self.task_states: Dict[str, str] = self.get_task_states()
        self.groups: List[str] = self.read_groups_run()
        self.rose_data: Dict[str, str] = self.read_rose_conf()
        self.dependencies: Dict[str, Dict] = self.read_dependencies()
        self.primary_source: str = self.determine_primary_source()
        self.temp_directory = Path(mkdtemp())
        self.clone_sources()
        self.populate_gitinfo()
        self.populate_gitbdiff()
        self.trac_log = []

    def parse_local_source(self, source: str) -> Tuple[str, str]:
        """
        Find the branch name or hash and remote reference for a given source
        """

        source = self.temp_directory / source

        branch_name = self.run_command(
            f"git -C {source} branch --show-current", rval=True
        ).stdout.strip("\n")
        if branch_name and branch_name not in ("main", "stable", "trunk"):
            ref = branch_name
        else:
            result = self.run_command(f"git -C {source} rev-parse HEAD", rval=True)
            ref = result.stdout.strip("\n")

        remote = self.run_command(f"git -C {source} remote -v", rval=True).stdout.split(
            "\n"
        )
        for line in remote:
            if "origin" not in line:
                continue
            reference = line.split()[1]

        return reference, ref

    def create_suite_info_table(self) -> None:
        """
        Create a table containing data on the suite run
        """

        self.trac_log.extend(create_markdown_row("Item", "Value", header=True))

        rows = (
            ("Suite Name", self.workflow_id),
            ("Suite User", self.suite_user),
            ("Workflow Start", self.suite_starttime),
            ("Groups Run", ",".join(g for g in self.groups)),
        )

        for row in rows:
            self.trac_log.extend(create_markdown_row(*row))

        self.trac_log.append("")

    def create_dependency_table(self) -> None:
        """
        Create a table containing dependency information
        """

        self.trac_log.extend(
            create_markdown_row("Dependency", "Reference", "Main Like", header=True)
        )

        for dependency, data in self.dependencies.items():
            ref = data["ref"] or ""
            reference = data["source"]

            if ".git" not in reference:
                # This is a local copy. To avoid exposing hostname and path, parse it to
                # get an equivalent url.
                # There is a danger a local source has been changed from the tested
                # version in the intervening time, but this is acceptable
                reference, ref = self.parse_local_source(dependency)

            # Extract organisation and repo from the source
            org_repo = extract_org_repo(reference)

            # Check if the ref is a hash and use short form if so
            if re.match(r"^\s*([0-9a-f]{40})\s*$", ref):
                ref = ref[:7]
            url = f"https://github.com/{org_repo}/tree/{ref}"
            reference = f"[{org_repo}@{ref}]({url})"

            self.trac_log.extend(
                create_markdown_row(dependency, reference, data["gitinfo"].is_main())
            )

        self.trac_log.append("")

    def create_task_tables(self, parsed_tasks: Dict[str, List[str]]) -> None:
        """
        Create tables containing summary of task states and number of tasks won
        """

        # Ensure pink failures and then normal failures appear at the top
        sort_order = {"pink failure": 0, "failed": 1, "submit-failed": 2}
        order = list(parsed_tasks.keys())
        order.sort(key=lambda val: sort_order.get(val, len(sort_order)))

        state_emojis = {
            "failed": ":x:",
            "succeeded": ":white_check_mark:",
            "submit-failed": ":no_entry_sign:",
            "waiting": ":hourglass:",
        }

        # Create Collapsed task tables
        for state in order:
            emoji = state_emojis[state]
            tasks = parsed_tasks[state]
            if state == "succeeded":
                continue
            self.trac_log.append(f"{emoji} {state} tasks - {len(tasks)}")
            if not tasks:
                continue
            if state == "pink failure":
                state = self.pink_text
            self.trac_log.append(self.open_collapsed)
            self.trac_log.append(
                f"<summary>{emoji} {state} tasks - {len(tasks)}</summary>"
            )
            self.trac_log.append("")
            self.trac_log.extend(create_markdown_row("Task", "State", header=True))
            for task in sorted(tasks):
                self.trac_log.extend(create_markdown_row(task, state))
            self.trac_log.append(self.close_collapsed)

    def create_um_code_owner_table(self, owners: Dict) -> None:
        """
        Create a table of required UM code owner approvals
        """

        changed_sections: Set[str] = self.get_changed_um_section()
        if changed_sections:
            self.trac_log.extend(
                create_markdown_row("Section", "Owner", "Deputy", "State", header=True)
            )
            for section in changed_sections:
                users = owners.get(section, "Unknown")
                self.trac_log.extend(
                    create_markdown_row(section, users[0], users[1], "Pending")
                )
        else:
            self.trac_log.append("* No UM Code Owners Required")

    def create_um_config_owner_table(self, owners: Dict) -> None:
        """
        Create a table of required UM config owner approvals
        """

        failed_configs: Set[str] = self.get_um_failed_configs()
        if not failed_configs:
            self.trac_log.append("No UM Config Owners Required")
            return

        self.trac_log.extend(
            create_markdown_row("Owner", "Config (others)", "State", header=True)
        )

        # Create a dict with owners as the key
        table_dict = defaultdict(list)
        for config in failed_configs:
            owner, others = owners[config]
            if others != "--":
                config = f"{config} ({others})"
            table_dict[owner].append(config)

        for owner, configs in table_dict.items():
            self.trac_log.extend(
                create_markdown_row(
                    owner, " ".join(f'"{c}"' for c in configs), "Pending"
                )
            )

    def create_um_owners_tables(self) -> None:
        """
        Create tables for any UM Code Owners and Config Owners required
        """

        self.trac_log.append("## Approvals")

        self.trac_log.append("### Code Owners")
        code_owners = self.get_um_owners("CodeOwners.txt")
        self.create_um_code_owner_table(code_owners)

        self.trac_log.append("### Config Owners")
        config_owners = self.get_um_owners("ConfigOwners.txt")
        self.create_um_config_owner_table(config_owners)

    def create_log(self) -> None:
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

        # If UM suite, populate UM Owners
        if self.primary_source == "um":
            self.create_um_owners_tables()

        # Write Tasks Info
        self.trac_log.append("## Task Information")
        parsed_tasks = self.parse_tasks()
        self.create_task_tables(parsed_tasks)

    def write_log(self, log_path: Path) -> None:
        """
        Write the log to provided output
        """

        if log_path:
            log_path = log_path / "trac.log"
        with file_or_stdout(log_path) as wfile:
            for line in self.trac_log:
                wfile.write(line + "\n")


def check_log_path(opt: str) -> Path:
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


def check_suite_path(opt: str) -> Path:
    """
    Check that the suite_path is a valid cylc-run directory
    """

    opt = Path(opt).expanduser()
    if not opt or not opt.is_dir() or "cylc-run" not in str(opt):
        raise argparse.ArgumentTypeError(
            f"The suite_path argument, {opt}, is not a valid directory in cylc-run"
        )
    return opt


def parse_args() -> argparse.Namespace:
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

    args, _ = parser.parse_known_args()

    # Check log file is writable, set as None if not (this will output to stdout)
    if args.log_path and not os.access(args.log_path, os.W_OK):
        args.log_path = None

    return args


def main() -> None:
    """
    Main function for this program
    """

    args = parse_args()

    suite_report = SuiteReport(args.suite_path)
    try:
        suite_report.create_log()
        suite_report.write_log(args.log_path)
    finally:
        suite_report.cleanup()


if __name__ == "__main__":
    main()
