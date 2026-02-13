# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
This script will read the details of pull requests from the Simulation Systems
Review Tracker project and print tables of the number of reviews assigned to
each reviewer.
"""

import argparse
import json
import subprocess
from pathlib import Path
from prettytable import PrettyTable

from review_project import ProjectData, REVIEW_ID

lfric_repositories = [
    "lfric_apps",
    "lfric_core",
]

adminID = "MGEX82"  # person in github teams as a central admin but not relevant here


class Team:
    """
    A class to hold GitHub team data.

    github_id: str GitHub team ID used for fetching team data
    members: A list of team members
    """

    def __init__(self, github_id: str = None, test: bool = False):
        self.github_id = github_id
        self.members = []

        if github_id:
            self.set_team_members(test)

    def set_team_members(self, test: bool):
        """
        Retrieve team members from GitHub API or a from a test file. Create
        a list of login IDs and sort it.
        """

        if test:
            team_file = self.github_id + ".json"
            file = Path(__file__).parent / "test" / team_file
            with open(file) as f:
                full_data = json.loads(f.read())
        else:
            command = (
                f"gh api "
                f'-H "Accept: application/vnd.github+json" '
                f"/orgs/MetOffice/teams/{self.github_id}/members"
            )

            output = subprocess.run(
                command, shell=True, capture_output=True, timeout=180
            )
            if output.returncode:
                raise RuntimeError(
                    "Error fetching GitHub Team data: \n" + output.stderr.decode()
                )

            full_data = json.loads(output.stdout)

        for item in full_data:
            person_id = item["login"]
            if person_id != adminID:
                self.members.append(person_id)

        self.members = sorted(self.members, key=str.lower)

    def get_team_members(self) -> list:
        """
        return: list of team members
        """
        return self.members


def other_repo_list(data: ProjectData, to_exclude: list) -> list:
    """
    Create a list of all repositories with data in the project, not including
    any repositories that are found elsewhere.
    """

    all_repos = data.get_repositories()

    return sorted(set(all_repos) - set(to_exclude))


def count_items(item_list: list) -> dict:
    """
    Count the number of occurrences of each item in a list.

    item_list: list
    returns: dict dictionary of unique items with a count of occurrences.
    """
    unique_items = set(item_list)

    count = {}
    for item in unique_items:
        count[item] = item_list.count(item)

    return count


def build_table(data: ProjectData, reviewer_list: list, repos: list) -> PrettyTable:
    """
    Build a pretty table from the data by extracting just the desired
    repositories and reviewers.

    data: Project GitHub project data of reviews
    reviewer_list: list reviewers desired in this table
    repos: list repositories desired in this table
    returns: PrettyTable table of number of reviews completed by each person.
    """
    table = PrettyTable()

    table.add_column("Reviewer", reviewer_list)

    totals = [0] * len(reviewer_list)

    for repo in repos:
        review_count = count_items(data.get_reviewers_for_repo(repo))

        sorted_count = []
        for index, person in enumerate(reviewer_list):
            if person in review_count:
                sorted_count.append(review_count[person])
                totals[index] += review_count[person]
            else:
                sorted_count.append(0)

        table.add_column(repo, sorted_count)

    table.add_column("Total", totals)

    return table


def print_table(title: str, table: PrettyTable, sortTotal: bool) -> None:
    """
    Print a pretty table and its title.

    title: str Title of table to be printed first
    table: PrettyTable table to be printed
    """
    print(title)
    # table.set_style(TableStyle.MARKDOWN) #requires newer version
    table.align["Reviewer"] = "l"

    if sortTotal:
        table.sortby = "Total"

    print(table)


def parse_args():
    """
    Read command line args
    """

    testfile = Path(__file__).parent / "test" / "test.json"

    parser = argparse.ArgumentParser(
        "Create tables of review workload based on Simulation Systems Review Tracker"
    )
    parser.add_argument(
        "--total",
        action="store_true",
        help="Sort tables by total number of reviews.",
    )
    parser.add_argument(
        "--test",
        action="store_true",
        help="Use test input files.",
    )
    parser.add_argument(
        "--capture_project",
        action="store_true",
        help="Capture the current project status into the test file",
    )
    parser.add_argument(
        "--file",
        default=testfile,
        help="Filepath to test data for either capture the project status, "
        "or use as input data.",
    )

    args = parser.parse_args()

    args.file = Path(args.file)
    args.file = args.file.expanduser().resolve()

    return args


def main(total: bool, test: bool, capture_project: bool, file: Path):

    # Extract data from github about the reviews and team members.
    if test:
        data = ProjectData.from_file(REVIEW_ID, file)
    else:
        data = ProjectData.from_github(REVIEW_ID, capture_project, file)

    teams = {
        "SSD": Team("ssdteam", test),
        "CCD": Team("core-capability-development", test),
        "TCD": Team("toolscollabdev", test),
        "Other": Team("SimSysCodeReviewers", test),
    }

    # Create tables for each combination of reviewers and reposotories
    tables = {}

    ## Table for non-LFRic repositories
    repo_list = other_repo_list(data, lfric_repositories)
    reviewers = teams["SSD"].get_team_members()
    tables["SSD"] = build_table(data, reviewers, repo_list)

    ## Table for LFRic repositories
    repo_list = lfric_repositories
    reviewers = []
    for team in teams.values():
        members = team.get_team_members()
        # Not using sets to deduplicate to preserve list order, keeping
        # people in their teams.
        for person in members:
            if person not in reviewers:
                reviewers.append(person)
    tables["LFRic"] = build_table(data, reviewers, repo_list)

    # Print tables
    for name, table in tables.items():
        print_table(name, table, total)


if __name__ == "__main__":
    args = parse_args()
    main(args.total, args.test, args.capture_project, args.file)
