import argparse
import json
import subprocess
from pathlib import Path

from prettytable import PrettyTable

lfric_repositories = [
    "lfric_apps",
    "lfric_core",
]

ssd_repositories = [
    "um",
    "jules",
    "socrates",
    "casim",
    "ukca",
    "simulation-systems",
    "SimSys_Scripts",
    "git_playground",
    "growss",
]

# Ideally get this list another way but for now hardcode it.
other_reviewers = [
    "MetBenjaminWent",
    "jedbakerMO",
    # Chris Maynard
    # James CS
    # Oakley Brunt
]


class ProjectData:
    """
    A class to hold GitHub project data. The focus is on review information.

    data: dict Raw data from the project
    review_data: list Data filtered to contain a list of review tuples
    """

    def __init__(self, test:bool = False):
        self.data = {}
        self.review_data = []

        self.fetch_project_data(test)
        self.filter_reviewers()

    def fetch_project_data(self, test:bool):
        """
        Retrieve data from GitHub API or a from a test file.
        """
        if test:
            file = Path(__file__).with_name("test.json")
            with open(file) as f:
                self.data = json.loads(f.read())

        else:
            command = "gh project item-list 376 -L 500 --owner MetOffice --format json"
            output = subprocess.run(command, shell=True, capture_output=True)
            self.data = json.loads(output.stdout)

    def filter_reviewers(self):
        """
        Filter the data to create a list of review tuples
        """
        all_reviews = self.data["items"]
        for review in all_reviews:
            if "code Review" in review:
                self.review_data.append((review["code Review"], review["repository"]))

            if "sciTech Review" in review:
                self.review_data.append(
                    (review["sciTech Review"], review["repository"])
                )

    def one_repo(self, repository: str) -> list:
        """
        Filter the review data to just that of one repository

        repository: string Name of repository to include
        return: list All reviewers that have reviews assigned in that repository
                including duplicates.
        """
        return [x[0] for x in self.review_data if repository in x[1]]


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

    def set_team_members(self, test:bool):
        """
        Retrieve team members from GitHub API or a from a test file. Create
        a list of login IDs and sort it.
        """

        if test:
            file = Path(__file__).with_name(self.github_id + ".json")
            with open(file) as f:
                full_data = json.loads(f.read())
        else:
            command = (
                f"gh api "
                f'-H "Accept: application/vnd.github+json" '
                f"/orgs/MetOffice/teams/{self.github_id}/members"
            )

            output = subprocess.run(command, shell=True, capture_output=True)

            full_data = json.loads(output.stdout)

        for item in full_data:
            self.members.append(item["login"])

        self.members = sorted(self.members, key=str.lower)

    def get_team_members(self) -> list:
        """
        return: list of team members
        """
        return self.members


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
        review_count = count_items(data.one_repo(repo))

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

    parser = argparse.ArgumentParser("Create tables of review workload based on Simulation Systems Review Tracker")
    parser.add_argument(
        "--total",
        action='store_true',
        help="Sort tables by total number of reviews.",
    )
    parser.add_argument(
        "--test",
        action='store_true',
        help="Use test input files.",
    )

    return parser.parse_args()

def main(total: bool, test: bool):

    # Extract data from github about the reviews and team members.
    data = ProjectData(test)

    other_team = Team(test = test)
    other_team.members = other_reviewers

    teams = {
        "SSD": Team("ssdteam", test),
        "CCD": Team("core-capability-development", test),
        "TCD": Team("toolscollabdev", test),
        "Other": other_team,
    }

    # Create tables for each combination of reviewers and reposotories
    tables = {}

    ## Table for SSD only repositories
    repo_list = ssd_repositories
    reviewers = teams["SSD"].get_team_members()
    tables["SSD"] = build_table(data, reviewers, repo_list)

    ## Table for LFRic repositories
    repo_list = lfric_repositories
    reviewers = []
    for team in teams.values():
        reviewers += team.get_team_members()
    tables["LFRic"] = build_table(data, reviewers, repo_list)

    # Print tables
    for name, table in tables.items():
        print_table(name, table, total)


if __name__ == "__main__":
    args = parse_args()
    main(args.total, args.test)
