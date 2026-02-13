# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
Classes and functions for interacting with the Simulation Systems Review Tracker
Project.
"""
from __future__ import annotations

import json
import subprocess
from pathlib import Path
import shlex
from collections import defaultdict

REVIEW_ID = 376
ISSUE_ID = 418
PROJECT_OWNER = "MetOffice"


def run_command(command: str) -> subprocess.CompletedProcess:
    output = subprocess.run(shlex.split(command), capture_output=True, timeout=180)

    if output.returncode:
        raise RuntimeError(output.stderr.decode())

    return output


class ProjectData:
    """
    A class to hold GitHub project data

    pull_requsts: list raw_data turned into a list of PullRequest objects.
    test: bool Run using test data and extra logging.
    milestones: list All milestones currently used in the project
    repos: list All repositories currectly represented in the project
    """

    def __init__(self, project: int, items: list, test: bool = False):
        self.project = project
        self.project_items = items
        self.test = test

        # Extract these once as useful lists
        self.milestones = self._extract_milestones()
        self.repos = self._extract_repositories()

    @classmethod
    def from_github(
        cls, project: int, capture: bool = False, file: Path = None
    ) -> ProjectData:
        """
        Retrieve data from GitHub API and initialise the class.

        capture: True if data should be stored to a test file.
        file: Path to the test file to be written to.
        """
        print("Retrieving project data from GitHub")
        command = f"gh project item-list {project} -L 500 --owner {PROJECT_OWNER} --format json"
        output = run_command(command)

        raw_data = json.loads(output.stdout)

        # Remove body as is large before working with or storing data.
        for pr in raw_data["items"]:
            pr["content"].pop("body")

        if capture:
            if file:
                with open(file, "w") as f:
                    json.dump(raw_data, f)
                print(f"Project data saved to {file}.")
            else:
                print("Unable to capture data as filename not specified.")

        items = cls._extract_data(raw_data)
        return cls(project, items, test=False)

    @classmethod
    def from_file(cls, project: int, file: Path) -> ProjectData:
        """
        Retrieve data from test file and initialise the class.

        file: Path to the test file to be read.
        """
        with open(file) as f:
            raw_data = json.loads(f.read())

        items = cls._extract_data(raw_data)
        return cls(project, items, test=True)

    @classmethod
    def _extract_data(cls, raw_data: dict) -> list:
        """
        Extract useful information from the raw data and
        store it in a list of PullRequest objects.

        raw_data: github data from the project
        """

        item_list = []

        for item_data in raw_data["items"]:
            if item_data["content"]["type"] == "PullRequest":
                item = PullRequest(
                    id=item_data["id"],
                    number=item_data["content"]["number"],
                    title=item_data["content"]["title"],
                    repo=item_data["content"]["repository"].replace("MetOffice/", ""),
                )

                if "code Review" in item_data:
                    item.codeReview = item_data["code Review"]

                if "sciTech Review" in item_data:
                    item.scitechReview = item_data["sciTech Review"]

            elif item_data["content"]["type"] == "Issue":
                item = Issue(
                    id=item_data["id"],
                    number=item_data["content"]["number"],
                    title=item_data["content"]["title"],
                    repo=item_data["content"]["repository"].replace("MetOffice/", ""),
                )

            if "status" in item_data:
                item.status = item_data["status"]

            if "milestone" in item_data:
                item.milestone = item_data["milestone"]["title"]

            if "assignee" in item_data:
                item.assignee = item_data["assignees"]

            item_list.append(item)

        return item_list

    def _extract_milestones(self) -> set:
        """
        Create a set of milestones from the pull request data
        """
        milestones = set()
        for item in self.project_items:
            milestones.add(item.milestone)

        return milestones

    def _extract_repositories(self) -> set:
        """
        Create a set of repositories from the pull request data
        """
        repositories = set()
        for item in self.project_items:
            repositories.add(item.repo)

        return repositories

    def get_reviewers_for_repo(self, repo: str) -> list:
        """
        Return a list of reviewers for a given repository.
        """
        if repo not in self.repos:
            return []

        reviewers = []

        if self.test:
            print("\n=== Reviewers for " + repo)

        for item in self.project_items:
            if isinstance(item, PullRequest):
                if item.repo == repo:
                    sr = item.scitechReview
                    if sr:
                        reviewers.append(sr)

                    cr = item.codeReview
                    if cr:
                        reviewers.append(cr)

                    if self.test and (cr or sr):
                        # Handle case where these are None
                        if not sr:
                            sr = ""
                        if not cr:
                            cr = ""

                        print(
                            "SciTech:",
                            f"{sr: <18}",
                            "Code:",
                            f"{cr: <18}",
                            item.title,
                        )

        return reviewers

    def get_repositories(self) -> set:
        """Return a list of repositories found in the project data."""

        return self.repos

    def get_all_milestones(self, status: str = "all") -> dict:
        """
        Return pull requests organized by milestone and repository. These can
        be filtered by status.

        status: str Status to include. Valid values are any project status
                    values and all, open or closed
        """

        milestone_data = {}

        for milestone in self.milestones:
            milestone_data[milestone] = self.get_milestone(milestone, status)

        return milestone_data

    def get_milestone(self, milestone: str, status: str = "all") -> dict:
        """
        Return all pull requests for one milestone organised by repository.
        These can be filtered by status.

        milestone: str Title of milestone to include.
        status: str Status to include. Valid values are any project status
                values and all, open or closed
        """

        milestone_data = defaultdict(list)

        for item in self.project_items:
            if item.milestone == milestone and (
                item.status == status
                or status == "all"
                or (status == "open" and item.status in item.open_states)
                or (status == "closed" and item.status not in item.open_states)
            ):

                milestone_data[item.repo].append(item)
        return milestone_data

    def archive_milestone(self, milestone: str, dry_run: bool = False) -> None:
        """
        Archive all items for a given milestone from the project.

        milestone: Title of milestone to archive
        dry_run: If true then dummy the commands
        """

        print(f"Archiving all completed items for {milestone}")

        dry_run = dry_run | self.test  # if test data, or a dryrun, then dummy commands

        closed = self.get_milestone(milestone=milestone, status="closed")
        for repo in closed:
            for item in closed[repo]:
                item.archive(self.project, dry_run)


class ProjectItem:
    """
    Base class for pull requests and issues

    id: github ID for the item
    number: number of the item in the repository
    title: title of the item
    repo: repository where the item is located
    status: status of the item
    milestone: title of the milestone
    """

    open_states = []
    command_type = None

    def __init__(
        self, id: str = None, number: str = None, title: str = None, repo: str = None
    ):
        self.id = id
        self.number = number
        self.title = title
        self.repo = repo

        self.status = None
        self.milestone = "None"
        self.assignee = None

    def archive(self, project: int, dry_run: bool = False) -> None:
        """
        Archive this item from the project.

        dry_run: If true, print the command used rather than archiving.
        """

        command = (
            f"gh project item-archive {project} --owner {PROJECT_OWNER} --id {self.id}"
        )
        message = f"Archiving #{self.number} in {self.repo}"

        if dry_run:
            print(f"[DRY RUN] {message: <40} {command}")
        else:
            print(message)
            run_command(command)

    def modify_milestone(self, milestone: str, dry_run: bool = False) -> None:
        """
        Change the milestone for this pull request.

        milestone: title of milestone to change to
        dry_run, If true, print the command rather than making a change.
        """

        command = f"gh {self.command_type} edit {self.number} --repo='{PROJECT_OWNER}/{self.repo}' --milestone='{milestone}'"
        message = f"Changing milestone for #{self.number} in {self.repo}"

        if dry_run:
            print(f"[DRY RUN] {message: <50} {command}")
        else:
            print(message)
            run_command(command)

        self.milestone = milestone

    def add_comment(self, text: str, dry_run: bool = False) -> None:
        """
        Add a comment to this item

        test: string to form the comment
        dry_run: If true, print the command rather than making a change.
        """

        command = f"gh {self.command_type} comment {self.number} --repo='{PROJECT_OWNER}/{self.repo}' --body='{text}'"
        message = f"Adding comment to #{self.number} in {self.repo}"

        if dry_run:
            print(f"[DRY RUN] {message: <50} {command}")
        else:
            print(message)
            run_command(command)


class PullRequest(ProjectItem):
    """
    Class for an individual pull request to hold key information and provide
    functions to modify the pull request.


    assignee: assignee of the pull request, which is the developer
    scitechReview: user assigned to sciTech review the pull request
    codeReview: user assigned to code review the pull request
    """

    open_states = [
        "In Progress",
        "SciTech Review",
        "Code Review",
        "Approved",
        "Changes Requested",
    ]

    command_type = "pr"

    def __init__(
        self, id: str = None, number: str = None, title: str = None, repo: str = None
    ):
        super().__init__(id, number, title, repo)

        self.scitechReview = None
        self.codeReview = None


class Issue:
    """
    Class for an individual issue to hold key information and provide
    functions to modify the issue.

    id: github ID for the pull request
    number: number of the pull request in the repository
    title: title of the pull request
    repo: repository where the pull request is located
    status: status of the pull request
    milestone: title of the milestone
    assignee: assignee of the pull request, which is the developer
    """

    open_states = ["New Issue", "Ready for Work", "In Progress", "In Review"]
