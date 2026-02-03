# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
Classes and functions for interacting with the Simulation Systems Review Tracker
Project.
"""

import json
import subprocess
from pathlib import Path
from collections import defaultdict

project_id = 376
project_owner = "MetOffice"


def run_command(command: str) -> subprocess.CompletedProcess:
    output = subprocess.run(command.split(), capture_output=True, timeout=180)

    if output.returncode:
        raise RuntimeError(output.stderr.decode())

    return output


class ProjectData:
    """
    A class to hold GitHub project data

    data: list raw_data turned into a list of PullRequest objects.
    test: bool Run using test data and extra logging.
    milestones: list All milestones currently used in the project
    repos: list All repositories currectly represented in the project
    """

    open_states = [
        "In Progress",
        "SciTech Review",
        "Code Review",
        "Approved",
        "Changes Requested",
    ]

    def __init__(
        self,
        data: list,
        test: bool = False,
        milestones: list = None,
        repos: list = None,
    ):
        self.data = data
        self.test = test
        self.milestones = milestones
        self.repos = repos

    @classmethod
    def from_github(cls, capture: bool = False, file: Path = None) -> "ProjectData":
        """
        Retrieve data from GitHub API and initialise the class.
        """
        command = f"gh project item-list {project_id} -L 500 --owner {project_owner} --format json"
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

        data, milestones, repositories = cls._extract_data(raw_data)
        return cls(data=data, test=False, milestones=milestones, repos=repositories)

    @classmethod
    def from_file(cls, file: Path) -> "ProjectData":
        """
        Retrieve data from test file and initialise the class.
        """
        with open(file) as f:
            raw_data = json.loads(f.read())

        data, milestones, repositories = cls._extract_data(raw_data)
        return cls(data=data, test=True, milestones=milestones, repos=repositories)

    @classmethod
    def _extract_data(cls, raw_data: dict) -> (dict, list):
        """
        Extract useful information from the raw data and
        store it in a list of PullRequest objects. Also extract a list of
        milestones and repositories found in the project.
        """

        data = []
        milestones = set()
        milestones.add("None")
        repositories = set()

        for pr in raw_data["items"]:
            pull_request = PullRequest(
                id=pr["id"],
                number=pr["content"]["number"],
                title=pr["content"]["title"],
                repo=pr["content"]["repository"].replace("MetOffice/", ""),
            )

            repositories.add(pull_request.repo)

            if "status" in pr:
                pull_request.status = pr["status"]

            if "milestone" in pr:
                pull_request.milestone = pr["milestone"]["title"]
                milestones.add(pull_request.milestone)

            if "assignee" in pr:
                pull_request.assignee = pr["assignees"]

            if "code Review" in pr:
                pull_request.codeReview = pr["code Review"]

            if "sciTech Review" in pr:
                pull_request.scitechReview = pr["sciTech Review"]

            data.append(pull_request)

        return data, milestones, repositories

    def get_reviewers_for_repo(self, repo: str) -> list:
        """
        Return a list of reviewers for a given repository.
        """
        if repo not in self.repos:
            return []

        reviewers = []

        if self.test:
            print("\n=== Reviewers for " + repo)

        for pr in self.data:
            if pr.repo == repo:
                sr = pr.scitechReview
                if sr:
                    reviewers.append(sr)

                cr = pr.codeReview
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
                        pr.title,
                    )

        return reviewers

    def get_repositories(self) -> list:
        """Return a list of repositories found in the project data."""

        return self.repos

    def get_by_milestone(self, status: str = "all") -> dict:
        """
        Return pull requests organized by milestone and repository. These can
        be filtered by status.

        status: str Status to include. Valid values are any project status
                    values and all, open or closed
        """

        milestone_data = {}
        for milestone in self.milestones:
            milestone_data[milestone] = defaultdict(list)

        for pr in self.data:
            if (
                pr.status == status
                or status == "all"
                or (status == "open" and pr.status in self.open_states)
                or (status == "closed" and pr.status not in self.open_states)
            ):
                milestone_data[pr.milestone][pr.repo].append(pr)

        return milestone_data

    def archive_milestone(self, milestone: str, dry_run: bool = False) -> None:

        print(f"Archiving all completed pull requests for {milestone}")

        dry_run = dry_run | self.test  # if test data, or a dryrun, then dummy commands

        closed_prs = self.get_by_milestone(status="closed")[milestone]
        for repo in closed_prs:
            for pr in closed_prs[repo]:
                pr.archive(dry_run)


class PullRequest:

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
        self.scitechReview = None
        self.codeReview = None

    def archive(self, dry_run: bool = False):
        """
        Archive this pull request from the project.

        dry_run: If true, print the command used rather than archiving.
        """

        command = f"gh project item-archive {project_id} --owner {project_owner} --id {self.id}"
        message = f"Archiving #{self.number} in {self.repo}"

        if dry_run:
            print(f"[DRY RUN] {message: <40} {command}")
        else:
            print(message)
            run_command(command)
