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

    pr_open_states = [
        "In Progress",
        "SciTech Review",
        "Code Review",
        "Approved",
        "Changes Requested",
    ]

    def __init__(self, project: int, pull_requests: list, test: bool = False):
        self.project = project
        self.pull_requests = pull_requests
        self.test = test

        # Extract these once as useful lists
        self.milestones = self._extract_milestones()
        self.repos = self._extract_repositories()

    @classmethod
    def from_github(cls, project: int, capture: bool = False, file: Path = None) -> ProjectData:
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

        pull_requests = cls._extract_data(raw_data)
        return cls(project, pull_requests, test=False)

    @classmethod
    def from_file(cls, project: int, file: Path) -> ProjectData:
        """
        Retrieve data from test file and initialise the class.

        file: Path to the test file to be read.
        """
        with open(file) as f:
            raw_data = json.loads(f.read())

        pull_requests = cls._extract_data(raw_data)
        return cls(project, pull_requests, test=True)

    @classmethod
    def _extract_data(cls, raw_data: dict) -> list:
        """
        Extract useful information from the raw data and
        store it in a list of PullRequest objects.

        raw_data: github data from the project
        """

        pr_list = []

        for pr in raw_data["items"]:
            pull_request = PullRequest(
                id=pr["id"],
                number=pr["content"]["number"],
                title=pr["content"]["title"],
                repo=pr["content"]["repository"].replace("MetOffice/", ""),
            )

            if "status" in pr:
                pull_request.status = pr["status"]

            if "milestone" in pr:
                pull_request.milestone = pr["milestone"]["title"]

            if "assignee" in pr:
                pull_request.assignee = pr["assignees"]

            if "code Review" in pr:
                pull_request.codeReview = pr["code Review"]

            if "sciTech Review" in pr:
                pull_request.scitechReview = pr["sciTech Review"]

            pr_list.append(pull_request)

        return pr_list

    def _extract_milestones(self) -> set:
        """
        Create a set of milestones from the pull request data
        """
        milestones = set()
        for pr in self.pull_requests:
            milestones.add(pr.milestone)

        return milestones

    def _extract_repositories(self) -> set:
        """
        Create a set of repositories from the pull request data
        """
        repositories = set()
        for pr in self.pull_requests:
            repositories.add(pr.repo)

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

        for pr in self.pull_requests:
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

        for pr in self.pull_requests:
            if pr.milestone == milestone and (
                pr.status == status
                or status == "all"
                or (status == "open" and pr.status in self.pr_open_states)
                or (status == "closed" and pr.status not in self.pr_open_states)
            ):

                milestone_data[pr.repo].append(pr)
        return milestone_data

    def archive_milestone(self, milestone: str, dry_run: bool = False) -> None:
        """
        Archive all pull requests for a given milestone from the project.

        milestone: Title of milestone to archive
        dry_run: If true then dummy the commands
        """

        print(f"Archiving all completed pull requests for {milestone}")

        dry_run = dry_run | self.test  # if test data, or a dryrun, then dummy commands

        closed_prs = self.get_milestone(milestone=milestone, status="closed")
        for repo in closed_prs:
            for pr in closed_prs[repo]:
                pr.archive(self.project, dry_run)


class PullRequest:
    """
    Class for an individual pull request to hold key information and provide
    functions to modify the pull request.

    id: github ID for the pull request
    number: number of the pull request in the repository
    title: title of the pull request
    repo: repository where the pull request is located
    status: status of the pull request
    milestone: title of the milestone
    assignee: assignee of the pull request, which is the developer
    scitechReview: user assigned to sciTech review the pull request
    codeReview: user assigned to code review the pull request
    """

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

    def archive(self, project: int, dry_run: bool = False) -> None:
        """
        Archive this pull request from the project.

        dry_run: If true, print the command used rather than archiving.
        """

        command = f"gh project item-archive {project} --owner {PROJECT_OWNER} --id {self.id}"
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

        command = f"gh pr edit {self.number} --repo='{PROJECT_OWNER}/{self.repo}' --milestone='{milestone}'"
        message = f"Changing milestone for #{self.number} in {self.repo}"

        if dry_run:
            print(f"[DRY RUN] {message: <50} {command}")
        else:
            print(message)
            run_command(command)

        self.milestone = milestone
