# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
Class and functions for interacting with the Simulation Systems Review Tracker
Project.
"""

import json
import subprocess
from pathlib import Path
from collections import defaultdict


class ProjectData:
    """
    A class to hold GitHub project data

    data: dict Data filtered to contain most needed pull request details,
               sorted by repository.
    test: bool Run using test data and extra logging.
    """

    open_states = [
        "In Progress",
        "SciTech Review",
        "Code Review",
        "Approved",
        "Changes Requested",
    ]

    def __init__(self, data: dict, test: bool = False, milestones: list = None):
        self.data = data
        self.test = test
        self.milestones = milestones

    @classmethod
    def from_github(cls, capture: bool = False, file: Path = None) -> "ProjectData":
        """
        Retrieve data from GitHub API and initialise the class.
        """
        command = "gh project item-list 376 -L 500 --owner MetOffice --format json"
        output = subprocess.run(command.split(), capture_output=True, timeout=180)
        if output.returncode:
            raise RuntimeError(
                "Error fetching GitHub Project data:  \n " + output.stderr.decode()
            )

        raw_data = json.loads(output.stdout)

        if capture:
            if file:
                with open(file, "w") as f:
                    json.dump(raw_data, f)
                print(f"Project data saved to {file}.")
            else:
                print("Unable to capture data as filename not specified.")

        data, milestones = cls._extract_data(raw_data)
        return cls(data=data, test=False, milestones=milestones)

    @classmethod
    def from_file(cls, file: Path) -> "ProjectData":
        """
        Retrieve data from test file and initialise the class.
        """
        with open(file) as f:
            raw_data = json.loads(f.read())

        data, milestones = cls._extract_data(raw_data)
        return cls(data=data, test=True, milestones=milestones)

    @classmethod
    def _extract_data(cls, raw_data: dict) -> dict:
        """
        Extract useful information from the raw data and
        store it in a dictionary keyed by repository.
        """

        data = defaultdict(list)
        milestones = set()

        for pr in raw_data["items"]:
            pull_request = {}
            pull_request["id"] = pr["id"]
            pull_request["title"] = pr["content"]["title"]
            pull_request["number"] = pr["content"]["number"]

            if "status" in pr:
                pull_request["status"] = pr["status"]
            else:
                pull_request["status"] = None

            if "milestone" in pr:
                pull_request["milestone"] = pr["milestone"]["title"]
            else:
                pull_request["milestone"] = "None"

            milestones.add(pull_request["milestone"])

            if "assignee" in pr:
                pull_request["assignee"] = pr["assignees"]
            else:
                pull_request["assignee"] = None

            if "code Review" in pr:
                pull_request["code review"] = pr["code Review"]
            else:
                pull_request["code review"] = None

            if "sciTech Review" in pr:
                pull_request["scitech review"] = pr["sciTech Review"]
            else:
                pull_request["scitech review"] = None

            repo = pr["content"]["repository"].replace("MetOffice/", "")
            data[repo].append(pull_request)

        return data, milestones

    def get_reviewers_for_repo(self, repo: str) -> list:
        """
        Return a list of reviewers for a given repository.
        """
        if repo in self.data:
            pull_requests = self.data[repo]
        else:
            return []

        reviewers = []

        if self.test:
            print("\n=== Reviewers for " + repo)

        for pr in pull_requests:
            sr = pr["scitech review"]
            if sr:
                reviewers.append(sr)

            cr = pr["code review"]
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
                    pr["title"],
                )

        return reviewers

    def get_repositories(self) -> list:
        """Return a list of repositories found in the project data."""

        return list(self.data.keys())

    def get_by_milestone(self, status: str = "all") -> dict:
        """
        Return pull requests organized by milestone and repository. These can
        be filtered by status.

        status: str Status to include. Valid values are any project status
                    values and all, open or closed
        """

        milestone_data = defaultdict(dict)

        for repo in self.data:
            for pr in self.data[repo]:
                if (
                    pr["status"] == status
                    or status == "all"
                    or (status == "open" and pr["status"] in self.open_states)
                    or (status == "closed" and pr["status"] not in self.open_states)
                ):
                    milestone = pr["milestone"]
                    if not milestone_data[milestone]:
                        milestone_data[milestone] = defaultdict(list)
                    milestone_data[milestone][repo].append(pr)

        return milestone_data
