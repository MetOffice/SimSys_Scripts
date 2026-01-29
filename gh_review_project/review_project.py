# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

'''
Class and functions for interacting with the Simulation Systems Review Tracker
Project.
'''

import json
import subprocess
from pathlib import Path

class ProjectData:
    """
    A class to hold GitHub project data

    raw_data: dict Raw data from the project
    data: dict Data filtered to contain most needed pull request details,
               sorted by repository.
    """

    def __init__(self, test: bool = False, capture: bool = False):
        self.raw_data = {}
        self.data = {}

        self.fetch_project_data(test, capture)
        self.extract_data()

    def fetch_project_data(self, test: bool, capture: bool):
        """
        Retrieve data from GitHub API or a from a test file.
        """
        if test:
            file = Path(__file__).parent / "test" / "test.json"
            with open(file) as f:
                self.raw_data = json.loads(f.read())

        else:
            command = "gh project item-list 376 -L 500 --owner MetOffice --format json"
            output = subprocess.run(command.split(), capture_output=True, timeout=180)
            if output.returncode:
                raise RuntimeError(
                    "Error fetching GitHub Project data:  \n " + output.stderr.decode()
                )

            self.raw_data = json.loads(output.stdout)

            if capture:
                file = Path(__file__).parent / "test" / "test.json"
                with open(file, "w") as f:
                    json.dump(self.raw_data, f)
                print(
                    "Project data saved to test.json. Use --test to run with"
                    " the captured data."
                )

    def extract_data(self):
        for pr in self.raw_data["items"]:
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
                pull_request["milestone"] = None

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
            if repo in self.data:
                self.data[repo].append(pull_request)
            else:
                self.data[repo] = [pull_request]

    def get_reviewers_for_repo(self, repo: str, test: bool = False) -> list:
        """
        Return a list of reviewers for a given repository.
        """
        if repo in self.data:
            pull_requests = self.data[repo]
        else:
            return []

        reviewers = []

        if test:
            print("\n=== Reviewers for repository " + repo)

        for pr in pull_requests:
            sr = pr["scitech review"]
            if sr:
                reviewers.append(sr)

            cr = pr["code review"]
            if cr:
                reviewers.append(cr)

            if test and (cr or sr):
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
        """ Return a list of repositories found in the project data."""

        return list(self.data.keys())
