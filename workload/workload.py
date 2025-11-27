import json
import subprocess
from prettytable import PrettyTable

test = False

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
                "growss"]


def count_items(item_list):
    unique_items = set(item_list)

    count = {}
    for item in unique_items:
        count[item] = item_list.count(item)

    return count


def build_table(project, reviewer_list, repos):
    table = PrettyTable()

    table.add_column("Reviewer", reviewer_list)

    totals = [0] * len(reviewer_list)

    for repo in repos:
        review_count = count_items(project.one_repo(repo))

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


def print_table(name, table):
    print(name)
    # table.set_style(TableStyle.MARKDOWN) #requires newer version
    table.align["Reviewer"] = "l"
    # table.sortby = "Total"
    print(table)


class ProjectData:
    def __init__(self):
        self.data = {}
        self.review_data = []

        self.fetch_project_data()
        self.filter_reviewers()


    def fetch_project_data(self):
        if test:
            with open("test.json") as f:
                self.data = json.loads(f.read())

        else:
            command = "gh project item-list 376 -L 500 --owner MetOffice --format json"
            output = subprocess.run(command, shell=True, capture_output=True)
            self.data = json.loads(output.stdout)


    def filter_reviewers(self):
        all_reviews = self.data["items"]
        for review in all_reviews:
            if "code Review" in review:
                self.review_data.append(Review(review["code Review"], review["repository"]))

            if "sciTech Review" in review:
                self.review_data.append(Review(review["sciTech Review"], review["repository"]))

    def one_repo(self, repository):
        return [x.get_reviewer() for x in self.review_data if repository in x.get_repo()]


class Team:

    def __init__(self, team_name, github_id):
        self.name = team_name
        self.github_id = github_id
        self.members = []

        self.set_team_members()

    def set_team_members(self):

        if test:
            file = self.github_id + ".json"
            with open(file) as f:
                full_data = json.loads(f.read())
        else:
            command = (f"gh api "
                        f"-H \"Accept: application/vnd.github+json\" "
                        f"/orgs/MetOffice/teams/{self.github_id}/members")

            output = subprocess.run(command, shell=True, capture_output=True)

            full_data = json.loads(output.stdout)

        for item in full_data:
            self.members.append(item["login"])

        self.members.sort()

    def get_team_members(self):
        return self.members


class Review:
    def __init__(self, reviewer, repository):
        self.reviewer = reviewer
        self.repository = repository

    def get_reviewer(self):
        return self.reviewer

    def get_repo(self):
        return self.repository


def main():
    data = ProjectData()

    teams = [Team("SSD", "ssdteam"),
             Team("CCD", "core-capability-development"),
             Team("TCD", "toolscollabdev")]

    tables = {}

    # Table for SSD only repositories
    repo_list = ssd_repositories
    reviewers = teams[0].get_team_members()
    tables["SSD"] = build_table(data, reviewers, repo_list)

    # Table for LFRic repositories
    repo_list = lfric_repositories
    reviewers = []
    for team in teams:
        reviewers += team.get_team_members()
    tables["LFRic"] = build_table(data, reviewers, repo_list)

    # Print tables
    for name, table in tables.items():
        print_table(name, table)


if __name__ == "__main__":
    main()


