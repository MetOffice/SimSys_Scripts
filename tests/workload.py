import json
import subprocess
from prettytable import PrettyTable
test = True
test_file = "/home/users/jennifer.hickson/test.json"

teams = {"SSD":
             ["cameronbateman-mo",
              "ericaneininger",
              "james-bruten-mo",
              "jennyhickson",
              "pierre-siddall",
              "r-sharp",
              "t00sa",
              "yaswant"],
         }

lfric_repositories = ["all",
                "lfric_apps",
                "lfric_core",
                ]

all_repositories = ["all",
                "lfric_apps",
                "lfric_core",
                "um",
                "jules",
                "socrates",
                "casim",
                "ukca",
                "simulation-systems",
                "SimSys_Scripts",
                "git_playground",
                "growss"]



def count_reviews(reviewers):
    unique_reviewers = set(reviewers)

    review_count = {}
    for reviewer in unique_reviewers:
        review_count[reviewer] = reviewers.count(reviewer)

    return review_count



class Reviews:

    def __init__(self, test):
        self.data = {}
        self.review_data = []
        self.fetch_project_data(test)
        self.filter_reviewers()
        self.tables = {}


    def fetch_project_data(self, test):
        if test:
            with open(test_file) as f:
                self.data = json.loads(f.read())

        else:
            command = "gh project item-list 376 -L 500 --owner MetOffice --format json"
            output = subprocess.run(command, shell=True, capture_output=True)
            self.data = json.loads(output.stdout)


    def filter_reviewers(self):
        all_reviews = self.data["items"]
        for review in all_reviews:
            if "code Review" in review:
                self.review_data.append((review["code Review"], review["repository"]))

            if "sciTech Review" in review:
                self.review_data.append((review["sciTech Review"], review["repository"]))


    def add_repo(self, repository=None):
        if repository == "all":
            reviewers = [x[0] for x in self.review_data]
        else:
            reviewers = [x[0] for x in self.review_data if repository in x[1]]

        return count_reviews(reviewers)


    def build_table(self, name, reviewer_list, repos):
        self.tables[name] = PrettyTable()

        self.tables[name].add_column("Reviewer", reviewer_list)

        for repo in repos:
            review_count = self.add_repo(repo)

            sorted_count = []
            for person in reviewer_list:
                if person in review_count:
                    sorted_count.append(review_count[person])
                else:
                    sorted_count.append(0)

            self.tables[name].add_column(repo, sorted_count)

    def all_reviewers(self):
        return sorted(list(set(review[0] for review in self.review_data)))

    def other_reviewers(self):
        reviewers = self.all_reviewers()

        for team, members in teams.items():
            for member in members:
                reviewers.remove(member)

        return reviewers


test_data = Reviews(True)
# full_data = Reviews(False)

teams["Other"] = test_data.other_reviewers()

for team, members in teams.items():
    if team == "SSD":
        repo_list = all_repositories
    else:
        repo_list = lfric_repositories
    test_data.build_table(team, members, repo_list)

for name, table in test_data.tables.items():
    print(name)
    table.align["Reviewer"] = "l"
    table.sortby = "all"
    print(table)

print("End")

