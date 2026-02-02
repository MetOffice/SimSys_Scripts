# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
This script will run the processes needed to close off and finish a milestone
    * Archive all completed PRs against the milestone in the Review Tracker Project
    * Close the milestone in each repository
    * Report on:
        * Closed PRs at a different milestone
        * Remaining open PRs and issues against this milestone
        * Closed PRs against this milestone
"""
from collections import defaultdict
from pathlib import Path
import argparse
from review_project import ProjectData


def print_banner(message: str) -> None:
    print("\n")
    print("=" * len(message))
    print(message)
    print("=" * len(message))


def still_open(open_prs: dict, current_milestone: str) -> int:
    """
    Report on all open pull requests for the current milestone
    """

    print_banner(f"Checking for open pull requests for {current_milestone}")

    total = 0

    for repo in open_prs:
        print(f"{repo} \n{'-'*len(repo)}")
        for pr in open_prs[repo]:
            print(f"{pr['status']: <18} #{pr['number']: <5} {pr['title']}")

        count = len(open_prs[repo])
        print(f"->   {count} open pull request(s) in {repo} \n")
        total += count

    if total == 0:
        print("No open pull requests for this milestone \n")

    return total


def closed_other(closed_prs: dict, current_milestone: str) -> int:
    """
    Report on closed pull requests not at the current milestone.
    """

    print_banner(f"Checking for closed pull requests not for {current_milestone}")

    total = 0

    for milestone in closed_prs:
        if milestone == current_milestone:
            continue

        for repo in closed_prs[milestone]:
            print(f"{repo} \n{'-' * len(repo)}")
            for pr in closed_prs[milestone][repo]:
                print(f"#{pr['number'] : <5} {pr['title']}")

            count = len(closed_prs[milestone][repo])
            print(
                f"->   {count} closed pull request(s) in {repo} at milestone {milestone} \n"
            )
            total += count

    if total == 0:
        print("No closed pull requests not at this milestone \n")

    return total


def check_ready(open_prs: dict, closed_prs: dict, milestone: str) -> None:
    total_open = still_open(open_prs[milestone], milestone)
    total_other = closed_other(closed_prs, milestone)

    if total_open or total_other:
        print("=" * 50)
        print(
            f"{total_open} open pull request(s) in {milestone} and "
            f"{total_other} closed pull request(s) not in {milestone}."
        )
        # cont = input("Would you like to continue with closing this milestone? (y/n) ")
        #
        # if cont == "n":
        #     exit(0)
        # elif cont != "y":
        #     print("Unrecognised input, please select y or n")

def report(closed: dict, milestone: str) -> None:

    print_banner(f"Pull requests completed for {milestone}")

    for repo in closed:
        print(f"{repo: <20} {len(closed[repo]): >3} pull requests")


def archive(closed_prs: dict, dryrun: bool) -> None:

    print_banner("Archiving all completed pull requests for this milestone")

    #gh project item-archive [<number>] [flags]

    for repo in closed_prs:
        for pr in closed_prs[repo]:
            if dryrun:
                print(f"Archiving #{pr['number']} in {repo}")

def parse_args():
    """
    Read command line args
    """

    testfile = Path(__file__).parent / "test" / "test.json"

    parser = argparse.ArgumentParser(
        "Archive milestone contents within the project and close the "
        "milestone within each repository."
    )

    parser.add_argument("--milestone", help="Milestone to archive and close.")
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
    parser.add_argument(
        "--dry",
        action="store_true",
        help="Dry run. Print commands, don't action them. Always true when "
             "running with test data."
    )

    args = parser.parse_args()

    args.file = Path(args.file)
    args.file = args.file.expanduser().resolve()

    if args.test:
        args.dry = True

    return args


def main(milestone: str, test: bool, capture_project: bool, file: Path, dry: bool) -> None:

    if test:
        data = ProjectData.from_file(file)
    else:
        data = ProjectData.from_github(capture_project, file)

    open_prs_by_milestones = data.get_by_milestone("open")
    closed_prs_by_milestones = data.get_by_milestone("closed")

    check_ready(open_prs_by_milestones, closed_prs_by_milestones, milestone)

    report(closed_prs_by_milestones[milestone], milestone)

    archive(closed_prs_by_milestones[milestone], dry)

    print("pause")


if __name__ == "__main__":
    args = parse_args()
    main(args.milestone, args.test, args.capture_project, args.file, args.dry)
