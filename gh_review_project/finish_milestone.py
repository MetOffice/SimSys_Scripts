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
from pathlib import Path
import argparse
from review_project import ProjectData, REVIEW_ID, ISSUE_ID


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
            print(f"{pr.status: <18} #{pr.number: <5} {pr.title}")

        count = len(open_prs[repo])
        print(f"->   {count} open pull request(s) in {repo} \n")
        total += count

    if total == 0:
        print(f"No open pull requests for {current_milestone} \n")

    return total


def closed_other(
    closed_prs: dict, current_milestone: str, dry_run: bool = False
) -> int:
    """
    Report on closed pull requests not at the current milestone.
    """

    print_banner(f"Checking for closed pull requests not for {current_milestone}")

    total = 0

    for milestone in closed_prs:
        if milestone == current_milestone:
            continue

        elif milestone == "None":
            print(f"Setting pull requests with no milestone " f"to {current_milestone}")
            for repo in closed_prs[milestone]:
                for pr in closed_prs[milestone][repo]:
                    pr.modify_milestone(current_milestone, dry_run)

        else:
            for repo in closed_prs[milestone]:
                print(f"{repo} \n{'-' * len(repo)}")
                for pr in closed_prs[milestone][repo]:
                    print(f"#{pr.number : <5} {pr.title}")

                count = len(closed_prs[milestone][repo])
                print(
                    f"->   {count} closed pull request(s) in {repo} at milestone {milestone} \n"
                )
                total += count

    return total


def check_ready(data: ProjectData, milestone: str, dry_run: bool = False) -> None:
    """
    Check if the milestone is ready to be closed by confirming that:
      * all pull requests for this milestone have been completed
      * all closed pull requests in the project are in this milestone.

    Give the user the choice to continue regardless since there may be valid
    exceptions.
    """
    open_prs = data.get_milestone(milestone=milestone, status="open")
    closed_prs = data.get_all_milestones(status="closed")
    total_open = still_open(open_prs, milestone)
    total_other = closed_other(closed_prs, milestone, dry_run)

    if total_open or total_other:
        print("=" * 50)
        print(
            f"{total_open} open pull request(s) in {milestone} and "
            f"{total_other} closed pull request(s) not in {milestone}."
        )
        cont = input("Would you like to continue with closing this milestone? (y/n) ")

        if cont == "n":
            exit(0)
        elif cont != "y":
            print("Unrecognised input, please select y or n")


def report(data: ProjectData, milestone: str) -> None:
    """
    Report on the pull requests completed in this milestone
    """

    print_banner(f"Pull requests completed for {milestone}")

    closed = data.get_milestone(milestone=milestone, status="closed")

    total = 0
    for repo in closed:
        count = len(closed[repo])
        total += count
        print(f"{repo: <20} {count: >3} pull requests")

    print(f"{total} pull requests completed in {milestone}")


def parse_args():
    """
    Read command line args
    """

    testfile_path = Path(__file__).parent / "test"

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
        default=testfile_path,
        help="Filepath to test data for either capturing the project status, "
        "or use as input data.",
    )
    parser.add_argument(
        "--dry",
        action="store_true",
        help="Dry run. Print commands, don't action them. Always true when "
        "running with test data.",
    )

    args = parser.parse_args()

    args.file = Path(args.file)
    args.file = args.file.expanduser().resolve()

    if args.test:
        args.dry = True

    return args


def main(
    milestone: str, test: bool, capture_project: bool, file: Path, dry: bool
) -> None:

    # Get milestone data
    if test:
        review_data = ProjectData.from_file(REVIEW_ID, file / "pr.json")
        issue_data = ProjectData.from_file(ISSUE_ID, file / "issue.json")
    else:
        review_data = ProjectData.from_github(REVIEW_ID, capture_project, file/ "pr.json")
        issue_data = ProjectData.from_github(ISSUE_ID, capture_project, file/ "issue.json")

    # Process data and report on status
    check_ready(review_data, milestone, dry)
    report(review_data, milestone)

    # Archive pull requests at the milestone
    print_banner(f"Archiving Milestone {milestone}")
    review_data.archive_milestone(milestone, dry_run=dry)

    # Close milestones
    # TODO: run this command from here, rather than prompting user. Leaving
    #  like this until script feels stable.
    print_banner("Milestone Completed in Simulation Systems Review Tracker project")
    print("Run this command to close the milestone in all repositories:")
    print(f'./sbin/gh_manage_milestones -t "{milestone}" -m close')


if __name__ == "__main__":
    args = parse_args()
    main(args.milestone, args.test, args.capture_project, args.file, args.dry)
