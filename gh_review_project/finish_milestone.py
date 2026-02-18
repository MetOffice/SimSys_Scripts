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


def closed_other(
    reviews: ProjectData, current_milestone: str, dry_run: bool = False
) -> None:
    """
    Set a milestone for closed PRs without one.

    reviews: ProjectData from the Review Tracker Project
    current_milestone: Milestone being closed
    dry_run: If true, do not actually modify the milestone
    """

    print_banner(f"Setting pull requests with no milestone to {current_milestone}")

    closed_prs = reviews.get_milestone(milestone="None", status="closed")

    for repo in closed_prs:
        for pr in closed_prs[repo]:
            pr.modify_milestone(current_milestone, dry_run)


def check_ready(
    reviews: ProjectData, issues: ProjectData, current_milestone: str
) -> None:
    """
    Check if the milestone is ready to be closed by confirming that:
      * all pull requests for this milestone have been completed
      * all closed pull requests in the project are in this milestone.
      * all In Review issues for this milestone have been completed

    Give the user the choice to continue regardless since there may be valid
    exceptions.
    """
    print_banner(f"Checking for open pull requests for {current_milestone}")
    total_open = reviews.count_items(
        milestone=current_milestone, status="open", message="open pull requests"
    )
    if total_open == 0:
        print("No open pull requests\n")

    print_banner(f"Checking for issues in review for {current_milestone}")
    total_issues_in_review = issues.count_items(
        milestone=current_milestone, status="In Review", message="In Review issues"
    )
    if total_issues_in_review == 0:
        print("No issues in review\n")

    print_banner(f"Checking for closed pull requests not set to {current_milestone}")
    total_other = 0
    for milestone in reviews.milestones:
        if milestone == current_milestone:
            continue
        else:
            total_other += reviews.count_items(
                milestone=milestone, status="closed", message="closed pull requests"
            )
    if total_other == 0:
        print("All closed pull requests are in this milestone\n")

    if total_open or total_other or total_issues_in_review:
        print("=" * 50)
        print(
            f"{total_open} open pull request(s) in {current_milestone}. \n"
            f"{total_other} closed pull request(s) not in {current_milestone}. \n"
            f"{total_issues_in_review} issues in {current_milestone} with status In Review. "
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


def tidy_issues(issue_data: ProjectData, milestone: str, dry_run: bool = False) -> None:
    """
    Remove any outstanding open issues from the current milestone.
    """

    print_banner(f"Removing uncompleted issues from {milestone}")

    issues = issue_data.get_milestone(milestone=milestone, status="open")
    comment = (
        f"[Bulk Update]\n\nThe {milestone} milestone is being closed. "
        f"Please review this issue and either select a new milestone or "
        f"close it as appropriate. Please contact MetOffice/ssdteam if "
        f"you think there has been an error.\n\n Thanks"
    )
    for repo in issues:
        for issue in issues[repo]:
            issue.add_comment(comment, dry_run=dry_run)
            issue.modify_milestone(milestone=None, dry_run=dry_run)


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
        review_data = ProjectData.from_github(
            REVIEW_ID, capture_project, file / "pr.json"
        )
        issue_data = ProjectData.from_github(
            ISSUE_ID, capture_project, file / "issue.json"
        )

    # Set a milestone on closed PRs
    closed_other(review_data, milestone, dry)

    # Process data and report on status
    check_ready(review_data, issue_data, milestone)

    # Tidy outstanding issues
    tidy_issues(issue_data, milestone, dry)

    # Archive pull requests at the milestone
    print_banner(f"Archiving Milestone {milestone}")
    review_data.archive_milestone(milestone, dry_run=dry)

    # Print report as final step so its visible
    report(review_data, milestone)

    # Close milestones
    # TODO: run this command from here, rather than prompting user. Leaving
    #  like this until script feels stable.
    print_banner("Milestone Completed in Simulation Systems Review Tracker project")
    print("Run this command to close the milestone in all repositories:")
    print(f'./sbin/gh_manage_milestones -t "{milestone}" -m close')


if __name__ == "__main__":
    args = parse_args()
    main(args.milestone, args.test, args.capture_project, args.file, args.dry)
