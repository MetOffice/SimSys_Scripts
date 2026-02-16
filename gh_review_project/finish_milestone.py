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


def count_items(data: dict, message: str):
    """
    For a dictionary of pull requests or issues keyed by repository, count
    per repo and return the total. Print details of items found.

    data: dict, lists of project items keyed by repository
    message: str, message to display
    """

    total = 0

    for repo in data:
        print(f"{repo} \n{'-'*len(repo)}")
        for item in data[repo]:
            print(f"{item.status: <18} #{item.number: <5} {item.title}")

        count = len(data[repo])
        print(f"->   {count} {message} in {repo} \n")
        total += count

    if total == 0:
        print(f"No {message} \n")

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
            print(f"Setting pull requests with no milestone to {current_milestone}")
            for repo in closed_prs[milestone]:
                for pr in closed_prs[milestone][repo]:
                    pr.modify_milestone(current_milestone, dry_run)

        else:
            if len(closed_prs[milestone]):
                total = count_items(closed_prs[milestone], "closed pull requests")

    return total


def check_ready(
    reviews: ProjectData, issues: ProjectData, milestone: str, dry_run: bool = False
) -> None:
    """
    Check if the milestone is ready to be closed by confirming that:
      * all pull requests for this milestone have been completed
      * all closed pull requests in the project are in this milestone.
      * all In Review issues for this milestone have been completed

    Give the user the choice to continue regardless since there may be valid
    exceptions.
    """
    print_banner(f"Checking for open pull requests for {milestone}")
    open_prs = reviews.get_milestone(milestone=milestone, status="open")
    total_open = count_items(open_prs, "open pull requests")

    closed_prs = reviews.get_all_milestones(status="closed")
    total_other = closed_other(closed_prs, milestone, dry_run)

    print_banner(f"Checking for issues in review for {milestone}")
    open_issues = issues.get_milestone(milestone=milestone, status="In Review")
    total_issues_in_review = count_items(open_issues, "In Review issues")

    if total_open or total_other or total_issues_in_review:
        print("=" * 50)
        print(
            f"{total_open} open pull request(s) in {milestone}. \n"
            f"{total_other} closed pull request(s) not in {milestone}. \n"
            f"{total_issues_in_review} issues in {milestone} with status In Review. "
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


def tidy_issues(data: ProjectData, milestone: str, dry_run: bool = False) -> None:
    # Check for uncompleted issues at milestone, remove milestone and leave a comment

    print_banner(f"Removing uncompleted issues from {milestone}")

    issues = data.get_milestone(milestone=milestone, status="open")
    comment = (
        f"[Automatic Update]\n\nThe {milestone} milestone is being closed. "
        f"Please review this issue and either select a new milestone or "
        f"close it as appropriate. Please contact @MetOffice/ssdteam if "
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

    # Process data and report on status
    check_ready(review_data, issue_data, milestone, dry)

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
