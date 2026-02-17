import argparse
from pathlib import Path
from review_project import ProjectData, ISSUE_ID


def remove_milestone(
    issue_data: ProjectData, milestone: str, dry_run: bool = False
) -> int:
    """
    Remove the milestone from all open issues that do not have any linked PRs
    attached to them. Leave a comment explaining why.
    """

    open_issues = issue_data.get_milestone(milestone=milestone, status="open")

    comment = (
        f"[Automatic Update]\n\nThe Code Review deadline for the {milestone} has "
        f"passed. As this issue does not have a linked pull request it has been "
        f"removed from the milestone. Please review this issue and either select "
        f"a new milestone or close it as appropriate. Please contact "
        f"@MetOffice/ssdteam if you think there has been an error.\n\n Thanks"
    )

    for repo in open_issues:
        print(f"\nRemoving issues in {repo}")
        for issue in open_issues[repo]:
            if not issue.linked_prs:
                issue.add_comment(comment, dry_run=dry_run)
                issue.modify_milestone(milestone=None, dry_run=dry_run)


def parse_args():
    """
    Read command line args
    """

    testfile_path = Path(__file__).parent / "test"

    parser = argparse.ArgumentParser(
        "Changes to the Simulation System projects required at the code"
        "review deadline."
    )

    parser.add_argument("--milestone", help="Milestone being released")
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
        issue_data = ProjectData.from_file(ISSUE_ID, file / "issue.json")
    else:
        issue_data = ProjectData.from_github(
            ISSUE_ID, capture_project, file / "issue.json"
        )

    remove_milestone(issue_data, milestone=milestone, dry_run=dry)


if __name__ == "__main__":
    args = parse_args()
    main(args.milestone, args.test, args.capture_project, args.file, args.dry)
