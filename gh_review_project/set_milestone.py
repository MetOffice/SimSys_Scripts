# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
This script will change the milestone on all closed pull requests that don't
have one.
"""

import argparse
from pathlib import Path
from review_project import ProjectData, REVIEW_ID


def print_banner(message: str) -> None:
    print("\n")
    print("=" * len(message))
    print(message)
    print("=" * len(message))


def add_milestone(
    reviews: ProjectData, current_milestone: str, dry_run: bool = False
) -> None:
    """
    Set a milestone for closed PRs without one.

    reviews: ProjectData from the Review Tracker Project
    current_milestone: Milestone to set
    dry_run: If true, do not actually modify the milestone
    """

    print_banner(
        f"Setting closed pull requests with no milestone to {current_milestone}"
    )

    closed_prs = reviews.get_milestone(milestone="None", status="closed")

    if closed_prs:
        for repo in closed_prs:
            for pr in closed_prs[repo]:
                pr.modify_milestone(current_milestone, dry_run)
    else:
        print("No closed pull requests without a milestone.")


def parse_args():
    """
    Read command line args
    """

    testfile_path = Path(__file__).parent / "test"

    parser = argparse.ArgumentParser(
        "Set all closed pull requests without a milestone to the requested milestone."
    )

    parser.add_argument("--milestone", help="Milestone to set")
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
    else:
        review_data = ProjectData.from_github(
            REVIEW_ID, capture_project, file / "pr.json"
        )

    add_milestone(review_data, milestone, dry)


if __name__ == "__main__":
    args = parse_args()
    main(args.milestone, args.test, args.capture_project, args.file, args.dry)
