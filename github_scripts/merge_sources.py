#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------
"""
Script to clone and merge git sources
"""

import argparse
import os
import yaml
from pathlib import Path
from get_git_sources import clone_and_merge, set_https, validate_dependencies
import logging


def parse_args():
    """
    Parse arguments
    """

    parser = argparse.ArgumentParser(description="Extract and merge git sources")
    parser.add_argument(
        "-d",
        "--dependencies",
        default=Path(__file__).parent,
        type=Path,
        help="Path to the dependencies.yaml file",
    )
    parser.add_argument(
        "-p",
        "--path",
        default=None,
        help="The path to extract the sources to. If part of a cylc suite, it will "
        "default to $CYLC_WORKFLOW_SHARE_DIR/source, otherwise __file__/source",
    )
    parser.add_argument(
        "-m",
        "--mirrors",
        action="store_true",
        help="If true, attempts to use local git mirrors",
    )
    parser.add_argument(
        "--mirror_loc",
        default="/data/users/gitassist/git_mirrors",
        help="Location of github mirrors",
    )
    parser.add_argument(
        "--tokens",
        action="store_true",
        help="If true, https github sources will be used, requiring github "
        "authentication via Personal Access Tokens",
    )
    args = parser.parse_args()
    args.dependencies = args.dependencies.resolve()
    if args.dependencies.is_dir():
        args.dependencies = args.dependencies / "dependencies.yaml"

    if not args.path:
        args.path = Path(os.getenv("CYLC_WORKFLOW_SHARE_DIR", __file__)) / "source"
    args.path = args.path.resolve()

    return args


def main():
    """
    Main Function
    """

    args = parse_args()

    logging.basicConfig(level=logging.INFO)

    dependencies = yaml.safe_load(args.dependencies.read_text())
    validate_dependencies(dependencies)

    if args.tokens:
        dependencies = set_https(dependencies)

    for dependency, sources in dependencies.items():
        dest = args.path / dependency
        clone_and_merge(dependency, sources, dest, args.mirrors, args.mirror_loc)


if __name__ == "__main__":
    main()
