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
from get_git_sources import get_source, merge_source, set_https


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
    if args.dependencies.name != "dependencies.yaml":
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

    with open(args.dependencies, "r") as stream:
        dependencies = yaml.safe_load(stream)

    if args.tokens:
        dependencies = set_https(dependencies)

    for dependency, opts in dependencies.items():
        dest = args.path / dependency

        if not isinstance(opts, list):
            opts = [opts]

        for i, values in enumerate(opts):
            if i == 0:
                get_source(
                    values["source"],
                    values["ref"],
                    dest,
                    dependency,
                    args.mirrors,
                    args.mirror_loc,
                )
                continue
            merge_source(
                values["source"],
                values["ref"],
                dependency,
                dest,
                args.mirrors,
                args.mirror_loc,
            )


if __name__ == "__main__":
    main()
