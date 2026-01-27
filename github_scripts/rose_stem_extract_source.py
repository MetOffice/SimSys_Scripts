#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
Clone sources for a rose-stem run for use with git bdiff module in scripts
Only intended for use with rose-stem suites that have provided appropriate
environment variables
"""

import os
from pathlib import Path
from ast import literal_eval
from get_git_sources import clone_repo, clone_repo_mirror, sync_repo
from datetime import datetime


def set_https(dependencies: dict) -> dict:
    """
    Change sources in a dependencies dictions to use https instead of ssh
    """

    print("Modifying Dependencies")
    for dependency, opts in dependencies.items():
        if not isinstance(opts, list):
            opts = [opts]
        for values in opts:
            if values["source"].startswith("git@github.com:"):
                source = dependencies[dependency]["source"]
                dependencies[dependency]["source"] = source.replace(
                    "git@github.com:", "https://github.com/"
                )

    return dependencies


def main() -> None:
    """
    1. Read environment variables for:
        SOURCE_DIRECTORY - location to clone sources,
        DEPENDENCIES - dictionary of dependencies,
        USE_TOKENS - whether to use tokens for https URLs,
        USE_MIRRORS - whether to use local git mirrors,
        GIT_MIRROR_LOC - location of local git mirrors
    2. For each dependency in DEPENDENCIES, clone or sync the source
    3. If USE_TOKENS is True, modify the source URLs to use https
    4. If USE_MIRRORS is True, clone from local mirrors at GIT_MIRROR_LOC
    """

    clone_loc = Path(os.environ["SOURCE_DIRECTORY"])
    dependencies: dict = literal_eval(os.environ["DEPENDENCIES"])

    if os.environ.get("USE_TOKENS", "False") == "True":
        dependencies = set_https(dependencies)

    for dependency, opts in dependencies.items():
        loc = clone_loc / dependency

        if not isinstance(opts, list):
            opts = [opts]

        for values in opts:
            if ".git" in values["source"]:
                if os.environ.get("USE_MIRRORS", "False") == "True":
                    mirror_loc = Path(os.environ["GIT_MIRROR_LOC"]) / values["parent"]
                    print(
                        f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] Cloning "
                        f"{dependency} from {mirror_loc} at ref {values['ref']}"
                    )
                    clone_repo_mirror(
                        values["source"],
                        values["ref"],
                        values["parent"],
                        mirror_loc,
                        loc,
                    )
                else:
                    print(
                        f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] Cloning "
                        f"{dependency} from {values['source']} at ref {values['ref']}"
                    )
                    clone_repo(values["source"], values["ref"], loc)
            else:
                print(
                    f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] Syncing "
                    f"{dependency} at ref {values['ref']}"
                )
                sync_repo(values["source"], values["ref"], loc)


if __name__ == "__main__":
    main()
