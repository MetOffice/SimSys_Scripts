#!/usr/bin/env python3
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
Clone sources for a rose-stem run for use with git bdiff module in scripts
Only intended for use with rose-stem suites that have provided appropriate environment
variables
"""

import os
from datetime import datetime
from pathlib import Path
from ast import literal_eval
from get_git_sources import clone_repo, clone_repo_mirror, sync_repo
from typing import Dict


def set_https(dependencies: Dict) -> Dict:
    """
    Change sources in a dependencies dictions to use https instead of ssh
    """

    print("Modifying Dependencies")
    for dependency, values in dependencies.items():
        if values["source"].startswith("git@github.com:"):
            source = dependencies[dependency]["source"]
            dependencies[dependency]["source"] = source.replace(
                "git@github.com:", "https://github.com/"
            )

    return dependencies


def main() -> None:

    clone_loc = Path(os.environ["SOURCE_DIRECTORY"])

    dependencies: Dict = literal_eval(os.environ["DEPENDENCIES"])

    if os.environ.get("USE_TOKENS", "False") == "True":
        dependencies = set_https(dependencies)

    for dependency, values in dependencies.items():

        print(
            f"Extracting {dependency} at time {datetime.now()} "
            f"using source {values['source']} and ref {values['ref']}"
        )

        loc = clone_loc / dependency

        if ".git" in values["source"]:
            if os.environ.get("USE_MIRRORS", "False") == "True":
                mirror_loc = Path(os.environ["GIT_MIRROR_LOC"]) / values["parent"]
                clone_repo_mirror(
                    values["source"], values["ref"], values["parent"], mirror_loc, loc
                )
            else:
                clone_repo(values["source"], values["ref"], loc)
        else:
            sync_repo(values["source"], values["ref"], loc)


if __name__ == "__main__":
    main()
