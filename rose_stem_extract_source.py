#!/usr/bin/env python3
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
Clone sources for a rose-stem run for use with git bdiff module in scripts
"""

import os
from datetime import datetime
from pathlib import Path
from ast import literal_eval
from get_git_sources import clone_repo, clone_repo_mirror, sync_repo, run_command


def main():

    clone_loc = Path(os.environ["SOURCE_DIRECTORY"])

    run_command(f"mkdir -p {clone_loc}")

    dependencies = literal_eval(os.environ["DEPENDENCIES"])

    for dependency, values in dependencies.items():

        print(f"Extracting {dependency} at time {datetime.now()}")

        loc = clone_loc / dependency

        if ".git" in values["source"]:
            if os.environ["USE_MIRRORS"] == "True":
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
