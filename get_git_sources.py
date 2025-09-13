#!/usr/bin/env python3
"""
Clone sources for a rose-stem run for use with git bdiff module in scripts
"""

import os
import re
import subprocess
from datetime import datetime
from pathlib import Path
from ast import literal_eval


def run_command(command, shell=False, rval=False):
    """
    Run a subprocess command and return the result object
    Inputs:
        - command, str with command to run
    Outputs:
        - result object from subprocess.run
    """
    if not shell:
        command = command.split()
    result = subprocess.run(
        command,
        capture_output=True,
        text=True,
        timeout=300,
        shell=shell,
        check=False,
    )
    if result.returncode:
        print(result.stdout, end="\n\n\n")
        raise RuntimeError(
            f"[FAIL] Issue found running command {command}\n\n{result.stderr}"
        )
    if rval:
        return result


def clone_repo_mirror(values, loc):
    """
    Clone a repo source using a local git mirror.
    Assume the mirror is set up as per the Met Office
    """

    mirror_loc = Path(os.environ["GIT_MIRROR_LOC"]) / values["parent"]

    repo_ref = values["ref"]
    if not repo_ref:
        repo_ref = "HEAD"

    source = values["source"].removeprefix("git@github.com:")
    user = source.split("/")[0]
    # Check that the user is different to the Upstream User
    if user in values["parent"].split("/")[0]:
        user = None

    # If the ref is a hash then we don't need the fork user as part of the fetch.
    # Equally, if the user is the Upstream User, it's not needed
    if re.match(r"^\s*([0-9a-f]{40})\s*$", repo_ref) or not user:
        fetch = repo_ref
    else:
        fetch = f"{user}/{repo_ref}"
    commands = (
        f"git clone {mirror_loc} {loc}",
        f"git -C {loc} fetch origin {fetch}",
        f"git -C {loc} checkout FETCH_HEAD",
    )
    for command in commands:
        run_command(command)


def clone_repo(repo_source, repo_ref, loc):
    """
    Clone the repo and checkout the provided ref
    Only if a remote source
    """

    command = f"git clone {repo_source} {loc}"
    run_command(command)
    if repo_ref:
        command = f"git -C {loc} checkout {repo_ref}"
        run_command(command)


def sync_repo(repo_source, repo_ref, loc):
    """
    Rsync a local git clone and checkout the provided ref
    """

    # Trailing slash required for rsync
    command = f"rsync -av {repo_source}/ {loc}"
    run_command(command)
    if repo_ref:
        command = f"git -C {loc} checkout {repo_ref}"
        run_command(command)


def main():

    clone_loc = Path(os.environ["SOURCE_DIRECTORY"])

    dependencies = literal_eval(os.environ["DEPENDENCIES"])

    for dependency, values in dependencies.items():

        print(f"Extracting {dependency} at time {datetime.now()}")

        loc = clone_loc / dependency

        if ".git" in values["source"]:
            if os.environ["USE_MIRRORS"] == "True":
                clone_repo_mirror(values, loc)
            else:
                clone_repo(values["source"], values["ref"], loc)
        else:
            sync_repo(values["source"], values["ref"], loc)


if __name__ == "__main__":
    main()
