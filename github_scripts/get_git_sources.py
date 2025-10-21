# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
Clone sources for a rose-stem run for use with git bdiff module in scripts
"""

import re
import subprocess
from typing import Optional
from pathlib import Path
from shutil import rmtree


def run_command(
    command: str, rval: bool = False
) -> Optional[subprocess.CompletedProcess]:
    """
    Run a subprocess command and return the result object
    Inputs:
        - command, str with command to run
    Outputs:
        - result object from subprocess.run
    """
    command = command.split()
    result = subprocess.run(
        command,
        capture_output=True,
        text=True,
        timeout=300,
        shell=False,
        check=False,
    )
    if result.returncode:
        print(result.stdout, end="\n\n\n")
        raise RuntimeError(
            f"[FAIL] Issue found running command {command}\n\n{result.stderr}"
        )
    if rval:
        return result


def clone_repo_mirror(
    source: str, repo_ref: str, parent: str, mirror_loc: Path, loc: Path
) -> None:
    """
    Clone a repo source using a local git mirror.
    Assume the mirror is set up as per the Met Office
    """

    # Remove if this clone already exists
    if loc.exists():
        rmtree(loc)

    command = f"git clone {mirror_loc} {loc}"
    run_command(command)

    # If not provided a ref, return
    if not repo_ref:
        return

    source = source.removeprefix("git@github.com:")
    user = source.split("/")[0]
    # Check that the user is different to the Upstream User
    if user in parent.split("/")[0]:
        user = None

    # If the ref is a hash then we don't need the fork user as part of the fetch.
    # Equally, if the user is the Upstream User, it's not needed
    if not user or re.match(r"^\s*([0-9a-f]{40})\s*$", repo_ref):
        fetch = repo_ref
    else:
        fetch = f"{user}/{repo_ref}"
    commands = (
        f"git -C {loc} fetch origin {fetch}",
        f"git -C {loc} checkout FETCH_HEAD",
    )
    for command in commands:
        run_command(command)


def clone_repo(repo_source: str, repo_ref: str, loc: Path) -> None:
    """
    Clone the repo and checkout the provided ref
    Only if a remote source
    """

    # Remove if this clone already exists
    if loc.exists():
        rmtree(loc)

    # Create a clean clone location
    loc.mkdir(parents=True)

    commands = (
        f"git -C {loc} init",
        f"git -C {loc} remote add origin {repo_source}",
        f"git -C {loc} fetch origin {repo_ref}",
        f"git -C {loc} checkout FETCH_HEAD",
    )
    for command in commands:
        run_command(command)


def sync_repo(repo_source: str, repo_ref: str, loc: Path) -> None:
    """
    Rsync a local git clone and checkout the provided ref
    """

    # Remove if this clone already exists
    if loc.exists():
        rmtree(loc)

    # Create a clean clone location
    loc.mkdir(parents=True)

    # Trailing slash required for rsync
    command = f"rsync -av {repo_source}/ {loc}"
    run_command(command)
    if repo_ref:
        command = f"git -C {loc} checkout {repo_ref}"
        run_command(command)
