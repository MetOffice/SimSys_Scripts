# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
Clone sources for a rose-stem run for use with git bdiff module in scripts
"""

import re
import subprocess
from typing import Optional
from pathlib import Path
from shutil import rmtree
import shlex
import logging

logger = logging.getLogger(__name__)


def run_command(
    command: str,
    check: bool = True,
    capture: bool = True,
    timeout: int = 600
) -> Optional[subprocess.CompletedProcess]:
    """
    Run a subprocess command and return the result object
    Inputs:
        - command, str with command to run
    Outputs:
        - result object from subprocess.run
    """

    args = shlex.split(command)

    try:
        # Note: text=True and capture_output=True have high overhead
        # for large buffers. Use capture=False for fire-and-forget tasks.
        result = subprocess.run(
            args,
            capture_output=capture,
            text=capture,
            timeout=timeout,
            shell=False,
            check=False
        )
        if check and result.returncode != 0:
            err_msg = (result.stderr or "").strip()
            logger.error(f"[FAIL] Command failed: {command}\nError: {err_msg}")
            raise subprocess.CalledProcessError(
                result.returncode, args, output=result.stdout, stderr=result.stderr
            )
        return result

    except (subprocess.TimeoutExpired, FileNotFoundError) as e:
        logger.error(f"[FAIL] Execution error for '{args[0]}': {e}")
        raise


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
        f"git -C {loc} fetch origin main:main",
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

    exclude_dirs = []
    host, path = repo_source.split(":", 1)
    result = run_command(f"ssh {host} git -C {path} status --ignored -s")
    for ignore_file in result.stdout.split():
        ignore_file = ignore_file.strip()
        if not ignore_file.startswith("!!"):
            continue
        ignore_file = ignore_file.removeprefix("!!")
        ignore_file = ignore_file.strip()
        exclude_dirs.append(ignore_file)
    print(exclude_dirs)

    # Trailing slash required for rsync
    command = f"rsync -av {repo_source}/ {loc}"
    for item in exclude_dirs:
        command = f"{command} --exclude '{item}'"
    run_command(command)

    # Fetch the main branch from origin
    # Ignore errors - these are likely because the main branch already exists
    # Instead write them as warnings
    command = f"git -C {loc} fetch origin main:main"
    result = run_command(command, check=False)
    if result and result.returncode:
        print("Warning - fetching main from origin resulted in an error")
        print("This is likely due to the main branch already existing")
        print(f"Error message:\n\n{result.stderr}")

    if repo_ref:
        command = f"git -C {loc} checkout {repo_ref}"
        run_command(command)
