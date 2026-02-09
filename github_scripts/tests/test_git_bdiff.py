#!/usr/bin/env python3
# *********************************COPYRIGHT************************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *********************************COPYRIGHT************************************
"""
Test suite for git_bdiff module.
"""

import os
import subprocess
import pytest

from ..git_bdiff import GitBDiff, GitBDiffError, GitBDiffNotGit, GitInfo, GitBase


# Disable warnings caused by the use of pytest fixtures
# pylint: disable=redefined-outer-name


def add_to_repo(start, end, message, mode="wt"):
    """Add and commit dummy files to a repo."""

    for i in range(start, end):
        with open(f"file{i}", mode, encoding="utf-8") as fd:
            print(f"Lorem ipsum dolor sit amet {i}", file=fd)

    subprocess.run(["git", "add", "-A"], check=True)
    subprocess.run(
        [
            "git",
            "-c",
            "user.name='Testing'",
            "-c",
            "user.email='Testing'",
            "commit",
            "--no-gpg-sign",
            "-m",
            message,
        ],
        check=True,
    )


@pytest.fixture(scope="session")
def git_repo(tmpdir_factory):
    """Create and populate a test git repo."""

    location = tmpdir_factory.mktemp("data")
    os.chdir(location)

    # Create the repo and add some files
    subprocess.run(["git", "init"], check=True)
    add_to_repo(0, 10, "Testing")

    # Create a branch and add some files
    subprocess.run(["git", "checkout", "-b", "mybranch"], check=True)
    add_to_repo(20, 30, "Commit to mybranch")

    # Create a branch-of-branch and add more files
    subprocess.run(["git", "checkout", "-b", "subbranch"], check=True)
    add_to_repo(40, 50, "Commit to subbranch")

    # Create a branch from main without any changes
    subprocess.run(["git", "checkout", "main"], check=True)
    subprocess.run(["git", "checkout", "-b", "unchanged"], check=True)

    # Create a branch from main and overwrite some things
    subprocess.run(["git", "checkout", "main"], check=True)
    subprocess.run(["git", "checkout", "-b", "overwrite"], check=True)
    add_to_repo(0, 10, "Overwriting", "at")

    # Switch back to the main branch
    subprocess.run(["git", "checkout", "main"], check=True)

    # Add other trunk-like branches, finishing back in main
    for branch in ("stable", "master", "trunk"):
        subprocess.run(["git", "checkout", "-b", branch], check=True)
        subprocess.run(["git", "checkout", "main"], check=True)

    return location


def test_init(git_repo):
    """Test creation of a new GitBDiff instance"""

    os.chdir(git_repo)
    bdiff = GitBDiff()

    assert bdiff.branch is not None
    assert bdiff.branch == "main"
    assert not bdiff.is_branch
    assert not bdiff.has_diverged


def test_repo_selection(git_repo):
    """Test selection of repository directory."""

    os.chdir("/")
    bdiff = GitBDiff(repo=git_repo)

    assert bdiff.branch is not None
    assert bdiff.branch == "main"
    assert not bdiff.is_branch
    assert not bdiff.has_diverged


def test_invalid_repo_selection(git_repo):
    """Test non-existent repo or plain file raises an error"""

    with pytest.raises(GitBDiffError):
        GitBDiff(repo="/nosuch")

    with pytest.raises(GitBDiffError):
        GitBDiff(repo="/etc/hosts")


def test_branch_diff(git_repo):
    """Test a simple branch diff."""

    os.chdir(git_repo)
    subprocess.run(["git", "checkout", "mybranch"], check=True)

    try:
        bdiff = GitBDiff()
        changes = list(bdiff.files())
    finally:
        subprocess.run(["git", "checkout", "main"], check=True)

    assert bdiff.branch == "mybranch"
    assert bdiff.is_branch
    assert bdiff.has_diverged
    assert len(changes) == 10
    assert changes[0] == "file20"


def test_branch_of_branch_diff(git_repo):
    """Test a branch of branch diff.

    This effectively tests whether all the commits since the branch
    point with main are picked up correctly.
    """

    os.chdir(git_repo)
    subprocess.run(["git", "checkout", "subbranch"], check=True)

    try:
        bdiff = GitBDiff()
        changes = list(bdiff.files())
    finally:
        subprocess.run(["git", "checkout", "main"], check=True)

    assert bdiff.branch == "subbranch"
    assert bdiff.is_branch
    assert bdiff.has_diverged
    assert len(changes) == 20
    assert changes[0] == "file20"
    assert changes[-1] == "file49"


def test_overwritten_branch(git_repo):
    """Test a diff of a branch with changed files."""

    os.chdir(git_repo)
    subprocess.run(["git", "checkout", "overwrite"], check=True)
    try:
        bdiff = GitBDiff()
        changes = list(bdiff.files())
    finally:
        subprocess.run(["git", "checkout", "main"], check=True)

    assert bdiff.branch == "overwrite"
    assert bdiff.is_branch
    assert bdiff.has_diverged
    assert len(changes) == 10


def test_unchanged_branch(git_repo):
    """Test a branch with no commits."""

    os.chdir(git_repo)
    subprocess.run(["git", "checkout", "unchanged"], check=True)

    try:
        bdiff = GitBDiff()
        changes = list(bdiff.files())
    finally:
        subprocess.run(["git", "checkout", "main"], check=True)

    assert bdiff.branch == "unchanged"
    assert bdiff.is_branch
    assert not bdiff.has_diverged
    assert not changes


def test_non_repo(tmpdir):
    """Test exception if working directory is not a git repo."""

    os.chdir(tmpdir)

    with pytest.raises(GitBDiffNotGit) as exc:
        GitBDiff()
    assert "not a repository" in str(exc.value)


def test_nonexistent_parent(git_repo):
    """Test exception if parent branch does not exist.

    This is a proxy test for the detection of all sorts of git
    errors.
    """

    os.chdir(git_repo)

    with pytest.raises(GitBDiffError) as exc:
        GitBDiff(parent="nosuch")
    assert "Not a valid object name nosuch" in str(exc.value)


def test_git_run(git_repo):
    """Test git interface and error handling."""

    bdiff = GitBDiff()

    with pytest.raises(TypeError) as exc:
        # Use a string in place of a list
        list(i for i in bdiff.run_git("commit -m ''"))
    assert "args must be a list" in str(exc.value)

    with pytest.raises(GitBDiffError) as exc:
        # Run a command that should return non-zero
        list(i for i in bdiff.run_git(["commit", "-m", "''"]))
    assert "command returned 1" in str(exc.value)


def test_is_main(git_repo):
    """Test is_trunk function"""

    os.chdir(git_repo)

    for branch in ("stable", "master", "trunk", "main", "mybranch"):
        info = GitInfo()
        subprocess.run(["git", "checkout", branch], check=True)
        if branch == "my_branch":
            assert not info.is_main()
        else:
            assert info.is_main()


def find_previous_hash():
    """
    Loop over a git log output and extract a hash that isn't the current head
    """

    result = subprocess.run(["git", "log"], check=True, capture_output=True, text=True)
    for line in result.stdout.split("\n"):
        if line.startswith("commit") and "HEAD" not in line:
            return line.split()[1]


def test_detached_head(git_repo):
    """Test Detached Head State"""

    os.chdir(git_repo)
    subprocess.run(["git", "checkout", "main"], check=True)

    commit_hash = find_previous_hash()
    subprocess.run(["git", "checkout", commit_hash], check=True)

    git_base = GitBase()
    assert git_base.get_branch_name() == git_base.detached_head_reference
