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

from git_bdiff import GitBDiff, GitBDiffError, GitBDiffNotGit


# Disable warnings caused by the use of pytest fixtures
# pylint: disable=redefined-outer-name


def add_to_repo(start, end, message):
    """Add and commit dummy files to a repo."""

    for i in range(start, end):
        with open(f"file{i}", "wt", encoding="utf-8") as fd:
            print(f"Hello {i}", file=fd)

    subprocess.run(["git", "add", "-A"], check=True)
    subprocess.run(["git", "commit", "--no-gpg-sign", "-m", message], check=True)


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

    # Create an branch from main without any changes
    subprocess.run(["git", "checkout", "main"], check=True)
    subprocess.run(["git", "checkout", "-b", "unchanged"], check=True)

    # Switch back to the main branch ready for testing
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

    with pytest.raises(GitBDiffNotGit):
        GitBDiff()


def test_unknown_parent(git_repo):
    """Test exception if parent branch does not exist.

    This is a proxy test for the detection of all sorts of git
    errors.
    """

    os.chdir(git_repo)

    with pytest.raises(GitBDiffError):
        GitBDiff(parent="nosuch")
