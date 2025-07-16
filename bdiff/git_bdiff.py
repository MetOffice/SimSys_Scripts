#!/usr/bin/env python3
# *********************************COPYRIGHT************************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *********************************COPYRIGHT************************************
"""
Module to obtain a list of all altered files on a git branch from
point where it diverged from the parent branch to the most recent
commit.

Usage is as follows:

>>> bdiff = GitBDiff()

And then:

>>> for change in bdiff.files():
...     print(change)
"""

import re
import subprocess
from pathlib import Path


class GitBDiffError(Exception):
    """Base bdiff error class."""


class GitBDiffNotGit(GitBDiffError):
    """Error if the target not part of a git repository."""

    def __init__(self, cmd):
        super().__init__(
            "not a repository (cmd:" + " ".join([str(i) for i in cmd]) + ")"
        )


class GitBDiff:
    """Class which generates a branch diff."""

    # Name of primary branch - default is main
    primary_branch = "main"

    # Match hex commit IDs
    _hash_pattern = re.compile(r"^\s*([0-9a-f]{40})\s*$")

    # Match branch names
    _branch_pattern = re.compile(r"^\s*(\S+)\s*$")

    def __init__(self, parent=None, repo=None):
        self.parent = parent or self.primary_branch

        if repo is None:
            self._repo = None
        else:
            self._repo = Path(repo)
            if not self._repo.is_dir():
                raise GitBDiffError(f"{repo} is not a directory")

        self.ancestor = self.get_branch_point()
        self.current = self.get_latest_commit()
        self.branch = self.get_branch_name()

    def get_branch_point(self):
        """Get the branch point from the parent repo.

        Find the commit which marks the point of divergence from the
        parent repository.  If there are no changes or this is the
        trunk, the branch point will be the same as the most recent
        commit.
        """

        result = None
        for line in self.run_git(["merge-base", self.parent, "HEAD"]):
            if m := self._hash_pattern.match(line):
                result = m.group(1)
                break
        else:
            raise GitBDiffError("branch point not found")
        return result

    def get_latest_commit(self):
        """Get the last commit ID on the branch."""

        result = None
        for line in self.run_git(["show", "--pretty=%H", "--no-patch"]):
            if m := self._hash_pattern.match(line):
                result = m.group(1)
                break
        else:
            raise GitBDiffError("current revision not found")
        return result

    def get_branch_name(self):
        """Get the name of the current branch."""
        result = None
        for line in self.run_git(["branch", "--show-current"]):
            if m := self._branch_pattern.match(line):
                result = m.group(1)
                break
        else:
            raise GitBDiffError("unable to get branch name")
        return result

    @property
    def is_branch(self):
        """Whether this is a branch or main."""
        return self.branch != self.primary_branch

    @property
    def has_diverged(self):
        """Whether the branch has diverged from its parent."""
        return self.ancestor != self.current

    def files(self):
        """Iterate over files changed on the branch."""

        for line in self.run_git(
            ["diff", "--name-only", "--diff-filter=AMX", self.ancestor]
        ):
            if line != "":
                yield line

    def run_git(self, args):
        """Run a git command and yield the output."""

        if not isinstance(args, list):
            raise TypeError("args must be a list")
        cmd = ["git"] + args

        # Run the the command in the repo directory, capture the
        # output, and check for errors.  The build in error check is
        # not used to allow specific git errors to be treated more
        # precisely
        proc = subprocess.run(
            cmd, capture_output=True, check=False, shell=False, cwd=self._repo
        )

        for line in proc.stderr.decode("utf-8").split("\n"):
            if line.startswith("fatal: not a git repository"):
                raise GitBDiffNotGit(cmd)
            if line.startswith("fatal: "):
                raise GitBDiffError(line[7:])

        if proc.returncode != 0:
            raise GitBDiffError(f"command returned {proc.returncode}")

        yield from proc.stdout.decode("utf-8").split("\n")
