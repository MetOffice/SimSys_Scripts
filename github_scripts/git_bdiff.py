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


class GitBase:
    """
    Base class for gitbdiff functionality
    """

    # Match branch names.  This should catch all valid names but may
    # also some invalid names through.  This should matter given that
    # it is being used to match git command output.  For a complete
    # overview of the naming scheme, see man git check-ref-format
    _branch_pattern = re.compile(r"^\s*([^\s~\^\:\?\*\[]+[^.])\s*$")

    # Text returned if in a detached head
    detached_head_reference = "detched_head_state"

    def __init__(self, working_directory=None):
        if working_directory is None:
            self._WC_dir = None
        else:
            self._WC_dir = Path(working_directory)
            if not self._WC_dir.is_dir():
                raise GitBDiffError(f"{working_directory} is not a directory")

    def get_branch_name(self):
        """Get the name of the current branch."""
        result = None
        for line in self.run_git(self._WC_dir, ["branch", "--show-current"]):
            # Set m to self._branch_pattern result
            # Then check m evaluates to True
            if m := self._branch_pattern.match(line):
                result = m.group(1)
                break
        else:
            # Check for being in a Detached Head state
            for line in self.run_git(self._WC_dir, ["branch"]):
                if "HEAD detached" in line:
                    result = self.detached_head_reference
                    break
            else:
                raise GitBDiffError("unable to get branch name")
        return result

    @staticmethod
    def run_git(wc_path, args):
        """Run a git command and yield the output."""

        if not isinstance(args, list):
            raise TypeError("args must be a list")
        cmd = ["git"] + args

        # Run the the command in the working directory, capture the
        # output, and check for errors.  The build in error check is
        # not used to allow specific git errors to be treated more
        # precisely
        proc = subprocess.run(
            cmd, capture_output=True, check=False, shell=False, cwd=wc_path
        )

        for line in proc.stderr.decode("utf-8").split("\n"):
            if line.startswith("fatal: not a git repository"):
                raise GitBDiffNotGit(cmd)
            if line.startswith("fatal: "):
                raise GitBDiffError(line[7:])

        if proc.returncode != 0:
            raise GitBDiffError(f"command returned {proc.returncode}")

        yield from proc.stdout.decode("utf-8").split("\n")


class GitBDiff(GitBase):
    """Class which generates a branch diff."""

    # Name of primary branch - default is main
    primary_branch = "main"

    # Match hex commit IDs
    _hash_pattern = re.compile(r"^\s*([0-9a-f]{40})\s*$")

    def __init__(
        self,
        parent=None,
        working_directory=None,
        first_hash=None,
        second_hash=None,
        branch_name=None,
    ):
        self.parent = parent or self.primary_branch

        super().__init__(working_directory)

        self.first_hash = first_hash
        self.second_hash = second_hash
        self.branch = self.get_branch_name()
        if self.branch == self.detached_head_reference:
            raise GitBDiffError(
                "Can't get a diff for a working_directory in detached head state"
            )

    @classmethod
    def get_branch_point(cls, parent, working_directory=None):
        """Get the branch point from the parent repo.

        Find the commit which marks the point of divergence from the
        parent repository.  If there are no changes or this is the
        trunk, the branch point will be the same as the most recent
        commit.
        """
        result = None
        for line in cls.run_git(working_directory, ["merge-base", parent, "HEAD"]):
            if m := cls._hash_pattern.match(line):
                result = m.group(1)
                break
        else:
            raise GitBDiffError("branch point not found")
        return result

    @classmethod
    def get_latest_commit(cls, working_directory=None):
        """Get the last commit ID on the branch."""

        result = None
        for line in cls.run_git(
            working_directory, ["show", "--pretty=%H", "--no-patch"]
        ):
            if m := cls._hash_pattern.match(line):
                result = m.group(1)
                break
        else:
            raise GitBDiffError("current revision not found")
        return result

    @classmethod
    def branch_difference(cls, working_directory=None, parent=None):
        """Get the difference between the current branch and its parent."""
        parent = parent or cls.primary_branch
        ancestor = cls.get_branch_point(parent, working_directory=working_directory)
        current = cls.get_latest_commit(working_directory=working_directory)
        return cls(
            working_directory=working_directory,
            first_hash=ancestor,
            second_hash=current,
            branch_name=None,
        )

    @property
    def is_branch(self):
        """Whether this is a branch or main."""
        return self.branch != self.primary_branch

    @property
    def has_diverged(self):
        """Whether the branch has diverged from its parent."""
        return self.first_hash != self.second_hash

    def files(self):
        """Iterate over files changed on the branch."""

        for line in self.run_git(
            self._WC_dir, ["diff", "--name-only", "--diff-filter=AMX", self.first_hash]
        ):
            if line != "":
                yield line


class GitInfo(GitBase):
    """
    Class to contain info of a git repo
    """

    def __init__(self, working_directory=None):
        super().__init__(working_directory=working_directory)

        self.branch = self.get_branch_name()

    def is_main(self):
        """
        Returns true if branch matches a main-like branch name as defined below
        Count detached head as main-like as we cannot get a diff for this
        """

        main_like = ("main", "stable", "trunk", "master", self.detached_head_reference)
        return self.branch in main_like
