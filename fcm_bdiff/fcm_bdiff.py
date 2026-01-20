#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------
"""
This module provides the functionality to return a list of local files to
run tests on based on the branch-difference (to allow checking of only files
which a developer has actually modified on their branch)
"""

import os
import re
import subprocess
import time
from pathlib import Path


# ------------------------------------------------------------------------------
class FCMError(Exception):
    """
    Exception class for FCM commands
    """

    def __str__(self):
        return '\nFCM command: "{0:s}"\nFailed with error: "{1:s}"'.format(
            " ".join(self.args[0]), self.args[1].strip()
        )


class FCMBase:
    """Class which generates a branch diff.

    This a modified (mangled) copy of the one Sam made in
    bdiff/git_bdiff.py, to allow current scripts to try and migrate to
    getting information from an instance of the same class.

    Note that the version for Git has a small handful of methods, mostly
    internal and some propeties. These are kept as close as possible to
    version in git_bdiff.py.
    
    Attributes used to navigate the horros of FCM and thus used in this
    package are therefore preceded with an '_' and shouldn't be what is
    being referred to outwith this class. Nor should the original
    'functions'...
    """

    # Name of primary branch - default is ~~main~~ Trunk,
    # Not sure this will be required/used. It's just the git version defines it.
    primary_branch = "trunk"

    def __init__(self, parent=None, repo=None):
        """
        The 'git' version of this gets to assume 'repo' is a directory,
        presumably containing a local 'clone' (of a fork of a repos). That
        is not how we have worked previously with FCM, to which you could
        give a path to a working copy, or a URL to a branch or the trunk on
        the remote server. So, much of the initial stages here replicate the
        kind of 'discovery' that was necessary for FCM that is hoped to
        become outdated with Git.
        """
        
        # use_mirror checks for SOURCE_UM_MIRROR env var, and if set
        # redirects the branch to that value and sets retries.
        # Otherwise it returns the branch unchanged and retries=0
        # Implies suite usage...
        # _branch is the URL of the branch which, after the call to use_mirror,
        # is the branch that was taken from the trunk (to avoid test branches etc)
        self._branch, self._retries = self.use_mirror(repo or Path("."))
        self._branch_info = self.get_branch_info(retries=self._retries)
        self._branch_url = self.get_url()
        self._parent = self.get_branch_parent()

        # The branch parent(ancestor in git_bdiff) should be the trunk(main); if it isn't assume this is a
        # branch-of-branch (a test branch), and redirect the request to point at
        # the parent branch
        while not self.is_trunk_test(self._parent):
            self._branch = self._parent
            self._branch_info = self.get_branch_info(retries=self._retries)
            self._branch_url = self.get_url()
            self._parent = self.get_branch_parent()

    def get_branch_name(self):
        """
        Get the branch name from the branch URL.
        Not sure how useful this will be in FCM world.
        For now, define it to be the contants of the URL after .*/main/ has been
        stripped off. i.e. it will start with trunk/... or branches/...
        """
        
        pattern = rf"{self.get_repository_root()}/main/(.*)$"
        match = re.match(pattern, self._branch_url)
        if match:
            result = match.group(1)
        else:
            raise FCMError("unable to get branch name")
        return result

    def run_fcm_command(self, command, max_retries, snooze):
        """
        Run an fcm command, optionally retrying on failure.
        """
        
        retries = 0
        while True:
            result = subprocess.run(
                command,
                capture_output=True,
                text=True,
                timeout=120,
                shell=False,
                check=False,
            )
            if result.returncode == 0:
                return result.stdout
            else:
                retries += 1
                if retries > max_retries:
                    raise FCMError(command, result.stderr)
                else:
                    time.sleep(snooze)

    def use_mirror(self, branch):
        """
        Catch to work out if this is running as part of a suite using an
        FCM mirror, if it is then redirect the request to the mirror.
        If using the mirror then fcm calls can sometimes fail so specify a number
        of retries for other routines to use.

        Returns updated branch URL and a number of retries
        """

        mirror_key = "SOURCE_UM_MIRROR"
        if mirror_key in os.environ:
            branch = os.environ[mirror_key]
            retries = 2
            print(f"[INFO] Switching branch used for fcm command to: {branch}")
        else:
            retries = 0
        return branch, retries

    def get_branch_info(self, snooze=300, retries=0):
        """
        Extract the output of the branch info command
        (if the branch is the mirror, allow for a few retries in case
        it hasn't picked up the latest commit yet)
        """

        command = ["fcm", "binfo", self._branch]
        branch_info = self.run_fcm_command(command, retries, snooze)
        return branch_info

    def get_branch_parent(self):
        """
        Given the raw output from an fcm binfo command - which can be retrieved by
        calling get_branch_info() - returns the Branch Parent Field
        """
        parent = re.search(
            r"^Branch Parent:\s*(?P<parent>.*)$",
            self._branch_info,
            flags=re.MULTILINE,
        )
        if parent:
            parent = parent.group("parent")
        else:
            # Will end up here if _branch is the trunk. In which case we shold possibly return _branch?
            parent = re.search(
                r"^URL:\s*(?P<parent>.*)$",
                self._branch_info,
                flags=re.MULTILINE,
            )
            if parent:
                parent = parent.group("parent")
            else:
                raise Exception("Could not find Branch Parent field")
        return parent

    def get_url(self):
        """
        Given the raw output from an fcm binfo command - which can be retrieved
        by calling get_branch_info() - returns the URL field
        """
        url = re.search(r"^URL:\s*(?P<url>.*)$", self._branch_info, flags=re.MULTILINE)
        if url:
            url = url.group("url")
        else:
            raise Exception("Could not find URL field")
        return url

    def is_trunk_test(self, url):
        """
        Given an FCM url, returns True if it appears to be pointing to the
        UM main trunk
        """
        search = re.search(
            r"""
                        (svn://fcm\d+/\w+_svn/\w+/trunk|
                        .*/svn/[\w\.]+/\w+/trunk|
                        ..*_svn/\w+/trunk)
                        """,
            url,
            flags=re.VERBOSE,
        )
        return search is not None

    def get_repository_root(self):
        """
        Given the raw output from an fcm binfo command - which can be retrieved by
        calling get_branch_info() - returns the Repository Root field
        """
        repos_root = re.search(
            r"^Repository Root:\s*(?P<url>.*)\s*$",
            self._branch_info,
            flags=re.MULTILINE,
        )
        if repos_root:
            repos_root = repos_root.group("url")
        else:
            raise Exception("Could not find Repository Root field")
        return repos_root

    def get_latest_commit(self):
        """
        Given the raw output from an fcm binfo command - which can be retrieved by
        calling get_branch_info() - returns the Last Changed Rev
        """
        repos_rev = re.search(
            r"^Last Changed Rev:\s*(?P<rev>.*)\s*$",
            self._branch_info,
            flags=re.MULTILINE,
        )
        if repos_rev:
            repos_rev = repos_rev.group("rev")
        else:
            raise Exception("Could not find Last Changed Rev field")
        return repos_rev


# --------------------------------------------------------------------
class FCMBDiff(FCMBase):
    """Class which generates a branch diff."""

    def __init__(self, parent=None, repo=None):
        super().__init__(parent, repo)
        self.parent = parent or self._parent
        self.ancestor = self.get_branch_parent()
        self.current = self.get_latest_commit()
        self.branch = self.get_branch_name()
        self.is_trunk = self.is_trunk_test(self._branch_url)
        self.is_branch = not self.is_trunk
        self.repos_root = self.get_repository_root()

    @property
    def has_diverged(self):
        """
        Whether the branch has diverged from its parent.
        Bit vague here, so we're going to check to see if 'parent' had
        an '@' in it denoting it's a branch of <something>
        """
        
        match = re.match(r".*@(\d+)$", self.parent)
        if match:
            return True
        else:
            return False

    def files(self):
        """Iterate over files changed on the branch."""
        
        dem_danged_files = self._get_files()
        for line in dem_danged_files:
            if line != "":
                yield line

    def _get_files(self, path_override=None):
        # The command `fcm bdiff --summarize <branch_name>` returns a different
        # format if the branch has been reversed off the trunk. The expected format
        # is svn://fcm1/um.xm_svn/main/trunk/rose-stem/bin/suite_report.py
        # but if it has been reversed then we get
        # svn://fcm1/um.xm_svn/main/branches/dev/USER/BRANCH_NAME/PATH
        # This results in an invalid path provided by relative_paths
        bdiff = self.get_bdiff_summarize(retries=self._retries)

        # Extract files from the bdiff that have been modified (M) or added (A).
        # Strip whitespace, and remove blank lines while turning the output into
        # a list of strings.
        bdiff_files = [x.strip() for x in bdiff.split("\n") if x.strip()]
        bdiff_files = [
            bfile.split()[1]
            for bfile in bdiff_files
            if bfile.split()[0].strip() == "M" or bfile.split()[0].strip() == "A"
        ]

        # Convert the file paths to be relative to the current URL; to do this
        # construct the base path of the trunk URL and compare it to the results
        # of the bdiff command above
        repos_root = self.repos_root
        relative_paths = [
            os.path.relpath(bfile, os.path.join(repos_root, "main", "trunk"))
            for bfile in bdiff_files
        ]

        # These relative paths can be joined to an appropriate base to complete
        # the filenames to return
        base_source_key = "SOURCE_UM_BASE"
        if path_override is not None:
            # Allows for 'user directed' path reconstruction.
            # Particularly useful in rose stem.
            base = path_override
            bdiff_files = [os.path.join(base, bfile) for bfile in relative_paths]
        elif base_source_key in os.environ:
            # If running as a suite, the base path to the working copy can be used
            # However, unless the suite task is running on a machine with the same
            # path to the working copy, the task can't really make much use of
            # this.
            base = os.environ[base_source_key]
            bdiff_files = [os.path.join(base, bfile) for bfile in relative_paths]
        else:
            # Otherwise stick to the original path/URL to the branch
            bdiff_files = [
                os.path.join(self._branch, bfile) for bfile in relative_paths
            ]

        return bdiff_files

    def get_bdiff_summarize(self, snooze=300, retries=0):
        """
        Extract the output of the branch diff command
        (if the branch is the mirror, allow for a few retries in case
        it hasn't picked up the latest commit yet)
        """
        
        command = ["fcm", "bdiff", "--summarize", self._branch]
        return self.run_fcm_command(command, retries, snooze)


class FCMInfo(FCMBase):
    """Class to hold FCM branch information. Mirroring the functionality
    in the git_bdiff.GitBranchInfo class."""

    def __init__(self, branch_info: str):
        super().__init__(self, repo=None)
        self.branch_name = self.get_branch_name()

    def is_main(self) -> bool:
        """Return True if the branch is the main trunk."""
        return self.is_trunk_test(self._branch_url)
