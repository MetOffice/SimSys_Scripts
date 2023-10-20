#!/bin/bash

# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

#set -eu

github_url="https://github.com/MetOffice/SimSys_Scripts.git"
github_url="git@github.com:MetOffice/SimSys_Scripts.git"
git_branch="Lucy_testing_um_scripts"
git_branch="add_umdp3checker_script"
clone_destination="${CYLC_SUITE_SHARE_DIR}/imported_github_scripts"


echo "Git-scripts updater has started running"

rm -rf "${CYLC_SUITE_SHARE_DIR}/imported_github_scripts"
if [[ $? != 0 ]]; then
    echo "Couldn't remove specified folder. Try checking permissions"
    echo "  \"${CYLC_SUITE_SHARE_DIR}/imported_github_scripts\""
    exit 1
  else
    echo "Successfully removed old simulation-systems git directory"
    echo "  \"${CYLC_SUITE_SHARE_DIR}/imported_github_scripts\""
fi

# Mildly circular dependency here... The name of the branch needs changing -
# once the branch has been committed..
git clone -b $git_branch --single-branch $github_url "${CYLC_SUITE_SHARE_DIR}/imported_github_scripts" 2>&1
if [[ $? != 0 ]]; then
    echo "Unable to clone remote git repo into specified location."
    echo "Check git branch, git url, destination path, permissions and git access"
    echo "GitHub url       = \"$github_url\""
    echo "git branch       = \"$git_branch\""
    echo "destination path = \"$clone_destination\""
    exit 1
  else
    echo "Git repo successfully cloned"
fi

echo "Github scripts updated"
