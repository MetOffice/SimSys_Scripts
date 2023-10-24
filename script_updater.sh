#!/bin/bash

# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

#set -eu

git_repos="SimSys_Scripts.git"
github_url="git@github.com:MetOffice/$git_repos"
git_branch="add_umdp3checker_script"
#git_branch="main"
clone_destination="${CYLC_SUITE_SHARE_DIR}/imported_github_scripts"


echo "Git-scripts updater has started running"

rm -rf "${CYLC_SUITE_SHARE_DIR}/imported_github_scripts"
if [[ $? != 0 ]]; then
    echo "Couldn't remove specified folder. Try checking permissions"
    echo "  \"$clone_destination\""
    exit 1
  else
    echo "Successfully removed old ${git_repos%.git} git directory"
    echo "  \"$clone_destination\""
fi

# Mildly circular dependency here... The name of the branch needs changing -
# once ths branch has been committed.. See commented out 'branch' name above.
git clone -b $git_branch --single-branch $github_url "$clone_destination" 2>&1
if [[ $? != 0 ]]; then
    echo -e "\nUnable to clone remote git repo into specified location."
    echo "  Check git branch, git url, git access as well as"
    echo -e "  destination path, and permissions\n"
    echo "GitHub url       = \"$github_url\""
    echo "git branch       = \"$git_branch\""
    echo "destination path = \"$clone_destination\""
    exit 1
  else
    echo -e "\nGit repo successfully cloned\n"
fi

echo -e "\nGithub scripts updated\n"
