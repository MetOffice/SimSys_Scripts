#!/usr/bin/env bash

# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

# set -eu
# Note (yp): if this is a standalone script, we should use set -e
# and avoid the || { exit $?; } construct below.

git_repos="SimSys_Scripts.git"
github_url="git@github.com:MetOffice/$git_repos"
git_branch="main"
clone_destination="${CYLC_SUITE_SHARE_DIR}/imported_github_scripts"

echo "Remove ${clone_destination} if it exists"
rm -rf "$clone_destination" 1>/dev/null || { exit $?; }

echo "Update ${git_repos%.git} in ${clone_destination}"
set -x
git clone -q -b $git_branch --single-branch $github_url "$clone_destination" \
  || { exit $?; }
{ set +x; } 2>/dev/null
