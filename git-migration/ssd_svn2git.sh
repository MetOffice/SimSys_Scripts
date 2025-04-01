#!/bin/bash --login
set -euo pipefail
# This script
# - Reads config file
# - For each repository
#   - Convert svn trunk to git repository in WORKDIR
#   - Attach trunk release tags to git repository
#   - Create remote repository (github.com/MetOffice/REPO_NAME), if necessary
#   - Push local branch to remote
#   - Push tags to remote

usage(){
  printf "%-s\n" "Usage: ${0##*/} [OPTION]

  ${0##*/} Migrates the Met Office Science Repositories to GitHub.

  Options:
    -h, --help              Display this help and exit
    -d, --debug             Enable command trace (debug mode)
    -u, --update            Update GitHub repository.
                            Please let the admin deal with this option.
    -c, --config[=VALUE]    Path to config.json file (default: config.json)
    -w, --workdir[=VALUE]   Working directory (default: ${DATADIR}/git-migration)

  Examples:
    ${0##*/} -c config.json -w ${DATADIR}/git-migration
    ${0##*/} --config=config.json --workdir=${DATADIR}/git-migration

  Report bugs to <yaswant.pradhan@metoffice.gov.uk>" | sed 's/^  //g'
}

OPTS=$(getopt \
    -o hduc:w: \
    -l help,debug,update,config:,workdir: \
    -n "${0##*/}" -- "$@") || { usage; exit 1; }
eval set -- "$OPTS"
# ----------------------------------------------------------------------------
SCRIPT_DIR=$(dirname -- "$(readlink -fe -- "$0")")
export CONFIG_FILE="${SCRIPT_DIR}/config.json"
export WORKDIR="${DATADIR}/git-migration"
UPDATE_GITHUB=0
# ----------------------------------------------------------------------------
while true; do
  case "$1" in
    -h |--help) usage; exit 0 ;;
    -d |--debug) PS4='L${LINENO}: '; set -x; shift 1 ;;
    -u |--update) UPDATE_GITHUB=1; shift 1 ;;
    -c |--config) CONFIG_FILE=$(readlink -fe "$2"); shift 2 ;;
    -w |--workdir) WORKDIR=$(readlink -fe "$2"); shift 2 ;;
    --)  shift; break ;;
    *)   break ;;
  esac
done
# ----------------------------------------------------------------------------

check_apps() {
    status=0
    echo "--------------------------------------------------------------------"
    for i in "$@"; do
        if ! command -V "$i" 2> /dev/null; then
            echo "** $i is not installed. Please install $i to run this script."
            ((status++))
        fi
    done
    echo "--------------------------------------------------------------------"
    if (( status > 0 )); then return 127; fi
}

# -- Create working directory
mkdir -p "$WORKDIR"
pushd "$WORKDIR"

# -- Add gitlify to PATH
git -C gitlify pull 2>/dev/null || gh repo clone MetOffice/gitlify
export PATH="$PATH:${WORKDIR}/gitlify"

# -- Display some diagnostics
check_apps gitlify jq gh
echo "WORKDIR: $WORKDIR
CONFIG_FILE: $CONFIG_FILE
UPDATE_GITHUB: $UPDATE_GITHUB
--------------------------------------------------------------------"

# -- Iterate through each repository and its tags
SVN_PREFIX=$(jq -r '.svn_prefix' "$CONFIG_FILE")
GH_ORG=$(jq -r '.gh_org' "$CONFIG_FILE")
GH_PREFIX="git@github.com:${GH_ORG}"
jq -c '.repo[]' "$CONFIG_FILE" | while read -r repo; do
    SECONDS=0  # reset timer
    # -- Extract git repository name from configuration file
    repo_name=$(jq -r '.name' <<< "$repo")
    svn_url=${SVN_PREFIX}/$(jq -r '.trunk' <<< "$repo")
    echo "$(date +'%F %T') Processing: $repo_name"
    {
        printf "%-.65s [%s]\n" \
            "$(date +'%F %T') Repository: $svn_url ........................." \
            "$repo_name"

        if [[ -d ${repo_name} ]]; then
            echo "-- Sync commits from head of trunk"
            pushd "${repo_name}" >/dev/null
            gitlify resync -t
            popd >/dev/null
        else
            echo "-- Convert svn to git"
            gitlify migrate -d enquiries@metoffice.gov.uk \
                -x -t -y "${svn_url}" "${repo_name}"
        fi

        pushd "${repo_name}" >/dev/null
        echo "-- Attach tags"
        tags=$(echo "$repo" | jq -c '.tags[]')
        for tag in $tags; do
            tag_name=$(echo "$tag" | jq -r 'keys[]')
            tag_value=$(echo "$tag" | jq -r '.[keys[]]')

            echo "  Tag: $tag_name, Revision: $tag_value"
            git tag "$tag_name" -f \
                "$(git svn find-rev "r${tag_value}")"
        done

        if (( UPDATE_GITHUB == 0 )); then
            deltaT=$(TZ=UTC0 printf '%(%H:%M:%S)T\n' $SECONDS)
            echo "$(date +'%F %T') Done: $repo_name in $deltaT"
            popd >/dev/null
            continue
        fi

        # -- Update GitHub repository
        echo "-- Create remote GitHub repository, if necessary"
        git ls-remote -h "${GH_PREFIX}/${repo_name}" || {
            DESCRIPTION=$(jq -r '.description' <<< "$repo")
            gh repo create "${GH_ORG}/${repo_name}" \
                --description="${DESCRIPTION}" \
                --private \
                --source=.
        }

        echo "-- Push local branch to GitHub"
        git config --get remote.origin.url || {
            git remote add origin "${GH_PREFIX}/${repo_name}.git"
        }
        git push --set-upstream origin trunk

        echo "-- Push tags to GitHub"
        # remove tags already created in remote?
        # git tag -l | xargs -n 1 git push --delete origin
        git push --tags

        deltaT=$(TZ=UTC0 printf '%(%H:%M:%S)T\n' $SECONDS)
        echo "$(date +'%F %T') Done: $repo_name in $deltaT"
        popd >/dev/null
    } > "${WORKDIR}/$(date +'%Y%m%dT%H%M%S')_${repo_name}.log" 2>&1
    # output to console and log file?
    # &> >(tee -a "${WORKDIR}/$(date +'%Y%m%dT%H%M%S')_${repo_name}.log")

done
echo "$(date +'%F %T') Done."
