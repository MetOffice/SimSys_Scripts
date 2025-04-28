#!/bin/bash --login
set -euo pipefail

# ----------------------------------------------------------------------------
# Script Metadata
# ----------------------------------------------------------------------------
# This script is designed to migrates Met Office Simulation Systems SVN
# repositories to GitHub.

# ----------------------------------------------------------------------------
# Functions
# ----------------------------------------------------------------------------

usage() {
  cat <<EOF
Usage: ${0##*/} [OPTION]

${0##*/} Migrates the Met Office Science Repositories to GitHub.

Options:
  -h, --help               Display this help and exit
  -d, --debug              Enable command trace (debug mode)
  -m, --from-mirror        Use the Met Office internal mirror as SVN repository.
                           Useful when SRS cannot be accessible (e.g., in batch jobs).
  -n, --list-repos         List repositories to be migrated (dry run)
  -u, --update             Update GitHub repository.
                           Please let the admin deal with this option.
  -j, --parallel[=VALUE]   Number of repositories to process in parallel (default: number of CPU cores)
  -r, --repository[=VALUE] Name of a single repository to migrate
                           (default: all repositories defined in config.json)
  -c, --config[=VALUE]     Path to config.json file (default: config.json in the parent directory)
  -w, --workdir[=VALUE]    Working directory (default: ${DATADIR}/git-migration)

Examples:
  ${0##*/} -c /path/to/config.json -w ${DATADIR}/git-migration
  ${0##*/} --config=/path/to/config.json --workdir=${DATADIR}/git-migration

Report bugs to <yaswant.pradhan@metoffice.gov.uk>
EOF
}

check_apps() {
  local status=0
  echo "--------------------------------------------------------------------"
  for app in "$@"; do
    if ! command -V "$app" 2>/dev/null; then
      echo "** $app is not installed. Please install $app to run this script."
      ((status++))
    fi
  done
  echo "--------------------------------------------------------------------"
  if ((status > 0)); then return 127; fi
}

validate_repo() {
  local repo_name=$1
  if ! jq -e ".repo[] | select(.name == \"$repo_name\")" "$CONFIG_FILE" >/dev/null; then
    echo "Repository: '$repo_name' not found in $CONFIG_FILE"
    exit 1
  fi
}

process_repo() {
  local repo_name=$1
  local svn_url=$2
  local repo_metadata=$3
  SECONDS=0  # start timer

  echo "$(date +'%F %T') Processing: $repo_name"

  if [[ -d $repo_name ]]; then
    echo "-- Sync commits from head of trunk"
    pushd "$repo_name" >/dev/null
    gitlify resync -t
    popd >/dev/null
  else
    echo "-- Convert SVN to Git"
    gitlify migrate -d enquiries@metoffice.gov.uk -x -t -y "$svn_url" "$repo_name"
  fi

  pushd "$repo_name" >/dev/null
  attach_tags "$repo_metadata"
  update_github "$repo_name" "$repo_metadata"
  deltaT=$(TZ=UTC0 printf '%(%H:%M:%S)T\n' $SECONDS)
  echo "$(date +'%F %T') Done: $repo_name in $deltaT"
  popd >/dev/null
}

attach_tags() {
  local repo_metadata=$1
  echo "-- Attach tags"
  local tags
  tags=$(echo "$repo_metadata" | jq -c '.tags[]')
  for tag in $tags; do
    local tag_name tag_value
    tag_name=$(echo "$tag" | jq -r 'keys[]')
    tag_value=$(echo "$tag" | jq -r '.[keys[]]')
    echo "  Tag: $tag_name, Revision: $tag_value"
    git tag "$tag_name" -f "$(git svn find-rev "r${tag_value}")"
  done
}

update_github() {
  local repo_name=$1
  local repo_metadata=$2

  if ((UPDATE_GITHUB == 0)); then
    return
  fi

  echo "-- Create remote GitHub repository, if necessary"
  git ls-remote -h "${GH_PREFIX}/${repo_name}" || {
    local description
    description=$(echo "$repo_metadata" | jq -r '.description')
    gh repo create "${GH_ORG}/${repo_name}" --description="$description" --private --source=.
  }

  echo "-- Push local branch to GitHub"
  git config --get remote.origin.url || git remote add origin "${GH_PREFIX}/${repo_name}.git"
  git push --set-upstream origin trunk

  echo "-- Push tags to GitHub"
  git push --tags
}

initialize_environment() {
  mkdir -p "${WORKDIR}/logs"
  pushd "$WORKDIR"

  git -C gitlify pull 2>/dev/null || gh repo clone MetOffice/gitlify
  export PATH="$PATH:${WORKDIR}/gitlify"

  check_apps gitlify jq gh
  printf "WORKDIR: %s\nCONFIG_FILE: %s\nUPDATE_GITHUB: %s\n" \
        "$WORKDIR" "$CONFIG_FILE" "$UPDATE_GITHUB"
  echo "--------------------------------------------------------------------"
}

process_repositories() {
  local process_ids=()
  local max_jobs
  max_jobs=${BATCH:-$(nproc)}
  TIMESTAMP=$(date +'%Y%m%dT%H%M%S')
  while read -r repo; do
    local repo_name trunk svn_url
    repo_name=$(jq -r '.name' <<<"$repo")
    [[ -z "$REPO_NAME" || "$REPO_NAME" == "$repo_name" ]] || continue
    trunk=$(jq -r '.trunk' <<<"$repo")
    svn_url="${SVN_PREFIX}/${trunk}"
    if (( FROM_MIRROR )); then
      svn_url="svn://fcm1/${trunk/$repo_name/$repo_name.xm_svn}"
    fi
    if (( DRY_RUN )); then
      printf "%-.52s %s\n" \
        "$svn_url ....................................." \
        "${GH_PREFIX}/${repo_name}"
      continue
    fi
    echo "$(date +'%F %T') Processing: $repo_name"
    process_repo "$repo_name" "$svn_url" "$repo" \
      &> "${WORKDIR}/logs/${TIMESTAMP}_${repo_name}.log" &
    process_ids+=("$!")

    # Wait for background jobs if max_jobs are running
    if (( ${#process_ids[@]} >= max_jobs )); then
      for pid in "${process_ids[@]}"; do
        wait "$pid"
      done
      process_ids=()
    fi
  done < <(jq -c '.repo[]' "$CONFIG_FILE")

  # Wait for any remaining background jobs
  for pid in "${process_ids[@]}"; do
    wait "$pid"
  done
}

# ----------------------------------------------------------------------------
# Main Script
# ----------------------------------------------------------------------------

OPTS=$(getopt \
  -o hdmnuj:c:r:w: \
  -l help,debug,from-mirror,list-repos,update,parallel:,config:,repository:,workdir: \
  -n "${0##*/}" -- "$@") || { usage; exit 1; }
eval set -- "$OPTS"

SCRIPT_DIR=$(dirname -- "$(readlink -fe -- "$0")")
export CONFIG_FILE="${SCRIPT_DIR}/config.json"
export WORKDIR="${DATADIR}/git-migration"
UPDATE_GITHUB=0
FROM_MIRROR=0
REPO_NAME=
DRY_RUN=

while true; do
  case "$1" in
  -h | --help) usage; exit 0 ;;
  -d | --debug) PS4='L${LINENO}: '; set -x; shift ;;
  -m | --from-mirror) FROM_MIRROR=1; shift ;;
  -n | --list-repos) DRY_RUN=1; echo "$(date +'%F %T') Dry run"; shift ;;
  -u | --update) UPDATE_GITHUB=1; shift ;;
  -j | --parallel) export BATCH="$2"; shift 2 ;;
  -c | --config) CONFIG_FILE=$(readlink -fe "$2"); shift 2 ;;
  -r | --repository) REPO_NAME="$2"; validate_repo "$REPO_NAME"; shift 2 ;;
  -w | --workdir) WORKDIR=$(readlink -fe "$2"); shift 2 ;;
  --) shift; break ;;
  *) break ;;
  esac
done

initialize_environment
SVN_PREFIX=$(jq -r '.svn_prefix' "$CONFIG_FILE")
GH_ORG=$(jq -r '.gh_org' "$CONFIG_FILE")
GH_PREFIX="git@github.com:${GH_ORG}"

process_repositories
echo "$(date +'%F %T') Done."
