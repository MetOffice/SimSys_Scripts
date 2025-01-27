#!/usr/bin/env bash
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

# This script is intended to be run by frum having been called by
# `../meto_update_kgo.sh` . It runs the `kgo_update.py` script which it
# assumes is in the same directory. It takes suite name, suite user, version
# number, ticket number and required platforms as inputs. It runs the script on
# each platform as required and then moves the generated variables file to
# ~frum/kgo_updated_variables/vnVV.V_tTTTT/ on linux. IF xc40 is being run it
# rsyncs with the xcs.
# This script is NOT intended to be run independently of '../meto_update_kgo.sh'

# Set colour codes
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Read in command line arguments
while getopts S:U:E:N:R:P:V:F: flag; do
    case "${flag}" in
        S) suite_name=${OPTARG} ;;
        U) suite_user=${OPTARG} ;;
        E) suite_user_ex1a=${OPTARG} ;;
        N) new_kgo_dir=${OPTARG} ;;
        R) new_release=${OPTARG} ;;
        P) platforms=${OPTARG} ;;
        F) variables_extension=${OPTARG} ;;
        V) version_number=${OPTARG} ;;
        *) echo "Invalid option: -$flag" >&2; exit 1 ;;
    esac
done
shift "$((OPTIND-1))"

# Create command to run python script
kgo_command="./kgo_update.py"
[ "$new_release" -eq 1 ] && kgo_command="${kgo_command} --new-release"
kgo_command="${kgo_command} -S $suite_name -N $new_kgo_dir -E $variables_extension"
kgo_command_spice="$kgo_command -U $suite_user -P spice"
kgo_command_azspice="$kgo_command -U $suite_user_ex1a -P azspice"
kgo_command_xc40="$kgo_command -U $suite_user -P xc40"
kgo_command_ex1a="$kgo_command -U $suite_user_ex1a -P ex1a"

# Make a directory to store the script and variables file in
variables_dir=~/kgo_update_files/vn$version_number/$new_kgo_dir
mkdir -p "$variables_dir"

run_update_script() {
    _platform="$1"
    _command="$2"
    _user="$3"
    _host="$4"

    echo;echo "${GREEN}Running KGO Update Script on ${_platform}.${NC}"

    if [ -n "$_host" ]; then
        scp -q kgo_update.py "${_user}@${_host}:~"
        _command="ssh -Y $_host \"$_command\""
    fi

    if eval "$_command"; then
        echo;echo "${GREEN}Successfully installed on ${_platform}.${NC}"
        echo "${GREEN}Moving the generated files into ${_platform} ${variables_dir}.${NC}"
        if [ "$new_release" -ne 1 ]; then
            rsync --remove-source-files -avz \
                "${_user}@${_host}:~/variables${variables_extension}_${new_kgo_dir}" \
                "${variables_dir}/${_platform}_updated_variables${variables_extension}"
        fi
        rsync --remove-source-files -avz \
            "${_user}@${_host}:~/kgo_update_${new_kgo_dir}.sh" \
            "${variables_dir}/${_platform}_update_script.sh"
    else
        echo "${RED}The installation script has failed on ${_platform}.${NC}"
    fi

    [ -n "$_host" ] && ssh -Yq "$_host" "rm kgo_update.py"
}

if [ "${platforms#*spice}" != "$platforms" ] && [ "${platforms#*azspice}" = "$platforms" ]; then
    run_update_script "spice" "$kgo_command_spice" "frum"
fi
[ "${platforms#*azspice}" != "$platforms" ] && run_update_script "azspice" "$kgo_command_azspice" "frum"
[ "${platforms#*xc40}" != "$platforms" ] && run_update_script "xc40" "$kgo_command_xc40" "frum" "$(rose host-select xc)"
[ "${platforms#*ex1a}" != "$platforms" ] && run_update_script "ex1a" "$kgo_command_ex1a" "umadmin" "$(rose host-select exab)"
