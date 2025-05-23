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
# $UMDIR/kgo_updated_variables/vnVV.V_tTTTT/
# This script is NOT intended to be run independently of '../meto_update_kgo.sh'

# shellcheck disable=SC2059

# Set colour codes
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Read in command line arguments
while getopts S:U:Z:N:R:P:V:F: flag
do
    case "${flag}" in
        S) suite_name=${OPTARG};;
        U) suite_user=${OPTARG};;
        Z) ex_kgo_host=${OPTARG};;
        N) new_kgo_dir=${OPTARG};;
        R) new_release=${OPTARG};;
        P) platforms=${OPTARG};;
        F) variables_extension=${OPTARG};;
        V) version_number=${OPTARG};;
        *) echo "Invalid option: -$flag" >&2; exit 1;;
    esac
done

# Create command to run python script
kgo_command="./kgo_update.py"
if [[ $new_release -eq 1 ]]; then
    kgo_command="${kgo_command} --new-release"
fi
kgo_command="${kgo_command} -S $suite_name -N $new_kgo_dir -E $variables_extension -U $suite_user"
kgo_command_azspice="$kgo_command -P azspice"
kgo_command_ex1a="$kgo_command -P ex1a"

# Make a directory to store the script and variables file file in
variables_dir=~/kgo_update_files/vn$version_number/$new_kgo_dir
mkdir -p "$variables_dir"

# If azspice has kgo updates
if [[ $platforms == *"azspice"* ]]; then
    printf "${GREEN}\n\nRunning KGO Update Script on azspice.\n${NC}"

    # Run the Update Script
    if $kgo_command_azspice; then
        # Move the updated variables file and script into the ticket folder
        printf "${GREEN}\n\nSuccessfully installed on azspice.\n${NC}"
        printf "${GREEN}Moving the generated files into azspice ${variables_dir}.${NC}\n"
        if [[ $new_release -ne 1 ]]; then
            mv ~/"variables${variables_extension}_${new_kgo_dir}" \
                "${variables_dir}/azspice_updated_variables${variables_extension}"
        fi
        mv ~/"kgo_update_${new_kgo_dir}.sh" \
            "${variables_dir}/azspice_update_script.sh"
    else
        printf "${RED}\nThe installation script has failed on azspice.\n${NC}"
    fi
fi


# If ex1a has kgo updates
if [[ $platforms == *"ex1a"* ]]; then
    printf "${GREEN}\n\nRunning KGO Update Script on ex1a.\n${NC}"

    host_ex=$(rose host-select "$ex_kgo_host")

    # SCP the python script to the ex1a
    scp -q kgo_update.py "umadmin@$host_ex":~

    # Define the commands to run on ex1a
    command="module load scitools ;
             ${kgo_command_ex1a}"

    # SSH to the ex1a with the run command
    if ssh -Y "$host_ex" "$command"; then
        # rsync the generated variables file and script back to frum on linux
        # This cleans up the original files
        printf "${GREEN}\n\nSuccessfully installed on ex1a.\n${NC}"
        printf "${GREEN}Rsyncing the generated files into azspice ${variables_dir}.\n${NC}"
        if [[ $new_release -ne 1 ]]; then
            rsync --remove-source-files -avz \
                "umadmin@$host_ex:~/variables${variables_extension}_${new_kgo_dir}" \
                "${variables_dir}/ex1a_updated_variables${variables_extension}"
        fi
        rsync --remove-source-files -avz \
            "umadmin@$host_ex:~/kgo_update_${new_kgo_dir}.sh" \
            "${variables_dir}/ex1a_update_script.sh"
    else
        printf "${RED}\nThe installation script has failed on ex1a.\n${NC}"
    fi

    # Clean up kgo_update.py on ex1a
    ssh -Yq "$host_ex" "rm kgo_update.py"
fi
