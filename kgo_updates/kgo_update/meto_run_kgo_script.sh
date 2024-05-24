#!/bin/bash
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
while getopts S:U:E:N:R:P:V:F: flag
do
    case "${flag}" in
        S) suite_name=${OPTARG};;
        U) suite_user=${OPTARG};;
        E) suite_user_ex1a=${OPTARG};;
        N) new_kgo_dir=${OPTARG};;
        R) new_release=${OPTARG};;
        P) platforms=${OPTARG};;
        F) variables_extension=${OPTARG};;
        V) version_number=${OPTARG};;
    esac
done

# Create command to run python script
kgo_command="./kgo_update.py"
if [ $new_release -eq 1 ]; then
    kgo_command="${kgo_command} --new-release"
fi
kgo_command="${kgo_command} -S $suite_name -N $new_kgo_dir -E $variables_extension"
kgo_command_spice="$kgo_command -U $suite_user -P spice"
kgo_command_xc40="$kgo_command -U $suite_user -P xc40"
kgo_command_ex1a="$kgo_command -U $suite_user_ex1a -P ex1a"

# Make a directory to store the script and variables file file in
variables_dir=~/kgo_update_files/vn$version_number/$new_kgo_dir
mkdir -p $variables_dir

# If Spice has kgo updates
if [[ $platforms == *"spice"* ]]; then
    printf "${GREEN}\n\nRunning KGO Update Script on SPICE.\n${NC}"

    # Run the Update Script
    $kgo_command_spice

    if [[ $? -ne 0 ]]; then
        printf "${RED}\nThe installation script has failed on spice.\n${NC}"
    else
        # Move the updated variables file and script into the ticket folder
        printf "${GREEN}\n\nSuccessfully installed on Spice.\n${NC}"
        printf "${GREEN}Moving the generated files into SPICE ${variables_dir}.${NC}\n"
        if [ $new_release -ne 1 ]; then
            mv ~/variables${variables_extension}_${new_kgo_dir} ${variables_dir}/spice_updated_variables${variables_extension}
        fi
        mv ~/kgo_update_${new_kgo_dir}.sh ${variables_dir}/spice_update_script.sh
    fi
fi

# If xc40 has kgo updates
if [[ $platforms == *"xc40"* ]]; then
    printf "${GREEN}\n\nRunning KGO Update Script on xc40.\n${NC}"

    host_xc40=$(rose host-select xc)

    # SCP the python script to the xc40
    scp -q kgo_update.py frum@$host_xc40:~

    # Define the commands to run on xc40
    command=". /etc/profile ; module load scitools ;
             ${kgo_command_xc40}"

    # SSH to the xc40 with the run command
    ssh -Y $host_xc40 $command

    if [[ $? -ne 0 ]]; then
        printf "${RED}\nThe installation script has failed on xc40.\n${NC}"
    else
        # rsync the generated variables file and script back to frum on linux
        # This cleans up the original files
        printf "${GREEN}\n\nSuccessfully installed on xc40.\n${NC}"
        printf "${GREEN}Rsyncing the generated files into SPICE ${variables_dir}.\n${NC}"
        if [ $new_release -ne 1 ]; then
            rsync --remove-source-files -avz \
                frum@$host_xc40:~/variables${variables_extension}_${new_kgo_dir} \
                ${variables_dir}/xc40_updated_variables${variables_extension}
        fi
        rsync --remove-source-files -avz \
                frum@$host_xc40:~/kgo_update_${new_kgo_dir}.sh \
                ${variables_dir}/xc40_update_script.sh
    fi

    # Clean up kgo_update.py on xc40
    ssh -Yq $host_xc40 "rm kgo_update.py"
fi

# If ex1a has kgo updates
if [[ $platforms == *"ex1a"* ]]; then
    printf "${GREEN}\n\nRunning KGO Update Script on ex1a.\n${NC}"

    # Should change to rose host-select when on the quads properly
    host_ex=login.exz

    # SCP the python script to the ex1a
    scp -q kgo_update.py umadmin@$host_ex:~

    # Define the commands to run on ex1a
    command="module load scitools ;
             ${kgo_command_ex1a}"

    # SSH to the ex1a with the run command
    ssh -Y $host_ex $command

    if [[ $? -ne 0 ]]; then
        printf "${RED}\nThe installation script has failed on ex1a.\n${NC}"
    else
        # rsync the generated variables file and script back to frum on linux
        # This cleans up the original files
        printf "${GREEN}\n\nSuccessfully installed on ex1a.\n${NC}"
        printf "${GREEN}Rsyncing the generated files into SPICE ${variables_dir}.\n${NC}"
        if [ $new_release -ne 1 ]; then
            rsync --remove-source-files -avz \
                    umadmin@$host_ex:~/variables${variables_extension}_${new_kgo_dir} \
                    ${variables_dir}/ex1a_updated_variables${variables_extension}
        fi
        rsync --remove-source-files -avz \
                umadmin@$host_ex:~/kgo_update_${new_kgo_dir}.sh \
                ${variables_dir}/ex1a_update_script.sh
    fi

    # Clean up kgo_update.py on ex1a
    ssh -Yq $host_ex "rm kgo_update.py"
fi
