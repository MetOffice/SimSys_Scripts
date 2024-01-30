#!/bin/bash
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

# This script will ask for kgo update information then moves the directory kgo_update
# to ~frum. It then calls `run_kgo_script.sh` as frum which itself calls the update
# script. It then moves the generated variables files back into the working copy.
# The kgo_update.py script can be run with the --new-release option by providing
# 'new-release' as a command line option to this script

# Set colour codes
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Move to the location of the script
cd "$(dirname "$0")"

# Check for command line argument to run with new release_mode
new_release=0
if [ $# -ne 0 ]; then
    if [[ $1 == *"new-release"* ]]; then
        new_release=1
    fi
fi

# Prompt user for Update Details
echo "Enter the platforms requiring a kgo update"
echo "Enter platforms lowercase and space separated, eg. spice xc40 ex1a"
read platforms
if [[ $platforms == *"spice"* ]] || [[ $platforms == *"xc40"* ]]; then
    read -p "SPICE/XC40 Suite Username: " suite_user
else
    suite_user=None
fi
if [[ $platforms == *"ex1a"* ]]; then
    read -p "EX1A Suite Username: " suite_user_ex1a
else
    suite_user_ex1a=None
fi
read -p "Suite Name: " suite_name
read -p "Enter the path to the merged trunk WC (top directory): " wc_path
# Trim any trailing / from the end of the path
wc_path=${wc_path%/}
if [ ! -d "${wc_path}" ]; then
    printf "${RED}${wc_path} is not a valid path${NC}\n"
    exit 1
fi
read -p "Version Number (VV.V): " version_number
read -p "Ticket Number (TTTT): " ticket_number
read -p "How should the new kgo directory be named (default vn${version_number}_t${ticket_number}): " new_kgo_dir
new_kgo_dir=${new_kgo_dir:-"vn${version_number}_t${ticket_number}"}

# Check in the working_copy rose-stem for .rc or .cylc files
# Need this for the variables file extension
# Can't use Cylc version as .rc can be used in compatability mode
if [ -f "${wc_path}/rose-stem/suite.rc" ]; then
    variables_extension=".rc"
elif [ -f "${wc_path}/rose-stem/suite.cylc" ] || [ -f "${wc_path}/rose-stem/flow.cylc" ]; then
    variables_extension=".cylc"
else
    printf "${RED}Couldn't detect a flow.cylc or suite.rc at ${wc_path}/rose-stem/${NC}"
    exit 1
fi

# Get user to double check settings
clear
echo "Suite Name: ${suite_name}"
if [[ $platforms == *"spice"* ]] || [[ $platforms == *"xc40"* ]]; then
    echo "User: ${suite_user}"
fi
if [[ $platforms == *"ex1a"* ]]; then
    echo "EX1A User: ${suite_user_ex1a}"
fi
echo "Trunk WC Path: ${wc_path}"
echo "Version Number: ${version_number}"
echo "Ticket Number: ${ticket_number}"
echo "Variables Extension: ${variables_extension}"
echo "New KGO Dir: ${new_kgo_dir}"
if [ $new_release -eq 1 ]; then
    echo "WARNING: Running with --new-release enabled"
fi
read -p "Run with the above settings y/n (default n): " run_script
run_script=${run_script:-n}
if [ $run_script != "y" ]; then
    exit 0
fi

# Move the kgo_update directory to frum on linux
scp -rq kgo_update frum@localhost:~

# Define command to run as frum
command=". /etc/profile ; module load scitools ; cd kgo_update ;
         ./meto_run_kgo_script.sh -S ${suite_name}
                                  -U ${suite_user}
                                  -E ${suite_user_ex1a}
                                  -N ${new_kgo_dir}
                                  -R ${new_release}
                                  -P '${platforms}'
                                  -F ${variables_extension}
                                  -V ${version_number} ;
         cd ~ ; rm -rf kgo_update"

# Run the command as frum
ssh -Y frum@localhost $command

# Error Checking and rsyncing
variables_dir=kgo_update_files/vn${version_number}/${new_kgo_dir}
succeeded_spice=0
succeeded_xc40=0
succeeded_ex1a=0
succeeded_all=1
if [[ $platforms == *"spice"* ]]; then
    file=~frum/${variables_dir}/spice_update_script.sh
    if [[ -e "$file" ]]; then
        succeeded_spice=1
        if [[ $new_release -ne 1 ]]; then
            printf "${GREEN}\n\nCopying the spice variables file into this working copy.\n${NC}"
            scp -q frum@localhost:~/${variables_dir}/spice_updated_variables${variables_extension} \
                                        ${wc_path}/rose-stem/site/meto/variables_spice${variables_extension}
            if [[ $? -ne 0 ]]; then
                printf "${RED}The copy of the spice variables file into this working copy has failed.\n${NC}"
                succeeded_spice=0
                succeeded_all=0
            fi
        fi
    else
        succeeded_all=0
    fi
fi
if [[ $platforms == *"xc40"* ]]; then
    file=~frum/${variables_dir}/xc40_update_script.sh
    if [[ -e "$file" ]]; then
        succeeded_xc40=1
        if [[ $new_release -ne 1 ]]; then
            printf "${GREEN}\n\nCopying the xc40 variables file into this working copy.\n${NC}"
            scp -q frum@localhost:~/${variables_dir}/xc40_updated_variables${variables_extension} \
                                        ${wc_path}/rose-stem/site/meto/variables_xc40${variables_extension}
            if [[ $? -ne 0 ]]; then
                printf "${RED}The copy of the xc40 variables file into this working copy has failed.\n${NC}"
                succeeded_xc40=0
                succeeded_all=0
            fi
        fi
    else
        succeeded_all=0
    fi
fi
if [[ $platforms == *"ex1a"* ]]; then
    file=~frum/${variables_dir}/ex1a_update_script.sh
    if [[ -e "$file" ]]; then
        succeeded_ex1a=1
        if [[ $new_release -ne 1 ]]; then
            printf "${GREEN}\n\nCopying the ex1a variables file into this working copy.\n${NC}"
            scp -q frum@localhost:~/${variables_dir}/ex1a_updated_variables${variables_extension} \
                                        ${wc_path}/rose-stem/site/meto/variables_ex1a${variables_extension}
            if [[ $? -ne 0 ]]; then
                printf "${RED}The copy of the ex1a variables file into this working copy has failed.\n${NC}"
                succeeded_ex1a=0
                succeeded_all=0
            fi
        fi
    else
        succeeded_all=0
    fi
fi

if [[ $succeeded_all -eq 1 ]]; then
    printf "${GREEN}All kgo has been successfully installed.\n${NC}"
else
    printf "${RED}\n\nAt least 1 platform suffered an error during installation.\n${NC}"
fi

# For the xc40s rsync the generated kgo to the xcs
if [[ $succeeded_xc40 -eq 1 ]]; then
    printf "${GREEN}\n\nrsyncing the kgo to xcs.\n${NC}"
    read -p "Enter 1 to rsync UM KGO, 2 to rsync lfricinputs KGO (default 1): " rsync_type
    if [[ $rsync_type == "2" ]]; then
        rsync_dir="lfricinputs/kgo/"
    else
        rsync_dir="kgo/"
    fi
    rsync_com="ssh -Y xcel00 'rsync -av /projects/um1/standard_jobs/${rsync_dir} xcslr0:/common/um1/standard_jobs/${rsync_dir}'"
    ssh -Y frum@localhost $rsync_com
    if [[ $? -ne 0 ]]; then
        printf "${RED}The rsync to the xcs has failed.\n${NC}"
    else
        printf "${Green}The rsync to the xcs has succeeded.\n${NC}"
    fi
elif [[ $platforms == *"xc40"* ]]; then
    printf "${RED}\n\nSkipping the rsync to the xcs as the xc40 install failed.\n${NC}"
fi

printf "\n\nInstallation Summary:\n\n"
if [[ $platforms == *"spice"* ]]; then
    if [[ $succeeded_spice -eq 1 ]]; then
        printf "${GREEN}Installation on spice successful.\n${NC}"
    else
        printf "${RED}Installation on spice unsuccessful. Review output for error.\n${NC}"
    fi
fi
if [[ $platforms == *"xc40"* ]]; then
    if [[ $succeeded_xc40 -eq 1 ]]; then
        printf "${GREEN}Installation on xc40 successful.\n${NC}"
    else
        printf "${RED}Installation on xc40 unsuccessful. Review output for error.\n${NC}"
    fi
fi
if [[ $platforms == *"ex1a"* ]]; then
    if [[ $succeeded_ex1a -eq 1 ]]; then
        printf "${GREEN}Installation on ex1a successful.\n${NC}"
    else
        printf "${RED}Installation on ex1a unsuccessful. Review output for error.\n${NC}"
    fi
fi