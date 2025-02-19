#!/bin/bash
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

# This script will ask for kgo update information then moves the directory kgo_update
# to ~${root_user}. It then calls `run_kgo_script.sh` as ${root_user} which itself calls the update
# script. It then moves the generated variables files back into the working copy.
# The kgo_update.py script can be run with the --new-release option by providing
# 'new-release' as a command line option to this script

# Set colour codes
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Move to the location of the script
script_loc="$(dirname "$0")"

# Work out if we're running from azspice or old spice
if [[ $HOSTNAME == "caz"* ]]; then
    root_user="umadmin"
    root_home="/home/users/umadmin"
    launch_platform=azspice
    # Check you can sudo in as umadmin
    sudo -iu ${root_user} bash -c "echo ''"
    if [[ $? -ne 0 ]]; then
        printf "${RED} You were unable to run commands as umadmin - this is required to run this script"
        printf "This may be because of a password typo or similar"
    fi
else
    root_user="frum"
    root_home="/home/h01/frum"
    launch_platform=spice
fi

# Check for command line argument to run with new-release mode
# If only option doesn't match ask if that is what was intended
new_release=0
if [ $# -ne 0 ]; then
    if [[ $1 == *"new-release"* ]]; then
        new_release=1
    else
        printf "${RED}'%s' is not a recognised command line argument.\n" "${1}"
        printf "The only command line option available is --new-release.\n"
        read -p "Would you like to run in new-release mode (default n)? " answer
        answer=${answer:-"n"}
        if [[ $answer == "y" ]]; then
            new_release=1
        fi
        printf "${NC}"
        clear
    fi
fi

# Prompt user for Update Details
echo "Enter the platforms requiring a kgo update"
echo "Enter platforms lowercase and space separated, eg. spice xc40 ex1a azspice"
read platforms
if [[ $platforms == *"spice"* ]] && [[ $platforms != *"azspice"* ]] || [[ $platforms == *"xc40"* ]]; then
    # Check we're not trying to install to spice while on azspice
    if [[ $launch_platform == "azspice" ]] && [[ $platforms == *"spice"* ]] && [[ $platforms != *"azspice"* ]]; then
        printf "${RED}Attempting to install spice kgo from azspice - this isn't possible"
        exit 1
    fi
    read -p "spice/xc40 Suite Username: " suite_user
else
    suite_user=None
fi
if [[ $platforms == *"ex1a"* ]] || [[ $platforms == *"azspice"* ]]; then
    # Check we're not trying to install to azspice while on spice
    if [[ $launch_platform == "spice" ]] && [[ $platforms == *"azspice"* ]]; then
        printf "${RED}Attempting to install azspice kgo from spice - this isn't possible"
        exit 1
    fi
    read -p "ex1a/azspice Suite Username: " suite_user_ex1a
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
    printf "${RED}Couldn't detect a flow.cylc or suite.rc at ${wc_path}/rose-stem/\n${NC}"
    exit 1
fi

# Get user to double check settings
clear
echo "Suite Name: ${suite_name}"
if [[ $platforms == *"spice"* ]] && [[ $platforms != *"azspice"* ]] || [[ $platforms == *"xc40"* ]]; then
    echo "User: ${suite_user}"
fi
if [[ $platforms == *"ex1a"* ]] || [[ $platforms == *"azspice"* ]]; then
    echo "ex1a User: ${suite_user_ex1a}"
fi
echo "Trunk WC Path: ${wc_path}"
echo "Version Number: ${version_number}"
echo "Ticket Number: ${ticket_number}"
echo "Variables Extension: ${variables_extension}"
echo "New KGO Dir: ${new_kgo_dir}"
if [ $new_release -eq 1 ]; then
    printf "${RED}WARNING: Running with --new-release enabled${NC}\n"
fi
read -p "Run with the above settings y/n (default n): " run_script
run_script=${run_script:-n}
if [ $run_script != "y" ]; then
    exit 0
fi

# Move the kgo_update directory to frum on linux
if [[ $launch_platform == "spice" ]]; then
    scp -rq $script_loc/kgo_update ${root_user}@localhost:~
else
    sudo -iu ${root_user} bash -c "cp -r $script_loc/kgo_update ${root_home}"
fi

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

# Run the command as admin user
if [[ $launch_platform == "spice" ]]; then
    ssh -Y ${root_user}@localhost $command
else
    sudo -iu ${root_user} bash -c "cd $UMDIR ; $command"
fi

# Error Checking and rsyncing
variables_dir=kgo_update_files/vn${version_number}/${new_kgo_dir}
succeeded_spice=0
succeeded_azspice=0
succeeded_xc40=0
succeeded_ex1a=0
succeeded_all=1
if [[ $platforms == *"spice"* ]] && [[ $platforms != *"azspice"* ]]; then
    file=${root_home}/${variables_dir}/spice_update_script.sh
    if [[ -e "$file" ]]; then
        succeeded_spice=1
        if [[ $new_release -ne 1 ]]; then
            printf "${GREEN}\n\nCopying the spice variables file into this working copy.\n${NC}"
            scp -q ${root_user}@localhost:~/${variables_dir}/spice_updated_variables${variables_extension} \
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
if [[ $platforms == *"azspice"* ]]; then
    file=${root_home}/${variables_dir}/azspice_update_script.sh
    if [[ -e "$file" ]]; then
        succeeded_azspice=1
        if [[ $new_release -ne 1 ]]; then
            printf "${GREEN}\n\nCopying the azspice variables file into this working copy.\n${NC}"
            cp ${root_home}/${variables_dir}/azspice_updated_variables${variables_extension} \
                                        ${wc_path}/rose-stem/site/meto/variables_azspice${variables_extension}
            if [[ $? -ne 0 ]]; then
                printf "${RED}The copy of the azspice variables file into this working copy has failed.\n${NC}"
                succeeded_azspice=0
                succeeded_all=0
            fi
        fi
    else
        echo $file
        succeeded_all=0
    fi
fi
if [[ $platforms == *"xc40"* ]]; then
    file=${root_home}/${variables_dir}/xc40_update_script.sh
    if [[ -e "$file" ]]; then
        succeeded_xc40=1
        if [[ $new_release -ne 1 ]]; then
            printf "${GREEN}\n\nCopying the xc40 variables file into this working copy.\n${NC}"
            if [[ $launch_platform == "spice" ]]; then
                scp -q ${root_user}@localhost:~/${variables_dir}/xc40_updated_variables${variables_extension} \
                                        ${wc_path}/rose-stem/site/meto/variables_xc40${variables_extension}
            else
                cp ${root_home}/${variables_dir}/xc40_updated_variables${variables_extension} \
                                        ${wc_path}/rose-stem/site/meto/variables_xc40${variables_extension}
            fi
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
    file=${root_home}/${variables_dir}/ex1a_update_script.sh
    if [[ -e "$file" ]]; then
        succeeded_ex1a=1
        if [[ $new_release -ne 1 ]]; then
            printf "${GREEN}\n\nCopying the ex1a variables file into this working copy.\n${NC}"
            if [[ $launch_platform == "spice" ]]; then
                scp -q ${root_user}@localhost:~/${variables_dir}/ex1a_updated_variables${variables_extension} \
                                        ${wc_path}/rose-stem/site/meto/variables_ex1a${variables_extension}
            else
                cp ${root_home}/${variables_dir}/ex1a_updated_variables${variables_extension} \
                                        ${wc_path}/rose-stem/site/meto/variables_ex1a${variables_extension}
            fi
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

if [[ $platforms == *"xc40"* ]] || [[ $platforms == *"ex1a"* ]]; then
    read -p "Enter 1 to rsync UM KGO, 2 to rsync lfricinputs KGO (default 1): " rsync_type
    if [[ $rsync_type == "2" ]]; then
        rsync_dir="lfricinputs/kgo/"
    else
        rsync_dir="kgo/"
    fi
fi

# For the xc40s rsync the generated kgo to the xcs
if [[ $succeeded_xc40 -eq 1 ]]; then
    printf "${GREEN}\n\nrsyncing the kgo to xcs.\n${NC}"
    host_rsync=$(rose host-select xc)
    rsync_com="ssh -Y ${host_rsync} 'rsync -av /projects/um1/standard_jobs/${rsync_dir} xcslr0:/common/um1/standard_jobs/${rsync_dir}'"
    if [[ $launch_platform == "spice" ]]; then
        ssh -Y ${root_user}@localhost $rsync_com
    else
        sudo -iu ${root_user} bash -c "$rsync_com"
    fi
    if [[ $? -ne 0 ]]; then
        printf "${RED}The rsync to the xcs has failed.\n${NC}"
    else
        printf "${Green}The rsync to the xcs has succeeded.\n${NC}"
    fi
elif [[ $platforms == *"xc40"* ]]; then
    printf "${RED}\n\nSkipping the rsync to the xcs as the xc40 install failed.\n${NC}"
fi

# For EX's currently rsync the generated kgo to the exz from the exab
# This process will need modifying as we go forward
# Currently hardcoded to UM kgo as lfricinputs not on ex machines
if [[ $succeeded_ex1a -eq 1 ]]; then
    printf "${GREEN}\n\nrsyncing the kgo to exz + excd.\n${NC}"
    printf "Warning: Always rsyncing UM KGO (not lfricinputs) on ex1a"
    rsync_dir="kgo/"
    host_rsync=$(rose host-select exab)

    # rsync to EXZ
    rsync_com="ssh -Y ${host_rsync} 'rsync -av /common/internal/umdir/standard_jobs/${rsync_dir} login.exz:/common/umdir/standard_jobs/${rsync_dir}'"
    if [[ $launch_platform == "spice" ]]; then
        ssh -Y ${root_user}@localhost $rsync_com
    else
        sudo -iu ${root_user} bash -c "$rsync_com"
    fi
    if [[ $? -ne 0 ]]; then
        printf "${RED}The rsync to the exz has failed.\n${NC}"
    else
        printf "${Green}The rsync to the exz has succeeded.\n${NC}"
    fi

    # rsync to EXCD
    excd_host=$(rose host-select excd)
    rsync_com="ssh -Y ${host_rsync} 'rsync -av /common/internal/umdir/standard_jobs/${rsync_dir} ${excd_host}:/common/internal/umdir/standard_jobs/${rsync_dir}'"
    if [[ $launch_platform == "spice" ]]; then
        ssh -Y ${root_user}@localhost $rsync_com
    else
        sudo -iu ${root_user} bash -c "$rsync_com"
    fi
    if [[ $? -ne 0 ]]; then
        printf "${RED}The rsync to the excd has failed.\n${NC}"
    else
        printf "${Green}The rsync to the excd has succeeded.\n${NC}"
    fi
elif [[ $platforms == *"ex1a"* ]]; then
    printf "${RED}\n\nSkipping the rsync to the exz/cd as the exab install failed.\n${NC}"
fi

printf "\n\nInstallation Summary:\n\n"
if [[ $platforms == *"spice"* ]] && [[ $platforms != *"azspice"* ]]; then
    if [[ $succeeded_spice -eq 1 ]]; then
        printf "${GREEN}Installation on spice successful.\n${NC}"
    else
        printf "${RED}Installation on spice unsuccessful. Review output for error.\n${NC}"
    fi
fi
if [[ $platforms == *"azspice"* ]]; then
    if [[ $succeeded_azspice -eq 1 ]]; then
        printf "${GREEN}Installation on azspice successful.\n${NC}"
    else
        printf "${RED}Installation on azspice unsuccessful. Review output for error.\n${NC}"
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