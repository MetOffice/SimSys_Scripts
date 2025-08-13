#!/bin/bash

# Needs to be run as julesadmin 'xsudo -iu julesadmin'

set -e

read -rp "KGO VERSION:" KGO_VERSION
read -rp "USER NAME:" USER_NAME
read -rp "SUITE/RUNX NAME:" SUITE
read -rp "EX ZONE:" EX_ZONE

# Azure spice kgo dir
AZURE_KGO_DIR=/data/users/julesadmin/jules/rose-stem/jules-kgo/$KGO_VERSION
mkdir -p "$AZURE_KGO_DIR"
cp /home/users/"$USER_NAME"/cylc-run/"$SUITE"/work/1/meto_azspice_*/output/* "$AZURE_KGO_DIR"

# select the zone to resync based on user inputted EX_ZONE
if [ "$EX_ZONE" == "exab" ]; then
    echo "exab choosen"
    EX_ZONE=login.exab.sc
    EX_ZONE_RSYNC=login.excd.sc

elif [ "$EX_ZONE" == "excd" ]; then
    echo "excd choosen"
    EX_ZONE=login.excd.sc
    EX_ZONE_RSYNC=login.exab.sc

else
    echo "ex zone not entered or incorrect, please enter \"exab\" or \"excd\"."
    exit
fi

# EXAB EXCD rsync files
EX_KGO_DIR=/common/internal/jules/rose-stem-kgo/$KGO_VERSION
ssh -Y $EX_ZONE "echo "new kgo dir: "$EX_KGO_DIR""; mkdir -p $EX_KGO_DIR; cp /home/users/$USER_NAME/cylc-run/$SUITE/work/1/meto_ex1a_*/output/* $EX_KGO_DIR; rsync -avz $EX_KGO_DIR $EX_ZONE_RSYNC:/common/internal/jules/rose-stem-kgo/"

exit
