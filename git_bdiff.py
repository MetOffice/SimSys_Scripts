#!/usr/bin/env python3
# *********************************COPYRIGHT************************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *********************************COPYRIGHT************************************
"""
This module provides the functionality to return a list of local files to
run tests on based on the branch-difference (to allow checking of only files
which a developer has actually modified on their branch)
"""

import os
import re
import subprocess
import time

def run_command(command, shell=False):
    """
    Run a subprocess command and return the result object
    Inputs:
        - command, str with command to run
    Outputs:
        - result object from subprocess.run
    """
    if not shell:
        command = command.split()
    return subprocess.run(
        command,
        capture_output=True,
        text=True,
        timeout=120,
        shell=shell,
        check=False,
    )

