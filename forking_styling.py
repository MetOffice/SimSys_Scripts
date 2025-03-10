# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

'''
Python script to apply LFRic styling to .F90/.f90 files, given a path/directory.

Current implementation takes a valid path and traverses from the top level file
structure identifying fortran files, and applying styling.

Styling currently being applied:
    -lowercasing keywords

'''

import re
import sys
import argparse
import os
from pathlib import Path
from styling import KEYWORDS


def lowercase_keywords(file):
    print("Lowercasing keywords in", file)
    with open(file, 'r') as fp:
        lines = fp.read()
        for keyword in KEYWORDS:
            print(keyword)
            pattern = rf"\b{re.escape(keyword.upper())}\b"
            lines = re.sub(pattern, convert_to_lower, lines)
    with open(file, 'w') as fp:
        for line in lines:
            fp.write(line)


def convert_to_lower(match_obj):
    if match_obj.group() is not None:
        return match_obj.group().lower()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "-directory",
        metavar="home/user/etc",
        type=Path,
        default=Path(),
        help="path to a directory of files."
    )

    arguments = parser.parse_args()
    if os.path.exists(arguments.directory):
        for root, dirs, files in os.walk(arguments.directory):
            for file in files:
                if file.endswith((".f90", ".F90")):
                    cur_path = os.path.join(root, file)
                    lowercase_keywords(cur_path)
    else:
        print("ERROR: Path supplied does not exist or is incorrect.")
