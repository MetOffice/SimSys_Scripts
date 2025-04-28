# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# Created date: 10/03/2025
# Modified date: 14/03/2025
# *****************************COPYRIGHT*******************************

'''
Python script to apply LFRic styling to .F90/.f90 files, given a path/directory.

Current implementation takes a valid path and traverses from the top level file
structure identifying fortran files, and applying styling.

Styling currently being applied:
    -lowercasing keywords
'''

import re
import argparse
import os
from pathlib import Path
from styling_keywords import NEW_KEYWORDS


def lowercase_keywords(file):
    """
    Lowercase words in a file when they match a word in the keywords set.
    """
    print("Lowercasing keywords in", file)
    with open(file, 'r') as fp:
        lines = fp.read()
        for keyword in NEW_KEYWORDS:
            # regex to check if a keyword is preceded with a '!' symbol or it matches a keyword and group each.
            pattern = rf"((?:(?<=!)).*|(\b{re.escape(keyword.upper())}\b))"
            lines = re.sub(pattern, convert_to_lower, lines, flags=re.MULTILINE)

    with open(file, 'w') as fp:
        for line in lines:
            fp.write(line)


def convert_to_lower(match_obj):
    """Checks if match is true and lowercases string."""
    if match_obj.group(2) is not None:
        return match_obj.group(2).lower()
    else:
        return match_obj.group()


def apply_styling(path_to_dir):
    """
    Take a path to a directory containing fortran files.
    Applying styling from other functions.
    Identifying fortran files by extension.
    """
    if os.path.exists(path_to_dir):
        for root, dirs, files in os.walk(path_to_dir):
            for file in files:
                if file.endswith((".f90", ".F90")):
                    cur_path = os.path.join(root, file)
                    lowercase_keywords(cur_path)
    else:
        print("ERROR: Path supplied does not exist or is incorrect.")


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "directory",
        type=Path,
        default=Path(),
        help="path to a directory of files."
    )

    arguments = parser.parse_args()
    apply_styling(arguments.directory)


if __name__ == '__main__':
    main()
