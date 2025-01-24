#!/usr/bin/env python3
# *********************************COPYRIGHT************************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *********************************COPYRIGHT************************************
"""
Script which tests code files within the UM repository to ensure they contain a
recognised copyright notice.
"""
import argparse
import os
import re
import subprocess
import sys
from textwrap import wrap

from fcm_bdiff import (get_branch_diff_filenames, get_branch_info, get_url,
                       is_trunk, text_decoder, use_mirror)

# Desired maximum column width for output - we make an exception
# for filenames, which are always printed on a single line to aid
# ease of selection by the user
_OUTPUT_LINE_WIDTH = 80

# Pattern which will match the intended input files
_FILENAME_FILTER = re.compile(r".*\.(py|c|F90|f90|h|sh)$")


# ------------------------------------------------------------------------------
def banner_print(message, maxwidth=_OUTPUT_LINE_WIDTH, char="%"):
    """
    Simple routine which prints a banner message
    """
    wrap_message = [
        char + " " + elt.ljust(maxwidth - 4) + " " + char
        for elt in wrap(message, width=maxwidth - 4)
    ]
    wrap_message = "\n".join(wrap_message)
    print(f"\n{char * maxwidth}\n{wrap_message}\n{char * maxwidth}")


# ------------------------------------------------------------------------------
def load_templates(filter_pattern):
    """
    Attempt load allowed copyright templates
    """

    loaded_templates = []

    template_path = "."
    if "CYLC_TASK_WORK_PATH" in os.environ:
        template_path = os.path.join(
            os.environ["CYLC_TASK_WORK_PATH"], "file", ""
        )

    template_files = files_to_process(
        template_path, [], filter_pattern=filter_pattern
    )

    for filename in template_files:
        with open(filename) as file:
            lines = file.read().splitlines()
            loaded_templates.append((filename, lines))

    return loaded_templates


# ------------------------------------------------------------------------------
def template_is_in_file(file, template):
    """
    Check if the contemt of a template exists somehwere within the file.
    """
    for i in range(len(file) - len(template) + 1):
        if all(template[j] == file[i + j] for j in range(len(template))):
            return True
    return False


# ------------------------------------------------------------------------------
def check_file_compliance(filename, templates, regex_templates):
    """
    Attempt to match the contents of the file "filename" to one of the
    pre-loaded templates.

    Returns True if any of the templates are found in the contents of the file.
    """

    with open(filename) as file:
        lines = file.read().splitlines()
        for _, template in templates:
            if template_is_in_file(lines, template):
                return True

    with open(filename) as file:
        lines = file.read()
        for _, template in regex_templates:
            if template.search(lines):
                return True

    return False


# ------------------------------------------------------------------------------
def files_to_process(filepath, ignore_list, filter_pattern=_FILENAME_FILTER):
    """
    Generate list of files in given filepath.  Ignore any files matching
    the patterns in the ignore list
    """
    files = []
    for root, _, filenames in os.walk(filepath):
        for filename in filenames:
            if filter_pattern.match(filename):
                path_to_file = os.path.join(root, filename)
                if any([ignore in path_to_file for ignore in ignore_list]):
                    print(f"WARNING: Ignoring file: {path_to_file}")
                    continue
                files.append(path_to_file)

    return files


# ------------------------------------------------------------------------------
def main(inputs, ignore_list):
    """main program block"""

    templates = []
    regex_templates_raw = []
    regex_templates = []

    banner_print("Running copyright checker")

    filter_tmp = re.compile(r".*\.template$")
    templates.extend(load_templates(filter_pattern=filter_tmp))

    filter_tmp = re.compile(r".*\.regex_template$")
    regex_templates_raw.extend(load_templates(filter_pattern=filter_tmp))

    for filename, template_lines in regex_templates_raw:
        regex_templates.append(
            (filename, re.compile(r"\n".join(template_lines)))
        )

    files_to_check = []
    for file_input in inputs:
        if os.path.isfile(file_input):
            if any([ignore in file_input for ignore in ignore_list]):
                print(f"WARNING: Ignoring file: {file_input}")
                continue
            else:
                print(f"Source (file): {file_input}")
                files_to_check.append(file_input)
        elif os.path.isdir(file_input):
            print(f"Source (dir) : {file_input}")
            files_to_check.extend(files_to_process(file_input, ignore_list))
        else:
            raise SystemExit(
                "[ERROR] Input sources must be files/directories"
                + "\n         : "
                + f'"{file_input}" is neither'
            )

    print(f"\nFound {len(files_to_check)} files to check")

    failed_files = []
    for item in files_to_check:
        print(f"file : {item}")
        file_pass = check_file_compliance(item, templates, regex_templates)
        if not file_pass:
            failed_files.append(item)

    fail_count = len(failed_files)
    plural = "s" if fail_count != 1 else ""
    banner_print(f"Checks completed with {fail_count} failure{plural}\n")

    if fail_count > 0:
        print(f": Failed file{plural} :")
        for filename in failed_files:
            full_fname = os.path.realpath(filename)
            banner_print(full_fname, maxwidth=(len(full_fname) + 10), char="#")
            print("")
        print("")
        plural2 = "have" if fail_count != 1 else "is"
        raise SystemExit(
            f"[ERROR] {fail_count} file{plural} "
            + f"{plural2} missing copyright notice{plural}"
        )


# ------------------------------------------------------------------------------
def parse_options():
    """Parse command line code options."""
    usage = "usage: %(prog)s [options] directory/file1 [[directory/file2] ...]"
    description = (
        "This script will scan for files to "
        "check for copyright notices. "
        "Arguments may be any combination of files and/or "
        "directories; "
        "individual files will be scanned, and all files within "
        "directories and their sub-directories will be scanned."
    )
    parser = argparse.ArgumentParser(usage=usage, description=description)
    excl_group = parser.add_mutually_exclusive_group()
    parser.add_argument(
        "--ignore",
        action="store",
        dest="ignore",
        default=None,
        help=(
            "ignore filename/s containing (comma separated list of patterns)"
        ),
    )
    parser.add_argument(
        "--base_path",
        action="store",
        dest="base_path",
        default=None,
        help="Override the base path to find the actual file.",
    )
    excl_group.add_argument(
        "--full_trunk",
        action="store_true",
        default=False,
        help=(
            "run on use the full file list when trunk, "
            "else run on fcm branch-diff"
        ),
    )
    excl_group.add_argument(
        "--bdiff",
        action="store_true",
        default=False,
        help="run on an fcm branch-diff",
    )
    excl_group.add_argument(
        "files",
        nargs="*",
        default=["./"],
        help="File(s) to check and/or directories to recursively search",
    )
    args = parser.parse_args()

    if args.full_trunk:
        branch, retries = use_mirror("./")

        # Get information about the branch
        info = get_branch_info(branch, retries=retries)

        branch_url = get_url(info)

        if is_trunk(branch_url):
            args.bdiff = False
        else:
            args.bdiff = True

    if args.bdiff:
        # Filter the files returned by fcm bdiff to just the *.py ones
        args.files = [
            code_file
            for code_file in get_branch_diff_filenames("./", args.base_path)
            if _FILENAME_FILTER.match(code_file)
        ]
    return args


# ------------------------------------------------------------------------------
if __name__ == "__main__":
    # Parse command line options
    OPTS = parse_options()

    # Change the ignore input into a list
    if OPTS.ignore is None:
        OPTS.ignore = []
    else:
        OPTS.ignore = OPTS.ignore.split(",")
        if len(OPTS.ignore) == 1 and OPTS.ignore[0] == "":
            OPTS.ignore = []

    main(OPTS.files, OPTS.ignore)
