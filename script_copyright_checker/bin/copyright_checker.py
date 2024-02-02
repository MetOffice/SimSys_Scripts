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
import re
import os
import sys
import subprocess

from fcm_bdiff import (get_branch_diff_filenames, text_decoder, is_trunk,
                       use_mirror, get_branch_info, get_url)
import argparse
from textwrap import wrap

# Desired maximum column width for output - we make an exception
# for filenames, which are always printed on a single line to aid
# ease of selection by the user
_OUTPUT_LINE_WIDTH = 80

# Pattern which will match the intended input files
_FILENAME_FILTER = re.compile(r".*\.(py|c|F90|f90|h|sh)$")

_TEMPLATES = []


# ------------------------------------------------------------------------------
def banner_print(message, maxwidth=_OUTPUT_LINE_WIDTH, char="%"):
    """
    Simple routine which prints a banner message
    """
    wrap_message = [char + " " + elt.ljust(maxwidth-4) + " " + char
                    for elt in wrap(message, width=maxwidth-4)]
    print("\n{0:s}\n{1:s}\n{0:s}".format(char*maxwidth,
                                         "\n".join(wrap_message)))


# ------------------------------------------------------------------------------
def load_templates():
    """
    Attempt load allowed copyright templates
    """

    global _FILENAME_FILTER

    template_path = "."
    if "CYLC_TASK_WORK_PATH" in os.environ:
        template_path = os.environ["CYLC_TASK_WORK_PATH"]

    filter_tmp = _FILENAME_FILTER
    _FILENAME_FILTER = re.compile(r".*\.template$")
    template_files = files_to_process(template_path, [])
    _FILENAME_FILTER = filter_tmp

    for filename in template_files:
        with open(filename) as file:
            lines = file.read().splitlines()
            _TEMPLATES.append((filename, lines))


# ------------------------------------------------------------------------------
def template_is_in_file(file, template):
    """
    Check if the contemt of a template exists somehwere within the file.
    """
    for i in range(len(file) - len(template) + 1):
        if all(template[j] == file[i+j] for j in range(len(template))):
            return True
    return False


# ------------------------------------------------------------------------------
def check_file_compliance(filename):
    """
    Attempt to compile the file and look for error messages.
    Note : compileall returns a 0 return code if the file does NOT exist
         : This may seem a little dangerous, but the assumption is that
         : 'filename' was found/generated elsewhere and is known to exist
         : by this point.
    """

    found_template = False

    with open(filename) as file:
        lines = file.read().splitlines()
        for _, template in _TEMPLATES:
            if not found_template:
                if template_is_in_file(lines, template):
                    found_template = True

    return found_template


# ------------------------------------------------------------------------------
def files_to_process(filepath, ignore_list):
    """
    Generate list of files in given filepath.  Ignore any files matching
    the patterns in the ignore list
    """
    files = []
    for root, _, filenames in os.walk(filepath):
        for filename in filenames:
            if _FILENAME_FILTER.match(filename):
                path_to_file = os.path.join(root, filename)
                if any([ignore in path_to_file for ignore in ignore_list]):
                    print("WARNING: Ignoring file: {0:s}".format(path_to_file))
                    continue
                files.append(path_to_file)

    return files


# ------------------------------------------------------------------------------
def main(inputs, ignore_list):
    """ main program block """

    banner_print("Running copyright checker")

    load_templates()

    files_to_check = []
    for file_input in inputs:
        if os.path.isfile(file_input):
            if any([ignore in file_input for ignore in ignore_list]):
                print("WARNING: Ignoring file: {0:s}".format(file_input))
                continue
            else:
                print("Source (file): {0:s}".format(file_input))
                files_to_check.append(file_input)
        elif os.path.isdir(file_input):
            print("Source (dir) : {0:s}".format(file_input))
            files_to_check.extend(files_to_process(file_input, ignore_list))
        else:
            raise SystemExit("[ERROR] Input sources must be files/directories"
                             + "\n         : "
                             + "\"{0:}\" is neither".format(file_input))

    print("\nFound {0:d} files to check".format(len(files_to_check)))

    failed_files = []
    for item in files_to_check:
        print("file : {0:}".format(item))
        file_pass = check_file_compliance(item)
        if not file_pass:
            failed_files.append(item)

    fail_count = len(failed_files)
    banner_print("Checks completed with {0:d} failure{1:s}\n"
                 .format(fail_count, "s" if fail_count != 1 else ""))

    if fail_count > 0:
        print(": Failed file{0:s} :"
              .format("s" if fail_count != 1 else ""))
        for filename in failed_files:
            full_fname = os.path.realpath(filename)
            banner_print(full_fname, maxwidth=(len(full_fname) + 10), char='#')
            print("")
        print("")
        raise SystemExit("[ERROR] {0:d} {1:s}"
                         .format(fail_count,
                                 "files have missing copyright notices"
                                 if fail_count != 1 else
                                 "file is missing a copyright notice"))


# ------------------------------------------------------------------------------
def parse_options():
    """Parse command line code options."""
    usage = "usage: %(prog)s [options] directory/file1 [[directory/file2] ...]"
    description = ("This script will scan for files to "
                   "check for copyright notices. "
                   "Arguments may be any combination of files and/or "
                   "directories; "
                   "individual files will be scanned, and all files within "
                   "directories and their sub-directories will be scanned.")
    parser = argparse.ArgumentParser(usage=usage, description=description)
    excl_group = parser.add_mutually_exclusive_group()
    parser.add_argument("--ignore", action="store", dest="ignore",
                        default=None,
                        help=("ignore filename/s containing "
                              "(comma separated list of patterns)"))
    parser.add_argument("--base_path", action="store", dest="base_path",
                        default=None,
                        help="Override the base path to find the actual file.")
    excl_group.add_argument("--full_trunk", action="store_true", default=False,
                            help=("run on use the full file list when trunk, "
                                  "else run on fcm branch-diff"))
    excl_group.add_argument("--bdiff", action="store_true", default=False,
                            help="run on an fcm branch-diff")
    excl_group.add_argument('files', nargs='*', default=['./'],
                            help="File(s) to check and/or directories to "
                                 "recursively search")
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
        args.files = [code_file
                      for code_file in
                      get_branch_diff_filenames("./", args.base_path)
                      if _FILENAME_FILTER.match(code_file)]
    return args


# ------------------------------------------------------------------------------
if __name__ == '__main__':

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
