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
from textwrap import wrap

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
def load_templates(filter_pattern, template_path):
    """
    Attempt load allowed copyright templates
    """

    loaded_templates = []

    template_files, _ = files_to_process(
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
    ignored = 0
    for root, _, filenames in os.walk(filepath):
        for filename in filenames:
            if filter_pattern.match(filename):
                path_to_file = os.path.join(root, filename)
                if any([ignore in path_to_file for ignore in ignore_list]):
                    ignored += 1
                    continue
                files.append(path_to_file)

    return files, ignored


# ------------------------------------------------------------------------------
def main(inputs, ignore_list, template_path):
    """main program block"""

    templates = []
    regex_templates_raw = []
    regex_templates = []

    filter_tmp = re.compile(r".*\.template$")
    templates.extend(load_templates(filter_tmp, template_path))

    filter_tmp = re.compile(r".*\.regex_template$")
    regex_templates_raw.extend(load_templates(filter_tmp, template_path))

    if not (templates or regex_templates_raw):
        raise SystemExit("[ERROR] no templates or regex templates found")

    for filename, template_lines in regex_templates_raw:
        regex_templates.append((filename, re.compile(r"\n".join(template_lines))))

    files_to_check = []
    ignored_count = 0
    for file_input in inputs:
        if os.path.isfile(file_input):
            if any([ignore in file_input for ignore in ignore_list]):
                ignored_count += 1
                continue
            else:
                files_to_check.append(file_input)
        elif os.path.isdir(file_input):
            files_found, files_ignored = files_to_process(file_input, ignore_list)
            files_to_check.extend(files_found)
            ignored_count += files_ignored
        else:
            raise SystemExit(
                "[ERROR] Input sources must be files/directories"
                + "\n         : "
                + f'"{file_input}" is neither'
            )

    failed_files = []
    for item in files_to_check:
        file_pass = check_file_compliance(item, templates, regex_templates)
        if not file_pass:
            failed_files.append(item)

    fail_count = len(failed_files)
    checked_count = len(files_to_check)
    plural = "s" if fail_count != 1 else ""

    if fail_count > 0:
        banner_print(
            f"Checked {checked_count}, ignored {ignored_count}, "
            f"with {fail_count} failure{plural}\n"
        )
        for filename in failed_files:
            print(os.path.realpath(filename))
        print()
        plural2 = "have" if fail_count != 1 else "is"
        raise SystemExit(
            f"[ERROR] {fail_count} file{plural} "
            + f"{plural2} missing copyright notice{plural}"
        )
    elif not files_to_check:
        message = "[WARNING] "
        if ignored_count == 1:
            message += "only possible file ignored"
        elif ignored_count > 1:
            message += f"all {ignored_count} possible file{plural} ignored"
        else:
            message += "no files found"
        print(message)

    else:
        if checked_count != 1:
            plural = "s"
            plural2 = "have"
        else:
            plural = ""
            plural2 = "has a"
        message = (
            f"[SUCCESS] {checked_count} file{plural} {plural2} valid copyright{plural}"
        )
        if ignored_count > 0:
            plural = "s" if ignored_count != 1 else ""
            message += f" with {ignored_count} file{plural} ignored"
        print(message)


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
    if "CYLC_TASK_WORK_PATH" in os.environ:
        template_path = os.path.join(os.environ["CYLC_TASK_WORK_PATH"], "file", "")
    else:
        template_path = "."
    parser = argparse.ArgumentParser(usage=usage, description=description)
    excl_group = parser.add_mutually_exclusive_group()
    parser.add_argument(
        "--ignore",
        action="store",
        dest="ignore",
        metavar="LIST",
        default=None,
        help=("ignore filename/s containing (comma separated list of patterns)"),
    )
    parser.add_argument(
        "--base_path",
        action="store",
        dest="base_path",
        metavar="DIR",
        default=None,
        help="Override the base path to find the actual file.",
    )
    excl_group.add_argument(
        "--full_trunk",
        action="store_true",
        default=False,
        help=("run on use the full file list when trunk, else run on fcm branch-diff"),
    )
    parser.add_argument(
        "--templates",
        action="store",
        dest="templates",
        metavar="DIR",
        default=template_path,
        help="path to the templates (default: %(default)s)",
    )
    excl_group.add_argument(
        "files",
        nargs="*",
        default=["./"],
        help="File(s) to check and/or directories to recursively search",
    )
    args = parser.parse_args()

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

    main(OPTS.files, OPTS.ignore, OPTS.templates)
