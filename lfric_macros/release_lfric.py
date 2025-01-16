#!/usr/bin/env python3
##############################################################################
# (c) Crown copyright 2025 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################

"""
Release a new version of LFRic by:
- update VN variable in rose-suite.conf to X.Y
- add an upgrade macro from vnA.B_tTTT to vnX.Y
- apply that macro
- copying HEAD metadata to vnX.Y
- update metadata imports to use vnX.Y metadata in new rose-app.confs
- copying versions.py to versions_A.B_to_X.Y.py
- set versions.py files to be blank
"""

import argparse
import getpass
import os
import re
import socket
import subprocess

from apply_macros import (
    ApplyMacros,
    apply_macros_main,
    get_root_path,
    read_versions_file,
    run_black,
    split_macros,
    version_number,
)

MACRO_TEMPLATE = """
class CLASS_NAME(MacroUpgrade):
    # Upgrade macro for TICKET by AUTHOR

    BEFORE_TAG = "BEFORE_EDIT"
    AFTER_TAG = "AFTER_EDIT"

    def upgrade(self, config, meta_config=None):
        return config, self.reports
"""


def run_command(command, timelimit=120):
    """
    Run a subprocess command and return the result object
    Inputs:
        - command, str with command to run
    Outputs:
        - result object from subprocess.run
    """
    return subprocess.run(
        command.split(),
        capture_output=True,
        text=True,
        timeout=timelimit,
        check=False,
    )


def raise_exception(result, command):
    """
    Raise an exception if a subprocess command has failed
    """

    raise Exception(
        f"[FAIL] Error running command: '{command}'\n" f"{result.stderr}"
    )


def set_dependency_path(args):
    """
    Edit an LFRic Apps dependencies.sh file so that it points at the provided
    LFRic Core source
    """

    print("[INFO] Updating dependencies.sh Core source")

    hostname = socket.gethostname()
    dep_path = os.path.join(args.apps, "dependencies.sh")
    with open(dep_path) as f:
        lines = f.readlines()
    for i, line in enumerate(lines):
        if line.strip().startswith("export lfric_core_rev"):
            lines[i] = "export lfric_core_rev=\n"
        if line.strip().startswith("export lfric_core_sources"):
            lines[i] = (
                "export lfric_core_sources="
                f"{hostname}:{os.path.abspath(args.core)}\n"
            )
    with open(dep_path, "w") as f:
        f.write("".join(x for x in lines))


def find_meta_dirs(paths):
    """
    Return a set of rose-metadata directories that can be found in the apps and
    core sources. Done by seaching for rose-meta.conf files. Records the parent
    directory of the current one, as rose-meta.conf files end up in HEAD/vnX.Y
    directories.
    """

    print("[INFO] Finding rose metadata directories")

    dirs = set()
    for path in paths:
        for dirpath, dirnames, filenames in os.walk(path):
            dirnames[:] = [
                d
                for d in dirnames
                if d not in [".svn", "rose-stem", "integration-test"]
            ]
            if "rose-meta.conf" in filenames:
                dirs.add(os.path.dirname(dirpath))
    return dirs


def update_version_number(args):
    """
    Update the "VN" variable number in the lfric_apps rose-suite.conf file, to
    be the new version number
    """

    print("[INFO] Updating rose-suite.conf version number")

    fpath = os.path.join(args.apps, "rose-stem", "rose-suite.conf")
    with open(fpath, "r") as f:
        lines = f.readlines()

    for i, line in enumerate(lines):
        line = line.strip()
        if line.startswith("VN="):
            line = f"VN='{args.version.removeprefix('vn')}'"
            lines[i] = line
            break

    with open(fpath, "w") as f:
        for line in lines:
            f.write(line)


def get_user():
    """
    Return a str of username with .'s replaced by ' '
    """
    return getpass.getuser().replace(".", " ")


def add_new_upgrade_macro(meta_dirs, args, macro_object):
    """
    Write out a new macro, updating to vnX.Y
    Use the template macro in the MACRO_TEMPLATE variable above
    Loop over all meta_dirs, work out before tag, and write in new macro
    """

    print("[INFO] Writing new upgrade macro")

    template_macro = MACRO_TEMPLATE

    # Replace Consistent Variables for all meta directories
    class_name = f"{args.old_version.replace('.', '')}_t{args.ticket}"
    template_macro = template_macro.replace("CLASS_NAME", class_name)
    template_macro = template_macro.replace("TICKET", args.ticket)
    template_macro = template_macro.replace("AUTHOR", get_user())
    template_macro = template_macro.replace("AFTER_EDIT", args.version)

    for meta_dir in meta_dirs:
        versions_file = os.path.join(meta_dir, "versions.py")

        macros = read_versions_file(meta_dir)
        macros = split_macros(macros)
        last_after_tag = macro_object.find_last_macro(macros, meta_dir)

        # Update Before Tag with found After Tag
        meta_dir_macro = template_macro.replace("BEFORE_EDIT", last_after_tag)

        # Write out new macro
        with open(versions_file, "a") as f:
            f.write(f"\n{meta_dir_macro}")


def copy_head_meta(meta_dirs, args):
    """
    Copy the HEAD metadata to vnX.Y/ for all meta_dirs
    """

    print("[INFO] Copying HEAD metadata")

    for meta_dir in meta_dirs:
        head = os.path.join(meta_dir, "HEAD")
        new = os.path.join(meta_dir, args.version)
        command = f"fcm cp {head} {new}"
        result = run_command(command)
        if result.returncode:
            raise_exception(result, command)


def update_meta_import_path(meta_dirs, args):
    """
    Change HEAD to vnX.Y in meta import statements in the newly created
    vnX.Y/rose-meta.conf files
    """

    print("[INFO] Updating metadata import statements")

    for meta_dir in meta_dirs:
        meta_file = os.path.join(meta_dir, args.version, "rose-meta.conf")
        with open(meta_file) as f:
            lines = f.readlines()

        in_imports = False
        for i, line in enumerate(lines):
            if line.strip().startswith("import="):
                in_imports = True
            elif in_imports and not line.strip().startswith("="):
                break
            if in_imports:
                line = line.replace("HEAD", args.version)
                lines[i] = line

        with open(meta_file, "w") as f:
            for line in lines:
                f.write(line)


def copy_versions_files(meta_dirs, args):
    """
    Copy versions.py files to versionAB_XY.py
    Returns the name of the AB->XY versions files.
    """

    upgrade_name = (
        f"version{args.old_version.replace('.', '').replace('vn', '')}_"
        f"{args.version.replace('.', '').replace('vn', '')}.py"
    )

    print("[INFO] Copying versions.py files to versionAB_XY.py files")

    for meta_dir in meta_dirs:
        versions_file = os.path.join(meta_dir, "versions.py")
        upgrade_file = os.path.join(meta_dir, upgrade_name)
        command = f"fcm cp {versions_file} {upgrade_file}"
        result = run_command(command)
        if result.returncode:
            raise_exception(result, command)

    return upgrade_name


def add_new_import(versions_file, upgrade_name):
    """
    Read through a versions.py file, finding the line that imports MacroUpgrade
    from rose. Add the new `from .versionsAB_XY import *` import after that line
    and then run isort and black to correctly format the file, moving the new
    import into the correct place.
    """

    upgrade_import = upgrade_name.removesuffix(".py")

    with open(versions_file, "r") as f:
        lines = f.readlines()

    for i, line in enumerate(lines):
        if "MacroUpgrade" in line:
            insert_pos = i + 1
            break
    else:
        raise Exception(
            "Failed to find the the import of 'MacroUpgrade' in the file "
            f"{versions_file}. This import is required for all versions.py "
            "and is used here to locate where to insert a new import."
        )

    lines.insert(insert_pos, f"from .{upgrade_import} import *\n")

    with open(versions_file, "w") as f:
        for line in lines:
            f.write(line)

    run_black(versions_file)


def remove_macro_classes(versions_file):
    """
    Delete all macro classes from the versions.py file
    Do this by removing all lines below the start of the first macro
    """

    with open(versions_file, "r") as f:
        lines = f.readlines()

    in_comment = False
    end_line = len(lines)
    for i, line in enumerate(lines):
        # Skip docstring comments
        if '"""' in line:
            for _ in range(line.count('"""')):
                in_comment = not in_comment
        if in_comment:
            continue
        # If we match the pattern of an upgrade macro, break here
        if re.search(r"vn\d+(_t\d+\w*)?", line):
            end_line = i
            break

    lines = lines[:end_line]

    with open(versions_file, "w") as f:
        for line in lines:
            f.write(line)


def update_versions_file(meta_dirs, upgrade_name):
    """
    Reset the versions.py files so they appear as they should during a release
    - Add import of versionAB_XY.py file
    - Remove macros
    """

    print("[INFO] Updating versions.py files")

    for meta_dir in meta_dirs:
        versions_file = os.path.join(meta_dir, "versions.py")
        add_new_import(versions_file, upgrade_name)
        remove_macro_classes(versions_file)


def ticket_number(opt):
    """
    Check that the command line supplied ticket number is of a suitable format
    """
    if not re.match(r"\d+", opt):
        raise argparse.ArgumentTypeError(
            f"The ticket number '{opt}' does not conform to the 'X.Y' format."
            "Please modify the command line argument and rerun."
        )
    return opt


def parse_args():
    """
    Read command line args
    """

    parser = argparse.ArgumentParser(
        "Move and edit files for lfric_apps release"
    )
    parser.add_argument(
        "-o",
        "--old_version",
        required=True,
        type=version_number,
        help="The old version number we are updating from (format X.Y)",
    )
    parser.add_argument(
        "-v",
        "--version",
        required=True,
        type=version_number,
        help="The new version number we are updating to (format X.Y)",
    )
    parser.add_argument(
        "-t",
        "--ticket",
        required=True,
        type=ticket_number,
        help="The ticket number of the release ticket",
    )
    parser.add_argument(
        "-a",
        "--apps",
        default=".",
        help="The path to the LFRic Apps working copy being used. Defaults to  "
        "the location the script is being run from - this assumes you are in a "
        "working copy.",
    )
    parser.add_argument(
        "-c",
        "--core",
        required=True,
        help="Path to the LFRic Core working copy being used.",
    )
    args = parser.parse_args()

    args.apps = os.path.abspath(args.apps)
    args.apps = get_root_path(args.apps)
    args.core = os.path.abspath(args.core)
    args.version = f"vn{args.version}"
    args.old_version = f"vn{args.old_version}"

    return args


def main():

    args = parse_args()

    macro_object = ApplyMacros(
        args.version,
        None,
        args.old_version.removeprefix("vn"),
        args.apps,
        args.core,
        None,
    )

    set_dependency_path(args)

    meta_dirs = find_meta_dirs([args.apps, args.core])

    update_version_number(args)

    add_new_upgrade_macro(meta_dirs, args, macro_object)

    # Create dictinary of arguments expected by apply_macros_main
    # Then run the apply_macros script with those arguments
    macro_args = {
        "tag": args.version,
        "cname": f"{args.old_version.replace('.', '')}_t{args.ticket}",
        "version": args.old_version,
        "apps": args.apps,
        "core": args.core,
        "jules": None,
    }
    apply_macros_main(macro_args)
    print("\n[INFO] Successfully upgraded apps")

    copy_head_meta(meta_dirs, args)

    update_meta_import_path(meta_dirs, args)

    upgrade_file_name = copy_versions_files(meta_dirs, args)

    update_versions_file(meta_dirs, upgrade_file_name)


if __name__ == "__main__":
    main()
