#!/usr/bin/env python3
##############################################################################
# (c) Crown copyright 2024 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
"""
Script to check that all version.py macro chains appear valid and that the final
after tag of the chain matches metadata import statement for each rose app.
Only intended to be run from rose-stem
"""

import os
import re
import shutil
import subprocess
import sys
from apply_macros import ApplyMacros


def run_command(command):
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
        timeout=120,
        shell=False,
        check=False,
    )


def find_upgradeable_apps(apps_dir):
    """
    Loop over rose-stem apps installed into the cylc_workflow and return a list
    of those with metadata imports and therefore available for rose upgrade
    macros.
    Inputs:
        - apps_dir, path to the rose app dir in the cylc-run directory
    Returns:
        - list of app names that can be upgraded
    """

    valid_apps = []
    for app in os.listdir(apps_dir):
        conf_path = os.path.join(apps_dir, app, "rose-app.conf")
        if not os.path.isfile(conf_path):
            continue
        grep_com = f'grep -E "meta=.*" {conf_path}'
        result = run_command(grep_com)
        if "meta=" in result.stdout:
            valid_apps.append(app)

    return valid_apps


def find_macro_tags(tag, path, errors):
    """
    Find tags with format BEFORE_TAG= or AFTER_TAG= in a versions.py file.
    Inputs:
        tag - either before or after
        path, path to the directory containing the versions.py file
    Returns:
        set of tags found in file
    """

    found_tags = set()
    in_comment = False
    with open(os.path.join(path, "versions.py")) as f:
        for line in f:
            line = line.strip()
            # Check whether this is a comment
            for _ in range(line.count('"""')):
                in_comment = not in_comment
            if in_comment:
                continue
            result = re.search(rf'^\s*{tag.upper()}_TAG\s*=\s*["\'](\S+)["\']', line)
            if result:
                if result.group(1) in found_tags:
                    errors.append(
                        f"[ERROR] - Found 2 instances of the {tag.capitalize()}"
                        f"tag {result.group(1)} in versions.py file located "
                        f"at {path}"
                    )
                found_tags.add(result.group(1))

    return found_tags


def compare_tags(before, after, path, errors):
    """
    Check that the before and after tags form a continuous chain. This is done
    by ensuring that only the initial before tag (the version number) and the
    final after tag are not in both sets.
    Inputs:
        before/after, sets of the tags in a given file
    """

    # Tags in only one of before and after sets
    single_tags = before.symmetric_difference(after)

    # There should be 2 single tags
    if len(single_tags) != 2 and len(single_tags) != 0:
        errors.append(
            f"[ERROR] - Found {len(single_tags)} unique before or after tags in "
            f"{os.path.join(path, 'versions.py')} that were ONLY a before or "
            "after tag.\nThere should be 2 of these - the beginning of the "
            "chain and the end of the chain.\nThis is likely a typo in the tags in "
            "the versions.py file. The identified tags were:\n"
            f"{'\n'.join(x for x in single_tags)}"
        )


def main():
    """
    Main function of the program
    """

    source_apps = os.path.join(os.environ["SOURCE_ROOT"], "apps")
    source_core = os.path.join(os.environ["SOURCE_ROOT"], "core")

    macro_object = ApplyMacros(
        "vn0.0_t0", None, "vn0.0", source_apps, source_core, None
    )
    macro_object.find_meta_dirs(os.path.join(macro_object.root_path, "applications"))
    macro_object.meta_dirs

    errors = []
    for meta_dir in macro_object.meta_dirs:

        before_tags = find_macro_tags("before", meta_dir, errors)
        after_tags = find_macro_tags("after", meta_dir, errors)

        compare_tags(before_tags, after_tags, meta_dir, errors)

    # Remove temp directories
    for _, directory in macro_object.temp_dirs.items():
        shutil.rmtree(directory)

    if errors:
        for item in errors:
            print(f"{item}\n\n\n\n", file=sys.stderr)
        print("[FAIL] - Found errors in macro chains - please check the job.err")
        exit(1)

    print("[PASS] - Successfully checked all macro chains")


if __name__ == "__main__":
    main()
