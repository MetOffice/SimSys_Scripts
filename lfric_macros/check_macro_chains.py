#!/usr/bin/env python3
##############################################################################
# (c) Crown copyright 2025 Met Office. All rights reserved.
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
import sys
from pathlib import Path

from apply_macros import ApplyMacros


def find_upgradeable_apps(apps_dir: Path, core_dir: Path) -> dict:
    """
    Loop over rose-stem apps installed into the cylc_workflow and return a list
    of those with metadata imports and therefore available for rose upgrade
    macros.
    Inputs:
        - apps_dir, path to the rose app dir in the cylc-run directory
    Returns:
        - list of app names that can be upgraded
    """
    for start in (apps_dir, core_dir):
        valid_apps = {}
        for app in start.iterdir():
            conf_path = app / "rose-app.conf"
            if not conf_path.is_file():
                continue
            with open(conf_path) as f:
                for line in f:
                    try:
                        version = re.search(r"\s*meta\s*=\s*([\w\.\-\/]+)", line).group(
                            1
                        )
                        valid_apps[app] = version.split("/")
                        break
                    except AttributeError:
                        pass
    return valid_apps


def find_macro_tags(tag: str, path: Path, errors: list) -> set[str]:
    """
    Find tags with format BEFORE_TAG= or AFTER_TAG= in a versions.py file.
    Inputs:
        tag - either before or after
        path - path to the directory containing the versions.py file
    Returns:
        set of tags found in file
    """

    found_tags = set()
    in_comment = False
    with open(path / "versions.py") as f:
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


def compare_tags(before: str, after: str, path: Path, errors: list) -> str | None:
    """
    Check that the before and after tags form a continuous chain. This is done
    by ensuring that only the initial before tag (the version number) and the
    final after tag are not in both sets.
    Inputs:
        before/after, sets of the tags in a given file
    Returns:
        Final after tag in the macro chain
    """

    # Tags in only one of before and after sets
    single_tags = before.symmetric_difference(after)

    # There should be 2 single tags
    if len(single_tags) != 2 and len(single_tags) != 0:
        msg = (
            f"[ERROR] - Found {len(single_tags)} tags in {path / 'versions.py'} that "
            "didn't have a 2nd instance.\nThere should be 2 of these - the beginning "
            "of the chain and the end of the chain.\nThis is likely a typo in the tags "
            "in the versions.py file. The identified tags were:\n"
        )
        msg += "\n".join(x for x in single_tags)
        errors.append(msg)
        return None

    for item in single_tags:
        if item in after:
            return item


def check_fcm() -> None:
    """
    Check if this script is being run for a fcm working copy and fail gracefully
    if so.
    """
    dependency = Path(os.environ["SOURCE_ROOT"]) / "apps" / "dependencies.sh"

    if dependency.exists():
        raise Exception(
            "[ERROR] check_macro_chains.py no longer works with FCM sources. "
            "Please ignore this error until you have migrated your work "
            "to GitHub. Thank you"
        )


def main() -> None:
    """
    Main function of the program
    """
    check_fcm()

    source_apps = Path(os.environ["SOURCE_ROOT"]) / "lfric_apps"
    source_core = Path(os.environ["SOURCE_ROOT"]) / "lfric_core"

    macro_object = ApplyMacros("vn0.0_t0", None, "vn0.0", source_apps, source_core)
    apps_meta_dirs = macro_object.find_meta_dirs(macro_object.root_path / "rose-meta")

    rose_apps = find_upgradeable_apps(
        source_apps / "rose-stem" / "app", source_core / "rose-stem" / "app"
    )
    latest_meta = {}

    errors = []
    for meta_dir in apps_meta_dirs:
        before_tags = find_macro_tags("before", meta_dir, errors)
        after_tags = find_macro_tags("after", meta_dir, errors)

        latest = compare_tags(before_tags, after_tags, meta_dir, errors)
        if latest:
            latest_meta[meta_dir.name] = latest

    if errors:
        for item in errors:
            print(f"{item}\n\n\n\n", file=sys.stderr)
        print("[FAIL] - Found errors in macro chains - please check the job.err")
        exit(1)
    print("[PASS] - Successfully checked all macro chains")

    errors = []
    for app, version in rose_apps.items():
        if version[0] not in latest_meta:
            continue
        if version[1] != latest_meta[version[0]]:
            msg = (
                f"The rose-stem app {app} is using a different macro tag "
                f"'{version[0]}/{version[1]}' compared with the latest upgrade macro, "
                f"'{latest_meta[version[0]]}'. This suggests a macro has not been "
                "successfully applied."
            )
            errors.append(msg)

    if errors:
        for item in errors:
            print(f"{item}\n\n\n\n", file=sys.stderr)
        print("[FAIL] - Found errors in rose app versions - please check the job.err")
        exit(1)
    print("[PASS] - Successfully checked all rose-stem apps")

    # Remove temp directories
    for _, directory in macro_object.temp_dirs.items():
        shutil.rmtree(directory)


if __name__ == "__main__":
    main()
