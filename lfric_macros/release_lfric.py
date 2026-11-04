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
import re
import subprocess
import shutil
import shlex
from pathlib import Path

from apply_macros import (
    ApplyMacros,
    apply_macros_main,
    apply_styling,
    get_root_path,
    read_versions_file,
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


def run_command(command: str, timelimit: int = 120) -> subprocess.CompletedProcess:
    """
    Run a subprocess command and return the result object
    Inputs:
        - command, str with command to run
    Outputs:
        - result object from subprocess.run
    """
    result = subprocess.run(
        shlex.split(command),
        capture_output=True,
        text=True,
        timeout=timelimit,
        check=False,
    )

    if result.returncode:
        raise_exception(result, command)

    return result


def raise_exception(result: subprocess.CompletedProcess, command: str) -> None:
    """
    Raise an exception if a subprocess command has failed
    """

    raise Exception(f"[FAIL] Error running command: '{command}'\n{result.stderr}")


def find_meta_dirs(paths: list[Path], exclude_dirs: tuple[str] = ()) -> set[Path]:
    """
    Return a set of rose-metadata directories that can be found in the apps and
    core sources. Done by seaching for rose-meta.conf files. Records the parent
    directory of the current one, as rose-meta.conf files end up in 'HEAD' or 'vnX.Y'
    directories.
    """

    dirs = set()
    for path in paths:
        print("[INFO] Finding rose metadata directories in", path)
        for dirpath, dirnames, filenames in path.walk():
            dirnames[:] = [d for d in dirnames if d not in exclude_dirs]
            if "rose-meta.conf" in filenames:
                dirs.add(dirpath.parent)
    return dirs


def update_version_number(apps: Path, version: str) -> None:
    """
    Update the "VN" variable number in the lfric_apps rose-suite.conf file, to
    be the new version number
    """

    print("[INFO] Updating rose-suite.conf version number")

    fpath = apps / "rose-stem" / "rose-suite.conf"
    with open(fpath, "r") as f:
        lines = f.readlines()

    for i, line in enumerate(lines):
        line = line.strip()
        if line.startswith("VN="):
            line = f"VN='{version.removeprefix('vn')}'"
            lines[i] = line
            break

    with open(fpath, "w") as f:
        for line in lines:
            f.write(line)


def update_variables_files(apps: Path) -> None:
    """
    Edit meto variables_platforms.cylc files to remove any ticket updates
    """

    meto_path = apps / "rose-stem" / "site" / "meto"
    variables_files = set()
    for filename in meto_path.iterdir():
        if str(filename).startswith("variables_"):
            variables_files.add(meto_path / filename)

    for fpath in variables_files:
        with open(fpath, "r") as f:
            lines = f.readlines()

        for i, line in enumerate(lines):
            if "BASE~" in line:
                line = line.split("BASE~")[0] + "BASE,\n"
                lines[i] = line

        with open(fpath, "w") as f:
            for line in lines:
                f.write(line)


def get_user() -> str:
    """
    Return a str of username with .'s replaced by ' '
    """
    return getpass.getuser().replace(".", " ")


def add_new_upgrade_macro(
    meta_dirs: list[Path],
    old_version: str,
    version: str,
    ticket: str,
    macro_object: ApplyMacros,
) -> None:
    """
    Write out a new macro, updating to vnX.Y
    Use the template macro in the MACRO_TEMPLATE variable above
    Loop over all meta_dirs, work out before tag, and write in new macro
    """

    print("[INFO] Writing new upgrade macro")

    template_macro = MACRO_TEMPLATE

    # Replace Consistent Variables for all meta directories
    class_name = f"{old_version.replace('.', '')}_t{ticket}"
    template_macro = template_macro.replace("CLASS_NAME", class_name)
    template_macro = template_macro.replace("TICKET", ticket)
    template_macro = template_macro.replace("AUTHOR", get_user())
    template_macro = template_macro.replace("AFTER_EDIT", version)

    for meta_dir in meta_dirs:
        versions_file = meta_dir / "versions.py"

        macros = read_versions_file(meta_dir)
        macros = split_macros(macros)
        last_after_tag = macro_object.find_last_macro(macros, meta_dir)

        # Update Before Tag with found After Tag
        meta_dir_macro = template_macro.replace("BEFORE_EDIT", last_after_tag)

        # Write out new macro
        with open(versions_file, "a") as f:
            f.write(f"\n{meta_dir_macro}")


def copy_head_meta(meta_dirs: list[Path], apps: Path, core: Path, version: str) -> None:
    """
    Copy the HEAD metadata to vnX.Y/ for all meta_dirs
    """

    print("[INFO] Copying HEAD metadata")

    for meta_dir in meta_dirs:
        head = meta_dir / "HEAD"
        new = meta_dir / version
        shutil.copytree(head, new)
        if core in new.parents:
            new = new.relative_to(core)
            command = f"git -C {core} add {new}"
        elif apps in new.parents:
            new = new.relative_to(apps)
            command = f"git -C {apps} add {new}"
        _ = run_command(command)


def update_meta_import_path(
    meta_dirs: list[Path], version: str, jules_version: str
) -> None:
    """
    Change HEAD to vnX.Y in meta import statements in the newly created
    vnX.Y/rose-meta.conf files
    """

    print("[INFO] Updating metadata import statements")

    for meta_dir in meta_dirs:
        meta_file = meta_dir / version / "rose-meta.conf"
        with open(meta_file) as f:
            lines = f.readlines()

        in_imports = False
        for i, line in enumerate(lines):
            if line.strip().startswith("import="):
                in_imports = True
            elif in_imports and not line.strip().startswith("="):
                break
            if in_imports:
                if "jules-lfric" in line:
                    line = line.replace("HEAD", jules_version)
                else:
                    line = line.replace("HEAD", version)
                lines[i] = line

        with open(meta_file, "w") as f:
            for line in lines:
                f.write(line)


def copy_versions_files(
    meta_dirs: list[Path], old_version: str, version: str, apps: Path, core: Path
) -> str:
    """
    Copy versions.py files to versionAB_XY.py
    Returns the name of the AB->XY versions files.
    """

    upgrade_name = (
        f"version{old_version.replace('.', '').replace('vn', '')}_"
        f"{version.replace('.', '').replace('vn', '')}.py"
    )

    print("[INFO] Copying versions.py files to versionAB_XY.py files")

    for meta_dir in meta_dirs:
        versions_file = meta_dir / "versions.py"
        upgrade_file = meta_dir / upgrade_name
        if not versions_file.exists():
            raise FileNotFoundError(f"The file {versions_file} doesn't exist")
        shutil.copyfile(versions_file, upgrade_file)
        if core in upgrade_file.parents:
            upgrade_file = upgrade_file.relative_to(core)
            command = f"git -C {core} add {upgrade_file}"
        elif apps in upgrade_file.parents:
            upgrade_file = upgrade_file.relative_to(apps)
            command = f"git -C {apps} add {upgrade_file}"
        _ = run_command(command)

    return upgrade_name


def add_new_import(versions_file: Path, upgrade_name: str) -> None:
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

    # Run isort (part of apply_styling) to move the newly added import to the
    # correct place in the import list
    apply_styling(versions_file)


def update_versions_file(meta_dirs: list[Path], upgrade_name: str) -> None:
    """
    - Add import of versionAB_XY.py file to the template_versions.py
    - Replace old versions.py files with that file
    """

    print("[INFO] Updating versions.py files")

    template_path = Path(__file__).absolute().parent / "files" / "template_versions.py"

    for meta_dir in meta_dirs:
        versions_file = meta_dir / "versions.py"
        shutil.copyfile(template_path, versions_file)
        add_new_import(versions_file, upgrade_name)


def update_versions_mod(repo: str, filepath: Path, version: str) -> None:
    """
    Update the versions numbers in lfric versions mod files
    """

    print(f"[INFO] Updating lfric_{repo}_versions_mod.F90")

    version = version.removeprefix("vn").split(".")
    updates = {
        "major": version[0],
        "minor": version[1],
        "patch": "0",
        "release": ".true.",
    }

    lines = filepath.read_text().split("\n")
    for i, line in enumerate(lines):
        if len(updates) == 0:
            break
        for item, val in updates.items():
            if re.match(rf".*::\s*lfric_{repo}_{item}_version\s*=", line):
                lines[i] = f"{line.split('=')[0]} = {val}"
                del updates[item]
                break

    filepath.write_text("\n".join(lines))


def ticket_number(opt: str) -> str:
    """
    Check that the command line supplied ticket number is of a suitable format
    """
    if not re.match(r"\d+", opt):
        raise argparse.ArgumentTypeError(
            f"The ticket number '{opt}' does not conform to the 'TTT' format."
            "Please modify the command line argument and rerun."
        )
    return opt


def parse_args() -> argparse.Namespace:
    """
    Read command line args
    """

    parser = argparse.ArgumentParser("Move and edit files for lfric_apps release")
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
        "-j",
        "--jules_version",
        required=True,
        help="The newly released version of Jules for jules-lfric metadata imports "
        "(format X.Y)",
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
        default=Path(".").absolute(),
        type=Path,
        help="The path to the LFRic Apps working copy being used. Defaults to the "
        "location the script is being run from - this assumes you are in an Apps clone",
    )
    parser.add_argument(
        "-c",
        "--core",
        required=True,
        type=Path,
        help="Path to the LFRic Core working copy being used.",
    )
    parser.add_argument(
        "-p", "--processes", type=int, default=4, help="Number of processes to use"
    )
    args = parser.parse_args()

    args.apps = get_root_path(args.apps)
    args.core = args.core.expanduser().absolute()
    args.version = f"vn{args.version}"
    args.old_version = f"vn{args.old_version}"
    args.jules_version = f"vn{args.jules_version}"

    return args


def main() -> None:
    args = parse_args()

    macro_object = ApplyMacros(
        args.version,
        None,
        args.old_version.removeprefix("vn"),
        args.apps,
        args.core,
    )

    # Find all metadata directories, excluing lfric-inputs as this has metadata but no
    # macros.
    exclude_dirs = (
        ".git",
        "rose-stem",
        "integration-test",
        "lfricinputs",
    )
    meta_dirs = find_meta_dirs([args.apps, args.core], exclude_dirs)

    update_version_number(args.apps, args.version)

    update_variables_files(args.apps)

    add_new_upgrade_macro(
        meta_dirs, args.old_version, args.version, args.ticket, macro_object
    )

    # Run the apply_macros script
    apply_macros_main(
        args.version,
        f"{args.old_version.replace('.', '')}_t{args.ticket}",
        args.old_version,
        args.apps,
        args.core,
        nproc=args.processes,
    )
    print("\n[INFO] Successfully upgraded apps")

    copy_head_meta(meta_dirs, args.apps, args.core, args.version)

    update_meta_import_path(meta_dirs, args.version, args.jules_version)

    upgrade_file_name = copy_versions_files(
        meta_dirs, args.old_version, args.version, args.apps, args.core
    )

    update_versions_file(meta_dirs, upgrade_file_name)

    update_versions_mod(
        "apps",
        args.apps
        / "science"
        / "shared"
        / "source"
        / "utilities"
        / "lfric_apps_version_mod.f90",
        args.version,
    )
    update_versions_mod(
        "core",
        args.core
        / "infrastructure"
        / "source"
        / "utilities"
        / "lfric_core_version_mod.f90",
        args.version,
    )


if __name__ == "__main__":
    main()
