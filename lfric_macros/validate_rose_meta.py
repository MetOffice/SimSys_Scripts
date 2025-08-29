#!/usr/bin/env python3
##############################################################################
# (c) Crown copyright 2024 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
"""
Validate the rose metadata in lfric_apps
"""

import os
import sys
import subprocess
import argparse

# A list of invalid metadata sections. Most are invalid as they are imported by
# lfric-gungho but also use the files namelist contained there, creating a circular
# dependency
INVALID_METADATA = [
    "jules-lfric",
    "lfric-jules-shared",
    "socrates-radiation",
    "um-aerosol",
    "um-boundary_layer",
    "um-chemistry",
    "um-cloud",
    "um-convection",
    "um-iau",
    "um-microphysics",
    "um-orographic_drag",
    "um-spectral_gwd",
    "um-stochastic_physics",
]

# A list of invalid rose-stem apps to ignore when checking metadata validity. These are
# mostly apps required for lfric coupled which use metadata from other repositories
INVALID_APPS = [
    "fcm_make_drivers",
    "fcm_make_ocean",
    "fcm_make_river",
    "lfric_coupled_rivers",
]


def run_command(command, shell=False, env=None):
    """
    Run a subprocess command and return the result object
    Inputs:
        - command, str with command to run
    Outputs:
        - result object from subprocess.run
    """
    if not shell:
        command = command.split()
    if not env:
        env = os.environ
    return subprocess.run(
        command,
        capture_output=True,
        text=True,
        timeout=120,
        shell=shell,
        check=False,
        env=env,
    )


def check_rose_metadata(rose_meta_path, source_path):
    """
    Auto find rose-meta sections from the top level rose-meta directory and run `rose
    metadata-check` on each
    """

    print("\n\n[INFO] - Checking rose metadata sections\n\n")
    failed = False

    # Add ROSE_META_PATH to env for rose metadata-check command
    my_env = os.environ.copy()
    my_env["ROSE_META_PATH"] = rose_meta_path

    start_dir = os.path.join(source_path, "rose-meta")
    dirs = os.listdir(start_dir)
    for section in dirs:
        if section in INVALID_METADATA:
            continue
        meta_dir = os.path.join(start_dir, section, "HEAD")
        command = f"rose metadata-check --verbose -C {meta_dir}"
        result = run_command(command, env=my_env)
        if result.returncode:
            print(f"[FAIL] - {section} failed to validate")
            print(
                f"Failure running rose metata-check on {section}:\n{result.stderr}",
                file=sys.stderr,
            )
            failed = True
        else:
            print(f"[PASS] - {section} validated")

    return failed


def parse_suite_controlled(err_msg):
    """
    Remove any app validation error messages resulting from suite_controlled option
    configs
    """

    err = []
    split_err = err_msg.split("\n")
    i = 0
    while i < len(split_err):
        line = split_err[i]
        if "opts=suite_controlled" in line:
            i += 2
            continue
        if line.strip():
            err.append(line)
        i += 1
    # There is always a single line in the error stating the number of failures.
    # So only return a non-empty list if there is more than just that line
    if len(err) > 1:
        return err
    return []


def check_rose_stem_apps(meta_paths, source_path):
    """
    Auto find rose-stem apps that use rose metadata and validate these using 'rose
    macro --validate'
    """

    print("\n\n[INFO] - Validating rose-stem apps\n\n")
    failed = False

    start_dir = os.path.join(source_path, "rose-stem", "app")
    apps = os.listdir(start_dir)
    for app in apps:
        if app in INVALID_APPS:
            continue
        app_dir = os.path.join(start_dir, app)
        conf_file = os.path.join(app_dir, "rose-app.conf")
        with open(conf_file, "r") as f:
            for line in f:
                if line.startswith("meta="):
                    break
            else:
                # We reach here if the for loop hasn't been broken out of. In that case
                # we don't want to execut the code below.
                continue
        command = f"rose macro --validate {meta_paths} -C {app_dir} --no-warn version"
        result = run_command(command)
        # Check that errors are only from rose-app-suite-controlled.conf
        errors = []
        if result.returncode:
            errors = parse_suite_controlled(result.stderr)
            errors = "\n".join(e for e in errors)

        if errors:
            print(f"[FAIL] - {app} failed to validate")
            print(f"Failure validating app {app}:\n{errors}", file=sys.stderr)
            failed = True
        else:
            print(f"[PASS] - {app} validated")

    return failed


def parse_args():
    """
    Read command line args
    """

    parser = argparse.ArgumentParser(
        "Validate rose-metadata in LFRic Meta files and apps"
    )
    parser.add_argument(
        "-a",
        "--apps",
        default=None,
        help="The path to the LFRic Apps source. At least one of this or Core are "
        "required. If both are provided then it will be assumed that Apps is the "
        "repository to check.",
    )
    parser.add_argument(
        "-c",
        "--core",
        default=None,
        help="The path to the LFRic Core source. At least one of this or Apps are "
        "required. If both are provided then it will be assumed that Apps is the "
        "repository to check.",
    )

    args = parser.parse_args()

    if args.apps:
        args.apps = os.path.expanduser(args.apps)
    if args.core:
        args.core = os.path.expanduser(args.core)

    return args


def main():
    """
    main function for this script
    """
    args = parse_args()

    meta_paths = ""
    rose_meta_path = ""
    if args.apps or args.core:
        if args.apps:
            source_path = args.apps
            meta_paths += f"-M {os.path.join(args.apps, "rose-meta")}"
            rose_meta_path += args.apps
        elif args.core:
            source_path = args.core
            meta_paths += f"-M {os.path.join(args.core, "rose-meta")} "
            if rose_meta_path:
                rose_meta_path += f":{args.core}"
            else:
                rose_meta_path = args.core
    else:
        raise RuntimeError(
            "At least one of the Apps or Core sources must be provided as a command "
            "line argument"
        )

    if check_rose_metadata(rose_meta_path, source_path) or check_rose_stem_apps(
        meta_paths, source_path
    ):
        sys.exit("There were metadata validation failures. See output for details")
    print("All metadata successfully validated")


if __name__ == "__main__":
    main()
