#!/usr/bin/env python3
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

"""
Script to generate a cron file, from which nightly testing is run.
Cron jobs are based on a provided yaml config file (-c command line option) and
the resulting cron file is installed if the --install option is added.
See https://metoffice.github.io/simulation-systems/Reviewers/nightlytesting.html
Used for nightly testing for UM, Jules, UKCA, LFRic_Apps
Compatible for both Cylc7 + Cylc8

YAML Config Options:
Required:
* repo: the repo being run
* time_launch: HH:MM, the time to start the run suite cronjob
* time_clean: HH:MM, the time to start the clean suite cronjob - for a given
              task this will be delayed by the amount of time given by the
              period value
* period: 'weekly' runs on Monday, cleans on Sunday. 'nightly' runs Tue-Fri,
          cleans Wed-Sat. 'nightly_all' runs Mon-Fri, cleans Tue-Sat
* groups: the groups to run
Optional:
* revisions: heads to use the HoT for sub-repos, set to use the set revisions
* vars: strings that follow the -S command on the command line
* monitoring: Boolean, whether to run the monitoring script on this suite
* cylc_version: Can be any string beginning 8 that is a valid cylc install
                at the site, such that `export CYLC_VERSION=<cylc_version>`
                works.
"""

import argparse
import os
import subprocess
import sys
import yaml

DEPENDENCIES = {
    "lfric_apps": [
        "casim",
        "jules",
        "lfric_core",
        "shumlib",
        "socrates",
        "ukca",
        "um",
    ],
    "um": ["casim", "jules", "mule", "shumlib", "socrates", "ukca"],
    "lfric_core": [],
    "jules": [],
    "ukca": [],
}

CLONE_DIR = os.path.join(os.environ["TMPDIR"], os.environ["USER"])
MIRROR_PATH = "/data/users/gitassist/git_mirrors/"
UMDIR = os.environ["UMDIR"]
PROFILE = ". /etc/profile"
DATE_BASE = "date +\\%Y-\\%m-\\%d"
MONITORING_TIME = "00 06"


def run_command(command):
    """
    Run a subprocess command and return the result object
    """
    return subprocess.run(
        command, shell=True, capture_output=True, text=True, timeout=5
    )


def create_git_clone_cron(repo):
    """
    Return a string of cron commands to get a git clone from the gitassist mirror
    Runs at 23:30 each day before all other tasks
    """

    clone_path = os.path.join(CLONE_DIR, f"clone_{repo}")
    repo_mirror = os.path.join(MIRROR_PATH, "MetOffice", f"{repo}.git")

    command = f"# Clone {repo} - every day at 23:30 #"
    length = len(command)
    command = f"{length*'#'}\n{command}\n{length*'#'}\n30 23 * * * {PROFILE} ; "
    command += f"rm -rf {clone_path} ; "
    command += f"git clone {repo_mirror} {clone_path}"
    return command + "\n\n\n"


def generate_cron_timing_str(suite, mode):
    """
    Return a string with the cron timing info included but no commands
    """

    if mode == "monitoring":
        cron = f"{MONITORING_TIME}"
    elif mode == "main":
        cron = suite["cron_launch"]
    elif mode == "clean":
        cron = suite["cron_clean"]
    else:
        sys.exit("Unrecognised mode for cron timing string")
    cron += " * * "

    if suite["period"] == "weekly":
        if mode == "main" or mode == "monitoring":
            cron += "1 "
        else:
            cron += "7 "
    elif suite["period"] == "nightly_all":
        if mode == "main" or mode == "monitoring":
            cron += "1-5 "
        else:
            cron += "2-6 "
    else:
        if mode == "main" or mode == "monitoring":
            cron += "2-5 "
        else:
            cron += "3-6 "
    return cron


def generate_header(name, suite):
    """
    Generate a comment describing the suite
    """

    header = f"# {name} #"
    hashes = "".join("#" for _ in header)
    header = f"{hashes}\n{header}\n{hashes}\n"
    header += f"# Launch at {suite['time_launch']} on "
    if suite["period"] == "weekly":
        header += "Mon\n"
    elif suite["period"] == "nightly_all":
        header += "Mon-Fri\n"
    else:
        header += "Tue-Fri\n"
    header += f"# Clean at {suite['time_clean']} on "
    if suite["period"] == "weekly":
        header += "Sun\n"
    elif suite["period"] == "nightly_all":
        header += "Tue-Sat\n"
    else:
        header += "Wed-Sat\n"
    return header


def generate_monitoring(name, suite, log_file):
    """
    Generate the monitoring command cron job
    Default to off if not specified in config
    """

    # Return empty string if not required - default to this state
    if "monitoring" not in suite or not suite["monitoring"]:
        return ""

    script = os.path.join(UMDIR, "bin", "monitoring.py")
    cylc_dir = os.path.expanduser(os.path.join("~", "cylc-run", name))

    monitoring = generate_cron_timing_str(suite, "monitoring")

    monitoring += (
        f"{PROFILE} ; module load scitools/default-current ; "
        f"{script} {cylc_dir} >> {log_file} 2>&1"
    )

    return monitoring + "\n"


def generate_clean_commands(cylc_version, name, log_file):
    """
    Generate the commands used to clean the suite
    """
    return (
        f"{PROFILE} ; "
        f"export CYLC_VERSION={cylc_version} ; "
        f"cylc stop --kill '{name}' >/dev/null 2>&1 ; sleep 10 ; "
        f"cylc clean --timeout=7200 -y -q {name} "
        f">> {log_file} 2>&1\n"
    )


def generate_clean_cron(suite_name, suite, log_file, cylc_version):
    """
    Return a string of the cronjob for cleaning the suite
    """

    clean_cron = generate_cron_timing_str(suite, "clean")
    if suite["period"] == "weekly":
        date_str = f'_$({DATE_BASE} -d "6 days ago")'
    else:
        date_str = f'_$({DATE_BASE} -d "1 day ago")'

    name = suite_name + date_str

    clean_cron += generate_clean_commands(cylc_version, name, log_file)

    return clean_cron


def generate_cylc_command(suite, wc_path, cylc_version, name):
    """
    Return a string with the rose-stem command
    Ignores any additional source arguments
    """

    command = (
        f"export CYLC_VERSION={cylc_version} ; "
        f"cylc vip -z g={suite['groups']} "
        f"-n {name} "
        f"-S USE_MIRRORS=true "
    )
    if "revisions" in suite and suite["revisions"] == "heads":
        command += "-S USE_HEADS=true "
    command += f"{os.path.join(wc_path, 'rose-stem')} "
    return command


def populate_cl_variables(suite):
    """
    Combine variables with the -S command line argument
    """

    cl_vars = ""

    if "vars" not in suite:
        return cl_vars

    for item in suite["vars"]:
        cl_vars += f"-S {item} "

    return cl_vars


def generate_main_job(name, suite, log_file, wc_path, cylc_version):
    """
    Generate the main cron_job commands
    """

    # Set up the timing for this job
    cron_job = generate_cron_timing_str(suite, "main")

    job_command = f"{PROFILE} ; "

    # Begin rose-stem command
    job_command += generate_cylc_command(suite, wc_path, cylc_version, name)

    # Add any -S vars defined
    job_command += populate_cl_variables(suite)

    job_command += f">> {log_file} 2>&1"

    # If this is a cylc-8-next job, check that the 8-next symlink in metomi points
    # elsewhere than the cylc-8 symlink
    if cylc_version == "8-next":
        next_link = os.path.join(CYLC_INSTALL, "cylc-8-next")
        def_link = os.path.join(CYLC_INSTALL, "cylc-8")
        cron_job += (
            f'[ "$(readlink -- {next_link})" != "$(readlink -- {def_link})" ] '
            f"&& ({job_command})"
        )
    else:
        cron_job += job_command

    return cron_job + "\n"


def generate_cron_job(suite_name, suite, log_file):
    """
    Using the suite settings from the config file, define a cronjob for the
    rose-stem task and for the suite-clean task
    """

    cylc_version = suite.get("cylc_version", "8")
    cylc_version = str(cylc_version)

    date_str = f"_$({DATE_BASE})"
    name = suite_name + date_str
    wc_path = os.path.join(CLONE_DIR, "clone_" + suite["repo"])

    header = generate_header(suite_name, suite)
    cron_job = generate_main_job(name, suite, log_file, wc_path, cylc_version)
    monitoring = generate_monitoring(name, suite, log_file)
    clean_cron = generate_clean_cron(suite_name, suite, log_file, cylc_version)

    return header + cron_job + monitoring + clean_cron


def parse_cl_args():
    """
    Parse command line arguments
    """

    parser = argparse.ArgumentParser("Generate Cronjobs for nightly testing")
    parser.add_argument(
        "-c",
        "--config",
        required=True,
        help="Path to a yaml config file with suites defined.",
    )
    parser.add_argument(
        "-f",
        "--cron_file",
        default="~/Crontabs/auto-gen_testing_git.cron",
        help="The file the cronjobs will be written to."
        "Installation assumes this ends with '.cron'",
    )
    parser.add_argument(
        "-l",
        "--cron_log",
        default="~/cron.log",
        help="The file any stdout or stderr will be piped to.",
    )
    parser.add_argument(
        "-p",
        "--cylc_path",
        default="~metomi/apps",
        help="The location of the cylc installation required for testing `next-cylc`"
        "configs.",
    )
    parser.add_argument(
        "--install",
        action="store_true",
        help="If True, will install the generated crontab.",
    )
    args = parser.parse_args()
    args.cron_file = os.path.expanduser(args.cron_file)
    args.cron_log = os.path.expanduser(args.cron_log)
    global CYLC_INSTALL
    CYLC_INSTALL = args.cylc_path
    return args


if __name__ == "__main__":
    args = parse_cl_args()

    with open(args.config) as stream:
        suites = yaml.safe_load(stream)

    main_crontab = (
        "# WARNING: This file is automatically generated by the "
        "'generate_test-suite_cron.py' file.\n# Use that script and associated "
        "config file to modify these cron jobs.\n\n"
    )
    for repo in DEPENDENCIES:
        main_crontab += create_git_clone_cron(repo)

    last_repo = None
    for suite_name in sorted(suites.keys()):
        if suite_name == "base":
            continue
        repo = suites[suite_name]["repo"]
        tlaunch = suites[suite_name]["time_launch"].split(":")
        tclean = suites[suite_name]["time_clean"].split(":")
        suites[suite_name]["cron_launch"] = f"{tlaunch[1]} {tlaunch[0]}"
        suites[suite_name]["cron_clean"] = f"{tclean[1]} {tclean[0]}"
        if repo != last_repo:
            main_crontab += 80 * "#" + "\n"
            main_crontab += f"# {repo.upper()} SUITES\n"
            main_crontab += 80 * "#" + 2 * "\n"
            last_repo = repo
        main_crontab += generate_cron_job(suite_name, suites[suite_name], args.cron_log)
        main_crontab += 3 * "\n"

    with open(args.cron_file, "w") as outfile:
        outfile.write(main_crontab)

    # Install any file with .cron extension in the specified dir
    cron_path = args.cron_file.strip(os.path.basename(args.cron_file))
    all_file = os.path.join(cron_path, "all_cron_jobs.cron")
    if args.install:
        command = f"cat {os.path.join(cron_path, '*.cron')} | crontab -"
        result = run_command(command)
        if result.returncode:
            print("Failed to install crontab. Error:")
            sys.exit(result.stderr)
