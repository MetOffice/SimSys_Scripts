"""
Script which launches a rose-stem suite.
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
Optional:
* groups: the groups to run
* revisions: heads to use the HoT for sub-repos, set to use the set revisions
* vars: strings that follow the -S command on the command line
* monitoring: Boolean, whether to run the monitoring script on this suite
* cylc_version: 7 or 8
* commands: list of commands to use. If set, will only run these commands
"""

import os
import sys
import yaml
import argparse
import subprocess

DEFAULT_CYLC_VERSION = 7
DEPENDENCIES = {
    "lfric_apps": [
        "casim",
        "jules",
        "lfric",
        "shumlib",
        "socrates",
        "ukca",
        "um",
    ],
    "um": ["casim", "jules", "mule", "shumlib", "socrates", "ukca"],
    "jules": [],
    "ukca": [],
}
CYLC_DIFFS = {
    7: {
        "name": "--name=",
        "clean": "rose suite-clean",
    },
    8: {
        "name": "--workflow-name=",
        "clean": "cylc clean --timeout=3600",
    },
}
SCRATCH_DIR = os.environ["SCRATCH"]
UMDIR = os.environ["UMDIR"]
PROFILE = ". /etc/profile"


def run_command(command):
    """
    Run a subprocess command and return the result object
    """
    return subprocess.run(
        command, shell=True, capture_output=True, text=True, timeout=5
    )


def fetch_working_copy_cron(repos, scratch):
    """
    Cleanup and then re-checkout working copies for each of the repos used
    Runs just after midnight ahead of all other tasks
    """

    command = "# Checkout Working Copies #"
    l = len(command)
    command = f"{l*'#'}\n{command}\n{l*'#'}\n01 00 * * 1-5 "

    for repo in repos:
        wc_path = os.path.join(scratch, "wc_" + repo)
        command += f"rm -rf {wc_path} ; "
        command += f"fcm co -q --force fcm:{repo}.xm_tr@HEAD {wc_path} ; "
    return command + "\n\n\n"


def lfric_heads_sed(wc_path):
    """
    Add sed commands to setup dependencies.sh for heads testing
    """

    dep_path = os.path.join(wc_path, "dependencies.sh")

    rstr = f"sed -i -e 's/^\(export .*_revision=@\).*/\\1HEAD/' {dep_path} ; "
    rstr += f"sed -i -e 's/^\(export .*_rev=\).*/\\1HEAD/' {dep_path} ; "
    return rstr


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
    else:
        if suite["period"] == "nightly_all":
            header += "Mon-Fri\n"
        else:
            header += "Tue-Fri\n"
    header += f"# Clean at {suite['time_clean']} on "
    if suite["period"] == "weekly":
        header += "Sun\n"
    else:
        if suite["period"] == "nightly_all":
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

    monitoring = "00 06 * * "
    if suite["period"] == "weekly":
        monitoring += "1 "
    else:
        if suite["period"] == "nightly_all":
            monitoring += "1-5 "
        else:
            monitoring += "2-5 "

    monitoring += (
        f"{PROFILE} ; " + "module load scitools/default-current ; "
        f"{script} {cylc_dir} >> {log_file} 2>&1"
    )

    return monitoring + "\n"


def generate_clean_cron(suite_name, suite, log_file, cylc_version):
    """
    Return a string of the cronjob for cleaning the suite
    """

    clean_cron = f"{suite['cron_clean']} * * "
    if suite["period"] == "weekly":
        clean_cron += "7 "
        date_str = '_$(date +\%Y-\%m-\%d -d "6 days ago")'
    else:
        if suite["period"] == "nightly_all":
            clean_cron += "2-6 "
        else:
            clean_cron += "3-6 "
        date_str = '_$(date +\%Y-\%m-\%d -d "1 day ago")'

    name = suite_name + date_str

    clean_cron += (
        f"{PROFILE} ; "
        f"export CYLC_VERSION={cylc_version} ; "
        f"{CYLC_DIFFS[cylc_version]['clean']} -y -q {name} "
        f">> {log_file} 2>&1\n"
    )
    return clean_cron


def generate_main_job(name, suite, log_file, wc_path, cylc_version):
    """
    Generate the main cron_job commands
    """

    # Set up the timing for this job
    cron_job = f"{suite['cron_launch']} * * "
    if suite["period"] == "weekly":
        cron_job += "1 "
    else:
        if suite["period"] == "nightly_all":
            cron_job += "1-5 "
        else:
            cron_job += "2-5 "

    cron_job += f"{PROFILE} ; "

    # If commands are defined, enter these then return the completed job
    if "command" in suite:
        for item in suite["command"]:
            cron_job += f"{item} "
            cron_job += f">> {log_file} 2>&1 ; "
        return cron_job + "\n"

    # LFRic Apps sets heads differently so add SED command here
    if suite["repo"] == "lfric_apps" and suite["revisions"] == "heads":
        cron_job += lfric_heads_sed(wc_path)

    # Begin rose-stem command
    cron_job += (
        f"export CYLC_VERSION={cylc_version} ; "
        + f"rose stem --group={suite['groups']} "
        + f"{CYLC_DIFFS[cylc_version]['name']}{name} "
        + f"--source={wc_path} "
    )

    # If using heads testing and not lfric_apps, define sources here
    if (
        "revisions" in suite
        and suite["revisions"] == "heads"
        and repo != "lfric_apps"
        and suite["repo"] in DEPENDENCIES
    ):
        for item in DEPENDENCIES[suite["repo"]]:
            cron_job += f"--source=fcm:{item}.xm_tr@HEAD "

    # Add any -S vars defined
    if "vars" in suite:
        for item in suite["vars"]:
            cron_job += f"-S {item} "

    cron_job += f">> {log_file} 2>&1"

    if cylc_version == 8:
        cron_job += f" ; cylc play {name} >> {log_file} 2>&1"

    return cron_job + "\n"


def generate_cron_job(suite_name, suite, log_file, wc_path):
    """
    Using the suite settings from the config file, define a cronjob for the
    rose-stem task and for the suite-clean task
    """

    if "cylc_version" in suite:
        cylc_version = suite["cylc_version"]
    else:
        cylc_version = DEFAULT_CYLC_VERSION

    date_str = "_$(date +\%Y-\%m-\%d)"
    name = suite_name + date_str
    wc_path = os.path.join(SCRATCH_DIR, "wc_" + suite["repo"])

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
        default="~/Crontabs/auto-gen_testing.cron",
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
        "--install",
        action="store_true",
        help="If True, will install the generated crontab.",
    )
    args = parser.parse_args()
    args.cron_file = os.path.expanduser(args.cron_file)
    args.cron_log = os.path.expanduser(args.cron_log)
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
    main_crontab += fetch_working_copy_cron(DEPENDENCIES.keys(), SCRATCH_DIR)

    last_repo = None
    for suite_name in sorted(suites.keys()):
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
        main_crontab += generate_cron_job(
            suite_name, suites[suite_name], args.cron_log
        )
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
