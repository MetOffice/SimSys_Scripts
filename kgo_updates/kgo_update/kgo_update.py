#!/usr/bin/env python
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
This script will parse the rose-ana-comparisons.db produced by a valid
rose-stem suite, writing and executing a sub-script which performs a
KGO update (creating new KGO folders and installing the required files)

This script will no longer ask questions of the user and instead expects to be
passed arguments as command line arguments. The suite expects as minimum:
    python3 kgo_update.py -S <suite_name> -U <suite_user> -N <new_kgo_dir>

Required Command Line Arguments:
=> -S The name of the suite with the new kgo
=> -U The username of the owner of the above suite

Optional Arguments:
=> --new-kgo-dir -N  Give the name of the new kgo dir. If not specified then the
                     suite will install kgo to the directory kgois currently
                     found in.
=> --non-interactive Run in non-interactive mode. The suite will not ask any
                     questions. This is not a recommended option
=> --new-release     Only for use by the Systems Team when releasing a new
                     version of the UM
=> -P Specify the platform this is being run on. Currently only relevant if the
      site is meto - if so then it will be automatically populated if not given
=> -E --extension    Specify the extension of the variables file. .rc or .cylc


"""
import argparse
import os
import re
import sqlite3
import subprocess
import sys
from collections import defaultdict

# Global criteria for updating - any statuses matched by this list will
# be treated as "failed" for the purposes of updating.  When running a
# "new version" update this list will be updated to include the "OK"
# status, causing all tested files to be updated.
UPDATE_CRITERIA = ["WARN", "FAIL"]


def banner(message):
    """Print a simple banner message"""
    return "{0}\n* {1} *\n{0}".format("%" * (len(message) + 4), message)


def confirm(message, skip=False):
    "Prompt the user for a response"
    if skip:
        print("Non-interactive mode, skipping question: ")
        print(message)
        return True
    while True:
        check = input(message)
        if check in ["Y", "y", "yes"]:
            print("")
            return True
        elif check in ["N", "n", "no"]:
            print("")
            return False


def write_update_script(kgo_dirs, new_dirname, script):
    "Write the update script to a file"
    total_filesize = 0

    # Start the script with a header
    script.write("#!/bin/bash\n")
    script.write("set -eu\n")

    for kgo_dir in sorted(kgo_dirs.keys()):
        update_dict = kgo_dirs[kgo_dir]
        # Construct the new KGO directory name
        if new_dirname == "install_in_place":
            # In non-interactive mode, the user may choose to install the
            # files to the existing KGO location instead of a new directory,
            # in which case we just use the existing directory here
            new_kgo_dir = kgo_dir
        else:
            # Otherwise construct it by replacing the existing name with
            # the new one
            new_kgo_dir = os.path.join(os.path.dirname(kgo_dir), new_dirname)

        # Write a sub-header for this KGO directory to help visually split
        # up the file (for when the user checks it by eye)
        script.write("#" * (len(new_kgo_dir) + 4) + "\n")
        script.write("# {0} #\n".format(new_kgo_dir))
        script.write("#" * (len(new_kgo_dir) + 4) + "\n")
        script.write(f"echo 'Installing {new_kgo_dir}'\n\n")

        # Build up several different text sections in the upcoming loop
        # (so that they can be printed in groups)
        keep_description = ["# Files to be kept from previous KGO: "]
        keep_commands = []
        copy_description = ["# Files to be copied from suite: "]
        copy_commands = []
        mkdir_commands = []
        mkdir_commands.append("echo 'mkdir -p {0}'".format(new_kgo_dir))
        mkdir_commands.append("mkdir -p {0}".format(new_kgo_dir))

        kgo_files = sorted(update_dict.keys())
        for kgo_file in kgo_files:
            source = update_dict[kgo_file]

            # Ensure any subdirectories are created as needed
            subdir = os.path.dirname(kgo_file)
            if subdir != "":
                full_subdir = os.path.join(new_kgo_dir, subdir)
                mkdir_commands.append("echo 'mkdir -p {0}'".format(full_subdir))
                mkdir_commands.append("mkdir -p {0}".format(full_subdir))

            if source is None:
                # Files being kept should be sym-linked back to the previous
                # revision, to save space
                if "OK" in UPDATE_CRITERIA:
                    # However, if this is a version upgrade any tasks
                    # without a source here must have been retired from
                    # the tests
                    continue
                keep_description.append("#    * {0}".format(kgo_file))

                # Construct the paths
                old_file = os.path.join(kgo_dir, kgo_file)
                new_file = os.path.join(new_kgo_dir, kgo_file)

                keep_commands.append(
                    "echo 'ln -s {0} {1}'".format(
                        os.path.relpath(old_file, os.path.dirname(new_file)),
                        new_file,
                    )
                )
                keep_commands.append(
                    "ln -s {0} {1}".format(
                        os.path.relpath(old_file, os.path.dirname(new_file)),
                        new_file,
                    )
                )
            else:
                # Files from the suite should be copied from the suite
                copy_description.append("#    * {0}".format(kgo_file))
                new_file = os.path.join(new_kgo_dir, kgo_file)
                copy_commands.append("echo 'cp {0} {1}'".format(source, new_file))
                copy_commands.append("cp {0} {1}".format(source, new_file))
                # In this case more disk-space is needed, so update
                # the running total for reporting later
                total_filesize += os.path.getsize(source)

        # Can now write this tasks output to the file:
        if len(keep_description) > 1:
            script.write("\n".join(keep_description) + "\n\n")
        if len(copy_description) > 1:
            script.write("\n".join(copy_description) + "\n\n")
        script.write("# Creating new KGO directories:\n")
        script.write("\n".join(mkdir_commands) + "\n\n")
        if len(keep_commands) > 0:
            script.write("# Linking back to unchanged KGO files:\n")
            script.write("\n".join(keep_commands) + "\n\n")
        if len(copy_commands) > 0:
            script.write("# Copying new KGO files from suite:\n")
            script.write("\n".join(copy_commands) + "\n\n")

    return total_filesize


def report_space_required(total_filesize, skip=False):
    "Let the user know how much space they need for the update"
    # Report the amount of disk space required
    units = ["TB", "GB", "MB", "kB", "B"]
    while total_filesize > 1024.0 and len(units) > 1:
        total_filesize = total_filesize / 1024.0
        units.pop()
    unit = units[-1]

    print(
        banner(
            "Update will require: {0:.2f} {1} of disk space".format(
                total_filesize, unit
            )
        )
    )
    if not confirm("Please confirm this much space is available (y/n)? ", skip=skip):
        sys.exit("Aborting...")


def add_untested_kgo_files(kgo_dirs):
    "Add any files present in the KGO directory but not tested by the suite"
    for kgo_dir, update_dict in kgo_dirs.items():
        for path, _, filenames in os.walk(kgo_dir):
            for filename in filenames:
                kgo_file = os.path.relpath(os.path.join(path, filename), kgo_dir)
                if kgo_file not in update_dict:
                    kgo_dirs[kgo_dir][kgo_file] = None
    return kgo_dirs


def group_comparisons_by_dir(comparisons, skip=False):
    """
    Group the key details from the list of comparisons by the KGO
    directory they are referencing.

    """
    kgo_dirs = defaultdict(dict)
    for _, kgo_file, suite_file, status, _ in comparisons:

        # If the kgo_file isn't set, move on (in some cases the test is not
        # comparing two files at all
        if kgo_file is None:
            continue

        # If it looks like this KGO file never existed at all, let the user
        # decide what they want to happen
        if not os.path.exists(kgo_file) and status.strip() in UPDATE_CRITERIA:
            if not confirm(
                "KGO file {0} doesn't appear to exist, should we "
                "install it from the suite (y/n)? ".format(kgo_file),
                skip=skip,
            ):
                # Note this is negated - i.e. if the user doesn't want to
                # include this new file, we skip it and move on
                continue

        # Extract the base KGO directory by moving backwards through the path
        # until a directory that matches the KGO directory naming style
        basedir = os.path.dirname(kgo_file)
        while basedir != "/" and not re.match(
            r".*((vn|\d+\.)\d+\.\d+(_t\d+|))$", basedir
        ):
            basedir = os.path.dirname(basedir)

        # If the above goes wrong it will eventually hit root; if this happens
        # we cannot continue as something is seriously not right
        if basedir == "/":
            msg = (
                "Problem locating KGO directory - "
                "is this actually a KGO file?\n  {0}"
            )
            sys.exit(msg.format(kgo_file))

        # Otherwise add the result to the list - for each entry we store the
        # relative path to the KGO file and the full path to the file in the
        # suite which should be used to update it (if it needs updating)
        if status.strip() in UPDATE_CRITERIA:

            # Here we could check if this update has already been lodged,
            # and if it has perhaps we can do some additional checking
            # (for instance, are the changes in answers the same?)
            relative_kgo_path = os.path.relpath(kgo_file, basedir)
            if (
                relative_kgo_path in kgo_dirs[basedir]
                and kgo_dirs[basedir][relative_kgo_path] != suite_file
            ):
                # Or not... it isn't clear what could be checked here
                continue

            kgo_dirs[basedir][relative_kgo_path] = suite_file

    return kgo_dirs


def get_all_kgo_comparisons(conn):
    "Retrieve all comparisons related to KGO tasks"
    res = conn.execute(
        "SELECT comp_task, kgo_file, suite_file, status, comparison FROM comparisons"
    )
    return res.fetchall()


def check_for_incomplete_tasks(conn, skip=False):
    "Check the database to see if all tasks are complete"
    res = conn.execute("SELECT task_name FROM tasks WHERE completed==?", (1,))
    failed_tasks = res.fetchall()
    if len(failed_tasks) > 0:
        print(
            "WARNING - Some comparisons are either still running or "
            "failed to complete properly:"
        )
        for task in failed_tasks:
            print("  * " + task[0])
        print(
            "Please investigate this manually - no KGO updates will be "
            "made for any of the tasks listed above"
        )
        if not confirm("Continue anyway (y/n)? ", skip=skip):
            sys.exit()


def connect_to_kgo_database(suite_dir):
    "Make a connection to the comparisons database in a given suite"
    db_filename = os.path.join(suite_dir, "log", "rose-ana-comparisons.db")
    if not os.path.exists(db_filename):
        msg = "ERROR: Suite comparison database not found at {0}"
        sys.exit(msg.format(db_filename))
    return sqlite3.connect(db_filename)


def get_suite_dir(user, suite_name):
    "Returns the path to the given suite directory of a given user"
    suite_dir = "~{0}/cylc-run/{1}".format(user, suite_name)
    expansion = os.path.expanduser(suite_dir)
    if expansion == suite_dir or not os.path.exists(expansion):
        msg = "ERROR: Unable to find suite ({0})"
        sys.exit(msg.format(expansion))
    else:
        return expansion


def get_site():
    """
    Function to return the site this is being run at using rose config
    """
    command = ["rose", "config", "rose-stem", "automatic-options"]
    cmd = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = cmd.communicate()
    retcode = cmd.returncode
    if retcode == 0:
        site = stdout.decode("utf-8").split("=")[1].strip()
        print("Found site via rose-config: {0}\n".format(site))
    else:
        sys.exit("No site was detected via rose config - this setting is required")
    return site


def get_variables_file_path(suite_dir, site, platform, variables_extension):
    """
    Find the path to the file containing kgo version info
    Uses the variables_extension to search for both .cylc and .rc files
    If the platform is set, it will search for variables_<platform>.extension
    otherwise just variables.extension
    """

    site_path = os.path.join(suite_dir, "site", site)

    # Search for a variables_<platform>. file
    if platform is not None:
        vars_file = f"variables_{platform}{variables_extension}"
        variables_path = os.path.join(site_path, vars_file)
        print("INFO: Looking for a kgo variables file at " + variables_path)
        if os.path.exists(variables_path):
            return variables_path

    # Search for a variables. file if a platform one doesn't exist, eg. VM site
    vars_file = f"variables{variables_extension}"
    variables_path = os.path.join(site_path, vars_file)
    print("INFO: Looking for a kgo variables file at " + variables_path)
    if os.path.exists(variables_path):
        return variables_path

    sys.exit("ERROR: Couldn't find a kgo variables file")


def update_variables_rc(
    suite_dir,
    kgo_dirs,
    new_kgo_dir,
    site,
    platform,
    variables_extension,
    skip=False,
):
    """
    Create an updated copy of the variables.rc with the KGO variables for
    any changed jobs updated
    """

    # Attempt to get a copy of the variables.rc
    variables_rc = get_variables_file_path(
        suite_dir, site, platform, variables_extension
    )

    # Create a file to hold the new variables.rc
    variables_rc_new = os.path.expanduser(
        f"~/variables{variables_extension}_{new_kgo_dir}"
    )
    if os.path.exists(variables_rc_new):
        print(
            "WARNING: New variables.rc file for this update already exists "
            "at {0} and will be overwritten".format(variables_rc_new)
        )
        if not confirm("Okay to overwrite variables.rc file (y/n)? ", skip=skip):
            sys.exit("Aborting...")

    # Get the KGO variable names that need updating
    kgo_vars = []
    for kgo_dir in kgo_dirs.keys():
        kgo_vars.append(os.path.basename(os.path.dirname(kgo_dir)).upper())

    # Get the suffix of the new directory
    if "_" not in new_kgo_dir:
        return
    ticket_suffix = new_kgo_dir.split("_")[1]

    # Rewrite the variables.rc
    with open(variables_rc, "r") as vrc_old:
        with open(variables_rc_new, "w") as vrc_new:
            for line in vrc_old.readlines():
                # Find lines with KGO variables and update them if they are
                # named in the database
                pattern = re.search(r'\s*"(\S+)"\s*:\s*BASE', line)
                if pattern and pattern.group(1).upper() in kgo_vars:
                    vrc_new.write(
                        re.sub(
                            r":\s*BASE.*",
                            r': BASE~"_{}",'.format(ticket_suffix),
                            line,
                        )
                    )
                else:
                    # Otherwise just write the line unaltered
                    vrc_new.write(line)


def main():
    "Toplevel script function - process arguments and run update"

    print(banner("Starting KGO Update"))

    # Create a quick version of the regular raw description formatter which
    # adds spaces between the option help text
    class BlankLinesHelpFormatter(argparse.HelpFormatter):
        "Formatter which adds blank lines between options"

        def _split_lines(self, text, width):
            return super(BlankLinesHelpFormatter, self)._split_lines(text, width) + [""]

    parser = argparse.ArgumentParser(
        usage="%(prog)s [--new-release]",
        description="""
        KGO Update - A semi-interactive tool for updating UM KGO.

        This script will scan the rose_ana comparisons database to
        prepare and run a shell script to update the KGO files for
        any failed tasks (or all tasks if the --new-release option
        is used)
        """,
        formatter_class=BlankLinesHelpFormatter,
    )

    parser.add_argument(
        "--new-release",
        help="if set, indicates that the task being "
        "performed is installing a full set of new KGO "
        "for a major model release instead of simply "
        "updating failed KGO tasks.",
        action="store_true",
    )

    parser.add_argument(
        "--non-interactive",
        help="if set, no questions will be asked and "
        "the script will proceed without any checking "
        "or warnings.  Use with caution.",
        action="store_true",
    )

    parser.add_argument(
        "-S",
        "--suite-name",
        help="specifies the (cylc-run dir) name of "
        "the suite containing the KGO database; for "
        "use with non-interactive mode.  If not given "
        "non-interactive mode will try to take "
        "this from the environment ($CYLC_SUITE_NAME).",
    )

    parser.add_argument(
        "-U",
        "--suite-user",
        help="specifies the (cylc-run dir) username "
        "where the KGO database suite can be found; for "
        "use with non-interactive mode.  If not given "
        "non-interactive mode will try to take "
        "this from the environment ($USER).",
    )

    parser.add_argument(
        "-N",
        "--new-kgo-dir",
        help="specifies the name of the new KGO "
        "subdirectory; for use with non-interactive "
        "mode. If not given the KGO files will be "
        "installed directly to the location currently "
        "specified by the suite.",
    )

    parser.add_argument(
        "-P",
        "--platform",
        help="specifies the platform this script is "
        "running on. Defaults as ''. If the site is meto then"
        "the platform will be autopopulated.",
    )

    parser.add_argument(
        "-E",
        "--extension",
        default=".cylc",
        help="The extension of the variables file, either .rc " "or .cylc",
    )

    args = parser.parse_args()

    if args.new_release:
        # If releasing a new version, extend the criteria for performing
        # a KGO update to include tasks which completed successfully
        UPDATE_CRITERIA.append("OK")

    suite_name = args.suite_name
    suite_user = args.suite_user
    new_kgo_dir = args.new_kgo_dir
    if new_kgo_dir is None:
        new_kgo_dir = "install_in_place"
    platform = args.platform
    variables_extension = args.extension

    if suite_name is None or suite_user is None:
        message = (
            "'kgo_update.py' will no longer ask for your suite "
            "information\n.To pass this into the script please use "
            "the command line arguments.\nThese are documented at "
            "the top of the file."
        )
        sys.exit(message)

    # Prompt the user for the key options
    if args.non_interactive:
        confirm_skip = True
        interactive = False
    else:
        confirm_skip = False
        interactive = True

    suite_dir = get_suite_dir(suite_user, suite_name)

    # Get the site being run at
    site = get_site()
    if site == "meto" and new_kgo_dir == "install_in_place":
        message = (
            "Site meto should not use the 'install_in_place' option. "
            "Check --new-kgo-dir is specified on the command line."
            "Do you want to continue with the update?"
        )
        if not confirm(message, False):
            sys.exit()

    # Populate platform if not provided and at meto
    if platform is None and site == "meto":
        hostname = os.uname()[1]
        if hostname == "uan01":
            platform = "ex1a"
        elif hostname.startswith("caz"):
            platform = "azspice"

    # Make a connection to the database
    conn = connect_to_kgo_database(suite_dir)

    # See if any tasks have not finished yet
    check_for_incomplete_tasks(conn, skip=confirm_skip)

    # Extract the comparisons
    comparisons = get_all_kgo_comparisons(conn)

    # Group these by KGO directory
    kgo_dirs = group_comparisons_by_dir(comparisons, skip=confirm_skip)

    # Add in any files in the previous KGO untested by the suite
    kgo_dirs = add_untested_kgo_files(kgo_dirs)

    # Create a file to hold the update script
    script_path = os.path.expanduser("~/kgo_update_{0}.sh".format(new_kgo_dir))
    if os.path.exists(script_path):
        print(
            "WARNING: Script file for this update already exists at {0} "
            "and will be overwritten".format(script_path)
        )
        if not confirm("Okay to overwrite script file (y/n)? ", skip=confirm_skip):
            sys.exit("Aborting...")

    # Write the script file
    with open(script_path, "w") as script:
        total_filesize = write_update_script(kgo_dirs, new_kgo_dir, script)

    print("Script file written to: {0}\n".format(script_path))

    # Report on the required space
    report_space_required(total_filesize, skip=confirm_skip)

    # Update the variables.rc (don't do this for new releases as it's part
    # of the release process already)
    if not args.new_release:
        update_variables_rc(
            suite_dir,
            kgo_dirs,
            new_kgo_dir,
            site,
            platform,
            variables_extension,
            skip=confirm_skip,
        )

    # Open the file for viewing in user's editor
    if interactive:
        print(
            f"\n\nOpening {script_path}\nHit Return to Step through, "
            "q to print all\n\n"
        )
        with open(script_path, "r") as f:
            line_count = 0
            print_all = False
            for line in f:
                print(line, end="")
                line_count += 1
                if line_count == 10 and not print_all:
                    line_count = 0
                    key = input()
                    if key.lower() == "q":
                        print_all = True

        print(
            "\n\nPlease carefully check the commands in the above file "
            "before continuing.\nIf changes need to be made then open a "
            "new terminal and edit the file there."
        )

    # Final confirmation before running
    print("WARNING: Once launched the script will make permanent changes")
    if not confirm("Are you sure you want to continue (y/n)? ", skip=confirm_skip):
        sys.exit("Aborting...")

    print(
        banner("Running KGO Update commands from {0}".format(script_path)),
        flush=True,
    )

    process = subprocess.Popen(
        ["bash", script_path], stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    while True:
        line = process.stdout.readline()
        if not line and process.poll() is not None:
            break
        print(line.decode(), end="", flush=True)
    retcode = process.returncode
    if retcode != 0:
        sys.exit(
            f"Script did not run successfully, update failed\n"
            f"{process.stderr.read().decode()}\n"
        )

    # Print a final message
    print(banner("Generated KGO Script has run successfully"))
    if site != "meto":
        print(
            "\nThe script has created a copy of the variables.rc in your "
            "$HOME directory; you should merge this with the one in your "
            "working copy using xxdiff to update the KGO variables"
        )
    else:
        print(
            f"\nThe kgo update for {platform} is complete.\n\nIf this was "
            "run from 'meto_update_kgo.sh' in the UM, the generated files "
            "will be moved to UMDIR on vdi and the process will continue "
            "for other platforms requested.\n\nOtherwise, eg. for "
            "lfric_inputs, you will need to merge the generated variables "
            "file with the one in your working copy and run the script "
            "again on the next platform."
        )


if __name__ == "__main__":
    main()
