#!/usr/bin/env python3
"""
Scan a users (inteded as frzz for nightly testing) cylc-run directory for
cylc8 suites with uncompleted tasks
Restart each found and retrigger failed/submit-failed tasks
"""

import os
import re
import subprocess
import sqlite3
from datetime import datetime, timedelta


def run_command(command):
    """
    Launch a subprocess command and return the output
    """
    result = subprocess.run(command.split(), capture_output=True, text=True)
    return result


def connect_to_kgo_database(suite_dir):
    """
    Make a connection to the suite database
    """
    db_filename = os.path.join(suite_dir, "log", "db")
    if not os.path.exists(db_filename):
        print(f"Warning: Suite database not found at {db_filename}")
        return None
    return sqlite3.connect(db_filename)


def check_suite_cylc8(conn):
    """
    Search the DB for a "workflow_params" table - if it exists this is a cylc8
    suite so return False
    """
    res = conn.execute(
        "SELECT name FROM sqlite_master "
        "WHERE type='table' AND name='workflow_params'"
    ).fetchall()
    if len(res) > 0:
        return True
    return False


def check_for_failed_tasks(conn):
    """
    Search for tasks in table "task_state" with failed in status
    If any exist return True, else False
    """
    res = conn.execute(
        "SELECT name, status FROM task_states WHERE status LIKE '%failed%'"
    ).fetchall()
    if len(res) > 0:
        return True
    return False


def ask_yn(message):
    """
    Ask a message with yes or no options
    Return True for y, False for n
    """
    rval = ""
    while rval not in ["y", "n"]:
        rval = input(f"{message}? (y/n) ")
    return {"y": True, "n": False}[rval]


def restart_suite(suite):
    """
    Generate and run the command to restart the suite
    """
    print(f"Restarting {suite}")
    play_command = f"cylc play -q {suite}"
    _ = run_command(play_command)


def retrigger_suite(suite):
    """
    Generate and run commands to retrigger failed and submit-failed tasks
    """
    failed_command = f"cylc trigger {suite}/*/*:failed"
    print(f"Triggering Failed Tasks in {suite}")
    _ = run_command(failed_command)


if __name__ == "__main__":
    # Get the current date
    today = datetime.now()
    # If Monday then include suites from the weekend, otherwise just today
    if today.weekday() == 0:
        day_delta = 2
    else:
        day_delta = 1

    # Get a list of suites from the last day(weekend) to check for failed tasks
    valid_suites = []
    cylc_run = os.path.expanduser(os.path.join("~frzz", "cylc-run"))
    for suite in os.listdir(cylc_run):
        # Get the date string from the suite name - if not present then skip
        try:
            date_str = re.findall(r"\d{4}-\d{2}-\d{2}", suite)[0]
        except IndexError:
            continue
        # Convert to datetime and compare with day difference
        suite_date = datetime.strptime(date_str, "%Y-%m-%d")
        if suite_date > today - timedelta(days=day_delta):
            valid_suites.append(suite)

    # Parse the valid_suites for ones which are cylc8 and have failed tasks
    failed_suites = []
    for suite in valid_suites:
        suite_dir = os.path.join(cylc_run, suite, "runN")
        conn = connect_to_kgo_database(suite_dir)
        if conn is None or not check_suite_cylc8(conn):
            continue
        if check_for_failed_tasks(conn):
            failed_suites.append(suite)

    print("\nFound the following suites with errors:")
    for suite in failed_suites:
        print(f"    * {suite}")
    print()

    # Restart failed suites
    run_all = ask_yn("Do you want to restart all failed suites")
    restarted_suites = []
    for suite in failed_suites:
        if not run_all and not ask_yn(f"Do you want to restart {suite}"):
            continue
        restart_suite(suite)
        restarted_suites.append(suite)
    print()

    Retrigger failed tasks
    for suite in restarted_suites:
        retrigger_suite(suite)
