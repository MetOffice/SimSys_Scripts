import pytest
from ..generate_test_suite_cron import (
    MIRROR_PATH,
    CLONE_DIR,
    generate_cron_timing_str,
    generate_clean_commands,
    populate_cl_variables,
    create_git_clone_cron,
    generate_cylc_command,
)


# Test generate_cron_timing_str
data_generate_cron_timing_str = [
    ({"period": "weekly", "cron_launch": "30 00"}, "main", "30 00 * * 1 "),
    ({"period": "nightly", "cron_launch": "30 00"}, "main", "30 00 * * 2-5 "),
    (
        {"period": "nightly_all", "cron_launch": "30 00"},
        "main",
        "30 00 * * 1-5 ",
    ),
    ({"period": "weekly", "cron_clean": "30 00"}, "clean", "30 00 * * 7 "),
    ({"period": "nightly", "cron_clean": "30 00"}, "clean", "30 00 * * 3-6 "),
    (
        {"period": "nightly_all", "cron_clean": "30 00"},
        "clean",
        "30 00 * * 2-6 ",
    ),
]


@pytest.mark.parametrize(
    ("suite", "mode", "expected"),
    [test_data for test_data in data_generate_cron_timing_str],
)
def test_generate_cron_timing_str(suite, mode, expected):
    assert generate_cron_timing_str(suite, mode) == expected


# Test generate_clean_commands
data_generate_clean_commands = [
    (
        "8",
        "suite_name",
        "cron_log",
        "export CYLC_VERSION=8 ; bash -l cylc stop --kill 'suite_name' >/dev/null 2>&1 ; sleep 10 ; bash -l cylc clean --timeout=7200 -y -q suite_name >> cron_log 2>&1\n",
    ),
    (
        "8-next",
        "suite_name",
        "cron_log",
        "export CYLC_VERSION=8-next ; bash -l cylc stop --kill 'suite_name' >/dev/null 2>&1 ; sleep 10 ; bash -l cylc clean --timeout=7200 -y -q suite_name >> cron_log 2>&1\n",
    ),
]


@pytest.mark.parametrize(
    ("cylc_version", "name", "log_file", "expected"),
    [test_data for test_data in data_generate_clean_commands],
)
def test_generate_clean_commands(cylc_version, name, log_file, expected):
    assert generate_clean_commands(cylc_version, name, log_file) == expected


# Test populate_cl_variables
data_populate_cl_variables = [
    ({"vars": ["var1", "var2", "var3"]}, "-S var1 -S var2 -S var3 "),
    ({}, ""),
]


@pytest.mark.parametrize(
    ("suite", "expected"),
    [test_data for test_data in data_populate_cl_variables],
)
def test_populate_cl_variables(suite, expected):
    assert populate_cl_variables(suite) == expected


# Test create_git_clone_cron
def test_create_git_clone_cron():
    expected = (
        "###################################\n"
        "# Clone repo - every day at 23:30 #\n"
        "###################################\n"
        f"30 23 * * * rm -rf {CLONE_DIR}/clone_repo ; git clone {MIRROR_PATH}MetOffice/repo.git {CLONE_DIR}/clone_repo\n\n\n"
    )
    assert create_git_clone_cron("repo") == expected


# Test generate_cylc_command
data_generate_cylc_command = [
    (
        {"groups": "groups"},
        "/path/to/clone",
        "8",
        "test_name",
        "export CYLC_VERSION=8 ; bash -l cylc vip -z g=groups -n test_name -S USE_MIRRORS=true /path/to/clone/rose-stem ",
    ),
    (
        {"groups": "groups", "revisions": "set"},
        "/path/to/clone",
        "8",
        "test_name",
        "export CYLC_VERSION=8 ; bash -l cylc vip -z g=groups -n test_name -S USE_MIRRORS=true /path/to/clone/rose-stem ",
    ),
    (
        {"groups": "groups", "revisions": "set"},
        "/path/to/clone",
        "8-next",
        "test_name",
        "export CYLC_VERSION=8-next ; bash -l cylc vip -z g=groups -n test_name -S USE_MIRRORS=true /path/to/clone/rose-stem ",
    ),
    (
        {"groups": "groups", "revisions": "heads"},
        "/path/to/clone",
        "8",
        "test_name",
        "export CYLC_VERSION=8 ; bash -l cylc vip -z g=groups -n test_name -S USE_MIRRORS=true -S USE_HEADS=true /path/to/clone/rose-stem ",
    ),
]


@pytest.mark.parametrize(
    ("suite", "clone_path", "version", "name", "expected"),
    [test_data for test_data in data_generate_cylc_command],
)
def test_generate_cylc_command(suite, clone_path, version, name, expected):
    assert generate_cylc_command(suite, clone_path, version, name) == expected
