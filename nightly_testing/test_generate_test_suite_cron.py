import pytest
from generate_test_suite_cron import *

PROFILE = ". /etc/profile"

# Test join_checkout_commands
data_join_checkout_commands = [
    (
        ["um"],
        "scratch/dir/",
        "rm -rf scratch/dir/wc_um ; fcm co -q --force fcm:um.xm_tr@HEAD scratch/dir/wc_um ; "
    ),
    (
        ["um", "lfric"],
        "scratch/dir",
        "rm -rf scratch/dir/wc_um ; fcm co -q --force fcm:um.xm_tr@HEAD scratch/dir/wc_um ; rm -rf scratch/dir/wc_lfric ; fcm co -q --force fcm:lfric.xm_tr@HEAD scratch/dir/wc_lfric ; "
    )
]
@pytest.mark.parametrize(
    ("inlist", "scratch", "expected"),
    [test_data for test_data in data_join_checkout_commands]
)
def test_join_checkout_commands(inlist, scratch, expected):
    assert join_checkout_commands(inlist, scratch) == expected


# Test lfric_heads_sed
data_lfric_heads_sed = [
    (
        "path/to/wc",
        "sed -i -e 's/^\\(export .*_revision=@\\).*/\\1HEAD/' path/to/wc/dependencies.sh ; sed -i -e 's/^\\(export .*_rev=\\).*/\\1HEAD/' path/to/wc/dependencies.sh ; "
    )
]
@pytest.mark.parametrize(
    ("wc_path", "expected"),
    [test_data for test_data in data_lfric_heads_sed]
)
def test_lfric_heads_sed(wc_path, expected):
    assert lfric_heads_sed(wc_path) == expected


# Test generate_cron_timing_str
data_generate_cron_timing_str = [
    (
        {"period": "weekly", "cron_launch": "30 00"},
        "main",
        "30 00 * * 1 "
    ),
    (
        {"period": "nightly", "cron_launch": "30 00"},
        "main",
        "30 00 * * 2-5 "
    ),
    (
        {"period": "nightly_all", "cron_launch": "30 00"},
        "main",
        "30 00 * * 1-5 "
    ),
    (
        {"period": "weekly", "cron_clean": "30 00"},
        "clean",
        "30 00 * * 7 "
    ),
    (
        {"period": "nightly", "cron_clean": "30 00"},
        "clean",
        "30 00 * * 3-6 "
    ),
    (
        {"period": "nightly_all", "cron_clean": "30 00"},
        "clean",
        "30 00 * * 2-6 "
    ),
    (
        {"period": "weekly", "cron_clean": "30 00"},
        "monitoring",
        "00 06 * * 1 "
    ),
    (
        {"period": "nightly", "cron_clean": "30 00"},
        "monitoring",
        "00 06 * * 2-5 "
    ),
    (
        {"period": "nightly_all", "cron_clean": "30 00"},
        "monitoring",
        "00 06 * * 1-5 "
    ),
]
@pytest.mark.parametrize(
    ("suite", "mode", "expected"),
    [test_data for test_data in data_generate_cron_timing_str]
)
def test_generate_cron_timing_str(suite, mode, expected):
    assert generate_cron_timing_str(suite, mode) == expected


# Test generate_clean_commands
data_generate_clean_commands = [
    (
        7,
        "suite_name",
        "cron_log",
        f"{PROFILE} ; export CYLC_VERSION=7 ; rose suite-clean -y -q suite_name >> cron_log 2>&1\n"
    ),
    (
        8,
        "suite_name",
        "cron_log",
        f"{PROFILE} ; export CYLC_VERSION=8 ; cylc clean --timeout=3600 -y -q suite_name >> cron_log 2>&1\n"
    )
]
@pytest.mark.parametrize(
    ("cylc_version", "name", "log_file", "expected"),
    [test_data for test_data in data_generate_clean_commands]
)
def test_generate_clean_commands(cylc_version, name, log_file, expected):
    assert generate_clean_commands(cylc_version, name, log_file) == expected
