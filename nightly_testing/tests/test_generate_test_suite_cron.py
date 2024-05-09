import pytest
from generate_test_suite_cron import *

PROFILE = ". /etc/profile"

# Test join_checkout_commands
data_join_checkout_commands = [
    (
        ["um"],
        "scratch/dir/",
        "fcm co -q --force fcm:um.xm_tr@HEAD scratch/dir/wc_um ; "
    ),
    (
        ["um", "lfric"],
        "scratch/dir",
        "fcm co -q --force fcm:um.xm_tr@HEAD scratch/dir/wc_um ; fcm co -q --force fcm:lfric.xm_tr@HEAD scratch/dir/wc_lfric ; "
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
        "cp -rf path/to/wc path/to/wc_heads ; sed -i -e 's/^\\(export .*_revision=@\\).*/\\1HEAD/' path/to/wc_heads/dependencies.sh ; sed -i -e 's/^\\(export .*_rev=\\).*/\\1HEAD/' path/to/wc_heads/dependencies.sh ; ",
    )
]
@pytest.mark.parametrize(
    ("wc_path", "expected"),
    [test_data for test_data in data_lfric_heads_sed]
)
def test_lfric_heads_sed(wc_path, expected):
    assert lfric_heads_sed(wc_path) == (expected)


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
        "7",
        "suite_name",
        "cron_log",
        f"{PROFILE} ; export CYLC_VERSION=7 ; cylc stop 'suite_name' >/dev/null 2>&1 ; sleep 10 ; rose suite-clean -y -q suite_name >> cron_log 2>&1\n"
    ),
    (
        "8",
        "suite_name",
        "cron_log",
        f"{PROFILE} ; export CYLC_VERSION=8 ; cylc stop 'suite_name' >/dev/null 2>&1 ; sleep 10 ; cylc clean --timeout=7200 -y -q suite_name >> cron_log 2>&1\n"
    ),
    (
        "8-next",
        "suite_name",
        "cron_log",
        f"{PROFILE} ; export CYLC_VERSION=8-next ; cylc stop 'suite_name' >/dev/null 2>&1 ; sleep 10 ; cylc clean --timeout=7200 -y -q suite_name >> cron_log 2>&1\n"
    )
]
@pytest.mark.parametrize(
    ("cylc_version", "name", "log_file", "expected"),
    [test_data for test_data in data_generate_clean_commands]
)
def test_generate_clean_commands(cylc_version, name, log_file, expected):
    assert generate_clean_commands(cylc_version, name, log_file) == expected


# Test generate_rose_stem_command
data_generate_rose_stem_command = [
    (
        {"groups": "all"},
        "path/to/wc",
        "7",
        "suite_name",
        "export CYLC_VERSION=7 ; rose stem --group=all --name=suite_name --source=path/to/wc "
    ),
    (
        {"groups": "nightly"},
        "path/to/wc",
        "8",
        "suite_name",
        "export CYLC_VERSION=8 ; rose stem --group=nightly --workflow-name=suite_name --source=path/to/wc "
    ),
    (
        {"groups": "nightly"},
        "path/to/wc",
        "8-next",
        "suite_name",
        "export CYLC_VERSION=8-next ; rose stem --group=nightly --workflow-name=suite_name --source=path/to/wc "
    )
]
@pytest.mark.parametrize(
    ("suite", "wc_path", "cylc_version", "name", "expected"),
    [test_data for test_data in data_generate_rose_stem_command]
)
def test_generate_rose_stem_command(suite, wc_path, cylc_version, name, expected):
    assert generate_rose_stem_command(suite, wc_path, cylc_version, name) == expected


# Test populate_heads_sources
data_populate_heads_sources = [
    (
        {
            "repo": "um",
            "revisions": "heads",
        },
        "--source=fcm:casim.xm_tr@HEAD --source=fcm:jules.xm_tr@HEAD --source=fcm:mule.xm_tr@HEAD --source=fcm:shumlib.xm_tr@HEAD --source=fcm:socrates.xm_tr@HEAD --source=fcm:ukca.xm_tr@HEAD "
    ),
    (
        {
            "repo": "um",
            "revisions": "set",
        },
        ""
    ),
    (
        {
            "repo": "um",
        },
        ""
    ),
    (
        {
            "repo": "jules",
            "revisions": "heads",
        },
        ""
    ),
    (
        {
            "repo": "lfric_apps",
            "revisions": "heads"
        },
        ""
    )
]
@pytest.mark.parametrize(
    ("suite", "expected"),
    [test_data for test_data in data_populate_heads_sources]
)
def test_populate_heads_sources(suite, expected):
    assert populate_heads_sources(suite) == expected


# Test populate_cl_variables
data_populate_cl_variables = [
    (
        {
            "vars": ["var1", "var2", "var3"]
        },
        "-S var1 -S var2 -S var3 "
    ),
    (
        {},
        ""
    )
]
@pytest.mark.parametrize(
    ("suite", "expected"),
    [test_data for test_data in data_populate_cl_variables]
)
def test_populate_cl_variables(suite, expected):
    assert populate_cl_variables(suite) == expected


# Test major_cylc_version
data_major_cylc_version = [
    (
        "7",
        7
    ),
    (
        "8",
        8
    ),
    (
        "8-next",
        8
    ),
    (
        8,
        8
    )
]
@pytest.mark.parametrize(
    ("version", "expected"),
    [test_data for test_data in data_major_cylc_version]
)
def test_major_cylc_version(version, expected):
    assert major_cylc_version(version) == expected
