# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------
"""
Unit tests for apply_macros
"""

from os import path
import pytest

from ..apply_macros import (
    ApplyMacros,
    split_macros,
    match_python_import,
    deduplicate_list,
    read_versions_file,
    read_python_imports,
)

# Commonly used paths
TEST_APPS_DIR = path.join("tests", "test_lfric_apps_dir")
TEST_META_DIR = path.join(TEST_APPS_DIR, "rose-meta")
TEST_ROSE_STEM = path.join(TEST_APPS_DIR, "rose-stem", "app")


# A macro that we want to find for these tests
desired_macro = """class vn00_t001(MacroUpgrade):
    # Upgrade macro for ticket #001 by <Test Author>.
    BEFORE_TAG = "vn0.0_t000"
    AFTER_TAG = "vn0.0_t001"
    def upgrade(self, config, meta_config=None):
        # Input your macro commands here
        self.add_setting(
            config, ["namelist:namelist1", "opt1"], "value1"
        )
        return config, self.reports"""


# A macro pre-existing in the file, forming part of the chain
existing_macro = """class vn00_t000(MacroUpgrade):
    # Upgrade macro for ticket #000 by <Previous Author>.
    BEFORE_TAG = "vn0.0"
    AFTER_TAG = "vn0.0_t000"
    def upgrade(self, config, meta_config=None):
        # Input your macro commands here
        self.add_setting(
            config, ["namelist:namelist0", "opt0"], "value0"
        )
        return config, self.reports"""


# test_versions_file is a string representing an imaginary versions.py file to
# unit test functions from. This is the parsed version so contains no blank line
test_versions_file = """import sys
from metomi.rose.upgrade import MacroUpgrade
class UpgradeError(Exception):
    def __init__(self, msg):
        self.msg = msg
    def __repr__(self):
        sys.tracebacklimit = 0
        return self.msg
    __str__ = __repr__"""
test_versions_file += f"\n{desired_macro}\n{existing_macro}"
test_versions_file = [f"{x}\n" for x in test_versions_file.split("\n")]

# The expected result from split_macros for the versions
expected_split_macros = [desired_macro, existing_macro]

# Create an instance of the apply_macros class
# Use /tmp for Core and Jules as these are not required for testing
applymacros = ApplyMacros("vn0.0_t001", None, None, TEST_APPS_DIR, "/tmp", "/tmp", True)


def test_read_versions_file():
    """
    Test read_versions_file
    """

    assert read_versions_file(
        path.join(TEST_APPS_DIR, "rose-meta", "lfric-gungho")
    ) == ["# line 1\n", "# line 2\n"]


def test_find_meta_dirs():
    result = applymacros.find_meta_dirs(path.join(TEST_APPS_DIR, "rose-meta"))
    expected = set()
    expected.add(path.join(TEST_META_DIR, "lfric-lfric_atm"))
    expected.add(path.join(TEST_META_DIR, "lfric-gungho"))
    expected.add(path.join(TEST_META_DIR, "lfric-driver"))
    expected.add(path.join(TEST_META_DIR, "um-iau"))
    expected.add(path.join(TEST_META_DIR, "lfric-transport"))
    assert result == expected
    applymacros.meta_dirs = result


def test_split_macros():
    result = split_macros(test_versions_file)
    # Remove trailing newlines as these are unimportant
    for i, item in enumerate(result):
        result[i] = item.strip("\n")
    assert result == expected_split_macros


def test_match_python_imports():
    assert match_python_import("import z")
    assert match_python_import("from x import y")
    assert match_python_import("from a import b.c")
    assert match_python_import("import m as n")
    assert not match_python_import("false")


def test_deduplicate_list():
    """
    Test deduplicate_list
    """

    before = [1, 2, 2, "a", "b", "b", {0: 1, 1: 2}, {0: 1, 1: 2}]
    after = [
        1,
        2,
        "a",
        "b",
        {0: 1, 1: 2},
    ]

    assert deduplicate_list(before) == after
    assert deduplicate_list(after) == after


def test_read_python_imports():
    """
    Test read_python_imports
    """

    test_file = path.join(TEST_APPS_DIR, "test_read_python_imports.py")
    expected = set()
    imports = (
        (("shlex",), ("split",), None),
        (("pathlib",), ("PurePath",), None),
        ((), ("os",), None),
        (("pathlib",), ("Path",), None),
        ((), ("networkx",), "nx"),
    )
    for item in imports:
        expected.add(item)
    assert read_python_imports(test_file) == expected


def test_find_macro():
    assert applymacros.find_macro("meta_dir", expected_split_macros) == desired_macro
    assert applymacros.find_macro("meta_dir", [existing_macro]) == ""
    expected_error = r".*meta_dir/versions.py.*"
    with pytest.raises(Exception, match=expected_error):
        applymacros.find_macro("meta_dir", [""])


def test_find_last_macro():
    assert (
        applymacros.find_last_macro(expected_split_macros, "meta_dir") == "vn0.0_t001"
    )


def test_parse_macro():
    for macro in (existing_macro, desired_macro):
        applymacros.parsed_macros["meta_dir"].append(
            applymacros.parse_macro(macro, "meta_dir")
        )
    expected_macros_list = [
        {
            "before_tag": "vn0.0",
            "after_tag": "vn0.0_t000",
            "commands": (
                "        self.add_setting(\n"
                '            config, ["namelist:namelist0", "opt0"], "value0"\n'
                "        )\n"
            ),
            "ticket_number": "#000",
            "author": "Previous Author",
            "class_name": "vn00_t000",
        },
        {
            "before_tag": "vn0.0_t000",
            "after_tag": "vn0.0_t001",
            "commands": (
                "        self.add_setting(\n"
                '            config, ["namelist:namelist1", "opt1"], "value1"\n'
                "        )\n"
            ),
            "ticket_number": "#001",
            "author": "Test Author",
            "class_name": "vn00_t001",
        },
    ]
    assert applymacros.parsed_macros["meta_dir"] == expected_macros_list
    with pytest.raises(Exception, match=r".*failed/versions.py"):
        applymacros.parse_macro("", "failed")


def test_check_missing_macro():
    macros = applymacros.parsed_macros["meta_dir"]
    applymacros.parsed_macros[path.join(TEST_META_DIR, "lfric-gungho")] = macros
    missing = applymacros.check_missing_macros(
        path.join(TEST_META_DIR, "lfric-lfric_atm"), ["lfric-gungho"]
    )
    assert missing == ["vn0.0_t000"]
    missing = applymacros.check_missing_macros(
        path.join(TEST_META_DIR, "lfric-gungho"), []
    )
    assert missing == []


def test_combine_missing_macros():
    combined = applymacros.combine_missing_macros(
        [path.join(TEST_META_DIR, "lfric-gungho")], ["vn0.0_t000"]
    )
    expected_combined = {
        "vn0.0_t000": {
            "before_tag": "vn0.0",
            "after_tag": "vn0.0_t000",
            "commands": (
                "        self.add_setting(\n"
                '            config, ["namelist:namelist0", "opt0"], "value0"\n'
                "        )\n"
            ),
            "ticket_number": "#000",
            "author": "Previous Author",
            "class_name": "vn00_t000",
        },
    }
    assert combined == expected_combined


def test_read_meta_imports():
    applymacros.parsed_macros[TEST_APPS_DIR] = {}
    applymacros.parsed_macros[TEST_APPS_DIR]["imports"] = applymacros.read_meta_imports(
        path.join(TEST_APPS_DIR, "rose-meta", "lfric-gungho")
    )
    expected_imports = [
        path.join(applymacros.root_path, "rose-meta", "lfric-driver"),
        path.join(applymacros.root_path, "rose-meta", "um-iau"),
    ]
    assert applymacros.parsed_macros[TEST_APPS_DIR]["imports"] == expected_imports

    expected_meta = [
        path.join(applymacros.root_path, "rose-meta", "lfric-lfric_atm", "vn0.0")
    ]
    assert (
        applymacros.read_meta_imports(
            path.join(TEST_APPS_DIR, "rose-stem", "app", "lfric_atm", "rose-app.conf"),
            "meta",
        )
        == expected_meta
    )


def test_determine_import_order():
    applymacros.target_macros["import1"] = {}
    applymacros.target_macros["import2"] = {}
    applymacros.target_macros["import3"] = {}
    applymacros.target_macros["import4"] = {}
    applymacros.target_macros["import1"]["imports"] = ["import3", "import2"]
    applymacros.target_macros["import3"]["imports"] = ["import4"]
    applymacros.target_macros["import4"]["imports"] = []
    applymacros.target_macros["import2"]["imports"] = []
    expected_order = ["import2", "import4", "import3", "import1"]
    assert applymacros.determine_import_order("import1") == expected_order


def test_combine_macros():
    applymacros.target_macros[applymacros.get_full_import_path("importA")] = {
        "commands": "        importA command",
        "author": "Test Author",
        "ticket_number": "#001",
    }
    applymacros.target_macros[applymacros.get_full_import_path("importB")] = {
        "commands": "        importB command",
        "author": "Test Author",
        "ticket_number": "#001",
    }
    expected_combined = (
        "        # Commands From: rose-meta/importA\n        importA command\n"
        "        # Commands From: rose-meta/importB\n        importB command\n"
    )
    result = applymacros.combine_macros(["importA", "importB"])
    assert result == expected_combined


def test_parse_application_section():
    assert (
        applymacros.parse_application_section(path.join("meta_dir", "HEAD"))
        == "meta_dir"
    )
    assert (
        applymacros.parse_application_section(path.join("meta_dir", "versions.py"))
        == "meta_dir"
    )
    assert (
        applymacros.parse_application_section(
            path.join(applymacros.root_path, "meta_dir")
        )
        == "meta_dir"
    )
    assert (
        applymacros.parse_application_section(
            path.join(applymacros.core_source, "meta_dir")
        )
        == "meta_dir"
    )


def test_read_dependencies():
    assert applymacros.read_dependencies("test_repo") == ("test_source", "test_ref")


def test_order_meta_dirs():
    applymacros.target_macros = {
        path.join(TEST_META_DIR, "lfric-driver"): {"imports": []},
        path.join(TEST_META_DIR, "lfric-gungho"): {
            "imports": [
                path.join(TEST_META_DIR, "lfric-driver"),
                path.join(TEST_META_DIR, "um-iau"),
            ]
        },
        path.join(TEST_META_DIR, "um-iau"): {"imports": []},
        path.join(TEST_META_DIR, "lfric-lfric_atm"): {
            "imports": [path.join(TEST_META_DIR, "lfric-gungho")]
        },
        path.join(TEST_META_DIR, "lfric-transport"): {
            "imports": [path.join(TEST_META_DIR, "lfric-gungho")]
        },
    }
    order = applymacros.order_meta_dirs()
    gungho = order.index(path.join(TEST_META_DIR, "lfric-gungho"))
    lfric_atm = order.index(path.join(TEST_META_DIR, "lfric-lfric_atm"))
    driver = order.index(path.join(TEST_META_DIR, "lfric-driver"))
    um = order.index(path.join(TEST_META_DIR, "um-iau"))
    assert gungho > driver
    assert gungho > um
    assert lfric_atm > gungho


def test_get_rose_apps():
    expected = set()
    expected.add(path.join(TEST_ROSE_STEM, "gungho"))
    expected.add(path.join(TEST_ROSE_STEM, "lfric_atm"))
    expected.add(path.join(TEST_ROSE_STEM, "transport"))
    assert applymacros.get_rose_apps() == expected


def test_apps_to_upgrade():
    applymacros.sections_with_macro = [
        path.join(TEST_META_DIR, "lfric-gungho"),
        path.join(TEST_META_DIR, "lfric-lfric_atm"),
    ]
    expected = (
        [path.join(TEST_ROSE_STEM, "gungho"), path.join(TEST_ROSE_STEM, "lfric_atm")],
        [path.join(TEST_ROSE_STEM, "lfric_atm"), path.join(TEST_ROSE_STEM, "gungho")],
    )
    assert applymacros.apps_to_upgrade() in expected
