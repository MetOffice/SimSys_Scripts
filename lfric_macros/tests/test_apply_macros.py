# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------
"""
Unit tests for apply_macros
"""

import os
import shutil
import subprocess
import tempfile
from shlex import split

import pytest

from ..apply_macros import (
    ApplyMacros,
    split_macros,
    match_python_import,
    deduplicate_list,
    read_versions_file,
    read_python_imports
)

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
    # Upgrade macro for ticket #000 by <Test Author>.
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

# ApplyMacros below requires an LFRic Apps working copy to work - check out the
# head of the lfric_apps trunk for this purpose. The actual contents of the
# working copy are not important for the purposes of the unit tests
appsdir = tempfile.mkdtemp()
result = subprocess.run(
    split(f"git clone --depth 1 https://github.com/MetOffice/lfric_apps.git {appsdir}"),
    check=False,
    capture_output=True,
    text=True,
    timeout=120,
)
if result.returncode:
    raise RuntimeError(
        "Failed to clone LFRic Apps with error:\n",
        result.stderr,
    )

# Create an instance of the apply_macros class
# Pass a known directory in as the Jules and Core sources as these are not
# required for testing
applymacros = ApplyMacros("vn0.0_t001", None, None, appsdir, "/tmp", "/tmp")


def test_split_macros():
    m = split_macros(test_versions_file)
    # Remove trailing newlines as these are unimportant
    for i, item in enumerate(m):
        m[i] = item.strip("\n")
    assert m == expected_split_macros


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
    applymacros.parsed_macros["meta_dir"] = applymacros.parse_macro(
        desired_macro, "meta_dir"
    )
    expected_dict = {
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
    }
    assert applymacros.parsed_macros["meta_dir"] == expected_dict
    with pytest.raises(Exception, match=r".*failed/versions.py"):
        applymacros.parse_macro("", "failed")


def test_read_meta_imports():
    applymacros.parsed_macros["tests/test_meta_dir"] = {}
    applymacros.parsed_macros["tests/test_meta_dir"]["imports"] = (
        applymacros.read_meta_imports("tests/test_meta_dir")
    )
    expected_imports = [
        os.path.join(applymacros.root_path, "rose-meta", "lfric-gungho"),
        os.path.join(applymacros.root_path, "rose-meta", "lfric-lfric_atm"),
    ]
    assert (
        applymacros.parsed_macros["tests/test_meta_dir"]["imports"] == expected_imports
    )

    expected_meta = [
        os.path.join(applymacros.root_path, "rose-meta", "lfric-lfric_atm")
    ]
    assert (
        applymacros.read_meta_imports("tests/test_meta_dir/rose-app.conf", "meta")
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
    print(result)
    print()
    print(expected_combined)
    assert result == expected_combined


def test_parse_application_section():
    assert applymacros.parse_application_section("meta_dir/HEAD") == "meta_dir"
    assert applymacros.parse_application_section("meta_dir/versions.py") == "meta_dir"
    assert (
        applymacros.parse_application_section(f"{applymacros.root_path}/meta_dir")
        == "meta_dir"
    )
    assert (
        applymacros.parse_application_section(f"{applymacros.core_source}/meta_dir")
        == "meta_dir"
    )


def test_match_python_imports():
    assert match_python_import("import z")
    assert match_python_import("from x import y")
    assert match_python_import("from a import b.c")
    assert match_python_import("import m as n")
    assert not match_python_import("false")


def test_read_versions_file():
    """
    Test read_versions_file
    """

    assert read_versions_file(os.path.join("tests", "test_meta_dir")) == ["# line 1\n", "# line 2\n"]


def test_deduplicate_list():
    """
    Test deduplicate_list
    """

    before = [1,2,2,"a","b","b",{0:1,1:2},{0:1,1:2}]
    after = [1,2,"a","b",{0:1,1:2},]

    assert deduplicate_list(before) == after
    assert deduplicate_list(after) == after


def test_read_python_imports():
    """
    Test read_python_imports
    """

    test_file = os.path.join("tests", "test_meta_dir", "test_read_python_imports.py")
    expected = set()
    imports = (('shlex',), ('split',), None), (('pathlib',), ('PurePath',), None), ((), ('os',), None), (('pathlib',), ('Path',), None), ((), ('networkx',), 'nx')
    for item in imports:
        expected.add(item)
    assert read_python_imports(test_file) == expected


# Remove appsdir
@pytest.fixture(scope="session", autouse=True)
def remove_tempdir():
    yield
    shutil.rmtree(appsdir)
