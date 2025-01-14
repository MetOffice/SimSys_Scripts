import pytest
import subprocess
import shutil
from ..apply_macros import *

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
expected_split_macros = [
    desired_macro,
    existing_macro
]

# ApplyMacros below requires an LFRic Apps working copy to work - check out the
# head of the lfric_apps trunk for this purpose. The actual contents of the
# working copy are not important for the purposes of the unit tests
appsdir = tempfile.mkdtemp()
result = subprocess.run(
    f"fcm co fcm:lfric_apps.x_tr {appsdir}".split(),
    check=False,
    capture_output=True,
    text=True,
    timeout=120
)
if result.returncode:
    raise RuntimeError(
        "Failed to checkout required LFRic Apps Working Copy with error ",
        result.stderr
    )

# Create an instance of the apply_macros class
# Pass a known directory in as the Jules and Core sources as these are not
# required for testing
am = ApplyMacros(
    "vn0.0_t001",
    None,
    appsdir,
    "/tmp",
    "/tmp"
)

def test_split_macros():
    m = split_macros(test_versions_file)
    # Remove trailing newlines as these are unimportant
    for i, item in enumerate(m):
        m[i] = item.strip("\n")
    assert m == expected_split_macros


def test_find_macro():
    assert am.find_macro("meta_dir", expected_split_macros) == desired_macro
    assert am.find_macro("meta_dir", [existing_macro]) == ""
    expected_error = r".*meta_dir/versions.py.*"
    with pytest.raises(Exception, match=expected_error):
        am.find_macro("meta_dir", [""])


def test_find_last_macro():
    assert am.find_last_macro(expected_split_macros, "meta_dir") == "vn0.0_t001"


def test_parse_macro():
    am.parse_macro(desired_macro, "meta_dir")
    expected_dict = {
        "before_tag": "vn0.0_t000",
        "commands": (
        '        self.add_setting(\n'
        '            config, ["namelist:namelist1", "opt1"], "value1"\n'
        '        )\n'
        )
    }
    assert am.parsed_macros["meta_dir"] == expected_dict
    assert am.ticket_number == "#001"
    assert am.author == "Test Author"
    with pytest.raises(Exception, match=r".*failed/versions.py"):
        am.parse_macro("", "failed")


def test_read_meta_imports():
    am.parsed_macros["tests/test_meta_dir"] = {}
    am.parsed_macros["tests/test_meta_dir"]["imports"] = am.read_meta_imports("tests/test_meta_dir")
    expected_imports = [
        os.path.join(am.root_path, "science", "gungho"),
        os.path.join(am.root_path, "applications", "lfric_atm")
    ]
    assert am.parsed_macros["tests/test_meta_dir"]["imports"] == expected_imports
    expected_meta = [os.path.join(am.root_path, "applications", "lfric_atm")]
    assert am.read_meta_imports("tests/test_meta_dir/rose-app.conf", "meta") == expected_meta


def test_determine_import_order():
    am.parsed_macros["import1"] = {}
    am.parsed_macros["import2"] = {}
    am.parsed_macros["import3"] = {}
    am.parsed_macros["import4"] = {}
    am.parsed_macros["import1"]["imports"] = ["import3", "import2"]
    am.parsed_macros["import3"]["imports"] = ["import4"]
    am.parsed_macros["import4"]["imports"] = []
    am.parsed_macros["import2"]["imports"] = []
    expected_order = [
        "import2",
        "import4",
        "import3",
        "import1"
    ]
    assert am.determine_import_order("import1") == expected_order


def test_combine_macros():
    am.parsed_macros["importA"] = {
        "commands": "        importA command"
    }
    am.parsed_macros["importB"] = {
        "commands": "        importB command"
    }
    expected_combined = (
        "        # Commands From: importA\n        importA command\n"
        "        # Commands From: importB\n        importB command\n"
    )
    assert am.combine_macros(["importA", "importB"]) == expected_combined


def test_parse_application_section():
    assert am.parse_application_section("meta_dir/HEAD") == "meta_dir"
    assert am.parse_application_section("meta_dir/versions.py") == "meta_dir"
    assert am.parse_application_section(f"{am.root_path}/meta_dir") == "meta_dir"
    assert am.parse_application_section(f"{am.core_source}/meta_dir") == "meta_dir"


def test_deduplicate_list():
    assert deduplicate_list([1,2,3]) == [1,2,3]
    assert deduplicate_list([1,2,2,3,3,3,]) == [1,2,3]
    assert deduplicate_list([1,2,1,3,2]) == [1,2,3]


def test_match_python_imports():
    assert match_python_import("import z") == True
    assert match_python_import("from x import y") == True
    assert match_python_import("from a import b.c") == True
    assert match_python_import("import m as n") == True
    assert match_python_import("false") == False

# Remove appsdir
@pytest.fixture(scope='session', autouse=True)
def remove_tempdir():
    yield
    shutil.rmtree(appsdir)
