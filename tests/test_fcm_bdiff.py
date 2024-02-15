import pytest
from fcm_bdiff import *

# Use Case Testing for get_branch_diff_filenames

"""
Use Case 1 - Test changes made to a working copy,
use path to working copy as the first parameter
"""
# Test Data
# Test with:
# <path_to_working_copy_in_user_home_directory>
use_case_1_branch = ""
# Expected
# A list of [ <path_to_working_copy_in_user_home_directory>/
# <paths_to_subdirectories_of_modified_files>,
# <path_to_working_copy_in_user_home_directory>/<paths_to_modified_files> ]
use_case_1_expected = []

# Use Case 2 - Test changes made to a working copy,
# use url of the branch as the first parameter
# svn/um/main variant
use_case_2_1_branch = 'https://code.metoffice.gov.uk/svn/um/main/branches/'\
                + 'dev/Share/r118291_fcm_bdiff_share_testing_branch'
use_case_2_1_expected = [
        'https://code.metoffice.gov.uk/svn/um/main/branches/'
        + 'dev/Share/r118291_fcm_bdiff_share_testing_branch/src/atmosphere/'
        + 'AC_assimilation/ac2.F90'
        ]
# trac/um/browser variant
use_case_2_2_branch = 'fcm:um.x_br/'\
        + 'dev/Share/r118291_fcm_bdiff_share_testing_branch'
use_case_2_2_expected = [
        'fcm:um.x_br/dev/Share/r118291_fcm_bdiff_share_testing_branch/src/'
        + 'atmosphere/AC_assimilation/ac2.F90'
        ]


# Use Case 3 - Test changes made to a working copy,
# use nothing as the first parameter to run from within a working copy
use_case_3_branch = ""
use_case_3_expected = []


@pytest.mark.parametrize(
    # this first tuple is the list of variable names for the test function
    ("url", "expected"),
    # This list is a list of values, as tuples, to loop over and feed into
    # the test function.
    [
        ("../../_svn/main/trunk", True),  # PASS
        ("../incorrect_value/trunk", False),  # PASS
        ("..*_svn/main/trunk", True),  # PASS
        ("..*incorrect_value/trunk", False)  # PASS
    ]
)
def test_is_trunk(url, expected):
    assert is_trunk(url) == expected


@pytest.mark.parametrize(
    ("bytes_type_string", "expected"),
    [
        (
            bytes("my_test_string", encoding='utf-8'),
            "my_test_string"
        ),  # PASS
        (
            bytes("my_test_string", encoding='cp1252'),
            "my_test_string"
        )  # PASS
    ]
)
def test_text_decoder(bytes_type_string, expected):
    assert text_decoder(bytes_type_string) == expected


@pytest.mark.parametrize(
    ("branch", "expected"),
    [
        # Use Case 2
        (use_case_2_1_branch, use_case_2_1_expected),  # PASS
        (use_case_2_2_branch, use_case_2_2_expected)  # PASS
    ]
)
def test_get_branch_diff_filenames(branch, expected):
    assert get_branch_diff_filenames(branch) == expected
