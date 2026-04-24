# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

# from pyparsing import remove_quotes
import pytest
import sys
from pathlib import Path

# Add the current directory to Python path
sys.path.insert(0, str(Path(__file__).parent.parent))
from umdp3_rules_S3 import (
    remove_comments,
    remove_quoted,
    concatenate_lines,
    r3_1_1_there_can_be_only_one,
    r3_2_1_check_crown_copyright,
    r3_3_2line_too_long,
    r3_4_1_capitalised_keywords,
    r3_4_2_no_full_uppercase_variable_names,
)
# from umdp3_checker_rules import TestResult, UMDP3Checker


def modify_fortran_lines(lines_in: list[str], changes: list[list]) -> list[str]:
    """Return a copy of example_fortran_lines with changes applied.

    ``changes`` is a list of operation list, each with:
      - ``[<operation>, <N>, [<new line(s)>]]``
      - where : <operation> = "replace" would replace line N with the new line(s)
      -         <operation> = delete" would remove line N
      - and     <operation> ="add" would insert the new line(s) before line N.

    Operations are applied in descending line order so that earlier
    line numbers are not shifted by later mutations.
    """
    """ TODO: Currently <new lines> is a string, but should become a list of strings,
    allowing multiple lines to be used in addition and as replacements. Although,
    this may make keeping track of line numbers more complex."""
    lines = lines_in.copy()
    for change in sorted(changes, key=lambda o: o[1], reverse=True):
        idx = int(change[1]) - 1
        if change[0] == "replace":
            del lines[idx]
            print(f"Line {idx} :  deleting line.")
            for new_line in change[2]:
                print(f'Line {idx} :  adding "{new_line}"')
                lines.insert(idx, new_line)
                idx += 1
        elif change[0] == "delete":
            print(f"Line {idx} :  deleting line.")
            del lines[idx]
        elif change[0] == "add":
            for new_line in change[2]:
                print(f'Line {idx} :  adding "{new_line}"')
                lines.insert(idx, new_line)
                idx += 1
        else:
            raise ValueError(f"Unknown operation: {change[0]}")
        # for count, line in enumerate(lines, 1):
        #      print(f"line [{count}]: {line}")
    return lines


# =================================================================
"""First : test the helper functions in umdp3_rules_S3.py."""


def test_modify_fortran_lines(example_fortran_lines):
    """TODO: Does this need to be more rigorous ?"""
    changes_list = [
        ["replace", 10, ["This is a replacement line."]],
        ["delete", 20, None],
        ["add", 30, ["This is an added line."]],
    ]
    modified_lines = modify_fortran_lines(example_fortran_lines, changes_list)
    # Line no.s below have to be carefully calculated. Basic is line no of change -1 but then add one for every line added aobve in the file and -1 for every line deleted above in the file.
    # for line_no, line in enumerate(modified_lines, 1):
    #     print(f"line [{line_no:04}]: {line}")
    assert modified_lines[9] == "This is a replacement line."
    assert len(modified_lines) == len(
        example_fortran_lines
    )  # One line deleted, One added
    # Added @ 30, so 29, but one line deleted above, so 28 - seeemples
    assert modified_lines[28] == "This is an added line."


def test_remove_quoted(example_fortran_lines):
    for line in example_fortran_lines.copy():
        uncommented_line = remove_quoted(line)
        assert '"' not in uncommented_line
        assert "'" not in uncommented_line


def test_remove_comments(example_fortran_lines):
    for line in example_fortran_lines.copy():
        line = remove_quoted(line)
        uncommented_line = remove_comments(line)
        comment_location = line.find("!")
        if comment_location != -1:
            assert uncommented_line == line[:comment_location].rstrip()
        assert "!" not in uncommented_line


def test_concatenate_lines(example_fortran_lines):
    for line_no, line in enumerate(example_fortran_lines.copy(), 1):
        if line.find("&") >= 0:
            concatenated_line = concatenate_lines(example_fortran_lines.copy(), line_no)
            assert "&" not in concatenated_line
            # This bit has to be a bit hard-wired and is going to break every time the example Fortran file line numbers are changed. If you can think of a better method - go ahead.
            if line_no == 31:
                expected_line = (
                    "SUBROUTINE example (xlen,ylen,l_unscale,input1,"
                    + "input2,   output, l_loud_opt)"
                )
                assert concatenated_line == expected_line
            elif line_no == 70:
                # This will look odd as the quoted text has been removed.
                expected_line = (
                    "my_char                                                          "
                    + "              =                           // "
                )
                assert concatenated_line == expected_line
            elif line_no == 96:
                expected_line = (
                    "            field2(i, j) = (1.0*i) - (2.0*j)    "
                    + "                               + (3.0*i*j) + "
                    + "(4.0*i**2) + field(i, j)*2"
                )
                assert concatenated_line == expected_line


# =================================================================

"""These examples hopefully demonstrate some of the complexity available...
    Setting up a test, may involve multiple changes to the demo Fortran file, hence the complex entries in the parametrization.
    Each error found is recorded with the line number(s) it was found on using the Error text as a dict key, and the line no(s) as a list, which means finding 'use' and 'Use' in the code would generate 2 different keys in the dict.
    Then a single value recording the total number of failures is also included in the 'TestResult' object.
    So for the dictionary of errors returned, we have to check they match, which at present involves checking it's 'len' but also that all the keys in one are in the other, i.e. youre not accidentally getting a matching count but different errors.
    Then for each error(key) in the dict, you need to check how many lines it occured on, and that the line numbers match, i.e. the list of lines numbers is a match with the expected list of line numbers.

    There has to be a better way.....
    As a side note, might it be better to write a function to compare 2 'TestResult' objects, which would be more robust to changes in the structure of the 'TestResult' object, and also make the test code more readable? If so, would it's place be here in the testing, or as a method in the 'TestResult' class itself?
    """

# =================================================================


@pytest.mark.parametrize(
    "changes_list, expected_passed, expected_failure_count, expected_errors",
    [
        # Valid: MODULE example_mod ... END MODULE example_mod (no changes)
        ([], True, 0, {}),
        # Invalid: No program unit found (delete MODULE line)
        (
            [["replace", 10, ["! No module declaration here"]]],
            False,
            1,
            {
                "First executable line doesn't define an accepted programming unit :"
                " IMPLICIT NONE": [0]
            },
        ),
        # Invalid: Mismatched END statement
        (
            [["replace", 118, ["END MODULE wrong_mod_name"]]],
            False,
            1,
            {
                "END statement found for a different program unit: should be example_mod got wrong_mod_name.": [
                    0
                ]
            },
        ),
        # Invalid: No END statement found
        (
            [["replace", 118, ["! Missing END MODULE"]]],
            False,
            1,
            {
                "Last executable line not a matching END statement for the first program unit found.": [
                    0
                ]
            },
        ),
    ],
    ids=[
        "Valid module with matching END",
        "No program unit found",
        "Mismatched END statement",
        "No END statement",
    ],
)
def test_r3_1_1_there_can_be_only_one(
    example_fortran_lines,
    changes_list,
    expected_passed,
    expected_failure_count,
    expected_errors,
):
    modified_fortran_lines = modify_fortran_lines(example_fortran_lines, changes_list)
    result = r3_1_1_there_can_be_only_one(modified_fortran_lines)
    assert result.passed == expected_passed
    assert result.failure_count == expected_failure_count
    errors = result.errors
    assert len(errors) == len(expected_errors)
    for error, lines_list in errors.items():
        assert error in expected_errors
        assert len(lines_list) == len(expected_errors[error])
        for line_no in lines_list:
            assert line_no in expected_errors[error]


# =================================================================


@pytest.mark.parametrize(
    "changes_list, expected_result, expected_errors",
    [
        (
            [
                ["delete", 1, None],
                ["delete", 2, None],
                ["delete", 3, None],
                ["delete", 4, None],
                ["delete", 5, None],
            ],
            1,
            {"missing copyright or crown copyright statement": [0]},
        ),
        ([], 0, []),  # No changes, expect no errors
    ],
    ids=["Missing copyright statement", "copyright statement present"],
)
def test_r3_2_1_check_crown_copyright(
    example_fortran_lines, changes_list, expected_result, expected_errors
):
    # checker = UMDP3Checker()
    modified_fortran_lines = modify_fortran_lines(example_fortran_lines, changes_list)
    result = r3_2_1_check_crown_copyright(modified_fortran_lines)
    failure_count = result.failure_count
    assert failure_count == expected_result
    errors = result.errors
    assert len(errors) == len(expected_errors)
    for error, lines_list in errors.items():
        assert error in expected_errors
        assert len(lines_list) == len(expected_errors[error])
        for line_no in lines_list:
            assert line_no in expected_errors[error]


# =================================================================

@pytest.mark.parametrize(
    "changes_list, expected_result, expected_errors",
    [
        (
            [
                [
                    "replace",
                    71,
                    [
                        '    = "This is a very very very very very very very "'
                        + "                                      &"
                    ],
                ],
                [
                    "replace",
                    115,
                    [
                        "IF (lhook) CALL dr_hook(ModuleName//     "
                        + '":"  //  RoutineName,  zhook_out,  zhook_handle) ! extra comment'
                    ],
                ],
                ["replace", 40, ["use yomhook, ONLY: lhook, dr_hook"]],
            ],
            2,
            {"line too long": [71, 115]},
        ),
        ([], 0, {}),  # No changes, expect no errors
    ],
    ids=["3 line too long Errors", "No line too long Errors"],
)
def test_r3_3_2line_too_long(
    example_fortran_lines, changes_list, expected_result, expected_errors
):
    # checker = UMDP3Checker()
    modified_fortran_lines = modify_fortran_lines(example_fortran_lines, changes_list)
    result = r3_3_2line_too_long(modified_fortran_lines)
    failure_count = result.failure_count
    assert failure_count == expected_result
    errors = result.errors
    assert len(errors) == len(expected_errors)
    for error, lines_list in errors.items():
        assert error in expected_errors
        assert len(lines_list) == len(expected_errors[error])
        for line_no in lines_list:
            assert line_no in expected_errors[error]


# =================================================================


@pytest.mark.parametrize(
    "changes_list, expected_result, expected_errors",
    [
        (
            [
                ["replace", 10, ["Module example_mod"]],
                ["replace", 37, ["use parkind1, ONLY: jpim, jprb"]],
                ["replace", 40, ["use yomhook, ONLY: lhook, dr_hook"]],
            ],
            3,
            {"lowercase keyword: Module": [10], "lowercase keyword: use": [37, 40]},
        ),
        ([], 0, []),  # No changes, expect no errors
    ],
    ids=["3 Lowercase Errors", "No Lowercase Errors"],
)
def test_r3_4_1_capitalised_keywords(
    example_fortran_lines, changes_list, expected_result, expected_errors
):
    modified_fortran_lines = modify_fortran_lines(example_fortran_lines, changes_list)
    result = r3_4_1_capitalised_keywords(modified_fortran_lines)
    failure_count = result.failure_count
    assert failure_count == expected_result
    errors = result.errors
    assert len(errors) == len(expected_errors)
    for error, lines_list in errors.items():
        assert error in expected_errors
        assert len(lines_list) == len(expected_errors[error])
        for line_no in lines_list:
            assert line_no in expected_errors[error]


# =================================================================

# See if you can spot why I hate ruff - it highlights my awful data structures really well.
@pytest.mark.parametrize(
    "changes_list, expected_result, expected_errors",
    [
        (
            [
                [
                    "replace",
                    43,
                    ["INTEGER, INTENT(IN) :: XLEN !Length of first dim of the arrays."],
                ],
                ["replace", 58, ["REAL :: var1, DAVE_2, HiPPo"]],
            ],
            2,
            {
                "Found UPPERCASE variable name in declaration at line 43: XLEN": [43],
                "Found UPPERCASE variable name in declaration at line 58: DAVE_2": [58],
            },
        ),
        (
            [
                [
                    "add",
                    58,
                    [
                        "REAL      :: VARIaBLE_1, variable_2,          &",
                        "     VARIABLE_3, Hot_Potato, Baked Potato     &",
                        "     nice_var, good_var, camelCase,   & !comment test",
                        "     CAPS_VAR, CASPVAR, the_ghost",
                    ],
                ],
                [
                    "replace",
                    54,
                    [
                        "INTEGER :: j ! Loop counter   &",
                        "INTEGER :: k ! Loop counter   &",
                        "INTEGER :: IJ ! Loop counter",
                    ],
                ],
                [
                    "replace",
                    43,
                    [
                        "INTEGER, INTENT(IN) :: XLEN !Length of first dimension of the"
                        + " arrays."
                    ],
                ],
            ],
            5,
            {
                "Found UPPERCASE variable name in declaration at line 43: XLEN": [
                    43
                ],
                "Found UPPERCASE variable name in declaration at line 56: IJ": [56],
                "Found UPPERCASE variable name in declaration at line 60: CASPVAR": [
                    60
                ],
                "Found UPPERCASE variable name in declaration at line 60: VARIABLE_3": [
                    60
                ],
                "Found UPPERCASE variable name in declaration at line 60: CAPS_VAR": [
                    60
                ],
            },
        ),
        ([], 0, []),  # No changes, expect no errors
    ],
    ids=["2 UpperCase Var Errors", "5 UpperCase Var Errors on extended lines",
        "No UpperCase Var Errors"],
)
def test_r3_4_2_no_full_uppercase_variable_names(
    example_fortran_lines, changes_list, expected_result, expected_errors
):
    modified_fortran_lines = modify_fortran_lines(example_fortran_lines, changes_list)
    result = r3_4_2_no_full_uppercase_variable_names(modified_fortran_lines)
    failure_count = result.failure_count
    assert failure_count == expected_result
    errors = result.errors
    assert len(errors) == len(expected_errors)
    for error, lines_list in errors.items():
        assert error in expected_errors
        assert len(lines_list) == len(expected_errors[error])
        for line_no in lines_list:
            assert line_no in expected_errors[error]
