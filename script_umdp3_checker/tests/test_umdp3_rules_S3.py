import pytest
import sys
from pathlib import Path

# Add the current directory to Python path
sys.path.insert(0, str(Path(__file__).parent.parent))
from umdp3_rules_S3 import capitulated_keywords, r3_2_1_check_crown_copyright, r3_4_1_capitalised_keywords
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
    # TODO: Currently <new lines> is a string, but should become a list of strings, allowing multiple lines to be used in addition and as replacements.
    lines = lines_in.copy()
    for change in sorted(changes, key=lambda o: o[1], reverse=True):
        idx = int(change[1]) - 1
        if change[0] == "replace":
            lines[idx] = change[2]
        elif change[0] == "delete":
            del lines[idx]
        elif change[0] == "add":
            lines.insert(idx, change[2])
        # if change[0] == "replace":
        #     lines[idx:idx+1] = change[2]
        # elif change[0] == "delete":
        #     del lines[idx]
        # elif change[0] == "add":
        #     lines[idx:idx], change[2])
        else:
            raise ValueError(f"Unknown operation: {change[0]}")
        # for count, line in enumerate(lines, 1):
        #      print(f"line [{count}]: {line}")
    return lines

# =================================================================

"""This example hopefully demonstrates some of the complexity available...
    Setting up a test, may involve multiple changes to the demo Fortran file, hence the complex entries in the parametrization.
    Each error found is recorded with the line number(s) it was found on using the Error text as a dict key, and the line no(s) as a list, which means finding 'use' and 'Use' in the code would generate 2 different keys in the dict.
    Then a single value recording the total number of failures is also included in the 'TestResult' object.
    So for the dictionary of errors returned, we have to check they match, which at present involves checking it's 'len' but also that all the keys in one are in the other, i.e. youre not accidentally getting a matching count but different errors.
    Then for each error(key) in the dict, you need to check how many lines it occured on, and that the line numbers match, i.e. the list of lines numbers is a match with the expected list of line numbers.

    There has to be a better way.....
    As a side note, might it be better to write a function to compare 2 'TestResult' objects, which would be more robust to changes in the structure of the 'TestResult' object, and also make the test code more readable? If so, would it's place be here in the testing, or as a method in the 'TestResult' class itself?
    """
@pytest.mark.parametrize(
    "changes_list, expected_result, expected_errors",
    [
        (
            [
                ["replace", 10, "Module example_mod"],
                ["replace", 37, "use parkind1, ONLY: jpim, jprb"],
                ["replace", 40, "use yomhook, ONLY: lhook, dr_hook"]
            ],
            3,
            {"lowercase keyword: Module": [10], "lowercase keyword: use": [37, 40]},
        ),
        ([["add", 10, ""]], 0, []),  # No changes, expect no errors
    ],
    ids = ["3 Errors", "No lowercase keyword Errors"]
)
def test_r3_4_1_capitalised_keywords(
    example_fortran_lines, changes_list, expected_result, expected_errors
):
    # checker = UMDP3Checker()
    modified_fortran_lines = modify_fortran_lines(example_fortran_lines, changes_list)
    result = r3_4_1_capitalised_keywords(modified_fortran_lines)
    failure_count = result.failure_count
    assert failure_count == expected_result
    errors = result.errors
    assert len(errors) == len(expected_errors)
    for error, lines_list  in errors.items():
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
                ["delete", 5, None]
            ],
            1,
            {"missing copyright or crown copyright statement": [0]},
        ),
        ([["add", 10, ""]], 0, []),  # No changes, expect no errors
    ],
    ids = ["Missing copyright statement", "copyright statemrent present"]
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
    for error, lines_list  in errors.items():
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
                ["replace", 10, "Module example_mod"],
                ["replace", 37, "use parkind1, ONLY: jpim, jprb"],
                ["replace", 40, "use yomhook, ONLY: lhook, dr_hook"]
            ],
            3,
            {"capitulated keyword: Module": [10], "capitulated keyword: use": [37, 40]},
        ),
        ([["add", 10, ""]], 0, []),  # No changes, expect no errors
    ],
)
def test_keywords_II(
    example_fortran_lines, changes_list, expected_result, expected_errors
):
#     checker = UMDP3Checker()
    modified_fortran_lines = modify_fortran_lines(example_fortran_lines, changes_list)
    result = capitulated_keywords(modified_fortran_lines)
    failure_count = result.failure_count
    assert failure_count == expected_result
    errors = result.errors
    assert len(errors) == len(expected_errors)
    for error, lines_list  in errors.items():
        assert error in expected_errors
        assert len(lines_list) == len(expected_errors[error])
        for line_no in lines_list:
            assert line_no in expected_errors[error]

