import pytest
import sys
from pathlib import Path

# from umdp3_rules_S3 import rule_S3_1
# Add the current directory to Python path
sys.path.insert(0, str(Path(__file__).parent.parent))
from umdp3_checker_rules import TestResult, UMDP3Checker

def modified_fortran_lines_fnc(lines: list[str], changes: list[dict]) -> list[str]:
    """Return a copy of example_fortran_lines with changes applied.

    ``request.param`` is a list of operation dicts, each with:
      - ``{"operation": "replace", "line": N, "text": "..."}`` : replace line N
      - ``{"operation": "delete",  "line": N}``                 : remove line N
      - ``{"operation": "add",     "line": N, "text": "..."}``  : insert before line N

    Operations are applied in descending line order so that earlier
    line numbers are not shifted by later mutations.
    """
    # lines = example_fortran_lines.copy()
    for change in sorted(changes, key=lambda o: o["line"], reverse=True):
        idx = change["line"] - 1
        if change["operation"] == "replace":
            lines[idx] = change["text"]
        elif change["operation"] == "delete":
            del lines[idx]
        elif change["operation"] == "add":
            lines.insert(idx, change["text"])
        # if change["operation"] == "replace":
        #     lines[idx:idx+1] = change["text"]
        # elif change["operation"] == "delete":
        #     del lines[idx]
        # elif change["operation"] == "add":
        #     lines[idx:idx], change["text"])
        else:
            raise ValueError(f"Unknown operation: {change['operation']}")
        for count, line in enumerate(lines):
             print(f"line [{count}]: {line}")
    return lines



# =================================================================

def test_example_fortran_lines_fixture(example_fortran_lines):
	assert example_fortran_lines


# =================================================================

@pytest.mark.parametrize(
    "modified_fortran_lines, expected_result, expected_errors",
    [([{"operation": "add", "line": 10, "text": ""}], 0, []),  # No changes, expect no errors
     ([{"operation": "replace", "line": 10, "text": "Module example_mod\n"}], 1, {'lowercase keyword: Module': [10]})
    ],
    indirect=["modified_fortran_lines"],
)
def test_keywords(modified_fortran_lines, expected_result, expected_errors):
    checker = UMDP3Checker()
    for line in modified_fortran_lines:
         assert isinstance(line, str)  # Ensure all lines are strings for debugging
    result = checker.capitalised_keywords(modified_fortran_lines)
    assert result.failure_count == expected_result
    for error in expected_errors:
        assert error in result.errors


# =================================================================

@pytest.mark.parametrize(
    "changes_list, expected_result, expected_errors",
    [([{"operation": "add", "line": 10, "text": ""}], 0, []),  # No changes, expect no errors
     ([{"operation": "replace", "line": 10, "text": "Module example_mod\n"}], 1, {'lowercase keyword: Module': [10]})
    ],
)
def test_keywords_II(example_fortran_lines, changes_list, expected_result, expected_errors):
    checker = UMDP3Checker()
    modified_fortran_lines = modified_fortran_lines_fnc(example_fortran_lines, changes_list)
    result = checker.capitalised_keywords(modified_fortran_lines)
    assert result.failure_count == expected_result
    for error in expected_errors:
        assert error in result.errors



# =================================================================

@pytest.mark.parametrize(
    "modified_fortran_lines, expected_fragment",
    [
        # replace: swap the module name
        ([{"operation": "replace", "line": 10, "text": "MODULE bad_mod\n"}], "bad_mod"),
        # delete: remove the IMPLICIT NONE line
        ([{"operation": "delete", "line": 11}], "MODULE example_mod"),
        # add: insert a comment before the module line
        ([{"operation": "add", "line": 10, "text": "! inserted comment\n"}], "! inserted comment"),
        # combined: replace module name and add a comment above it
        (
            [
                {"operation": "replace", "line": 10, "text": "MODULE renamed_mod\n"},
                {"operation": "add", "line": 10, "text": "! renamed module\n"},
            ],
            "renamed_mod",
        ),
    ],
    indirect=["modified_fortran_lines"],
)
def test_example_fortran_lines_parametrized(
    modified_fortran_lines, expected_fragment
):
    assert any(expected_fragment in line for line in modified_fortran_lines)
