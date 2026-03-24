import pytest

from umdp3_rules_S3 import rule_S3_1
from umdp3_checker_rules import TestResult


def test_example_fortran_lines_fixture(example_fortran_lines):
	assert example_fortran_lines


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
