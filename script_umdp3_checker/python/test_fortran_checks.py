import pytest
from umdp3 import UMDP3

keyword_data = [
    ("IF THEN END", 0, {}, "All UPPERCASE keywords"),
    ("if then end", 3, {"lowercase keyword: if"}, "All lowercase keywords"),
    ("If Then End", 3, {"lowercase keyword: If"}, "All mixed case keywords"),
    ("foo bar baz", 0, {}, "No keywords"),
    ("! if then end", 0, {}, "Commented keywords"),
]
keyword_test_parameters = [data[:3] for data in keyword_data]
keyword_test_ids = [data[3] for data in keyword_data]
@pytest.mark.parametrize("lines, expected_result, expected_errors", keyword_test_parameters,
                          ids=keyword_test_ids)
def test_keywords(lines, expected_result, expected_errors):
    checker = UMDP3()
    result = checker.capitalised_keywords([lines])
    assert result == expected_result
    for error in expected_errors:
        assert error in checker.get_extra_error_information()

fake_code_block = [
        "PROGRAM test",
        "IMPLICIT NONE",
        "INTEGER :: i",
        "END PROGRAM"
    ]
implicit_none_paramters = [
    ([line for line in fake_code_block if line != "IMPLICIT NONE"], 1, "Missing IMPLICIT NONE"),
    (fake_code_block, 0, "With IMPLICIT NONE")
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in implicit_none_paramters],
                          ids=[data[2] for data in implicit_none_paramters])
def test_implicit_none(lines, expected_result):
    checker = UMDP3()
    result = checker.implicit_none(lines)
    assert result == expected_result

openmp_sentinels_parameters = [
    (["!$OMP PARALLEL"], 0, "OpenMP sentinel in column one"),
    (["  !$OMP PARALLEL"], 1, "OpenMP sentinel not in column one"),
    (["!$OMP PARALLEL", "  !$OMP END PARALLEL"], 1, "One sentinel in column one, one not"),
    (["  !$OMP PARALLEL", "  !$OMP END PARALLEL"], 2, "No sentinels in column one"),
    (["! This is a comment", "  !$OMP PARALLEL"], 1, "Comment line and sentinel not in column one"),
    (["!$OMP PARALLEL", "!$OMP END PARALLEL"], 0, "Both sentinels in column one")
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in openmp_sentinels_parameters],
                          ids=[data[2] for data in openmp_sentinels_parameters])
def test_openmp_sentinels_in_column_one(lines, expected_result):
    checker = UMDP3()
    result = checker.openmp_sentinels_in_column_one(lines)
    assert result == expected_result

unseparated_keywords_parameters = [
    (["ELSEIF", "ENDDO", "ENDSUBROUTINE"], 3, "All keywords unseparated"),
    (["ELSE IF", "ENDMODULE", "ENDSUBROUTINE"], 2, "One keyword separated"),
    (["ELSE IF", "END DO", "END IF"], 0, "All keywords separated"),
    (["i=0", "i=i+1", "PRINT*,i"], 0, "No keywords"),
    (["PROGRAM test", "i=0", "ENDIF"], 1, "One keyword unseparated"),
    (["i=0", "ENDIF", "END DO"], 1, "One keyword unseparated in middle")
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in unseparated_keywords_parameters],
                          ids=[data[2] for data in unseparated_keywords_parameters])
def test_unseparated_keywords(lines, expected_result):
    checker = UMDP3()
    result = checker.unseparated_keywords(lines)
    assert result == expected_result

go_to_other_than_9999_parameters = [
    (["      GO TO 1000", "      GO TO 2000"], 2, "All GO TO statements to labels other than 9999"),
    (["      GO TO 9999", "      GO TO 2000"], 1, "One GO TO statement to label other than 9999"),
    (["      GO TO 9999", "      GO TO 9999"], 0, "All GO TO statements to label 9999"),
    (["      PRINT *, 'Hello, World!'", "      i = i + 1"], 0, "No GO TO statements"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in go_to_other_than_9999_parameters],
                          ids=[data[2] for data in go_to_other_than_9999_parameters])
def test_go_to_other_than_9999(lines, expected_result  ):
    checker = UMDP3()
    result = checker.go_to_other_than_9999(lines)
    assert result == expected_result