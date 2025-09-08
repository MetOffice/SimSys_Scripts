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

write_using_default_format_parameters = [
    (["      WRITE(*,*) 'Hello, World!'"], 1, "WRITE using default format"),
    (["      WRITE(6,*) 'Hello, World!'"], 0, "WRITE using correct format"),
    (["      PRINT *, 'Hello, World!'"], 0, "PRINT statement"),
    (["      i = i + 1"], 0, "No WRITE statements"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in write_using_default_format_parameters],
                         ids=[data[2] for data in write_using_default_format_parameters])
def test_write_using_default_format(lines, expected_result):
    checker = UMDP3()
    result = checker.write_using_default_format(lines)
    assert result == expected_result

test_lowercase_variable_names_parameters = [
    (["INTEGER  :: lowercase_variable"], 0, "Lowercase variable name"),
    (["REAL :: Lowercase_Variable"], 0, "Pascal case variable name"),
    (["CHARACTER  :: LOWERCASE_VARIABLE"], 1, "Uppercase variable name"),
    (["  REAL  :: lowercase_variable"], 0, "Lowercase variable name with leading whitespace"),
    (["  CHARACTER  :: Lowercase_Variable"], 0, "Pascal case variable name with leading whitespace"),
    (["  INTEGER  :: LOWERCASE_VARIABLE"], 1, "Uppercase variable name with leading whitespace"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_lowercase_variable_names_parameters],
                         ids=[data[2] for data in test_lowercase_variable_names_parameters])
def test_lowercase_variable_names(lines, expected_result):
    checker = UMDP3()
    result = checker.lowercase_variable_names(lines)
    assert result == expected_result

test_dimension_forbidden_parameters = [
    (["REAL :: array(ARR_LEN)"], 0, "Dimension specified in variable declaration"),
    (["REAL :: array"], 0, "No dimension specified in variable declaration"),
    (["DIMENSION  matrix(5,5)"], 1, "Dimension specified for declared variable"),
    (["INTEGER, DIMENSION(10) :: array"], 1, "Dimension specified in variable declaration with attributes"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_dimension_forbidden_parameters],
                         ids=[data[2] for data in test_dimension_forbidden_parameters])
def test_dimension_forbidden(lines, expected_result):
    checker = UMDP3()
    result = checker.dimension_forbidden(lines)
    assert result == expected_result

test_ampersand_continuation_parameters = [
    (["  PRINT *, 'This is a long line &", "  & that continues here'"], 1, "Ampersand continuation on both lines"),
    (["  PRINT *, 'This is a long line &", "    that continues here'"], 0, "Correct ampersand continuation"),
    (["  PRINT *, 'This is a long line", "& that continues here'"], 1, "Incorrect ampersand continuation"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_ampersand_continuation_parameters],
                         ids=[data[2] for data in test_ampersand_continuation_parameters])
def test_ampersand_continuation(lines, expected_result):
    checker = UMDP3()
    result = checker.ampersand_continuation(lines)
    assert result == expected_result

test_forbidden_keywords_parameters = [
    (["COMMON /BLOCK/ var1, var2"], 0, "Use of COMMON block"),
    (["EQUIVALENCE (var1, var2)"], 1, "Use of EQUIVALENCE"),
    (["PAUSE 1"], 1, "Use of PAUSE statement"),
    (["REAL :: var1"], 0, "No forbidden keywords"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_forbidden_keywords_parameters],
                         ids=[data[2] for data in test_forbidden_keywords_parameters])
def test_forbidden_keywords(lines, expected_result):
    checker = UMDP3()
    result = checker.forbidden_keywords(lines)
    assert result == expected_result

test_forbidden_operators_parameters = [
    (["IF (x .GT. y) THEN"], 1, "Use of .GT. operator"),
    (["IF (x > y) THEN"], 0, "Use of > operator"),
    (["IF (x .GE. y) THEN"], 1, "Use of .GE. operator"),
    (["IF (x .EQ. y) THEN"], 1, "Use of .EQ. operator"),
    (["IF (x .NE. y) THEN"], 1, "Use of .NE. operator"),
    (["IF (x >= y) THEN"], 0, "Use of >= operator"),
    (["IF (x == y) THEN"], 0, "Use of == operator"),
    (["IF (x >= y) .AND. (y <= z) THEN"], 0, "Use of >= operator"),
    (["IF (x == y) .OR. (y .LE. z) THEN"], 1, "Use of .LE. operator"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_forbidden_operators_parameters],
                         ids=[data[2] for data in test_forbidden_operators_parameters])
def test_forbidden_operators(lines, expected_result):
    checker = UMDP3()
    result = checker.forbidden_operators(lines)
    assert result == expected_result

test_line_over_80chars_parameters = [
    (["  PRINT *, 'This line is definitely way over the eighty character limit set by the UM coding standards'"], 1, "Line over 80 characters"),
    (["  PRINT *, 'This line is within the limit'"], 0, "Line within 80 characters"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_line_over_80chars_parameters],
                         ids=[data[2] for data in test_line_over_80chars_parameters])
def test_line_over_80chars(lines, expected_result):
    checker = UMDP3()
    result = checker.line_over_80chars(lines)
    assert result == expected_result

test_tab_detection_parameters = [
    (["  PRINT *, 'This line has no tabs'"], 0, "No tabs"),
    (["  PRINT *, 'This line has a tab\tcharacter'"], 1, "Line with tab character"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_tab_detection_parameters],
                         ids=[data[2] for data in test_tab_detection_parameters])
def test_tab_detection(lines, expected_result):
    checker = UMDP3()
    result = checker.tab_detection(lines)
    assert result == expected_result

test_printstatus_mod_parameters = [
    (["  USE PrintStatus_mod"], 1, "Use of PRINTSTATUS_Mod"),
    (["  USE umPrintMgr_mod"], 0, "Use of umPrintMgr_Mod"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_printstatus_mod_parameters],
                         ids=[data[2] for data in test_printstatus_mod_parameters])
def test_printstatus_mod(lines, expected_result):
    checker = UMDP3()
    result = checker.printstatus_mod(lines)
    assert result == expected_result

test_printstar_parameters = [
    (["  PRINT *, 'Hello, World!'"], 1, "Use of PRINT *"),
    (["  PRINT '(A)', 'Hello, World!'"], 0, "Use of PRINT with format"),
    (["  umMessage = 'Hello, World!'"], 0, "Use of umMessage"),
    (["  umPrint(umMessage)"], 0, "Use of umPrint"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_printstar_parameters],
                         ids=[data[2] for data in test_printstar_parameters])
def test_printstar(lines, expected_result):
    checker = UMDP3()
    result = checker.printstar(lines)
    assert result == expected_result

test_write6_parameters = [
    (["  WRITE(6,*) 'Hello, World!'"], 1, "Use of WRITE(6,*)"),
    (["  umPrint(umMessage)"], 0, "Use of umPrint"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_write6_parameters],
                         ids=[data[2] for data in test_write6_parameters])
def test_write6(lines, expected_result):
    checker = UMDP3()
    result = checker.write6(lines)
    assert result == expected_result

test_um_fort_flush_parameters = [
    (["  CALL um_fort_flush()"], 1, "Use of um_fort_flush"),
    (["  CALL umPrintFlush()"], 0, "No use of um_fort_flush"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_um_fort_flush_parameters],
                         ids=[data[2] for data in test_um_fort_flush_parameters])
def test_um_fort_flush(lines, expected_result):
    checker = UMDP3()
    result = checker.um_fort_flush(lines)
    assert result == expected_result

test_svn_keyword_subst_parameters = [
    (["  ! $Id$"], 1, "Use of SVN keyword substitution"),
    (["  ! This is a comment"], 0, "No SVN keyword substitution"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_svn_keyword_subst_parameters],
                         ids=[data[2] for data in test_svn_keyword_subst_parameters])
def test_svn_keyword_subst(lines, expected_result):
    checker = UMDP3()
    result = checker.svn_keyword_subst(lines)
    assert result == expected_result

test_omp_missing_dollar_parameters = [
    (["!$OMP PARALLEL"], 0, "Correct OpenMP sentinel"),
    (["!OMP PARALLEL"], 1, "Missing $ in OpenMP sentinel"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_omp_missing_dollar_parameters],
                         ids=[data[2] for data in test_omp_missing_dollar_parameters])
def test_omp_missing_dollar(lines, expected_result):
    checker = UMDP3()
    result = checker.omp_missing_dollar(lines)
    assert result == expected_result

test_cpp_ifdef_parameters = [
    (["#ifndef DEBUG"], 1, "Incorrect #ifndef"),
    (["#if defined(DEBUG)"], 0, "Correct #if defined"),
    (["#if !defined(DEBUG)"], 0, "Correct #if !defined"),
    (["#ifdef DEBUG"], 1, "Incorrect #ifdef"),
]
@pytest.mark.parametrize("lines, expected_result", [data[:2] for data in test_cpp_ifdef_parameters],
                         ids=[data[2] for data in test_cpp_ifdef_parameters])
def test_cpp_ifdef(lines, expected_result):
    checker = UMDP3()
    result = checker.cpp_ifdef(lines)
    assert result == expected_result


"""ToDo:
# Other tests to consider adding:
    # def test_cpp_ifdef():
    # def test_cpp_comment():
    # def test_obsolescent_fortran_intrinsic():
    # def test_exit_stmt_label():
    # def test_intrinsic_modules():
    # def test_read_unit_args():
    # def test_retire_if_def():
    # def test_implicit_none():
    # def test_forbidden_stop():
    # def test_intrinsic_as_variable():
    # def test_check_crown_copyright():
    # def test_check_code_owner():
    # def test_array_init_form():
    # def test_line_trail_whitespace():
    """