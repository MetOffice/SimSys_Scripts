# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

import pytest
import sys
from pathlib import Path

# Add the current directory to Python path
sys.path.insert(0, str(Path(__file__).parent.parent))
from umdp3_checker_rules import UMDP3Checker, TestResult


# Prevent pytest from trying to collect TestResult as more tests:
TestResult.__test__ = False  # type: ignore
"""
ToDo :
      THere has been a LOT of refactoring in the umdp3 module since these
      tests were written. To persuade them to 'work' for now, only two
      attributes of the TestResult class are used - failure_count and
      errors.
      Something more rigorous should be done to bring these tests up to
      date. Especially as errors is (I think) only checking for the presence
      of a given string in the keys of the error log dict.
"""

fake_code_block = ["PROGRAM test", "IMPLICIT NONE", "INTEGER :: i", "END PROGRAM"]
implicit_none_paramters = [
    (
        [line for line in fake_code_block if line != "IMPLICIT NONE"],
        1,
        "Missing IMPLICIT NONE",
    ),
    (fake_code_block, 0, "With IMPLICIT NONE"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in implicit_none_paramters],
    ids=[data[2] for data in implicit_none_paramters],
)
def test_implicit_none(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.implicit_none(lines)
    assert result.failure_count == expected_result


openmp_sentinels_parameters = [
    (["!$OMP PARALLEL"], 0, "OpenMP sentinel in column one"),
    (["  !$OMP PARALLEL"], 1, "OpenMP sentinel not in column one"),
    (
        ["!$OMP PARALLEL", "  !$OMP END PARALLEL"],
        1,
        "One sentinel in column one, one not",
    ),
    (["  !$OMP PARALLEL", "  !$OMP END PARALLEL"], 2, "No sentinels in column one"),
    (
        ["! This is a comment", "  !$OMP PARALLEL"],
        1,
        "Comment line and sentinel not in column one",
    ),
    (["!$OMP PARALLEL", "!$OMP END PARALLEL"], 0, "Both sentinels in column one"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in openmp_sentinels_parameters],
    ids=[data[2] for data in openmp_sentinels_parameters],
)
def test_openmp_sentinels_in_column_one(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.openmp_sentinels_in_column_one(lines)
    assert result.failure_count == expected_result


test_dimension_forbidden_parameters = [
    (["REAL :: array(ARR_LEN)"], 0, "Dimension specified in variable declaration"),
    (["REAL :: array"], 0, "No dimension specified in variable declaration"),
    (["DIMENSION  matrix(5,5)"], 1, "Dimension specified for declared variable"),
    (
        ["INTEGER, DIMENSION(10) :: array"],
        1,
        "Dimension specified in variable declaration with attributes",
    ),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_dimension_forbidden_parameters],
    ids=[data[2] for data in test_dimension_forbidden_parameters],
)
def test_dimension_forbidden(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.dimension_forbidden(lines)
    assert result.failure_count == expected_result


test_ampersand_continuation_parameters = [
    (
        ["  PRINT *, 'This is a long line &", "  & that continues here'"],
        1,
        "Ampersand continuation on both lines",
    ),
    (
        ["  PRINT *, 'This is a long line &", "    that continues here'"],
        0,
        "Correct ampersand continuation",
    ),
    (
        ["  PRINT *, 'This is a long line", "& that continues here'"],
        1,
        "Incorrect ampersand continuation",
    ),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_ampersand_continuation_parameters],
    ids=[data[2] for data in test_ampersand_continuation_parameters],
)
def test_ampersand_continuation(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.ampersand_continuation(lines)
    assert result.failure_count == expected_result


test_forbidden_keywords_parameters = [
    (["COMMON /BLOCK/ var1, var2"], 0, "Use of COMMON block"),
    (["EQUIVALENCE (var1, var2)"], 1, "Use of EQUIVALENCE"),
    (["PAUSE 1"], 1, "Use of PAUSE statement"),
    (["REAL :: var1"], 0, "No forbidden keywords"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_forbidden_keywords_parameters],
    ids=[data[2] for data in test_forbidden_keywords_parameters],
)
def test_forbidden_keywords(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.forbidden_keywords(lines)
    assert result.failure_count == expected_result


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


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_forbidden_operators_parameters],
    ids=[data[2] for data in test_forbidden_operators_parameters],
)
def test_forbidden_operators(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.forbidden_operators(lines)
    assert result.failure_count == expected_result


test_printstatus_mod_parameters = [
    (["  USE PrintStatus_mod"], 1, "Use of PRINTSTATUS_Mod"),
    (["  USE umPrintMgr_mod"], 0, "Use of umPrintMgr_Mod"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_printstatus_mod_parameters],
    ids=[data[2] for data in test_printstatus_mod_parameters],
)
def test_printstatus_mod(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.printstatus_mod(lines)
    assert result.failure_count == expected_result


test_um_fort_flush_parameters = [
    (["  CALL um_fort_flush()"], 1, "Use of um_fort_flush"),
    (["  CALL umPrintFlush()"], 0, "No use of um_fort_flush"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_um_fort_flush_parameters],
    ids=[data[2] for data in test_um_fort_flush_parameters],
)
def test_um_fort_flush(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.um_fort_flush(lines)
    assert result.failure_count == expected_result


test_svn_keyword_subst_parameters = [
    (["  ! $Id$"], 1, "Use of SVN keyword substitution"),
    (["  ! This is a comment"], 0, "No SVN keyword substitution"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_svn_keyword_subst_parameters],
    ids=[data[2] for data in test_svn_keyword_subst_parameters],
)
def test_svn_keyword_subst(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.svn_keyword_subst(lines)
    assert result.failure_count == expected_result


test_cpp_ifdef_parameters = [
    (["#ifndef DEBUG"], 1, "Incorrect #ifndef"),
    (["#if defined(DEBUG)"], 0, "Correct #if defined"),
    (["#if !defined(DEBUG)"], 0, "Correct #if !defined"),
    (["#ifdef DEBUG"], 1, "Incorrect #ifdef"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_cpp_ifdef_parameters],
    ids=[data[2] for data in test_cpp_ifdef_parameters],
)
def test_cpp_ifdef(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.cpp_ifdef(lines)
    assert result.failure_count == expected_result


test_intrinsic_modules_parameters = [
    (["  USE ISO_C_BINDING"], 1, "Incorrect Use of ISO_C_BINDING module"),
    (
        ["  USE, INTRINSIC :: ISO_FORTRAN_ENV"],
        0,
        "Correct Use of ISO_FORTRAN_ENV module",
    ),
    (["  USE  :: ISO_FORTRAN_ENV"], 1, "Incorrect Use of ISO_FORTRAN_ENV module"),
    (["  USE, INTRINSIC :: ISO_C_BINDING"], 0, "Correct Use of ISO_C_BINDING module"),
    (
        ["  USE SOME_OTHER_MODULE"],
        0,
        "Use of non-intrinsic module without INTRINSIC keyword",
    ),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_intrinsic_modules_parameters],
    ids=[data[2] for data in test_intrinsic_modules_parameters],
)
def test_intrinsic_modules(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.intrinsic_modules(lines)
    assert result.failure_count == expected_result


test_retire_if_def_parameters = [
    (["#ifdef DEBUG"], 0, "Correct Use of #ifdef"),
    (["#ifndef DEBUG"], 0, "Correct Use of #ifndef"),
    (["#if defined(DEBUG)"], 0, "Correct Use of #if defined"),
    (["#if !defined(DEBUG)"], 0, "Correct Use of #if !defined"),
    (["#elif defined(DEBUG)"], 0, "Correct Use of #elif defined"),
    (["#else"], 0, "Correct Use of #else"),
    (["#ifdef VATPOLES"], 1, "Incorrect Use of VATPOLES"),
    (["#ifndef A12_3A"], 1, "Incorrect Use of A12_3A"),
    (["#if defined(A12_4A)"], 1, "Incorrect Use of A12_4A"),
    (["#if !defined(UM_JULES)"], 1, "Incorrect Use of UM_JULES"),
    (["#elif defined(VATPOLES)"], 1, "Incorrect Use of VATPOLES"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_retire_if_def_parameters],
    ids=[data[2] for data in test_retire_if_def_parameters],
)
def test_retire_if_def(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.retire_if_def(lines)
    assert result.failure_count == expected_result


test_forbidden_stop_parameters = [
    (["  STOP 0"], 1, "Use of STOP statement"),
    (["STOP"], 1, "Use of STOP statement without code"),
    (["  PRINT *, 'Hello, World!'"], 0, "No STOP statement"),
    (["CALL ABORT"], 1, "Use of call abort statement"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_forbidden_stop_parameters],
    ids=[data[2] for data in test_forbidden_stop_parameters],
)
def test_forbidden_stop(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.forbidden_stop(lines)
    assert result.failure_count == expected_result


test_intrinsic_as_variable_parameters = [
    (["  INTEGER :: SIN"], 1, "Use of intrinsic name as variable"),
    (["  REAL :: COS"], 1, "Use of intrinsic name as variable"),
    (["  REAL :: MYVAR"], 0, "No use of intrinsic name as variable"),
    (["  INTEGER :: TAN, MYVAR"], 1, "One intrinsic name as variable"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_intrinsic_as_variable_parameters],
    ids=[data[2] for data in test_intrinsic_as_variable_parameters],
)
def test_intrinsic_as_variable(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.intrinsic_as_variable(lines)
    assert result.failure_count == expected_result


test_check_code_owner_parameters = [
    (["! Code Owner: John Doe"], 0, "code owner statement"),
    (["! Code Owner : John Doe"], 0, "Another code owner statement"),
    (["! This is a comment"], 1, "No code owner statement"),
    (["! Code Owner: "], 0, "Code owner statement with no name"),
]


@pytest.mark.parametrize(
    "lines, expected_result",
    [data[:2] for data in test_check_code_owner_parameters],
    ids=[data[2] for data in test_check_code_owner_parameters],
)
def test_check_code_owner(lines, expected_result):
    checker = UMDP3Checker()
    result = checker.check_code_owner(lines)
    assert result.failure_count == expected_result
