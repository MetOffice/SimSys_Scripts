# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

"""
Standalone version of the dispatch tables from UMDP3Job
Python translation of the original Perl module
"""

from typing import Dict, Callable
from umdp3_checker_rules import UMDP3Checker

"""
ToDo : This list was checked to ensure it had something for each
       test in the original.
"""

# Declare version
VERSION = "13.5.0"


class CheckerDispatchTables:
    """Class containing dispatch tables for UMDP3 tests"""

    def __init__(self):
        self.umdp3_checker = UMDP3Checker()

    def get_diff_dispatch_table_fortran(self) -> Dict[str, Callable]:
        """Get dispatch table for Fortran diff tests"""
        return {
            # 'Captain Daves doomed test of destruction': self.umdp3_checker.capitulated_keywords,
            "Lowercase Fortran keywords not permitted": self.umdp3_checker.capitalised_keywords,
            "OpenMP sentinels not in column one": self.umdp3_checker.openmp_sentinels_in_column_one,
            "Omitted optional space in keywords": self.umdp3_checker.unseparated_keywords,
            "GO TO other than 9999": self.umdp3_checker.go_to_other_than_9999,
            "WRITE without format": self.umdp3_checker.write_using_default_format,
            "Lowercase or CamelCase variable names only": self.umdp3_checker.lowercase_variable_names,
            "Use of dimension attribute": self.umdp3_checker.dimension_forbidden,
            "Continuation lines shouldn't start with &": self.umdp3_checker.ampersand_continuation,
            "Use of EQUIVALENCE or PAUSE": self.umdp3_checker.forbidden_keywords,
            "Use of older form of relational operator (.GT. etc.)": self.umdp3_checker.forbidden_operators,
            "Line longer than 80 characters": self.umdp3_checker.line_over_80chars,
            "Line includes tab character": self.umdp3_checker.tab_detection,
            "USEd printstatus_mod instead of umPrintMgr": self.umdp3_checker.printstatus_mod,
            "Used PRINT rather than umMessage and umPrint": self.umdp3_checker.printstar,
            "Used WRITE(6) rather than umMessage and umPrint": self.umdp3_checker.write6,
            "Used um_fort_flush rather than umPrintFlush": self.umdp3_checker.um_fort_flush,
            "Used Subversion keyword substitution which is prohibited": self.umdp3_checker.svn_keyword_subst,
            "Used !OMP instead of !$OMP": self.umdp3_checker.omp_missing_dollar,
            "Used #ifdef or #ifndef rather than #if defined() or #if !defined()": self.umdp3_checker.cpp_ifdef,
            "Presence of fortran comment in CPP directive": self.umdp3_checker.cpp_comment,
            "Used an archaic fortran intrinsic function": self.umdp3_checker.obsolescent_fortran_intrinsic,
            "EXIT statements should be labelled": self.umdp3_checker.exit_stmt_label,
            "Intrinsic modules must be USEd with an INTRINSIC keyword specifier": self.umdp3_checker.intrinsic_modules,
            "READ statements should have an explicit UNIT= as their first argument": self.umdp3_checker.read_unit_args,
        }

    def get_file_dispatch_table_fortran(
        self, filename: str = ""
    ) -> Dict[str, Callable]:
        """Get dispatch table for Fortran file tests"""
        return {
            "Warning - used an if-def due for retirement": self.umdp3_checker.retire_if_def,
            "File is missing at least one IMPLICIT NONE": self.umdp3_checker.implicit_none,
            "Never use STOP or CALL abort": self.umdp3_checker.forbidden_stop,
            "Use of Fortran function as a variable name": self.umdp3_checker.intrinsic_as_variable,
            "File missing crown copyright statement or agreement reference": self.umdp3_checker.check_crown_copyright,
            "File missing correct code owner comment": self.umdp3_checker.check_code_owner,
            "Used (/ 1,2,3 /) form of array initialisation, rather than [1,2,3] form": self.umdp3_checker.array_init_form,
        }

    def get_diff_dispatch_table_c(self) -> Dict[str, Callable]:
        """Get dispatch table for C diff tests"""
        return {
            "Line longer than 80 characters": self.umdp3_checker.line_over_80chars,
            "Line includes tab character": self.umdp3_checker.tab_detection,
            'Fixed-width Integer format specifiers must have a space between themselves and the string delimiter (the " character)': self.umdp3_checker.c_integral_format_specifiers,
        }

    def get_file_dispatch_table_c(self) -> Dict[str, Callable]:
        """Get dispatch table for C file tests"""
        return {
            "Warning - used an if-def due for retirement": self.umdp3_checker.retire_if_def,
            "Used a deprecated C identifier": self.umdp3_checker.c_deprecated,
            "File missing crown copyright statement or agreement reference": self.umdp3_checker.check_crown_copyright,
            "File missing correct code owner comment": self.umdp3_checker.check_code_owner,
            "Used an _OPENMP if-def without also testing against SHUM_USE_C_OPENMP_VIA_THREAD_UTILS. (Or _OPENMP does not come first in the test.)": self.umdp3_checker.c_openmp_define_pair_thread_utils,
            "Used an _OPENMP && SHUM_USE_C_OPENMP_VIA_THREAD_UTILS if-def test in a logical combination with a third macro": self.umdp3_checker.c_openmp_define_no_combine,
            "Used !defined(_OPENMP) rather than defined(_OPENMP) with #else branch": self.umdp3_checker.c_openmp_define_not,
            "Used an omp #pragma (or #include <omp.h>) without protecting it with an _OPENMP if-def": self.umdp3_checker.c_protect_omp_pragma,
            "Used the #ifdef style of if-def, rather than the #if defined() style": self.umdp3_checker.c_ifdef_defines,
            "C Unit does not end with a final newline character": self.umdp3_checker.c_final_newline,
        }

    def get_file_dispatch_table_all(self) -> Dict[str, Callable]:
        """Get dispatch table for universal file tests"""
        return {
            "Line includes trailing whitespace character(s)": self.umdp3_checker.line_trail_whitespace,
        }
