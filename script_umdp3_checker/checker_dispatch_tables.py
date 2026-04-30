# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
Standalone version of the dispatch tables from UMDP3Job
Python translation of the original Perl module
"""

from typing import Callable
from umdp3_checker_rules import UMDP3Checker

"""
TODO : This module has lost it's way. It uses a class to define methods which just
       return lists of functions. I don't think a class is required for this.
       As the functions themselves are shifted, renamed and hopefully improved, this
       class should eventually get emptied out and the file removed.

"""


class CheckerDispatchTables:
    """Class containing dispatch tables for UMDP3 tests"""

    def __init__(self):
        self.umdp3_checker = UMDP3Checker()

    def get_diff_dispatch_table_fortran(self) -> list[Callable]:
        """Get dispatch table for Fortran diff tests"""
        return [
            self.umdp3_checker.openmp_sentinels_in_column_one,
            self.umdp3_checker.unseparated_keywords,
            self.umdp3_checker.go_to_other_than_9999,
            self.umdp3_checker.write_using_default_format,
            self.umdp3_checker.dimension_forbidden,
            self.umdp3_checker.ampersand_continuation,
            self.umdp3_checker.forbidden_keywords,
            self.umdp3_checker.forbidden_operators,
            self.umdp3_checker.tab_detection,
            self.umdp3_checker.printstatus_mod,
            self.umdp3_checker.printstar,
            self.umdp3_checker.write6,
            self.umdp3_checker.um_fort_flush,
            self.umdp3_checker.svn_keyword_subst,
            self.umdp3_checker.omp_missing_dollar,
            self.umdp3_checker.cpp_ifdef,
            self.umdp3_checker.cpp_comment,
            self.umdp3_checker.obsolescent_fortran_intrinsic,
            self.umdp3_checker.exit_stmt_label,
            self.umdp3_checker.intrinsic_modules,
            self.umdp3_checker.read_unit_args,
        ]

    def get_file_dispatch_table_fortran(self, filename: str = "") -> list[Callable]:
        """Get dispatch table for Fortran file tests"""
        return [
            self.umdp3_checker.retire_if_def,
            self.umdp3_checker.implicit_none,
            self.umdp3_checker.forbidden_stop,
            self.umdp3_checker.intrinsic_as_variable,
            self.umdp3_checker.check_code_owner,
            self.umdp3_checker.array_init_form,
        ]

    def get_diff_dispatch_table_c(self) -> list[Callable]:
        """Get dispatch table for C diff tests"""
        return [
            self.umdp3_checker.tab_detection,
            self.umdp3_checker.c_integral_format_specifiers,
        ]

    def get_file_dispatch_table_c(self) -> list[Callable]:
        """Get dispatch table for C file tests"""
        return [
            self.umdp3_checker.retire_if_def,
            self.umdp3_checker.c_deprecated,
            self.umdp3_checker.check_code_owner,
            self.umdp3_checker.c_openmp_define_pair_thread_utils,
            self.umdp3_checker.c_openmp_define_no_combine,
            self.umdp3_checker.c_openmp_define_not,
            self.umdp3_checker.c_protect_omp_pragma,
            self.umdp3_checker.c_ifdef_defines,
            self.umdp3_checker.c_final_newline,
        ]

    def get_file_dispatch_table_all(self) -> list[Callable]:
        """Get dispatch table for universal file tests"""
        return [
            self.umdp3_checker.line_trail_whitespace,
        ]
