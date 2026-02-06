# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

"""
Package to contain functions which test for UMDP3 compliance.
Python translation of the original Perl UMDP3.pm module.
"""

import re
import threading
from typing import List, Dict
from fortran_keywords import fortran_keywords
from search_lists import (
    obsolescent_intrinsics,
    unseparated_keywords_list,
    retired_ifdefs,
    deprecated_c_identifiers,
)
from dataclasses import dataclass, field

"""
TODO : Several of the test functions are poor shadows of the original
       Perl versions. They would benefit from improving to catch more
       cases.
       Equally, there could probably be more consistancly in how things
       like comments are stripped from the ends of lines
       and/or full comment lines are skipped.
"""

# Declare version
VERSION = "13.5.0"


@dataclass
class TestResult:
    """Result from running a single style checker test on a file."""

    """
    TODO : unsure if both output and errors are required.
       They make a bit more sense in the 'external_checkers' where
       they hold stdout and stderr."""
    checker_name: str = "Unnamed Checker"
    failure_count: int = 0
    passed: bool = False
    output: str = ""
    errors: Dict = field(default_factory=dict)


class UMDP3Checker:
    """UMDP3 compliance checker class"""

    """
    TODO : This class could possibly be abandoned, or replaced
       by a similar class at a different level. Presently only one
       instance is created in such a way that the original
       _extra_error_info can't be used to hold extra information
       at a per file level. resulting in the need to pass error_log
       back, which feels like a bodge."""
    # precompiled, regularly used search patterns.
    comment_line = re.compile(r"!.*$")
    word_splitter = re.compile(r"\b\w+\b")

    def __init__(self):
        self._extra_error_info = {}
        self._lock = threading.Lock()
        """
    TODO: The Perl version had a dodgy looking subroutine to calculate
        this, but I can't find where it was called from within the files in
        'bin'. It used all args as a 'list' - searched them for '#include' and
        then returned the count as well as adding 1 to this global var if any
        were found.
        This is either redundant and needs removing, or needs implementing
        properly."""
        self._number_of_files_with_variable_declarations_in_includes = 0

    def reset_extra_error_information(self):
        """Reset extra error information :
        Appears to be used 'between' blocks of tests such as those on diffs and
        those on full files.
        """
        with self._lock:
            self._extra_error_info = {}

    def get_extra_error_information(self) -> Dict:
        """
        Get extra error information. Dictionary with file names as the keys.
        """
        """

    TODO: I presume this is what's used when creating the report of the
        actual failures and not just the count. However, this information
        doesn't seem to be output as yet and will need implementing.
        """
        with self._lock:
            return self._extra_error_info.copy()

    def add_extra_error(self, key: str, value: str = ""):
        """Add extra error information to the dictionary"""
        """
    TODO: The usefulness of the information added has not been assesed,
        nor does it appear to be reported as yet."""
        with self._lock:
            self._extra_error_info[key] = value

    def add_error_log(
        self, error_log: Dict, key: str = "no key", value: int = 0
    ) -> Dict:
        """Add extra error information to the dictionary"""
        """
    TODO: This is a bodge to get more detailed info about
           the errors back to the calling program. The info is
           useful, but is currently presented on a per-test basis
           rather than a per-file which would be easier to read
           and make use of."""
        if key not in error_log:
            error_log[key] = []
        error_log[key].append(value)
        return error_log

    def get_include_number(self) -> int:
        """Get number of files with variable declarations in includes"""
        """
    TODO: At present, this is hardwired to zero and I don't think
        anything alters it along the way. Plus it doesn't seem to be called
        from anywhere..... So this getter is probably very redundant."""
        return self._number_of_files_with_variable_declarations_in_includes

    def remove_quoted(self, line: str) -> str:
        """Remove quoted strings from a line"""
        """
    TODO: The original version replaced the quoted sections with a
        "blessed reference", presumably becuase they were 're-inserted' at some
        stage. No idea if that capability is still required."""
        # Simple implementation - remove single and double quoted strings
        result = line

        # Remove double quoted strings
        result = re.sub(r'"[^"]*"', "", result)

        # Remove single quoted strings
        result = re.sub(r"'[^']*'", "", result)

        return result

    """Test functions :
        Each accepts a list of 'lines' to search and returns a
        TestResult object containing all the information."""
    """
    TODO: One thought here is each test should also be told whether it's
    being passed the contents of a full file, or just a selection of lines
    involved in a change as some of the tests appear to really only be useful
    if run on a full file (e.g. the Implicit none checker). Thus if only passed
    a selection of lines, these tests could be skipped/return 'pass'
    regardless.
    Although, a brief look seems to imply that there are two 'dispatch tables'
    one for full files and one for changed lines."""

    def capitulated_keywords(self, lines: List[str]) -> TestResult:
        """A fake test, put in for testing purposes.
        Probably not needed any more, but left in case."""
        failures = 0
        line_count = 0
        error_log = {}
        # print("Debug: In capitulated_keywords test")
        for line in lines:
            line_count += 1
            # Remove quoted strings and comments
            if line.lstrip(" ").startswith("!"):
                continue
            clean_line = self.remove_quoted(line)
            clean_line = self.comment_line.sub("", clean_line)

            # Check for lowercase keywords
            for word in self.word_splitter.findall(clean_line):
                upcase = word.upper()
                if upcase in fortran_keywords and word != upcase:
                    self.add_extra_error(f"lowercase keyword: {word}")
                    error_log = self.add_error_log(
                        error_log, f"capitulated keyword: {word}", line_count
                    )
                    failures += 1

        return TestResult(
            checker_name="Capitulated Keywords",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {line_count} lines, found {failures} failures.",
            # errors=self.get_extra_error_information()
            errors=error_log,
        )

    def capitalised_keywords(self, lines: List[str]) -> TestResult:
        """Check for the presence of lowercase Fortran keywords, which are
        taken from an imported list 'fortran_keywords'."""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            # Remove quoted strings and comments
            if line.lstrip(" ").startswith("!"):
                continue
            clean_line = self.remove_quoted(line)
            clean_line = self.comment_line.sub("", clean_line)
            # Check for lowercase keywords
            for word in self.word_splitter.findall(clean_line):
                upcase = word.upper()
                if upcase in fortran_keywords and word != upcase:
                    self.add_extra_error(f"lowercase keyword: {word}")
                    failures += 1
                    error_log = self.add_error_log(
                        error_log, f"lowercase keyword: {word}", count + 1
                    )

        return TestResult(
            checker_name="Capitalised Keywords",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def openmp_sentinels_in_column_one(self, lines: List[str]) -> TestResult:
        """Check OpenMP sentinels are in column one"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            if re.search(r"^\s+!\$OMP", line):
                self.add_extra_error("OpenMP sentinel not in column 1")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "OpenMP sentinel not in column 1:", count + 1
                )
        output = f"Checked {count+1} lines, found {failures} failures."
        return TestResult(
            checker_name="OpenMP sentinels not in column one",
            failure_count=failures,
            passed=(failures == 0),
            output=output,
            errors=error_log,
        )

    def unseparated_keywords(self, lines: List[str]) -> TestResult:
        """Check for omitted optional spaces in keywords"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            if line.lstrip(" ").startswith("!"):
                continue
            clean_line = self.remove_quoted(line)
            for pattern in [f"\\b{kw}\\b" for kw in unseparated_keywords_list]:
                if re.search(pattern, clean_line, re.IGNORECASE):
                    self.add_extra_error(f"unseparated keyword in line: "
                                         f"{line.strip()}")
                    failures += 1
                    error_log = self.add_error_log(
                        error_log,
                        f"unseparated keyword in line: {line.strip()}",
                        count + 1,
                    )
        output = f"Checked {count+1} lines, found {failures} failures."
        return TestResult(
            checker_name="Unseparated Keywords",
            failure_count=failures,
            passed=(failures == 0),
            output=output,
            errors=error_log,
        )

    def go_to_other_than_9999(self, lines: List[str]) -> TestResult:
        """Check for GO TO statements other than 9999"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            if match := re.search(r"\bGO\s*TO\s+(\d+)", clean_line,
                                  re.IGNORECASE):
                label = match.group(1)
                if label != "9999":
                    self.add_extra_error(f"GO TO {label}")
                    failures += 1
                    error_log = self.add_error_log(
                        error_log, f"GO TO {label}", count + 1
                    )
        output = f"Checked {count+1} lines, found {failures} failures."
        return TestResult(
            checker_name="GO TO other than 9999",
            failure_count=failures,
            passed=(failures == 0),
            output=output,
            errors=error_log,
        )

    def write_using_default_format(self, lines: List[str]) -> TestResult:
        """Check for WRITE without format"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            if re.search(r"\bWRITE\s*\(\s*\*\s*,\s*\*\s*\)", clean_line,
                         re.IGNORECASE):
                self.add_extra_error("WRITE(*,*) found")
                failures += 1
                error_log = self.add_error_log(error_log, "WRITE(*,*) found",
                                               count + 1)
        output = f"Checked {count+1} lines, found {failures} failures."
        return TestResult(
            checker_name="WRITE using default format",
            failure_count=failures,
            passed=(failures == 0),
            output=output,
            errors=error_log,
        )

    def lowercase_variable_names(self, lines: List[str]) -> TestResult:
        """Check for lowercase or CamelCase variable names only"""
        """
    TODO: This is a very simplistic check and will not detect many
        cases which break UMDP3. I suspect the Perl Predeccessor concattenated
        continuation lines prior to 'cleaning' and checking. Having identified
        a declaration, it also then scanned the rest of the file for that
        variable name in any case."""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            # Simple check for UPPERCASE variable declarations
            if re.search(
                r"^\s*(INTEGER|REAL|LOGICAL|CHARACTER|TYPE)\s*.*::\s*[A-Z_]+",
                clean_line,
                re.IGNORECASE,
            ):
                # print("Debug: Found variable declaration line: "
                # f"{clean_line}")
                clean_line = re.sub(
                    r"^\s*(INTEGER|REAL|LOGICAL|CHARACTER|TYPE)\s*.*::\s*",
                    "",
                    clean_line,
                )
                if re.search(r"[A-Z]{2,}", clean_line):
                    # print(f"Debug: Found UPPERCASE variable name: "
                    # f"{clean_line}")
                    self.add_extra_error("UPPERCASE variable name")
                    failures += 1
                    error_log = self.add_error_log(
                        error_log, "UPPERCASE variable name", count + 1
                    )

        output = f"Checked {count+1} lines, found {failures} failures."
        return TestResult(
            checker_name="Lowercase or CamelCase variable names only",
            failure_count=failures,
            passed=(failures == 0),
            output=output,
            errors=error_log,
        )

    def dimension_forbidden(self, lines: List[str]) -> TestResult:
        """Check for use of dimension attribute"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            if re.search(r"\bDIMENSION\b", clean_line, re.IGNORECASE):
                self.add_extra_error("DIMENSION attribute used")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "DIMENSION attribute used", count + 1
                )

        output = f"Checked {count+1} lines, found {failures} failures."
        return TestResult(
            checker_name="Use of dimension attribute",
            failure_count=failures,
            passed=(failures == 0),
            output=output,
            errors=error_log,
        )

    def ampersand_continuation(self, lines: List[str]) -> TestResult:
        """Check continuation lines shouldn't start with &"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            if re.search(r"^\s*&", line):
                self.add_extra_error("continuation line starts with &")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "continuation line starts with &", count + 1
                )

        return TestResult(
            checker_name="Continuation lines shouldn't start with &",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def forbidden_keywords(self, lines: List[str]) -> TestResult:
        """Check for use of EQUIVALENCE or PAUSE"""
        """
    TODO: Can't believe this will allow a COMMON BLOCK....
        Need to check against what the original did.."""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            if re.search(r"\b(EQUIVALENCE|PAUSE)\b", clean_line,
                         re.IGNORECASE):
                self.add_extra_error("forbidden keyword")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "forbidden keyword", count + 1
                )

        return TestResult(
            checker_name="Use of forbidden keywords EQUIVALENCE or PAUSE",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def forbidden_operators(self, lines: List[str]) -> TestResult:
        """Check for older form of relational operators"""
        failures = 0
        error_log = {}
        count = -1
        old_operators = [".GT.", ".GE.", ".LT.", ".LE.", ".EQ.", ".NE."]

        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            for op in old_operators:
                if op in clean_line.upper():
                    self.add_extra_error(f"old operator {op}")
                    failures += 1
                    error_log = self.add_error_log(
                        error_log, f"old operator {op}", count + 1
                    )

        return TestResult(
            checker_name="Use of older form of relational operator " +
            "(.GT. etc.)",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def line_over_80chars(self, lines: List[str]) -> TestResult:
        """Check for lines longer than 80 characters"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            if len(line.rstrip()) > 80:
                self.add_extra_error("line too long")
                failures += 1
                error_log = self.add_error_log(error_log, "line too long",
                                               count + 1)

        return TestResult(
            checker_name="Line longer than 80 characters",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def tab_detection(self, lines: List[str]) -> TestResult:
        """Check for tab characters"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            if "\t" in line:
                self.add_extra_error("tab character found")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "tab character found", count + 1
                )

        return TestResult(
            checker_name="Line includes tab character",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def printstatus_mod(self, lines: List[str]) -> TestResult:
        """Check for use of printstatus_mod instead of umPrintMgr"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            if re.search(r"\bUSE\s+printstatus_mod\b", line, re.IGNORECASE):
                self.add_extra_error("printstatus_mod used")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "printstatus_mod used", count + 1
                )

        return TestResult(
            checker_name="Use of printstatus_mod instead of umPrintMgr",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def printstar(self, lines: List[str]) -> TestResult:
        """Check for PRINT rather than umMessage and umPrint"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            if re.search(r"\bPRINT\s*\*", clean_line, re.IGNORECASE):
                self.add_extra_error("PRINT * used")
                failures += 1
                error_log = self.add_error_log(error_log, "PRINT * used",
                                               count + 1)

        return TestResult(
            checker_name="Use of PRINT rather than umMessage and umPrint",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def write6(self, lines: List[str]) -> TestResult:
        """Check for WRITE(6) rather than umMessage and umPrint"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            if re.search(r"\bWRITE\s*\(\s*6\s*,", clean_line, re.IGNORECASE):
                self.add_extra_error("WRITE(6) used")
                failures += 1
                error_log = self.add_error_log(error_log, "WRITE(6) used",
                                               count + 1)

        return TestResult(
            checker_name="Use of WRITE(6) rather than umMessage and umPrint",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def um_fort_flush(self, lines: List[str]) -> TestResult:
        """Check for um_fort_flush rather than umPrintFlush"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            if re.search(r"\bum_fort_flush\b", line):
                self.add_extra_error("um_fort_flush used")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "um_fort_flush used", count + 1
                )
        return TestResult(
            checker_name="Use of um_fort_flush rather than umPrintFlush",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def svn_keyword_subst(self, lines: List[str]) -> TestResult:
        """Check for Subversion keyword substitution"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            if re.search(r"\$\w+\$", line):
                self.add_extra_error("SVN keyword substitution")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "SVN keyword substitution", count + 1
                )
        return TestResult(
            checker_name="Subversion keyword substitution",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def omp_missing_dollar(self, lines: List[str]) -> TestResult:
        """Check for !OMP instead of !$OMP"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            if (re.search(r"!\s*OMP\b", line) and
                    not re.search(r"!\$OMP", line)):
                self.add_extra_error("!OMP without $")
                failures += 1
                error_log = self.add_error_log(error_log, "!OMP without $",
                                               count + 1)

        return TestResult(
            checker_name="!OMP without $",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def cpp_ifdef(self, lines: List[str]) -> TestResult:
        """Check for #ifdef/#ifndef rather than #if defined()"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            if re.search(r"^\s*#\s*if(n)?def\b", line):
                self.add_extra_error("#ifdef/#ifndef used")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "#ifdef/#ifndef used", count + 1
                )

        return TestResult(
            checker_name="#ifdef/#ifndef used",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def cpp_comment(self, lines: List[str]) -> TestResult:
        """Check for Fortran comments in CPP directives"""
        """
    TODO: This looks like it will incorrectly fail # if !defined(X)
        How did the original do this test?"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            match = re.search(
                r"^\s*#if *(!)?defined\s*\(\s*\w+\s*\)(.*)", line
            ) or re.search(r"^\s*#(else) *(.*)", line)
            if match:
                if re.search(r".*!", match.group(2)):
                    self.add_extra_error("Fortran comment in CPP directive")
                    failures += 1
                    error_log = self.add_error_log(
                        error_log, "Fortran comment in CPP directive",
                        count + 1
                    )

        return TestResult(
            checker_name="Fortran comment in CPP directive",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def obsolescent_fortran_intrinsic(self, lines: List[str]) -> TestResult:
        """Check for archaic Fortran intrinsic functions"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            for intrinsic in obsolescent_intrinsics:
                if re.search(rf"\b{intrinsic}\b", clean_line, re.IGNORECASE):
                    self.add_extra_error(f"obsolescent intrinsic: {intrinsic}")
                    failures += 1
                    error_log = self.add_error_log(
                        error_log, f"obsolescent intrinsic: {intrinsic}",
                        count + 1
                    )

        return TestResult(
            checker_name="obsolescent intrinsic",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def exit_stmt_label(self, lines: List[str]) -> TestResult:
        """Check that EXIT statements are labelled"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            if re.search(r"\bEXIT\s*$", clean_line, re.IGNORECASE):
                self.add_extra_error("unlabelled EXIT statement")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "unlabelled EXIT statement", count + 1
                )

        return TestResult(
            checker_name="unlabelled EXIT statement",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def intrinsic_modules(self, lines: List[str]) -> TestResult:
        """Check intrinsic modules are USEd with INTRINSIC keyword"""
        failures = 0
        intrinsic_modules = ["ISO_C_BINDING", "ISO_FORTRAN_ENV"]
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            for module in intrinsic_modules:
                if re.search(
                    rf"\bUSE\s+(::)*\s*{module}\b", clean_line, re.IGNORECASE
                ) and not re.search(r"\bINTRINSIC\b", clean_line,
                                    re.IGNORECASE):
                    self.add_extra_error(
                        f"intrinsic module {module} without INTRINSIC")
                    failures += 1
                    error_log = self.add_error_log(
                        error_log,
                        f"intrinsic module {module} without INTRINSIC",
                        count + 1,
                    )

        return TestResult(
            checker_name="intrinsic modules",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def read_unit_args(self, lines: List[str]) -> TestResult:
        """Check READ statements have explicit UNIT= as first argument"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            if match := re.search(r"\bREAD\s*\(\s*([^,)]+)", clean_line,
                                  re.IGNORECASE):
                first_arg = match.group(1).strip()
                if not first_arg.upper().startswith("UNIT="):
                    self.add_extra_error("READ without explicit UNIT=")
                    failures += 1
                    error_log = self.add_error_log(
                        error_log, "READ without explicit UNIT=", count + 1
                    )

        return TestResult(
            checker_name="read unit args",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def retire_if_def(self, lines: List[str]) -> TestResult:
        """Check for if-defs due for retirement"""
        # retired_ifdefs = ['VATPOLES', 'A12_4A', 'A12_3A', 'UM_JULES',
        # 'A12_2A',]
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)
            if match := re.search(
                r"^#(?:(?:ifn?def|"  # ifdef/ifndef
                r"(?:el)?if\s*\S*?defined\s*\()"  # elif/if defined(
                r"\s*([^\)\s]*)\)?)",  # SYMBOL
                line,
                re.IGNORECASE,
            ):
                # # The above match either returns [None, SYMBOL] or
                # [SYMBOL, None]
                # SYMBOL = [x for x in match.groups() if x] # reduce to a
                # list of 1 element
                if match.group(1) in retired_ifdefs:
                    self.add_extra_error(f"retired if-def: {match.group(1)}")
                    failures += 1
                    error_log = self.add_error_log(
                        error_log, f"retired if-def: {match.group(1)}",
                        count + 1
                    )
        return TestResult(
            checker_name="retired if-def",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def implicit_none(self, lines: List[str]) -> TestResult:
        """Check file has at least one IMPLICIT NONE"""
        error_log = {}
        no_implicit_none = True
        for line in lines:
            if re.search(r"\bIMPLICIT\s+NONE\b", line, re.IGNORECASE):
                no_implicit_none = False
                break

        if no_implicit_none:
            self.add_extra_error("missing IMPLICIT NONE")
            error_log = self.add_error_log(
                error_log, "No IMPLICIT NONE found in file", 0
            )

        return TestResult(
            checker_name="implicit none",
            failure_count=1 if no_implicit_none else 0,
            passed=not no_implicit_none,
            output="Checked for IMPLICIT NONE statement.",
            errors=error_log,
        )

    def forbidden_stop(self, lines: List[str]) -> TestResult:
        """Check for STOP or CALL abort"""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r"!.*$", "", clean_line)

            if re.search(r"\b(STOP|CALL\s+abort)\b", clean_line,
                         re.IGNORECASE):
                self.add_extra_error("STOP or CALL abort used")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "STOP or CALL abort used", count + 1
                )

        return TestResult(
            checker_name="forbidden stop",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def intrinsic_as_variable(self, lines: List[str]) -> TestResult:
        """Check for Fortran function used as variable name"""
        failures = 0
        error_log = {}
        count = -1
        # This would check for intrinsic function names used as variables
        # Simplified implementation
        # The AI said that - This needs to be compared to the Perl
        # as I doubt this does anything near what that did...
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            check = (r"^\s*(INTEGER|REAL|LOGICAL|CHARACTER)\s*.*:" +
                     r":\s*(SIN|COS|LOG|EXP|TAN)\b")
            if re.search(check, clean_line, re.IGNORECASE, ):
                self.add_extra_error("intrinsic function used as variable")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "intrinsic function used as variable", count + 1
                )

        return TestResult(
            checker_name="intrinsic as variable",
            failure_count=failures,
            passed=(failures == 0),
            output=f"Checked {count+1} lines, found {failures} failures.",
            errors=error_log,
        )

    def check_crown_copyright(self, lines: List[str]) -> TestResult:
        """Check for crown copyright statement"""
        """
    TODO: This is a very simplistic check and will not detect many
        cases which break UMDP3. I suspect the Perl Predeccessor
        did much more convoluted tests"""
        comment_lines = [
            line.upper() for line in lines if line.lstrip(" ").startswith("!")
        ]
        file_content = "\n".join(comment_lines)
        error_log = {}
        found_copyright = False
        if "CROWN COPYRIGHT" in file_content or "COPYRIGHT" in file_content:
            found_copyright = True

        if not found_copyright:
            self.add_extra_error(
                "missing copyright or crown copyright statement")
            error_log = self.add_error_log(
                error_log, "missing copyright or crown copyright statement", 0
            )
        return TestResult(
            checker_name="Crown Copyright Statement",
            failure_count=0 if found_copyright else 1,
            passed=found_copyright,
            output="Checked for crown copyright statement.",
            errors=error_log,
        )

    def check_code_owner(self, lines: List[str]) -> TestResult:
        """Check for correct code owner comment"""
        """
    TODO: oh wow is this test worthless. We don't even guarentee to put
        the wrds "code owner" in a file. Plus, that's before you take into
        account both returns were '0' - so it couldn't possibly fail
        (picard.gif)
        The Perl looks to have been designed to check the whole file, and turns
        various logicals on/off dependent on previously processed lines."""
        # Simplified check for code owner information
        file_content = "\n".join(lines)
        found_code_owner = False
        error_log = {}
        if ("Code Owner:" in file_content or
                "code owner" in file_content.lower()):
            # print(f"Debug: Found {file_content.lower()}")
            found_code_owner = True

        # This is often a warning rather than an error
        if not found_code_owner:
            self.add_extra_error("missing code owner comment")
            error_log = self.add_error_log(error_log,
                                           "missing code owner comment", 0)
        return TestResult(
            checker_name="Code Owner Comment",
            failure_count=0 if found_code_owner else 1,
            passed=found_code_owner,
            output="Checked for code owner comment.",
            errors=error_log,
        )

    def array_init_form(self, lines: List[str]) -> TestResult:
        """Check for old array initialization form"""
        """
    TODO: Another instance that assumes continuation lines are
        concatenated prior to executing the actual test to ensure both forward
        slashes are on the same line."""
        failures = 0
        error_log = {}
        count = -1
        for count, line in enumerate(lines):
            clean_line = self.remove_quoted(line)
            if re.search(r"\(/.*?\/\)", clean_line):
                self.add_extra_error("old array initialization form (/ /)")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "old array initialization form (/ /)", count + 1
                )

        return TestResult(
            checker_name="Old Array Initialization Form",
            failure_count=failures,
            passed=(failures == 0),
            output="Checked for old array initialization form (/ /).",
            errors=error_log,
        )

    def line_trail_whitespace(self, lines: List[str]) -> TestResult:
        """Check for trailing whitespace"""
        failures = 0
        error_log = {}
        for count, line in enumerate(lines):
            if re.search(r"\s+$", line):
                self.add_extra_error("trailing whitespace")
                failures += 1
                error_log = self.add_error_log(
                    error_log, "trailing whitespace", count + 1
                )
        return TestResult(
            checker_name="Trailing Whitespace",
            failure_count=failures,
            passed=(failures == 0),
            output="Checked for trailing whitespace.",
            errors=error_log,
        )

    # C-specific tests

    def c_integral_format_specifiers(self, lines: List[str]) -> int:
        """Check C integral format specifiers have space"""
        failures = 0
        for line in lines:
            if re.search(r'%\d+[dioxX]"', line):
                self.add_extra_error("missing space in format specifier")
                failures += 1

        return failures

    def c_deprecated(self, lines: List[str]) -> int:
        """Check for deprecated C identifiers"""
        failures = 0
        for line in lines:
            for identifier in deprecated_c_identifiers:
                if re.search(rf"\b{identifier}\b", line):
                    self.add_extra_error(
                        f"deprecated C identifier: {identifier}")
                    failures += 1

        return failures

    def c_openmp_define_pair_thread_utils(self, lines: List[str]) -> int:
        """Check C OpenMP define pairing with thread utils"""
        failures = 0
        for line in lines:
            if re.search(r"#\s*if.*_OPENMP", line):
                if not re.search(r"SHUM_USE_C_OPENMP_VIA_THREAD_UTILS", line):
                    self.add_extra_error(
                        "_OPENMP without SHUM_USE_C_OPENMP_VIA_THREAD_UTILS"
                    )
                    failures += 1

        return failures

    def c_openmp_define_no_combine(self, lines: List[str]) -> int:
        """Check C OpenMP defines not combined with third macro"""
        failures = 0
        for line in lines:
            if re.search(
                r"_OPENMP.*&&.*SHUM_USE_C_OPENMP_VIA_THREAD_UTILS.*&&", line
            ) or re.search(
                r"&&.*_OPENMP.*&&.*SHUM_USE_C_OPENMP_VIA_THREAD_UTILS", line
            ):
                self.add_extra_error(
                    "OpenMP defines combined with third macro")
                failures += 1

        return failures

    def c_openmp_define_not(self, lines: List[str]) -> int:
        """Check for !defined(_OPENMP) usage"""
        failures = 0
        for line in lines:
            if re.search(r"!\s*defined\s*\(\s*_OPENMP\s*\)", line):
                self.add_extra_error("!defined(_OPENMP) used")
                failures += 1

        return failures

    def c_protect_omp_pragma(self, lines: List[str]) -> int:
        """Check OMP pragma is protected with ifdef"""
        failures = 0
        in_openmp_block = False

        for line in lines:
            if re.search(r"#\s*if.*_OPENMP", line):
                in_openmp_block = True
            elif re.search(r"#\s*endif", line):
                in_openmp_block = False
            elif re.search(r"#\s*pragma\s+omp", line) or re.search(
                r"#\s*include\s*<omp\.h>", line
            ):
                if not in_openmp_block:
                    self.add_extra_error("unprotected OMP pragma/include")
                    failures += 1

        return failures

    def c_ifdef_defines(self, lines: List[str]) -> int:
        """Check for #ifdef style rather than #if defined()"""
        failures = 0
        for line in lines:
            if re.search(r"^\s*#\s*ifdef\b", line):
                self.add_extra_error("#ifdef used instead of #if defined()")
                failures += 1

        return failures

    def c_final_newline(self, lines: List[str]) -> int:
        """Check C unit ends with final newline"""
        if lines and not lines[-1].endswith("\n"):
            self.add_extra_error("missing final newline")
            return 1

        return 0
