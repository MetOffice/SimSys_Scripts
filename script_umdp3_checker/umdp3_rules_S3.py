# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""This is the storage location for rules based on what's defined as the standard in the UMDP: 003 document. These rules are not necessarily the same as the rules defined in the UMDP: 003 document, but they are based on them. The rules in this file are used by the umdp3_checker script to check for compliance with the UMDP: 003 standard.
For now, the document has been copied into this file as a placeholder for the rules as they appear."""

import re
from typing import List, Dict
from umdp3_checker_rules import TestResult
from fortran_keywords import fortran_keywords

"""ToDo: This lot will need putting back as and when the tests that use them get imported/re-created"""
# from fortran_keywords import fortran_keywords
# from search_lists import (
#     obsolescent_intrinsics,
#     unseparated_keywords_list,
#     retired_ifdefs,
#     deprecated_c_identifiers,
# )
# from dataclasses import dataclass, field
# from script_umdp3_checker import fortran_keywords

comment_line = re.compile(r"!.*$")
word_splitter = re.compile(r"\b\w+\b")


def add_error_log(error_log: Dict, key: str = "no key", value: int = 0) -> Dict:
    """Add error information to the dictionary.
    This adds the error statement, some are more unique than others, along with the
    line number the error occurs on. Line number(s) are stored in a list in case the
    same error occurs multiple times in a file."""
    if key not in error_log:
        error_log[key] = []
    error_log[key].append(value)
    return error_log


def remove_quoted(line: str) -> str:
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


"""This is in case I return to the idea of replacing the quoted sections from above
with something unique and storing them in order to put them back at some stage..."""


def create_unique_random_string(storage_set, length: int = 7) -> str:
    """Create a unique random string of a given length.
    Creates a random string of given length and ensures it hasn't been used before."""
    import random
    import string

    new_value = "".join(random.choices(string.ascii_letters + string.digits, k=length))
    while new_value in storage_set:
        new_value = "".join(
            random.choices(string.ascii_letters + string.digits, k=length)
        )
    storage_set.add(new_value)
    return new_value


def remove_comments(line: str) -> str:
    """Remove comments from the lines :
    There is a bit of an assumption here that quoted text has already been removed, so that we don't accidentally remove text after an "!" in a string."""
    return comment_line.sub("", line).rstrip()


def concatenate_lines(lines: List[str], line_no: int) -> str:
    """Concatenate the continuation lines into a single string"""
    # Find first line and check for continuation character.
    line = lines[line_no - 1]
    line = remove_comments(line)
    line = remove_quoted(line)
    while line.rstrip().endswith("&"):
        line = line.rstrip()[:-1]  # Remove the continuation character
        line_no += 1
        if line_no > len(lines):
            break  # Avoid going out of bounds
        next_line = lines[line_no - 1]
        next_line = remove_comments(next_line)
        next_line = remove_quoted(next_line)
        line += (
            next_line.lstrip()
        )  # Concatenate with the next line, removing leading whitespace
    return line


"""
3.1 Source files should only contain a single program unit
"""


def r3_1_1_there_can_be_only_one(
    lines: List[str],
) -> TestResult:  # ..one programming unit in a file, that is.
    """Check for multiple program units in a file:
    Given that a MODULE or a PROGRAM can contain multiples of the other program units,
    just going to confirm the first one decalred is the last one ENDed."""
    program_unit_keywords = {"PROGRAM", "MODULE", "SUBROUTINE", "FUNCTION"}

    # unsure if  "TYPE" should be included.
    def find_first(lines: List[str]) -> tuple[bool, str]:
        for line in lines:
            executable_line = remove_comments(line).strip()
            if not executable_line:
                continue  # Skip empty lines
            for keyword in program_unit_keywords:
                unit_name_search = re.search(rf"{keyword}\s+(\w+)", executable_line)
                if unit_name_search:
                    unit_type = keyword
                    unit_name = unit_name_search.group(1)
                    return (True, f"{unit_type} {unit_name}")
            return (
                False,
                "First executable line doesn't define an accepted programming unit"
                f" : {line}",
            )
        return (False, "No program unit found.")

    def find_last(lines: List[str], unit_type: str, unit_name: str) -> tuple[bool, str]:
        for line in reversed(lines):
            executable_line = remove_comments(line).strip()
            if not executable_line:
                continue  # Skip empty lines
            unit_name_search = re.search(rf"END\s+{unit_type}\s+(\w+)", executable_line)
            if unit_name_search:
                if unit_name_search.group(1) == unit_name:
                    return (True, "")
                else:
                    return (
                        False,
                        "END statement found for a different program unit: "
                        f"should be {unit_name} got {unit_name_search.group(1)}.",
                    )
            else:
                return (
                    False,
                    "Last executable line not a matching END statement for the first program unit found.",
                )
        return (False, "No matching END statement found for the first program unit.")

    found_first, first_result = find_first(lines)
    if not found_first:
        failure_count = 1
        found_last = False
        error_log = add_error_log({}, first_result, 0)
    else:
        unit_type, unit_name = first_result.split()
        found_last, last_result = find_last(lines, unit_type, unit_name)
        if found_last:
            failure_count = 0
            error_log = {}
        else:
            failure_count = 1
            error_log = add_error_log({}, last_result, 0)
    return TestResult(
        checker_name="Test 3.1 Only One Program Unit",
        failure_count=failure_count,
        passed=found_first and found_last,
        output="Checked for multiple program units.",
        errors=error_log,
    )


"""
* TODO: Modules may be used to group related variables, subroutines and functions.
        - Really unsure as to how to test that's what they're doing...
* TODO: Each separate file within the source tree should be uniquely named.
        - Not sure that's possible/easy within the current framework to process one file at a time...
* TODO: The name of the file should reflect the name of the programming unit.
* TODO: Multiple versions of the same file should be named filename-#ver where #ver is the section/version number (e.g. 1a,2a,2b. . . ).
* TODO: You should avoid naming your program units and variables with names that match an intrinsic FUNCTION, SUBROUTINE or MODULE. We recommend the use of unique names within a program unit.
* TODO: You should also avoid naming your program units and variables with names that match a keyword in a Fortran statement.
* TODO: Avoid giving program units names that are likely to be used as variable names elsewhere in the code, e.g. field or string. This makes searching the code difficult and can cause the code browser to make erroneous connections between unrelated routines.
* TODO: Subroutines should be kept reasonably short, where appropriate, say up to about 100 lines of executable code, but don’t forget there are start up overheads involved in calling an external subroutine so they should do a reasonable amount of work
"""

"""3.2 Headers
* All programming units require a suitable copyright header."""


def r3_2_1_check_crown_copyright(lines: List[str]) -> TestResult:
    """Check for crown copyright statement"""
    """
    TODO: This is a very simplistic check and will not detect many cases which break UMDP3.
    It will pass if the word copyright appears on a commented out line. This could
    include passing :
    ! I should put a copyright statement here...

    I suspect the Perl Predeccessor
    did much more convoluted tests"""
    comment_lines = [line.upper() for line in lines if line.lstrip(" ").startswith("!")]
    file_content = "\n".join(comment_lines)
    error_log = {}
    found_copyright = False
    if "CROWN COPYRIGHT" in file_content or "COPYRIGHT" in file_content:
        found_copyright = True

    if not found_copyright:
        error_log = add_error_log(
            error_log, "missing copyright or crown copyright statement", 0
        )
    return TestResult(
        checker_name="Test 2.1 Check Copyright",
        failure_count=0 if found_copyright else 1,
        passed=found_copyright,
        output="Checked for crown copyright statement.",
        errors=error_log,
    )


"""
* TODO: Headers are an immensely important part of any code as they document what it does, and how it does it.
* TODO: The description of the MODULE and its contained SUBROUTINE may be the same and thus it need not berepeated in the latter. If a MODULE contains more than one subroutine then further descriptions are required.
* TODO: History comments should not be included in the header or routine code. FCM TRAC provides the history of our codes.
* TODO: Code author names should NOT be included explicitly within the code as they quickly become out of date and are sometimes misleading. Instead we reference a single maintainable text file which is included within the UM code repository.

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: <section_name_to_be_entered>

* TODO: Example UM templates are provided with the source of this document; subroutine, function and module
templates"""

"""3.3 Free source form
* TODO: All code should be written using the free source form.
        - Could be fun, look for 7 leading spaces regularly. Or only something in col6 and/or only nos in cols 1 to 5
* Please restrict code to 80 columns
    Note that CreateBC uses a limit of 100 columns, due to the nature of
    the object-orientated code.
"""


def r3_3_2_line_too_long(lines: List[str]) -> TestResult:
    """Check for lines longer than 80 characters"""
    failures = 0
    error_log = {}
    count = -1
    for count, line in enumerate(lines, 1):
        if len(line.rstrip()) > 80:
            failures += 1
            error_log = add_error_log(error_log, "line too long", count)

    return TestResult(
        checker_name="Test 3.2 Line Length",
        failure_count=failures,
        passed=(failures == 0),
        output=f"Checked {count + 1} lines, found {failures} failures.",
        errors=error_log,
    )


"""
* TODO: Never put more than one statement on a line.
* TODO: Write your program in UK English, unless you have a very good reason for not doing so."""

"""3.4 Fortran style
* To improve readability, write your code using the ALL CAPS Fortran keywords approach."""


def r3_4_1_capitalised_keywords(lines: List[str]) -> TestResult:
    """Check for the presence of lowercase Fortran keywords, which are
    taken from an imported list 'fortran_keywords'."""
    failures = 0
    error_log = {}
    count = -1
    for count, line in enumerate(lines):
        # Remove quoted strings and comments
        if line.lstrip(" ").startswith("!"):
            continue
        clean_line = remove_quoted(line)
        clean_line = comment_line.sub("", clean_line)
        # Check for lowercase keywords
        for word in word_splitter.findall(clean_line):
            upcase = word.upper()
            if upcase in fortran_keywords and word != upcase:
                failures += 1
                error_log = add_error_log(
                    error_log, f"lowercase keyword: {word}", count + 1
                )

    return TestResult(
        checker_name="Test 4.1 Capitalised Keywords",
        failure_count=failures,
        passed=(failures == 0),
        output=f"Checked {count + 1} lines, found {failures} failures.",
        errors=error_log,
    )


"""
* The rest of the code may be written in either lower-case with underscores or
    CamelCase.
"""


def r3_4_2_no_full_uppercase_variable_names(lines: List[str]) -> TestResult:
    """Check for lowercase or CamelCase variable names only"""
    """
TODO: This is a very simplistic check and will not detect many
    cases which break UMDP3. I suspect the Perl Predecessor concatenated
    continuation lines prior to 'cleaning' and checking. Having identified
    a declaration, it also then scanned the rest of the file for that
    variable name in any case."""
    failures = 0
    error_log = {}
    count = -1
    declaration_search = re.compile(
        r"^\s*(INTEGER|REAL|LOGICAL|CHARACTER|TYPE)\s*.*::\s*", re.IGNORECASE
    )
    for count, line in enumerate(lines, 1):
        clean_line = remove_quoted(line)
        clean_line = remove_comments(clean_line)
        # Simple check for UPPERCASE variable declarations
        if declaration_search.search(clean_line):
            full_line = concatenate_lines(lines, count)
            clean_line = full_line.split("::", 1)[1].strip()
            clean_line = re.sub(r"\([^)]*\)", "", clean_line)
            variables = [var.strip() for var in clean_line.split(",")]
            for var in variables:
                var = var.split(r"=", 1)[0].strip()  # Remove any assignment part
                if var and var.upper() == var:
                    failures += 1
                    error_log = add_error_log(
                        error_log,
                        f"Found UPPERCASE variable name in declaration at line {count}:"
                        + f" \"{var}\"",
                        count,
                    )
    return TestResult(
        checker_name="No Full Uppercase variable names",
        failure_count=failures,
        passed=(failures == 0),
        output=f"Checked {count} lines, found {failures} UPPERCASE variables.",
        errors=error_log,
    )


"""
* TODO: To improve readability, you should always use the optional space to separate
        the Fortran keywords.
        This rule also applies to OpenMP keywords. (See: 3.15)
* TODO: The full version of END should be used at all times,
        eg END SUBROUTINE <name> and END FUNCTION <name>
* TODO: New code should be written using Fortran 95/2003 features. Avoid non-portable
        vendor/compiler extensions.
* TODO: When writing a REAL literal with an integer value, put a 0 after the decimal
        point (i.e. 1.0 as opposed to 1.) to improve readability.
* TODO: Avoid using obsolescent features of the Fortran language, instead make use of
        F95/2003 alternatives. For example, statement functions are among the list of
        deprecated features in the F95 standard and these can be replaced by FUNCTIONs
        within appropriate MODULEs.
* TODO: Do not use archaic forms of intrinsic functions. For example:
        LOG() should be used in place of ALOG(),
        MAX() instead of AMAX1(), REAL() instead of FLOAT() etc.
* TODO: Never use the PAUSE statement.
* TODO: Never use the STOP statement, see 3.19
* TODO: The standard delimiter for namelists is /. In particular, note that &END is
        non-standard and should be avoided. For further information on namelists please
        refer to 4.1
* TODO: Only use the generic names of intrinsic functions, avoid the use of ’hardware’
        specific intrinsic functions.
        Use the latter if an only if there is an optimisation benefit and then it must
        be protected by a platform specific CPP flag 3.17
3.5 Comments and white spacing :
TODO : Copy in the rules from section 3.5 and add the appropriate tests.

3.6 The use of modules :
TODO : Copy in the rules from section 3.6 and add the appropriate tests.

3.7 Argument and variable declaration :
TODO : Copy in the rules from section 3.7 and add the appropriate tests.

3.8 Allocatables :
TODO : Copy in the rules from section 3.8 and add the appropriate tests.

3.9 Code IF blocks, DO LOOPs, and other constructs :
TODO : Copy in the rules from section 3.9 and add the appropriate tests.

3.10 Line continuation :
TODO : Copy in the rules from section 3.10 and add the appropriate tests.

3.11 Fortran I/O :
TODO : Copy in the rules from section 3.11 and add the appropriate tests.

3.12 Formatting and output of text :
TODO : Copy in the rules from section 3.12 and add the appropriate tests.

3.13 PrintStatus :
TODO : Copy in the rules from section 3.13 and add the appropriate tests.

3.14 DrHook :
TODO : Copy in the rules from section 3.14 and add the appropriate tests.
        I /think/ DrHook is being (possibly has been) retired, so this section of the docs needs updating and tests for the new system need to be added.

3.15 OpenMP :
TODO : Copy in the rules from section 3.15 and add the appropriate tests.

3.16 MPI :
TODO : Copy in the rules from section 3.16 and add the appropriate tests.

3.17 Preprocessing :
TODO : Copy in the rules from section 3.17 and add the appropriate tests.

3.18 Code duplication :
TODO : Copy in the rules from section 3.18 and add the appropriate tests.

3.19 Error reporting :
TODO : Copy in the rules from section 3.19 and add the appropriate tests.

TODO : Section 4 has guidelines on "specific standards".
        Unsure as yet if they can be tested for, but it needs looking into.
"""

# A list, of all the tests defined above, so it can be imported easily.
list_O_tests = [
    r3_1_1_there_can_be_only_one,
    r3_2_1_check_crown_copyright,
    r3_3_2_line_too_long,
    r3_4_1_capitalised_keywords,
    r3_4_2_no_full_uppercase_variable_names,
]
