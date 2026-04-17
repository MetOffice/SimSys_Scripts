# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

""" This is the storage location for rules based on what's defined as the standard in the UMDP: 003 document. These rules are not necessarily the same as the rules defined in the UMDP: 003 document, but they are based on them. The rules in this file are used by the umdp3_checker script to check for compliance with the UMDP: 003 standard.
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

def add_error_log(
    error_log: Dict, key: str = "no key", value: int = 0
) -> Dict:
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
        new_value = "".join(random.choices(string.ascii_letters + string.digits, k=length))
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
        line += next_line.lstrip()  # Concatenate with the next line, removing leading whitespace
    return line


"""
3.1 Source files should only contain a single program unit
"""
"""TODO: routine to identify the first program unit in the file and store it's name.
Then check it's the same name as the last thing closed."""

"""
* Modules may be used to group related variables, subroutines and functions.
* Each separate file within the source tree should be uniquely named.
        - Not sure that's possible/easy within the current framework to process one file at a time...
* The name of the file should reflect the name of the programming unit.
* Multiple versions of the same file should be named filename-#ver where #ver is the section/version number (e.g. 1a,2a,2b. . . ).
* You should avoid naming your program units and variables with names that match an intrinsic FUNCTION, SUBROUTINE or MODULE. We recommend the use of unique names within a program unit.
* You should also avoid naming your program units and variables with names that match a keyword in a Fortran statement.
* Avoid giving program units names that are likely to be used as variable names elsewhere in the code, e.g. field or string. This makes searching the code difficult and can cause the code browser to make erroneous connections between unrelated routines.
* Subroutines should be kept reasonably short, where appropriate, say up to about 100 lines of executable code, but don’t forget there are start up overheads involved in calling an external subroutine so they should do a reasonable amount of work
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
    comment_lines = [
        line.upper() for line in lines if line.lstrip(" ").startswith("!")
    ]
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
* Headers are an immensely important part of any code as they document what it does, and how it does it.
* The description of the MODULE and its contained SUBROUTINE may be the same and thus it need not berepeated in the latter. If a MODULE contains more than one subroutine then further descriptions are required.
* History comments should not be included in the header or routine code. FCM TRAC provides the history of our codes.
* Code author names should NOT be included explicitly within the code as they quickly become out of date and are sometimes misleading. Instead we reference a single maintainable text file which is included within the UM code repository.

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: <section_name_to_be_entered>

* Example UM templates are provided with the source of this document; subroutine, function and module
templates"""

"""3.3 Free source form
* All code should be written using the free source form.
* Please restrict code to 80 columns, so that your code can be easily viewed on any editor and screen and
can be printed easily on A4 paper. Note that CreateBC uses a limit of 100 columns, due to the nature of
the object-orientated code.
"""
def r3_3_2line_too_long(lines: List[str]) -> TestResult:
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
* Never put more than one statement on a line.
* Write your program in UK English, unless you have a very good reason for not doing so."""

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
* The rest of the code may be written in either lower-case with underscores or CamelCase.
• To improve readability, you should always use the optional space to separate the Fortran keywords. The
full list of Fortran keywords with an optional spaces is:
ELSE IF END DO END FORALL END FUNCTION
END IF END INTERFACE END MODULE END PROGRAM
END SELECT END SUBROUTINE END TYPE END WHERE
SELECT CASE ELSE WHERE DOUBLE PRECISION END ASSOCIATE
END BLOCK END BLOCK DATA END ENUM END FILE
END PROCEDURE GO TO IN OUT SELECT TYPE
Note that not all of these are approved or appropriate for use in UM code. This rule also applies to OpenMP
keywords. (See: 3.15)
• The full version of END should be used at all times, eg END SUBROUTINE <name> and END FUNCTION <name>
• New code should be written using Fortran 95/2003 features. Avoid non-portable vendor/compiler exten-
sions.
• When writing a REAL literal with an integer value, put a 0 after the decimal point (i.e. 1.0 as opposed to 1.)
to improve readability.
• Avoid using obsolescent features of the Fortran language, instead make use of F95/2003 alternatives. For
example, statement functions are among the list of deprecated features in the F95 standard and these can
be replaced by FUNCTIONs within appropriate MODULEs.
• Do not use archaic forms of intrinsic functions. For example, LOG() should be used in place of ALOG(),
MAX() instead of AMAX1(), REAL() instead of FLOAT() etc.
• Never use the PAUSE statement.
• Never use the STOP statement, see 3.19
• The standard delimiter for namelists is /. In particular, note that &END is non-standard and should be
avoided. For further information on namelists please refer to 4.1
• Only use the generic names of intrinsic functions, avoid the use of ’hardware’ specific intrinsic functions.
Use the latter if an only if there is an optimisation benefit and then it must be protected by a platform
specific CPP flag 3.17"""


def capitulated_keywords(lines: List[str]) -> TestResult:
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
        clean_line = remove_quoted(line)
        clean_line = comment_line.sub("", clean_line)

        # Check for lowercase keywords
        for word in word_splitter.findall(clean_line):
            upcase = word.upper()
            if upcase in fortran_keywords and word != upcase:
                error_log = add_error_log(
                    error_log, f"capitulated keyword: {word}", line_count
                )
                failures += 1

    return TestResult(
        checker_name="Capitulated Keywords",
        failure_count=failures,
        passed=(failures == 0),
        output=f"Checked {line_count} lines, found {failures} failures.",
        errors=error_log,
    )

list_O_tests = [
    r3_2_1_check_crown_copyright,
    r3_3_2line_too_long,
    r3_4_1_capitalised_keywords,
]
