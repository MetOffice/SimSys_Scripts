""" This is the storage location for rules based on what's defined as the standard in the UMDP: 003 document. These rules are not necessarily the same as the rules defined in the UMDP: 003 document, but they are based on them. The rules in this file are used by the umdp3_checker script to check for compliance with the UMDP: 003 standard.
For now, the document has been copied into this file as a placeholder for the rules as they appear."""
import re
from typing import List, Dict
# from fortran_keywords import fortran_keywords
# from search_lists import (
#     obsolescent_intrinsics,
#     unseparated_keywords_list,
#     retired_ifdefs,
#     deprecated_c_identifiers,
# )
from dataclasses import dataclass, field
from umdp3_checker_rules import TestResult
"""
3.1 Source files should only contain a single program unit

* Modules may be used to group related variables, subroutines and functions. Each separate file within the source tree should be uniquely named.
* The name of the file should reflect the name of the programming unit. Multiple versions of the same file should be named filename-#ver where #ver is the section/version number (e.g. 1a,2a,2b. . . ). For example:
     <filename-#ver>.F90 when writing a <subroutine>
     <filename_mod-#ver>.F90 with writing a <module_mod>
     <existing filename>.F90 with <module_mod> only if upgrading existing subroutine since Subversion does not handle renaming of files very well and this allows history of the file to be easily retrieved.
  This makes it easier to navigate the UM code source tree for given routines.
* You should avoid naming your program units and variables with names that match an intrinsic FUNCTION, SUBROUTINE or MODULE. We recommend the use of unique names within a program unit.
* You should also avoid naming your program units and variables with names that match a keyword in a Fortran statement.
* Avoid giving program units names that are likely to be used as variable names elsewhere in the code, e.g. field or string. This makes searching the code difficult and can cause the code browser to make erroneous connections between unrelated routines.
* Subroutines should be kept reasonably short, where appropriate, say up to about 100 lines of executable code, but don’t forget there are start up overheads involved in calling an external subroutine so they should do a reasonable amount of work
"""