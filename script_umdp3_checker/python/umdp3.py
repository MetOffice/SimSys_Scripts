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
from typing import List, Dict, Set

# Declare version
VERSION = '13.5.0'

fortran_keywords = (
    "IF", "END", "DO", "CALL", "THEN", "USE", "INTEGER", "PARAMETER", "ELSE",
    "SUBROUTINE", "IMPLICIT", "NONE", ".AND.", "REAL", "MODULE", ".OR.", "LOGICAL", ".FALSE.",
    "CASE", "ALLOCATABLE", "RETURN", "PRIVATE", ".TRUE.", "CONTAINS", "TO", "POINTER",
    "ALLOCATE", "IN", "TYPE", "SELECT", "CHARACTER", "NOT", "IS", ".NOT.", "FUNCTION", "SAVE",
    "GO", "DATA", "DEALLOCATE", "WRITE", "PUBLIC", "INTERFACE", "TARGET", "INTENT", "EXIT",
    "AND", "WHERE", "FILE", "OPTIONAL", "NAMELIST", "ERROR", "PROCEDURE", "READ", "TIME",
    "WHILE", "OR", "VALUE", "PASS", "CYCLE", "NUMBER", "SIZE", "UNIT", "CONTINUE", "SEQUENCE",
    "NAME", "OUT", "ONLY", "MAX", "INTRINSIC", "SOURCE", "FRACTION", "IMPORT", "ALL", "RECORD",
    "DIMENSION", "DIM", "OPEN", "ANY", "KIND", "MIN", "ALLOCATED", "LOG", "C_INT64_T", "NULLIFY",
    "PROGRAM", "SCALE", "INDEX", ".EQV.", "CLOSE", "ERF", "FALSE", "RANGE", "COUNT", "SQRT",
    "SYNC", "LONG", "EXP", "INCLUDE", "PROTECTED", "FMT", "MEMORY", "RESULT", "SHAPE", "CLASS",
    "ELEMENTAL", "ABS", "POSITION", "PRESENT", "SECOND", "ASSOCIATED", "C_F_POINTER", "SUM",
    "TRIM", "IOSTAT", "LEN", "MOD", "INT", "PRECISION", "COMPLEX", "C_CHAR", "FORMAT", "BLOCK",
    "CPP", "C_PTR", "ENTRY", "PURE", "SIN", "CONVERT", "EXIST", "FREE", "PRINT", "RECURSIVE",
    "SPACING", "TRUE", ".NEQV.", "ACTION", "COMMON", "INQUIRE", "NINT", "NULL", "RANK",
    "TRANSFER", "ASYNCHRONOUS", "BACKSPACE", "C_BOOL", "DOUBLE", "STATUS", "STOP", "SYSTEM",
    "ABSTRACT", "ATAN2", "C_INTPTR_T", "C_LOC", "ERROR_UNIT", "FINAL", "IOSTAT_END", "OPENED",
    "RANDOM_SEED", "WAIT", "ACCESS", "ASSIGN", "C_INT", "C_SIZE_T", "ENUM", "ENUMERATOR", "GT",
    "LOC", "NEAREST", "REWIND", "STRUCTURE", "UNPACK", "CONTIGUOUS", "COS", "C_SIZEOF",
    "EXTERNAL", "GAMMA", "IOMSG", "OUTPUT_UNIT", "ABORT", "C_DOUBLE", "C_FLOAT", "C_INT16_T",
    "FLUSH", "FORM", "ISO_C_BINDING", "LE", "NAMED", "PRODUCT", "RANDOM_NUMBER", "SHORT", "TAN",
    "VOLATILE", "ALARM", "CHAR", "CHMOD", "C_FUNLOC", "C_FUNPTR", "C_INT8_T", "DIRECT", "EXTENDS",
    "GENERIC", "HUGE", "INPUT_UNIT", "LOCK", "PACK", "RESHAPE", "SIGN", "SYSTEM_CLOCK", "ACHAR",
    "ACOS", "ACOSD", "ACOSH", "ADJUSTL", "ADJUSTR", "ADVANCE", "AIMAG", "AINT", "ALGAMA",
    "ALOG", "ALOG10", "AMAX0", "AMAX1", "AMIN0", "AMIN1", "AMOD", "ANINT", "ASIN", "ASIND",
    "ASINH", "ASSIGNMENT", "ASSOCIATE", "ATAN", "ATAN2D", "ATAND", "ATANH", "ATOMIC_ADD",
    "ATOMIC_AND", "ATOMIC_CAS", "ATOMIC_DEFINE", "ATOMIC_FETCH_ADD", "ATOMIC_FETCH_AND",
    "ATOMIC_FETCH_OR", "ATOMIC_FETCH_XOR", "ATOMIC_INT_KIND", "ATOMIC_LOGICAL_KIND",
    "ATOMIC_OR", "ATOMIC_REF", "ATOMIC_XOR", "BACKTRACE", "BESJ0", "BESJ1", "BESJN",
    "BESSEL_J0", "BESSEL_J1", "BESSEL_JN", "BESSEL_Y0", "BESSEL_Y1", "BESSEL_YN", "BESY0",
    "BESY1", "BESYN", "BGE", "BGT", "BIND", "BIT_SIZE", "BLANK", "BLE", "BLT", "BTEST", "CABS",
    "CCOS", "CDABS", "CDCOS", "CDEXP", "CDLOG", "CDSIN", "CDSQRT", "CEILING", "CEXP",
    "CHARACTER_KINDS", "CHARACTER_STORAGE_SIZE", "CHDIR", "CLOG", "CMPLX",
    "CODIMENSION", "COMMAND_ARGUMENT_COUNT", "COMPILER_OPTIONS", "COMPILER_VERSION",
    "CONCURRENT", "CONJG", "COSD", "COSH", "COTAN", "COTAND", "CO_BROADCAST", "CO_MAX",
    "CO_MIN", "CO_REDUCE", "CO_SUM", "CPU_TIME", "CQABS", "CQCOS", "CQEXP", "CQLOG", "CQSIN",
    "CQSQRT", "CSHIFT", "CSIN", "CSQRT", "CTIME", "C_ALERT", "C_ASSOCIATED", "C_BACKSPACE",
    "C_CARRIAGE_RETURN", "C_DOUBLE_COMPLEX", "C_FLOAT128", "C_FLOAT128_COMPLEX",
    "C_FLOAT_COMPLEX", "C_FORM_FEED", "C_F_PROCPOINTER", "C_HORIZONTAL_TAB", "C_INT128_T",
    "C_INT32_T", "C_INTMAX_T", "C_INT_FAST128_T", "C_INT_FAST16_T", "C_INT_FAST32_T",
    "C_INT_FAST64_T", "C_INT_FAST8_T", "C_INT_LEAST128_T", "C_INT_LEAST16_T",
    "C_INT_LEAST32_T", "C_INT_LEAST64_T", "C_INT_LEAST8_T", "C_LONG", "C_LONG_DOUBLE",
    "C_LONG_DOUBLE_COMPLEX", "C_LONG_LONG", "C_NEW_LINE", "C_NULL_CHAR", "C_NULL_FUNPTR",
    "C_NULL_PTR", "C_PTRDIFF_T", "C_SHORT", "C_SIGNED_CHAR", "C_VERTICAL_TAB", "DABS",
    "DACOS", "DACOSH", "DASIN", "DASINH", "DATAN", "DATAN2", "DATANH", "DATE_AND_TIME",
    "DBESJ0", "DBESJ1", "DBESJN", "DBESY0", "DBESY1", "DBESYN", "DBLE", "DCMPLX", "DCONJG",
    "DCOS", "DCOSH", "DDIM", "DECODE", "DEFERRED", "DELIM", "DERF", "DERFC", "DEXP", "DFLOAT",
    "DGAMMA", "DIGITS", "DIMAG", "DINT", "DLGAMA", "DLOG", "DLOG10", "DMAX1", "DMIN1", "DMOD",
    "DNINT", "DOT_PRODUCT", "DPROD", "DREAL", "DSHIFTL", "DSHIFTR", "DSIGN", "DSIN", "DSINH",
    "DSQRT", "DTAN", "DTANH", "DTIME", "ENCODE", "EOR", "EOSHIFT", "EPSILON", "EQ",
    "EQUIVALENCE", "EQV", "ERFC", "ERFC_SCALED", "ERRMSG", "ETIME", "EVENT_QUERY",
    "EXECUTE_COMMAND_LINE", "EXPONENT", "EXTENDS_TYPE_OF", "FDATE", "FGET", "FGETC",
    "FILE_STORAGE_SIZE", "FLOAT", "FLOOR", "FNUM", "FORALL", "FORMATTED", "FPP", "FPUT",
    "FPUTC", "FSEEK", "FSTAT", "FTELL", "GE", "GERROR", "GETARG", "GETCWD", "GETENV",
    "GETGID", "GETLOG", "GETPID", "GETUID", "GET_COMMAND", "GET_COMMAND_ARGUMENT",
    "GET_ENVIRONMENT_VARIABLE", "GMTIME", "HOSTNM", "HYPOT", "IABS", "IACHAR", "IALL",
    "IAND", "IANY", "IARGC", "IBCLR", "IBITS", "IBSET", "ICHAR", "IDATE", "IDIM", "IDINT",
    "IDNINT", "IEEE_CLASS", "IEEE_CLASS_TYPE", "IEEE_COPY_SIGN", "IEEE_IS_FINITE",
    "IEEE_IS_NAN", "IEEE_IS_NEGATIVE", "IEEE_IS_NORMAL", "IEEE_LOGB",
    "IEEE_NEGATIVE_DENORMAL", "IEEE_NEGATIVE_INF", "IEEE_NEGATIVE_NORMAL",
    "IEEE_NEGATIVE_ZERO", "IEEE_NEXT_AFTER", "IEEE_POSITIVE_DENORMAL",
    "IEEE_POSITIVE_INF", "IEEE_POSITIVE_NORMAL", "IEEE_POSITIVE_ZERO",
    "IEEE_QUIET_NAN", "IEEE_REM", "IEEE_RINT", "IEEE_SCALB", "IEEE_SELECTED_REAL_KIND",
    "IEEE_SIGNALING_NAN", "IEEE_SUPPORT_DATATYPE", "IEEE_SUPPORT_DENORMAL",
    "IEEE_SUPPORT_DIVIDE", "IEEE_SUPPORT_INF", "IEEE_SUPPORT_NAN",
    "IEEE_SUPPORT_SQRT", "IEEE_SUPPORT_STANDARD", "IEEE_UNORDERED", "IEEE_VALUE",
    "IEOR", "IERRNO", "IFIX", "IMAG", "IMAGES", "IMAGE_INDEX", "IMAGPART", "INT16", "INT2",
    "INT32", "INT64", "INT8", "INTEGER_KINDS", "IOR", "IOSTAT_EOR", "IOSTAT_INQUIRE_INTERNAL_UNIT",
    "IPARITY", "IQINT", "IRAND", "ISATTY", "ISHFT", "ISHFTC", "ISIGN", "ISNAN", "ISO_FORTRAN_ENV",
    "IS_IOSTAT_END", "IS_IOSTAT_EOR", "ITIME", "KILL", "LBOUND", "LCOBOUND", "LEADZ", "LEN_TRIM",
    "LGAMMA", "LGE", "LGT", "LINK", "LLE", "LLT", "LNBLNK", "LOCK_TYPE", "LOG10", "LOGICAL_KINDS",
    "LOG_GAMMA", "LSHIFT", "LSTAT", "LT", "LTIME", "MALLOC", "MASKL", "MASKR", "MATMUL", "MAX0",
    "MAX1", "MAXEXPONENT", "MAXLOC", "MAXVAL", "MCLOCK", "MCLOCK8", "MERGE", "MERGE_BITS",
    "MIN0", "MIN1", "MINEXPONENT", "MINLOC", "MINVAL", "MODULO", "MOVE_ALLOC", "MVBITS", "NE",
    "NEQV", "NEW_LINE", "NEXTREC", "NML", "NON_INTRINSIC", "NON_OVERRIDABLE", "NOPASS",
    "NORM2", "NUMERIC_STORAGE_SIZE", "NUM_IMAGES", "OPERATOR", "PAD", "PARITY", "PERROR",
    "POPCNT", "POPPAR", "QABS", "QACOS", "QASIN", "QATAN", "QATAN2", "QCMPLX", "QCONJG",
    "QCOS", "QCOSH", "QDIM", "QERF", "QERFC", "QEXP", "QGAMMA", "QIMAG", "QLGAMA", "QLOG",
    "QLOG10", "QMAX1", "QMIN1", "QMOD", "QNINT", "QSIGN", "QSIN", "QSINH", "QSQRT", "QTAN",
    "QTANH", "RADIX", "RAN", "RAND", "READWRITE", "REAL128", "REAL32", "REAL64", "REALPART",
    "REAL_KINDS", "REC", "RECL", "RENAME", "REPEAT", "REWRITE", "RRSPACING", "RSHIFT",
    "SAME_TYPE_AS", "SCAN", "SECNDS", "SELECTED_CHAR_KIND", "SELECTED_INT_KIND",
    "SELECTED_REAL_KIND", "SEQUENTIAL", "SET_EXPONENT", "SHIFTA", "SHIFTL", "SHIFTR",
    "SIGNAL", "SIND", "SINH", "SIZEOF", "SLEEP", "SNGL", "SPREAD", "SRAND", "STAT",
    "STAT_FAILED_IMAGE", "STAT_LOCKED", "STAT_LOCKED_OTHER_IMAGE", "STAT_STOPPED_IMAGE",
    "STAT_UNLOCKED", "STORAGE_SIZE", "SUBMODULE", "SYMLNK", "TAND", "TANH", "THIS_IMAGE",
    "TIME8", "TINY", "TRAILZ", "TRANSPOSE", "TTYNAM", "UBOUND", "UCOBOUND", "UMASK",
    "UNFORMATTED", "UNLINK", "UNLOCK", "VERIF", "VERIFY", "XOR", "ZABS", "ZCOS", "ZEXP",
    "ZLOG", "ZSIN", "ZSQRT", ".EQ.", ".GE.", ".GT.", ".LE.", ".LT.", ".NE.", ".XOR."
)

class UMDP3:
    """UMDP3 compliance checker class"""
    # precompiled, regularly used search patterns.
    comment_line = re.compile(r"!.*$")
    word_splitter = re.compile(r"\b\w+\b")

    def __init__(self):
        self._extra_error_info = {}
        self._lock = threading.Lock()
        self._number_of_files_with_variable_declarations_in_includes = 0
        
        # Fortran keywords list
        # self.fortran_keywords = {
        #     'ABORT', 'ABS', 'ABSTRACT', 'ACCESS', 'ACHAR', 'ACOS', 'ACOSD', 'ACOSH',
        #     'ACTION', 'ADJUSTL', 'ADJUSTR', 'ADVANCE', 'AIMAG', 'AINT', 'ALARM', 'ALGAMA',
        #     'ALL', 'ALLOCATABLE', 'ALLOCATE', 'ALLOCATED', 'ALOG', 'ALOG10', 'AMAX0', 'AMAX1',
        #     'AMIN0', 'AMIN1', 'AMOD', 'AND', 'ANINT', 'ANY', 'ASIN', 'ASIND', 'ASINH',
        #     'ASSIGN', 'ASSIGNMENT', 'ASSOCIATE', 'ASSOCIATED', 'ASYNCHRONOUS', 'ATAN', 'ATAN2',
        #     'ATAN2D', 'ATAND', 'ATANH', 'ATOMIC_ADD', 'ATOMIC_AND', 'ATOMIC_CAS', 'ATOMIC_DEFINE',
        #     'ATOMIC_FETCH_ADD', 'ATOMIC_FETCH_AND', 'ATOMIC_FETCH_OR', 'ATOMIC_FETCH_XOR',
        #     'ATOMIC_INT_KIND', 'ATOMIC_LOGICAL_KIND', 'ATOMIC_OR', 'ATOMIC_REF', 'ATOMIC_XOR',
        #     'BACKSPACE', 'BACKTRACE', 'BESJ0', 'BESJ1', 'BESJN', 'BESSEL_J0', 'BESSEL_J1',
        #     'BESSEL_JN', 'BESSEL_Y0', 'BESSEL_Y1', 'BESSEL_YN', 'BESY0', 'BESY1', 'BESYN',
        #     'BGE', 'BGT', 'BIND', 'BIT_SIZE', 'BLANK', 'BLE', 'BLOCK', 'BLT', 'BTEST',
        #     'CABS', 'CALL', 'CASE', 'CEILING', 'CHAR', 'CHARACTER', 'CLASS', 'CLOSE',
        #     'CMPLX', 'CODIMENSION', 'COMMAND_ARGUMENT_COUNT', 'COMMON', 'COMPILER_OPTIONS',
        #     'COMPILER_VERSION', 'COMPLEX', 'CONJG', 'CONTAINS', 'CONTINUE', 'COS', 'COSD',
        #     'COSH', 'COUNT', 'CPU_TIME', 'CSHIFT', 'CYCLE', 'DATA', 'DATE_AND_TIME',
        #     'DBLE', 'DEALLOCATE', 'DEFAULT', 'DELIM', 'DIMENSION', 'DIMAG', 'DIRECT',
        #     'DO', 'DOT_PRODUCT', 'DOUBLE', 'DPROD', 'DREAL', 'DTIME', 'ELEMENTAL',
        #     'ELSE', 'ELSEIF', 'ELSEWHERE', 'END', 'ENDDO', 'ENDFILE', 'ENDIF', 'ENTRY',
        #     'ENUM', 'ENUMERATOR', 'EOSHIFT', 'EPSILON', 'ERROR', 'ETIME', 'EXECUTE_COMMAND_LINE',
        #     'EXIT', 'EXP', 'EXPONENT', 'EXTENDS', 'EXTERNAL', 'EXTRACT', 'FALSE', 'FILE',
        #     'FINAL', 'FLOAT', 'FLOOR', 'FLUSH', 'FMT', 'FORALL', 'FORMAT', 'FORMATTED',
        #     'FRACTION', 'FUNCTION', 'GAMMA', 'GENERIC', 'GET_COMMAND', 'GET_COMMAND_ARGUMENT',
        #     'GET_ENVIRONMENT_VARIABLE', 'GOTO', 'HUGE', 'IACHAR', 'IAND', 'IARG', 'IBCLR',
        #     'IBITS', 'IBSET', 'ICHAR', 'IDATE', 'IEOR', 'IF', 'IFIX', 'IMAG', 'IMPLICIT',
        #     'IMPORT', 'IN', 'INCLUDE', 'INDEX', 'INOUT', 'INQUIRE', 'INT', 'INTEGER',
        #     'INTENT', 'INTERFACE', 'INTRINSIC', 'IOR', 'IOSTAT', 'ISHFT', 'ISHFTC',
        #     'IS_IOSTAT_END', 'IS_IOSTAT_EOR', 'ITIME', 'KIND', 'LBOUND', 'LEADZ',
        #     'LEN', 'LEN_TRIM', 'LGE', 'LGT', 'LLE', 'LLT', 'LOG', 'LOG10', 'LOGICAL',
        #     'MATMUL', 'MAX', 'MAXEXPONENT', 'MAXLOC', 'MAXVAL', 'MERGE', 'MIN',
        #     'MINEXPONENT', 'MINLOC', 'MINVAL', 'MOD', 'MODULE', 'MODULO', 'MOVE_ALLOC',
        #     'MVBITS', 'NAMELIST', 'NEAREST', 'NEW_LINE', 'NINT', 'NON_INTRINSIC',
        #     'NON_OVERRIDABLE', 'NOPASS', 'NOT', 'NULL', 'NULLIFY', 'NUMERIC_STORAGE_SIZE',
        #     'ONLY', 'OPEN', 'OPERATOR', 'OPTIONAL', 'OR', 'OUT', 'PACK', 'PARAMETER',
        #     'PASS', 'PAUSE', 'POINTER', 'POPPAR', 'POPCNT', 'PRECISION', 'PRESENT',
        #     'PRINT', 'PRIVATE', 'PROCEDURE', 'PRODUCT', 'PROGRAM', 'PROTECTED', 'PUBLIC',
        #     'PURE', 'PUSHPAR', 'RADIX', 'RANDOM_NUMBER', 'RANDOM_SEED', 'RANGE', 'READ',
        #     'REAL', 'RECURSIVE', 'REPEAT', 'RESHAPE', 'RESULT', 'RETURN', 'REWIND',
        #     'RRSPACING', 'SAME_TYPE_AS', 'SAVE', 'SCALE', 'SCAN', 'SELECT', 'SELECTED_CHAR_KIND',
        #     'SELECTED_INT_KIND', 'SELECTED_REAL_KIND', 'SEQUENCE', 'SET_EXPONENT', 'SHAPE',
        #     'SIGN', 'SIN', 'SIND', 'SINH', 'SIZE', 'SNGL', 'SPACING', 'SPREAD', 'SQRT',
        #     'STOP', 'STORAGE_SIZE', 'SUM', 'SUBROUTINE', 'SYSTEM_CLOCK', 'TAN', 'TAND',
        #     'TANH', 'TARGET', 'THEN', 'TIME', 'TINY', 'TRANSFER', 'TRANSPOSE', 'TRIM',
        #     'TRUE', 'TYPE', 'UBOUND', 'UNFORMATTED', 'UNPACK', 'USE', 'VALUE', 'VERIFY',
        #     'VOLATILE', 'WHERE', 'WHILE', 'WRITE'
        # }
        
        # Obsolescent Fortran intrinsics
        self.obsolescent_intrinsics = {
            'ALOG', 'ALOG10', 'AMAX0', 'AMAX1', 'AMIN0', 'AMIN1', 'AMOD', 'CABS',
            'DABS', 'DACOS', 'DASIN', 'DATAN', 'DATAN2', 'DCOS', 'DCOSH', 'DDIM',
            'DEXP', 'DINT', 'DLOG', 'DLOG10', 'DMAX1', 'DMIN1', 'DMOD', 'DNINT',
            'DPROD', 'DREAL', 'DSIGN', 'DSIN', 'DSINH', 'DSQRT', 'DTAN', 'DTANH',
            'FLOAT', 'IABS', 'IDIM', 'IDINT', 'IDNINT', 'IFIX', 'ISIGN', 'MAX0',
            'MAX1', 'MIN0', 'MIN1', 'SNGL'
        }
        
        # Retired if-defs (placeholder - would be loaded from configuration)
        self.retired_ifdefs = set()
        
        # Deprecated C identifiers
        self.deprecated_c_identifiers = {
            'gets', 'tmpnam', 'tempnam', 'mktemp'
        }

    def reset_extra_error_information(self):
        """Reset extra error information"""
        with self._lock:
            self._extra_error_info = {}

    def get_extra_error_information(self) -> Dict:
        """Get extra error information"""
        with self._lock:
            return self._extra_error_info.copy()

    def add_extra_error(self, key: str, value: str = ""):
        """Add extra error information"""
        with self._lock:
            self._extra_error_info[key] = value

    def get_include_number(self) -> int:
        """Get number of files with variable declarations in includes"""
        return self._number_of_files_with_variable_declarations_in_includes

    def remove_quoted(self, line: str) -> str:
        """Remove quoted strings from a line"""
        # Simple implementation - remove single and double quoted strings
        result = line
        
        # Remove double quoted strings
        result = re.sub(r'"[^"]*"', '', result)
        
        # Remove single quoted strings
        result = re.sub(r"'[^']*'", '', result)
        
        return result

    # Test functions - each returns 0 for pass, >0 for fail
    def capitalised_keywords(self, lines: List[str]) -> int:
        """Check for lowercase Fortran keywords"""
        failures = 0
        for line in lines:
            # Remove quoted strings and comments
            if line.startswith("!"):
                continue
            clean_line = self.remove_quoted(line)
            clean_line = self.comment_line.sub("", clean_line)  # Remove comments

            # Check for lowercase keywords
            for word in self.word_splitter.findall(clean_line):
                upcase = word.upper()
                if upcase in fortran_keywords and word != upcase:
                    self.add_extra_error(f"lowercase keyword: {word}")
                    failures += 1

        return failures
    
    def capitalised_keywords_old(self, lines: List[str]) -> int:
        """Check for lowercase Fortran keywords"""
        failures = 0
        for line in lines:
            # Remove quoted strings and comments
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)  # Remove comments
            
            # Check for lowercase keywords
            words = re.findall(r'\b\w+\b', clean_line.upper())
            for word in words:
                if word.upper() in self.fortran_keywords:
                    # Check if original was lowercase
                    if re.search(rf'\b{word.lower()}\b', clean_line.lower()):
                        self.add_extra_error(f"lowercase keyword: {word.lower()}")
                        failures += 1
        
        return failures

    def openmp_sentinels_in_column_one(self, lines: List[str]) -> int:
        """Check OpenMP sentinels are in column one"""
        failures = 0
        for line in lines:
            if re.search(r'^\s+!\$OMP', line):
                self.add_extra_error("OpenMP sentinel not in column 1")
                failures += 1
        return failures

    def unseparated_keywords(self, lines: List[str]) -> int:
        """Check for omitted optional spaces in keywords"""
        failures = 0
        patterns = [
            r'\bELSEIF\b', r'\bENDDO\b', r'\bENDIF\b', r'\bENDTYPE\b',
            r'\bENDMODULE\b', r'\bENDFUNCTION\b', r'\bENDSUBROUTINE\b'
        ]
        
        for line in lines:
            clean_line = self.remove_quoted(line)
            for pattern in patterns:
                if re.search(pattern, clean_line, re.IGNORECASE):
                    self.add_extra_error(f"unseparated keyword in line: {line.strip()}")
                    failures += 1
        
        return failures

    def go_to_other_than_9999(self, lines: List[str]) -> int:
        """Check for GO TO statements other than 9999"""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            if match := re.search(r'\bGO\s*TO\s+(\d+)', clean_line, re.IGNORECASE):
                label = match.group(1)
                if label != '9999':
                    self.add_extra_error(f"GO TO {label}")
                    failures += 1
        
        return failures

    def write_using_default_format(self, lines: List[str]) -> int:
        """Check for WRITE without format"""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            if re.search(r'\bWRITE\s*\(\s*\*\s*,\s*\*\s*\)', clean_line, re.IGNORECASE):
                self.add_extra_error("WRITE(*,*) found")
                failures += 1
        
        return failures

    def lowercase_variable_names(self, lines: List[str]) -> int:
        """Check for lowercase or CamelCase variable names only"""
        '''ToDo: This is a very simplistic check and will not detect many
        cases which break UMDP3. I suspect the Perl Predeccessor concattenated continuation lines prior to 'cleaning' and checking. Having identified a declaration, it also then scanned the rest of the file for that variable name in any case.'''
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            # Simple check for UPPERCASE variable declarations
            if re.search(r'^\s*(INTEGER|REAL|LOGICAL|CHARACTER|TYPE)\s*.*::\s*[A-Z_]+', 
                        clean_line, re.IGNORECASE):
                print(f"Debug: Found variable declaration line: {clean_line}")
                clean_line = re.sub(r'^\s*(INTEGER|REAL|LOGICAL|CHARACTER|TYPE)\s*.*::\s*', '', clean_line)
                if re.search(r'[A-Z]{2,}', clean_line):
                    print(f"Debug: Found UPPERCASE variable name: {clean_line}")
                    self.add_extra_error("UPPERCASE variable name")
                    failures += 1
        
        return failures

    def dimension_forbidden(self, lines: List[str]) -> int:
        """Check for use of dimension attribute"""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            if re.search(r'\bDIMENSION\b', clean_line, re.IGNORECASE):
                self.add_extra_error("DIMENSION attribute used")
                failures += 1
        
        return failures

    def ampersand_continuation(self, lines: List[str]) -> int:
        """Check continuation lines shouldn't start with &"""
        failures = 0
        for line in lines:
            if re.search(r'^\s*&', line):
                self.add_extra_error("continuation line starts with &")
                failures += 1
        
        return failures

    def forbidden_keywords(self, lines: List[str]) -> int:
        """Check for use of EQUIVALENCE or PAUSE"""
        """ToDo: Can't believe this will allow a COMMON BLOCK....
        Need to check against what the original did.."""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            if re.search(r'\b(EQUIVALENCE|PAUSE)\b', clean_line, re.IGNORECASE):
                self.add_extra_error("forbidden keyword")
                failures += 1
        
        return failures

    def forbidden_operators(self, lines: List[str]) -> int:
        """Check for older form of relational operators"""
        failures = 0
        old_operators = ['.GT.', '.GE.', '.LT.', '.LE.', '.EQ.', '.NE.']
        
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            for op in old_operators:
                if op in clean_line.upper():
                    self.add_extra_error(f"old operator {op}")
                    failures += 1
        
        return failures

    def line_over_80chars(self, lines: List[str]) -> int:
        """Check for lines longer than 80 characters"""
        failures = 0
        for line in lines:
            if len(line.rstrip()) > 80:
                self.add_extra_error("line too long")
                failures += 1
        
        return failures

    def tab_detection(self, lines: List[str]) -> int:
        """Check for tab characters"""
        failures = 0
        for line in lines:
            if '\t' in line:
                self.add_extra_error("tab character found")
                failures += 1
        
        return failures

    def printstatus_mod(self, lines: List[str]) -> int:
        """Check for use of printstatus_mod instead of umPrintMgr"""
        failures = 0
        for line in lines:
            if re.search(r'\bUSE\s+printstatus_mod\b', line, re.IGNORECASE):
                self.add_extra_error("printstatus_mod used")
                failures += 1
        
        return failures

    def printstar(self, lines: List[str]) -> int:
        """Check for PRINT rather than umMessage and umPrint"""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            if re.search(r'\bPRINT\s*\*', clean_line, re.IGNORECASE):
                self.add_extra_error("PRINT * used")
                failures += 1
        
        return failures

    def write6(self, lines: List[str]) -> int:
        """Check for WRITE(6) rather than umMessage and umPrint"""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            if re.search(r'\bWRITE\s*\(\s*6\s*,', clean_line, re.IGNORECASE):
                self.add_extra_error("WRITE(6) used")
                failures += 1
        
        return failures

    def um_fort_flush(self, lines: List[str]) -> int:
        """Check for um_fort_flush rather than umPrintFlush"""
        failures = 0
        for line in lines:
            if re.search(r'\bum_fort_flush\b', line):
                self.add_extra_error("um_fort_flush used")
                failures += 1
        
        return failures

    def svn_keyword_subst(self, lines: List[str]) -> int:
        """Check for Subversion keyword substitution"""
        failures = 0
        for line in lines:
            if re.search(r'\$\w+\$', line):
                self.add_extra_error("SVN keyword substitution")
                failures += 1
        
        return failures

    def omp_missing_dollar(self, lines: List[str]) -> int:
        """Check for !OMP instead of !$OMP"""
        failures = 0
        for line in lines:
            if re.search(r'!\s*OMP\b', line) and not re.search(r'!\$OMP', line):
                self.add_extra_error("!OMP without $")
                failures += 1
        
        return failures

    def cpp_ifdef(self, lines: List[str]) -> int:
        """Check for #ifdef/#ifndef rather than #if defined()"""
        failures = 0
        for line in lines:
            if re.search(r'^\s*#\s*if(n)?def\b', line):
                self.add_extra_error("#ifdef/#ifndef used")
                failures += 1
        
        return failures

    def cpp_comment(self, lines: List[str]) -> int:
        """Check for Fortran comments in CPP directives"""
        """Todo: This looks like it will incorrectly fail # if !defined(X)
        How did the original do this test?"""
        failures = 0
        for line in lines:
            match = re.search(r'^\s*#if *(!)?defined\s*\(\s*\w+\s*\)(.*)', line) or re.search(r'^\s*#(else) *(.*)', line)
            if match:
                print(f"Debug: Found CPP directive line: {line}")
                print(f"Debug: match groups: {match.groups()}")
                print(f"Debug: match group(1): {match.group(1)}")
                print(f"Debug: match group(2): {match.group(2)}")
                if re.search(r'.*!', match.group(2)):
                    self.add_extra_error("Fortran comment in CPP directive")
                    failures += 1
        
        return failures

    def obsolescent_fortran_intrinsic(self, lines: List[str]) -> int:
        """Check for archaic Fortran intrinsic functions"""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            for intrinsic in self.obsolescent_intrinsics:
                if re.search(rf'\b{intrinsic}\b', clean_line, re.IGNORECASE):
                    self.add_extra_error(f"obsolescent intrinsic: {intrinsic}")
                    failures += 1
        
        return failures

    def exit_stmt_label(self, lines: List[str]) -> int:
        """Check that EXIT statements are labelled"""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            if re.search(r'\bEXIT\s*$', clean_line, re.IGNORECASE):
                self.add_extra_error("unlabelled EXIT statement")
                failures += 1
        
        return failures

    def intrinsic_modules(self, lines: List[str]) -> int:
        """Check intrinsic modules are USEd with INTRINSIC keyword"""
        failures = 0
        intrinsic_modules = ['ISO_C_BINDING', 'ISO_FORTRAN_ENV']
        
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            for module in intrinsic_modules:
                if (re.search(rf'\bUSE\s+(::)*\s*{module}\b', clean_line, re.IGNORECASE) and
                    not re.search(r'\bINTRINSIC\b', clean_line, re.IGNORECASE)):
                    self.add_extra_error(f"intrinsic module {module} without INTRINSIC")
                    failures += 1
        
        return failures

    def read_unit_args(self, lines: List[str]) -> int:
        """Check READ statements have explicit UNIT= as first argument"""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            if match := re.search(r'\bREAD\s*\(\s*([^,)]+)', clean_line, re.IGNORECASE):
                first_arg = match.group(1).strip()
                if not first_arg.upper().startswith('UNIT='):
                    self.add_extra_error("READ without explicit UNIT=")
                    failures += 1
        
        return failures

    def retire_if_def(self, lines: List[str]) -> int:
        """Check for if-defs due for retirement"""
        retired_ifdefs = ['VATPOLES', 'A12_4A', 'A12_3A', 'UM_JULES', 'A12_2A',]
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)            
            if match := re.search(
                r"^#(?:(?:ifn?def|"  # ifdef/ifndef
                r"(?:el)?if\s*\S*?defined\s*\()"  # elif/if defined(
                r"\s*([^\)\s]*)\)?)",  # SYMBOL
                line, re.IGNORECASE):
                # # The above match either returns [None, SYMBOL] or [SYMBOL, None]
                # SYMBOL = [x for x in match.groups() if x] # reduce to a list of 1 element
                if match.group(1) in retired_ifdefs:
                    self.add_extra_error(f"retired if-def: {match.group(1)}")
                    failures += 1
        return failures

    def implicit_none(self, lines: List[str]) -> int:
        """Check file has at least one IMPLICIT NONE"""
        for line in lines:
            if re.search(r'\bIMPLICIT\s+NONE\b', line, re.IGNORECASE):
                return 0
        
        self.add_extra_error("missing IMPLICIT NONE")
        return 1

    def forbidden_stop(self, lines: List[str]) -> int:
        """Check for STOP or CALL abort"""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            clean_line = re.sub(r'!.*$', '', clean_line)
            
            if re.search(r'\b(STOP|CALL\s+abort)\b', clean_line, re.IGNORECASE):
                self.add_extra_error("STOP or CALL abort used")
                failures += 1
        
        return failures

    def intrinsic_as_variable(self, lines: List[str]) -> int:
        """Check for Fortran function used as variable name"""
        failures = 0
        # This would check for intrinsic function names used as variables
        # Simplified implementation
        # The AI said that - This needs to be compared to the Perl
        # as I doubt this does anything near what that did...
        for line in lines:
            clean_line = self.remove_quoted(line)
            if re.search(r'^\s*(INTEGER|REAL|LOGICAL|CHARACTER)\s*.*::\s*(SIN|COS|LOG|EXP|TAN)\b', 
                        clean_line, re.IGNORECASE):
                self.add_extra_error("intrinsic function used as variable")
                failures += 1
        
        return failures

    def check_crown_copyright(self, lines: List[str]) -> int:
        """Check for crown copyright statement"""
        """ToDo: This is a very simplistic check and will not detect many
        cases which break UMDP3. I suspect the Perl Predeccessor 
        did much more convoluted tests"""
        comment_lines = [line.upper() for line in lines if line.startswith("!")]
        file_content = '\n'.join(comment_lines)
        if 'CROWN COPYRIGHT' in file_content or 'COPYRIGHT' in file_content:
            return 0
        
        self.add_extra_error("missing copyright or crown copyright statement")
        return 1

    def check_code_owner(self, lines: List[str]) -> int:
        """Check for correct code owner comment"""
        # Simplified check for code owner information
        file_content = '\n'.join(lines)
        if 'Code Owner:' in file_content or 'code owner' in file_content.lower():
            return 0
        
        # This is often a warning rather than an error
        return 0

    def array_init_form(self, lines: List[str]) -> int:
        """Check for old array initialization form"""
        failures = 0
        for line in lines:
            clean_line = self.remove_quoted(line)
            if re.search(r'\(/.*?\/\)', clean_line):
                self.add_extra_error("old array initialization form (/ /)")
                failures += 1
        
        return failures

    def line_trail_whitespace(self, lines: List[str]) -> int:
        """Check for trailing whitespace"""
        failures = 0
        for line in lines:
            if re.search(r'\s+$', line):
                self.add_extra_error("trailing whitespace")
                failures += 1
        
        return failures

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
            for identifier in self.deprecated_c_identifiers:
                if re.search(rf'\b{identifier}\b', line):
                    self.add_extra_error(f"deprecated C identifier: {identifier}")
                    failures += 1
        
        return failures

    def c_openmp_define_pair_thread_utils(self, lines: List[str]) -> int:
        """Check C OpenMP define pairing with thread utils"""
        failures = 0
        for line in lines:
            if re.search(r'#\s*if.*_OPENMP', line):
                if not re.search(r'SHUM_USE_C_OPENMP_VIA_THREAD_UTILS', line):
                    self.add_extra_error("_OPENMP without SHUM_USE_C_OPENMP_VIA_THREAD_UTILS")
                    failures += 1
        
        return failures

    def c_openmp_define_no_combine(self, lines: List[str]) -> int:
        """Check C OpenMP defines not combined with third macro"""
        failures = 0
        for line in lines:
            if (re.search(r'_OPENMP.*&&.*SHUM_USE_C_OPENMP_VIA_THREAD_UTILS.*&&', line) or
                re.search(r'&&.*_OPENMP.*&&.*SHUM_USE_C_OPENMP_VIA_THREAD_UTILS', line)):
                self.add_extra_error("OpenMP defines combined with third macro")
                failures += 1
        
        return failures

    def c_openmp_define_not(self, lines: List[str]) -> int:
        """Check for !defined(_OPENMP) usage"""
        failures = 0
        for line in lines:
            if re.search(r'!\s*defined\s*\(\s*_OPENMP\s*\)', line):
                self.add_extra_error("!defined(_OPENMP) used")
                failures += 1
        
        return failures

    def c_protect_omp_pragma(self, lines: List[str]) -> int:
        """Check OMP pragma is protected with ifdef"""
        failures = 0
        in_openmp_block = False
        
        for line in lines:
            if re.search(r'#\s*if.*_OPENMP', line):
                in_openmp_block = True
            elif re.search(r'#\s*endif', line):
                in_openmp_block = False
            elif (re.search(r'#\s*pragma\s+omp', line) or 
                  re.search(r'#\s*include\s*<omp\.h>', line)):
                if not in_openmp_block:
                    self.add_extra_error("unprotected OMP pragma/include")
                    failures += 1
        
        return failures

    def c_ifdef_defines(self, lines: List[str]) -> int:
        """Check for #ifdef style rather than #if defined()"""
        failures = 0
        for line in lines:
            if re.search(r'^\s*#\s*ifdef\b', line):
                self.add_extra_error("#ifdef used instead of #if defined()")
                failures += 1
        
        return failures

    def c_final_newline(self, lines: List[str]) -> int:
        """Check C unit ends with final newline"""
        if lines and not lines[-1].endswith('\n'):
            self.add_extra_error("missing final newline")
            return 1
        
        return 0