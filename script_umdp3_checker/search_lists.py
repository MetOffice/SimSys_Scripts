# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
Lists of words for Fortran checks. Some to confirm they are found in the approved form, some to test for as the intention is that they should no longer appear in the code.
"""

# Obsolescent Fortran intrinsics : These should not be used in new code and
# their use in existing code should be reviewed.
obsolescent_intrinsics = {
    "ALOG",
    "ALOG10",
    "AMAX0",
    "AMAX1",
    "AMIN0",
    "AMIN1",
    "AMOD",
    "CABS",
    "CCOS",
    "CEXP",
    "CLOG",
    "CSIN",
    "CSQRT",
    "DABS",
    "DACOS",
    "DASIN",
    "DATAN",
    "DATAN2",
    "DBESJ0",
    "DBESJ1",
    "DBESJN",
    "DBESY0",
    "DBESY1",
    "DBESYN",
    "DCOS",
    "DCOSH",
    "DDIM",
    "DERF",
    "DERFC",
    "DEXP",
    "DINT",
    "DLOG",
    "DLOG10",
    "DMAX1",
    "DMIN1",
    "DMOD",
    "DNINT",
    "DSIGN",
    "DSIN",
    "DSINH",
    "DSQRT",
    "DTAN",
    "DTANH",
    "FLOAT",
    "IABS",
    "IDIM",
    "IDINT",
    "IDNINT",
    "IFIX",
    "ISIGN",
    "LONG",
    "MAX0",
    "MAX1",
    "MIN0",
    "MIN1",
    "SNGL",
    "ZABS",
    "ZCOS",
    "ZEXP",
    "ZLOG",
    "ZSIN",
    "ZSQRT",
}

openmp_keywords = {
    "PARALLEL",
    "MASTER",
    "CRITICAL",
    "ATOMIC",
    "SECTIONS",
    "WORKSHARE",
    "TASK",
    "BARRIER",
    "TASKWAIT",
    "FLUSH",
    "ORDERED",
    "THREADPRIVATE",
    "SHARED",
    "DEFAULT",
    "FIRSTPRIVATE",
    "LASTPRIVATE",
    "COPYIN",
    "COPYPRIVATE",
    "REDUCTION",
}

fortran_types = {
    "TYPE",
    "CLASS",
    "INTEGER",
    "REAL",
    "DOUBLE PRECISION",
    "CHARACTER",
    "LOGICAL",
    "COMPLEX",
    "ENUMERATOR",
}

# These keywords should all appear with the requisite spaces in them
# (i.e. not 'ENDIF' but 'END IF')
unseparated_keywords_list = {
    "BLOCKDATA",
    "DOUBLEPRECISION",
    "ELSEIF",
    "ELSEWHERE",
    "ENDASSOCIATE",
    "ENDBLOCK",
    "ENDBLOCKDATA",
    "ENDCRITICAL",
    "ENDDO",
    "ENDENUM",
    "ENDFILE",
    "ENDFORALL",
    "ENDFUNCTION",
    "ENDIF",
    "ENDINTERFACE",
    "ENDMODULE",
    "ENDPARALLEL",
    "ENDPARALLELDO",
    "ENDPROCEDURE",
    "ENDPROGRAM",
    "ENDSELECT",
    "ENDSUBROUTINE",
    "ENDTYPE",
    "ENDWHERE",
    "GOTO",
    "INOUT",
    "PARALLELDO",
    "SELECTCASE",
    "SELECTTYPE",
}

# Retired if-defs (placeholder - would be loaded from configuration)
retired_ifdefs = set(
    [
        "VATPOLES",
        "A12_4A",
        "A12_3A",
        "UM_JULES",
        "A12_2A",
    ]
)

# Deprecated C identifiers
deprecated_c_identifiers = {"gets", "tmpnam", "tempnam", "mktemp"}
