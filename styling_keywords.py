# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# Created date: 10/03/2025
# Modified date: 14/03/2025
# *****************************COPYRIGHT*******************************


KEYWORDS = {"abort", "abs", "abstract", "access", "achar", "acos", "acosd", "acosh", "action", "adjustl", "adjustr",
            "advance", "aimag", "aint", "alarm", "algama", "all", "allocatable", "allocate", "allocated", "alog",
            "alog10", "amax0", "amax1", "amin0", "amin1", "amod", "and", "anint", "any", "asin", "asind", "asinh",
            "assign", "assignment", "associate", "associated", "asynchronous", "atan", "atan2", "atan2d", "atand",
            "atanh", "atomic", "atomic_add", "atomic_and", "atomic_cas", "atomic_define", "atomic_fetch_add",
            "atomic_fetch_and", "atomic_fetch_or", "atomic_fetch_xor", "atomic_int_kind", "atomic_logical_kind",
            "atomic_or", "atomic_ref", "atomic_xor", "backspace", "backtrace", "barrier", "besj0", "besj1", "besjn",
            "bessel_j0", "bessel_j1", "bessel_jn", "bessel_y0", "bessel_y1", "bessel_yn", "besy0", "besy1", "besyn",
            "bge", "bgt", "bind", "bit_size", "blank", "ble", "block", "blt", "btest", "c_alert", "c_associated",
            "c_backspace", "c_bool", "c_carriage_return", "c_char", "c_double", "c_double_complex", "c_f_pointer",
            "c_f_procpointer", "c_float", "c_float128", "c_float128_complex", "c_float_complex", "c_form_feed",
            "c_funloc", "c_funptr", "c_horizontal_tab", "c_int", "c_int128_t", "c_int16_t", "c_int32_t", "c_int64_t",
            "c_int8_t", "c_int_fast128_t", "c_int_fast16_t", "c_int_fast32_t", "c_int_fast64_t", "c_int_fast8_t",
            "c_int_least128_t", "c_int_least16_t", "c_int_least32_t", "c_int_least64_t", "c_int_least8_t", "c_intmax_t",
            "c_intptr_t", "c_loc", "c_long", "c_long_double", "c_long_double_complex", "c_long_long", "c_new_line",
            "c_null_char", "c_null_funptr", "c_null_ptr", "c_ptr", "c_ptrdiff_t", "c_short", "c_signed_char",
            "c_size_t", "c_sizeof", "c_vertical_tab", "cabs", "call", "case", "ccos", "cdabs", "cdcos", "cdexp",
            "cdlog", "cdsin", "cdsqrt", "ceiling", "cexp", "char", "character", "character_kinds",
            "character_storage_size", "chdir", "chmod", "class", "clog", "close", "cmplx", "co_broadcast", "co_max",
            "co_min", "co_reduce", "co_sum", "codimension", "command_argument_count", "common", "compiler_options",
            "compiler_version", "complex", "concurrent", "conjg", "contains", "contiguous", "continue", "convert",
            "copyin", "copyprivate", "cos", "cosd", "cosh", "cotan", "cotand", "count", "cpp", "cpu_time", "cqabs",
            "cqcos", "cqexp", "cqlog", "cqsin", "cqsqrt", "critical", "cshift", "csin", "csqrt", "ctime", "cycle",
            "dabs", "dacos", "dacosh", "dasin", "dasinh", "data", "datan", "datan2", "datanh", "date_and_time",
            "dbesj0", "dbesj1", "dbesjn", "dbesy0", "dbesy1", "dbesyn", "dble", "dcmplx", "dconjg", "dcos", "dcosh",
            "ddim", "deallocate", "decode", "default", "deferred", "delim", "derf", "derfc", "dexp", "dfloat", "dgamma",
            "digits", "dim", "dimag", "dimension", "dint", "direct", "dlgama", "dlog", "dlog10", "dmax1", "dmin1",
            "dmod", "dnint", "do", "dot_product", "double", "dprod", "dreal", "dshiftl", "dshiftr", "dsign", "dsin",
            "dsinh", "dsqrt", "dtan", "dtanh", "dtime", "elemental", "else", "elsewhere", "encode", "end", "endfile",
            "entry", "enum", "enumerator", "eor", "eoshift", "epsilon", "equivalence", "eqv", "erf", "erfc",
            "erfc_scaled", "errmsg", "error", "error_unit", "etime", "event_query", "execute_command_line", "exist",
            "exit", "exp", "exponent", "extends", "extends_type_of", "external", "false", "fdate", "fget", "fgetc",
            "file", "file_storage_size", "final", "firstprivate", "float", "floor", "flush", "fmt", "fnum", "forall",
            "form", "format", "formatted", "fpp", "fput", "fputc", "fraction", "free", "fseek", "fstat", "ftell",
            "function", "gamma", "generic", "gerror", "get_command", "get_command_argument", "get_environment_variable",
            "getarg", "getcwd", "getenv", "getgid", "getlog", "getpid", "getuid", "gmtime", "go", "hostnm", "huge",
            "hypot", "iabs", "iachar", "iall", "iand", "iany", "iargc", "ibclr", "ibits", "ibset", "ichar", "idate",
            "idim", "idint", "idnint", "ieee_class", "ieee_class_type", "ieee_copy_sign", "ieee_is_finite",
            "ieee_is_nan", "ieee_is_negative", "ieee_is_normal", "ieee_logb", "ieee_negative_denormal",
            "ieee_negative_inf", "ieee_negative_normal", "ieee_negative_zero", "ieee_next_after",
            "ieee_positive_denormal", "ieee_positive_inf", "ieee_positive_normal", "ieee_positive_zero",
            "ieee_quiet_nan", "ieee_rem", "ieee_rint", "ieee_scalb", "ieee_selected_real_kind", "ieee_signaling_nan",
            "ieee_support_datatype", "ieee_support_denormal", "ieee_support_divide", "ieee_support_inf",
            "ieee_support_nan", "ieee_support_sqrt", "ieee_support_standard", "ieee_unordered", "ieee_value", "ieor",
            "ierrno", "if", "ifix", "imag", "image_index", "images", "imagpart", "implicit", "import", "in", "include",
            "index", "inout", "input_unit", "inquire", "int", "int16", "int2", "int32", "int64", "int8", "integer",
            "integer_kinds", "intent", "interface", "intrinsic", "iomsg", "ior", "iostat", "iostat_end", "iostat_eor",
            "iostat_inquire_internal_unit", "iparity", "iqint", "irand", "is", "is_iostat_end", "is_iostat_eor",
            "isatty", "ishft", "ishftc", "isign", "isnan", "iso_c_binding", "iso_fortran_env", "itime", "kill", "kind",
            "lastprivate", "lbound", "lcobound", "leadz", "len", "len_trim", "lgamma", "lge", "lgt", "link", "lle",
            "llt", "lnblnk", "loc", "lock", "lock_type", "log", "log10", "log_gamma", "logical", "logical_kinds",
            "long", "lshift", "lstat", "ltime", "malloc", "maskl", "maskr", "master", "matmul", "max", "max0", "max1",
            "maxexponent", "maxloc", "maxval", "mclock", "mclock8", "memory", "merge", "merge_bits", "min", "min0",
            "min1", "minexponent", "minloc", "minval", "mod", "module", "modulo", "move_alloc", "mvbits", "name",
            "named", "namelist", "nearest", "neqv", "new_line", "nextrec", "nint", "nml", "non_intrinsic",
            "non_overridable", "none", "nopass", "norm2", "not", "null", "nullify", "num_images", "number",
            "numeric_storage_size", "only", "open", "opened", "operator", "optional", "or", "ordered", "out",
            "output_unit", "pack", "pad", "parallel", "parameter", "parity", "pass", "perror", "pointer", "popcnt",
            "poppar", "position", "precision", "present", "print", "private", "procedure", "product", "program",
            "protected", "public", "pure", "qabs", "qacos", "qasin", "qatan", "qatan2", "qcmplx", "qconjg", "qcos",
            "qcosh", "qdim", "qerf", "qerfc", "qexp", "qgamma", "qimag", "qlgama", "qlog", "qlog10", "qmax1", "qmin1",
            "qmod", "qnint", "qsign", "qsin", "qsinh", "qsqrt", "qtan", "qtanh", "radix", "ran", "rand",
            "random_number", "random_seed", "range", "rank", "read", "readwrite", "real", "real128", "real32", "real64",
            "real_kinds", "realpart", "rec", "recl", "record", "recursive", "reduction", "rename", "repeat", "reshape",
            "result", "return", "rewind", "rewrite", "rrspacing", "rshift", "same_type_as", "save", "scale", "scan",
            "secnds", "second", "sections", "select", "selected_char_kind", "selected_int_kind", "selected_real_kind",
            "sequence", "sequential", "set_exponent", "shape", "shared", "shifta", "shiftl", "shiftr", "short", "sign",
            "signal", "sin", "sind", "sinh", "size", "sizeof", "sleep", "sngl", "source", "spacing", "spread", "sqrt",
            "srand", "stat", "stat_failed_image", "stat_locked", "stat_locked_other_image", "stat_stopped_image",
            "stat_unlocked", "status", "stop", "storage_size", "structure", "submodule", "subroutine", "sum", "symlnk",
            "sync", "system", "system_clock", "tan", "tand", "tanh", "target", "task", "taskwait", "then", "this_image",
            "threadprivate", "time", "time8", "tiny", "to", "trailz", "transfer", "transpose", "trim", "true", "ttynam",
            "type", "ubound", "ucobound", "umask", "unformatted", "unit", "unlink", "unlock", "unpack", "use", "value",
            "verif", "verify", "volatile", "wait", "where", "while", "workshare", "write", "xor", "zabs", "zcos",
            "zexp", "zlog", "zsin", "zsqrt"}

CODE_REPLACEMENTS = [
    # Replace Fortran 77 style conditional keywords
    (r'\.eq\.', ' == '),
    (r'\.ne\.', ' /= '),
    (r'\.gt\.', ' >  '),
    (r'\.lt\.', ' <  '),
    (r'\.ge\.', ' >= '),
    (r'\.le\.', ' <= '),
    # protect 'operator' definitions e.g. "OPERATOR(/)", from the array
    # initiialisation enforcement by enforcing spaces around the operators.
    # Make all operators follow the same style but only (/) and (/=) had issues.
    (r'\(\s*\*\s*\)', '( * )'),
    (r'\(\s*\+\s*\)', '( + )'),
    (r'\(\s*-\s*\)', '( - )'),
    (r'\(\s*\/\s*\)', '( / )'),
    (r'\(\s*==\s*\)', '( == )'),
    (r'\(\s*\/=\s*\)', '( /= )'),
    (r'\(\s*<\s*\)', '( < )'),
    (r'\(\s*<=\s*\)', '( <= )'),
    (r'\(\s*>\s*\)', '( > )'),
    (r'\(\s*>=\s*\)', '( >= )'),
    # Replace array initialisations
    (r'\(\/', '['),
    (r'\/\)', ']'),
    # Ensure remaining comparitive logicals have spaces either side
    (r'([^\s])(?<!\()\.not\.', r'\g<1> .not.'),
    (r'\.not\.([^\s])', r'.not. \g<1>'),
    (r'([^\s])\.and\.', r'\g<1> .and.'),
    (r'\.and\.([^\s])', r'.and. \g<1>'),
    (r'([^\s])\.or\.', r'\g<1> .or.'),
    (r'\.or\.([^\s])', r'.or. \g<1>'),
    (r'([^\s])\.eqv\.', r'\g<1> .eqv.'),
    (r'\.eqv\.([^\s])', r'.eqv. \g<1>'),
    (r'([^\s])\.neqv\.', r'\g<1> .neqv.'),
    (r'\.neqv\.([^\s])', r'.neqv. \g<1>'),
    # Ensure hard-coded real numbers have a zero after the decimal point
    (r'([0-9])\.([^0-9]|$)', r'\g<1>.0\g<2>'),
    # Remove start of line ampersands, without changing the spacing of
    # the line they appear on
    (r'^(\s*)&(.*\w.*)$', r'\g<1> \g<2>'),
    # Make constructs which include brackets have exactly one space
    # between the construct and the bracket character
    (r'(^\s*)(\w+\s*:\s*|[0-9]+\s*|)((else|)\s*if)(|\s\s+)\(',
     r'\g<1>\g<2>\g<3> ('),
    (r'(^\s*)(\w+\s*:\s*|[0-9]+\s*|)where(|\s\s+)\(', r'\g<1>\g<2>where ('),
    (r'(^\s*)case(|\s\s+)\(', r'\g<1>case ('),
    (r'\)(|\s\s+)then(\W|$)', r') then\g<1>'),
    # Make intent statements contain no extra spacing inside the brackets
    (r'(.*intent\s*)\(\s*in\s*\)(.*)', r'\g<1>(in)\g<2>'),
    (r'(.*intent\s*)\(\s*out\s*\)(.*)', r'\g<1>(out)\g<2>'),
    (r'(.*intent\s*)\(\s*in out\s*\)(.*)', r'\g<1>(in out)\g<2>'),
    # Make module USE, ONLY statments have exactly no space between the ONLY
    # and the colon character after it
    (r'^(\s*)use(\s*,\s*\w+\s*::|)(\s+\w+\s*,\s*)only\s*:(.*)$',
     r'\g<1>use\g<2>\g<3>only:\g<4>'),
]

COMMENT_REPLACEMENTS = [
    # DEPENDS ON fcm constructions
    (r'^(\s*!)\s*depends\s*on\s*:\s*', r'\g<1> DEPENDS ON: '),
]

FORTRAN_TYPES = [
    "CHARACTER",
    "CLASS",
    "COMPLEX",
    "DOUBLE PRECISION",
    "ENUMERATOR",
    "INTEGER",
    "LOGICAL",
    "REAL",
    "TYPE",
]
