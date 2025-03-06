import re
import sys
import fstring_parse


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


def upcase_keywords(line, str_continuation):
    '''Upper-case any Fortran keywords on the given line, and down-case any
    all capital words which aren't keywords, returning the result'''

    workline = fstring_parse.clean_str_continuation(line, str_continuation)

    stripline = workline.strip()

    if len(stripline) == 0 or stripline[0] == "!" or stripline[0] == "#":
        return line

    nloop = len(line)

    try:
        simple_line = fstring_parse.blank_fstring(workline)
    except ParsingError as e:
        str_cont_test = fstring_parse.is_str_continuation(workline)
        check_quote = str_cont_test[SQUOTE] or str_cont_test[DQUOTE]
        if check_quote is True:
            simple_line = fstring_parse.partial_blank_fstring(workline)
        else:
            print("keyword split simplify line has failed.")
            print("{0:s} Line simplification has failed for:".format(e.msg))
            print(line)
            exit(1)

    # remove comments
    simple_line = fstring_parse.blank_fcomments(simple_line)

    # Split the line of code into a set of words
    line_words = set(re.findall(r"[\w]+", simple_line))

    for word in line_words:
        # Exclude special "__FILE__" or "__LINE__" directives
        if (word.isupper() and
                not re.match(r"__\w+__", word)):
            recomp = re.compile(r'(^|\b){0:s}(\b|$)'.format(word))
            simple_line = recomp.sub(r'\g<1>{0:s}'
                                     r'\g<2>'.format(word.lower()),
                                     simple_line)

    line_words = set([word.lower() for word in line_words])
    words_to_upcase = list(line_words.intersection(KEYWORDS))
    line = list(line)
    for keyword in words_to_upcase:
        recomp = re.compile(r'(?i)(^|\b){0:s}(\b|$)'.format(keyword))
        simple_line = recomp.sub(r'\g<1>{0:s}\g<2>'.format(keyword.upper()),
                                 simple_line)

    # Now add back any comments/strings
    simple_line = list(simple_line)
    out_line = []

    for i in range(nloop):
        if simple_line[i] == " ":
            out_line.append(line[i])
        else:
            out_line.append(simple_line[i])

    # Join the line back up into a string.
    out_line = "".join(out_line)

    return out_line


def apply_styling(lines):
    '''
    For a lit of lines apply UMDP3 styling to each line and return
    the result
    '''

    output_lines = []
    continuation = False
    pp_continuation = False
    str_continuation = [False, False]
    pseudo_str_continuation = [False, False]
    pseudo_comment = False
    pp_line_previous = ""
    line_previous = ""

    for iline, line in enumerate(lines):

        if pp_continuation:
            if not pseudo_comment:
                line = upcase_keywords(line, pseudo_str_continuation)
        else:
            line = upcase_keywords(line, str_continuation)

        # Check for the start of new pre-processor continuation
        if fstring_parse.is_pp_continuation(line) and not pp_continuation:
            pp_continuation = True

        # test the line continuation properties of this line
        if pp_continuation:
            if not fstring_parse.is_pp_continuation(line):
                pp_continuation = False
                pp_line_previous = ""
                pseudo_comment = False
        elif continuation:
            if fstring_parse.is_continuation(line, str_continuation):
                # check if still string continuation
                str_continuation = fstring_parse.is_str_continuation(line, str_continuation)
            else:
                continuation = False
                line_previous = ""
                str_continuation = [False, False]
        else:
            # Finally, detect if the following line is a continuation
            # of this one (and therefore requires no indentation)
            if fstring_parse.is_continuation(line, str_continuation):
                continuation = True
                str_continuation = fstring_parse.is_str_continuation(line, str_continuation)

        # if we are a (pp) continuation, save the partial line
        if pp_continuation:
            pp_line_previous = ''.join([re.sub(r"\\\s*$", "",
                                               pp_line_previous),
                                        re.sub(r"&\s*$", "", line_previous),
                                        line])
            line_previous = ""
            pseudo_line = re.sub(r"\\\s*$", "&", pp_line_previous)
            pseudo_str_continuation = fstring_parse.is_str_continuation(pseudo_line,
                                                          str_continuation)
            if not pseudo_comment:
                pseudo_line = fstring_parse.partial_blank_fstring(pseudo_line,
                                                    str_continuation)
                if pseudo_line.strip()[0] == "#":
                    pseudo_comment = True
                if pseudo_line.find("!") != -1:
                    pseudo_line = fstring_parse.blank_fcomments(pseudo_line,
                                                  str_continuation)
                    if pseudo_line.find("!") == -1:
                        pseudo_comment = True
        elif continuation:
            line_previous = re.sub(r"&\s*$", "", line_previous)
            line_previous += fstring_parse.blank_fcomments(line, str_continuation)

        output_lines.append(line)

    return output_lines


def main():
    '''Main toplevel function for testing'''
    input_file = sys.argv[-1]
    with open(input_file, "r+") as file_in:
        print("Styling " + input_file)
        lines_in = file_in.read().split("\n")
        new_lines = apply_styling(lines_in)
        file_in.seek(0)
        file_in.write("\n".join(new_lines))
        file_in.truncate()


if __name__ == '__main__':
    main()
