# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

package UMDP3;

# Package to contain subroutines which test for UMDP3 compliance.

# Each subroutine has a standard interface:
# Input:  Array of lines to test
# Output: Scalar value 0=pass, >0 = fail

# Subroutines which don't obey this interface:
#  get_include_number - returns the value of a variable scoped to this file
#                   (number of files using includes for variable declarations)
#  remove_quoted      - returns the input string having removed any quoted
#                        substrings (single or double).

# Standard modules
use strict;
use warnings;
use 5.010;
use Text::Balanced qw(extract_quotelike extract_multiple);

# Declare version - this is the last UM version this script was updated for:
our $VERSION = '13.5.0';

# Global variables

my $number_of_files_with_variable_declarations_in_includes = 0;

sub get_include_number {
    return $number_of_files_with_variable_declarations_in_includes;
}

sub remove_quoted {
    my $line = shift;

    # Replace quoted strings with a blessed reference:
    my @strings = extract_multiple(
        $line,
        [
            {
                Quoted => sub { extract_quotelike( $_[0] ) }
            },
        ]
    );

    # Stitch the non-quoted fields back together into a single string:
    my $remainder = "";
    foreach my $string (@strings) {
        $remainder .= $string if not( $string =~ /^Quoted=SCALAR/sxm );
    }
    return $remainder;
}

my @fortran_keywords = (
    'ABORT',                        'ABS',
    'ABSTRACT',                     'ACCESS',
    'ACHAR',                        'ACOS',
    'ACOSD',                        'ACOSH',
    'ACTION',                       'ADJUSTL',
    'ADJUSTR',                      'ADVANCE',
    'AIMAG',                        'AINT',
    'ALARM',                        'ALGAMA',
    'ALL',                          'ALLOCATABLE',
    'ALLOCATE',                     'ALLOCATED',
    'ALOG',                         'ALOG10',
    'AMAX0',                        'AMAX1',
    'AMIN0',                        'AMIN1',
    'AMOD',                         'AND',
    'ANINT',                        'ANY',
    'ASIN',                         'ASIND',
    'ASINH',                        'ASSIGN',
    'ASSIGNMENT',                   'ASSOCIATE',
    'ASSOCIATED',                   'ASYNCHRONOUS',
    'ATAN',                         'ATAN2',
    'ATAN2D',                       'ATAND',
    'ATANH',                        'ATOMIC_ADD',
    'ATOMIC_AND',                   'ATOMIC_CAS',
    'ATOMIC_DEFINE',                'ATOMIC_FETCH_ADD',
    'ATOMIC_FETCH_AND',             'ATOMIC_FETCH_OR',
    'ATOMIC_FETCH_XOR',             'ATOMIC_INT_KIND',
    'ATOMIC_LOGICAL_KIND',          'ATOMIC_OR',
    'ATOMIC_REF',                   'ATOMIC_XOR',
    'BACKSPACE',                    'BACKTRACE',
    'BESJ0',                        'BESJ1',
    'BESJN',                        'BESSEL_J0',
    'BESSEL_J1',                    'BESSEL_JN',
    'BESSEL_Y0',                    'BESSEL_Y1',
    'BESSEL_YN',                    'BESY0',
    'BESY1',                        'BESYN',
    'BGE',                          'BGT',
    'BIND',                         'BIT_SIZE',
    'BLANK',                        'BLE',
    'BLOCK',                        'BLT',
    'BTEST',                        'CABS',
    'CALL',                         'CASE',
    'CCOS',                         'CDABS',
    'CDCOS',                        'CDEXP',
    'CDLOG',                        'CDSIN',
    'CDSQRT',                       'CEILING',
    'CEXP',                         'CHAR',
    'CHARACTER',                    'CHARACTER_KINDS',
    'CHARACTER_STORAGE_SIZE',       'CHDIR',
    'CHMOD',                        'CLASS',
    'CLOG',                         'CLOSE',
    'CMPLX',                        'CODIMENSION',
    'COMMAND_ARGUMENT_COUNT',       'COMMON',
    'COMPILER_OPTIONS',             'COMPILER_VERSION',
    'COMPLEX',                      'CONCURRENT',
    'CONJG',                        'CONTAINS',
    'CONTIGUOUS',                   'CONTINUE',
    'CONVERT',                      'COS',
    'COSD',                         'COSH',
    'COTAN',                        'COTAND',
    'COUNT',                        'CO_BROADCAST',
    'CO_MAX',                       'CO_MIN',
    'CO_REDUCE',                    'CO_SUM',
    'CPP',                          'CPU_TIME',
    'CQABS',                        'CQCOS',
    'CQEXP',                        'CQLOG',
    'CQSIN',                        'CQSQRT',
    'CSHIFT',                       'CSIN',
    'CSQRT',                        'CTIME',
    'CYCLE',                        'C_ALERT',
    'C_ASSOCIATED',                 'C_BACKSPACE',
    'C_BOOL',                       'C_CARRIAGE_RETURN',
    'C_CHAR',                       'C_DOUBLE',
    'C_DOUBLE_COMPLEX',             'C_FLOAT',
    'C_FLOAT128',                   'C_FLOAT128_COMPLEX',
    'C_FLOAT_COMPLEX',              'C_FORM_FEED',
    'C_FUNLOC',                     'C_FUNPTR',
    'C_F_POINTER',                  'C_F_PROCPOINTER',
    'C_HORIZONTAL_TAB',             'C_INT',
    'C_INT128_T',                   'C_INT16_T',
    'C_INT32_T',                    'C_INT64_T',
    'C_INT8_T',                     'C_INTMAX_T',
    'C_INTPTR_T',                   'C_INT_FAST128_T',
    'C_INT_FAST16_T',               'C_INT_FAST32_T',
    'C_INT_FAST64_T',               'C_INT_FAST8_T',
    'C_INT_LEAST128_T',             'C_INT_LEAST16_T',
    'C_INT_LEAST32_T',              'C_INT_LEAST64_T',
    'C_INT_LEAST8_T',               'C_LOC',
    'C_LONG',                       'C_LONG_DOUBLE',
    'C_LONG_DOUBLE_COMPLEX',        'C_LONG_LONG',
    'C_NEW_LINE',                   'C_NULL_CHAR',
    'C_NULL_FUNPTR',                'C_NULL_PTR',
    'C_PTR',                        'C_PTRDIFF_T',
    'C_SHORT',                      'C_SIGNED_CHAR',
    'C_SIZEOF',                     'C_SIZE_T',
    'C_VERTICAL_TAB',               'DABS',
    'DACOS',                        'DACOSH',
    'DASIN',                        'DASINH',
    'DATA',                         'DATAN',
    'DATAN2',                       'DATANH',
    'DATE_AND_TIME',                'DBESJ0',
    'DBESJ1',                       'DBESJN',
    'DBESY0',                       'DBESY1',
    'DBESYN',                       'DBLE',
    'DCMPLX',                       'DCONJG',
    'DCOS',                         'DCOSH',
    'DDIM',                         'DEALLOCATE',
    'DECODE',                       'DEFERRED',
    'DELIM',                        'DERF',
    'DERFC',                        'DEXP',
    'DFLOAT',                       'DGAMMA',
    'DIGITS',                       'DIM',
    'DIMAG',                        'DIMENSION',
    'DINT',                         'DIRECT',
    'DLGAMA',                       'DLOG',
    'DLOG10',                       'DMAX1',
    'DMIN1',                        'DMOD',
    'DNINT',                        'DO',
    'DOT_PRODUCT',                  'DOUBLE',
    'DPROD',                        'DREAL',
    'DSHIFTL',                      'DSHIFTR',
    'DSIGN',                        'DSIN',
    'DSINH',                        'DSQRT',
    'DTAN',                         'DTANH',
    'DTIME',                        'ELEMENTAL',
    'ELSE',                         'ENCODE',
    'END',                          'ENTRY',
    'ENUM',                         'ENUMERATOR',
    'EOR',                          'EOSHIFT',
    'EPSILON',                      'EQ',
    'EQUIVALENCE',                  'EQV',
    'ERF',                          'ERFC',
    'ERFC_SCALED',                  'ERRMSG',
    'ERROR',                        'ERROR_UNIT',
    'ETIME',                        'EVENT_QUERY',
    'EXECUTE_COMMAND_LINE',         'EXIST',
    'EXIT',                         'EXP',
    'EXPONENT',                     'EXTENDS',
    'EXTENDS_TYPE_OF',              'EXTERNAL',
    'FALSE',                        'FDATE',
    'FGET',                         'FGETC',
    'FILE',                         'FILE_STORAGE_SIZE',
    'FINAL',                        'FLOAT',
    'FLOOR',                        'FLUSH',
    'FMT',                          'FNUM',
    'FORALL',                       'FORM',
    'FORMAT',                       'FORMATTED',
    'FPP',                          'FPUT',
    'FPUTC',                        'FRACTION',
    'FREE',                         'FSEEK',
    'FSTAT',                        'FTELL',
    'FUNCTION',                     'GAMMA',
    'GE',                           'GENERIC',
    'GERROR',                       'GETARG',
    'GETCWD',                       'GETENV',
    'GETGID',                       'GETLOG',
    'GETPID',                       'GETUID',
    'GET_COMMAND',                  'GET_COMMAND_ARGUMENT',
    'GET_ENVIRONMENT_VARIABLE',     'GMTIME',
    'GO',                           'GT',
    'HOSTNM',                       'HUGE',
    'HYPOT',                        'IABS',
    'IACHAR',                       'IALL',
    'IAND',                         'IANY',
    'IARGC',                        'IBCLR',
    'IBITS',                        'IBSET',
    'ICHAR',                        'IDATE',
    'IDIM',                         'IDINT',
    'IDNINT',                       'IEEE_CLASS',
    'IEEE_CLASS_TYPE',              'IEEE_COPY_SIGN',
    'IEEE_IS_FINITE',               'IEEE_IS_NAN',
    'IEEE_IS_NEGATIVE',             'IEEE_IS_NORMAL',
    'IEEE_LOGB',                    'IEEE_NEGATIVE_DENORMAL',
    'IEEE_NEGATIVE_INF',            'IEEE_NEGATIVE_NORMAL',
    'IEEE_NEGATIVE_ZERO',           'IEEE_NEXT_AFTER',
    'IEEE_POSITIVE_DENORMAL',       'IEEE_POSITIVE_INF',
    'IEEE_POSITIVE_NORMAL',         'IEEE_POSITIVE_ZERO',
    'IEEE_QUIET_NAN',               'IEEE_REM',
    'IEEE_RINT',                    'IEEE_SCALB',
    'IEEE_SELECTED_REAL_KIND',      'IEEE_SIGNALING_NAN',
    'IEEE_SUPPORT_DATATYPE',        'IEEE_SUPPORT_DENORMAL',
    'IEEE_SUPPORT_DIVIDE',          'IEEE_SUPPORT_INF',
    'IEEE_SUPPORT_NAN',             'IEEE_SUPPORT_SQRT',
    'IEEE_SUPPORT_STANDARD',        'IEEE_UNORDERED',
    'IEEE_VALUE',                   'IEOR',
    'IERRNO',                       'IF',
    'IFIX',                         'IMAG',
    'IMAGES',                       'IMAGE_INDEX',
    'IMAGPART',                     'IMPLICIT',
    'IMPORT',                       'IN',
    'INCLUDE',                      'INDEX',
    'INPUT_UNIT',                   'INQUIRE',
    'INT',                          'INT16',
    'INT2',                         'INT32',
    'INT64',                        'INT8',
    'INTEGER',                      'INTEGER_KINDS',
    'INTENT',                       'INTERFACE',
    'INTRINSIC',                    'IOMSG',
    'IOR',                          'IOSTAT',
    'IOSTAT_END',                   'IOSTAT_EOR',
    'IOSTAT_INQUIRE_INTERNAL_UNIT', 'IPARITY',
    'IQINT',                        'IRAND',
    'IS',                           'ISATTY',
    'ISHFT',                        'ISHFTC',
    'ISIGN',                        'ISNAN',
    'ISO_C_BINDING',                'ISO_FORTRAN_ENV',
    'IS_IOSTAT_END',                'IS_IOSTAT_EOR',
    'ITIME',                        'KILL',
    'KIND',                         'LBOUND',
    'LCOBOUND',                     'LE',
    'LEADZ',                        'LEN',
    'LEN_TRIM',                     'LGAMMA',
    'LGE',                          'LGT',
    'LINK',                         'LLE',
    'LLT',                          'LNBLNK',
    'LOC',                          'LOCK',
    'LOCK_TYPE',                    'LOG',
    'LOG10',                        'LOGICAL',
    'LOGICAL_KINDS',                'LOG_GAMMA',
    'LONG',                         'LSHIFT',
    'LSTAT',                        'LT',
    'LTIME',                        'MALLOC',
    'MASKL',                        'MASKR',
    'MATMUL',                       'MAX',
    'MAX0',                         'MAX1',
    'MAXEXPONENT',                  'MAXLOC',
    'MAXVAL',                       'MCLOCK',
    'MCLOCK8',                      'MEMORY',
    'MERGE',                        'MERGE_BITS',
    'MIN',                          'MIN0',
    'MIN1',                         'MINEXPONENT',
    'MINLOC',                       'MINVAL',
    'MOD',                          'MODULE',
    'MODULO',                       'MOVE_ALLOC',
    'MVBITS',                       'NAME',
    'NAMED',                        'NAMELIST',
    'NE',                           'NEAREST',
    'NEQV',                         'NEW_LINE',
    'NEXTREC',                      'NINT',
    'NML',                          'NONE',
    'NON_INTRINSIC',                'NON_OVERRIDABLE',
    'NOPASS',                       'NORM2',
    'NOT',                          'NULL',
    'NULLIFY',                      'NUMBER',
    'NUMERIC_STORAGE_SIZE',         'NUM_IMAGES',
    'ONLY',                         'OPEN',
    'OPENED',                       'OPERATOR',
    'OPTIONAL',                     'OR',
    'OUT',                          'OUTPUT_UNIT',
    'PACK',                         'PAD',
    'PARAMETER',                    'PARITY',
    'PASS',                         'PERROR',
    'POINTER',                      'POPCNT',
    'POPPAR',                       'POSITION',
    'PRECISION',                    'PRESENT',
    'PRINT',                        'PRIVATE',
    'PROCEDURE',                    'PRODUCT',
    'PROGRAM',                      'PROTECTED',
    'PUBLIC',                       'PURE',
    'QABS',                         'QACOS',
    'QASIN',                        'QATAN',
    'QATAN2',                       'QCMPLX',
    'QCONJG',                       'QCOS',
    'QCOSH',                        'QDIM',
    'QERF',                         'QERFC',
    'QEXP',                         'QGAMMA',
    'QIMAG',                        'QLGAMA',
    'QLOG',                         'QLOG10',
    'QMAX1',                        'QMIN1',
    'QMOD',                         'QNINT',
    'QSIGN',                        'QSIN',
    'QSINH',                        'QSQRT',
    'QTAN',                         'QTANH',
    'RADIX',                        'RAN',
    'RAND',                         'RANDOM_NUMBER',
    'RANDOM_SEED',                  'RANGE',
    'RANK',                         'READ',
    'READWRITE',                    'REAL',
    'REAL128',                      'REAL32',
    'REAL64',                       'REALPART',
    'REAL_KINDS',                   'REC',
    'RECL',                         'RECORD',
    'RECURSIVE',                    'RENAME',
    'REPEAT',                       'RESHAPE',
    'RESULT',                       'RETURN',
    'REWIND',                       'REWRITE',
    'RRSPACING',                    'RSHIFT',
    'SAME_TYPE_AS',                 'SAVE',
    'SCALE',                        'SCAN',
    'SECNDS',                       'SECOND',
    'SELECT',                       'SELECTED_CHAR_KIND',
    'SELECTED_INT_KIND',            'SELECTED_REAL_KIND',
    'SEQUENCE',                     'SEQUENTIAL',
    'SET_EXPONENT',                 'SHAPE',
    'SHIFTA',                       'SHIFTL',
    'SHIFTR',                       'SHORT',
    'SIGN',                         'SIGNAL',
    'SIN',                          'SIND',
    'SINH',                         'SIZE',
    'SIZEOF',                       'SLEEP',
    'SNGL',                         'SOURCE',
    'SPACING',                      'SPREAD',
    'SQRT',                         'SRAND',
    'STAT',                         'STATUS',
    'STAT_FAILED_IMAGE',            'STAT_LOCKED',
    'STAT_LOCKED_OTHER_IMAGE',      'STAT_STOPPED_IMAGE',
    'STAT_UNLOCKED',                'STOP',
    'STORAGE_SIZE',                 'STRUCTURE',
    'SUBMODULE',                    'SUBROUTINE',
    'SUM',                          'SYMLNK',
    'SYNC',                         'SYSTEM',
    'SYSTEM_CLOCK',                 'TAN',
    'TAND',                         'TANH',
    'TARGET',                       'THEN',
    'THIS_IMAGE',                   'TIME',
    'TIME8',                        'TINY',
    'TO',                           'TRAILZ',
    'TRANSFER',                     'TRANSPOSE',
    'TRIM',                         'TRUE',
    'TTYNAM',                       'TYPE',
    'UBOUND',                       'UCOBOUND',
    'UMASK',                        'UNFORMATTED',
    'UNIT',                         'UNLINK',
    'UNLOCK',                       'UNPACK',
    'USE',                          'VALUE',
    'VERIF',                        'VERIFY',
    'VOLATILE',                     'WAIT',
    'WHERE',                        'WHILE',
    'WRITE',                        'XOR',
    'ZABS',                         'ZCOS',
    'ZEXP',                         'ZLOG',
    'ZSIN',                         'ZSQRT',
    '\.AND\.',                      '\.EQV\.',
    '\.EQ\.',                       '\.FALSE\.',
    '\.GE\.',                       '\.GT\.',
    '\.LE\.',                       '\.LT\.',
    '\.NEQV\.',                     '\.NE\.',
    '\.NOT\.',                      '\.OR\.',
    '\.TRUE\.',                     '\.XOR\.',
);

my @archaic_fortran_keywords = (
    'ALOG',   'ALOG10', 'AMAX0',  'AMAX1',  'AMIN0',  'AMIN1',
    'AMOD',   'CABS',   'CCOS',   'CEXP',   'CLOG',   'CSIN',
    'CSQRT',  'DABS',   'DACOS',  'DASIN',  'DATAN',  'DATAN2',
    'DBESJ0', 'DBESJ1', 'DBESJN', 'DBESY0', 'DBESY1', 'DBESYN',
    'DCOS',   'DCOSH',  'DDIM',   'DERF',   'DERFC',  'DEXP',
    'DINT',   'DLOG',   'DLOG10', 'DMAX1',  'DMIN1',  'DMOD',
    'DNINT',  'DSIGN',  'DSIN',   'DSINH',  'DSQRT',  'DTAN',
    'DTANH',  'FLOAT',  'IABS',   'IDIM',   'IDINT',  'IDNINT',
    'IFIX',   'ISIGN',  'LONG',   'MAX0',   'MAX1',   'MIN0',
    'MIN1',   'SNGL',   'ZABS',   'ZCOS',   'ZEXP',   'ZLOG',
    'ZSIN',   'ZSQRT',
);

my @openmp_keywords = (
    'PARALLEL', 'MASTER',      'CRITICAL',     'ATOMIC',
    'SECTIONS', 'WORKSHARE',   'TASK',         'BARRIER',
    'TASKWAIT', 'FLUSH',       'ORDERED',      'THREADPRIVATE',
    'SHARED',   'DEFAULT',     'FIRSTPRIVATE', 'LASTPRIVATE',
    'COPYIN',   'COPYPRIVATE', 'REDUCTION',
);

my @fortran_types = (
    'TYPE',             'CLASS',     'INTEGER', 'REAL',
    'DOUBLE PRECISION', 'CHARACTER', 'LOGICAL', 'COMPLEX',
    'ENUMERATOR',
);

my @unseparated_keywords = (
    'BLOCKDATA',    'DOUBLEPRECISION', 'ELSEIF',       'ELSEWHERE',
    'ENDASSOCIATE', 'ENDBLOCK',        'ENDBLOCKDATA', 'ENDCRITICAL',
    'ENDDO',        'ENDENUM',         'ENDFILE',      'ENDFORALL',
    'ENDFUNCTION',  'ENDIF',           'ENDINTERFACE', 'ENDMODULE',
    'ENDPARALLEL',  'ENDPARALLELDO',   'ENDPROCEDURE', 'ENDPROGRAM',
    'ENDSELECT',    'ENDSUBROUTINE',   'ENDTYPE',      'ENDWHERE',
    'GOTO',         'INOUT',           'PARALLELDO',   'SELECTCASE',
    'SELECTTYPE',
);

my @intrinsic_modules_keywords = (
    'ISO_C_BINDING',   'ISO_FORTRAN_ENV',
    'IEEE_ARITHMETIC', 'IEEE_EXCEPTIONS',
    'IEEE_FEATURES',
);

sub get_fortran_keywords {
    return @fortran_keywords;
}

sub get_openmp_keywords {
    return @openmp_keywords;
}

sub get_archaic_fortran_keywords {
    return @archaic_fortran_keywords;
}

sub get_unseparated_keywords {
    return @unseparated_keywords;
}

sub get_intrinsic_modules_keywords {
    return @intrinsic_modules_keywords;
}

# List of uncapitalised keywords present in most recently tested file
my %extra_error_information = ();

sub get_extra_error_information {
    return %extra_error_information;
}

sub reset_extra_error_information {
    %extra_error_information = ();
}
################################# UMDP3 tests #################################

# Check for uncapitalised keywords
sub capitalised_keywords {
    my @lines  = @_;
    my $failed = 0;

    # Iterate over lines and keywords
    foreach my $line (@lines) {
        my @keywords_to_check = get_fortran_keywords();

        $line = remove_quoted($line);

        next unless $line;
        next unless $line =~ /\S/sxm;    # If line empty, try the next

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        if ( $line =~ /^!\$/sxm ) {
            push @keywords_to_check, get_openmp_keywords();
        }

        foreach my $keyword (@keywords_to_check) {

            # If the keyword is present on the line
            if ( $line =~ /(^|\W)$keyword(\W|$)/sxmi ) {

                if ( $line =~ /\(\s*kind\s*=.*::/sxm ) {
                    $extra_error_information{'KIND'}++;
                    $failed++;
                }

                # Ignore cases such as RESHAPE(len=something) where 'len' would
                # otherwise be triggered
                next if ( $line =~ /,\s*$keyword\s*=/sxmi );
                next if ( $line =~ /\(\s*$keyword\s*=/sxmi );

                # Ignore CPP
                next if ( $line =~ /^\s*\#/sxm );

                # Fail if the keyword occurance(s) are not uppercase
                while ( $line =~ s/(^|\W)($keyword)(\W|$)/ /sxmi ) {
                    unless ( $2 =~ /$keyword/sxm ) {
                        $extra_error_information{$keyword}++;
                        $failed++;
                    }
                }

            }
        }
    }

    return $failed;
}

# OpenMP sentinels must be in column one
sub openmp_sentinels_in_column_one {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {

        # Check for one or more spaces before !$
        $failed++ if ( $line =~ /\s+!\$/sxm );
    }

    return $failed;
}

# ENDIF, etc should be END IF
sub unseparated_keywords {
    my @lines = @_;

    my @keywords = get_unseparated_keywords();

    my $failed = 0;
    foreach my $line (@lines) {

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        # Check for frequent ones - should rewrite as a loop
        unless ( $line =~ /^\s*\#/sxm ) {    # Ignore CPP
            foreach my $keyword (@keywords) {
                if ( $line =~ /(^|\W)$keyword(\W|$)/sxmi ) {
                    $failed++;
                    $extra_error_information{$keyword}++;
                }
            }
        }
    }

    return $failed;
}

# PAUSE and EQUIVALENCE are forbidden
sub forbidden_keywords {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        $failed++ if ( $line =~ /(^|\W)EQUIVALENCE(\W|$)/sxmi );
        $failed++ if ( $line =~ /(^|\W)PAUSE(\W|$)/sxmi );
    }

    return $failed;
}

# Older forms of relational operators are forbidden
sub forbidden_operators {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        $failed++ if ( $line =~ /\.GT\./sxmi );
        $failed++ if ( $line =~ /\.GE\./sxmi );
        $failed++ if ( $line =~ /\.LT\./sxmi );
        $failed++ if ( $line =~ /\.LE\./sxmi );
        $failed++ if ( $line =~ /\.EQ\./sxmi );
        $failed++ if ( $line =~ /\.NE\./sxmi );
    }

    return $failed;
}

# Any GO TO must go to 9999
sub go_to_other_than_9999 {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        # Find lines matching GO TO
        if ( $line =~ /GO\s*TO/sxmi ) {

            # If the line number isn't 9999
            unless ( $line =~ /GO\s*TO\s*9999/sxmi ) {
                $failed++;
            }
        }

    }

    return $failed;
}

# WRITE must specify a proper format
sub write_using_default_format {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        # Check for WRITE(...*)
        if ( $line =~ /WRITE\s*\(.*\*\)/sxmi ) {
            $failed++;
        }

    }

    return $failed;
}

sub lowercase_variable_names {
    my @lines = @_;

    my $failed = 0;
    my @variables;

    # Make a list of variables
    foreach my $line (@lines) {

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        if (   $line =~ /^\s*REAL/sxmi
            or $line =~ /^\s*INTEGER/sxmi
            or $line =~ /^\s*LOGICAL/sxm
            or $line =~ /^\s*CHARACTER/sxm )
        {
            if ( $line =~ /::/sxm ) {
                $line =~ /::\s*(\w+)/sxm;
                my $variable = $1;
                next unless ($variable);
                push @variables, $variable;
            }
        }
    }

    # Search the code for these variables
    foreach my $line (@lines) {

        # Ignore CPP defs:
        next if ( $line =~ /^\s*\#/sxm );

        $line = remove_quoted($line);

        foreach my $variable (@variables) {
            if ( $line =~ /\b($variable)\b/sxmi ) {
                my $instance_of_variable = $1;

# If the variable is 4 or more characters and is uppercase in the declaration fail the test
# The length test is because some short scientific quantities could legitimately be uppercase.
                next if ( length $variable < 4 );

                if ( $instance_of_variable eq "\U$instance_of_variable" ) {
                    $failed++;
                    $extra_error_information{$instance_of_variable}++;
                }
            }
        }
    }

    return $failed;
}

sub include_files_for_variable_declarations {
    my @lines = @_;

    my $failed = 0;

    my $found_dr_hook = 0;
    foreach my $line (@lines) {
        $found_dr_hook++ if ( $line =~ /CALL\s+dr_hook/sxmi );
    }

    # File which don't have directly executable code automatically pass this
    return 0 unless $found_dr_hook;

    foreach my $line (@lines) {
        $failed++ if ( $line =~ /^\s*\#include/sxm );
        last if ( $line =~ /CALL\s+dr_hook/sxmi );
    }

    $number_of_files_with_variable_declarations_in_includes++ if $failed;
    return $failed;
}

sub dimension_forbidden {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        next unless $line;
        $failed++ if ( $line =~ /(^|\W)DIMENSION\W/sxmi );
    }

    return $failed;
}

sub forbidden_stop {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        $failed++ if ( $line =~ /^\s*STOP\s/sxmi );
        $failed++ if ( $line =~ /^\s*CALL\s*abort\W/sxmi );
    }

    return $failed;
}

sub ampersand_continuation {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {

        $failed++ if ( $line =~ /^\s*&/sxmi );
        $failed++ if ( $line =~ /^\s*!\$\s*&/sxmi );
    }

    return $failed;
}

sub implicit_none {
    my @lines = @_;

    my $failed  = 0;
    my $foundit = 0;
    my $modules = 0;
    my @lines_to_test;

    my $in_interface = 0;
    foreach my $input_line (@lines) {

        $input_line = remove_quoted($input_line);

        # Remove comments unless they're OpenMP commands
        if ( $input_line =~ /![^\$]/sxm ) {
            $input_line =~ s/![^\$].*?$//sxmg;
        }

        # MODULEs etc in INTERFACEs don't have implicit none, so ignore these
        if ( $input_line =~ /^\s*INTERFACE\s/sxmi ) {
            $in_interface = 1;
        }
        push @lines_to_test, $input_line unless $in_interface;
        if ( $input_line =~ /^\s*END\s*INTERFACE/sxmi ) {
            $in_interface = 0;
        }
    }

    foreach my $line (@lines_to_test) {

        $foundit++ if ( $line =~ /^\s*IMPLICIT\s+NONE/sxmi );
        $modules++
          if ( $line =~ /^\s*SUBROUTINE\W/sxmi
            or $line =~ /^\s*MODULE\W/sxmi
            or $line =~ /^\s*FUNCTION\W/sxmi
            or $line =~ /^\s*REAL\s*FUNCTION\W/sxmi
            or $line =~ /^\s*LOGICAL\s*FUNCTION\W/sxmi
            or $line =~ /^\s*INTEGER\s*FUNCTION\W/sxmi
            or $line =~ /^\s*PROGRAM\W/sxmi );
    }

    $failed = 1 unless ( $foundit >= $modules );

    return $failed;
}

sub intrinsic_as_variable {
    my @lines    = @_;
    my $failed   = 0;
    my @keywords = get_fortran_keywords();

    my @fixed_lines = ();

    push @keywords, get_openmp_keywords();

    # Steps:
    #  i)   sanitise lines
    #  ii)  look for match
    #  iii) check if match is a declaration (which must start with a type)
    #  iv)  exclude any false positives from initialisation.

    # i) sanitise lines

    foreach my $line (@lines) {

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        # Remove pre-processing directives
        if ( $line =~ /^\s*\#/sxm ) {
            $line = "";
        }

        push @fixed_lines, $line;
    }

    my $entire = join( "", @fixed_lines );

    # Sort out continuation lines
    $entire =~ s/&\s*\n//sxmg;

    @fixed_lines = split /\n/sxm, $entire;

    foreach my $line (@fixed_lines) {

        next unless $line;
        next unless $line =~ /\S/sxm;

        my $oline = $line;

        foreach my $keyword (@keywords) {
            my $decl_match = 0;
            $line = $oline;

            #  ii)  look for match
            if ( $line =~ /(^|\W)$keyword($|\W)/sxmi ) {
                foreach my $type (@fortran_types) {
                    my $type_r = $type;
                    $type_r =~ s/[ ]/[ ]/sxm;

#  iii) check if match is a variable declaration (which always starts with a type):
                    if ( $line =~ /^\s*$type_r(\W.*\W|\W)$keyword/sxmi ) {
                        if (    $type =~ 'CLASS'
                            and $keyword =~ /(IS|DEFAULT)/sxm )
                        {

                  # statments within SELECT TYPE constructs are not declarations
                            unless ( $line =~ /^\s*CLASS\s+(IS|DEFAULT)/sxmi ) {
                                $decl_match = 1;
                            }
                        }
                        elsif ( $type =~ 'TYPE' and $keyword =~ 'IS' ) {

                  # statments within SELECT TYPE constructs are not declarations
                            unless ( $line =~ /^\s*TYPE\s+IS/sxmi ) {
                                $decl_match = 1;
                            }
                        }
                        else {
                            $decl_match = 1;
                        }
                        last;
                    }
                }
            }

            #  iv)  exclude any false positives from initialisation.
            if ($decl_match) {

     # This is a variable declaration with a matching keyword
     # make sure this is not because of initialising to the result of a function
     # (i.e. the keyword is the RHS of the = in this variable initialisation).

                # remove any type attributes which may match the keyword
                $line =~ s/^.*:://sxm;

                # If we have a function declaration of the form
                # FUNCTION foo() RESULT(bar)
                # We need to strip the RESULT keyword out.
                if ( $line =~ /^(.*?\bFUNCTION\s.*?\b)RESULT(\s*\()/sxm ) {
                    my $grp1 = quotemeta($1);
                    my $grp2 = quotemeta($2);
                    $line =~ s/($grp1)RESULT($grp2)/$grp1$grp2/sxm;
                }

            # at this point, things in brackets aren't relevant because they can
            # only be attributes of a variable, not the definition of a variable
            # itself
                while ( $line =~ /\(.*\)/sxm ) {
                    $line =~ s/\([^()]*\)//sxm;
                }

               # At this point, remove array initialisations, as they mess with
               # breaking on commas. "[]" and "(/ /)" forms may exist. We assume
               # other checks enforce the "[]" -> "(/ /)" conversion, so here we
               # simply deals with "[]". They may also be nested, so first we
               # flatten to a single array initialiser.

                # The following matches a "[", followed by a capture group, then
                # a pair of "[" and "]" enclosing a capture group which does not
                # contain either a "[" or "]", followed by a capture group and a
                # "]". It repeatedly removes the innermost pair of "[" and "]"
                # in a nest until no more exist.

                while ( $line =~ /\[(.*?)\[([^\[]*?)\](.*?)\]/sxm ) {
                    $line =~ s/\[(.*?)\[([^\[]*?)\](.*?)\]/[$1$2$3]/sxm;
                }

                # The following removes the actual array initialisations, which
                # must be flattened and of the "[]" form following an "=" sign.

                while ( $line =~ /\=\s*\[.*?\]/sxm ) {
                    $line =~ s/\=\s*\[.*?\]//sxm;
                }

             # split on commas, in case there are multiple variable declarations
                my @decls = split /,/sxm, $line;

                foreach my $decl (@decls) {

            # As anything to the right of '=' signs are not variable definitions
            # (they are instead initialiser etc.) we're not interested in them.
                    $decl =~ s/=.*$//sxm;

                    # Remove function declarations
                    $decl =~ s/^.*?\bFUNCTION\s//sxmi;

                    # If we get this far any matches are fails
                    if ( $decl =~ /(^|\W)$keyword(\W|$)/sxmi ) {
                        $line = "\n    $keyword";
                        $failed++;
                        $extra_error_information{$line}++;
                    }
                }
            }
        }
    }

    return $failed;
}

sub line_over_80chars {
    my @lines  = @_;
    my $failed = 0;

    foreach my $line (@lines) {

        # This needs to be 81, as Perl counts the newline as having length 1
        if ( length $line > 81 ) {
            $failed++;

            # Reformat line so it prints the offending line neatly
            chomp($line);
            $line = "\n    '$line'";
            $extra_error_information{$line}++;
        }
    }
    return $failed;
}

sub tab_detection {
    my @lines  = @_;
    my $failed = 0;
    foreach my $line (@lines) {

        # If any line contains a tab character
        if ( $line =~ /\t/sxm ) {
            $failed++;

            # Reformat line so it prints the offending line neatly
            chomp($line);
            $line = "\n    '$line'";
            $extra_error_information{$line}++;
        }
    }
    return $failed;
}

sub check_crown_copyright {
    my @lines            = @_;
    my $failed           = 1;
    my @valid_agreements = (
        'L0195',            'NERC',
        'SC0138',           'UKCA',
        'SC0171',           'ACCESS',
        'SC0237',           'JULES',
        'IBM',              'of Bath',
        'Centre National',  'Lawrence Livermore',
        'Roger Marchand, ', 'of Colorado',
        'of Reading',
    );

    foreach my $line (@lines) {
        $failed = 0 if ( $line =~ /^\s*(!|\/\*).*Crown\s*copyright/sxmi );
        foreach my $agreement (@valid_agreements) {
            my $agreement_r = $agreement;
            $agreement_r =~ s/[ ]/[ ]/sxm;
            $failed = 0 if ( $line =~ /^\s*(!|\/\*).*$agreement_r/sxmi );
        }
    }

    return $failed;
}

sub check_code_owner {
    my @lines      = @_;
    my $failed     = 1;
    my $failed_co  = 0;
    my $failed_bi  = 0;
    my $is_shumlib = 0;

    foreach my $line (@lines) {
        $is_shumlib = 1
          if ( $line =~
/^\s*(!|\/\*)\s*This\s*file\s*is\s*part\s*of\s*the\s*UM\s*Shared\s*Library\s*project/sxmi
          );
    }

    if ( $is_shumlib == 1 ) {
        $failed = 0;
    }
    else {
        foreach my $line (@lines) {
            $failed_co++
              if ( $line =~
/^\s*(!|\/\*)\s*Code\s*Owner:\s*Please\s*refer\s*to\s*the\s*UM\s*file\s*CodeOwners\.txt/sxmi
              );
            $failed_bi++
              if ( $line =~
                /^\s*(!|\/\*)\s*This\s*file\s*belongs\s*in\s*section:/sxmi );
        }

        if ( $failed_co > 1 or $failed_bi > 1 ) {
            $extra_error_information{"(multiple statements found)"}++;
        }

        if ( $failed_co == 1 and $failed_bi == 1 ) {
            $failed = 0;
        }
    }

    return $failed;
}

sub array_init_form {
    my @lines  = @_;
    my $failed = 0;

    my @fixed_lines = ();

    # First we clean up the lines by removing string contents and comments
    foreach my $line (@lines) {

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        # Remove pre-processing directives
        if ( $line =~ /^\s*\#/sxm ) {
            $line = "";
        }

        push @fixed_lines, $line;
    }

    my $entire = join( "", @fixed_lines );

    # Sort out continuation lines
    $entire =~ s/&\s*\n//sxmg;

    @fixed_lines = split /\n/sxm, $entire;

    # Now check for the existence of lines containing (/ /)
    foreach my $line (@fixed_lines) {

        next unless $line;
        next unless $line =~ /\S/sxm;

        $failed = 1 if ( $line =~ /\(\/.*\/\)/sxm );
    }

    return $failed;
}

sub retire_if_def {
    my @lines = @_;
    my @ifdefs = ( 'VATPOLES', 'A12_4A', 'A12_3A', 'UM_JULES', 'A12_2A', );

    # Sort out C continuation lines
    my $entire = join( "", @lines );
    $entire =~ s/\\\s*\n//sxmg;
    @lines = split /\n/sxm, $entire;

    my $failed = 0;
    for ( my $i = 0 ; $i < scalar @lines ; $i++ ) {
        my $line = $lines[$i];
        foreach my $ifdef (@ifdefs) {

# matches #if defined(<def>), #elif defined(<def>), #ifdef <def>, and #ifndef <def>
            if ( $line =~ /^\s*\#(el)?if.*\W$ifdef/sxm ) {
                $failed++;
                $extra_error_information{$ifdef}++;
            }
        }
    }

    return $failed;
}

sub c_deprecated {
    my @lines       = @_;
    my %deprecateds = (
        'strcpy'  => "(): please use strncpy() instead",
        'sprintf' => "(): please use snprintf() instead",
        'usleep'  => "(): please use nanosleep() instead",
        '_BSD_SOURCE' =>
": please find alternative functionality, or an equivalent feature test macro (if possible)",
        '_DEFAULT_SOURCE' =>
": please find alternative functionality, or an equivalent feature test macro (if possible)",
    );

    my $entire = join( "", @lines );

    #remove commented sections
    $entire =~ s/\/\*(.|\n)+?(\*\/)//sxmg;

    # Sort out continuation lines
    $entire =~ s/\\\s*\n//sxmg;

    #remove #pragmas
    $entire =~ s/(^|\n)\s*\#pragma.+?\n/\n/sxmg;

    @lines = split /\n/sxm, $entire;

    my $failed = 0;
    for ( my $i = 0 ; $i < scalar @lines ; $i++ ) {
        my $line = $lines[$i];

        foreach my $dep ( keys %deprecateds ) {
            if ( $line =~ /$dep/sxm ) {
                my $extra_msg = "$dep$deprecateds{$dep}";
                $failed++;
                $extra_error_information{$extra_msg}++;
            }
        }
    }

    return $failed;
}

sub printstatus_mod {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {
        $failed++ if ( $line =~ /^\s*USE\s*printstatus_mod/sxmi );
    }
    return $failed;
}

sub write6 {
    my @lines  = @_;
    my $failed = 0;

    for ( my $i = 0 ; $i < scalar @lines ; $i++ ) {
        if ( $lines[$i] =~ /^\s*WRITE/sxmi ) {
            if ( $lines[$i] =~ /^\s*WRITE\s*\(\s*6/sxm ) {
                $failed++;
            }
        }

    }

    return $failed;
}

sub printstar {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {
        $failed++ if ( $line =~ /^\s*PRINT\s*\*/sxmi );
    }
    return $failed;
}

sub um_fort_flush {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {
        $failed++ if ( $line =~ /^\s*CALL\s*UM_FORT_FLUSH/sxmi );
    }
    return $failed;
}

sub svn_keyword_subst {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {
        $failed++ if ( $line =~ /\$Date\$/sxm );
        $failed++ if ( $line =~ /\$LastChangedDate\$/sxm );
        $failed++ if ( $line =~ /\$Revision\$/sxm );
        $failed++ if ( $line =~ /\$Rev\$/sxm );
        $failed++ if ( $line =~ /\$LastChangedRevision\$/sxm );
        $failed++ if ( $line =~ /\$Author\$/sxm );
        $failed++ if ( $line =~ /\$LastChangedBy\$/sxm );
        $failed++ if ( $line =~ /\$HeadURL\$/sxm );
        $failed++ if ( $line =~ /\$URL\$/sxm );
        $failed++ if ( $line =~ /\$Id\$/sxm );
        $failed++ if ( $line =~ /\$Header\$/sxm );
    }

    return $failed;

}

sub omp_missing_dollar {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {
        if ( $line =~ /^\s*!OMP/sxm ) {
            $failed = 1;
        }
    }

    return $failed;

}

sub intrinsic_modules {
    my @lines = @_;

    my $failed = 0;

    foreach my $line (@lines) {
        my @keywords_to_check = get_intrinsic_modules_keywords();

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        next unless $line;
        next unless $line =~ /\S/sxm;    # If line empty, try the next

        foreach my $keyword (@keywords_to_check) {
            if ( $line =~ /^\s*USE\s*$keyword/sxmi ) {
                $extra_error_information{$keyword}++;
                $failed++;
            }
        }
    }
    return $failed;
}

sub cpp_ifdef {

    # ifdefs should be of the form "#if defined(MY_IFDEF)"
    # rather than "#ifdef(MY_IFDEF)"
    my @lines  = @_;
    my $failed = 0;
    foreach my $line (@lines) {
        if ( $line =~ /^\s*\#ifdef/sxm ) {
            $failed++;
        }
        elsif ( $line =~ /^\s*\#ifndef/sxm ) {
            $failed++;
        }
    }
    return $failed;
}

sub cpp_comment {

    # C pre-processor directives should not be intermingled with
    # fortran style comments
    my @lines    = @_;
    my $failed   = 0;
    my @comments = ();
    foreach my $line (@lines) {

        # is this an #if statement?
        if ( ( $line =~ m/^\s*\#if[ ]/sxm ) || ( $line =~ m/^\s*\#elif[ ]/sxm ) )
        {

            # does this ifdef have a ! in it?
            if ( $line =~ /!/sxm ) {

                # split the possible regions (ignoring the 0th)
                # and loop over to check each one in turn
                @comments = split /!/sxm, $line, -1;
                splice( @comments, 0, 1 );
                foreach my $comment (@comments) {

                    # must be a recognisable CPP directive
                    if ( $comment !~ /(^\s*\(?\s*defined)|(^=\s*[0-9])/sxm ) {
                        $failed++;
                    }
                }
            }
        }

        # is this an #else?
        elsif ( $line =~ /^\s*\#else\s*!/sxm ) {
            $failed++;
        }

        # is this an #endif?
        elsif ( $line =~ /^\s*\#endif\s*!/sxm ) {
            $failed++;
        }

        # is this an #include?
        elsif ( $line =~ /^\s*\#include[^!]+!/sxm ) {
            $failed++;
        }
    }
    return $failed;
}

sub obsolescent_fortran_intrinsic {
    my @lines = @_;

    my $failed = 0;

    # Iterate over lines and keywords
    foreach my $line (@lines) {
        my @keywords_to_check = get_archaic_fortran_keywords();

        $line = remove_quoted($line);

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        next unless $line;
        next unless $line =~ /\S/sxm;    # If line empty, try the next

        # Ignore CPP
        next if ( $line =~ /^\s*\#/sxm );

        foreach my $keyword (@keywords_to_check) {

            # If the keyword is present on the line
            if ( $line =~ /(^|\W)$keyword(\W|$)/sxmi ) {
                $extra_error_information{$keyword}++;
                $failed++;
            }
        }
    }
    return $failed;
}

sub exit_stmt_label {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        # Find if the line appears to contain a solitary EXIT
        if ( $line =~ /\bEXIT\b/sxm ) {

            # fail if that EXIT is not followed by a label
            $failed++ if ( $line =~ /EXIT\s*$/sxm );
        }
    }

    return $failed;

}

sub read_unit_args {
    my @lines = @_;

    my $failed = 0;
    foreach my $line (@lines) {

        # Remove comments unless they're OpenMP commands
        if ( $line =~ /![^\$]/sxm ) {
            $line =~ s/![^\$].*?$//sxmg;
        }

        # Find if the line appears to be a READ statement
        if ( $line =~ /^\s*READ\s*\(/sxm ) {

            # fail if that READ does not have UNIT= as the first argument
            $failed++ if ( !( $line =~ /^\s*READ\s*\(\s*UNIT\s*=/sxm ) );
        }
    }

    return $failed;

}

sub c_openmp_define_pair_thread_utils {
    my @lines = c_sanitise_lines(@_);

    my $failed = 0;
    for ( my $i = 0 ; $i < scalar @lines ; $i++ ) {
        my $line = $lines[$i];

        # match ifdef and defined style for _OPENMP
        if ( $line =~ /^\s*\#(el)?if.*defined\(_OPENMP\)/sxm ) {

            # fail if _OPENMP is not the first defined() test, or it is not
            # followed by SHUM_USE_C_OPENMP_VIA_THREAD_UTILS
            if ( $line !~
/^\s*\#(el)?if\s*!?defined\(_OPENMP\)\s*&&\s*!?defined\(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS\)/sxm
              )
            {
                $failed++;
            }
        }
    }

    return $failed;
}

sub c_openmp_define_no_combine {
    my @lines = c_sanitise_lines(@_);

    my $failed = 0;
    for ( my $i = 0 ; $i < scalar @lines ; $i++ ) {
        my $line = $lines[$i];

        # fail if we match defined(_OPENMP) + at least two other defined()
        if ( $line =~
            /^\s*\#(el)?if\s*defined\(_OPENMP\)(.*?!?defined\(\w+\)){2,}/sxm )
        {
            $failed++;
        }
    }

    return $failed;
}

sub c_openmp_define_not {
    my @lines = c_sanitise_lines(@_);

    my $failed = 0;
    for ( my $i = 0 ; $i < scalar @lines ; $i++ ) {
        my $line = $lines[$i];

        # fail if we match !defined(_OPENMP)
        if ( $line =~ /^\s*\#(el)?if.*!defined\(_OPENMP\)/sxm ) {
            $failed++;
        }
    }

    return $failed;
}

sub c_ifdef_defines {
    my @lines = c_sanitise_lines(@_);

    my $failed = 0;
    for ( my $i = 0 ; $i < scalar @lines ; $i++ ) {
        my $line = $lines[$i];

        # fail if we match #ifdef or #ifndef
        if ( $line =~ /^\s*\#if(n)?def/sxm ) {
            $failed++;
        }
    }

    return $failed;
}

sub c_protect_omp_pragma {
    my @lines = c_sanitise_lines(@_);

    # remove _OPENMP if-def protected lines.
    # As #ifs may be nested, successivly remove all the #if blocks until
    # none are remaining.
    for ( my $i = scalar @lines ; $i > 0 ; $i-- ) {
        my $line = $lines[ $i - 1 ];

        # as we are going from the bottom, the first #if will be an
        # innermost one.
        if ( $line =~ /^\s*\#if/sxm ) {

            splice @lines, $i - 1, 1, '';

            my $whipe = 0;

            if ( $line =~ /defined\(_OPENMP\)/sxm ) {
                $whipe = 1;
            }

            for ( my $j = $i - 1 ; $j < scalar @lines ; $j++ ) {
                my $jline = $lines[$j];

                if ( $whipe == 1 ) {
                    splice @lines, $j, 1, '';
                }

                if ( $jline =~ /\#else/sxm ) {
                    if ( $whipe == 1 ) {
                        $whipe = 0;
                    }
                    splice @lines, $j, 1, '';
                }

                if ( $jline =~ /\#elif/sxm ) {
                    if ( $whipe == 1 ) {
                        $whipe = 0;
                    }
                    if ( $jline =~ /defined\(_OPENMP\)/sxm ) {
                        $whipe = 1;
                    }
                    splice @lines, $j, 1, '';
                }

                if ( $jline =~ /\#endif/sxm ) {
                    splice @lines, $j, 1, '';
                    last;
                }

            }
        }
    }

    my $failed = 0;
    for ( my $i = 0 ; $i < scalar @lines ; $i++ ) {
        my $line = $lines[$i];

        # as we have removed all lines protected by _OPENMP,
        # any remaining pragma lines are a fail.
        if ( $line =~ /\#pragma\s+omp/sxm ) {
            $failed++;
        }

        # as are omp includes
        if ( $line =~ /\#include\s+(<|")omp.h(>|")/sxm ) {
            $failed++;
        }
    }

    return $failed;
}

sub c_sanitise_lines {
    my @lines = @_;

    my $entire = join( "", @lines );

    #remove commented sections
    $entire =~ s/\/\*(.|\n)+?(\*\/)//sxmg;

    # Sort out continuation lines
    $entire =~ s/\\\s*\n//sxmg;

    # standardise format for defined(<DEF>) style tests
    $entire =~
s/defined\s*?\(?\s*?(\w+)[^\S\n]*\)?([|&><*+%^$()\/\-\s])/defined($1) $2/sxmg;

    @lines = split /\n/sxm, $entire;

    return @lines;
}

sub line_trail_whitespace {
    my @lines  = @_;
    my $failed = 0;

    foreach my $line (@lines) {

        $line =~ s/\n//sxmg;

        # Fail if there are whitespace characters at the end of a line.
        if ( $line =~ /\s+$/sxm ) {
            $failed++;
            $line = "\n    '$line'";
            $extra_error_information{$line}++;
        }
    }
    return $failed;
}

sub c_integral_format_specifiers {
    my @lines  = @_;
    my $failed = 0;

    my @fixed_width_size = ( '8', '16', '32', '64' );

    my @fixed_width_type = ( 'MAX', 'PTR' );

    my @fixed_prefix = ( 'PRI', 'SCN' );

    my @fixed_suffix = ( '', 'FAST', 'LEAST' );

    my @print_style = ( 'd', 'i', 'u', 'o', 'x', 'X' );

    # Exact numerical width style (e.g. PRIdFAST64)
    foreach my $line (@lines) {
        foreach my $fwpre (@fixed_prefix) {
            foreach my $fwps (@print_style) {
                foreach my $fwsz (@fixed_width_size) {
                    foreach my $fwsfx (@fixed_suffix) {

        # Fail if format specifier immediately follows or proceeds a " character
                        if ( $line =~ /"${fwpre}${fwps}${fwsfx}${fwsz}/sxm ) {
                            $failed++;
                            chomp($line);
                            $line = "\n    '$line'";
                            $extra_error_information{$line}++;
                        }
                        elsif ( $line =~ /${fwpre}${fwps}${fwsfx}${fwsz}"/sxm )
                        {
                            $failed++;
                            chomp($line);
                            $line = "\n    '$line'";
                            $extra_error_information{$line}++;
                        }
                    }
                }
            }
        }
    }

    # Style defining the width by type (e.g. SCNuMAX)
    foreach my $line (@lines) {
        foreach my $fwpre (@fixed_prefix) {
            foreach my $fwps (@print_style) {
                foreach my $fwt (@fixed_width_type) {

        # Fail if format specifier immediately follows or proceeds a " character
                    if ( $line =~ /"${fwpre}${fwps}${fwt}/sxm ) {
                        $failed++;
                        chomp($line);
                        $line = "\n    '$line'";
                        $extra_error_information{$line}++;
                    }
                    elsif ( $line =~ /${fwpre}${fwps}${fwt}"/sxm ) {
                        $failed++;
                        chomp($line);
                        $line = "\n    '$line'";
                        $extra_error_information{$line}++;
                    }
                }
            }
        }
    }

    return $failed;
}

sub c_final_newline {
    my @lines  = @_;
    my $failed = 0;

    my $line = $lines[-1];
    my $fchar = substr $line, -1;

    # Fail if the final line does not end with a newline character
    if ( ord $fchar != 10 ) {
        $failed++;
    }

    return $failed;
}

1;
