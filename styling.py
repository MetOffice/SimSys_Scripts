#!/usr/bin/env python3
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# Created date: 24/10/2023
# Modified date: 14/03/2025
# *****************************COPYRIGHT*******************************
'''
## NOTE ##
This module is one of several for which the Master copy is in the
UM repository. When making changes, please ensure the changes are made in the UM
repository or they will be lost during the release process when the UM copy is
copied over.

This module contains various functions for applying UMDP3 styling to
Fortran source code
'''
import re
import sys
from fstring_parse import *
from styling_keywords import KEYWORDS, CODE_REPLACEMENTS, COMMENT_REPLACEMENTS, FORTRAN_TYPES



def replace_patterns(line, str_continuation):
    '''Replace various patterns according to the styling guidelines on
    the provided line, returning the result'''

    if len(line.strip()) == 0:
        return line

    workline = clean_str_continuation(line, str_continuation)

    stripline = workline.strip()

    # Pre-processor lines start with #. Ignore them completely.
    pre_proc = (stripline[0] == "#")

    # Lines that are completely commented start with a bang and are also
    # ignored completely.
    all_comment = (stripline[0] == "!")

    if pre_proc or all_comment:
        return line

    # remove strings
    try:
        blank_line = blank_fstring(workline)
    except ParsingError as e:
        str_cont_test = is_str_continuation(workline)
        check_quote = str_cont_test[SQUOTE] or str_cont_test[DQUOTE]
        if check_quote is True:
            blank_line = partial_blank_fstring(workline)
        else:
            print("keyword split simplify line has failed.")
            print("{0:s} Line simplification has failed for:".format(e.msg))
            print(line)
            exit(1)

    # remove comments
    match_line = blank_fcomments(blank_line)

    # Look for existance of code replacement patterns
    for pattern, replacement in CODE_REPLACEMENTS:
        m = re.search(pattern, match_line, flags=re.IGNORECASE)
        if m:
            for n in range(len(m.groups())+1):
                replacement = re.sub(r'(\\{0:s}|\\g<{0:s}>)'.format(str(n)),
                                     line[m.start(n):m.end(n)],
                                     replacement)

            line = "".join([line[:m.start(0)],
                            replacement,
                            line[m.end(0):]])

            workline = clean_str_continuation(line, str_continuation)

            # remove strings
            try:
                blank_line = blank_fstring(workline)
            except ParsingError as e:
                str_cont_test = is_str_continuation(workline)
                check_quote = str_cont_test[SQUOTE] or str_cont_test[DQUOTE]
                if check_quote is True:
                    blank_line = partial_blank_fstring(workline)
                else:
                    print("keyword split simplify line has failed.")
                    print("{0:s} Line simplification has failed for:" \
                          "".format(e.msg))
                    print(line)
                    exit(1)

            # remove comments
            match_line = blank_fcomments(blank_line)

    return line


def replace_comment_patterns(line, str_continuation):
    '''Replace various patterns according to the styling guidelines on
    the provided line, returning the result'''

    if len(line.strip()) == 0:
        return line

    workline = clean_str_continuation(line, str_continuation)

    stripline = workline.strip()

    # Pre-processor lines start with #. Ignore them completely.
    pre_proc = (stripline[0] == "#")

    if pre_proc:
        return line

    # remove strings
    try:
        match_line = blank_fstring(workline)
    except ParsingError as e:
        str_cont_test = is_str_continuation(workline)
        check_quote = str_cont_test[SQUOTE] or str_cont_test[DQUOTE]
        if check_quote is True:
            match_line = partial_blank_fstring(workline)
        else:
            print("keyword split simplify line has failed.")
            print("{0:s} Line simplification has failed for:".format(e.msg))
            print(line)
            exit(1)

    # Look for existance of code replacement patterns
    for pattern, replacement in COMMENT_REPLACEMENTS:
        m = re.search(pattern, match_line, flags=re.IGNORECASE)
        if m:
            for n in range(len(m.groups())+1):
                replacement = re.sub(r'(\\{0:s}|\\g<{0:s}>)'.format(str(n)),
                                     line[m.start(n):m.end(n)],
                                     replacement)

            line = "".join([line[:m.start(0)],
                            replacement,
                            line[m.end(0):]])

            workline = clean_str_continuation(line, str_continuation)

            # remove strings
            try:
                match_line = blank_fstring(workline)
            except ParsingError as e:
                str_cont_test = is_str_continuation(workline)
                check_quote = str_cont_test[SQUOTE] or str_cont_test[DQUOTE]
                if check_quote is True:
                    match_line = partial_blank_fstring(workline)
                else:
                    print("keyword split simplify line has failed.")
                    print("{0:s} Line simplification has failed for:" \
                          "".format(e.msg))
                    print(line)
                    exit(1)

    return line


def upcase_keywords(line, str_continuation):
    '''Upper-case any Fortran keywords on the given line, and down-case any
    all capital words which aren't keywords, returning the result'''

    workline = clean_str_continuation(line, str_continuation)

    stripline = workline.strip()

    if len(stripline) == 0 or stripline[0] == "!" or stripline[0] == "#":
        return line

    nloop = len(line)

    try:
        simple_line = blank_fstring(workline)
    except ParsingError as e:
        str_cont_test = is_str_continuation(workline)
        check_quote = str_cont_test[SQUOTE] or str_cont_test[DQUOTE]
        if check_quote is True:
            simple_line = partial_blank_fstring(workline)
        else:
            print("keyword split simplify line has failed.")
            print("{0:s} Line simplification has failed for:".format(e.msg))
            print(line)
            exit(1)

    # remove comments
    simple_line = blank_fcomments(simple_line)

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


def declaration_double_colon(iline, lines, pp_line_previous, line_previous):
    '''
    Attempt to add the double colon to definition lines which do not already
    have it
    '''

    line = lines[iline]

    workline = re.sub(r"\\(\s*)$", r" \1", pp_line_previous)
    workline = workline + re.sub(r"&(\s*)$", r" \1", line_previous) + line

    found_dec_type = None

    for declare_type in FORTRAN_TYPES:
        if re.search(r"^\s*{0:s}\W".format(declare_type),
                     workline, flags=re.IGNORECASE):
            found_dec_type = declare_type
            break

    if found_dec_type is not None:
        xlines = lines[iline:]
        xlines[0] = workline

        # Pre-process the line to pull in any continuation lines
        simple_line = simplify_line(xlines)

        if not re.search(r"\s+FUNCTION(,|\s|\()",
                         simple_line, flags=re.IGNORECASE):
            # The presence of declaration attributes (ALLOCATABLE,
            # PUBLIC, POINTER, etc) are only valid when used with
            # the double-colon.  Therefore after allowing for the
            # presence of either a (KIND/LEN=...) statement or an
            # older "*INT" declaration the first character should
            # not be a comma
            search = re.search(
                    r"^(\s*{0:s}\s*?(\(.*?\)|\*\s*[0-9]+|))\s+(\w+)".format(
                        found_dec_type), simple_line, flags=re.IGNORECASE)
            if search:
                # avoid CLASS IS, TYPE IS and CLASS DEFAULT statements
                classtype = search.group(3).strip().upper()
                if classtype != "IS" and classtype != "DEFAULT":
                    # Group 1 contains everything up to the start of the
                    # variable definition
                    endpos = search.end(1)
                    endpos -= len(pp_line_previous)
                    endpos -= len(line_previous)
                    if endpos > 0:
                        statement = line[:endpos].rstrip()
                    else:
                        statement = search.group(1).rstrip()

                    # Attempt to fit the double-colon into an existing space to
                    # preserve indentation, otherwise just add it to the line
                    line = re.sub(r"^{0:s}"
                                  r"\s\s\s\s".format(re.escape(statement)),
                                  r"{0:s} :: ".format(statement),
                                  line, count=1)
                    line = re.sub(r"^{0:s}\s*"
                                  r"((?<!\s\s\s\s)(\w|\\|&))".format(
                                      re.escape(statement)),
                                  r"{0:s} :: \g<1>".format(
                                      statement), line, count=1)

    return line


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
        line = declaration_double_colon(iline, lines, pp_line_previous,
                                        line_previous)

        if pp_continuation:
            if not pseudo_comment:
                line = replace_patterns(line, pseudo_str_continuation)
                line = replace_comment_patterns(line, pseudo_str_continuation)
                line = upcase_keywords(line, pseudo_str_continuation)
        else:
            line = replace_patterns(line, str_continuation)
            line = replace_comment_patterns(line, str_continuation)
            line = upcase_keywords(line, str_continuation)

        # Check for the start of new pre-processor continuation
        if is_pp_continuation(line) and not pp_continuation:
            pp_continuation = True

        # test the line continuation properties of this line
        if pp_continuation:
            if not is_pp_continuation(line):
                pp_continuation = False
                pp_line_previous = ""
                pseudo_comment = False
        elif continuation:
            if is_continuation(line, str_continuation):
                # check if still string continuation
                str_continuation = is_str_continuation(line, str_continuation)
            else:
                continuation = False
                line_previous = ""
                str_continuation = [False, False]
        else:
            # Finally, detect if the following line is a continuation
            # of this one (and therefore requires no indentation)
            if is_continuation(line, str_continuation):
                continuation = True
                str_continuation = is_str_continuation(line, str_continuation)

        # if we are a (pp) continuation, save the partial line
        if pp_continuation:
            pp_line_previous = ''.join([re.sub(r"\\\s*$", "",
                                               pp_line_previous),
                                        re.sub(r"&\s*$", "", line_previous),
                                        line])
            line_previous = ""
            pseudo_line = re.sub(r"\\\s*$", "&", pp_line_previous)
            pseudo_str_continuation = is_str_continuation(pseudo_line,
                                                          str_continuation)
            if not pseudo_comment:
                pseudo_line = partial_blank_fstring(pseudo_line,
                                                    str_continuation)
                if pseudo_line.strip()[0] == "#":
                    pseudo_comment = True
                if pseudo_line.find("!") != -1:
                    pseudo_line = blank_fcomments(pseudo_line,
                                                  str_continuation)
                    if pseudo_line.find("!") == -1:
                        pseudo_comment = True
        elif continuation:
            line_previous = re.sub(r"&\s*$", "", line_previous)
            line_previous += blank_fcomments(line, str_continuation)

        output_lines.append(line)

    return output_lines


def main():
    '''Main toplevel function for testing'''
    input_file = sys.argv[-1]
    with open(input_file, "r+") as file_in:
        print("Styling "+input_file)
        lines_in = file_in.read().split("\n")
        new_lines = apply_styling(lines_in)
        file_in.seek(0)
        file_in.write("\n".join(new_lines))
        file_in.truncate()


if __name__ == '__main__':
    main()
