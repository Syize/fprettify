import io
import re
import shlex
import sys

from . import re as re_str
from ._cli import get_arg_parser, process_args
from .exception import FprettifyInternalException, FprettifyParseException
from .indenter import F90Indenter, build_scope_parser
from .log import log_message
from .parser import parse_fprettify_directives
from .scanner import CharFilter, InputStream
from .utils import get_curr_delim, inspect_ffile_format, preprocess_labels


def format_single_fline(
    f_line,
    whitespace,
    whitespace_dict: dict[str, bool],
    linebreak_pos,
    ampersand_sep,
    scope_parser,
    format_decl,
    filename,
    line_nr,
    auto_format=True,
):
    """
    format a single Fortran line - imposes white space formatting
    and inserts linebreaks.
    Takes a logical Fortran line `f_line` as input as well as the positions
    of the linebreaks (`linebreak_pos`), and the number of
    separating whitespace characters before ampersand (`ampersand_sep`).
    `filename` and `line_nr` just for error messages.
    The higher `whitespace`, the more white space characters inserted -
    whitespace = 0, 1, 2, 3 are currently supported.
    whitespace formatting can additionally controlled more fine-grained
    via a dictionary of bools (whitespace_dict)
    auto formatting can be turned off by setting `auto_format` to False.
    """

    # define whether to put whitespaces around operators:
    mapping = {
        "comma": 0,  # 0: comma, semicolon
        "assignments": 1,  # 1: assignment operators
        "relational": 2,  # 2: relational operators
        "logical": 3,  # 3: logical operators
        "plusminus": 4,  # 4: arithm. operators plus and minus
        "multdiv": 5,  # 5: arithm. operators multiply and divide
        "print": 6,  # 6: print / read statements
        "type": 7,  # 7: select type components
        "intrinsics": 8,  # 8: intrinsics
        "decl": 9,  # 9: declarations
        "concat": 10,  # 10: string concatenation
    }

    if whitespace == 0:
        spacey = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    elif whitespace == 1:
        spacey = [1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0]
    elif whitespace == 2:
        spacey = [1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0]
    elif whitespace == 3:
        spacey = [1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0]
    elif whitespace == 4:
        spacey = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    else:
        raise NotImplementedError("unknown value for whitespace")

    if whitespace_dict:
        # iterate over dictionary and override settings for 'spacey'
        for key, value in mapping.items():
            if whitespace_dict[key] is not None:
                spacey[value] = 1 if whitespace_dict[key] else 0

    line = f_line
    line_orig = line

    if auto_format:

        line = rm_extra_whitespace(line, format_decl)
        line = add_whitespace_charwise(
            line, spacey, scope_parser, format_decl, filename, line_nr
        )
        line = add_whitespace_context(line, spacey)

    lines_out = split_reformatted_line(
        line_orig, linebreak_pos, ampersand_sep, line, filename, line_nr
    )
    
    return lines_out


def rm_extra_whitespace(line, format_decl):
    """rm all unneeded whitespace chars, except for declarations"""
    line_ftd = ""
    pos_prev = -1
    pos = -1
    for pos, char in CharFilter(line):
        if format_decl:
            is_decl = False
        else:
            is_decl = line[pos:].lstrip().startswith("::") or line[
                :pos
            ].rstrip().endswith("::")

        if pos > pos_prev + 1:  # skipped string
            line_ftd = line_ftd + line[pos_prev + 1 : pos]

        if char == " ":
            # remove double spaces:
            if line_ftd and (re.search(r"[\w]", line_ftd[-1]) or is_decl):
                line_ftd = line_ftd + char
        else:
            if (
                line_ftd
                and line_ftd[-1] == " "
                and (not re.search(r"[\w]", char) and not is_decl)
            ):
                line_ftd = line_ftd[:-1]  # remove spaces except between words
            line_ftd = line_ftd + char
        pos_prev = pos

    line_ftd = line_ftd + line[pos + 1 :]
    return line_ftd


def add_whitespace_charwise(line, spacey, scope_parser, format_decl, filename, line_nr):
    """add whitespace character wise (no need for context aware parsing)"""
    line_ftd = line
    pos_eq = []
    end_of_delim = -1
    level = 0
    for pos, char in CharFilter(line):
        # offset w.r.t. unformatted line
        offset = len(line_ftd) - len(line)

        # format delimiters
        what_del_open = None
        what_del_close = None
        if pos > end_of_delim:
            [what_del_open, what_del_close] = get_curr_delim(line, pos)

        if what_del_open or what_del_close:
            sep1 = 0
            sep2 = 0

            if what_del_open:
                delim = what_del_open.group()
            else:
                delim = what_del_close.group()

            lhs = line_ftd[: pos + offset]
            rhs = line_ftd[pos + len(delim) + offset :]

            # format opening delimiters
            if what_del_open:
                level += 1  # new scope
                # add separating whitespace before opening delimiter
                # with some exceptions:
                # FIXME: duplication of regex, better to include them into
                # INTR_STMTS_PAR
                if (
                    (
                        not re.search(
                            (r"(" + re_str.DEL_OPEN_STR + r"|[\w\*/=\+\-:])\s*$"),
                            line[:pos],
                            re_str.RE_FLAGS,
                        )
                        and not re_str.EMPTY_RE.search(line[:pos])
                    )
                    or re.search(
                        re_str.SOL_STR + r"(\w+\s*:)?(ELSE)?\s*IF\s*$", line[:pos], re_str.RE_FLAGS
                    )
                    or re.search(
                        re_str.SOL_STR + r"(\w+\s*:)?\s*DO\s+WHILE\s*$", line[:pos], re_str.RE_FLAGS
                    )
                    or re.search(
                        re_str.SOL_STR + r"(SELECT)?\s*CASE\s*$", line[:pos], re_str.RE_FLAGS
                    )
                    or re.search(
                        re_str.SOL_STR + r"(SELECT)?\s*RANK\s*$", line[:pos], re_str.RE_FLAGS
                    )
                    or re.search(re_str.SOL_STR + r"SELECT\s*TYPE\s*$", line[:pos], re_str.RE_FLAGS)
                    or re.search(re_str.SOL_STR + r"CLASS\s*DEFAULT\s*$", line[:pos], re_str.RE_FLAGS)
                    or re.search(
                        re_str.SOL_STR + r"(TYPE|CLASS)\s+IS\s*$", line[:pos], re_str.RE_FLAGS
                    )
                    or re.search(
                        r"(?<!%)\b" + re_str.INTR_STMTS_PAR + r"\s*$", line[:pos], re_str.RE_FLAGS
                    )
                ):
                    sep1 = 1 * spacey[8]

            # format closing delimiters
            else:
                if level > 0:
                    level += -1  # close scope
                else:
                    log_message(
                        "unpaired bracket delimiters", "info", filename, line_nr
                    )

                # add separating whitespace after closing delimiter
                # with some exceptions:
                if not re.search(
                    r"^\s*(" + re_str.DEL_CLOSE_STR + r"|[,%:/\*])", line[pos + 1 :], re_str.RE_FLAGS
                ):
                    sep2 = 1
                elif re.search(r"^\s*::", line[pos + 1 :], re_str.RE_FLAGS):
                    sep2 = len(rhs) - len(rhs.lstrip(" ")) if not format_decl else 1

            # where delimiter token ends
            end_of_delim = pos + len(delim) - 1

            line_ftd = (
                lhs.rstrip(" ") + " " * sep1 + delim + " " * sep2 + rhs.lstrip(" ")
            )

        # format commas and semicolons
        if char in [",", ";"]:
            lhs = line_ftd[: pos + offset]
            rhs = line_ftd[pos + 1 + offset :]
            line_ftd = lhs.rstrip(" ") + char + " " * spacey[0] + rhs.lstrip(" ")
            line_ftd = line_ftd.rstrip(" ")

        # format type selector %
        if char == "%":
            lhs = line_ftd[: pos + offset]
            rhs = line_ftd[pos + 1 + offset :]
            line_ftd = (
                lhs.rstrip(" ")
                + " " * spacey[7]
                + char
                + " " * spacey[7]
                + rhs.lstrip(" ")
            )
            line_ftd = line_ftd.rstrip(" ")

        # format string concatenation operator '//'
        if (
            char == "/"
            and line[pos : pos + 2] == "//"
            and (pos == 0 or line[pos - 1] != "/")
            and level == 0
            and pos > end_of_delim
        ):
            lhs = line_ftd[: pos + offset]
            rhs = line_ftd[pos + 2 + offset :]
            line_ftd = (
                lhs.rstrip(" ")
                + " " * spacey[10]
                + "//"
                + " " * spacey[10]
                + rhs.lstrip(" ")
            )

        # format '::'
        if format_decl and line[pos : pos + 2] == "::":
            lhs = line_ftd[: pos + offset]
            rhs = line_ftd[pos + 2 + offset :]
            line_ftd = (
                lhs.rstrip(" ")
                + " " * spacey[9]
                + "::"
                + " " * spacey[9]
                + rhs.lstrip(" ")
            )
            line_ftd = line_ftd.rstrip(" ")

        # format .NOT.
        if re.search(r"^\.NOT\.", line[pos : pos + 5], re_str.RE_FLAGS):
            lhs = line_ftd[: pos + offset]
            rhs = line_ftd[pos + 5 + offset :]
            line_ftd = (
                lhs.rstrip(" ")
                + line[pos : pos + 5]
                + " " * spacey[3]
                + rhs.lstrip(" ")
            )

        # strip whitespaces from '=' and prepare assignment operator
        # formatting:
        if char == "=" and not re_str.REL_OP_RE.search(line[pos - 1 : pos + 2]):
            is_pointer = line[pos + 1] == ">" if pos + 1 < len(line) else False
            lhs = line_ftd[: pos + offset]
            rhs = line_ftd[pos + 1 + is_pointer + offset :]
            assign_op = "=" + ">" * is_pointer
            line_ftd = lhs.rstrip(" ") + assign_op + rhs.lstrip(" ")
            if (not level) or is_pointer:  # remember position of assignment operator
                pos_eq.append(len(lhs.rstrip(" ")))

    line = line_ftd

    for pos in pos_eq:
        offset = len(line_ftd) - len(line)
        is_pointer = line[pos + 1] == ">" if pos + 1 < len(line) else False
        lhs = line_ftd[: pos + offset]
        rhs = line_ftd[pos + 1 + is_pointer + offset :]
        assign_op = "=" + ">" * is_pointer
        line_ftd = (
            lhs.rstrip(" ")
            + " " * spacey[1]
            + assign_op
            + " " * spacey[1]
            + rhs.lstrip(" ")
        )
        # offset w.r.t. unformatted line

    is_end = False
    if re_str.END_RE.search(line_ftd):
        for endre in scope_parser["end"]:
            if endre and endre.search(line_ftd):
                is_end = True
    if is_end:
        line_ftd = re_str.END_RE.sub(r"\1" + " " * spacey[8] + r"\2", line_ftd)

    if level != 0:
        log_message("unpaired bracket delimiters", "info", filename, line_nr)

    return line_ftd


def add_whitespace_context(line, spacey):
    """
    for context aware whitespace formatting we extract line parts that are
    not comments or strings in order to be able to apply a context aware regex.
    """

    pos_prev = -1
    pos = -1
    line_parts = [""]
    for pos, char in CharFilter(line):
        if pos > pos_prev + 1:  # skipped string
            line_parts.append(line[pos_prev + 1 : pos].strip())  # append string
            line_parts.append("")

        line_parts[-1] += char

        pos_prev = pos

    if pos + 1 < len(line):
        line_parts.append(line[pos + 1 :])

    # format namelists with spaces around /
    if re_str.NML_STMT_RE.match(line):
        for pos, part in enumerate(line_parts):
            # exclude comments, strings:
            if not re_str.STR_OPEN_RE.match(part):
                partsplit = re_str.NML_RE.split(part)
                line_parts[pos] = " ".join(partsplit)

    # Two-sided operators
    for n_op, lr_re in enumerate(re_str.LR_OPS_RE):
        for pos, part in enumerate(line_parts):
            # exclude comments, strings:
            if not re_str.STR_OPEN_RE.match(part):
                # also exclude / if we see a namelist and data statement
                if not (
                    re_str.NML_STMT_RE.match(line)
                    or re_str.DATA_STMT_RE.match(line)
                    or re_str.CUDA_CHEVRONS_RE.search(line)
                ):
                    partsplit = lr_re.split(part)
                    line_parts[pos] = (" " * spacey[n_op + 2]).join(partsplit)

    line = "".join(line_parts)

    for newre in [re_str.IF_RE, re_str.DO_RE, re_str.BLK_RE]:
        if newre.search(line) and re.search(re_str.SOL_STR + r"\w+\s*:", line):
            line = ": ".join(_.strip() for _ in line.split(":", 1))

    # format ':' for labels and use only statements
    if re_str.USE_RE.search(line):
        line = re.sub(
            r"(only)\s*:\s*", r"\g<1>:" + " " * spacey[0], line, flags=re_str.RE_FLAGS
        )

    return line


def split_reformatted_line(
    line_orig, linebreak_pos_orig, ampersand_sep, line, filename, line_nr
):
    """
    Infer linebreak positions of formatted line from linebreak positions in
    original line and split line.
    """
    # shift line break positions from original to reformatted line
    pos_new = 0
    pos_old = 0
    linebreak_pos_orig.sort(reverse=True)
    linebreak_pos_ftd = []
    while 1:

        if pos_new == len(line) or pos_old == len(line_orig):
            break

        if line[pos_new] != line_orig[pos_old]:
            raise FprettifyInternalException(
                "failed at finding line break position", filename, line_nr
            )

        if linebreak_pos_orig and pos_old > linebreak_pos_orig[-1]:
            linebreak_pos_orig.pop()
            linebreak_pos_ftd.append(pos_new)
            continue

        pos_new += 1
        while pos_new < len(line) and line[pos_new] == " ":
            pos_new += 1

        pos_old += 1
        while pos_old < len(line_orig) and line_orig[pos_old] == " ":
            pos_old += 1

    linebreak_pos_ftd.insert(0, 0)

    # We split line into parts and we insert ampersands at line end, but not
    # for empty lines and comment lines
    lines_split = [
        (line[l:r].rstrip(" ") + " " * ampersand_sep[pos] + "&" * min(1, r - l))
        for pos, (l, r) in enumerate(
            zip(linebreak_pos_ftd[0:-1], linebreak_pos_ftd[1:])
        )
    ]

    lines_split.append(line[linebreak_pos_ftd[-1] :])

    return lines_split


def reformat_inplace(
    filename, stdout=False, diffonly=False, **kwargs
):  # pragma: no cover
    """reformat a file in place."""
    if filename == "-":
        infile = io.StringIO()
        infile.write(sys.stdin.read())
    else:
        infile = io.open(filename, "r", encoding="utf-8")

    newfile = io.StringIO()

    # check fprettify annotations overriding any previously parsed options
    infile.seek(0)
    arg_parser = get_arg_parser()
    annotated_args = {}
    for line in infile:
        match = re_str.FPRETTIY_ANNOTATION_RE.search(line)
        if match:
            if annotated_args:
                log_message(
                    "Ignoring subsequent '! fprettify: ...' comments within same file."
                )
                continue

            args_tmp = arg_parser.parse_args(shlex.split(match.group(1)))
            annotated_args = process_args(args_tmp)

    kwargs.update(annotated_args)

    reformat_ffile(infile, newfile, orig_filename=filename, **kwargs)

    if diffonly:
        infile.seek(0)
        newfile.seek(0)
        diff_contents = diff(infile.read(), newfile.read(), filename, filename)
        sys.stdout.write(diff_contents)
    else:

        if stdout:
            sys.stdout.write(newfile.getvalue())
        else:
            outfile = io.open(filename, "r", encoding="utf-8")

            # write to outfile only if content has changed

            import hashlib

            hash_new = hashlib.md5()
            hash_new.update(newfile.getvalue().encode("utf-8"))
            hash_old = hashlib.md5()
            hash_old.update(outfile.read().encode("utf-8"))

            outfile.close()

            if hash_new.digest() != hash_old.digest():
                outfile = io.open(filename, "w", encoding="utf-8")
                outfile.write(newfile.getvalue())


def reformat_ffile(
    infile,
    outfile,
    impose_indent=True,
    indent_size=3,
    strict_indent=False,
    impose_whitespace=True,
    case_dict={},
    impose_replacements=False,
    cstyle=False,
    whitespace=2,
    whitespace_dict={},
    llength=132,
    strip_comments=False,
    comment_spacing=1,
    format_decl=False,
    orig_filename=None,
    indent_fypp=True,
    indent_mod=True,
):
    """main method to be invoked for formatting a Fortran file."""

    # note: whitespace formatting and indentation may require different parsing rules
    # (e.g. preprocessor statements may be indented but not whitespace formatted)
    # therefore we invoke reformat_ffile independently for:
    # 1) whitespace formatting
    # 2) indentation

    # print("\n")
    # print(whitespace)
    # print(whitespace_dict)
    # exit(1)

    if not orig_filename:
        orig_filename = infile.name

    # 1) whitespace formatting
    oldfile = infile
    newfile = infile

    if impose_whitespace:
        _impose_indent = False

        newfile = io.StringIO()
        reformat_ffile_combined(
            oldfile,
            newfile,
            _impose_indent,
            indent_size,
            strict_indent,
            impose_whitespace,
            case_dict,
            impose_replacements,
            cstyle,
            whitespace,
            whitespace_dict,
            llength,
            strip_comments,
            comment_spacing,
            format_decl,
            orig_filename,
            indent_fypp,
            indent_mod,
        )
        oldfile = newfile

    # 2) indentation
    if impose_indent:

        _impose_whitespace = False
        _impose_replacements = False

        newfile = io.StringIO()
        reformat_ffile_combined(
            oldfile,
            newfile,
            impose_indent,
            indent_size,
            strict_indent,
            _impose_whitespace,
            case_dict,
            _impose_replacements,
            cstyle,
            whitespace,
            whitespace_dict,
            llength,
            strip_comments,
            comment_spacing,
            format_decl,
            orig_filename,
            indent_fypp,
            indent_mod,
        )
    outfile.write(newfile.getvalue())


def reformat_ffile_combined(
    infile,
    outfile,
    impose_indent=True,
    indent_size=3,
    strict_indent=False,
    impose_whitespace=True,
    case_dict={},
    impose_replacements=False,
    cstyle=False,
    whitespace=2,
    whitespace_dict={},
    llength=132,
    strip_comments=False,
    comment_spacing=1,
    format_decl=False,
    orig_filename=None,
    indent_fypp=True,
    indent_mod=True,
):

    if not orig_filename:
        orig_filename = infile.name

    if not impose_indent:
        indent_fypp = False

    infile.seek(0)
    req_indents, first_indent, has_fypp = inspect_ffile_format(
        infile, indent_size, strict_indent, indent_fypp, orig_filename
    )
    infile.seek(0)

    if not has_fypp:
        indent_fypp = False

    scope_parser = build_scope_parser(fypp=indent_fypp, mod=indent_mod)

    # initialization

    # special cases for indentation:
    # indent_special = 0: parse syntax and impose indent
    # indent_special = 1: no indentation
    # indent_special = 2: use indent from previous line
    # indent_special = 3: take indent from input file (leave as is)
    indent_special = 0

    if impose_indent:
        indenter = F90Indenter(scope_parser, first_indent, indent_size, orig_filename)
    else:
        indent_special = 3

    impose_case = not all(v == 0 for v in case_dict.values())

    nfl = 0  # fortran line counter
    use_same_line = False
    stream = InputStream(infile, not indent_fypp, orig_filename=orig_filename)
    skip_blank = False
    in_format_off_block = False

    while 1:
        f_line, comments, lines = stream.next_fortran_line()

        if not lines:
            break

        nfl += 1
        orig_lines = lines

        f_line, lines, is_omp_conditional = preprocess_omp(f_line, lines)
        f_line, lines, label = preprocess_labels(f_line, lines)

        if indent_special != 3:
            indent = [0] * len(lines)
        else:
            indent = [len(l) - len((l.lstrip(" ")).lstrip("&")) for l in lines]

        comment_lines = format_comments(
            lines, comments, strip_comments, comment_spacing
        )

        auto_align, auto_format, in_format_off_block = parse_fprettify_directives(
            lines, comment_lines, in_format_off_block, orig_filename, stream.line_nr
        )

        lines, do_format, prev_indent, is_blank, is_special = preprocess_line(
            f_line, lines, comments, orig_filename, stream.line_nr, indent_fypp
        )

        if is_special[0]:
            indent_special = 3

        if prev_indent and indent_special == 0:
            indent_special = 2

        if is_blank and skip_blank:
            continue
        if not do_format:
            if indent_special == 2:
                # inherit indent from previous line
                indent[:] = [indenter.get_fline_indent()] * len(indent)
            elif indent_special == 0:
                indent_special = 1
        else:

            if not auto_align:
                manual_lines_indent = get_manual_alignment(lines)
            else:
                manual_lines_indent = []

            lines, pre_ampersand, ampersand_sep = remove_pre_ampersands(
                lines, is_special, orig_filename, stream.line_nr
            )

            linebreak_pos = get_linebreak_pos(
                lines, not indent_fypp, orig_filename, stream.line_nr
            )

            f_line = f_line.strip(" ")

            if impose_replacements:
                f_line = replace_relational_single_fline(f_line, cstyle)

            if impose_case:
                f_line = replace_keywords_single_fline(f_line, case_dict)

            if impose_whitespace:
                lines = format_single_fline(
                    f_line,
                    whitespace,
                    whitespace_dict,
                    linebreak_pos,
                    ampersand_sep,
                    scope_parser,
                    format_decl,
                    orig_filename,
                    stream.line_nr,
                    auto_format,
                )

                lines = append_comments(lines, comment_lines, is_special)

            # target indent for next line
            rel_indent = req_indents[nfl] if nfl < len(req_indents) else 0

            # print(f"[formatter] LINE: {f_line}")
            # print(f"[formatter] rel_indent: {rel_indent}")

            if indent_special != 3:
                indenter.process_lines_of_fline(
                    f_line,
                    lines,
                    rel_indent,
                    indent_size,
                    stream.line_nr,
                    indent_fypp,
                    manual_lines_indent,
                )
                indent = indenter.get_lines_indent()

            lines, indent = prepend_ampersands(lines, indent, pre_ampersand)

        if any(is_special):
            for pos, line in enumerate(lines):
                if is_special[pos]:
                    indent[pos] = len(line) - len(line.lstrip(" "))
                    lines[pos] = line.lstrip(" ")

        lines = remove_trailing_whitespace(lines)

        # need to shift indents if label wider than first indent
        if label and impose_indent:
            if indent[0] < len(label):
                indent = [ind + len(label) - indent[0] for ind in indent]

        allow_auto_split = auto_format and (impose_whitespace or impose_indent)
        write_formatted_line(
            outfile,
            indent,
            lines,
            orig_lines,
            indent_special,
            indent_size,
            llength,
            use_same_line,
            is_omp_conditional,
            label,
            orig_filename,
            stream.line_nr,
            allow_split=allow_auto_split,
        )

        # rm subsequent blank lines
        skip_blank = (
            re_str.EMPTY_RE.search(f_line)
            and not any(comments)
            and not is_omp_conditional
            and not label
            and not use_same_line
        )

        do_indent, use_same_line = pass_defaults_to_next_line(f_line)

        if impose_indent:
            if do_indent:
                indent_special = 0
            else:
                indent_special = 1


def format_comments(lines, comments, strip_comments, comment_spacing=1):
    comments_ftd = []
    for line, comment in zip(lines, comments):
        has_comment = bool(comment.strip())
        if has_comment:
            if strip_comments:
                sep = 0 if comment.strip() == line.strip() else comment_spacing
            else:
                line_minus_comment = line.replace(comment, "")
                sep = len(line_minus_comment.rstrip("\n")) - len(
                    line_minus_comment.rstrip()
                )
        else:
            sep = 0

        if line.strip():  # empty lines between linebreaks are ignored
            comments_ftd.append(" " * sep + comment.strip())
    return comments_ftd


def preprocess_omp(f_line, lines):
    """convert omp conditional to normal fortran"""

    is_omp_conditional = bool(re_str.OMP_COND_RE.search(f_line))
    if is_omp_conditional:
        f_line = re_str.OMP_COND_RE.sub("   ", f_line, count=1)
        lines = [re_str.OMP_COND_RE.sub("   ", l, count=1) for l in lines]

    return [f_line, lines, is_omp_conditional]


def preprocess_line(f_line, lines, comments, filename, line_nr, indent_fypp):
    """preprocess lines: identification and formatting of special cases"""
    is_blank = False
    prev_indent = False
    do_format = False

    # is_special: special directives that should not be treated as Fortran
    # currently supported: fypp preprocessor directives or comments for FORD documentation
    is_special = [False] * len(lines)

    for pos, line in enumerate(lines):
        line_strip = line.lstrip()
        if indent_fypp:
            is_special[pos] = line_strip.startswith("!!") or (
                re_str.FYPP_LINE_RE.search(line_strip) if pos > 0 else False
            )
        else:
            is_special[pos] = re_str.FYPP_LINE_RE.search(line_strip) or line_strip.startswith(
                "!!"
            )

    # if first line is special, all lines should be special
    if is_special[0]:
        is_special = [True] * len(lines)

    if re_str.EMPTY_RE.search(f_line):  # empty lines including comment lines
        if any(comments):
            if lines[0].startswith(" ") and not re_str.OMP_DIR_RE.search(lines[0]):
                # indent comment lines only if they were not indented before.
                prev_indent = True
        else:
            is_blank = True
        lines = [l.strip(" ") if not is_special[n] else l for n, l in enumerate(lines)]
    else:
        do_format = True

    return [lines, do_format, prev_indent, is_blank, is_special]


def pass_defaults_to_next_line(f_line):
    """defaults to be transferred from f_line to next line"""
    if re.search(r";\s*$", f_line, re_str.RE_FLAGS):
        # if line ended with semicolon, don't indent next line
        do_indent = False
        use_same_line = True
    else:
        do_indent = True
        use_same_line = False

    return [do_indent, use_same_line]


def remove_trailing_whitespace(lines):
    """remove trailing whitespaces from lines"""
    lines = [re.sub(r"\s+$", "\n", l, re_str.RE_FLAGS) for l in lines]
    return lines


def prepend_ampersands(lines, indent, pre_ampersand):
    """prepend ampersands and correct indent"""
    for pos, line in enumerate(lines):
        amp_insert = pre_ampersand[pos]
        if amp_insert:
            indent[pos] += -1
            lines[pos] = amp_insert + line.lstrip()

    return [lines, indent]


def append_comments(lines, comment_lines, is_special):
    """append comments to lines"""
    for pos, (line, comment) in enumerate(zip(lines, comment_lines)):
        if pos < len(lines) - 1:
            has_nl = True  # has next line
            if not line.strip() and not is_special[pos]:
                comment = comment.lstrip()
        else:
            has_nl = not re.search(re_str.EOL_SC, line)
        lines[pos] = lines[pos].rstrip(" ") + comment + "\n" * has_nl

    return lines


def get_linebreak_pos(lines, filter_fypp, filename, line_nr):
    """extract linebreak positions in Fortran line from lines"""
    linebreak_pos = []
    if filter_fypp:
        notfortran_re = re_str.NOTFORTRAN_LINE_RE
    else:
        notfortran_re = re_str.NOTFORTRAN_FYPP_LINE_RE

    for line in lines:
        found = None
        for char_pos, _ in CharFilter(line, filter_strings=False):
            if re.match(re_str.LINEBREAK_STR, line[char_pos:], re_str.RE_FLAGS):
                found = char_pos
        if found:
            if re.search("&&", line, re_str.RE_FLAGS):
                raise FprettifyParseException(
                    "Non-standard expression involving '&&'", filename, line_nr
                )
            linebreak_pos.append(found)
        elif notfortran_re.search(line.lstrip(" ")):
            linebreak_pos.append(0)

    linebreak_pos = [
        sum(linebreak_pos[0 : _ + 1]) - 1 for _ in range(0, len(linebreak_pos))
    ]

    return linebreak_pos


def remove_pre_ampersands(lines, is_special, filename, line_nr):
    """
    remove and return preceding ampersands ('pre_ampersand'). Also return
    number of whitespace characters before ampersand of previous line
    ('ampersand_sep').

    Note: Don't do any whitespace formatting on ampersands if next line starts
    with an ampersand but remember the original number of spaces
    (ampersand_sep). This "special rule" is necessary since ampersands starting
    a line can be used to break literals, so changing the number of whitespaces
    before the ampersand ending the previous line may lead to invalid syntax or
    may change the number of whitespace characters in a string.
    """
    pre_ampersand = []
    ampersand_sep = []

    for pos, line in enumerate(lines):
        match = re.search(re_str.SOL_STR + r"(&\s*)", line)
        if match:
            pre_ampersand.append(match.group(1))
            # amount of whitespace before ampersand of previous line:
            m = re.search(r"(\s*)&[\s]*(?:!.*)?$", lines[pos - 1])
            if not m:
                raise FprettifyParseException(
                    "Bad continuation line format", filename, line_nr
                )
            sep = len(m.group(1))

            ampersand_sep.append(sep)
        else:
            pre_ampersand.append("")
            if pos > 0:
                # use default 1 whitespace character before ampersand
                ampersand_sep.append(1)

    lines = [l.strip(" ").strip("&") if not s else l for l, s in zip(lines, is_special)]
    return [lines, pre_ampersand, ampersand_sep]


def get_manual_alignment(lines):
    """extract manual indents for line continuations from line"""
    manual_lines_indent = [len(l) - len(l.lstrip(" ").lstrip("&")) for l in lines]
    manual_lines_indent = [ind - manual_lines_indent[0] for ind in manual_lines_indent]
    return manual_lines_indent


def _find_split_position(text, max_width):
    """
    Locate a suitable breakpoint (prefer whitespace or comma) within max_width.
    Returns None if no such breakpoint exists.
    """
    if max_width < 1:
        return None
    search_limit = min(len(text) - 1, max_width)
    if search_limit < 0:
        return None

    spaces = []
    commas = []

    for pos, char in CharFilter(text):
        if pos > search_limit:
            break
        if char == " ":
            spaces.append(pos)
        elif char == ",":
            commas.append(pos)

    for candidate in reversed(spaces):
        if len(text) - candidate >= 12:
            return candidate

    for candidate in reversed(commas):
        if len(text) - candidate > 4:
            return candidate + 1

    return None


def _auto_split_line(line, ind_use, llength, indent_size):
    """
    Attempt to split a long logical line into continuation lines that
    respect the configured line-length limit. Returns a list of new line
    fragments when successful, otherwise None.
    """
    if llength < 40:
        return None

    stripped = line.lstrip(" ")
    if not stripped:
        return None
    if stripped.startswith("&"):
        return None
    line_has_newline = stripped.endswith("\n")
    if line_has_newline:
        stripped = stripped[:-1]

    has_comment = False
    for _, char in CharFilter(stripped, filter_comments=False):
        if char == "!":
            has_comment = True
            break
    if has_comment:
        return None

    max_first = llength - ind_use - 2  # reserve for trailing ampersand
    if max_first <= 0:
        return None

    break_pos = _find_split_position(stripped, max_first)
    if break_pos is None or break_pos >= len(stripped):
        return None

    remainder = stripped[break_pos:].lstrip()
    if not remainder:
        return None

    first_chunk = stripped[:break_pos].rstrip()
    new_lines = [first_chunk + " &"]

    current_indent = ind_use + indent_size
    current = remainder

    while current:
        available = llength - current_indent
        if available <= 0:
            return None

        # final chunk (fits without ampersand)
        if len(current) + 2 <= available:
            new_lines.append(current)
            break

        split_limit = available - 2  # account for ' &' suffix
        if split_limit <= 0:
            return None

        cont_break = _find_split_position(current, split_limit)
        if cont_break is None or cont_break >= len(current):
            return None

        chunk = current[:cont_break].rstrip()
        if not chunk:
            return None
        new_lines.append(chunk + " &")
        current = current[cont_break:].lstrip()

    if line_has_newline:
        new_lines = [chunk.rstrip("\n") + "\n" for chunk in new_lines]

    return new_lines


def _insert_split_chunks(idx, split_lines, indent, indent_size, lines, orig_lines):
    """Replace the original line at `idx` with its split chunks and matching indents."""
    base_indent = indent[idx]
    indent.pop(idx)
    lines.pop(idx)
    orig_lines.pop(idx)

    follow_indent = base_indent + indent_size
    new_indents = [base_indent] + [follow_indent] * (len(split_lines) - 1)

    for new_line, new_indent in reversed(list(zip(split_lines, new_indents))):
        lines.insert(idx, new_line)
        indent.insert(idx, new_indent)
        orig_lines.insert(idx, new_line)


def _split_inline_comment(line):
    """Return (code, comment) strings if line contains a detachable inline comment."""
    if "!" not in line:
        return None

    has_newline = line.endswith("\n")
    body = line[:-1] if has_newline else line

    comment_pos = None
    for pos, _ in CharFilter(body, filter_comments=False):
        if body[pos] == "!":
            comment_pos = pos
            break
    if comment_pos is None:
        return None

    code = body[:comment_pos].rstrip()
    comment = body[comment_pos:].lstrip()

    if not code or not comment:
        return None

    if has_newline:
        code += "\n"
        comment += "\n"

    return code, comment


def _detach_inline_comment(idx, indent, lines, orig_lines):
    """Split an inline comment into its own line keeping indentation metadata."""
    splitted = _split_inline_comment(lines[idx])
    if not splitted:
        return False

    code_line, comment_line = splitted
    base_indent = indent[idx]

    lines[idx] = code_line
    orig_lines[idx] = code_line

    indent.insert(idx + 1, base_indent)
    lines.insert(idx + 1, comment_line)
    orig_lines.insert(idx + 1, comment_line)

    return True


def write_formatted_line(
    outfile,
    indent,
    lines,
    orig_lines,
    indent_special,
    indent_size,
    llength,
    use_same_line,
    is_omp_conditional,
    label,
    filename,
    line_nr,
    allow_split,
):
    """Write reformatted line to file"""

    idx = 0
    while idx < len(lines):
        ind = indent[idx]
        line = lines[idx]
        orig_line = orig_lines[idx]

        # get actual line length excluding comment:
        line_length = 0
        for line_length, _ in CharFilter(line):
            pass
        line_length += 1

        if indent_special != 1:
            ind_use = ind
        else:
            if use_same_line:
                ind_use = 1
            else:
                ind_use = 0

        if re_str.CPP_RE.search(line.lstrip()):
            ind_use = 0

        if label:
            label_use = label
            label = ""  # no label for continuation lines
        else:
            label_use = ""

        padding = (
            ind_use
            - 3 * is_omp_conditional
            - len(label_use)
            + len(line)
            - len(line.lstrip(" "))
        )
        padding = max(0, padding)

        stripped_line = line.lstrip(" ")
        rendered_length = len(
            "!$ " * is_omp_conditional
            + label_use
            + " " * padding
            + stripped_line.rstrip("\n")
        )

        needs_split = allow_split and rendered_length > llength

        if needs_split:
            split_lines = _auto_split_line(line, ind_use, llength, indent_size)
            if split_lines:
                _insert_split_chunks(
                    idx, split_lines, indent, indent_size, lines, orig_lines
                )
                continue
            if _detach_inline_comment(idx, indent, lines, orig_lines):
                continue

        if rendered_length <= llength:
            outfile.write(
                "!$ " * is_omp_conditional + label_use + " " * padding + stripped_line
            )
        elif line_length <= (llength + 1):
            # Recompute padding to right-align at the line length limit
            padding_overflow = (
                (llength + 1)
                - 3 * is_omp_conditional
                - len(label_use)
                - len(line.lstrip(" "))
            )
            padding_overflow = max(0, padding_overflow)
            outfile.write(
                "!$ " * is_omp_conditional
                + label_use
                + " " * padding_overflow
                + line.lstrip(" ")
            )

            log_message(
                re_str.LINESPLIT_MESSAGE + " (limit: " + str(llength) + ")",
                "warning",
                filename,
                line_nr,
            )
        else:
            outfile.write(orig_line)
            log_message(
                re_str.LINESPLIT_MESSAGE + " (limit: " + str(llength) + ")",
                "warning",
                filename,
                line_nr,
            )

        if label:
            label = ""

        idx += 1


def replace_relational_single_fline(f_line, cstyle):
    """
    format a single Fortran line - replaces scalar relational
    operators in logical expressions to either Fortran or C-style.
    .lt.  <-->  <
    .le.  <-->  <=
    .gt.  <-->  >
    .ge.  <-->  >=
    .eq.  <-->  ==
    .ne.  <-->  /=
    """

    new_line = f_line

    # only act on lines that do contain a relation
    if re_str.REL_OP_RE.search(f_line):
        # check that relation is not inside quotes, a string, or commented
        # (think of underlining a heading with === or things like markup being printed which we do not replace)
        pos_prev = -1
        pos = -1
        line_parts = [""]
        for pos, char in CharFilter(f_line):
            if pos > pos_prev + 1:  # skipped string
                line_parts.append(f_line[pos_prev + 1 : pos].strip())  # append string
                line_parts.append("")

            line_parts[-1] += char

            pos_prev = pos

        if pos + 1 < len(f_line):
            line_parts.append(f_line[pos + 1 :])

        for pos, part in enumerate(line_parts):
            # exclude comments, strings:
            if not re_str.STR_OPEN_RE.match(part):
                # also exclude / if we see a namelist and data statement
                if cstyle:
                    part = re.sub(r"\.LT\.", "<   ", part, flags=re_str.RE_FLAGS)
                    part = re.sub(r"\.LE\.", "<=  ", part, flags=re_str.RE_FLAGS)
                    part = re.sub(r"\.GT\.", ">   ", part, flags=re_str.RE_FLAGS)
                    part = re.sub(r"\.GE\.", ">=  ", part, flags=re_str.RE_FLAGS)
                    part = re.sub(r"\.EQ\.", "==  ", part, flags=re_str.RE_FLAGS)
                    part = re.sub(r"\.NE\.", "/=  ", part, flags=re_str.RE_FLAGS)
                else:
                    part = re.sub(r"<=", ".le.", part, flags=re_str.RE_FLAGS)
                    part = re.sub(r"<", ".lt.", part, flags=re_str.RE_FLAGS)
                    part = re.sub(r">=", ".ge.", part, flags=re_str.RE_FLAGS)
                    part = re.sub(r">", ".gt.", part, flags=re_str.RE_FLAGS)
                    part = re.sub(r"==", ".eq.", part, flags=re_str.RE_FLAGS)
                    part = re.sub(r"\/=", ".ne.", part, flags=re_str.RE_FLAGS)

            line_parts[pos] = part

        new_line = "".join(line_parts)

    return new_line


def replace_keywords_single_fline(f_line, case_dict):
    """
    format a single Fortran line - change case of keywords
    """

    new_line = f_line

    # Collect words list
    pos_prev = -1
    pos = -1
    line_parts = [""]
    for pos, char in CharFilter(f_line):
        if pos > pos_prev + 1:  # skipped string
            line_parts.append(f_line[pos_prev + 1 : pos].strip())  # append string
            line_parts.append("")

        line_parts[-1] += char

        pos_prev = pos

    if pos + 1 < len(f_line):
        line_parts.append(f_line[pos + 1 :])

    line_parts = [
        [a] if re_str.STR_OPEN_RE.match(a) else re.split(re_str.F90_OPERATORS_RE, a)
        for a in line_parts
    ]  # problem, split "."
    line_parts = [b for a in line_parts for b in a]

    ## line_parts = [[a] if STR_OPEN_RE.match(a) else re.split('(\W)',a)
    ##               for a in line_parts]  # problem, split "."
    line_parts = [
        [a] if re_str.STR_OPEN_RE.match(a) else re.split("([^a-zA-Z0-9_.])", a)
        for a in line_parts
    ]
    line_parts = [b for a in line_parts for b in a]

    swapcase = lambda s, a: s if a == 0 else (s.lower() if a == 1 else s.upper())

    nbparts = len(line_parts)
    for pos, part in enumerate(line_parts):
        # exclude comments, strings:
        if part.strip() and not re_str.STR_OPEN_RE.match(part):
            if re_str.F90_KEYWORDS_RE.match(part):
                part = swapcase(part, case_dict["keywords"])
            elif re_str.F90_MODULES_RE.match(part):
                part = swapcase(part, case_dict["procedures"])
            elif re_str.F90_PROCEDURES_RE.match(part):
                ok = False
                for pos2 in range(pos + 1, nbparts):
                    part2 = line_parts[pos2]
                    if part2.strip() and not (
                        part2 == "\n" or re_str.STR_OPEN_RE.match(part2)
                    ):
                        ok = part2 == "("
                        break
                if ok:
                    part = swapcase(part, case_dict["procedures"])
            elif re_str.F90_OPERATORS_RE.match(part):
                part = swapcase(part, case_dict["operators"])
            elif re_str.F90_CONSTANTS_RE.match(part):
                part = swapcase(part, case_dict["constants"])
            elif re_str.F90_CONSTANTS_TYPES_RE.match(part):
                part = swapcase(part, case_dict["constants"])
            elif re_str.F90_NUMBER_ALL_REC.match(part):
                part = swapcase(part, case_dict["constants"])

            line_parts[pos] = part

    new_line = "".join(line_parts)

    return new_line


def diff(a, b, a_name, b_name):
    # type: (str, str, str, str) -> str

    """Return a unified diff string between strings `a` and `b`."""
    import difflib

    a_lines = [line + "\n" for line in a.splitlines()]
    b_lines = [line + "\n" for line in b.splitlines()]
    return "".join(
        difflib.unified_diff(a_lines, b_lines, fromfile=a_name, tofile=b_name, n=5)
    )
