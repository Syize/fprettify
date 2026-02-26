from . import re as re_str
from .scanner import InputStream


def get_curr_delim(line, pos):
    """get delimiter token in line starting at pos, if it exists"""
    what_del_open = re_str.DEL_OPEN_RE.search(line[pos : pos + 2])
    what_del_close = re_str.DEL_CLOSE_RE.search(line[pos : pos + 2])
    return [what_del_open, what_del_close]


def inspect_ffile_format(
    infile, indent_size, strict_indent, indent_fypp=False, orig_filename=None
):
    """
    Determine indentation by inspecting original Fortran file.

    This is mainly for finding aligned blocks of DO/IF statements.
    Also check if it has f77 constructs.
    :param infile: open file
    :param indent_size: the default indent size
    :orig_filename: filename used for messages
    :returns: [ target indent sizes for each line,
                indent of first line (offset) ]
    """
    if not orig_filename:
        orig_filename = infile.name

    num_labels = False
    indents = []
    stream = InputStream(
        infile, filter_fypp=not indent_fypp, orig_filename=orig_filename
    )
    prev_offset = 0
    first_indent = -1
    has_fypp = False

    while 1:
        f_line, _, lines = stream.next_fortran_line()
        if not lines:
            break

        if re_str.FYPP_LINE_RE.search(f_line):
            has_fypp = True

        f_line, lines, label = preprocess_labels(f_line, lines)

        offset = len(lines[0]) - len(lines[0].lstrip(" "))
        if f_line.strip() and first_indent == -1:
            if re_str.PROG_RE.match(f_line) or re_str.MOD_RE.match(f_line):
                first_indent = 0
            else:
                first_indent = offset

        indents.append(offset - prev_offset)

        # don't impose indentation for blocked do/if constructs:
        if re_str.IF_RE.search(f_line) or re_str.DO_RE.search(f_line):
            indent_misaligned = indent_size > 0 and offset % indent_size != 0
            # if prev_offset != offset or strict_indent or indent_misaligned:
            if strict_indent or indent_misaligned or not indent_misaligned:
                indents[-1] = indent_size
        else:
            indents[-1] = indent_size

        prev_offset = offset

    return indents, first_indent, has_fypp


def preprocess_labels(f_line, lines):
    """remove statement labels"""

    match = re_str.STATEMENT_LABEL_RE.search(f_line)
    if match:
        label = match.group(1)
    else:
        label = ""

    if label:
        f_line = re_str.STATEMENT_LABEL_RE.sub(len(label) * " ", f_line, count=1)
        lines[0] = re_str.STATEMENT_LABEL_RE.sub(len(label) * " ", lines[0], count=1)

    return [f_line, lines, label]


__all__ = ["get_curr_delim", "inspect_ffile_format", "preprocess_labels"]
