import re

from . import re as re_str
from .exception import FprettifyParseException
from .scanner import CharFilter
from .utils import get_curr_delim


class RegexParser(object):
    def __init__(self, regex: re.Pattern, spec=True):
        self._re = regex
        self.spec = spec

    def search(self, line):
        return self._re.search(line)

    def split(self, line):
        return self._re.split(line)

    def __str__(self) -> str:
        return f'RegexParser({re_str.get_pattern_name(self._re)}: "{self._re.pattern}")'

    def __repr__(self) -> str:
        return f'RegexParser({re_str.get_pattern_name(self._re)}: "{self._re.pattern}")'

    @property
    def pattern(self):
        return self._re


class WhereForAllParser(RegexParser):
    """parser for where / forall construct"""

    def search(self, line):
        match = self._re.search(line)

        if match:
            level = 0
            for pos, char in CharFilter(line):
                [what_del_open, what_del_close] = get_curr_delim(line, pos)

                if what_del_open:
                    if what_del_open.group() == r"(":
                        level += 1

                if what_del_close and what_del_close.group() == r")":
                    if level == 1:
                        if re_str.EMPTY_RE.search(line[pos + 1 :]):
                            return True
                        else:
                            return False
                    else:
                        level += -1

        return False


class PlusMinusParser(RegexParser):
    """parser for +/- in addition"""

    def __init__(self, regex):
        super().__init__(regex)
        self._re_excl = re.compile(r"\b(\d+\.?\d*|\d*\.?\d+)[de]" + re_str.EOL_STR, re_str.RE_FLAGS)

    def split(self, line):
        split_parts = self._re.split(line)
        split_parts_out = []

        # exclude splits due to '+/-' in real literals
        for n, part in enumerate(split_parts):
            if re.search(r"^(\+|-)$", part):
                if self._re_excl.search(split_parts[n - 1]):
                    if n == 1:
                        split_parts_out = [split_parts[n - 1]]
                    if n + 1 >= len(split_parts) or not split_parts_out:
                        raise FprettifyParseException(
                            "non-standard expression involving + or -", "", 0
                        )
                    split_parts_out[-1] += part + split_parts[n + 1]
                else:
                    if n == 1:
                        split_parts_out = [split_parts[n - 1]]
                    if n + 1 >= len(split_parts):
                        raise FprettifyParseException(
                            "non-standard expression involving + or -", "", 0
                        )
                    split_parts_out += [part, split_parts[n + 1]]

        if not split_parts_out:
            split_parts_out = split_parts

        return split_parts_out


def parse_fprettify_directives(
    lines, comment_lines, in_format_off_block, filename, line_nr
):
    """
    parse formatter directives '!&' and line continuations starting with an
    ampersand.
    """
    auto_align = not any(re_str.NO_ALIGN_RE.search(_) for _ in lines)
    auto_format = not (
        in_format_off_block or any(_.lstrip().startswith("!&") for _ in comment_lines)
    )
    if not auto_format:
        auto_align = False
    if (len(lines)) == 1:
        valid_directive = True
        if lines[0].strip().startswith("!&<"):
            if in_format_off_block:
                valid_directive = False
            else:
                in_format_off_block = True
        if lines[0].strip().startswith("!&>"):
            if not in_format_off_block:
                valid_directive = False
            else:
                in_format_off_block = False
        if not valid_directive:
            raise FprettifyParseException(re_str.FORMATTER_ERROR_MESSAGE, filename, line_nr)

    return [auto_align, auto_format, in_format_off_block]


__all__ = ["RegexParser", "WhereForAllParser", "PlusMinusParser", "parse_fprettify_directives"]
