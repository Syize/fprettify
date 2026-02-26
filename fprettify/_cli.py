import argparse
import os

from . import re as re_str


def build_ws_dict(args):
    """helper function to build whitespace dictionary"""
    ws_dict = {"comma": args.whitespace_comma, "assignments": args.whitespace_assignment, "decl": args.whitespace_decl,
               "relational": args.whitespace_relational, "logical": args.whitespace_logical,
               "plusminus": args.whitespace_plusminus, "multdiv": args.whitespace_multdiv,
               "print": args.whitespace_print, "type": args.whitespace_type, "intrinsics": args.whitespace_intrinsics,
               "concat": args.whitespace_concat}
    return ws_dict


def process_args(args):
    args_out = {"whitespace_dict": build_ws_dict(args), "case_dict": {
        "keywords": args.case[0],
        "procedures": args.case[1],
        "operators": args.case[2],
        "constants": args.case[3],
    }, "impose_indent": not args.disable_indent, "indent_size": args.indent, "strict_indent": args.strict_indent,
                "impose_whitespace": not args.disable_whitespace, "impose_replacements": args.enable_replacements,
                "cstyle": args.c_relations, "whitespace": args.whitespace,
                "llength": 1024 if args.line_length == 0 else args.line_length, "strip_comments": args.strip_comments,
                "comment_spacing": args.comment_spacing, "format_decl": args.enable_decl,
                "indent_fypp": not args.disable_fypp, "indent_mod": not args.disable_indent_mod}

    return args_out


def str2bool(string):
    """helper function to convert strings to bool"""
    if string.lower() in ("yes", "true", "t", "y", "1"):
        return True
    elif string.lower() in ("no", "false", "f", "n", "0"):
        return False
    else:
        return None


def non_negative_int(value):
    """helper function to ensure a non-negative integer"""
    try:
        int_value = int(value)
    except ValueError as exc:
        raise argparse.ArgumentTypeError(str(exc))
    if int_value < 0:
        raise argparse.ArgumentTypeError("expected a non-negative integer")
    return int_value


def get_arg_parser(args={}):
    """helper function to create the parser object"""
    parser = argparse.ArgumentParser(**args)

    parser.add_argument(
        "-i", "--indent", type=int, default=3, help="relative indentation width"
    )
    parser.add_argument(
        "-l",
        "--line-length",
        type=int,
        default=132,
        help="column after which a line should end, viz. -ffree-line-length-n for GCC",
    )
    parser.add_argument(
        "-w",
        "--whitespace",
        type=int,
        choices=range(0, 5),
        default=2,
        help="Presets for the amount of whitespace - "
             "   0: minimal whitespace"
             " | 1: operators (except arithmetic), print/read"
             " | 2: operators, print/read, plus/minus"
             " | 3: operators, print/read, plus/minus, muliply/divide"
             " | 4: operators, print/read, plus/minus, muliply/divide, type component selector",
    )
    parser.add_argument(
        "--whitespace-comma",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for comma/semicolons",
    )
    parser.add_argument(
        "--whitespace-assignment",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for assignments",
    )
    parser.add_argument(
        "--whitespace-decl",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for declarations (requires '--enable-decl')",
    )
    parser.add_argument(
        "--whitespace-relational",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for relational operators",
    )
    parser.add_argument(
        "--whitespace-logical",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for logical operators",
    )
    parser.add_argument(
        "--whitespace-plusminus",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for plus/minus arithmetic",
    )
    parser.add_argument(
        "--whitespace-multdiv",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for multiply/divide arithmetic",
    )
    parser.add_argument(
        "--whitespace-print",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for print/read statements",
    )
    parser.add_argument(
        "--whitespace-type",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for select type components",
    )
    parser.add_argument(
        "--whitespace-intrinsics",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for intrinsics like if/write/close",
    )
    parser.add_argument(
        "--whitespace-concat",
        type=str2bool,
        nargs="?",
        default="None",
        const=True,
        help="boolean, en-/disable whitespace for string concatenation operator //",
    )
    parser.add_argument(
        "--strict-indent",
        action="store_true",
        default=False,
        help="strictly impose indentation even for nested loops",
    )
    parser.add_argument(
        "--enable-decl",
        action="store_true",
        default=False,
        help="enable whitespace formatting of declarations ('::' operator).",
    )
    parser.add_argument(
        "--disable-indent",
        action="store_true",
        default=False,
        help="don't impose indentation",
    )
    parser.add_argument(
        "--disable-whitespace",
        action="store_true",
        default=False,
        help="don't impose whitespace formatting",
    )
    parser.add_argument(
        "--enable-replacements",
        action="store_true",
        default=False,
        help="replace relational operators (e.g. '.lt.' <--> '<')",
    )
    parser.add_argument(
        "--c-relations",
        action="store_true",
        default=False,
        help="C-style relational operators ('<', '<=', ...)",
    )
    parser.add_argument(
        "--case",
        nargs=4,
        default=[1, 1, 1, 1],
        type=int,
        help="Enable letter case formatting of intrinsics by specifying which of "
             "keywords, procedures/modules, operators and constants (in this order) should be lowercased or uppercased - "
             "   0: do nothing"
             " | 1: lowercase"
             " | 2: uppercase",
    )

    parser.add_argument(
        "--strip-comments",
        action="store_true",
        default=False,
        help="strip whitespaces before comments",
    )
    parser.add_argument(
        "--comment-spacing",
        type=non_negative_int,
        default=1,
        help="number of spaces between code and inline comments when '--strip-comments' is used",
    )
    parser.add_argument(
        "--disable-fypp",
        action="store_true",
        default=False,
        help="Disables the indentation of fypp preprocessor blocks.",
    )
    parser.add_argument(
        "--disable-indent-mod",
        action="store_true",
        default=False,
        help="Disables the indentation after module / program.",
    )

    parser.add_argument(
        "-d",
        "--diff",
        action="store_true",
        default=False,
        help="Write file differences to stdout instead of formatting inplace",
    )
    parser.add_argument(
        "-s",
        "--stdout",
        action="store_true",
        default=False,
        help="Write to stdout instead of formatting inplace",
    )

    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "-S",
        "--silent",
        "--no-report-errors",
        action="store_true",
        default=False,
        help="Don't write any errors or warnings to stderr",
    )
    group.add_argument(
        "-D", "--debug", action="store_true", default=False, help=argparse.SUPPRESS
    )
    parser.add_argument(
        "path",
        type=str,
        nargs="*",
        help="Paths to files to be formatted inplace. If no paths are given, stdin (-) is used by default. Path can be a directory if --recursive is used.",
        default=["-"],
    )
    parser.add_argument(
        "-r",
        "--recursive",
        action="store_true",
        default=False,
        help="Recursively auto-format all Fortran files in subdirectories of specified path; recognized filename extensions: {}".format(
            ", ".join(re_str.FORTRAN_EXTENSIONS)
        ),
    )
    parser.add_argument(
        "-e",
        "--exclude-pattern",
        "--exclude",
        action="append",
        default=[],
        type=str,
        help="File or directory patterns to be excluded when searching for Fortran files to format",
    )
    parser.add_argument(
        "-m",
        "--exclude-max-lines",
        type=int,
        help="Exclude large files when searching for Fortran files to format by specifying the maximum number of lines per file",
    )
    parser.add_argument(
        "-f",
        "--fortran",
        type=str,
        action="append",
        default=[],
        help="Overrides default fortran extensions recognized by --recursive. Repeat this option to specify more than one extension.",
    )
    parser.add_argument("--version", action="version", version="%(prog)s 0.3.7")
    return parser


def get_config_file_list(filename):
    """helper function to create list of config files found in parent directories"""
    config_file_list = []
    dir = os.path.dirname(filename)
    while True:
        config_file = os.path.join(dir, ".fprettify.rc")
        if os.path.isfile(config_file):
            config_file_list.insert(0, config_file)
        parent = os.path.dirname(dir)
        if parent == dir:
            break
        dir = parent
    return config_file_list
