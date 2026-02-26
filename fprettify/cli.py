try:
    import configargparse as argparse
except ImportError:
    import argparse

import logging
import os
import sys

from . import re as re_str
from ._cli import process_args, get_arg_parser, get_config_file_list
from .exception import FprettifyException
from .formatter import reformat_inplace
from .log import log_exception, set_fprettify_logger


def run(argv=sys.argv):  # pragma: no cover
    """Command line interface"""
    arguments = {
        "prog": argv[0],
        "description": "Auto-format modern Fortran source files.",
        "formatter_class": argparse.ArgumentDefaultsHelpFormatter,
    }

    if argparse.__name__ == "configargparse":
        arguments["args_for_setting_config_path"] = ["-c", "--config-file"]
        arguments["description"] = (
                arguments["description"]
                + " Config files ('.fprettify.rc') in the home (~) directory and any such files located in parent directories of the input file will be used. When the standard input is used, the search is started from the current directory."
        )

    parser = get_arg_parser(arguments)

    args = parser.parse_args(argv[1:])

    # support legacy input:
    if "stdin" in args.path and not os.path.isfile("stdin"):
        args.path = ["-" if _ == "stdin" else _ for _ in args.path]

    for directory in args.path:
        if directory == "-":
            if args.recursive:
                sys.stderr.write("--recursive requires a directory.\n")
                sys.exit(1)
        else:
            if not os.path.exists(directory):
                sys.stderr.write("directory " + directory + " does not exist!\n")
                sys.exit(1)
            if (
                    not os.path.isfile(directory)
                    and directory != "-"
                    and not args.recursive
            ):
                sys.stderr.write("file " + directory + " does not exist!\n")
                sys.exit(1)

        if not args.recursive:
            filenames = [directory]
        else:
            if args.fortran:
                ext = args.fortran
            else:
                ext = re_str.FORTRAN_EXTENSIONS
            filenames = []

            from fnmatch import fnmatch

            for dirpath, dirnames, files in os.walk(directory, topdown=True):

                # Prune excluded patterns from list of child directories
                dirnames[:] = [
                    dirname
                    for dirname in dirnames
                    if not any(
                        [
                            fnmatch(dirname, exclude_pattern)
                            or fnmatch(os.path.join(dirpath, dirname), exclude_pattern)
                            for exclude_pattern in args.exclude_pattern
                        ]
                    )
                ]

                for ffile in [
                    os.path.join(dirpath, f)
                    for f in files
                    if any(f.endswith(_) for _ in ext)
                       and not any(
                        [
                            fnmatch(f, exclude_pattern)
                            for exclude_pattern in args.exclude_pattern
                        ]
                    )
                ]:

                    include_file = True
                    if args.exclude_max_lines is not None:
                        line_count = 0
                        with open(ffile) as f:
                            for i in f:
                                line_count += 1
                                if line_count > args.exclude_max_lines:
                                    include_file = False
                                    break

                    if include_file:
                        filenames.append(ffile)

        for filename in filenames:

            # reparse arguments using the file's list of config files
            filearguments = arguments
            if argparse.__name__ == "configargparse":
                filearguments["default_config_files"] = [
                                                            "~/.fprettify.rc"
                                                        ] + get_config_file_list(
                    os.path.abspath(filename) if filename != "-" else os.getcwd()
                )
            file_argparser = get_arg_parser(filearguments)

            args_tmp = file_argparser.parse_args(argv[1:])
            file_args = process_args(args_tmp)
            file_args["stdout"] = args_tmp.stdout or directory == "-"
            file_args["diffonly"] = args.diff

            if args.debug:
                args.debug_level = logging.DEBUG
            elif args.silent:
                debug_level = logging.CRITICAL
            else:
                debug_level = logging.WARNING

            set_fprettify_logger(debug_level)

            try:
                reformat_inplace(filename, **file_args)

            except FprettifyException as e:
                log_exception(e, "Fatal error occured")
                sys.exit(1)
