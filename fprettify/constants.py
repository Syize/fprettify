import re

from .fparse_utils import parser_re
from .exception import FprettifyParseException

FORTRAN_EXTENSIONS = [".f", ".for", ".ftn",
                      ".f90", ".f95", ".f03", ".fpp"]
FORMATTER_ERROR_MESSAGE = (" Wrong usage of formatting-specific directives"
                           " '&', '!&', '!&<' or '!&>'.")
LINESPLIT_MESSAGE = ("auto indentation failed due to chars limit, "
                     "line should be split")
EOL_STR = r"\s*;?\s*$"  # end of fortran line
EOL_SC = r"\s*;\s*$"  # whether line is ended with semicolon
SOL_STR = r"^\s*"  # start of fortran line
RE_FLAGS = re.IGNORECASE | re.UNICODE

# FIXME bad ass regex!
VAR_DECL_RE = re.compile(
    r"^ *(?P<type>integer(?: *\* *[0-9]+)?|logical|character(?: *\* *[0-9]+)?|real(?: *\* *[0-9]+)?|complex(?: *\* *[0-9]+)?|type) *(?P<parameters>\((?:[^()]+|\((?:[^()]+|\([^()]*\))*\))*\))? *(?P<attributes>(?: *, *[a-zA-Z_0-9]+(?: *\((?:[^()]+|\((?:[^()]+|\([^()]*\))*\))*\))?)+)? *(?P<dpnt>::)?(?P<vars>[^\n]+)\n?", RE_FLAGS)

OMP_COND_RE = re.compile(r"^\s*(!\$ )", RE_FLAGS)
OMP_DIR_RE = re.compile(r"^\s*(!\$OMP)", RE_FLAGS)

# supported preprocessors
FYPP_LINE_STR = r"^(#!|#:|\$:|@:)"
FYPP_WITHOUT_PREPRO_STR = r"^(#!|\$:|@:)"
CPP_STR = r"^#[^!:{}]"
COMMENT_LINE_STR = r"^!"
FYPP_OPEN_STR = r"(#{|\${|@{)"
FYPP_CLOSE_STR = r"(}#|}\$|}@)"
NOTFORTRAN_LINE_RE = re.compile(r"("+FYPP_LINE_STR+r"|"+CPP_STR+r"|"+COMMENT_LINE_STR+r")", RE_FLAGS)
NOTFORTRAN_FYPP_LINE_RE = re.compile(r"("+CPP_STR+r"|"+COMMENT_LINE_STR+r")", RE_FLAGS)
FYPP_LINE_RE = re.compile(FYPP_LINE_STR, RE_FLAGS)
FYPP_WITHOUT_PREPRO_RE = re.compile(FYPP_WITHOUT_PREPRO_STR, RE_FLAGS)
FYPP_OPEN_RE = re.compile(FYPP_OPEN_STR, RE_FLAGS)
FYPP_CLOSE_RE = re.compile(FYPP_CLOSE_STR, RE_FLAGS)

STR_OPEN_RE = re.compile(r"("+FYPP_OPEN_STR+r"|"+r"'|\"|!)", RE_FLAGS)
CPP_RE = re.compile(CPP_STR, RE_FLAGS)

STATEMENT_LABEL_RE = re.compile(r"^\s*(\d+\s)(?!" + EOL_STR + ")", RE_FLAGS)
IF_RE = re.compile(
    SOL_STR + r"(\w+\s*:)?\s*IF\s*\(.*\)\s*THEN" + EOL_STR, RE_FLAGS
)
ELSE_RE = re.compile(
    SOL_STR + r"ELSE(\s*IF\s*\(.*\)\s*THEN)?" + EOL_STR, RE_FLAGS
)
ENDIF_RE = re.compile(SOL_STR + r"END\s*IF(\s+\w+)?" + EOL_STR, RE_FLAGS)
DO_RE = re.compile(SOL_STR + r"(\w+\s*:)?\s*DO(" + EOL_STR + r"|\s+\w)", RE_FLAGS)
ENDDO_RE = re.compile(SOL_STR + r"END\s*DO(\s+\w+)?" + EOL_STR, RE_FLAGS)
SELCASE_RE = re.compile(
    SOL_STR + r"SELECT\s*(CASE|RANK|TYPE)\s*\(.*\)" + EOL_STR, RE_FLAGS
)
CASE_RE = re.compile(
    SOL_STR + r"((CASE|RANK|TYPE\s+IS|CLASS\s+IS)\s*(\(.*\)|DEFAULT)|CLASS\s+DEFAULT)" + EOL_STR, RE_FLAGS
)
ENDSEL_RE = re.compile(SOL_STR + r"END\s*SELECT" + EOL_STR, RE_FLAGS)
ASSOCIATE_RE = re.compile(SOL_STR + r"ASSOCIATE\s*\(.*\)" + EOL_STR, RE_FLAGS)
ENDASSOCIATE_RE = re.compile(SOL_STR + r"END\s*ASSOCIATE" + EOL_STR, RE_FLAGS)
BLK_RE = re.compile(SOL_STR + r"(\w+\s*:)?\s*BLOCK" + EOL_STR, RE_FLAGS)
ENDBLK_RE = re.compile(SOL_STR + r"END\s*BLOCK(\s+\w+)?" + EOL_STR, RE_FLAGS)
SUBR_RE = re.compile(
    r"^([^\"']* )?SUBROUTINE\s+\w+\s*(\(.*\))?" + EOL_STR, RE_FLAGS
)
ENDSUBR_RE = re.compile(
    SOL_STR + r"END\s*SUBROUTINE(\s+\w+)?" + EOL_STR, RE_FLAGS
)
FCT_RE = re.compile(
    r"^([^\"']* )?FUNCTION\s+\w+\s*(\(.*\))?(\s*RESULT\s*\(\w+\))?" + EOL_STR,
    RE_FLAGS
)
ENDFCT_RE = re.compile(
    SOL_STR + r"END\s*FUNCTION(\s+\w+)?" + EOL_STR, RE_FLAGS
)
MOD_RE = re.compile(SOL_STR + r"MODULE\s+\w+" + EOL_STR, RE_FLAGS)
ENDMOD_RE = re.compile(SOL_STR + r"END\s*MODULE(\s+\w+)?" + EOL_STR, RE_FLAGS)
SMOD_RE = re.compile(SOL_STR + r"SUBMODULE\s*\(\w+\)\s+\w+" + EOL_STR, RE_FLAGS)
ENDSMOD_RE = re.compile(SOL_STR + r"END\s*SUBMODULE(\s+\w+)?" + EOL_STR, RE_FLAGS)
TYPE_RE = re.compile(
    SOL_STR +
    r"TYPE(\s*,\s*(BIND\s*\(\s*C\s*\)|EXTENDS\s*\(.*\)|ABSTRACT|PUBLIC|PRIVATE))*(\s*,\s*)?(\s*::\s*|\s+)\w+" + EOL_STR,
    RE_FLAGS
)
ENDTYPE_RE = re.compile(SOL_STR + r"END\s*TYPE(\s+\w+)?" + EOL_STR, RE_FLAGS)
PROG_RE = re.compile(SOL_STR + r"PROGRAM\s+\w+" + EOL_STR, RE_FLAGS)
ENDPROG_RE = re.compile(
    SOL_STR + r"END\s*PROGRAM(\s+\w+)?" + EOL_STR, RE_FLAGS
)
INTERFACE_RE = re.compile(
    r"^([^\"']* )?INTERFACE(\s+\w+|\s+(OPERATOR|ASSIGNMENT)\s*\(.*\))?" + EOL_STR, RE_FLAGS
)
ENDINTERFACE_RE = re.compile(
    SOL_STR + r"END\s*INTERFACE(\s+\w+|\s+(OPERATOR|ASSIGNMENT)\s*\(.*\))?" + EOL_STR, RE_FLAGS
)
CONTAINS_RE = re.compile(SOL_STR + r"CONTAINS" + EOL_STR, RE_FLAGS)
ENUM_RE = re.compile(
    SOL_STR + r"ENUM(\s*,\s*(BIND\s*\(\s*C\s*\)))?((\s*::\s*|\s+)\w+)?" + EOL_STR,
    RE_FLAGS
)
ENDENUM_RE = re.compile(SOL_STR + r"END\s*ENUM(\s+\w+)?" + EOL_STR, RE_FLAGS)
ENDANY_RE = re.compile(SOL_STR + r"END" + EOL_STR, RE_FLAGS)
FORALL_RE = re.compile(SOL_STR + r"(\w+\s*:)?\s*FORALL\s*\(.*\)" + EOL_STR, RE_FLAGS)
ENDFORALL_RE = re.compile(SOL_STR + r"END\s*FORALL(\s+\w+)?" + EOL_STR, RE_FLAGS)
WHERE_RE = re.compile(SOL_STR + r"(\w+\s*:)?\s*WHERE\s*\(.*\)" + EOL_STR, RE_FLAGS)
ELSEWHERE_RE = re.compile(SOL_STR + r"ELSE\s*WHERE(\(.*\))?(\s*\w+)?" + EOL_STR, RE_FLAGS)
ENDWHERE_RE = re.compile(SOL_STR + r"END\s*WHERE(\s+\w+)?" + EOL_STR, RE_FLAGS)
FYPP_DEF_RE = re.compile(SOL_STR + r"#:DEF\s+", RE_FLAGS)
FYPP_ENDDEF_RE = re.compile(SOL_STR + r"#:ENDDEF", RE_FLAGS)
FYPP_IF_RE = re.compile(SOL_STR + r"#:IF\s+", RE_FLAGS)
FYPP_ELIF_ELSE_RE = re.compile(SOL_STR + r"#:(ELIF\s+|ELSE)", RE_FLAGS)
FYPP_ENDIF_RE = re.compile(SOL_STR + r"#:ENDIF", RE_FLAGS)
FYPP_FOR_RE = re.compile(SOL_STR + r"#:FOR\s+", RE_FLAGS)
FYPP_ENDFOR_RE = re.compile(SOL_STR + r"#:ENDFOR", RE_FLAGS)
FYPP_BLOCK_RE = re.compile(SOL_STR + r"#:BLOCK\s+", RE_FLAGS)
FYPP_ENDBLOCK_RE = re.compile(SOL_STR + r"#:ENDBLOCK", RE_FLAGS)
FYPP_CALL_RE = re.compile(SOL_STR + r"#:CALL\s+", RE_FLAGS)
FYPP_ENDCALL_RE = re.compile(SOL_STR + r"#:ENDCALL", RE_FLAGS)
FYPP_MUTE_RE = re.compile(SOL_STR + r"#:MUTE", RE_FLAGS)
FYPP_ENDMUTE_RE = re.compile(SOL_STR + r"#:ENDMUTE", RE_FLAGS)
PRIVATE_RE = re.compile(SOL_STR + r"PRIVATE\s*::", RE_FLAGS)
PUBLIC_RE = re.compile(SOL_STR + r"PUBLIC\s*::", RE_FLAGS)
END_RE = re.compile(SOL_STR + r"(END)\s*(IF|DO|SELECT|ASSOCIATE|BLOCK|SUBROUTINE|FUNCTION|MODULE|SUBMODULE|TYPE|PROGRAM|INTERFACE|ENUM|WHERE|FORALL)", RE_FLAGS)
INTR_STMTS_PAR = (r"(ALLOCATE|DEALLOCATE|"
                  r"OPEN|CLOSE|READ|WRITE|"
                  r"FLUSH|ENDFILE|REWIND|BACKSPACE|INQUIRE|"
                  r"FORALL|WHERE|ASSOCIATE|NULLIFY)")
LINEBREAK_STR = r"(&)[\s]*(?:!.*)?$"
PLUSMINUS_RE = re.compile(r"(?<=[\w\)\]])\s*(\+|-)\s*", RE_FLAGS)
MULTDIV_RE = re.compile(
    r"(?<=[\w\)\]])\s*((?<!\*)\*(?!\*)|(?<!/)/(?!/))(?=[\s\w\(])(?!.*::)", RE_FLAGS
)
REL_OP_RE = re.compile(
    r"(?<!\()\s*(\.(?:EQ|NE|LT|LE|GT|GE)\.|(?:==|\/=|<(?!=)|<=|(?<!=)>(?!=)|>=))\s*(?!\))",
    RE_FLAGS
)
LOG_OP_RE = re.compile(r"\s*(\.(?:AND|OR|EQV|NEQV)\.)\s*", RE_FLAGS)
PRINT_RE = re.compile(r"(?:(?<=\bPRINT)|(?<=\bREAD))\s*(\*,?)\s*", RE_FLAGS)
DEL_OPEN_STR = r"(\(\/?|\[)"
DEL_OPEN_RE = re.compile(r"^" + DEL_OPEN_STR, RE_FLAGS)
DEL_CLOSE_STR = r"(\/?\)|\])"
DEL_CLOSE_RE = re.compile(r"^" + DEL_CLOSE_STR, RE_FLAGS)
EMPTY_RE = re.compile(SOL_STR + r"$", RE_FLAGS)
PREPRO_NEW_SCOPE = [parser_re(FYPP_DEF_RE), parser_re(FYPP_IF_RE), parser_re(FYPP_FOR_RE),
                    parser_re(FYPP_BLOCK_RE), parser_re(FYPP_CALL_RE), parser_re(FYPP_MUTE_RE)]
PREPRO_CONTINUE_SCOPE = [None, parser_re(FYPP_ELIF_ELSE_RE), None, None, None, None]
PREPRO_END_SCOPE = [parser_re(FYPP_ENDDEF_RE), parser_re(FYPP_ENDIF_RE), parser_re(FYPP_ENDFOR_RE),
                    parser_re(FYPP_ENDBLOCK_RE), parser_re(FYPP_ENDCALL_RE),
                    parser_re(FYPP_ENDMUTE_RE)]




class PlusminusParser(parser_re):
    """parser for +/- in addition
    """

    def __init__(self, regex):
        self._re = regex
        self._re_excl = re.compile(r"\b(\d+\.?\d*|\d*\.?\d+)[de]" + EOL_STR, RE_FLAGS)

    def split(self, line):
        partsplit = self._re.split(line)
        partsplit_out = []

        # exclude splits due to '+/-' in real literals
        for n, part in enumerate(partsplit):
            if re.search(r"^(\+|-)$", part):
                if self._re_excl.search(partsplit[n - 1]):
                    if n == 1:
                        partsplit_out = [partsplit[n - 1]]
                    if n + 1 >= len(partsplit) or not partsplit_out:
                        raise FprettifyParseException("non-standard expression involving + or -", '', 0)
                    partsplit_out[-1] += part + partsplit[n + 1]
                else:
                    if n == 1:
                        partsplit_out = [partsplit[n - 1]]
                    if n + 1 >= len(partsplit):
                        raise FprettifyParseException("non-standard expression involving + or -", '', 0)
                    partsplit_out += [part, partsplit[n + 1]]

        if not partsplit_out:
            partsplit_out = partsplit

        return partsplit_out


LR_OPS_RE = [REL_OP_RE, LOG_OP_RE, PlusminusParser(PLUSMINUS_RE), MULTDIV_RE, PRINT_RE]
USE_RE = re.compile(
    SOL_STR + "USE(\s+|(,.+?)?::\s*)\w+?((,.+?=>.+?)+|,\s*only\s*:.+?)?$" + EOL_STR, RE_FLAGS
)
NO_ALIGN_RE = re.compile(SOL_STR + r"&\s*[^\s*]+")
NML_RE = re.compile(r"(/\w+/)", RE_FLAGS)
NML_STMT_RE = re.compile(SOL_STR + r"NAMELIST.*/.*/", RE_FLAGS)
DATA_STMT_RE = re.compile(SOL_STR + r"DATA\s+\w", RE_FLAGS)
F90_KEYWORDS_RE = re.compile(
    r"\b(" + "|".join(
        (
            "allocatable", "allocate", "assign", "assignment", "backspace",
            "block", "call", "case", "character", "close", "common", "complex",
            "contains", "continue", "cycle", "data", "deallocate",
            "dimension", "do", "double", "else", "elseif", "elsewhere", "end",
            "enddo", "endfile", "endif", "entry", "equivalence", "exit",
            "external", "forall", "format", "function", "goto", "if",
            "implicit", "include", "inquire", "integer", "intent",
            "interface", "intrinsic", "logical", "module", "namelist", "none",
            "nullify", "only", "open", "operator", "optional", "parameter",
            "pause", "pointer", "precision", "print", "private", "procedure",
            "program", "public", "read", "real", "recursive", "result", "return",
            "rewind", "save", "select", "sequence", "stop", "subroutine",
            "target", "then", "type", "use", "where", "while", "write",
            # F95 keywords.
            "elemental", "pure",
            # F2003
            "abstract", "associate", "asynchronous", "bind", "class",
            "deferred", "enum", "enumerator", "extends", "extends_type_of",
            "final", "generic", "import", "non_intrinsic", "non_overridable",
            "nopass", "pass", "protected", "same_type_as", "value", "volatile",
            # F2008.
            "contiguous", "submodule", "concurrent", "codimension",
            "sync all", "sync memory", "critical", "image_index",
        )
    ) + r")\b", RE_FLAGS
)
F90_PROCEDURES_RE = re.compile(
    r"\b(" + "|".join(
        (
            "abs", "achar", "acos", "adjustl", "adjustr", "aimag", "aint",
            "all", "allocated", "anint", "any", "asin", "associated",
            "atan", "atan2", "bit_size", "btest", "ceiling", "char", "cmplx",
            "conjg", "cos", "cosh", "count", "cshift", "date_and_time", "dble",
            "digits", "dim", "dot_product", "dprod", "eoshift", "epsilon",
            "exp", "exponent", "floor", "fraction", "huge", "iachar", "iand",
            "ibclr", "ibits", "ibset", "ichar", "ieor", "index", "int", "ior",
            "ishft", "ishftc", "kind", "lbound", "len", "len_trim", "lge", "lgt",
            "lle", "llt", "log", "log10", "logical", "matmul", "max",
            "maxexponent", "maxloc", "maxval", "merge", "min", "minexponent",
            "minloc", "minval", "mod", "modulo", "mvbits", "nearest", "nint",
            "not", "pack", "precision", "present", "product", "radix",
            # Real is taken out here to avoid highlighting declarations.
            "random_number", "random_seed", "range",  # "real"
            "repeat", "reshape", "rrspacing", "scale", "scan",
            "selected_int_kind", "selected_real_kind", "set_exponent",
            "shape", "sign", "sin", "sinh", "size", "spacing", "spread", "sqrt",
            "sum", "system_clock", "tan", "tanh", "tiny", "transfer",
            "transpose", "trim", "ubound", "unpack", "verify",
            # F95 intrinsic functions.
            "null", "cpu_time",
            # F2003.
            "move_alloc", "command_argument_count", "get_command",
            "get_command_argument", "get_environment_variable",
            "selected_char_kind", "wait", "flush", "new_line",
            "extends", "extends_type_of", "same_type_as", "bind",
            # F2003 ieee_arithmetic intrinsic module.
            "ieee_support_underflow_control", "ieee_get_underflow_mode",
            "ieee_set_underflow_mode",
            # F2003 iso_c_binding intrinsic module.
            "c_loc", "c_funloc", "c_associated", "c_f_pointer",
            "c_f_procpointer",
            # F2008.
            "bge", "bgt", "ble", "blt", "dshiftl", "dshiftr", "leadz", "popcnt",
            "poppar", "trailz", "maskl", "maskr", "shifta", "shiftl", "shiftr",
            "merge_bits", "iall", "iany", "iparity", "storage_size",
            "bessel_j0", "bessel_j1", "bessel_jn",
            "bessel_y0", "bessel_y1", "bessel_yn",
            "erf", "erfc", "erfc_scaled", "gamma", "hypot", "log_gamma",
            "norm2", "parity", "findloc", "is_contiguous",
            "sync images", "lock", "unlock", "image_index",
            "lcobound", "ucobound", "num_images", "this_image",
            # F2008 iso_fortran_env module.
            "compiler_options", "compiler_version",
            # F2008 iso_c_binding module.
            "c_sizeof"

        )
    ) + r")\b", RE_FLAGS
)
F90_MODULES_RE = re.compile(
    r"\b(" + "|".join(
        (
            # F2003/F2008 module names
            "iso_fortran_env",
            "iso_c_binding",
            "ieee_exceptions",
            "ieee_arithmetic",
            "ieee_features"
        )
    ) + r")\b", RE_FLAGS
)
F90_OPERATORS_RE = re.compile(
    r"(" + "|".join(
        [r"\." + a + r"\." for a in (
            "and", "eq", "eqv", "false", "ge", "gt", "le", "lt", "ne",
            "neqv", "not", "or", "true"
        )]
    ) + r")", RE_FLAGS
)
F90_CONSTANTS_RE = re.compile(
    r"\b(" + "|".join(
        (
            # F2003 iso_fortran_env constants.
            "input_unit", "output_unit", "error_unit",
            "iostat_end", "iostat_eor",
            "numeric_storage_size", "character_storage_size",
            "file_storage_size",
            # F2003 iso_c_binding constants.
            "c_int", "c_short", "c_long", "c_long_long", "c_signed_char",
            "c_size_t",
            "c_int8_t", "c_int16_t", "c_int32_t", "c_int64_t",
            "c_int_least8_t", "c_int_least16_t", "c_int_least32_t",
            "c_int_least64_t",
            "c_int_fast8_t", "c_int_fast16_t", "c_int_fast32_t",
            "c_int_fast64_t",
            "c_intmax_t", "c_intptr_t",
            "c_float", "c_double", "c_long_double",
            "c_float_complex", "c_double_complex", "c_long_double_complex",
            "c_bool", "c_char",
            "c_null_char", "c_alert", "c_backspace", "c_form_feed",
            "c_new_line", "c_carriage_return", "c_horizontal_tab",
            "c_vertical_tab",
            "c_ptr", "c_funptr", "c_null_ptr", "c_null_funptr",
            # F2008 iso_fortran_env constants.
            "character_kinds", "int8", "int16", "int32", "int64",
            "integer_kinds", "iostat_inquire_internal_unit",
            "logical_kinds", "real_kinds", "real32", "real64", "real128",
            "lock_type", "atomic_int_kind", "atomic_logical_kind",
        )
    ) + r")\b", RE_FLAGS
)
F90_INT_RE = r"[-+]?[0-9]+"
F90_FLOAT_RE = r"[-+]?([0-9]+\.[0-9]*|\.[0-9]+)"
F90_NUMBER_RE = "(" + F90_INT_RE + "|" + F90_FLOAT_RE + ")"
F90_FLOAT_EXP_RE = F90_NUMBER_RE + r"[eEdD]" + F90_NUMBER_RE
F90_NUMBER_ALL_RE = "(" + F90_NUMBER_RE + "|" + F90_FLOAT_EXP_RE + ")"
F90_NUMBER_ALL_REC = re.compile(F90_NUMBER_ALL_RE, RE_FLAGS)
F90_CONSTANTS_TYPES_RE = re.compile(
    r"(" + F90_NUMBER_ALL_RE + ")*_(" + "|".join(
        (
            # F2003 iso_fortran_env constants.
            # F2003 iso_c_binding constants.
            "c_int", "c_short", "c_long", "c_long_long", "c_signed_char",
            "c_size_t",
            "c_int8_t", "c_int16_t", "c_int32_t", "c_int64_t",
            "c_int_least8_t", "c_int_least16_t", "c_int_least32_t",
            "c_int_least64_t",
            "c_int_fast8_t", "c_int_fast16_t", "c_int_fast32_t",
            "c_int_fast64_t",
            "c_intmax_t", "c_intptr_t",
            "c_float", "c_double", "c_long_double",
            "c_float_complex", "c_double_complex", "c_long_double_complex",
            "c_bool", "c_char",
            # F2008 iso_fortran_env constants.
            "character_kinds", "int8", "int16", "int32", "int64",
            "integer_kinds",
            "logical_kinds", "real_kinds", "real32", "real64", "real128",
            "lock_type", "atomic_int_kind", "atomic_logical_kind",
        )
    ) + r")\b", RE_FLAGS
)

__all__ = ["ASSOCIATE_RE", "BLK_RE", "CASE_RE", "CONTAINS_RE", "DATA_STMT_RE", "DEL_CLOSE_RE", "DEL_CLOSE_STR", "DEL_OPEN_RE", "DEL_OPEN_STR", "DO_RE", "ELSEWHERE_RE", "ELSE_RE",
           "EMPTY_RE", "ENDANY_RE", "ENDASSOCIATE_RE", "ENDBLK_RE", "ENDDO_RE", "ENDENUM_RE", "ENDFCT_RE", "ENDFORALL_RE", "ENDIF_RE", "ENDINTERFACE_RE",
           "ENDMOD_RE", "ENDPROG_RE", "ENDSEL_RE", "ENDSMOD_RE", "ENDSUBR_RE",
           "ENDTYPE_RE", "ENDWHERE_RE", "END_RE", "ENUM_RE", "EOL_SC", "F90_CONSTANTS_RE", "F90_CONSTANTS_TYPES_RE", "F90_KEYWORDS_RE", "F90_MODULES_RE", "F90_NUMBER_ALL_REC",
           "F90_OPERATORS_RE", "F90_PROCEDURES_RE", "FCT_RE", "FORALL_RE", "FORMATTER_ERROR_MESSAGE", "FORTRAN_EXTENSIONS", "IF_RE",
           "INTERFACE_RE", "INTR_STMTS_PAR", "LINEBREAK_STR", "LINESPLIT_MESSAGE",
           "LR_OPS_RE", "MOD_RE", "NML_RE", "NML_STMT_RE", "NO_ALIGN_RE", "PREPRO_CONTINUE_SCOPE", "PREPRO_END_SCOPE", "PREPRO_NEW_SCOPE", "PRIVATE_RE", "PROG_RE", "PUBLIC_RE",
           "REL_OP_RE", "SELCASE_RE", "SMOD_RE", "SOL_STR", "STATEMENT_LABEL_RE", "SUBR_RE", "TYPE_RE", "USE_RE", "WHERE_RE"]
