import re

# from fprettify import parser_re, plusminus_parser, where_parser
from .parser import PlusMinusParser, RegexParser


class PatternWithName:
    @classmethod
    def compile(cls, pattern: str, flags, name: str) -> re.Pattern:
        re_obj = re.compile(pattern, flags)
        return re_obj


FORTRAN_EXTENSIONS = [".f", ".for", ".ftn", ".f90", ".f95", ".f03", ".fpp"]
FORTRAN_EXTENSIONS += [x.upper() for x in FORTRAN_EXTENSIONS]
FORMATTER_ERROR_MESSAGE = (
    " Wrong usage of formatting-specific directives" " '&', '!&', '!&<' or '!&>'."
)
LINESPLIT_MESSAGE = (
    "auto indentation failed due to chars limit, " "line should be split"
)
EOL_STR = r"\s*;?\s*$"  # end of fortran line
EOL_SC = r"\s*;\s*$"  # whether line is ended with semicolon
SOL_STR = r"^\s*"  # start of fortran line
RE_FLAGS = re.IGNORECASE | re.UNICODE
STATEMENT_LABEL_RE = PatternWithName.compile(r"^\s*(\d+\s)(?!" + EOL_STR + ")", RE_FLAGS, "STATEMENT_LABEL_RE")
IF_RE = PatternWithName.compile(SOL_STR + r"(\w+\s*:)?\s*IF\s*\(.*\)\s*THEN" + EOL_STR, RE_FLAGS, "IF_RE")
ELSE_RE = PatternWithName.compile(SOL_STR + r"ELSE(\s*IF\s*\(.*\)\s*THEN)?" + EOL_STR, RE_FLAGS, "ELSE_RE")
ENDIF_RE = PatternWithName.compile(SOL_STR + r"END\s*IF(\s+\w+)?" + EOL_STR, RE_FLAGS, "")
DO_RE = PatternWithName.compile(SOL_STR + r"(\w+\s*:)?\s*DO(" + EOL_STR + r"|\s+\w)", RE_FLAGS, "DO_RE")
ENDDO_RE = PatternWithName.compile(SOL_STR + r"END\s*DO(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDDO_RE")
SELCASE_RE = PatternWithName.compile(
    SOL_STR + r"SELECT\s*(CASE|RANK|TYPE)\s*\(.*\)" + EOL_STR, RE_FLAGS, "SELCASE_RE"
)
CASE_RE = PatternWithName.compile(
    SOL_STR
    + r"((CASE|RANK|TYPE\s+IS|CLASS\s+IS)\s*(\(.*\)|DEFAULT)|CLASS\s+DEFAULT)"
    + EOL_STR,
    RE_FLAGS,
    "CASE_RE",
)
ENDSEL_RE = PatternWithName.compile(SOL_STR + r"END\s*SELECT" + EOL_STR, RE_FLAGS, "ENDSEL_RE")
ASSOCIATE_RE = PatternWithName.compile(SOL_STR + r"ASSOCIATE\s*\(.*\)" + EOL_STR, RE_FLAGS, "ASSOCIATE_RE")
ENDASSOCIATE_RE = PatternWithName.compile(SOL_STR + r"END\s*ASSOCIATE" + EOL_STR, RE_FLAGS, "ENDASSOCIATE_RE")
BLK_RE = PatternWithName.compile(SOL_STR + r"(\w+\s*:)?\s*BLOCK" + EOL_STR, RE_FLAGS, "BLK_RE")
ENDBLK_RE = PatternWithName.compile(SOL_STR + r"END\s*BLOCK(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDBLK_RE")
SUBR_RE = PatternWithName.compile(r"^([^\"']* )?SUBROUTINE\s+\w+\s*(\(.*\))?" + EOL_STR, RE_FLAGS, "SUBR_RE")
ENDSUBR_RE = PatternWithName.compile(SOL_STR + r"END\s*SUBROUTINE(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDSUBR_RE")
FCT_RE = PatternWithName.compile(
    r"^([^\"']* )?FUNCTION\s+\w+\s*(\(.*\))?(\s*RESULT\s*\(\w+\))?" + EOL_STR, RE_FLAGS, "FCT_RE"
)
ENDFCT_RE = PatternWithName.compile(SOL_STR + r"END\s*FUNCTION(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDFCT_RE")
MOD_RE = PatternWithName.compile(SOL_STR + r"MODULE\s+\w+" + EOL_STR, RE_FLAGS, "MOD_RE")
ENDMOD_RE = PatternWithName.compile(SOL_STR + r"END\s*MODULE(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDMOD_RE")
SMOD_RE = PatternWithName.compile(SOL_STR + r"SUBMODULE\s*\(\w+\)\s+\w+" + EOL_STR, RE_FLAGS, "SMOD_RE")
ENDSMOD_RE = PatternWithName.compile(SOL_STR + r"END\s*SUBMODULE(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDSMOD_RE")
TYPE_RE = PatternWithName.compile(
    SOL_STR
    + r"TYPE(\s*,\s*(BIND\s*\(\s*C\s*\)|EXTENDS\s*\(.*\)|ABSTRACT|PUBLIC|PRIVATE))*(\s*,\s*)?(\s*::\s*|\s+)\w+"
    + EOL_STR,
    RE_FLAGS,
    "TYPE_RE",
)
ENDTYPE_RE = PatternWithName.compile(SOL_STR + r"END\s*TYPE(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDTYPE_RE")
PROG_RE = PatternWithName.compile(SOL_STR + r"PROGRAM\s+\w+" + EOL_STR, RE_FLAGS, "PROG_RE")
ENDPROG_RE = PatternWithName.compile(SOL_STR + r"END\s*PROGRAM(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDPROG_RE")
INTERFACE_RE = PatternWithName.compile(
    r"^([^\"']* )?INTERFACE(\s+\w+|\s+(OPERATOR|ASSIGNMENT)\s*\(.*\))?" + EOL_STR,
    RE_FLAGS,
    "INTERFACE_RE",
)
ENDINTERFACE_RE = PatternWithName.compile(
    SOL_STR + r"END\s*INTERFACE(\s+\w+|\s+(OPERATOR|ASSIGNMENT)\s*\(.*\))?" + EOL_STR,
    RE_FLAGS,
    "ENDINTERFACE_RE",
)
CONTAINS_RE = PatternWithName.compile(SOL_STR + r"CONTAINS" + EOL_STR, RE_FLAGS, "CONTAINS_RE")
ENUM_RE = PatternWithName.compile(
    SOL_STR + r"ENUM(\s*,\s*(BIND\s*\(\s*C\s*\)))?((\s*::\s*|\s+)\w+)?" + EOL_STR,
    RE_FLAGS,
    "ENUM_RE",
)
ENDENUM_RE = PatternWithName.compile(SOL_STR + r"END\s*ENUM(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDENUM_RE")
ENDANY_RE = PatternWithName.compile(SOL_STR + r"END" + EOL_STR, RE_FLAGS, "ENDANY_RE")
FORALL_RE = PatternWithName.compile(SOL_STR + r"(\w+\s*:)?\s*FORALL\s*\(.*\)" + EOL_STR, RE_FLAGS, "FORALL_RE")
ENDFORALL_RE = PatternWithName.compile(SOL_STR + r"END\s*FORALL(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDFORALL_RE")
WHERE_RE = PatternWithName.compile(SOL_STR + r"(\w+\s*:)?\s*WHERE\s*\(.*\)" + EOL_STR, RE_FLAGS, "WHERE_RE")
ELSEWHERE_RE = PatternWithName.compile(
    SOL_STR + r"ELSE\s*WHERE(\(.*\))?(\s*\w+)?" + EOL_STR, RE_FLAGS, "ELSEWHERE_RE"
)
ENDWHERE_RE = PatternWithName.compile(SOL_STR + r"END\s*WHERE(\s+\w+)?" + EOL_STR, RE_FLAGS, "ENDWHERE_RE")
FYPP_DEF_RE = PatternWithName.compile(SOL_STR + r"#:DEF\s+", RE_FLAGS, "FYPP_DEF_RE")
FYPP_ENDDEF_RE = PatternWithName.compile(SOL_STR + r"#:ENDDEF", RE_FLAGS, "FYPP_ENDDEF_RE")
FYPP_IF_RE = PatternWithName.compile(SOL_STR + r"#:IF\s+", RE_FLAGS, "FYPP_IF_RE")
FYPP_ELIF_ELSE_RE = PatternWithName.compile(SOL_STR + r"#:(ELIF\s+|ELSE)", RE_FLAGS, "FYPP_ELIF_ELSE_RE")
FYPP_ENDIF_RE = PatternWithName.compile(SOL_STR + r"#:ENDIF", RE_FLAGS, "FYPP_ENDIF_RE")
FYPP_FOR_RE = PatternWithName.compile(SOL_STR + r"#:FOR\s+", RE_FLAGS, "")
FYPP_ENDFOR_RE = PatternWithName.compile(SOL_STR + r"#:ENDFOR", RE_FLAGS, "FYPP_ENDFOR_RE")
FYPP_BLOCK_RE = PatternWithName.compile(SOL_STR + r"#:BLOCK\s+", RE_FLAGS, "FYPP_BLOCK_RE")
FYPP_ENDBLOCK_RE = PatternWithName.compile(SOL_STR + r"#:ENDBLOCK", RE_FLAGS, "FYPP_ENDBLOCK_RE")
FYPP_CALL_RE = PatternWithName.compile(SOL_STR + r"#:CALL\s+", RE_FLAGS, "FYPP_CALL_RE")
FYPP_ENDCALL_RE = PatternWithName.compile(SOL_STR + r"#:ENDCALL", RE_FLAGS, "FYPP_ENDCALL_RE")
FYPP_MUTE_RE = PatternWithName.compile(SOL_STR + r"#:MUTE", RE_FLAGS, "FYPP_MUTE_RE")
FYPP_ENDMUTE_RE = PatternWithName.compile(SOL_STR + r"#:ENDMUTE", RE_FLAGS, "FYPP_ENDMUTE_RE")
PRIVATE_RE = PatternWithName.compile(SOL_STR + r"PRIVATE\s*::", RE_FLAGS, "PRIVATE_RE")
PUBLIC_RE = PatternWithName.compile(SOL_STR + r"PUBLIC\s*::", RE_FLAGS, "PUBLIC_RE")
END_RE = PatternWithName.compile(
    SOL_STR
    + r"(END)\s*(IF|DO|SELECT|ASSOCIATE|BLOCK|SUBROUTINE|FUNCTION|MODULE|SUBMODULE|TYPE|PROGRAM|INTERFACE|ENUM|WHERE|FORALL)",
    RE_FLAGS,
    "END_RE",
)
INTR_STMTS_PAR = (
    r"(ALLOCATE|DEALLOCATE|"
    r"OPEN|CLOSE|READ|WRITE|"
    r"FLUSH|ENDFILE|REWIND|BACKSPACE|INQUIRE|"
    r"FORALL|WHERE|ASSOCIATE|NULLIFY)"
)
LINEBREAK_STR = r"(&)[\s]*(?:!.*)?$"
PLUSMINUS_RE = PatternWithName.compile(r"(?<=[\w\)\]])\s*(\+|-)\s*", RE_FLAGS, "PLUSMINUS_RE")
MULTDIV_RE = PatternWithName.compile(
    r"(?<=[\w\)\]])\s*((?<!\*)\*(?!\*)|(?<!/)/(?!/))(?=[\s\w\(])(?!.*::)", RE_FLAGS, "MULTDIV_RE"
)
REL_OP_RE = PatternWithName.compile(
    r"(?<!\()\s*(\.(?:EQ|NE|LT|LE|GT|GE)\.|(?:==|\/=|<(?!=)|<=|(?<!=)>(?!=)|>=))\s*(?!\))",
    RE_FLAGS,
    "REL_OP_RE",
)
LOG_OP_RE = PatternWithName.compile(r"\s*(\.(?:AND|OR|EQV|NEQV)\.)\s*", RE_FLAGS, "LOG_OP_RE")
PRINT_RE = PatternWithName.compile(r"(?:(?<=\bPRINT)|(?<=\bREAD))\s*(\*,?)\s*", RE_FLAGS, "PRINT_RE")
DEL_OPEN_STR = r"(\(\/?|\[)"
DEL_OPEN_RE = PatternWithName.compile(r"^" + DEL_OPEN_STR, RE_FLAGS, "DEL_OPEN_RE")
DEL_CLOSE_STR = r"(\/?\)|\])"
DEL_CLOSE_RE = PatternWithName.compile(r"^" + DEL_CLOSE_STR, RE_FLAGS, "PatternWithName")
EMPTY_RE = PatternWithName.compile(SOL_STR + r"$", RE_FLAGS, "PatternWithName")
PREPRO_NEW_SCOPE = [
    RegexParser(FYPP_DEF_RE),
    RegexParser(FYPP_IF_RE),
    RegexParser(FYPP_FOR_RE),
    RegexParser(FYPP_BLOCK_RE),
    RegexParser(FYPP_CALL_RE),
    RegexParser(FYPP_MUTE_RE),
]
PREPRO_CONTINUE_SCOPE = [None, RegexParser(FYPP_ELIF_ELSE_RE), None, None, None, None]
PREPRO_END_SCOPE = [
    RegexParser(FYPP_ENDDEF_RE),
    RegexParser(FYPP_ENDIF_RE),
    RegexParser(FYPP_ENDFOR_RE),
    RegexParser(FYPP_ENDBLOCK_RE),
    RegexParser(FYPP_ENDCALL_RE),
    RegexParser(FYPP_ENDMUTE_RE),
]
FPRETTIY_ANNOTATION_RE = PatternWithName.compile("^\s*!\s*fprettify:\s*(.*)\s*$", RE_FLAGS, "FPRETTIY_ANNOTATION_RE")
LR_OPS_RE = [REL_OP_RE, LOG_OP_RE, PlusMinusParser(PLUSMINUS_RE), MULTDIV_RE, PRINT_RE]
USE_RE = PatternWithName.compile(
    SOL_STR + "USE(\s+|(,.+?)?::\s*)\w+?((,.+?=>.+?)+|,\s*only\s*:.+?)?$" + EOL_STR,
    RE_FLAGS,
    "USE_RE",
)
NO_ALIGN_RE = PatternWithName.compile(SOL_STR + r"&\s*[^\s*]+", 0, "NO_ALIGN_RE")
# forall_parser = where_parser
NML_RE = PatternWithName.compile(r"(/\w+/)", RE_FLAGS, "NML_RE")
NML_STMT_RE = PatternWithName.compile(SOL_STR + r"NAMELIST.*/.*/", RE_FLAGS, "NML_STMT_RE")
DATA_STMT_RE = PatternWithName.compile(SOL_STR + r"DATA\s+\w", RE_FLAGS, "DATA_STMT_RE")
CUDA_CHEVRONS_RE = PatternWithName.compile(r"<<<.*>>>", RE_FLAGS, "CUDA_CHEVRONS_RE")
F90_KEYWORDS_RE = PatternWithName.compile(
    r"\b("
    + "|".join(
        (
            "allocatable",
            "allocate",
            "assign",
            "assignment",
            "backspace",
            "block",
            "call",
            "case",
            "character",
            "close",
            "common",
            "complex",
            "contains",
            "continue",
            "cycle",
            "data",
            "deallocate",
            "dimension",
            "do",
            "double",
            "else",
            "elseif",
            "elsewhere",
            "end",
            "enddo",
            "endfile",
            "endif",
            "entry",
            "equivalence",
            "exit",
            "external",
            "forall",
            "format",
            "function",
            "goto",
            "if",
            "implicit",
            "include",
            "inquire",
            "integer",
            "intent",
            "interface",
            "intrinsic",
            "logical",
            "module",
            "namelist",
            "none",
            "nullify",
            "only",
            "open",
            "operator",
            "optional",
            "parameter",
            "pause",
            "pointer",
            "precision",
            "print",
            "private",
            "procedure",
            "program",
            "public",
            "read",
            "real",
            "recursive",
            "result",
            "return",
            "rewind",
            "save",
            "select",
            "sequence",
            "stop",
            "subroutine",
            "target",
            "then",
            "type",
            "use",
            "where",
            "while",
            "write",
            ## F95 keywords.
            "elemental",
            "pure",
            ## F2003
            "abstract",
            "associate",
            "asynchronous",
            "bind",
            "class",
            "deferred",
            "enum",
            "enumerator",
            "extends",
            "extends_type_of",
            "final",
            "generic",
            "import",
            "non_intrinsic",
            "non_overridable",
            "nopass",
            "pass",
            "protected",
            "same_type_as",
            "value",
            "volatile",
            ## F2008.
            "contiguous",
            "submodule",
            "concurrent",
            "codimension",
            "sync all",
            "sync memory",
            "critical",
            "image_index",
        )
    )
    + r")\b",
    RE_FLAGS,
    "F90_KEYWORDS_RE",
)
F90_PROCEDURES_RE = PatternWithName.compile(
    r"\b("
    + "|".join(
        (
            "abs",
            "achar",
            "acos",
            "adjustl",
            "adjustr",
            "aimag",
            "aint",
            "all",
            "allocated",
            "anint",
            "any",
            "asin",
            "associated",
            "atan",
            "atan2",
            "bit_size",
            "btest",
            "ceiling",
            "char",
            "cmplx",
            "conjg",
            "cos",
            "cosh",
            "count",
            "cshift",
            "date_and_time",
            "dble",
            "digits",
            "dim",
            "dot_product",
            "dprod",
            "eoshift",
            "epsilon",
            "exp",
            "exponent",
            "floor",
            "fraction",
            "huge",
            "iachar",
            "iand",
            "ibclr",
            "ibits",
            "ibset",
            "ichar",
            "ieor",
            "index",
            "int",
            "ior",
            "ishft",
            "ishftc",
            "kind",
            "lbound",
            "len",
            "len_trim",
            "lge",
            "lgt",
            "lle",
            "llt",
            "log",
            "log10",
            "logical",
            "matmul",
            "max",
            "maxexponent",
            "maxloc",
            "maxval",
            "merge",
            "min",
            "minexponent",
            "minloc",
            "minval",
            "mod",
            "modulo",
            "mvbits",
            "nearest",
            "nint",
            "not",
            "pack",
            "precision",
            "present",
            "product",
            "radix",
            ## Real is taken out here to avoid highlighting declarations.
            "random_number",
            "random_seed",
            "range",  ## "real"
            "repeat",
            "reshape",
            "rrspacing",
            "scale",
            "scan",
            "selected_int_kind",
            "selected_real_kind",
            "set_exponent",
            "shape",
            "sign",
            "sin",
            "sinh",
            "size",
            "spacing",
            "spread",
            "sqrt",
            "sum",
            "system_clock",
            "tan",
            "tanh",
            "tiny",
            "transfer",
            "transpose",
            "trim",
            "ubound",
            "unpack",
            "verify",
            ## F95 intrinsic functions.
            "null",
            "cpu_time",
            ## F2003.
            "move_alloc",
            "command_argument_count",
            "get_command",
            "get_command_argument",
            "get_environment_variable",
            "selected_char_kind",
            "wait",
            "flush",
            "new_line",
            "extends",
            "extends_type_of",
            "same_type_as",
            "bind",
            ## F2003 ieee_arithmetic intrinsic module.
            "ieee_support_underflow_control",
            "ieee_get_underflow_mode",
            "ieee_set_underflow_mode",
            ## F2003 iso_c_binding intrinsic module.
            "c_loc",
            "c_funloc",
            "c_associated",
            "c_f_pointer",
            "c_f_procpointer",
            ## F2008.
            "bge",
            "bgt",
            "ble",
            "blt",
            "dshiftl",
            "dshiftr",
            "leadz",
            "popcnt",
            "poppar",
            "trailz",
            "maskl",
            "maskr",
            "shifta",
            "shiftl",
            "shiftr",
            "merge_bits",
            "iall",
            "iany",
            "iparity",
            "storage_size",
            "bessel_j0",
            "bessel_j1",
            "bessel_jn",
            "bessel_y0",
            "bessel_y1",
            "bessel_yn",
            "erf",
            "erfc",
            "erfc_scaled",
            "gamma",
            "hypot",
            "log_gamma",
            "norm2",
            "parity",
            "findloc",
            "is_contiguous",
            "sync images",
            "lock",
            "unlock",
            "image_index",
            "lcobound",
            "ucobound",
            "num_images",
            "this_image",
            ## F2008 iso_fortran_env module.
            "compiler_options",
            "compiler_version",
            ## F2008 iso_c_binding module.
            "c_sizeof",
        )
    )
    + r")\b",
    RE_FLAGS,
    "F90_PROCEDURES_RE",
)
F90_MODULES_RE = PatternWithName.compile(
    r"\b("
    + "|".join(
        (
            ## F2003/F2008 module names
            "iso_fortran_env",
            "iso_c_binding",
            "ieee_exceptions",
            "ieee_arithmetic",
            "ieee_features",
        )
    )
    + r")\b",
    RE_FLAGS,
    "F90_MODULES_RE",
)
F90_OPERATORS_RE = PatternWithName.compile(
    r"("
    + "|".join(
        [
            r"\." + a + r"\."
            for a in (
            "and",
            "eq",
            "eqv",
            "false",
            "ge",
            "gt",
            "le",
            "lt",
            "ne",
            "neqv",
            "not",
            "or",
            "true",
        )
        ]
    )
    + r")",
    RE_FLAGS,
    "F90_OPERATORS_RE",
)
F90_CONSTANTS_RE = PatternWithName.compile(
    r"\b("
    + "|".join(
        (
            ## F2003 iso_fortran_env constants.
            "input_unit",
            "output_unit",
            "error_unit",
            "iostat_end",
            "iostat_eor",
            "numeric_storage_size",
            "character_storage_size",
            "file_storage_size",
            ## F2003 iso_c_binding constants.
            "c_int",
            "c_short",
            "c_long",
            "c_long_long",
            "c_signed_char",
            "c_size_t",
            "c_int8_t",
            "c_int16_t",
            "c_int32_t",
            "c_int64_t",
            "c_int_least8_t",
            "c_int_least16_t",
            "c_int_least32_t",
            "c_int_least64_t",
            "c_int_fast8_t",
            "c_int_fast16_t",
            "c_int_fast32_t",
            "c_int_fast64_t",
            "c_intmax_t",
            "c_intptr_t",
            "c_float",
            "c_double",
            "c_long_double",
            "c_float_complex",
            "c_double_complex",
            "c_long_double_complex",
            "c_bool",
            "c_char",
            "c_null_char",
            "c_alert",
            "c_backspace",
            "c_form_feed",
            "c_new_line",
            "c_carriage_return",
            "c_horizontal_tab",
            "c_vertical_tab",
            "c_ptr",
            "c_funptr",
            "c_null_ptr",
            "c_null_funptr",
            ## F2008 iso_fortran_env constants.
            "character_kinds",
            "int8",
            "int16",
            "int32",
            "int64",
            "integer_kinds",
            "iostat_inquire_internal_unit",
            "logical_kinds",
            "real_kinds",
            "real32",
            "real64",
            "real128",
            "lock_type",
            "atomic_int_kind",
            "atomic_logical_kind",
        )
    )
    + r")\b",
    RE_FLAGS,
    "F90_CONSTANTS_RE",
)
F90_INT_RE = r"[-+]?[0-9]+"
F90_FLOAT_RE = r"[-+]?([0-9]+\.[0-9]*|\.[0-9]+)"
F90_NUMBER_RE = "(" + F90_INT_RE + "|" + F90_FLOAT_RE + ")"
F90_FLOAT_EXP_RE = F90_NUMBER_RE + r"[eEdD]" + F90_NUMBER_RE
F90_NUMBER_ALL_RE = "(" + F90_NUMBER_RE + "|" + F90_FLOAT_EXP_RE + ")"
F90_NUMBER_ALL_REC = PatternWithName.compile(F90_NUMBER_ALL_RE, RE_FLAGS, "F90_NUMBER_ALL_REC")
F90_CONSTANTS_TYPES_RE = PatternWithName.compile(
    r"("
    + F90_NUMBER_ALL_RE
    + ")_("
    + "|".join(
        (
            ## F2003 iso_c_binding constants.
            "c_int",
            "c_short",
            "c_long",
            "c_long_long",
            "c_signed_char",
            "c_size_t",
            "c_int8_t",
            "c_int16_t",
            "c_int32_t",
            "c_int64_t",
            "c_int_least8_t",
            "c_int_least16_t",
            "c_int_least32_t",
            "c_int_least64_t",
            "c_int_fast8_t",
            "c_int_fast16_t",
            "c_int_fast32_t",
            "c_int_fast64_t",
            "c_intmax_t",
            "c_intptr_t",
            "c_float",
            "c_double",
            "c_long_double",
            "c_float_complex",
            "c_double_complex",
            "c_long_double_complex",
            "c_bool",
            "c_char",
            ## F2008 iso_fortran_env constants.
            "character_kinds",
            "int8",
            "int16",
            "int32",
            "int64",
            "integer_kinds",
            "logical_kinds",
            "real_kinds",
            "real32",
            "real64",
            "real128",
            "lock_type",
            "atomic_int_kind",
            "atomic_logical_kind",
        )
    )
    + r")\b",
    RE_FLAGS,
    "F90_CONSTANTS_TYPES_RE",
)
VAR_DECL_RE = PatternWithName.compile(
    r"^ *(?P<type>integer(?: *\* *[0-9]+)?|logical|character(?: *\* *[0-9]+)?|real(?: *\* *[0-9]+)?|complex(?: *\* *[0-9]+)?|type) *(?P<parameters>\((?:[^()]+|\((?:[^()]+|\([^()]*\))*\))*\))? *(?P<attributes>(?: *, *[a-zA-Z_0-9]+(?: *\((?:[^()]+|\((?:[^()]+|\([^()]*\))*\))*\))?)+)? *(?P<dpnt>::)?(?P<vars>[^\n]+)\n?",
    RE_FLAGS,
    "VAR_DECL_RE",
)
OMP_COND_RE = PatternWithName.compile(r"^\s*(!\$ )", RE_FLAGS, "OMP_COND_RE")
OMP_DIR_RE = PatternWithName.compile(r"^\s*(!\$OMP)", RE_FLAGS, "OMP_DIR_RE")
FYPP_LINE_STR = r"^(#!|#:|\$:|@:)"
FYPP_WITHOUT_PREPRO_STR = r"^(#!|\$:|@:)"
CPP_STR = r"^#[^!:{}]"
COMMENT_LINE_STR = r"^!"
FYPP_OPEN_STR = r"(#{|\${|@{)"
FYPP_CLOSE_STR = r"(}#|}\$|}@)"
NOTFORTRAN_LINE_RE = PatternWithName.compile(
    r"(" + FYPP_LINE_STR + r"|" + CPP_STR + r"|" + COMMENT_LINE_STR + r")", RE_FLAGS, "NOTFORTRAN_LINE_RE"
)
NOTFORTRAN_FYPP_LINE_RE = PatternWithName.compile(
    r"(" + CPP_STR + r"|" + COMMENT_LINE_STR + r")", RE_FLAGS, "NOTFORTRAN_FYPP_LINE_RE"
)
FYPP_LINE_RE = PatternWithName.compile(FYPP_LINE_STR, RE_FLAGS, "FYPP_LINE_RE")
FYPP_WITHOUT_PREPRO_RE = PatternWithName.compile(FYPP_WITHOUT_PREPRO_STR, RE_FLAGS, "FYPP_WITHOUT_PREPRO_RE")
FYPP_OPEN_RE = PatternWithName.compile(FYPP_OPEN_STR, RE_FLAGS, "FYPP_OPEN_RE")
FYPP_CLOSE_RE = PatternWithName.compile(FYPP_CLOSE_STR, RE_FLAGS, "FYPP_CLOSE_RE")
STR_OPEN_RE = PatternWithName.compile(r"(" + FYPP_OPEN_STR + r"|" + r"'|\"|!)", RE_FLAGS, "STR_OPEN_RE")
CPP_RE = PatternWithName.compile(CPP_STR, RE_FLAGS, "CPP_RE")


def get_pattern_name(pattern: re.Pattern) -> str:
    value_dict = globals()
    value_name = value_dict.keys()
    pattern_name_list = [x for x in value_name if x.endswith("_RE")]

    for _name in pattern_name_list:
        if pattern == value_dict[_name]:
            return _name

    return ""
