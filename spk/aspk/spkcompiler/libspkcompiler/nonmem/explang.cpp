/* A Bison parser, made from explang.ypp
   by GNU bison 1.35.  */

#define YYBISON 1  /* Identify Bison output.  */

#define yyparse nm_parse
#define yylex nm_lex
#define yyerror nm_error
#define yylval nm_lval
#define yychar nm_char
#define yydebug nm_debug
#define yynerrs nm_nerrs
# define	NAME	257
# define	OPEN_ARRAY_ELEM_LIST	258
# define	CLOSE_ARRAY_ELEM_LIST	259
# define	POWER_OP	260
# define	DEFINED_UNARY_FUNCTION	261
# define	DIGIT_STRING	262
# define	ENG_NOTATION	263
# define	SIGNIFICAND	264
# define	COMMENT	265
# define	EQ_OP	266
# define	NE_OP	267
# define	LT_OP	268
# define	LE_OP	269
# define	GT_OP	270
# define	GE_OP	271
# define	AND_OP	272
# define	OR_OP	273
# define	NOT_OP	274
# define	EQV_OP	275
# define	NEQV_OP	276
# define	EXIT	277
# define	IF	278
# define	THEN	279
# define	ELSE	280
# define	ENDIF	281
# define	TRUE	282
# define	FALSE	283
# define	EOF_MARKER	284
# define	SIGN	285

#line 1 "explang.ypp"

/**
 * @file explang.cpp
 * This file is generated from the YACC (BISON) syntax analyzer 
 * specification file, nonmem/explang.ypp, which defines the nm_parse() for NONMEM.
 * @ingroup nonmem
 */
/**
 * @example explangTest.cpp
 */

#include <iostream>
#include <fstream>
#include <string>
#include "../SymbolTable.h"

using namespace std;

/**
 * Global counter for errors encountered during parsing.
 * This counter is initialized to zero at compilation time only
 * and it is the caller's responsibility to re-initialize the value
 * when necessarily.  In other words, nm_parse() does not reset the value.
 */
int gSpkExpErrors = 0;

/**
 * Global counter for lines that have been read so far during parsing.
 * This counter is initialized to zero at compilation time only
 * and it is the caller's responsibility to re-initialize the value
 * when necessarily.  In other words, nm_parse() does not reset the value.
 */
int gSpkExpLines  = 0;

/**
 * Global pointer to a SymbolTable object.
 *
 * This pointer has to be initialized to point to a valid object
 * in the caller (of nm_parse()) space.  The allocated resource
 * must be released by the caller too.
 *
 * @note If the pointer is found to be pointing to NULL at
 * the very beginning of parsing, nm_parse() will call
 * nm_error() which in turn forces nm_parse() to return immediately with
 * a non-zero value.
 */
SymbolTable * gSpkExpSymbolTable;

extern "C"{
   /**
    * This parser converts "algebraic expressions + control statements"
    *  written in
    * the NONMEM Abbriviated language to C++ language.
    * The NONMEM version of nm_lex() is expected to tokenize the input,
    * convert it to all @em lower-case and pass back to this routine.  
    * The input expressions must be
    * stored in an open readable file pointed by a FILE handler, @a nm_in.
    * The generated C++ source code is appended to an open writable
    * file pointed by a FILE handler, gSpkExpOut.
    * output to a file pointed by yet another file handler, gSpkExpOutput.
    * The grobal counter, gSpkExpErrors, shall be incremented
    * as nm_parse() or nm_lex() encounters syntax errors.
    * 
    * @note The code for this function is generated from
    * a YACC (BISON) specification file, explang.ypp.  
    * 
    * The source language allows only the following statements:
    * - assignment
    * - if conditional statement in the form of either if-assign or if-then-else
    *
    * The functions and operators it supports are:
    * - sqrt, log, log10, exp
    * - + (addition), - (subtraction), * (multiplication), / (division), ** (power)
    *
    * The primitive data types it supports are:
    * - real (eq. float)
    * - double real (eq. double)
    * - integer (eq. int) for indexing arrays
    * - logical (eq. bool)
    *
    * The types of data containers allowed are:
    * - scalar
    * - one dimensional array
    * - two dimensional array (not supported as of 09/24/03)
    *
    * The methods to access array elements of a dimensional space are:
    * - by constant scalar indexing
    * - by providing constant start and end indeces (not supported as of 09/24/03)
    * - by providing constant start and end indeces and a stride (not supported as of 09/24/03)
    * (thus, for a vector, you have a(start:end:stride) and for a matrix, A(start:end:stride, start:end:stride).
    *
    */
    int nm_parse(void);

   /**
    * The lexical analyzer for NONMEM Abbriviated code.
    * The input string stream nm_lex() reads in is assumed to 
    * be pointed by a FILE handler, @a nm_in.
    *
    * @note The code for this function is generated from
    * a LEX (FLEX) specification file, lex_explang.l.
    */
    int nm_lex(void);

    int nm_wrap()
    {
      return 1;
    }
   /**
    * The error handler used by nm_parse() when it finds an error.  
    * Currently, it is only to display an error message and
    * and return from nm_parse() with a value -1.
    *
    * @todo This error handler forces nm_parse() to immediately return.
    * It probably want to do more than that.
    *
    */
    int nm_error( char * err_message )
    { 
      ++gSpkExpErrors;
      cerr <<  "!!! Error (" << gSpkExpErrors << ") at line " << gSpkExpLines << ": " << endl;
      cerr << "\t" << err_message << endl;
    
      return -1;
    }

   /**
    * A signal that forces nm_parse() to immediately return normally.
    */
    int NM_ACCEPT;

   /**
    * A signal that forces nm_parse() to immediately return abnormaly.
    */
    int NM_ABORT;

   /**
    * Global pointer to a FILE handler to which output is redirected.
    */
    FILE * gSpkExpOutput;

   /**
    * The file hander pointing to the input file to read in.  It must be open & readable.
    */
    extern FILE * nm_in;
};

namespace{
   const char * const STR_TYPE     = "type";
   const char * const STR_VALUE    = "value";
   const char * const STR_NAME     = "name";
   const char * const STR_OPERATOR = "operator";
   const char * const STR_OPERAND  = "operand";
   const char * const STR_SIGN     = "sign";
   const char * const STR_ROWS     = "rows";
   const char * const STR_COLS     = "cols";
   const char * const STR_LHS      = "lhs";
   const char * const STR_RHS      = "rhs";
   const char * const STR_BINARY   = "binary";
   const char * const STR_UNARY    = "unary";
   const char * const STR_FUNCTION = "function";
   const char * const STR_ARGC     = "argc";
   const char * const STR_STRUCTURE= "structure";
   const char * const STR_INDEX    = "index";
   const char * const STR_START    = "start";
   const char * const STR_END      = "end";
   const char * const STR_STRIDE   = "stride";
   const char * const STR_CONSTANT = "constant";
   const char * const STR_TRUE     = "true";
   const char * const STR_FALSE    = "false";
   const char * const STR_COMMENT  = "comment";
   const char * const STR_EXIT     = "exit";

   const char * const STR_ASSIGN   = "assign";
   const char * const STR_ARRAY_ELEMENT_LIST = "array_element_list";
   const char * const STR_VARIABLE = "variable";
   const char * const STR_PRIORITIZED = "prioritized";
   const char * const STR_IF       = "if";
   const char * const STR_ELSE     = "else";
   const char * const STR_THEN     = "then";
   const char * const STR_CONDITION= "condition";
   const char * const STR_BLOCK    = "block";

   const char * const STR_SQRT     = "sqrt";
   const char * const STR_EXP      = "exp";
   const char * const STR_LOG      = "log";
   const char * const STR_LOG10    = "log10";
   const char * const STR_ADD      = "add";
   const char * const STR_SUBTRACT = "subtract";
   const char * const STR_MULTIPLY = "multiply";
   const char * const STR_DIVIDE   = "divide";
   const char * const STR_POWER    = "power";
   const char * const STR_MINUS    = "minus";
   const char * const STR_PLUS     = "plus";

   const char * const STR_EQ       = "eq";
   const char * const STR_NE       = "ne";
   const char * const STR_LT       = "lt";  
   const char * const STR_LE       = "le";
   const char * const STR_GT       = "gt";
   const char * const STR_GE       = "ge";
   const char * const STR_NXOR     = "nxor";
   const char * const STR_XOR      = "xor";
   const char * const STR_OR       = "or";
   const char * const STR_AND      = "and";
   const char * const STR_NEGATE   = "negate";

   const char * const STR_SEMICOLON= ";";
   const char * const STR_NEWLINE  = "\n";
   const char * const STR_TAB      = "\t";
   bool inRHS = false;
};

#line 215 "explang.ypp"
#ifndef YYSTYPE
typedef union{
  /**
   * A type of those tokens which return a character.
   */
  char   ch;

  /**
   * A type of those tokens which return a character string.
   */
  char*  c_str;
} yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif
#ifndef YYDEBUG
# define YYDEBUG 1
#endif



#define	YYFINAL		131
#define	YYFLAG		-32768
#define	YYNTBASE	42

/* YYTRANSLATE(YYLEX) -- Bison token number corresponding to YYLEX. */
#define YYTRANSLATE(x) ((unsigned)(x) <= 285 ? yytranslate[x] : 80)

/* YYTRANSLATE[YYLEX] -- Bison token number corresponding to YYLEX. */
static const char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      37,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      38,    39,    35,    32,    40,    33,     2,    36,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    41,     2,
       2,    31,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    34
};

#if YYDEBUG
static const short yyprhs[] =
{
       0,     0,     1,     4,     7,    10,    12,    14,    16,    19,
      22,    26,    28,    30,    32,    35,    39,    40,    41,    42,
      43,    51,    53,    55,    57,    59,    61,    66,    73,    75,
      79,    85,    87,    89,    91,    93,    95,    97,    99,   101,
     105,   107,   113,   117,   122,   126,   130,   136,   137,   140,
     142,   144,   146,   148,   150,   152,   154,   158,   160,   164,
     169,   173,   177,   180,   183,   187,   191,   195,   197,   201,
     205,   209,   213,   217,   221,   223,   225,   229,   233,   237,
     241,   244
};
static const short yyrhs[] =
{
      -1,    42,    44,     0,    42,    43,     0,    42,    30,     0,
      64,     0,    65,     0,    37,     0,    11,    37,     0,    45,
      37,     0,    45,    11,    37,     0,    47,     0,    46,     0,
      23,     0,    23,     8,     0,    23,     8,     8,     0,     0,
       0,     0,     0,    48,    52,    49,    31,    50,    63,    51,
       0,    53,     0,    54,     0,     3,     0,    55,     0,    56,
       0,     3,    38,    57,    39,     0,     3,    38,    57,    40,
      57,    39,     0,    61,     0,    58,    41,    59,     0,    58,
      41,    59,    41,    60,     0,    61,     0,    61,     0,    61,
       0,    77,     0,    70,     0,    52,     0,    74,     0,    76,
       0,    38,    63,    39,     0,    79,     0,    24,    38,    79,
      39,    47,     0,    66,    27,    37,     0,    66,    67,    27,
      37,     0,    68,    37,    69,     0,    26,    37,    69,     0,
      24,    38,    79,    39,    25,     0,     0,    69,    44,     0,
      71,     0,    72,     0,     8,     0,    10,     0,     9,     0,
      28,     0,    29,     0,     4,    75,     5,     0,    63,     0,
      75,    40,    63,     0,     7,    38,    77,    39,     0,    77,
      32,    77,     0,    77,    33,    77,     0,    33,    77,     0,
      32,    77,     0,    77,    35,    77,     0,    77,    36,    77,
       0,    77,     6,    77,     0,    62,     0,    78,    12,    78,
       0,    78,    13,    78,     0,    78,    14,    78,     0,    78,
      15,    78,     0,    78,    16,    78,     0,    78,    17,    78,
       0,    77,     0,    73,     0,    79,    21,    79,     0,    79,
      22,    79,     0,    79,    19,    79,     0,    79,    18,    79,
       0,    20,    79,     0,    78,     0
};

#endif

#if YYDEBUG
/* YYRLINE[YYN] -- source line where rule number YYN was defined. */
static const short yyrline[] =
{
       0,   334,   342,   356,   371,   394,   399,   413,   424,   439,
     447,   473,   477,   513,   519,   527,   543,   543,   543,   543,
     543,   563,   568,   574,   593,   597,   613,   649,   665,   670,
     676,   691,   705,   719,   733,   756,   761,   766,   771,   776,
     793,   805,   828,   836,   846,   857,   875,   897,   902,   930,
     934,   947,   964,   971,   988,   995,  1012,  1028,  1034,  1051,
    1097,  1106,  1115,  1123,  1132,  1141,  1150,  1159,  1179,  1188,
    1197,  1206,  1215,  1224,  1233,  1237,  1259,  1268,  1277,  1286,
    1295,  1303
};
#endif


#if (YYDEBUG) || defined YYERROR_VERBOSE

/* YYTNAME[TOKEN_NUM] -- String name of the token TOKEN_NUM. */
static const char *const yytname[] =
{
  "$", "error", "$undefined.", "NAME", "OPEN_ARRAY_ELEM_LIST", 
  "CLOSE_ARRAY_ELEM_LIST", "POWER_OP", "DEFINED_UNARY_FUNCTION", 
  "DIGIT_STRING", "ENG_NOTATION", "SIGNIFICAND", "COMMENT", "EQ_OP", 
  "NE_OP", "LT_OP", "LE_OP", "GT_OP", "GE_OP", "AND_OP", "OR_OP", 
  "NOT_OP", "EQV_OP", "NEQV_OP", "EXIT", "IF", "THEN", "ELSE", "ENDIF", 
  "TRUE", "FALSE", "EOF_MARKER", "'='", "'+'", "'-'", "SIGN", "'*'", 
  "'/'", "'\\n'", "'('", "')'", "','", "':'", "input", "if_stmt_or_block", 
  "line", "statement", "exit_stmt", "assignment_stmt", "@1", "@2", "@3", 
  "@4", "variable", "whole_object", "subobject", 
  "vector_section_variable", "matrix_section_variable", "slice", 
  "start_subscript", "end_subscript", "stride", "int_expr", "primary", 
  "expr", "if_stmt", "if_construct", "if_then_clause", "else_clause", 
  "if_then_stmt", "block", "literal_constant", "int_literal_constant", 
  "real_literal_constant", "logical_constant", "array_constructor", 
  "array_element_list", "function_ref", "arithmatic_expr", 
  "relational_expr", "logical_expr", 0
};
#endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives. */
static const short yyr1[] =
{
       0,    42,    42,    42,    42,    43,    43,    44,    44,    44,
      44,    45,    45,    46,    46,    46,    48,    49,    50,    51,
      47,    52,    52,    53,    54,    54,    55,    56,    57,    57,
      57,    58,    59,    60,    61,    62,    62,    62,    62,    62,
      63,    64,    65,    65,    66,    67,    68,    69,    69,    70,
      70,    71,    72,    72,    73,    73,    74,    75,    75,    76,
      77,    77,    77,    77,    77,    77,    77,    77,    78,    78,
      78,    78,    78,    78,    78,    78,    79,    79,    79,    79,
      79,    79
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN. */
static const short yyr2[] =
{
       0,     0,     2,     2,     2,     1,     1,     1,     2,     2,
       3,     1,     1,     1,     2,     3,     0,     0,     0,     0,
       7,     1,     1,     1,     1,     1,     4,     6,     1,     3,
       5,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       1,     5,     3,     4,     3,     3,     5,     0,     2,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     3,     4,
       3,     3,     2,     2,     3,     3,     3,     1,     3,     3,
       3,     3,     3,     3,     1,     1,     3,     3,     3,     3,
       2,     1
};

/* YYDEFACT[S] -- default rule to reduce with in state S when YYTABLE
   doesn't specify something else to do.  Zero means the default is an
   error. */
static const short yydefact[] =
{
       1,    16,     0,    13,     0,     4,     7,     3,     2,     0,
      12,    11,     0,     5,     6,     0,     0,     8,    14,     0,
       0,     9,    23,    17,    21,    22,    24,    25,     0,     0,
       0,    47,    15,     0,     0,    51,    53,    52,     0,    54,
      55,     0,     0,     0,    36,    67,    35,    49,    50,    75,
      37,    38,    74,    81,     0,    10,     0,     0,    47,    42,
       0,    44,    57,     0,    40,     0,    80,    63,    62,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    16,     0,     0,    28,    34,
      18,    16,    43,    48,    56,     0,     0,    39,    66,    60,
      61,    64,    65,    68,    69,    70,    71,    72,    73,    79,
      78,    76,    77,    46,    41,    26,     0,     0,     0,    58,
      59,     0,    29,    32,    19,    27,     0,    20,    30,    33,
       0,     0
};

static const short yydefgoto[] =
{
       1,     7,    93,     9,    10,    11,    12,    57,   118,   127,
      44,    24,    25,    26,    27,    86,    87,   122,   128,    88,
      45,    62,    13,    14,    15,    30,    16,    61,    46,    47,
      48,    49,    50,    63,    51,    52,    53,    64
};

static const short yypact[] =
{
  -32768,    11,   -25,     6,   -22,-32768,-32768,-32768,-32768,    -1,
  -32768,-32768,    24,-32768,-32768,    -8,     1,-32768,    12,    79,
      10,-32768,    16,-32768,-32768,-32768,-32768,-32768,    40,    42,
      34,-32768,-32768,    79,    43,-32768,-32768,-32768,    79,-32768,
  -32768,    36,    36,    79,-32768,-32768,-32768,-32768,-32768,-32768,
  -32768,-32768,    94,   119,   -13,-32768,    36,    49,-32768,-32768,
      47,    14,-32768,     2,    41,    36,-32768,    22,    22,    53,
      36,    36,    36,    36,    36,    87,    87,    87,    87,    87,
      87,    79,    79,    79,    79,    73,    31,    52,    60,    94,
  -32768,    91,-32768,-32768,-32768,    79,    17,-32768,    97,    22,
      22,    97,    97,   125,   125,   125,   125,   125,   125,    86,
      54,   131,   131,-32768,-32768,-32768,    36,    36,    79,-32768,
  -32768,    66,    65,-32768,-32768,-32768,    36,-32768,-32768,-32768,
     109,-32768
};

static const short yypgoto[] =
{
  -32768,-32768,   112,-32768,-32768,    25,-32768,-32768,-32768,-32768,
     110,-32768,-32768,-32768,-32768,     5,-32768,-32768,-32768,  -113,
  -32768,   -40,-32768,-32768,-32768,-32768,-32768,    93,-32768,-32768,
  -32768,-32768,-32768,-32768,-32768,   -41,    68,   -17
};


#define	YYLAST		153


static const short yytable[] =
{
      67,    68,    54,    69,   123,    81,    82,    94,    83,    84,
      20,   130,    17,   129,    18,    89,    19,   -16,    28,    29,
      32,    66,     2,    70,    96,     2,    85,    22,    70,    98,
      99,   100,   101,   102,     3,     4,    21,     3,    31,    22,
      33,     5,    95,    34,    35,    36,    37,    55,     6,    71,
      72,     6,    73,    74,    56,   119,   120,    73,    74,    81,
      82,    60,    83,    84,   109,   110,   111,   112,    41,    42,
     115,   116,    81,-32768,    43,    89,    89,    58,   124,    59,
      90,    65,    22,    33,    92,    89,    34,    35,    36,    37,
      22,    33,    97,   117,    34,    35,    36,    37,   113,    38,
      70,   -31,     2,    70,-32768,   125,   126,    39,    40,   131,
     114,    41,    42,     8,     3,    39,    40,    43,   -45,    41,
      42,   121,    23,     0,     0,    43,    71,    72,     6,    73,
      74,    75,    76,    77,    78,    79,    80,-32768,-32768,-32768,
  -32768,-32768,-32768,   103,   104,   105,   106,   107,   108,    81,
      82,    91,-32768,-32768
};

static const short yycheck[] =
{
      41,    42,    19,    43,   117,    18,    19,     5,    21,    22,
      11,     0,    37,   126,     8,    56,    38,     3,    26,    27,
       8,    38,    11,     6,    65,    11,    39,     3,     6,    70,
      71,    72,    73,    74,    23,    24,    37,    23,    37,     3,
       4,    30,    40,     7,     8,     9,    10,    37,    37,    32,
      33,    37,    35,    36,    38,    95,    39,    35,    36,    18,
      19,    27,    21,    22,    81,    82,    83,    84,    32,    33,
      39,    40,    18,    19,    38,   116,   117,    37,   118,    37,
      31,    38,     3,     4,    37,   126,     7,     8,     9,    10,
       3,     4,    39,    41,     7,     8,     9,    10,    25,    20,
       6,    41,    11,     6,    18,    39,    41,    28,    29,     0,
      85,    32,    33,     1,    23,    28,    29,    38,    27,    32,
      33,   116,    12,    -1,    -1,    38,    32,    33,    37,    35,
      36,    12,    13,    14,    15,    16,    17,    12,    13,    14,
      15,    16,    17,    75,    76,    77,    78,    79,    80,    18,
      19,    58,    21,    22
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison/bison.simple"

/* Skeleton output parser for bison,

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software
   Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser when
   the %semantic_parser declaration is not specified in the grammar.
   It was written by Richard Stallman by simplifying the hairy parser
   used when %semantic_parser is specified.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

#if ! defined (yyoverflow) || defined (YYERROR_VERBOSE)

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || defined (YYERROR_VERBOSE) */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYLTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
# if YYLSP_NEEDED
  YYLTYPE yyls;
# endif
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAX (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# if YYLSP_NEEDED
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE) + sizeof (YYLTYPE))	\
      + 2 * YYSTACK_GAP_MAX)
# else
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAX)
# endif

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAX;	\
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif


#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");			\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).

   When YYLLOC_DEFAULT is run, CURRENT is set the location of the
   first token.  By default, to implement support for ranges, extend
   its range to the last symbol.  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)       	\
   Current.last_line   = Rhs[N].last_line;	\
   Current.last_column = Rhs[N].last_column;
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#if YYPURE
# if YYLSP_NEEDED
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, &yylloc, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval, &yylloc)
#  endif
# else /* !YYLSP_NEEDED */
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval)
#  endif
# endif /* !YYLSP_NEEDED */
#else /* !YYPURE */
# define YYLEX			yylex ()
#endif /* !YYPURE */


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)
/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
#endif /* !YYDEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif

#ifdef YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif
#endif

#line 315 "/usr/share/bison/bison.simple"


/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
#  define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL
# else
#  define YYPARSE_PARAM_ARG YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
# endif
#else /* !YYPARSE_PARAM */
# define YYPARSE_PARAM_ARG
# define YYPARSE_PARAM_DECL
#endif /* !YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
# ifdef YYPARSE_PARAM
int yyparse (void *);
# else
int yyparse (void);
# endif
#endif

/* YY_DECL_VARIABLES -- depending whether we use a pure parser,
   variables are global, or local to YYPARSE.  */

#define YY_DECL_NON_LSP_VARIABLES			\
/* The lookahead symbol.  */				\
int yychar;						\
							\
/* The semantic value of the lookahead symbol. */	\
YYSTYPE yylval;						\
							\
/* Number of parse errors so far.  */			\
int yynerrs;

#if YYLSP_NEEDED
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES			\
						\
/* Location data for the lookahead symbol.  */	\
YYLTYPE yylloc;
#else
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES
#endif


/* If nonreentrant, generate the variables here. */

#if !YYPURE
YY_DECL_VARIABLES
#endif  /* !YYPURE */

int
yyparse (YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  /* If reentrant, generate the variables here. */
#if YYPURE
  YY_DECL_VARIABLES
#endif  /* !YYPURE */

  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yychar1 = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack. */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;

#if YYLSP_NEEDED
  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
#endif

#if YYLSP_NEEDED
# define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
# define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  YYSIZE_T yystacksize = YYINITDEPTH;


  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
#if YYLSP_NEEDED
  YYLTYPE yyloc;
#endif

  /* When reducing, the number of symbols on the RHS of the reduced
     rule. */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
#if YYLSP_NEEDED
  yylsp = yyls;
#endif
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  */
# if YYLSP_NEEDED
	YYLTYPE *yyls1 = yyls;
	/* This used to be a conditional around just the two extra args,
	   but that might be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
# else
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);
# endif
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
# if YYLSP_NEEDED
	YYSTACK_RELOCATE (yyls);
# endif
# undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
#if YYLSP_NEEDED
      yylsp = yyls + yysize - 1;
#endif

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yychar1 = YYTRANSLATE (yychar);

#if YYDEBUG
     /* We have to keep this `#if YYDEBUG', since we use variables
	which are defined only if `YYDEBUG' is set.  */
      if (yydebug)
	{
	  YYFPRINTF (stderr, "Next token is %d (%s",
		     yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise
	     meaning of a token, for further debugging info.  */
# ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
# endif
	  YYFPRINTF (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %d (%s), ",
	      yychar, yytname[yychar1]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to the semantic value of
     the lookahead token.  This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

#if YYLSP_NEEDED
  /* Similarly for the default location.  Let the user run additional
     commands if for instance locations are ranges.  */
  yyloc = yylsp[1-yylen];
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
#endif

#if YYDEBUG
  /* We have to keep this `#if YYDEBUG', since we use variables which
     are defined only if `YYDEBUG' is set.  */
  if (yydebug)
    {
      int yyi;

      YYFPRINTF (stderr, "Reducing via rule %d (line %d), ",
		 yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (yyi = yyprhs[yyn]; yyrhs[yyi] > 0; yyi++)
	YYFPRINTF (stderr, "%s ", yytname[yyrhs[yyi]]);
      YYFPRINTF (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif

  switch (yyn) {

case 1:
#line 339 "explang.ypp"
{
   yyval.c_str = NULL;
}
    break;
case 2:
#line 349 "explang.ypp"
{
  
//   printf( "%s", $2 ); 
   fprintf( gSpkExpOutput, "%s", yyvsp[0].c_str );
   delete yyvsp[0].c_str;
   yyval.c_str = NULL;
}
    break;
case 3:
#line 364 "explang.ypp"
{

//   printf( "%s", $2 );
   fprintf( gSpkExpOutput, "%s", yyvsp[0].c_str );
   delete yyvsp[0].c_str;
   yyval.c_str = NULL;
}
    break;
case 4:
#line 375 "explang.ypp"
{

  YYACCEPT;
}
    break;
case 5:
#line 395 "explang.ypp"
{
   
   yyval.c_str = yyvsp[0].c_str;
}
    break;
case 6:
#line 400 "explang.ypp"
{
   yyval.c_str = yyvsp[0].c_str;
}
    break;
case 7:
#line 418 "explang.ypp"
{ 
  gSpkExpLines++; 
  char buf[2];
  sprintf( buf, "\n" );
  yyval.c_str = (char*)strdup(buf);
}
    break;
case 8:
#line 431 "explang.ypp"
{
  gSpkExpLines++;
  
  char buf[ strlen( yyvsp[-1].c_str ) + strlen( "// " ) + strlen( STR_NEWLINE ) + 1 ];
  sprintf( buf, "// %s\n", yyvsp[-1].c_str );
  delete yyvsp[-1].c_str;
  yyval.c_str = (char*)strdup(buf);
}
    break;
case 9:
#line 440 "explang.ypp"
{
  gSpkExpLines++;
  char buf[ strlen( yyvsp[-1].c_str ) + strlen( STR_NEWLINE ) + 1 ];
  sprintf( buf, "%s\n", yyvsp[-1].c_str );
  delete yyvsp[-1].c_str;
  yyval.c_str = (char*)strdup(buf);
}
    break;
case 10:
#line 453 "explang.ypp"
{

  gSpkExpLines++;

  char buf[ strlen( yyvsp[-2].c_str ) + strlen( STR_TAB ) + strlen( "// " ) 
          + strlen( STR_NEWLINE ) + strlen( yyvsp[-1].c_str ) + 1 ];
  sprintf( buf, "%s\t// %s\n", yyvsp[-2].c_str, yyvsp[-1].c_str );
  delete yyvsp[-2].c_str;
  delete yyvsp[-1].c_str;
  yyval.c_str = (char*)strdup(buf);
}
    break;
case 11:
#line 474 "explang.ypp"
{
  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 12:
#line 478 "explang.ypp"
{

  char buf[ strlen( yyvsp[0].c_str ) + 1 ];
  sprintf( buf, "%s;", yyvsp[0].c_str );
  delete yyvsp[0].c_str;
  yyval.c_str = (char*)strdup(buf);
}
    break;
case 13:
#line 514 "explang.ypp"
{
   
   char *buf = "nonmem_exit( 1, 0 );";
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 14:
#line 520 "explang.ypp"
{

   char buf[ strlen( yyvsp[0].c_str ) + strlen("nonmem_exit( , 0 );") + 1 ];
   sprintf( buf, "nonmem_exit( %s, 0 );", yyvsp[0].c_str );
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 15:
#line 528 "explang.ypp"
{

   char buf[ strlen( yyvsp[-1].c_str ) + strlen( yyvsp[0].c_str ) + strlen("nonmem_exit( ,  );") + 1 ];
   sprintf( buf, "nonmem_exit( %s, %s );", yyvsp[-1].c_str, yyvsp[0].c_str );
   delete yyvsp[-1].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 16:
#line 544 "explang.ypp"
{inRHS=false;}
    break;
case 17:
#line 544 "explang.ypp"
{inRHS=false;}
    break;
case 18:
#line 544 "explang.ypp"
{inRHS=true;}
    break;
case 19:
#line 544 "explang.ypp"
{inRHS=false;}
    break;
case 20:
#line 544 "explang.ypp"
{
   
   char buf[ strlen(yyvsp[-5].c_str) + strlen(" = ") + strlen(yyvsp[-1].c_str) + strlen(";") + 1 ];
   sprintf( buf, "%s = %s;", yyvsp[-5].c_str, yyvsp[-1].c_str );
   delete yyvsp[-5].c_str;
   delete yyvsp[-1].c_str;
   yyval.c_str = (char*)strdup(buf);

}
    break;
case 21:
#line 564 "explang.ypp"
{ 

  yyval.c_str = yyvsp[0].c_str; 
}
    break;
case 22:
#line 569 "explang.ypp"
{

  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 23:
#line 575 "explang.ypp"
{ 
  if( gSpkExpSymbolTable->findi( yyvsp[0].c_str ) == gSpkExpSymbolTable->end() )
  {
     if( inRHS )
     {
        char buf[128];
        sprintf( buf, "%s is not defined in the symbol table.", yyvsp[0].c_str );
        nm_error( buf );
     }
     else
     {
        gSpkExpSymbolTable->insertUserVar( yyvsp[0].c_str );
     }
  }
  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 24:
#line 594 "explang.ypp"
{
  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 25:
#line 598 "explang.ypp"
{
  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 26:
#line 614 "explang.ypp"
{ 

   if( gSpkExpSymbolTable->findi( yyvsp[-3].c_str ) == gSpkExpSymbolTable->end() )
   {
      if( inRHS )
      {
         char buf[128];
         sprintf( buf, "%s is not defined in the symbol table.", yyvsp[-3].c_str );
         nm_error( buf );
      }
      else
      {
         gSpkExpSymbolTable->insertUserVar( yyvsp[-3].c_str );
      }
   }

   //
   // Assumption: $3 is an arithmatic expression or a mere integer for now.
   //
   char buf[ strlen( yyvsp[-3].c_str ) + strlen( "[ (  ) - 1 ]" ) + 1 ];
   sprintf( buf, "%s[ ( %s ) - 1 ]", yyvsp[-3].c_str, yyvsp[-1].c_str );
   delete yyvsp[-3].c_str;
   delete yyvsp[-1].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 27:
#line 650 "explang.ypp"
{
 
   // NOT SUPPORTED YET!
   yyval.c_str = (char*)strdup("not supported");
}
    break;
case 28:
#line 666 "explang.ypp"
{ 

   yyval.c_str = yyvsp[0].c_str;
}
    break;
case 29:
#line 671 "explang.ypp"
{ 

   // NOT SUPPORTED YET!
   yyval.c_str = (char*)strdup("not supported");
}
    break;
case 30:
#line 677 "explang.ypp"
{

   // NOT SUPPORTED YET!
   yyval.c_str = (char*)strdup("not supported");
}
    break;
case 31:
#line 692 "explang.ypp"
{
 
  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 32:
#line 706 "explang.ypp"
{

  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 33:
#line 720 "explang.ypp"
{

  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 34:
#line 734 "explang.ypp"
{

  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 35:
#line 757 "explang.ypp"
{ 

   yyval.c_str = yyvsp[0].c_str; 
}
    break;
case 36:
#line 762 "explang.ypp"
{ 

   yyval.c_str = yyvsp[0].c_str; 
}
    break;
case 37:
#line 767 "explang.ypp"
{ 

   yyval.c_str = (char*)strdup("not supported"); 
}
    break;
case 38:
#line 772 "explang.ypp"
{
 
   yyval.c_str = yyvsp[0].c_str; 
}
    break;
case 39:
#line 777 "explang.ypp"
{ 

   char buf[ strlen( yyvsp[-1].c_str ) + strlen( "(  )" ) + 1 ];
   sprintf( buf, "( %s )", yyvsp[-1].c_str );
   delete yyvsp[-1].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 40:
#line 794 "explang.ypp"
{ 
  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 41:
#line 806 "explang.ypp"
{ 

   //
   // A logical_expr can be an arithmatic_expr by syntactic rule
   // but here we expect only an expression that evaluates to true or false.
   // FORTRAN does not treat a numeric value (like 0 or 1) as a logical value.
   // So, we kicks out arithmatic expressions here.
   // 
   char buf[ strlen( "if(  ){}" ) + strlen( STR_NEWLINE ) * 3 + strlen( yyvsp[-2].c_str ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "if( %s )\n{\n%s\n}\n", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);

}
    break;
case 42:
#line 829 "explang.ypp"
{ 
  char buf[ strlen( yyvsp[-2].c_str ) + strlen( STR_NEWLINE ) + 1 ];
  sprintf( buf, "%s\n", yyvsp[-2].c_str );
  delete yyvsp[-2].c_str;
  yyval.c_str = (char*)strdup(buf); 
}
    break;
case 43:
#line 836 "explang.ypp"
{

  char buf[ strlen( yyvsp[-3].c_str ) + strlen( yyvsp[-2].c_str ) + strlen( STR_NEWLINE) * 2 + 1 ];
  sprintf( buf, "%s%s\n", yyvsp[-3].c_str, yyvsp[-2].c_str );
  delete yyvsp[-3].c_str;
  delete yyvsp[-2].c_str;
  yyval.c_str = (char*)strdup(buf);
}
    break;
case 44:
#line 847 "explang.ypp"
{

  char buf[ strlen( yyvsp[-2].c_str ) + strlen( "{}" ) + strlen( STR_NEWLINE ) * 3 + strlen( yyvsp[0].c_str ) + 1 ];
  sprintf( buf, "%s\n{\n%s}\n", yyvsp[-2].c_str, yyvsp[0].c_str );
  delete yyvsp[-2].c_str;
  delete yyvsp[0].c_str;

  yyval.c_str = (char*)strdup(buf);
}
    break;
case 45:
#line 858 "explang.ypp"
{

  char buf[ strlen( "else" ) + strlen( STR_NEWLINE ) * 3 + strlen( "{}" ) + strlen( yyvsp[0].c_str ) + 1 ];
  sprintf( buf, "else\n{\n%s}\n", yyvsp[0].c_str );
  delete yyvsp[0].c_str;
  yyval.c_str = (char*)strdup(buf);
}
    break;
case 46:
#line 876 "explang.ypp"
{ 
  //
  // A logical_expr can be an arithmatic_expr by syntactic rule
  // but here we expect only an expression that evaluates to true or false.
  // FORTRAN does not treat a numeric value (like 0 or 1) as a logical value.
  // So, we kicks out arithmatic expressions here.
  // 
  char buf[ strlen( "if(  )" ) + strlen( yyvsp[-2].c_str ) + 1 ];
  sprintf( buf, "if( %s )", yyvsp[-2].c_str );
  delete yyvsp[-2].c_str;
  yyval.c_str = (char*)strdup(buf);
}
    break;
case 47:
#line 899 "explang.ypp"
{ 
   yyval.c_str = NULL;
}
    break;
case 48:
#line 903 "explang.ypp"
{

   if( yyvsp[-1].c_str == NULL )
   {
      char buf[ strlen( yyvsp[0].c_str ) + strlen( STR_NEWLINE ) + 1 ];
      sprintf( buf, "%s", yyvsp[0].c_str );
      delete yyvsp[0].c_str; 
      yyval.c_str = (char*)strdup(buf);
   }	
   else
   {
      char buf[ strlen( yyvsp[-1].c_str ) + strlen( yyvsp[0].c_str ) + strlen( STR_NEWLINE ) + 1 ];
      sprintf( buf, "%s%s", yyvsp[-1].c_str, yyvsp[0].c_str );
      delete yyvsp[-1].c_str;
      delete yyvsp[0].c_str; 
      yyval.c_str = (char*)strdup(buf);
   }
}
    break;
case 49:
#line 931 "explang.ypp"
{ 
  yyval.c_str = yyvsp[0].c_str; 
}
    break;
case 50:
#line 935 "explang.ypp"
{ 
  yyval.c_str = yyvsp[0].c_str; 
}
    break;
case 51:
#line 948 "explang.ypp"
{ 

  //
  // This is an integer constant.
  //
  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 52:
#line 965 "explang.ypp"
{ 
  //
  // This is a real constant in the form of "0.0".
  //
  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 53:
#line 972 "explang.ypp"
{ 
  // 
  // This is a real constant in the form of "0.0E0"
  //
  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 54:
#line 989 "explang.ypp"
{ 
  //
  // This is a boolean value "true".
  //  
  yyval.c_str = (char*)strdup("true");
}
    break;
case 55:
#line 996 "explang.ypp"
{
  //
  // This is a boolean value "false".
  //
  yyval.c_str = (char*)strdup("false");
}
    break;
case 56:
#line 1013 "explang.ypp"
{
  
  // NOT SUPPORTED YET!
  yyval.c_str = (char*)strdup("not supported");  
}
    break;
case 57:
#line 1029 "explang.ypp"
{

   // NOT SUPPORTED YET!
   yyval.c_str = (char*)strdup("not supported");
}
    break;
case 58:
#line 1035 "explang.ypp"
{

   // NOT SUPPORTED YET!
   yyval.c_str = (char*)strdup("not supported");
}
    break;
case 59:
#line 1052 "explang.ypp"
{ 

   //
   // allocate for the longest string.
   //
   int n = strlen( yyvsp[-3].c_str ); 
   char buf[ n + strlen( "(  )" ) + 1 ];
   char lowercase[ n + 1 ];
   strcpy( lowercase, yyvsp[-3].c_str );
   for( int i=0; i<n; i++ )
      lowercase[ i ] = tolower( lowercase[i] );

   if( strcmp( lowercase, STR_LOG ) == 0 )
       sprintf( buf, "log( %s )",   yyvsp[-1].c_str );
   else if( strcmp( lowercase, STR_LOG10 ) == 0 )
       sprintf( buf, "log10( %s )", yyvsp[-1].c_str );
   else if( strcmp( lowercase, STR_EXP ) == 0 )
       sprintf( buf, "exp( %s )",   yyvsp[-1].c_str );
   else if( strcmp( lowercase, STR_SQRT ) == 0 )
       sprintf( buf, "sqrt( %s )",  yyvsp[-1].c_str );
   else
   {
      char err[128];
      sprintf( err, "%s is not a supported operation!\n", lowercase );
      nm_error( err );
   }
   delete yyvsp[-1].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 60:
#line 1098 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " + " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s + %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 61:
#line 1107 "explang.ypp"
{ 

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " - " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s - %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 62:
#line 1116 "explang.ypp"
{

   char buf[ strlen( "-" ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "-%s", yyvsp[0].c_str );
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 63:
#line 1124 "explang.ypp"
{

   char buf[ strlen( "+" ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "+%s", yyvsp[0].c_str );
   delete yyvsp[0].c_str;

   yyval.c_str = (char*)strdup(buf);
}
    break;
case 64:
#line 1133 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " * " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s * %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 65:
#line 1142 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " / " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s / %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 66:
#line 1151 "explang.ypp"
{

   char buf[ strlen( "pow( ,  )" ) + strlen( yyvsp[-2].c_str ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "pow( %s, %s )", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 67:
#line 1160 "explang.ypp"
{
   yyval.c_str = yyvsp[0].c_str; 
}
    break;
case 68:
#line 1180 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " == " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s == %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 69:
#line 1189 "explang.ypp"
{ 

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " != " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s != %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 70:
#line 1198 "explang.ypp"
{ 

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " < " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s < %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 71:
#line 1207 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " <= " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s <= %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 72:
#line 1216 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " > " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s > %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 73:
#line 1225 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " >= " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s >= %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 74:
#line 1234 "explang.ypp"
{
  yyval.c_str = yyvsp[0].c_str;
}
    break;
case 75:
#line 1238 "explang.ypp"
{ 
  yyval.c_str = yyvsp[0].c_str; 
}
    break;
case 76:
#line 1260 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " == " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s == %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 77:
#line 1269 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " != " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s != %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 78:
#line 1278 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " || " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s || %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 79:
#line 1287 "explang.ypp"
{

   char buf[ strlen( yyvsp[-2].c_str ) + strlen( " && " ) + strlen( yyvsp[0].c_str ) + 1 ];
   sprintf( buf, "%s && %s", yyvsp[-2].c_str, yyvsp[0].c_str );
   delete yyvsp[-2].c_str;
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 80:
#line 1296 "explang.ypp"
{
   
   char buf[ strlen( yyvsp[0].c_str ) + strlen( " !(  ) " ) + 1 ];
   sprintf( buf, "!( %s )", yyvsp[-1].ch );
   delete yyvsp[0].c_str;
   yyval.c_str = (char*)strdup(buf);
}
    break;
case 81:
#line 1304 "explang.ypp"
{
   yyval.c_str = yyvsp[0].c_str;
}
    break;
}

#line 705 "/usr/share/bison/bison.simple"


  yyvsp -= yylen;
  yyssp -= yylen;
#if YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;
#if YYLSP_NEEDED
  *++yylsp = yyloc;
#endif

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("parse error, unexpected ") + 1;
	  yysize += yystrlen (yytname[YYTRANSLATE (yychar)]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "parse error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[YYTRANSLATE (yychar)]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exhausted");
	}
      else
#endif /* defined (YYERROR_VERBOSE) */
	yyerror ("parse error");
    }
  goto yyerrlab1;


/*--------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action |
`--------------------------------------------------*/
yyerrlab1:
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;
      YYDPRINTF ((stderr, "Discarding token %d (%s).\n",
		  yychar, yytname[yychar1]));
      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;


/*-------------------------------------------------------------------.
| yyerrdefault -- current state does not do anything special for the |
| error token.                                                       |
`-------------------------------------------------------------------*/
yyerrdefault:
#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */

  /* If its default is to accept any token, ok.  Otherwise pop it.  */
  yyn = yydefact[yystate];
  if (yyn)
    goto yydefault;
#endif


/*---------------------------------------------------------------.
| yyerrpop -- pop the current state because it cannot handle the |
| error token                                                    |
`---------------------------------------------------------------*/
yyerrpop:
  if (yyssp == yyss)
    YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#if YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "Error: state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

/*--------------.
| yyerrhandle.  |
`--------------*/
yyerrhandle:
  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

/*---------------------------------------------.
| yyoverflowab -- parser overflow comes here.  |
`---------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}
#line 1308 "explang.ypp"

