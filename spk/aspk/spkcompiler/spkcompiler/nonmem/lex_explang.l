%{
/**
 * @file lex_explang.c
 * This file is generated from the FLEX lexical analyzer 
 * specification file, nonmem/lex_explang.l, which defines the nm_lex() for NONMEM.
 * @ingroup nonmem
 */
/**
 * @example lex_explangTest.cpp
 */
#include <ctype.h>
#include <string.h>
#include "explang.h"

#define YYSTYPE yystype

extern void nm_error(char*);

// A global counter for keeping track of # of lines read so far.
// It is defined in the BISON parser specification file (explang.ypp).
//
extern int gSpkExpLines;

//
// A global counter for counting errors.
// It is defined in the BISON parser specification file (explang.ypp).
//
extern int gSpkExpErrors;

%}
%option noyywrap

tok_letter                  [ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]
tok_digit                   [0123456789]
tok_underscore               _
tok_character               ({tok_alphanumeric_character} |{tok_special_character})
tok_alphanumeric_character  ({tok_letter}|{tok_digit}|{tok_underscore})
tok_special_character       [=,()+-/*]
tok_other_character         [^{tok_character}]
tok_name                    {tok_letter}{tok_alphanumeric_character}*
tok_named_constant          {tok_name}

tok_sign                    [+-]
tok_ws                      [ \t]
tok_digit_string            {tok_digit}+
tok_signed_digit_string     {tok_sign}?{tok_digit_string}

tok_exponent_letter         [EDed]
tok_exponent                {tok_signed_digit_string}
tok_significand             ({tok_digit_string}?[.]{tok_digit_string}|{tok_digit_string}[.])
tok_engineering_notation    ({tok_significand}{tok_exponent_letter}{tok_exponent}|{tok_digit_string}{tok_exponent_letter}{tok_exponent})

tok_power_op                [*]{2}
tok_eq_op                   ([=]{2}|".EQ.")
tok_ne_op                   ([/][=]|".NE.")
tok_le_op                   ([<][=]|".LE.")
tok_ge_op                   ([>][=]|".GE.")
tok_lt_op                   ([<]|".LT.")
tok_gt_op                   ([>]|".GT.")
tok_rel_op                  ({tok_eq_op}|{tok_ne_op}|{tok_lt_op}|{tok_gt_op}|{tok_le_op}|{tok_ge_op})
tok_mult_op                 [*/]
tok_add_op                  [+-]
tok_not_op                  ".NOT."
tok_and_op                  ".AND."
tok_or_op                   ".OR."
tok_xor_op                  ".NEQV."
tok_not_xor_op              ".EQV."
tok_defined_binary_op       ({tok_and_op}|{tok_or_op}|{tok_rel_op}|{tok_mult_op}|{tok_add_op}|tok_linear_op})
tok_intrinsic_operator      ({tok_power_op}|{tok_rel_op}|{tok_mult_op}|{tok_add_op}|{tok_not_op}|{tok_and_op}|{tok_or_op})

tok_true                    ".TRUE."
tok_false                   ".FALSE."

tok_extended_intrinsic_op   {tok_intrinsic_operator}
tok_defined_operator        ({tok_defined_unary_op}|{tok_defined_binary_op}|{tok_extended_intrinsic_op})
tok_unary_function          ("ABS"|"ACOS"|"ASIN"|"ATAN"|"ATAN2"|"COS"|"COSH"|"EXP"|"LOG"|"LOG10"|"MAX"|"MIN"|"MOD"|"SIN"|"SINH"|"SQRT"|"TAN"|"TANH"|"LININTERP")

tok_open_array_elem_list    "(/"
tok_close_array_elem_list   "/)"

tok_exit                    "EXIT"
tok_if                      "IF"
tok_then                    "THEN"
tok_else                    "ELSE"
tok_endif                   "ENDIF"

%%
{tok_ws}                    { /* white space */ }
[;].*                       { nm_lval.c_str = (char*)strdup(nm_text+1);  return COMMENT; }
{tok_engineering_notation}  { 
                              nm_lval.c_str = (char*)strdup(nm_text);
                              int i;
                              for( i=0; i<nm_leng; i++ )
                                 nm_lval.c_str[i] = tolower(nm_lval.c_str[i]);  
                              return ENG_NOTATION; 
                            }
{tok_significand}           { nm_lval.c_str = (char*)strdup(nm_text);  return SIGNIFICAND; }
{tok_exit}                  { return EXIT;; }
{tok_if}                    { return IF; }
{tok_else}                  { return ELSE; }
{tok_then}                  { return THEN; }
{tok_endif}                 { return ENDIF; }
{tok_open_array_elem_list}  { return OPEN_ARRAY_ELEM_LIST; }
{tok_close_array_elem_list} { return CLOSE_ARRAY_ELEM_LIST; }
{tok_power_op}              { return POWER_OP; }
{tok_digit_string}          { nm_lval.c_str = (char*)strdup(nm_text);  return DIGIT_STRING; }
{tok_unary_function}        { nm_lval.c_str = (char*)strdup(nm_text);  return DEFINED_UNARY_FUNCTION; }
{tok_true}                  { return TRUE; }
{tok_false}                 { return FALSE; }
{tok_eq_op}                 { return EQ_OP; }
{tok_ne_op}                 { return NE_OP; }
{tok_lt_op}                 { return LT_OP; }
{tok_le_op}                 { return LE_OP; }
{tok_gt_op}                 { return GT_OP; }
{tok_ge_op}                 { return GE_OP; }
{tok_and_op}                { return AND_OP; } 
{tok_or_op}                 { return OR_OP; }
{tok_not_op}                { return NOT_OP; }
{tok_xor_op}                { return NEQV_OP; }
{tok_not_xor_op}            { return EQV_OP; }
{tok_named_constant}        { if( nm_leng > 6 ) 
                              {  
                                 char mess[128];
                                 strcpy( mess, "The length of a variable name, " );
                                 strcat( mess, nm_text );
                                 strcat( mess, ", exceeds 6!"  );
                                 nm_error( (char*)strdup( mess ) );
                                 nm_terminate(); 
                              }
                              nm_lval.c_str = (char*)strdup(nm_text); 
                              return NAME; 
                            }
{tok_special_character}     { nm_lval.ch = nm_text[0];  return nm_text[0]; }
<<EOF>>                     { return EOF_MARKER; }
\n                          { return '\n'; }
.                           { 
                              char buf[128];
                              sprintf( buf, "Invalid charactor '%s'.", nm_text );
                              nm_error( (char*)strdup( buf ) ); 
                              nm_terminate();  
                            }
%%
