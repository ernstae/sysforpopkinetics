/**
 * @file explang.y
 * @brief This is a syntax analyzer specification file (BISON) for NMTRAN->C++.
*/
%{
//
// int yyparse( void )
//
//  Tree-generator for NONMEM abbriviated code
//
// The source language allows only the following statements:
// * assignment
// * if conditional statement in the form of either if-assign or if-then-else
//
// The functions and operators it supports are:
// * sqrt, log, log10, exp
// * + (addition), - (subtraction), * (multiplication), / (division), ** (power)
//
// The primitive data types it supports are:
// * real (eq. float)
// * double real (eq. double)
// * integer (eq. int) for indexing arrays
// * logical (eq. bool)
//
// The types of data containers allowed are:
// * scalar
// * one dimensional array
// * two dimensional array
//
// The methods to access array elements of a dimensional space are:
// * by constant scalar indexing
// * by providing constant start and end indeces
// * by providing constant start and end indeces and a stride
// (thus, for a vector, you have a(start:end:stride) and for a matrix, A(start:end:stride, start:end:stride).
//
//
#include <iostream>
#include <cmath>
#include <exception>
#include <string>

// DOM related (DOM framework is used to create a forest of expression trees)
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

// Our stuff
#include "../libcommon/SymbolTable.h"
#include "../libcommon/ExpTreeGenerator.h"
#include "../libcommon/ExpNodeCarrier.h"
#include "../libcommon/SpkCompilerUtil.h"

using namespace xercesc;
using namespace std;

/**
 * Global counter for errors that encountered during parsing.
 */
int gSpkExpErrors = 0;

/**
 * Global counter for lines that have been read so far during parsing.
 */
int gSpkExpLines  = 0;

/**
 * Global pointer to a DOMDocument object.
 *
 * This pointer has to be initialized to point to a valid object
 * in the caller (of yyparse()) space.  The allocated resource
 * must be released by the caller too.
 *
 * @note If the pointer is found to be pointing to NULL at
 * the very beginning of parsing, yyparse() will call yyerror()
 * and probably terminate the program execution completely.
 */

DOMDocument * gSpkExpTree;

/**
 * Global pointer to a SymbolTable object.
 *
 * This pointer has to be initialized to point to a valid object
 * in the caller (of yyparse()) space.  The allocated resource
 * must be released by the caller too.
 *
 * @note If the pointer is found to be pointing to NULL at
 * the very beginning of parsing, yyparse() will call
 * yyerror() and probably terminate the program execution completely.
 */
SymbolTable * gSpkExpSymbolTable;

extern "C"{
  int yyparse(void);
  int yylex(void);  
  int yywrap()
  {
    return 1;
  }
  int yyerror( char * s )
  { 
    ++gSpkExpErrors;
    cerr <<  "!!! Error (" << gSpkExpErrors << ") at line " << gSpkExpLines << ": " << endl;
    cerr << "\t" << s << endl;
    
    return -1;
  }
};

  static const char * const STR_TYPE     = "type";
  static const char * const STR_VALUE    = "value";
  static const char * const STR_NAME     = "name";
  static const char * const STR_OPERATOR = "operator";
  static const char * const STR_OPERAND  = "operand";
  static const char * const STR_SIGN     = "sign";
  static const char * const STR_ROWS     = "rows";
  static const char * const STR_COLS     = "cols";
  static const char * const STR_LHS      = "lhs";
  static const char * const STR_RHS      = "rhs";
  static const char * const STR_BINARY   = "binary";
  static const char * const STR_UNARY    = "unary";
  static const char * const STR_FUNCTION = "function";
  static const char * const STR_ARGC     = "argc";
  static const char * const STR_STRUCTURE= "structure";
  static const char * const STR_INDEX    = "index";
  static const char * const STR_START    = "start";
  static const char * const STR_END      = "end";
  static const char * const STR_STRIDE   = "stride";
  static const char * const STR_CONSTANT = "constant";
  static const char * const STR_TRUE     = "true";
  static const char * const STR_FALSE    = "false";
  static const char * const STR_COMMENT  = "comment";
  static const char * const STR_EXIT     = "exit";

  static const char * const STR_ASSIGN   = "assign";
  static const char * const STR_ARRAY_ELEMENT_LIST = "array_element_list";
  static const char * const STR_VARIABLE = "variable";
  static const char * const STR_PRIORITIZED = "prioritized";
  static const char * const STR_IF       = "if";
  static const char * const STR_ELSE     = "else";
  static const char * const STR_THEN     = "then";
  static const char * const STR_CONDITION= "condition";
  static const char * const STR_BLOCK    = "block";

  static const char * const STR_SQRT     = "sqrt";
  static const char * const STR_EXP      = "exp";
  static const char * const STR_LOG      = "log";
  static const char * const STR_LOG10    = "log10";
  static const char * const STR_ADD      = "add";
  static const char * const STR_SUBTRACT = "subtract";
  static const char * const STR_MULTIPLY = "multiply";
  static const char * const STR_DIVIDE   = "divide";
  static const char * const STR_POWER    = "power";
  static const char * const STR_MINUS    = "minus";
  static const char * const STR_PLUS     = "plus";

  static const char * const STR_EQ       = "eq";
  static const char * const STR_NE       = "ne";
  static const char * const STR_LT       = "lt";  
  static const char * const STR_LE       = "le";
  static const char * const STR_GT       = "gt";
  static const char * const STR_GE       = "ge";
  static const char * const STR_NXOR     = "nxor";
  static const char * const STR_XOR      = "xor";
  static const char * const STR_OR       = "or";
  static const char * const STR_AND      = "and";
  static const char * const STR_NEGATE   = "negate";

  static bool inRHS = false;
  static bool inConditional = false;

  static int DEFAULT_BUFF_SIZE = 128;

  ExpTreeGenerator expTreeUtils;
%}

%union{
  struct ExpNodeCarrier* node;
  char   ch;
  char*  str;
}

//
// Char* type tokens / non-terminals
//
%token <str>  NAME 
%token <str>  OPEN_ARRAY_ELEM_LIST
%token <str>  CLOSE_ARRAY_ELEM_LIST
%token <str>  POWER_OP
%token <str>  DEFINED_UNARY_FUNCTION
%token <str>  DIGIT_STRING
%token <str>  ENG_NOTATION
%token <str>  SIGNIFICAND
%token <str>  COMMENT

//
// char type tokens / non-terminals
//
%token <ch>   EQ_OP
%token <ch>   NE_OP
%token <ch>   LT_OP
%token <ch>   LE_OP
%token <ch>   GT_OP
%token <ch>   GE_OP
%token <ch>   AND_OP
%token <ch>   OR_OP
%token <ch>   NOT_OP
%token <ch>   EQV_OP
%token <ch>   NEQV_OP
%token <ch>   EXIT
%token <ch>   IF
%token <ch>   THEN
%token <ch>   ELSE
%token <ch>   ENDIF
%token <ch>   TRUE FALSE

%type  <node> input
%type  <node> statement
%type  <node> if_stmt_or_block
%type  <node> if_construct
%type  <node> if_then_stmt
%type  <node> if_then_clause
%type  <node> else_clause
%type  <node> block
%type  <node> line
%type  <node> assignment_stmt
%type  <node> variable
%type  <node> whole_object
%type  <node> subobject
%type  <node> literal_constant
%type  <node> int_literal_constant 
%type  <node> real_literal_constant
%type  <node> logical_constant
%type  <node> int_expr 
%type  <node> start_subscript 
%type  <node> end_subscript 
%type  <node> stride
%type  <node> slice
%type  <node> vector_section_variable 
%type  <node> matrix_section_variable
%type  <node> relational_expr
%type  <node> logical_expr
%type  <node> if_stmt 
%type  <node> expr
%type  <node> arithmatic_expr
%type  <node> primary 
%type  <node> array_constructor 
%type  <node> array_element_list 
%type  <node> function_ref
%type  <node> exit_stmt

%right '='
%nonassoc EQV_OP NEQV_OP
%nonassoc OR_OP
%nonassoc AND_OP
%nonassoc NOT_OP
%nonassoc EQ_OP NE_OP LT_OP LE_OP GT_OP GE_OP
%left '+' '-'
%nonassoc SIGN
%left '*' '/'
%right POWER_OP

%start input

%%
//==================================================================================
//
// input
//
// This is the fundamental unit that this parser accepts.
// An input is the root of the entire tree and
// can have any number of *line*s (each of which is an assignment statement)
// or if_stmt_or_block (which is either an if-stmt or if-construct).
//
//==================================================================================*/
input: 
//
// A unit can be empty.
//
/* empty */ {

  if( gSpkExpTree == NULL )
    {
      yyerror( "gSpkExpTree is not allocated memory!" );
    }
  if( gSpkExpSymbolTable == NULL )
    {
      yyerror( "gSpkExpSymbolTable is not allocated memory!" );
    }
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = gSpkExpTree->getDocumentElement();
  $$ = carrier;
}
| 
//
// A unit can contain lines.
//
input line {
  struct ExpNodeCarrier * carrier;

  if( $1 == NULL )
    carrier = expTreeUtils.createExpNodeCarrier();
  else
    carrier = $1;

  carrier->node = gSpkExpTree->getDocumentElement();

  //
  // If the line just read is empty, do nothing.
  // If it is NON empty, then append the tree as a child of the tree given
  // back by the previous argument.
  // 
  if( $2 == NULL )
    {
      $$ = $1;
    }
  else //( $2 != NULL )
    {
      carrier->node->appendChild( $2->node );
    }

  $$ = carrier;
}
|
//
// A unit can contain if-constructs (if-then-else).
// 
input { inConditional=true; } if_stmt_or_block { inConditional=false; } {
  struct ExpNodeCarrier * carrier;

  if( $1 == NULL )
    carrier = expTreeUtils.createExpNodeCarrier();
  else
    carrier = $1;

  carrier->node = gSpkExpTree->getDocumentElement();

  //
  // If the line just read is empty, do nothing.
  // If it is NON empty, then append the tree as a child of the tree given
  // back by the previous argument.
  // 
  if( $3 == NULL )
    {
      $$ = $1;
    }
  else //( $3 != NULL )
    {
      carrier->node->appendChild( $3->node );
    }
 
  $$ = carrier;
}
;

//==================================================================================
//
// if_stmt_or_block
//
// Either an if statement or a set of if-then-else clauses.
// One might argue an if statement is a statement, therefore it should be
// part of "statement" non-terminal. It is true.  However, Bison, wich uses only 
// a single lookahead token, cannot distinguish if-stmt and if-construct since
// both begin with the IF token.  This is to trick Bison so that it won't 
// produce a shift/reduce conflict.
//
//==================================================================================
if_stmt_or_block : if_stmt {$$ = $1;}
| if_construct {$$ = $1;}
;

//==================================================================================
//
// line
//
// A sequence of (may be empty) strings terminated by a new line character ('\n').
// When a legal "line" is detected, the line count is incremented by one.
//
// A line is a tree when it contains more than just new line characters
// and may have a comment.  When a comment follows a statement
// the comment is appended as a leaf of the statement tree and
// the statement tree is returned as this production tree.  
// If the line contains only a comment, the comment is returned as this production
// tree.
//
// <comment value=""/>
//
// where the value enclosed by double-quotes could be empty or 
// has type of test containing
// any number of spaces, legal characters or/and numbers.
//
//==================================================================================
line : 

//
// An empty line.  Produces nothing (NULL) but incrementing the line count. 
//  
'\n'  { 
  gSpkExpLines++; 
  $$ = NULL;
} 
| 
//
// A comment only line.  Returns a comment leaf and increment the line count.
//
COMMENT '\n' {
  gSpkExpLines++;

  struct ExpNodeCarrier* carrier = expTreeUtils.createExpNodeCarrier();
  DOMElement * myComment = gSpkExpTree->createElement( X( STR_COMMENT ) );

  // a value can be empty.
  myComment->setAttribute( X( STR_VALUE ), X($1) );
    
  carrier->node = myComment;
  $$ = carrier;
}
|
statement '\n' {
  gSpkExpLines++;
  $$ = $1;
}
|
//
// A statement followed by a comment.  Returns the statement tree with
// the comment as a leaf in the tree.
// 
statement COMMENT '\n' {

  // Keep track of #of lines read so far.  This is a global variable.
  gSpkExpLines++;

  struct ExpNodeCarrier* carrier = $1;

  DOMElement * myComment = gSpkExpTree->createElement( X( STR_COMMENT ) );
  myComment->setAttribute( X( STR_VALUE ), X($2) );
  $1->node->appendChild( myComment );

  $$ = carrier;
}
;

//==================================================================================
//
// statement
//
// A statement may be an assignement_stmt, if_stmt or exit_stmt.
//
//==================================================================================
statement : 
assignment_stmt {
  $$ = $1;
}
/*
|
{ inConditional=true; } if_stmt { inConditional=false; }{ 
  $$ = $2;
}
*/
|
exit_stmt {
  $$ = $1;
}
;

//==================================================================================
//
// exit_stmt
//
// An exit_stmt has three forms; 1) the terminal EXIT only, 2) the terminal
// followed by a number and 3) the terminal followed by two numbers.
//
// 1) <exit/>
// 2) <exit pre_defined_code=""/>
// 3) <exit pre_defined_code=""  user_defined_code=""/>
//
//
// Attention!  The NONMEM abbriviated code's EXIT means something different from
// that of Fortran.
//
// NONMEM's EXIT statement shall cause an immediate exit from the routine,
// and, if PREDPP is being used, an immediate exit from PREDPP.  
// It is typically used in an IF statement to avoid computing further.
// (see p79, NONMEM User's Guide Part VIII for complete specification)
//
//==================================================================================
exit_stmt :
EXIT {
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();

  DOMElement * exit_stmt = gSpkExpTree->createElement( X( STR_EXIT ) );
  carrier->node = exit_stmt; 
  $$ = carrier;
}
|
EXIT DIGIT_STRING {
  //
  // The number specifies the PRED exit code; it must be 1 or 2.  The default is 1.
  //
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();

  DOMElement * exit_stmt = gSpkExpTree->createElement( X( STR_EXIT ) );
  char message[DEFAULT_BUFF_SIZE];
  sprintf( message, "User requested termination with predefined code, %d.\n", $2 );
  exit_stmt->setAttribute( X( STR_VALUE ), X( message ) );
  carrier->node = exit_stmt; 
  $$ = carrier;
}
|
EXIT DIGIT_STRING DIGIT_STRING {
  //
  // The first number is a PRED exit code (1 or 2) and
  // the second number is the user error code, which must be 0-999.  The default is 0.
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();

  DOMElement * exit_stmt = gSpkExpTree->createElement( X( STR_EXIT ) );
  char message[DEFAULT_BUFF_SIZE];
  sprintf( message, "User requested termination with predefined code, %d.\n", $2 );

  sprintf( message, "User requested termination with predefined code, %d.\n", $2 );
  exit_stmt->setAttribute( X( "pre_defined_code" ), X( message ) );
  sprintf( message, "User requested termination with user-defined code, %d.\n", $3 );
  exit_stmt->setAttribute( X( "user_defined_code" ), X( message ) );
  carrier->node = exit_stmt; 
  $$ = carrier;
}
;

//==================================================================================
//
// assignment_stmt
//
// An assignment is a tree topeed with a label "operator" whose value is "assign".
// The tree is a binary tree, where lhs is an instance of "variable" and rhs
// is an instance of "expr".  The type of lhs is inherited from the rhs.
//
// <operator operand="binary" value="assign" type=of-rhs>
//    <lhs>
//        variable
//    </lhs>
//    <rhs>
//        expr
//    </rhs>
//==================================================================================
assignment_stmt :
{inRHS=false;}variable '=' {inRHS=true;} expr {inRHS=false;}{

  DOMElement * assign = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  assign->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  assign->setAttribute( X( STR_VALUE ),   X( STR_ASSIGN ) );

  DOMElement *lhs = gSpkExpTree->createElement( X( STR_LHS ) );
  lhs->appendChild( $2->node );
  lhs->setAttribute( X(STR_TYPE), $2->node->getAttribute( X(STR_TYPE) ) );
  lhs->setAttribute( X(STR_STRUCTURE), $2->node->getAttribute( X(STR_STRUCTURE) ) );
  lhs->setAttribute( X(STR_ROWS), $2->node->getAttribute( X(STR_ROWS) ) );
  lhs->setAttribute( X(STR_COLS), $2->node->getAttribute( X(STR_COLS) ) );

  DOMElement *rhs = gSpkExpTree->createElement( X( STR_RHS ) ); 
  rhs->appendChild( $5->node );
  rhs->setAttribute( X(STR_TYPE), $5->node->getAttribute( X(STR_TYPE) ) );
  rhs->setAttribute( X(STR_STRUCTURE), $5->node->getAttribute( X(STR_STRUCTURE) ) );
  rhs->setAttribute( X(STR_SIGN), $5->node->getAttribute( X(STR_SIGN) ) );
  rhs->setAttribute( X(STR_ROWS), $5->node->getAttribute( X(STR_ROWS) ) );
  rhs->setAttribute( X(STR_COLS), $5->node->getAttribute( X(STR_COLS) ) );

  // The sign of lhs is the sign of the rhs.
  lhs->setAttribute( X(STR_SIGN), rhs->getAttribute( X(STR_SIGN) ) );

  // Obtain the data type of rhs.
  const char *rhs_datatype_str;
  rhs_datatype_str = C( dynamic_cast<DOMElement*>(rhs)->getAttribute( X( STR_TYPE ) ) );

  // Convert the string representation of data type to enum.
  Symbol *s;
  enum Symbol::SYMBOLTYPE rhs_datatype = Symbol::toEnum( rhs_datatype_str );

  // Obtain the data structure of rhs.
  const char * rhs_structure_str = NULL; 
  const char * rhs_tagname = NULL;
  int  lhs_rows = 1;
  int  lhs_cols = 1;
  int  rhs_rows = 1;
  int  rhs_cols = 1;

  // Obtain the right hand side structure type
  rhs_structure_str = C( dynamic_cast<DOMElement*>(rhs)->getAttribute( X( STR_STRUCTURE ) ) );

  if( rhs_structure_str == NULL || strcmp( rhs_structure_str, "") == 0 )
    {
      rhs_structure_str = Symbol::C_SCALAR;
    }

  // Convert the string representation of structure to enum.
  enum Symbol::SYMBOLTYPE rhs_structure = Symbol::toEnum( rhs_structure_str ); 
  
  rhs_rows = atoi( C( dynamic_cast<DOMElement*>(rhs)->getAttribute( X( STR_ROWS ) ) ) );
  rhs_cols = atoi( C( dynamic_cast<DOMElement*>(rhs)->getAttribute( X( STR_COLS ) ) ) );
  ////////////////////////////////////////////////////////////////////////////////
  // Beginning of symbol table maintenance block.
  //
  const char * lhs_datatype_str = Symbol::C_DOUBLE;
  const char * lhs_structure_str = Symbol::C_SCALAR;
  enum Symbol::SYMBOLTYPE lhs_datatype = Symbol::DOUBLE;
  enum Symbol::SYMBOLTYPE lhs_structure = Symbol::SCALAR;

  // Check if the variable name exists in the symbol table already.
  // If it does, update the entry.  If not, insert the variable name as new.
  const XMLCh* name = dynamic_cast<DOMElement*>(lhs->getFirstChild())->getAttribute( X(STR_NAME) );
  const char* name_cstr = C( name );

  if( ( s = gSpkExpSymbolTable->find( name_cstr ) ) != NULL ) // This is where the variable has been registed in the symbol table.
    {	 
      // Default to lhs = (what SymbolTable entry says)
      lhs_datatype  = s->dataType();
      lhs_structure = s->objectType();
      lhs_rows = s->dim().first;
      lhs_cols = s->dim().second;

      if( s->dataType() == Symbol::UNKNOWN )
	{
	  lhs_datatype     = rhs_datatype;
	}
      else if( s->dataType() == Symbol::DOUBLE )
	{
	}
      else if( s->dataType() == Symbol::INT && rhs_datatype == Symbol::DOUBLE )
        {
          lhs_datatype = rhs_datatype;
        }
      else if( s->dataType() == Symbol::BOOL )
	{
	  if( rhs_datatype == Symbol::DOUBLE || rhs_datatype == Symbol::INT )
	    {
	      lhs_datatype = rhs_datatype;
	    }
	}
	  
      if( s->objectType() == Symbol::UNKNOWN )
	{
	  lhs_structure     = rhs_structure;
       	  lhs_rows          = rhs_rows;
	  lhs_cols          = rhs_cols;
	 }
      else if( s->objectType() == Symbol::MATRIX )
	 {
	 }
      else if( s->objectType() == Symbol::VECTOR )
	 {
	   if( rhs_structure == Symbol::MATRIX )
	   {
	      lhs_structure     = rhs_structure;
	      lhs_rows          = rhs_rows;
	      lhs_cols          = rhs_cols;
	   }
         }
       else if( s->objectType() == Symbol::SCALAR )
	 {
	   if( rhs_structure == Symbol::MATRIX || rhs_structure == Symbol::VECTOR )
	     {
	       lhs_structure     = rhs_structure;
	       lhs_rows          = rhs_rows;
	       lhs_cols          = rhs_cols;
	     }
	 }
     }

   else // This is where the varible is discovered new.
     {
	s = gSpkExpSymbolTable->insert( name_cstr );
	lhs_datatype  = rhs_datatype;
	lhs_structure = rhs_structure;
	lhs_rows = rhs_rows;
	lhs_cols = lhs_cols;
    }


   assert( s != NULL );
   // Don't update the symbol table entry if this assignment statement appears in a conditional block!
   //   if( !inConditional )
     {
       s->dataType  ( lhs_datatype );
       s->objectType( lhs_structure );
       s->dim       ( lhs_rows, lhs_cols );
     }
  //
  // End of sybmol table maintenace.
  /////////////////////////////////////////////////////////////////////////////////
  lhs_datatype_str  = Symbol::toCString( lhs_datatype );
  lhs_structure_str = Symbol::toCString( lhs_structure );

  dynamic_cast<DOMElement*>(lhs)->setAttribute( X( STR_TYPE ),      X( lhs_datatype_str ) ); 

  dynamic_cast<DOMElement*>(lhs)->setAttribute( X( STR_STRUCTURE ), X( lhs_structure_str ) );

  char lhs_rows_str[DEFAULT_BUFF_SIZE];
  sprintf( lhs_rows_str, "%d", lhs_rows );
  dynamic_cast<DOMElement*>(lhs)->setAttribute( X( STR_ROWS ),      X( lhs_rows_str ) );

  char lhs_cols_str[DEFAULT_BUFF_SIZE];
  sprintf( lhs_cols_str, "%d", lhs_cols );
  dynamic_cast<DOMElement*>(lhs)->setAttribute( X( STR_COLS ),      X( lhs_cols_str ) );


  // Append lhs and rhs
  assign->appendChild( lhs );
  assign->appendChild( rhs );

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = assign;
  $$ = carrier;  
}
;

//==================================================================================
//
// variable
//
// A variable is a named object that may represents a whole object or
// subset of a whole object.  It returns a tree topped with a labele "var" and
// its type is DOUBLE by default unless the variable name is found in the
// symbol table.
//
// For whole objects:
//    <var name="" type="DOUBLE">
//
// For subobjects:
//    the subobject's tree is the root tree of this variable.
//
//==================================================================================
variable :
whole_object { 
  $$ = $1; 
} // this includes scalar objects and whole array (vector/matrix) objects
|
subobject { 
  $$ = $1;
 }
;
whole_object :
NAME { 
  DOMElement * var = gSpkExpTree->createElement( X( STR_VARIABLE ) );
  var->setAttribute( X( STR_NAME ), X( $1 ) );
  var->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );

  ///////////////////////////////////////////////////////////////
  // Symbol table lookup
  //
  int  rows;
  int  cols;
  enum Symbol::SYMBOLTYPE datatype;
  enum Symbol::SYMBOLTYPE structure;
  const char * datatype_str;
  const char * structure_str;
  char rows_str[DEFAULT_BUFF_SIZE];
  char cols_str[DEFAULT_BUFF_SIZE];
  
  Symbol * s = NULL;

  if( ( s = gSpkExpSymbolTable->find( $1 ) ) != NULL )
    {
      datatype  = s->dataType();
      structure = s->objectType();
      rows      = s->dim().first;
      cols      = s->dim().second;
    }
  else
    {
      if( inRHS )
	{
	  char buf[DEFAULT_BUFF_SIZE];
	  sprintf( buf, "A symbol <%s> must be defined before its use.\n", $1 );
	  yyerror( buf );
	  return -1;
	}
      else
	{
	  datatype  = Symbol::DOUBLE;
	  structure = Symbol::SCALAR;
	  rows = 1;
	  cols = 1;
	}
    }

  // End of symbol table lookup
  ///////////////////////////////////////////////////////////////

  // Convert enum to strings
  datatype_str  = Symbol::toCString( datatype );
  structure_str = Symbol::toCString( structure );

  // Convert integer to strings
  sprintf( rows_str, "%d", rows );
  sprintf( cols_str, "%d", cols );

  var->setAttribute( X( STR_TYPE ),      X( datatype_str ) );
  var->setAttribute( X( STR_STRUCTURE ), X( structure_str ) );

  var->setAttribute( X( STR_ROWS ),      X( rows_str ) );
  var->setAttribute( X( STR_COLS ),      X( cols_str ) );

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = var;
  $$ = carrier;
} // scalar, vector, matrix
;

subobject :
vector_section_variable { 
  $$ = $1;
} // x(i:j)
|
matrix_section_variable { 
  $$ = $1;
} // x(i:j, i:j)
;

//==================================================================================
//
// vector_section_variable
//
// A variable that is a subset of a vector.  This itself is a tree topped with
// a label "vector".  The tree contains a subtree describing how to select
// the subset elements.
//
// <vector name="" type="">
//
// The type attribute is undefined unless the information associated with
// the name is found in the symbol table.
// 
//==================================================================================
vector_section_variable : 
NAME '(' slice ')' { 

  DOMElement * vector = gSpkExpTree->createElement( X( Symbol::C_VECTOR) );
  vector->setAttribute( X( STR_NAME), X( $1 ) );

  Symbol * s = NULL;
  if( ( s = gSpkExpSymbolTable->find( $1 ) ) != NULL )
    {
      vector->setAttribute( X( STR_TYPE ), X( Symbol::toCString( s->dataType() ) ) );
    }
  else
    {
      vector->setAttribute( X( STR_TYPE ), X( Symbol::C_DOUBLE ) );
    }

  vector->setAttribute( X( STR_STRUCTURE) , $3->node->getAttribute( X( STR_STRUCTURE ) ) );
  vector->setAttribute( X( STR_SIGN ),      X( Symbol::C_UNKNOWN ) );
  vector->setAttribute( X( STR_ROWS ),      $3->node->getAttribute( X( STR_ROWS ) ) );
  vector->setAttribute( X( STR_COLS ),      $3->node->getAttribute( X( STR_COLS ) ) );

  vector->appendChild( $3->node );

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = vector;
  $$ = carrier;
}
;

//==================================================================================
//
// matrix_section_variable
//
// A variable that is a subset of a matrix.  This itself is a tree topped with
// a label "matrix".  The tree contains a subtree describing how to select
// the subset elements.
//
// <matrix name="" type="">
//
// The type attribute is undefined unless the information associated with
// the name is found in the symbol table.
// 
//==================================================================================
matrix_section_variable :
NAME '(' slice ',' slice ')' { 

  DOMElement * matrix = gSpkExpTree->createElement( X( Symbol::C_MATRIX ) );
  matrix->setAttribute( X( STR_NAME ), X( $1 ) );
  // matrix->setAttribute( X( STR_TYPE ), X( Symbol::C_UNKNOWN ) );  
  // matrix->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_MATRIX ) );
  // matrix->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );

  Symbol * s = NULL;
  if( ( s = gSpkExpSymbolTable->find( $1 ) ) != NULL )
    {
      matrix->setAttribute( X( STR_TYPE ), X( Symbol::toCString( s->dataType() ) ) );
    }
  else
    {
      matrix->setAttribute( X( STR_TYPE ), X( Symbol::C_DOUBLE ) );
    }

  if( XMLString::equals( $3->node->getAttribute( X( STR_STRUCTURE ) ), X( Symbol::C_SCALAR ) ) )
    {
      if( XMLString::equals( $5->node->getAttribute( X( STR_STRUCTURE ) ), X( Symbol::C_SCALAR ) ) )
	{
	  matrix->setAttribute( X( STR_STRUCTURE), X( Symbol::C_SCALAR ) );
	  matrix->setAttribute( X( STR_ROWS ),     X( "1" ) );
	  matrix->setAttribute( X( STR_COLS ),     X( "1" ) );
	}
      else if(  XMLString::equals( $5->node->getAttribute( X( STR_STRUCTURE ) ), X( Symbol::C_VECTOR ) ) )
	{
	  // The 1st is a scalar and the 2nd slice is a vector, 
	  //which means the column selection is vectorized.
	  matrix->setAttribute( X( STR_STRUCTURE), X( Symbol::C_VECTOR ) );
	  matrix->setAttribute( X( STR_ROWS ),     X( "1" ) );
	  matrix->setAttribute( X( STR_COLS ),     $5->node->getAttribute( X( STR_ROWS ) ) );
	}
      else
	{
	  yyerror( "The return type of \"slice\" should be either scalar or vector" );
	}
    }
  else if( XMLString::equals( $3->node->getAttribute( X( STR_STRUCTURE ) ), X( Symbol::C_VECTOR ) ) )
    {
      if( XMLString::equals( $5->node->getAttribute( X( STR_STRUCTURE ) ), X( Symbol::C_SCALAR ) ) )
	{
	  matrix->setAttribute( X( STR_STRUCTURE), X( Symbol::C_SCALAR ) );
	  matrix->setAttribute( X( STR_ROWS ),     $5->node->getAttribute( X( STR_ROWS ) ) );
	  matrix->setAttribute( X( STR_COLS ),     X( "1" ) );
	}
      else if(  XMLString::equals( $5->node->getAttribute( X( STR_STRUCTURE ) ), X( Symbol::C_VECTOR ) ) )
	{
	  matrix->setAttribute( X( STR_STRUCTURE), X( Symbol::C_VECTOR ) );
	  matrix->setAttribute( X( STR_ROWS ),     $5->node->getAttribute( X( STR_ROWS ) ) );
	  matrix->setAttribute( X( STR_COLS ),     $5->node->getAttribute( X( STR_ROWS ) ) );
	}
      else
	{
	  yyerror( "The return type of \"slice\" should be either scalar or vector" );
	}
    } 
  else
    {
      yyerror( "The return type of \"slice\" should be either scalar or vector" );
    }
  matrix->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );

  matrix->appendChild( $3->node );
  matrix->appendChild( $5->node );

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = matrix;
  $$ = carrier;
}
;

//==================================================================================
//
// slice
//
// A slice is a tree describing how to access an array (both vector and matrix).
// When the method is simply an index, the tree will have a subtree topped with
// a label "index" followed by an expression tree resulting in an index.
//
// <index>
//    int_expr
// </index>
//
// When the method is explained in terms of starting and ending
// locations and stride, the appended subtree is topped with a label "index"
// followed by three children expression trees.
// 
// <index>
//    <start>
//        int_expr
//    </start>
//    <end>
//        int_expr
//    </end>
//    <stride>
//        int_expr
//    </stride>
// </index>
//
//==================================================================================
slice :
int_expr { 

  DOMElement * index_expr = gSpkExpTree->createElement( X( STR_INDEX ) );

  index_expr->appendChild( $1->node );
  index_expr->setAttribute( X( STR_TYPE ), X( Symbol::C_DOUBLE) );
  index_expr->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_SCALAR ) ) ;
  index_expr->setAttribute( X( STR_ROWS ), X( "1" ) );
  index_expr->setAttribute( X( STR_COLS ), X( "1" ) );
  index_expr->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = index_expr;
  $$ = carrier;
}
|
start_subscript ':' end_subscript { 

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  DOMElement * mySlice  = gSpkExpTree->createElement( X( STR_INDEX ) );
  // type & structure?

  DOMElement * myStart  = gSpkExpTree->createElement( X( STR_START ) );
  // type & structure?

  DOMElement * myEnd    = gSpkExpTree->createElement( X( STR_END ) );
  // type & structure?
 
  DOMElement * myStride = gSpkExpTree->createElement( X( STR_STRIDE ) );

  DOMElement * myStrideVal = gSpkExpTree->createElement( X( STR_CONSTANT ) );
  myStrideVal->setAttribute( X( STR_VALUE ), X( "1" ) );
  myStrideVal->setAttribute( X( STR_TYPE ),  X( Symbol::C_INT ) );
  myStrideVal->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_SCALAR ) );
  myStrideVal->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );

  myStart ->appendChild( $1->node );
  myEnd   ->appendChild( $3->node );
  myStride->appendChild( myStrideVal );

  mySlice->appendChild( myStart );
  mySlice->appendChild( myEnd );
  mySlice->appendChild( myStride );

  mySlice->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_VECTOR ) );
  mySlice->setAttribute( X( STR_TYPE ), X( Symbol::C_DOUBLE ) );
  mySlice->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );
  mySlice->setAttribute( X( STR_ROWS ), X( Symbol::C_UNKNOWN ) );
  mySlice->setAttribute( X( STR_COLS ), X( Symbol::C_UNKNOWN ) );

  carrier->node = mySlice;
  $$ = carrier;
}
|
start_subscript ':' end_subscript ':' stride { 
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  DOMElement * mySlice  = gSpkExpTree->createElement( X( STR_INDEX ) );
  // type & structure?

  DOMElement * myStart  = gSpkExpTree->createElement( X( STR_START ) );
  // type & structure?

  DOMElement * myEnd    = gSpkExpTree->createElement( X( STR_END ) );
  // type & structure?

  DOMElement * myStride = gSpkExpTree->createElement( X( STR_STRIDE ) );
  // type & structure?

  myStart ->appendChild( $1->node );
  myEnd   ->appendChild( $3->node );
  myStride->appendChild( $5->node );

  mySlice->appendChild( myStart );
  mySlice->appendChild( myEnd );
  mySlice->appendChild( myStride );

  mySlice->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_VECTOR ) );
  mySlice->setAttribute( X( STR_TYPE ), X( Symbol::C_DOUBLE ) );
  mySlice->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );
  mySlice->setAttribute( X( STR_ROWS ), X( Symbol::C_UNKNOWN ) );
  mySlice->setAttribute( X( STR_COLS ), X( Symbol::C_UNKNOWN ) );

  carrier->node = mySlice;
  $$ = carrier;
}
;

//==================================================================================
//
// start_subscript
//
// A start_subscript is an int_expr.
//
//==================================================================================
start_subscript :
int_expr { 
  $$ = $1;
}
;

//==================================================================================
//
// end_subscript
//
// An end_subscript is an int_expr.
//
//==================================================================================
end_subscript :
int_expr {
  $$ = $1;
}
;

//==================================================================================
// 
// stride
//
// A stride is an int_expr.
//
//==================================================================================
stride :
int_expr {
  $$ = $1;
}
;

//==================================================================================
//
// int_expr
//
// An int_expr is an expression of type integer.
//
//==================================================================================
int_expr : 
arithmatic_expr {
  $$ = $1;
}
;

//==================================================================================
//
// primary
//
// A primary object is one of the followings:
//
// 1) literal_constant (ex. 1234, 1234.56)
// 2) variable (ex. A, A(i:j), A(i:j:h, i:j:h))
// 3) array_constructor (ex. (/1,2,3/))
// 4) function_ref (ex. log(a))
// 5) parenthesized expression (i.e. ( expr ) )
//
// In the form (5), the expression tree is appended as the only child of 
// "prioritized" tree which is going to be returned by this production.
//
//==================================================================================
primary :
literal_constant { $$ = $1; }
|
variable { $$ = $1; }
|
array_constructor { $$ = $1; }
|
function_ref { $$ = $1; }
|
'(' expr ')' { 
  DOMElement * paren = gSpkExpTree->createElement( X( STR_PRIORITIZED ) );
  paren->setAttribute( X( STR_TYPE ), $2->node->getAttribute( X( STR_TYPE ) ) );
  paren->setAttribute( X( STR_STRUCTURE ), $2->node->getAttribute( X( STR_STRUCTURE ) ) );
  paren->setAttribute( X( STR_SIGN ), $2->node->getAttribute( X( STR_SIGN ) ) );
  paren->setAttribute( X( STR_ROWS ), $2->node->getAttribute( X( STR_ROWS ) ) );
  paren->setAttribute( X( STR_COLS ), $2->node->getAttribute( X( STR_COLS ) ) );
  paren->appendChild( $2->node );

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = paren;
  $$ = carrier;
 }
;

//==================================================================================
//
// expr
//
// An "expr" is a logical_expr.
//
//==================================================================================
expr :
logical_expr { 
  $$ = $1;
}
;

//==================================================================================
//
// if_stmt
//
// An if_stmt is a statement tree whose root is labeled as "if" and has TWO child trees:
// one for conditional statement and another for a SINGLE assignment statement excuted
// when the condtion evaluates to true.
//
// <if>
//    <condition>
//       logical_expr
//    </condition>
//    <then>
//       assignment
//    </then>
// </if>
//
//==================================================================================
if_stmt :
IF '(' logical_expr ')' assignment_stmt { 

  //
  // A logical_expr can be an arithmatic_expr by syntactic rule
  // but here we expect only an expression that evaluates to true or false.
  // FORTRAN does not treat a numeric value (like 0 or 1) as a logical value.
  // So, we kicks out arithmatic expressions here.
  // 
  if( !XMLString::equals( $3->node->getAttribute( X(STR_TYPE) ), X( Symbol::C_BOOL) ) )
  {
    yyerror( "Wrong type!  Only expressions that evalute to true/false are accepted!" );
  }

  DOMElement * if_stmt   = gSpkExpTree->createElement( X( STR_IF ) );
  DOMElement * condition = gSpkExpTree->createElement( X( STR_CONDITION ) );
  condition->appendChild( $3->node );
  if_stmt->appendChild( condition );

  DOMElement * then_block = gSpkExpTree->createElement( X( STR_THEN ) );
  then_block->appendChild( $5->node );
  if_stmt->appendChild( then_block );

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = if_stmt;
  $$ = carrier;
}
;
//==================================================================================
//
// if_construct
//
// An if_stmt is a tree whose root is labeled as "if" and has THREE child trees:
// one for conditional statement, another for a block of expressions excuted
// when the condtion evaluates to true and lastly for a block of expressions
// excuted otherwise.
//
// <if>
//    <condition>
//       logical_expr
//    </condition>
//    <then>
//       block
//    </then>
//    <else>
//       block
//    </else>
// </if>
//
//==================================================================================
/*
if_construct :
if_then_stmt '\n'  { inConditional = true; } block  { inConditional = false; }
             ELSE '\n' { inConditional = true; } block { inConditional = false; } ENDIF '\n' { 

  DOMElement * node = gSpkExpTree->createElement( X( STR_IF ) );

  DOMElement * myConditional_expr = gSpkExpTree->createElement( X( STR_CONDITION ) );
  myConditional_expr->appendChild( $1->node );

  DOMElement * then_block = gSpkExpTree->createElement( X( STR_THEN ) );
  then_block->appendChild( $4->node );

  DOMElement * else_block = gSpkExpTree->createElement( X( STR_ELSE ) );
  else_block->appendChild( $9->node );

  node->appendChild( myConditional_expr );
  node->appendChild( then_block );
  node->appendChild( else_block );

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = node;
  $$ = carrier;
}
;
*/

if_construct :
if_then_clause ENDIF '\n' { 
  $$ = $1; 
} 

| if_then_clause else_clause ENDIF '\n' {
  $1->node->appendChild( $2->node->getFirstChild() );
  $$ = $1;
}
;

if_then_clause :
if_then_stmt '\n' { inConditional = true; } block  { inConditional = false; } {
  DOMElement * node = gSpkExpTree->createElement( X( STR_IF ) );

  DOMElement * myConditional_expr = gSpkExpTree->createElement( X( STR_CONDITION ) );
  myConditional_expr->appendChild( $1->node );

  DOMElement * then_block = gSpkExpTree->createElement( X( STR_THEN ) );
  then_block->appendChild( $4->node );

  node->appendChild( myConditional_expr );
  node->appendChild( then_block );

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = node;
  $$ = carrier;
}
;
else_clause :
ELSE '\n' { inConditional = true; } block  { inConditional = false; } {

  DOMElement * else_block = gSpkExpTree->createElement( X( STR_ELSE ) );
  else_block->appendChild( $4->node );

  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  carrier->node = else_block;
  $$ = carrier;
}
;

//==================================================================================
//
// if_then_stmt
//
// An if_then_stmt is a portion of if_construct that extracts and returns 
// the conditional statement.
//
//==================================================================================
if_then_stmt :
IF '(' logical_expr ')' THEN { 
  //
  // A logical_expr can be an arithmatic_expr by syntactic rule
  // but here we expect only an expression that evaluates to true or false.
  // FORTRAN does not treat a numeric value (like 0 or 1) as a logical value.
  // So, we kicks out arithmatic expressions here.
  // 
  if( !XMLString::equals( $3->node->getAttribute( X(STR_TYPE) ), X(Symbol::C_BOOL) ) )
  {
    yyerror( "Wrong type!  Only expressions that evalute to true/false are accepted!" );
  }
  $$ = $3;
}
;

//==================================================================================
//
// block
//
// A block is a set of expressions and blank lines. it can be made of only blanks.
// A block tree is topped with label "block" and follows expressions as subtrees.
//
//    line
//    line
//    line
//    ...
//
//==================================================================================
block :
/* empty */
{ 
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  DOMElement * block = gSpkExpTree->createElement( X( "block" ) );
  carrier->node = block;
  $$ = carrier;
}
|
block line { 

  struct ExpNodeCarrier * carrier = $1;
  $1->node->appendChild( $2->node );

  $$ = carrier;
}
;

//==================================================================================
//
// literal_constant
//
// A literal_constant can be either integer type or real type.
//
//==================================================================================
literal_constant :
int_literal_constant  { 
  $$ = $1; 
}
|
real_literal_constant { 
  $$ = $1; 
}
;

//==================================================================================
//
// int_literal_constant
//
// An int_literal_constant is a literal constant (ex. 1234) of type integer.
// This production returns a tree topped with label "constant" whose type
// is Symbol::C_INT and value is the token value.
//
// <constant type="integer" value="" rows=1 cols=1/>
//
//==================================================================================
int_literal_constant :
DIGIT_STRING { 

  //
  // This is an integer constant.
  //
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();

  DOMElement * int_constant = gSpkExpTree->createElement( X(STR_CONSTANT) );
  int_constant->setAttribute( X( STR_TYPE ), X( Symbol::C_INT ) );
  int_constant->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_SCALAR ) );
  int_constant->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );
  int_constant->setAttribute( X( STR_VALUE ), X( $1 ) );
  int_constant->setAttribute( X( STR_ROWS ), X( "1" ) );
  int_constant->setAttribute( X( STR_COLS ), X( "1" ) );

  carrier->node = int_constant;
  $$ = carrier;
}
;

//==================================================================================
//
// real_literal_constant
//
// An real_literal_constant is a literal constant (ex. 12.34) of type real.
// This production returns a tree topped with label "constant" whose type
// is "real" and value is the token value.
// 
// <constant type="double" value="" rows=1 cols=1/>
//
//==================================================================================
real_literal_constant :
SIGNIFICAND { 
  //
  // This is a real constant in the form of "0.0".
  //
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  DOMElement * real_constant = gSpkExpTree->createElement( X(STR_CONSTANT) );
  real_constant->setAttribute( X( STR_TYPE ), X( Symbol::C_DOUBLE ) );
  real_constant->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_SCALAR ) );
  real_constant->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN) );
  real_constant->setAttribute( X( STR_VALUE ), X( $1 ) );
  real_constant->setAttribute( X( STR_ROWS ), X( "1" ) );
  real_constant->setAttribute( X( STR_COLS ), X( "1" ) );
  carrier->node = real_constant;
  $$ = carrier;
}
|
ENG_NOTATION { 
  // 
  // This is a real constant in the form of "0.0E0"
  //
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  DOMElement * real_constant = gSpkExpTree->createElement( X(STR_CONSTANT) );
  real_constant->setAttribute( X( STR_TYPE ), X( Symbol::C_DOUBLE ) );
  real_constant->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_SCALAR ) );
  real_constant->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );
  real_constant->setAttribute( X( STR_VALUE ), X( $1 ) );
  real_constant->setAttribute( X( STR_ROWS ), X( "1" ) );
  real_constant->setAttribute( X( STR_COLS ), X( "1" ) );
  carrier->node = real_constant;
  $$ = carrier;
}
;

//==================================================================================
//
// logical_constant
//
// A logical_constant is a literal constant of type logical (boolean).
// It has two values: TRUE or FALSE.
//
// for TRUE:
// <constant type="bool" value="true" rows=1 cols=1/>
//
// for FALSE:
// <constant type="bool" value"false" rows=1 cols=1/>
//
//==================================================================================
logical_constant :
TRUE { 
  //
  // This is a boolean value "true".
  //
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  DOMElement * bool_constant = gSpkExpTree->createElement( X(STR_CONSTANT) );
  bool_constant->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  bool_constant->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_SCALAR ) );
  bool_constant->setAttribute( X( STR_SIGN ), X( STR_PLUS ) );// + sign means the same as non-negate, - means negate
  bool_constant->setAttribute( X( STR_VALUE ), X( STR_TRUE ) );
  bool_constant->setAttribute( X( STR_ROWS ), X( "1" ) );
  bool_constant->setAttribute( X( STR_COLS ), X( "1" ) );
  carrier->node = bool_constant;
  $$ = carrier;
}
|
FALSE {
  //
  // This is a boolean value "false".
  //
  struct ExpNodeCarrier * carrier = expTreeUtils.createExpNodeCarrier();
  DOMElement * bool_constant = gSpkExpTree->createElement( X(STR_CONSTANT) );
  bool_constant->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  bool_constant->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_SCALAR ) );
  bool_constant->setAttribute( X( STR_SIGN ), X( STR_PLUS ) );// + sign means the same as non-negate, - means negate
  bool_constant->setAttribute( X( STR_VALUE ), X( STR_FALSE ) );
  bool_constant->setAttribute( X( STR_ROWS ), X( "1" ) );
  bool_constant->setAttribute( X( STR_COLS ), X( "1" ) );
  carrier->node = bool_constant;
  $$ = carrier;
}
;

//==================================================================================
//
// array_constructor
//
// An array_constructor is a list of array initials enclosed by "(/" and "/)".
//
//==================================================================================
array_constructor :
OPEN_ARRAY_ELEM_LIST array_element_list CLOSE_ARRAY_ELEM_LIST {
  $$ = $2;
}
;

//==================================================================================
//
// array_element_list
//
// An array_element_list is a list of values or expressions that initialize
// an array.  It is a tree topped with label "array_element_list".
// The type of the array element list is determined by the value(s) of
// the highest precision or tightest.  However, logical values and other
// numerical values do not mix.
//
// <array_element_list type=TBD size="">
// 
//==================================================================================
array_element_list :
expr {

  DOMElement * elements_list = gSpkExpTree->createElement( X( STR_ARRAY_ELEMENT_LIST ) );
  elements_list->setAttribute( X( STR_TYPE ), $1->node->getAttribute( X( STR_TYPE ) ) );
  elements_list->setAttribute( X( STR_STRUCTURE ), $1->node->getAttribute( X( STR_STRUCTURE ) ) );
  elements_list->setAttribute( X( STR_SIGN ), $1->node->getAttribute( X( STR_SIGN ) ) );
  elements_list->setAttribute( X( STR_ROWS ), $1->node->getAttribute( X( STR_ROWS ) ) );
  elements_list->setAttribute( X( STR_COLS ), $1->node->getAttribute( X( STR_COLS ) ) );
  elements_list->appendChild( $1->node );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = elements_list;
  $$ = carrier;
}
|
array_element_list ',' expr {
  struct ExpNodeCarrier * carrier = $1;

  const XMLCh * type1 = $1->node->getAttribute( X( STR_TYPE ) );
  const XMLCh * type2 = $3->node->getAttribute( X( STR_TYPE ) );

  if( XMLString::equals( type1, type2 ) )
    {
      carrier->node->setAttribute( X( STR_TYPE ), type1 );
    }
  else if( !XMLString::equals( type1, X( Symbol::C_BOOL ) ) &&  XMLString::equals( type2, X( Symbol::C_BOOL ) )  )
    {
      // mismatch!
      yyerror( "Array elements cannot mix boolean and other types( integer/double )." );
      //exit(-1);
    }
  else if( XMLString::equals( type1, X( Symbol::C_INT ) ) )
    {
      if( XMLString::equals( type2, X( Symbol::C_DOUBLE ) ) )
      {
	// promote precision
	carrier->node->setAttribute( X( STR_TYPE ), type2 );
      }
    }
  else if( XMLString::equals( type1, X( Symbol::C_UNKNOWN ) ) && !XMLString::equals( type2, X( Symbol::C_UNKNOWN ) ) )
  {
      carrier->node->setAttribute( X( STR_TYPE ), type2 );
  }
  else if( !XMLString::equals( type1, X( Symbol::C_UNKNOWN ) ) && XMLString::equals( type2, X( Symbol::C_UNKNOWN ) ) )
  {
      carrier->node->setAttribute( X( STR_TYPE ), type1 );
  }
  else if( XMLString::equals( type1, X( Symbol::C_DOUBLE ) ) )
    {
      if( XMLString::equals( type2, X( Symbol::C_INT ) ) )
      {
	/* do nothing */
      }
    }
  else
    {
      assert( false );
    }

  int lhs_rows = atoi( C( carrier->node->getAttribute( X( STR_ROWS ) ) ) );
  int rhs_rows = atoi( C( $3->node->getAttribute( X( STR_ROWS ) ) ) );
  char buf [DEFAULT_BUFF_SIZE];
  sprintf( buf, "%d", lhs_rows+rhs_rows );

  carrier->node->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );
  carrier->node->setAttribute( X( STR_ROWS ), X( buf ) );
  carrier->node->setAttribute( X( STR_STRUCTURE ), X( Symbol::C_VECTOR ) );
  carrier->node->appendChild( $3->node );
  $$ = carrier;
}
;

//==================================================================================
//
// function_ref
//
// A function_ref is a reference to an existing function.
// NONMEM abbriviated code supports only sqrt, exp, log and log10.
// It retrurns a tree topped with label "function".
// The name attribute of the function is the function name.
// The type attirbute of the function is the type of the value resulted from evaluting
// the function.  Thus, in this limited set of functions, it is set to
// "double".  The "argc" attribute of the tree specifies the number of arguments
// the function takes; in all these cases, it is one.
//
// <function name="" argc="1" type="double">
//
//==================================================================================
function_ref :
DEFINED_UNARY_FUNCTION '(' arithmatic_expr ')' { 

  DOMElement * function = gSpkExpTree->createElement( X( STR_FUNCTION ) );
  function->setAttribute( X( STR_ARGC ), X( "1" ) );
  function->setAttribute( X( STR_TYPE ), X( Symbol::C_DOUBLE ) );
  function->setAttribute( X( STR_STRUCTURE ), $3->node->getAttribute( X( STR_STRUCTURE ) ) );
  function->setAttribute( X( STR_SIGN ), $3->node->getAttribute( X( Symbol::C_UNKNOWN ) ) );// the sign is actually undermined
  function->setAttribute( X( STR_ROWS ), $3->node->getAttribute( X( STR_ROWS ) ) );
  function->setAttribute( X( STR_COLS ), $3->node->getAttribute( X( STR_COLS ) ) );

  if( stricmp( $1, "SQRT" ) == 0 )
    {
      function->setAttribute( X( STR_NAME ), X( STR_SQRT ) );
    }
  else if( stricmp( $1, "EXP" ) == 0 )
    {
      function->setAttribute( X( STR_NAME ), X( STR_EXP ) );
    }
  else if( stricmp( $1, "LOG" ) == 0 )
    {
      function->setAttribute( X( STR_NAME ), X( STR_LOG ) );
    }
  else if( stricmp( $1, "LOG10" ) == 0 )
    {
      function->setAttribute( X( STR_NAME ), X( STR_LOG10 ) );
    }
  else
    {
      yyerror( $1 );
    }
  function->appendChild( $3->node );

  struct ExpNodeCarrier * carrier = $3;
  carrier->node = function;
  $$ = carrier;
}
;

//==================================================================================
//
// arithmatic_expr
//
// An arithmatic_expr is an expression that evalutes to a numerical value.
// 
// The precedence is 
//
// 4) a + b (addition),  a - b (subtraction)
// 3) +a (plus), -a (minus)
// 2) a * b (multiplication), a / b (division)
// 1) a ** b (power).
// 
// It returns a tree topped with label "operator" for +, -, * and / and
// their operand attribute describes the number of operand(s) they take;
// for example, for + as in addition, the attribute is set to "binary".
// whereas, + as in sign, it is set to "unary".
// For ** (power), it is considered as a "function" and thus the tree
// top is labeled as "function". The value attribute describes the kind of
// operator or function.  The type attribute is inherited from one of the operands
// whose precision is the highest or tightest.
//
//==================================================================================
arithmatic_expr :
arithmatic_expr '+' arithmatic_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_ADD ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );  // the sign is actual undetermined.

  DOMElement * lhs   = gSpkExpTree->createElement( X( STR_LHS ) );
  lhs->setAttribute( X( STR_TYPE ), $1->node->getAttribute( X( STR_TYPE ) ) );
  lhs->setAttribute( X( STR_STRUCTURE ), $1->node->getAttribute( X( STR_STRUCTURE ) ) );
  lhs->setAttribute( X( STR_SIGN ), $1->node->getAttribute( X( STR_SIGN ) ) );
  lhs->setAttribute( X( STR_ROWS ), $1->node->getAttribute( X( STR_ROWS ) ) );
  lhs->setAttribute( X( STR_COLS ), $1->node->getAttribute( X( STR_COLS ) ) );

  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  rhs->setAttribute( X( STR_TYPE ), $3->node->getAttribute( X( STR_TYPE ) ) );
  rhs->setAttribute( X( STR_STRUCTURE ), $3->node->getAttribute( X( STR_STRUCTURE ) ) );
  rhs->setAttribute( X( STR_SIGN ), $3->node->getAttribute( X( STR_SIGN ) ) );
  rhs->setAttribute( X( STR_ROWS ), $3->node->getAttribute( X( STR_ROWS ) ) );
  rhs->setAttribute( X( STR_COLS ), $3->node->getAttribute( X( STR_COLS ) ) );

  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  const XMLCh* lhs_type = $1->node->getAttribute( X( STR_TYPE ) );
  const XMLCh* rhs_type = $3->node->getAttribute( X( STR_TYPE ) );

  // Inherit the data type from either left or right hand side which has higher precision.
  if( XMLString::equals( lhs_type, rhs_type ) )
    {
      op->setAttribute( X( STR_TYPE ), lhs_type );
    }
  else if( XMLString::equals( lhs_type, X( Symbol::C_UNKNOWN ) ) && !XMLString::equals( rhs_type, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_TYPE ), rhs_type );
  }
  else if( !XMLString::equals( lhs_type, X( Symbol::C_UNKNOWN ) ) && XMLString::equals( rhs_type, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_TYPE ), lhs_type );
  }
  else if( XMLString::equals( lhs_type, X( Symbol::C_DOUBLE ) ) || XMLString::equals( rhs_type, X( Symbol::C_DOUBLE ) )  )
    {
      op->setAttribute( X( STR_TYPE ),  X( Symbol::C_DOUBLE ) );
    }
  else
  {
    assert( false );
  }

  const XMLCh* lhs_structure = $1->node->getAttribute( X( STR_STRUCTURE ) );
  const XMLCh* rhs_structure = $3->node->getAttribute( X( STR_STRUCTURE ) );
  // Inherit the data structure from either left or righ hand side which has greater aggregation.
  if( XMLString::equals( lhs_structure, rhs_structure ) )
    {
      op->setAttribute( X( STR_STRUCTURE ), lhs_structure );
    }
  else if( XMLString::equals( lhs_structure, X( Symbol::C_UNKNOWN ) )
	   && !XMLString::equals( rhs_structure, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_STRUCTURE ), rhs_structure );
  }
  else if( !XMLString::equals( lhs_structure, X( Symbol::C_UNKNOWN ) ) 
	   && XMLString::equals( rhs_structure, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_STRUCTURE ), lhs_structure );
  }
  else if( XMLString::equals( lhs_structure, X( Symbol::C_MATRIX ) ) 
	   || XMLString::equals( rhs_structure, X( Symbol::C_MATRIX ) )  )
    {
      op->setAttribute( X( STR_STRUCTURE ),  X( Symbol::C_MATRIX ) );
    }
  else if( ( XMLString::equals( lhs_structure, X( Symbol::C_VECTOR ) )
	     || XMLString::equals( rhs_structure, X( Symbol::C_VECTOR ) ) ) )
    {
      op->setAttribute( X( STR_STRUCTURE ),  X( Symbol::C_VECTOR ) );
    }
  else
  {
    assert( false );
  }

  // Inherit the dimensions
  const XMLCh* lhs_rows = $1->node->getAttribute( X( STR_ROWS ) );
  const XMLCh* rhs_rows = $3->node->getAttribute( X( STR_ROWS ) );
  const XMLCh* lhs_cols = $1->node->getAttribute( X( STR_COLS ) );
  const XMLCh* rhs_cols = $3->node->getAttribute( X( STR_COLS ) );
  //assert( atoi(C(lhs_rows))==atoi(C(rhs_rows)) || atoi(C(lhs_rows))==1 || atoi(C(rhs_rows))==1 );
  //assert( atoi(C(lhs_cols))==atoi(C(rhs_cols)) || atoi(C(lhs_cols))==1 || atoi(C(rhs_cols))==1 );

  op->setAttribute( X( STR_ROWS ), ( atoi(C(lhs_rows))>atoi(C(rhs_rows))? lhs_rows : rhs_rows ) );
  op->setAttribute( X( STR_COLS ), ( atoi(C(lhs_cols))>atoi(C(rhs_cols))? lhs_cols : rhs_cols ) );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
arithmatic_expr '-' arithmatic_expr { 

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_SUBTRACT ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) ); // the sign is actually undetermined.

  DOMElement * lhs   = gSpkExpTree->createElement( X( STR_LHS ) );
  lhs->setAttribute( X( STR_TYPE ), $1->node->getAttribute( X( STR_TYPE ) ) );
  lhs->setAttribute( X( STR_STRUCTURE ), $1->node->getAttribute( X( STR_STRUCTURE ) ) );
  lhs->setAttribute( X( STR_SIGN ), $1->node->getAttribute( X( STR_SIGN ) ) );
  lhs->setAttribute( X( STR_ROWS ), $1->node->getAttribute( X( STR_ROWS ) ) );
  lhs->setAttribute( X( STR_COLS ), $1->node->getAttribute( X( STR_COLS ) ) );

  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  rhs->setAttribute( X( STR_TYPE ), $3->node->getAttribute( X( STR_TYPE ) ) );
  rhs->setAttribute( X( STR_STRUCTURE ), $3->node->getAttribute( X( STR_STRUCTURE ) ) );
  rhs->setAttribute( X( STR_SIGN ), $3->node->getAttribute( X( STR_SIGN ) ) );
  rhs->setAttribute( X( STR_ROWS ), $3->node->getAttribute( X( STR_ROWS ) ) );
  rhs->setAttribute( X( STR_COLS ), $3->node->getAttribute( X( STR_COLS ) ) );

  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  const XMLCh* lhs_type = $1->node->getAttribute( X( STR_TYPE ) );
  const XMLCh* rhs_type = $3->node->getAttribute( X( STR_TYPE ) );

  if( XMLString::equals( lhs_type, rhs_type ) )
    {
      op->setAttribute( X( STR_TYPE ), lhs_type );
    }
  else if( XMLString::equals( lhs_type, X( Symbol::C_UNKNOWN ) ) && !XMLString::equals( rhs_type, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_TYPE ), rhs_type );
  }
  else if( !XMLString::equals( lhs_type, X( Symbol::C_UNKNOWN ) ) && XMLString::equals( rhs_type, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_TYPE ), lhs_type );
  }
  else if( XMLString::equals( lhs_type, X( Symbol::C_DOUBLE ) ) || XMLString::equals( rhs_type, X( Symbol::C_DOUBLE ) )  )
    {
      op->setAttribute( X( STR_TYPE ),  X( Symbol::C_DOUBLE ) );
    }
  else
  {
    assert( false );
  }

  const XMLCh* lhs_structure = $1->node->getAttribute( X( STR_STRUCTURE ) );
  const XMLCh* rhs_structure = $3->node->getAttribute( X( STR_STRUCTURE ) );
  // Inherit the data structure from either left or righ hand side which has greater aggregation.
  if( XMLString::equals( lhs_structure, rhs_structure ) )
    {
      op->setAttribute( X( STR_STRUCTURE ), lhs_structure );
    }
  else if( XMLString::equals( lhs_structure, X( Symbol::C_UNKNOWN ) )
	   && !XMLString::equals( rhs_structure, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_STRUCTURE ), rhs_structure );
  }
  else if( !XMLString::equals( lhs_structure, X( Symbol::C_UNKNOWN ) ) 
	   && XMLString::equals( rhs_structure, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_STRUCTURE ), lhs_structure );
  }
  else if( XMLString::equals( lhs_structure, X( Symbol::C_MATRIX ) ) 
	   || XMLString::equals( rhs_structure, X( Symbol::C_MATRIX ) )  )
    {
      op->setAttribute( X( STR_STRUCTURE ),  X( Symbol::C_MATRIX ) );
    }
  else if( ( XMLString::equals( lhs_structure, X( Symbol::C_VECTOR ) )
	     || XMLString::equals( rhs_structure, X( Symbol::C_VECTOR ) ) ) )
    {
      op->setAttribute( X( STR_STRUCTURE ),  X( Symbol::C_VECTOR ) );
    }
  else
  {
    assert( false );
  }

  const XMLCh* lhs_rows = $1->node->getAttribute( X( STR_ROWS ) );
  const XMLCh* rhs_rows = $3->node->getAttribute( X( STR_ROWS ) );
  const XMLCh* lhs_cols = $1->node->getAttribute( X( STR_COLS ) );
  const XMLCh* rhs_cols = $3->node->getAttribute( X( STR_COLS ) );
  //assert( atoi(C(lhs_rows))==atoi(C(rhs_rows)) || atoi(C(lhs_rows))==1 || atoi(C(rhs_rows))==1 );
  //assert( atoi(C(lhs_cols))==atoi(C(rhs_cols)) || atoi(C(lhs_cols))==1 || atoi(C(rhs_cols))==1 );

  op->setAttribute( X( STR_ROWS ), ( atoi(C(lhs_rows))>atoi(C(rhs_rows))? lhs_rows : rhs_rows ) );
  op->setAttribute( X( STR_COLS ), ( atoi(C(lhs_cols))>atoi(C(rhs_cols))? lhs_cols : rhs_cols ) );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
'-' arithmatic_expr %prec SIGN {
  DOMElement * rhs = $2->node;
  rhs->setAttribute( X( STR_SIGN ), X( STR_MINUS ) );
  rhs->setAttribute( X( STR_TYPE), $2->node->getAttribute( X( STR_TYPE ) ) );  
  rhs->setAttribute( X( STR_STRUCTURE), $2->node->getAttribute( X( STR_STRUCTURE ) ) ); 
  rhs->setAttribute( X( STR_ROWS), $2->node->getAttribute( X( STR_ROWS ) ) );
  rhs->setAttribute( X( STR_COLS), $2->node->getAttribute( X( STR_COLS ) ) );

  struct ExpNodeCarrier * carrier = $2;
  carrier->node = rhs;
  $$ = carrier;
}
|
'+' arithmatic_expr %prec SIGN {

  DOMElement * rhs = $2->node;
  rhs->setAttribute( X( STR_SIGN ), X( STR_PLUS ) );
  rhs->setAttribute( X( STR_TYPE), $2->node->getAttribute( X( STR_TYPE ) ) );  
  rhs->setAttribute( X( STR_STRUCTURE), $2->node->getAttribute( X( STR_STRUCTURE ) ) ); 
  rhs->setAttribute( X( STR_ROWS), $2->node->getAttribute( X( STR_ROWS ) ) );
  rhs->setAttribute( X( STR_COLS), $2->node->getAttribute( X( STR_COLS ) ) );

  struct ExpNodeCarrier * carrier = $2;
  carrier->node = rhs;
  $$ = carrier;
}
|
arithmatic_expr '*' arithmatic_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_MULTIPLY ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// the sign is actually undermined.

  DOMElement * lhs   = gSpkExpTree->createElement( X( STR_LHS ) );
  lhs->setAttribute( X( STR_TYPE ), $1->node->getAttribute( X( STR_TYPE ) ) );
  lhs->setAttribute( X( STR_STRUCTURE ), $1->node->getAttribute( X( STR_STRUCTURE ) ) );
  lhs->setAttribute( X( STR_SIGN ), $1->node->getAttribute( X( STR_SIGN ) ) );
  lhs->setAttribute( X( STR_ROWS ), $1->node->getAttribute( X( STR_ROWS ) ) );
  lhs->setAttribute( X( STR_COLS ), $1->node->getAttribute( X( STR_COLS ) ) );

  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  rhs->setAttribute( X( STR_TYPE ), $3->node->getAttribute( X( STR_TYPE ) ) );
  rhs->setAttribute( X( STR_STRUCTURE ), $3->node->getAttribute( X( STR_STRUCTURE ) ) );
  rhs->setAttribute( X( STR_SIGN ), $3->node->getAttribute( X( STR_SIGN ) ) );
  rhs->setAttribute( X( STR_ROWS ), $3->node->getAttribute( X( STR_ROWS ) ) );
  rhs->setAttribute( X( STR_COLS ), $3->node->getAttribute( X( STR_COLS ) ) );

  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  const XMLCh* lhs_type = $1->node->getAttribute( X( STR_TYPE ) );
  const XMLCh* rhs_type = $3->node->getAttribute( X( STR_TYPE ) );

  if( XMLString::equals( lhs_type, rhs_type ) )
    {
      op->setAttribute( X( STR_TYPE ), lhs_type );
    }
  else if( XMLString::equals( lhs_type, X( Symbol::C_UNKNOWN ) ) && !XMLString::equals( rhs_type, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_TYPE ), rhs_type );
  }
  else if( !XMLString::equals( lhs_type, X( Symbol::C_UNKNOWN ) ) && XMLString::equals( rhs_type, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_TYPE ), lhs_type );
  }
  else if( XMLString::equals( lhs_type, X( Symbol::C_DOUBLE ) ) || XMLString::equals( rhs_type, X( Symbol::C_DOUBLE ) )  )
    {
      op->setAttribute( X( STR_TYPE ),  X( Symbol::C_DOUBLE ) );
    }
  else
  {
    assert( false );
  }
  const XMLCh* lhs_structure = $1->node->getAttribute( X( STR_STRUCTURE ) );
  const XMLCh* rhs_structure = $3->node->getAttribute( X( STR_STRUCTURE ) );
  // Inherit the data structure from either left or righ hand side which has greater aggregation.
  if( XMLString::equals( lhs_structure, rhs_structure ) )
    {
      op->setAttribute( X( STR_STRUCTURE ), lhs_structure );
    }
  else if( XMLString::equals( lhs_structure, X( Symbol::C_UNKNOWN ) )
	   && !XMLString::equals( rhs_structure, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_STRUCTURE ), rhs_structure );
  }
  else if( !XMLString::equals( lhs_structure, X( Symbol::C_UNKNOWN ) ) 
	   && XMLString::equals( rhs_structure, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_STRUCTURE ), lhs_structure );
  }
  else if( XMLString::equals( lhs_structure, X( Symbol::C_MATRIX ) ) 
	   || XMLString::equals( rhs_structure, X( Symbol::C_MATRIX ) )  )
    {
      op->setAttribute( X( STR_STRUCTURE ),  X( Symbol::C_MATRIX ) );
    }
  else if( ( XMLString::equals( lhs_structure, X( Symbol::C_VECTOR ) )
	     || XMLString::equals( rhs_structure, X( Symbol::C_VECTOR ) ) ) )
    {
      op->setAttribute( X( STR_STRUCTURE ),  X( Symbol::C_VECTOR ) );
    }
  else
  {
    assert( false );
  }

  const XMLCh* lhs_rows = $1->node->getAttribute( X( STR_ROWS ) );
  const XMLCh* rhs_rows = $3->node->getAttribute( X( STR_ROWS ) );
  const XMLCh* lhs_cols = $1->node->getAttribute( X( STR_COLS ) );
  const XMLCh* rhs_cols = $3->node->getAttribute( X( STR_COLS ) );
  //assert( atoi(C(lhs_rows))==atoi(C(rhs_rows)) || atoi(C(lhs_rows))==1 || atoi(C(rhs_rows))==1 );
  //assert( atoi(C(lhs_cols))==atoi(C(rhs_cols)) || atoi(C(lhs_cols))==1 || atoi(C(rhs_cols))==1 );

  op->setAttribute( X( STR_ROWS ), ( atoi(C(lhs_rows))>atoi(C(rhs_rows))? lhs_rows : rhs_rows ) );
  op->setAttribute( X( STR_COLS ), ( atoi(C(lhs_cols))>atoi(C(rhs_cols))? lhs_cols : rhs_cols ) );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
arithmatic_expr '/' arithmatic_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_DIVIDE ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// the sign is actually undermined.

  DOMElement * lhs   = gSpkExpTree->createElement( X( STR_LHS ) );
  lhs->setAttribute( X( STR_TYPE ), $1->node->getAttribute( X( STR_TYPE ) ) );
  lhs->setAttribute( X( STR_STRUCTURE ), $1->node->getAttribute( X( STR_STRUCTURE ) ) );
  lhs->setAttribute( X( STR_SIGN ), $1->node->getAttribute( X( STR_SIGN ) ) );
  lhs->setAttribute( X( STR_ROWS ), $1->node->getAttribute( X( STR_ROWS ) ) );
  lhs->setAttribute( X( STR_COLS ), $1->node->getAttribute( X( STR_COLS ) ) );

  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  rhs->setAttribute( X( STR_TYPE ), $3->node->getAttribute( X( STR_TYPE ) ) );
  rhs->setAttribute( X( STR_STRUCTURE ), $3->node->getAttribute( X( STR_STRUCTURE ) ) );
  rhs->setAttribute( X( STR_SIGN ), $3->node->getAttribute( X( STR_SIGN ) ) );
  rhs->setAttribute( X( STR_ROWS ), $3->node->getAttribute( X( STR_ROWS ) ) );
  rhs->setAttribute( X( STR_COLS ), $3->node->getAttribute( X( STR_COLS ) ) );

  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  const XMLCh* lhs_type = $1->node->getAttribute( X( STR_TYPE ) );
  const XMLCh* rhs_type = $3->node->getAttribute( X( STR_TYPE ) );

  if( XMLString::equals( lhs_type, rhs_type ) )
    {
      op->setAttribute( X( STR_TYPE ), lhs_type );
    }
  else if( XMLString::equals( lhs_type, X( Symbol::C_UNKNOWN ) ) && !XMLString::equals( rhs_type, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_TYPE ), rhs_type );
  }
  else if( !XMLString::equals( lhs_type, X( Symbol::C_UNKNOWN ) ) && XMLString::equals( rhs_type, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_TYPE ), lhs_type );
  }
  else if( XMLString::equals( lhs_type, X( Symbol::C_DOUBLE ) ) || XMLString::equals( rhs_type, X( Symbol::C_DOUBLE ) )  )
    {
      op->setAttribute( X( STR_TYPE ),  X( Symbol::C_DOUBLE ) );
    }
  else
  {
    assert( false );
  }
  const XMLCh* lhs_structure = $1->node->getAttribute( X( STR_STRUCTURE ) );
  const XMLCh* rhs_structure = $3->node->getAttribute( X( STR_STRUCTURE ) );
  // Inherit the data structure from either left or righ hand side which has greater aggregation.
  if( XMLString::equals( lhs_structure, rhs_structure ) )
    {
      op->setAttribute( X( STR_STRUCTURE ), lhs_structure );
    }
  else if( XMLString::equals( lhs_structure, X( Symbol::C_UNKNOWN ) )
	   && !XMLString::equals( rhs_structure, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_STRUCTURE ), rhs_structure );
  }
  else if( !XMLString::equals( lhs_structure, X( Symbol::C_UNKNOWN ) ) 
	   && XMLString::equals( rhs_structure, X( Symbol::C_UNKNOWN ) ) )
  {
    op->setAttribute( X( STR_STRUCTURE ), lhs_structure );
  }
  else if( XMLString::equals( lhs_structure, X( Symbol::C_MATRIX ) ) 
	   || XMLString::equals( rhs_structure, X( Symbol::C_MATRIX ) )  )
    {
      op->setAttribute( X( STR_STRUCTURE ),  X( Symbol::C_MATRIX ) );
    }
  else if( ( XMLString::equals( lhs_structure, X( Symbol::C_VECTOR ) )
	     || XMLString::equals( rhs_structure, X( Symbol::C_VECTOR ) ) ) )
    {
      op->setAttribute( X( STR_STRUCTURE ),  X( Symbol::C_VECTOR ) );
    }
  else
  {
    assert( false );
  }

  const XMLCh* lhs_rows = $1->node->getAttribute( X( STR_ROWS ) );
  const XMLCh* rhs_rows = $3->node->getAttribute( X( STR_ROWS ) );
  const XMLCh* lhs_cols = $1->node->getAttribute( X( STR_COLS ) );
  const XMLCh* rhs_cols = $3->node->getAttribute( X( STR_COLS ) );
  //assert( atoi(C(lhs_rows))==atoi(C(rhs_rows)) || atoi(C(lhs_rows))==1 || atoi(C(rhs_rows))==1 );
  //assert( atoi(C(lhs_cols))==atoi(C(rhs_cols)) || atoi(C(lhs_cols))==1 || atoi(C(rhs_cols))==1 );

  op->setAttribute( X( STR_ROWS ), ( atoi(C(lhs_rows))>atoi(C(rhs_rows))? lhs_rows : rhs_rows ) );
  op->setAttribute( X( STR_COLS ), ( atoi(C(lhs_cols))>atoi(C(rhs_cols))? lhs_cols : rhs_cols ) );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
arithmatic_expr POWER_OP arithmatic_expr {

  DOMElement * function = gSpkExpTree->createElement( X( STR_FUNCTION ) );
  function->setAttribute( X( STR_ARGC ), X( "2" ) );
  function->setAttribute( X( STR_NAME ), X( STR_POWER ) );
  function->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// the sign is actually undermined.

  DOMElement * lhs   = gSpkExpTree->createElement( X( STR_LHS ) );
  lhs->setAttribute( X( STR_TYPE ), $1->node->getAttribute( X( STR_TYPE ) ) );
  lhs->setAttribute( X( STR_STRUCTURE ), $1->node->getAttribute( X( STR_STRUCTURE ) ) );
  lhs->setAttribute( X( STR_SIGN ), $1->node->getAttribute( X( STR_SIGN ) ) );
  lhs->setAttribute( X( STR_ROWS ), $1->node->getAttribute( X( STR_ROWS ) ) );
  lhs->setAttribute( X( STR_COLS ), $1->node->getAttribute( X( STR_COLS ) ) );

  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  rhs->setAttribute( X( STR_TYPE ), $3->node->getAttribute( X( STR_TYPE ) ) );
  rhs->setAttribute( X( STR_STRUCTURE ), $3->node->getAttribute( X( STR_STRUCTURE ) ) );
  rhs->setAttribute( X( STR_SIGN ), $3->node->getAttribute( X( STR_SIGN ) ) );
  rhs->setAttribute( X( STR_ROWS ), $3->node->getAttribute( X( STR_ROWS ) ) );
  rhs->setAttribute( X( STR_COLS ), $3->node->getAttribute( X( STR_COLS ) ) );

  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  function->appendChild( lhs );
  function->appendChild( rhs );

  const XMLCh* lhs_type = $1->node->getAttribute( X( STR_TYPE ) );
  const XMLCh* rhs_type = $3->node->getAttribute( X( STR_TYPE ) );

  if( XMLString::equals( lhs_type, rhs_type ) )
    {
      function->setAttribute( X( STR_TYPE ), lhs_type );
    }
  else if( XMLString::equals( lhs_type, X( Symbol::C_UNKNOWN ) ) && !XMLString::equals( rhs_type, X( Symbol::C_UNKNOWN ) ) )
  {
    function->setAttribute( X( STR_TYPE ), rhs_type );
  }
  else if( !XMLString::equals( lhs_type, X( Symbol::C_UNKNOWN ) ) && XMLString::equals( rhs_type, X( Symbol::C_UNKNOWN ) ) )
  {
    function->setAttribute( X( STR_TYPE ), lhs_type );
  }
  else if( XMLString::equals( lhs_type, X( Symbol::C_DOUBLE ) ) || XMLString::equals( rhs_type, X( Symbol::C_DOUBLE ) )  )
    {
      function->setAttribute( X( STR_TYPE ),  X( Symbol::C_DOUBLE ) );
    }
  else
  {
    assert( false );
  }

  const XMLCh* lhs_structure = $1->node->getAttribute( X( STR_STRUCTURE ) );
  const XMLCh* rhs_structure = $3->node->getAttribute( X( STR_STRUCTURE ) );
  // Inherit the data structure from either left or righ hand side which has greater aggregation.
  if( XMLString::equals( lhs_structure, rhs_structure ) )
    {
      function->setAttribute( X( STR_STRUCTURE ), lhs_structure );
    }
  else if( XMLString::equals( lhs_structure, X( Symbol::C_UNKNOWN ) )
	   && !XMLString::equals( rhs_structure, X( Symbol::C_UNKNOWN ) ) )
  {
    function->setAttribute( X( STR_STRUCTURE ), rhs_structure );
  }
  else if( !XMLString::equals( lhs_structure, X( Symbol::C_UNKNOWN ) ) 
	   && XMLString::equals( rhs_structure, X( Symbol::C_UNKNOWN ) ) )
  {
    function->setAttribute( X( STR_STRUCTURE ), lhs_structure );
  }
  else if( XMLString::equals( lhs_structure, X( Symbol::C_MATRIX ) ) 
	   || XMLString::equals( rhs_structure, X( Symbol::C_MATRIX ) )  )
    {
      function->setAttribute( X( STR_STRUCTURE ),  X( Symbol::C_MATRIX ) );
    }
  else if( ( XMLString::equals( lhs_structure, X( Symbol::C_VECTOR ) )
	     || XMLString::equals( rhs_structure, X( Symbol::C_VECTOR ) ) ) )
    {
      function->setAttribute( X( STR_STRUCTURE ),  X( Symbol::C_VECTOR ) );
    }
  else
  {
    assert( false );
  }
  
  const XMLCh* lhs_rows = $1->node->getAttribute( X( STR_ROWS ) );
  const XMLCh* rhs_rows = $3->node->getAttribute( X( STR_ROWS ) );
  const XMLCh* lhs_cols = $1->node->getAttribute( X( STR_COLS ) );
  const XMLCh* rhs_cols = $3->node->getAttribute( X( STR_COLS ) );
  //assert( atoi(C(lhs_rows))==atoi(C(rhs_rows)) || atoi(C(lhs_rows))==1 || atoi(C(rhs_rows))==1 );
  //assert( atoi(C(lhs_cols))==atoi(C(rhs_cols)) || atoi(C(lhs_cols))==1 || atoi(C(rhs_cols))==1 );

  function->setAttribute( X( STR_ROWS ), ( atoi(C(lhs_rows))>atoi(C(rhs_rows))? lhs_rows : rhs_rows ) );
  function->setAttribute( X( STR_COLS ), ( atoi(C(lhs_cols))>atoi(C(rhs_cols))? lhs_cols : rhs_cols ) );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = function;
  $$ = carrier;
}
|
primary { $$ = $1; }
;

//==================================================================================
//
// relational_expr
//
// A relational_expr is an operator that compares the magnitudes or quantities of
// lhs and rhs and evaluates to true or false.
//
// Although a relational_expr, by syntactic rule, accepts an appearance of 
// arithmatic_expr by itself, a numerical value resulted from evaluating an 
// arithmatic expression, by FORTRAN's specification, is NOT interpreted 
// as true or false at all.  So, the clients of this production should
// check the type of the expression when necessarily.
// 
// The tree top is labeled "operator".  "operand" attribute of the root indicates
// the number of operands it takes; thus either binary or unary.
// "value" attribute specifies the kind of operator.  "type" of these relational
// expressions are all boolean.
// If the operator is a binary operator, the tree (as in binary operators) will
// have two child trees for each left hand side and right hand side expressions.
//  
//==================================================================================
relational_expr :
relational_expr EQ_OP relational_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_EQ ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// -sign means negate.  the sign is actually undetermined.

  DOMElement * lhs    = gSpkExpTree->createElement( X( STR_LHS ) );
  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
relational_expr NE_OP relational_expr { 

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_NE ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// -sign means negate.  the sign is actually undetermined.

  DOMElement * lhs    = gSpkExpTree->createElement( X( STR_LHS ) );
  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
relational_expr LT_OP relational_expr { 

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_LT ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// -sign means negate.  the sign is actually undetermined.

  DOMElement * lhs    = gSpkExpTree->createElement( X( STR_LHS ) );
  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
relational_expr LE_OP relational_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_LE ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// -sign means negate.  the sign is actually undetermined.

  DOMElement * lhs    = gSpkExpTree->createElement( X( STR_LHS ) );
  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
relational_expr GT_OP relational_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_GT ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// -sign means negate.  the sign is actually undetermined.

  DOMElement * lhs    = gSpkExpTree->createElement( X( STR_LHS ) );
  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
relational_expr GE_OP relational_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_GE ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// -sign means negate.  the sign is actually undetermined.

  DOMElement * lhs    = gSpkExpTree->createElement( X( STR_LHS ) );
  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
arithmatic_expr {
  $$ = $1;
}
|
logical_constant { 
  $$ = $1; 
}
;

//==================================================================================
//
// logical_expr
//
// A logical_expr is an operator that computes logical expressions.
// The operand(s) of such an operator must evaluete to true or false
// and the operator further evaluates to true or false.
//
//
// Although a logical_expr, by syntactic rule, accepts an appearance of 
// arithmatic_expr by itself, a numerical value resulted from evaluating an 
// arithmatic expression, by FORTRAN's specification, is NOT interpreted 
// as true or false at all.  So, the clients of this production should
// check the type of the expression when necessarily.
// 
// The tree top is labeled "operator".  "operand" attribute of the root indicates
// the number of operands it takes; thus either binary or unary.
// "value" attribute specifies the kind of operator.  "type" of these relational
// expressions are all boolean.  If the operator is a unary operator, it will
// have a single subtree for the expression on the right hand side. 
// When the operator is binary, it will have two subtrees, each for left and
// right hand side expressions.
// 
//==================================================================================
logical_expr :
logical_expr EQV_OP logical_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_NXOR ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// -sign means negate.  the sign is actually undetermined.

  DOMElement * lhs    = gSpkExpTree->createElement( X( STR_LHS ) );
  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
logical_expr NEQV_OP logical_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_XOR ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// -sign means negate.  the sign is actually undetermined.

  DOMElement * lhs    = gSpkExpTree->createElement( X( STR_LHS ) );
  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
logical_expr OR_OP logical_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_OR ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// -sign means negate.  the sign is actually undetermined.

  DOMElement * lhs = gSpkExpTree->createElement( X( STR_LHS ) );
  DOMElement * rhs = gSpkExpTree->createElement( X( STR_RHS ) );
  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
logical_expr AND_OP logical_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_BINARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_AND ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( Symbol::C_UNKNOWN ) );// -sign means negate.  the sign is actually undetermined.

  DOMElement * lhs   = gSpkExpTree->createElement( X( STR_LHS ) );
  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  lhs->appendChild( $1->node );
  rhs->appendChild( $3->node );
  op->appendChild( lhs );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $1;
  carrier->node = op;
  $$ = carrier;
}
|
NOT_OP logical_expr {

  DOMElement * op = gSpkExpTree->createElement( X( STR_OPERATOR ) );
  op->setAttribute( X( STR_OPERAND ), X( STR_UNARY ) );
  op->setAttribute( X( STR_VALUE ), X( STR_NEGATE ) );
  op->setAttribute( X( STR_TYPE ), X( Symbol::C_BOOL ) );
  op->setAttribute( X( STR_SIGN ), X( STR_MINUS ) );// -sign means negate.

  DOMElement * rhs   = gSpkExpTree->createElement( X( STR_RHS ) );
  rhs->appendChild( $2->node );
  op->appendChild( rhs );

  struct ExpNodeCarrier * carrier = $2;
  carrier->node = op;
  $$ = carrier;
}
|
relational_expr {
  $$ = $1;
}
;
%%
// Callers are defined outside of this file.
