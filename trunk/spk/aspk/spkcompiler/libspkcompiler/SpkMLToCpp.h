#ifndef SPKMLTOCPP_H
#define SPKMLTOCPP_H

#include <vector>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include "SpkParameters.h"
#include "ClientTranslator.h"
#include "client.h"
#include "SymbolTable.h"
#include "ExpTreeGenerator.h"
#include "explang.h"

/**
 * @file SpkMLToCpp.h
 * 
 * This header declares the base SpkML->C++ translator class and
 * extern-s the global variables that serve as the communication
 * channel between the SpkML->C++ translator and 
 * the expression language parser (and lexical analyizer).
*/
extern "C"{
  int yylex(void);  
  int yyparse(void);
};
extern int yydebug;
extern FILE *yyin;
  
/**
 * A global counter used by yylex() to count the number of lines it
 * has read so far.
 *
 * This counter is declared in a file that defines yylex().
 * yylex() only increments (hence, it does not initialize)
 * the counter from the value set prior to the call.
 * It will probably most make sense if the caller of yyparse()
 * initialize this counter just before a call to yyparse() which
 * in turn calls yylex() repetively until all contents in a file
 * are read.  One example of situations where the counter is
 * initialized to a value greater than zero is where
 * the file contains expressions extracted from another file
 * and the first expression appeared somewhere in the middle
 * of the original file.
 */
extern int gSpkExpLines;


/**
 * A global counter used by yylex() and yyparse() to keep track of
 * the number of syntax and semantics errors detected during the process.
 *
 * This counter is declared in a file that defines yyparse().
 * yylex() and yyparse() only increment the counter from the value
 * set prior to the call to yyparse() which in turn calls yylex() repectively.
 * It will probably most make sense if the caller of yyparse()
 * initialize this counter just before a call to yyparse()
 */
extern int gSpkExpErrors;

/**
 * A global pointer to the symbol table used by yyparse().
 *
 * This pointer is declared in a file that defines yyparse().
 * yyparse() inserts new user-defined variables discovered 
 * in a given set of expressions that are not found in the
 * table prior to the call.  In other words, yyparse()
 * does not initialize or allocate memory and
 * only adds new variables.  yyparse() also does not 
 * disturb the information about the variables already
 * found in the table prior to the call.
 *
 */
extern SymbolTable *gSpkExpSymbolTable;

/**
 * A global pointer to a DOM based parse tree used by yyparse().
 *
 * This pointer is declared in a file that defines yyparse().
 * yyparse() only inserts nodes as it parses the set of 
 * expressions.  In other words, yyparse() does not 
 * allocate memory and only inserts new nodes below the
 * root node pointed by the pointer set prior to the call.
 *
 */
extern xercesc::DOMDocument *gSpkExpTree;

/**
 * SpkMLToCpp class packages the interfaces to the 
 * SpkML document -> C++ source code translation
 * related processes.
 * 
 */
class SpkMLToCpp
{
 public:
  SpkMLToCpp( const char* inputSpkpMLIn );
  ~SpkMLToCpp();

  void translate();
  const struct SpkParameters * getSpkParameters() const;
  const void * getClientParameters() const;

 protected:

  SpkMLToCpp();
  SpkMLToCpp( const SpkMLToCpp& right );
  const SpkMLToCpp& operator=( const SpkMLToCpp& right );

 private:
  void                        initializeDOM     () const;
  void                        terminateDOM      () const;
  enum client::type           discoverClient    ( const xercesc::DOMDocument* tree ) const;
  xercesc::DOMDocument      * buildTreeFromSpkML( const char * inputSpkMLIn );
  ClientTranslator          * createTranslator  ( enum client::type ) const;

  const char                * inputSpkML;
  enum client::type           who;
  ClientTranslator          * client_translator;
  xercesc::XercesDOMParser  * parser;
  xercesc::DOMDocument      * tree;
};

#endif

