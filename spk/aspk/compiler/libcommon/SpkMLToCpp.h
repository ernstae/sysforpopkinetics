#ifndef SPKMLTOCPP_H
#define SPKMLTOCPP_H

#include <vector>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include "client.h"
#include "SymbolTable.h"
#include "ExpTreeGenerator.h"
#include "../libnonmem/explang.tab.h"

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
 * ClientTranslator class defines the interfaces
 * to be implemented by sublasses specific to clients (ex. NONMEM, SAAM2...).
 *
 * These interfaces include a method to initiate the process of reading an
 * SpkML document and convert it to C++ source code files,
 * ways to acquire the names of these C++ files created and so on.
 */
class ClientTranslator
{
 public:
  /**
   * Collect and assemble information mined in the tree necessarily
   * to generate a SPK driver file and an SpkModel definition
   * files.
   *
   * When process is completed successfully, the pointer returned 
   * by getDriverFilename() shall point to a character array containing
   * a path followed by a filename indicating the C++ source
   * code file for a SPK driver.  In addition, the vector returned
   * by getModelFilenameList() shall contain character arrays, each
   * specifies a path followed by a filename indicating the C++ 
   * source code file containing a portion of SpkModel subclass
   * definition.
   * 
   * When process is completed successfully, two sets of information are
   * expected, but not required, to be assembled: A data set returned by
   * getSpkParameters() and a data set returned by getClientParameters().
   * The first set is composed in the form of FitParameters data structure.
   * It has place holders like "popParIn" for the initial value for
   * the population parameter.  The second set is in an arbitrary form (void*).
   * It can contain any value in any form with any names for convenience.
   * This set will be primarily used for debugging purpose.
   * 
   * @param tree is the DOMDocument-based tree representation
   * of an SpkML input document.
   */
  virtual void translate ( xercesc::DOMDocument * tree ) = 0;

  /**
   * @return a FitParameters data structured object that may or may
   * not have valid values associated with its declared variables.
   */
  virtual const struct FitParameters * getSpkParameters() const = 0;

  /**
   * @return an arbitrary object that may or may not contain 
   * a set of information gathered during the translation process
   * and specific to the client.
   */
  virtual const void * getClientParameters() const = 0;

  /**
   * @return a character array containing a path followed by the
   * filename associated with the C++ source code file definining
   * a SPK driver.
   */
  virtual const char * getDriverFilename() const = 0;

  /*
   *
   * @return a vector of character arrays contaings paths followed by
   * the filenames associated with the C++ source code files defining
   * a SpkModel subclass.
   */
  virtual const std::vector< const char * > getModelFilenameList() const = 0;
};

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
  const struct FitParameters * getSpkParameters() const;
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

