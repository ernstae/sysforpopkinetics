/**
 * @file nmtranToCppTest.cpp
 *
 * A test driver for the lexical analyzer and syntax analyzer for NONMEM abbriviated code
 */
#include <iostream>
#include <cmath>
#include <exception>
#include <string>

// Symbol table stuff
// DOM related (DOM framework is used to create a forest of expression trees)
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/dom/DOMDocumentTraversal.hpp>
#include <xercesc/dom/DOMNodeFilter.hpp>

#include "SymbolTable.h"
#include "ParseTree.h"

using namespace std;
using namespace xercesc;


  extern "C"{
    int yyparse(void);
  };
  extern int yydebug;
  extern FILE *yyin;
  
extern int lines;

/**
 * A global counter to keep track of # of errors detected during a call to yyparse().
 *
 * This counter is declared and initialized in a file that specifies and defines yyparse().
 * The version of yyparse() for analyzing NONMEM abbriviated code
 * is specified in @c SpkCompier/NonmemAbbToC++/nmabb.y.
 *
 * \note The documentation for the yyparse specification found in @c nmabb.y 
 * is not visible because DOXYGEN does not support YACC.
 */
extern int errors;

/**
 * A global pointer to a ParseTree object (providing a set of utilities re. DOM-based tree).
 *
 * The ParseTree object pointed by this pointer is used to create and initialze DOM-based
 * parse tree built during the syntax recoginition process.  The class provides
 * means to properly allocate and release resources required to generate
 * tree and its components as well as other utilities to print out the tree contents
 * to a file or standard output and possibly more.
 */
ParseTree *util;

/**
 * A global pointer to a DOM document (tree).
 *
 * The DOMDocument object pointed by this pointer is created within the util (ParseTree) object.
 * This is used to call tools like DOMDocument::createElement() to directly create DOM elements.
 *
 * \todo Perhaps this pointer should not be exposed or should be accessed always though the
 * global pointer (util) to the ParseTree object.
 */
DOMDocument *doc;

/**
 * A global pointer to the symbol table.
 *
 * This table is used to bookkeep symbols found in the NONMEM-like control files.
 * These symbols include both keywords (symbols predefined/reserved by the NMTRAN)
 * and arbitary symbols defined by the end user.
 */
SymbolTable *table;

int main( int argc, const char* argv[] )
{
  yydebug = 0;

  if( argc > 1 )
    yydebug = atoi( argv[1] );

  //
  // Populate the symbol table with pre-defined symbols.
  //
  FILE * file;
  if( argc > 2 )
    {
      file = fopen( argv[2], "r" );
      if( !file ) {
        fprintf( stderr, "Failed to open %s\n", argv[2] );
	delete table;
	return -1;
      }
      yyin = file;
    }

  util = new ParseTree;
  doc  = util->handler();

  errors = 0;
  lines  = 0;
  table = new SymbolTable( client::NONMEM );

  yyparse();

  fclose( file );
  if( errors == 0 )
  {
    util->printToStdout( );
  }
  else
    {    
      cerr << "!!! Compilation failed (" << errors << ") !!! " << endl;
    }

  cout << endl;
  table->dump();

  cout << endl << "Read " << lines << " lines of code from " << argv[2] << endl;
  cout << "Encountered " << errors << " errors." << endl;
  
  delete table;
  delete util;
  XMLPlatformUtils::Terminate();

  return 0;
}
