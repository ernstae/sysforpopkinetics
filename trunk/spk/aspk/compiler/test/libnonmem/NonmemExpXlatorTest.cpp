#include <iostream>
#include <valarray>

#include "NonmemExpXlatorTest.h"

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include "../../libcommon/SymbolTable.h"
#include "../../libcommon/ParseTree.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

  extern "C"{
    int yyparse(void);
  };
  extern int yydebug;
  extern FILE *yyin;
  
extern int gSpkExpLines;
extern int gSpkExpErrors;
extern ParseTree * gSpkExpUtil;
extern SymbolTable * gSpkExpSymbolTable;
extern DOMDocument * gSpkExpDOMDocument;

void NonmemExpXlatorTest::setUp()
{
}
void NonmemExpXlatorTest::tearDown()
{
}
void NonmemExpXlatorTest::testParse()
{
  yydebug = 1;

  // Populate the symbol table with pre-defined symbols.
  //
  FILE * file = fopen( "exp.in", "r" );
  if( !file )
    {
      fprintf( stderr, "Failed to open %s!\n", "exp.in" );
      delete gSpkExpSymbolTable;
      CPPUNIT_ASSERT_MESSAGE( "???", false );
    }
  yyin = file;
  assert( yyin != NULL );

  //  gSpkExpDOMDocument = util->handler();

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpUtil   = new ParseTree;
  gSpkExpDOMDocument = gSpkExpUtil->handler();
  gSpkExpSymbolTable = new SymbolTable( client::NONMEM );

  yyparse();

  fclose( file );
  if( gSpkExpErrors == 0 )
  {
    gSpkExpUtil->printToStdout( );
  }
  else
    {    
      cerr << "!!! Compilation failed (" << gSpkExpErrors << ") !!! " << endl;
    }

  cout << endl;
  gSpkExpSymbolTable->dump();

  cout << endl;
  cout << "Read " << gSpkExpLines << " lines of code from " << "exp.in" << endl;
  cout << "Encountered " << gSpkExpErrors << " errors." << endl;
  
  delete gSpkExpSymbolTable;
  delete gSpkExpUtil;
  XMLPlatformUtils::Terminate();

}
CppUnit::Test * NonmemExpXlatorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "NonmemExpXlatorTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<NonmemExpXlatorTest>("testParse",
						    &NonmemExpXlatorTest::testParse ) );

   return suiteOfTests;
}

