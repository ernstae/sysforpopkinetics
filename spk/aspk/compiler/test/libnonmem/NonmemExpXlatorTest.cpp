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
#include "../../libcommon/ExpTreeGenerator.h"

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
extern ExpTreeGenerator * gSpkExpTreeGenerator;
extern SymbolTable * gSpkExpSymbolTable;
extern DOMDocument * gSpkExpTree;

void NonmemExpXlatorTest::setUp()
{
  yydebug = 0;

  // Populate the symbol table with pre-defined symbols.
  //
  file = fopen( "exp.in", "r" );
  if( !file )
    {
      fprintf( stderr, "Failed to open %s!\n", "exp.in" );
      CPPUNIT_ASSERT_MESSAGE( "???", false );
    }
  yyin = file;
  assert( yyin != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpTreeGenerator = new ExpTreeGenerator;
  gSpkExpTree = gSpkExpTreeGenerator->getRoot();
  gSpkExpSymbolTable = new SymbolTable( client::NONMEM );
}
void NonmemExpXlatorTest::tearDown()
{
  delete gSpkExpSymbolTable;
  delete gSpkExpTreeGenerator;
}
void NonmemExpXlatorTest::testParse()
{
  Symbol dose( "DOSE", Symbol::VECTOR, Symbol::DOUBLE, false );
  gSpkExpSymbolTable->insert( dose );
  Symbol w( "W", Symbol::VECTOR, Symbol::DOUBLE, false );
  gSpkExpSymbolTable->insert( w );
  Symbol wt( "WT", Symbol::VECTOR, Symbol::DOUBLE, false );
  gSpkExpSymbolTable->insert( wt );
  Symbol time( "TIME", Symbol::VECTOR, Symbol::DOUBLE, false );
  gSpkExpSymbolTable->insert( time );
  Symbol ds( "DS", Symbol::VECTOR, Symbol::DOUBLE, false );
  gSpkExpSymbolTable->insert( ds );

  yyparse();

  fclose( file );
  if( gSpkExpErrors == 0 )
  {
    //  gSpkExpTreeGenerator->printToStdout( );
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
  

}
CppUnit::Test * NonmemExpXlatorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "NonmemExpXlatorTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<NonmemExpXlatorTest>("testParse",
						    &NonmemExpXlatorTest::testParse ) );

   return suiteOfTests;
}

