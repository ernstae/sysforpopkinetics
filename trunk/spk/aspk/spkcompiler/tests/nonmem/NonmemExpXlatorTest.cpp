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

#include "SymbolTable.h"
#include "ExpTreeGenerator.h"
#include "explang.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

extern "C"{
  int yylex(void);  
  int yyparse(void);
};
extern int                gSpkExpLines;
extern int                gSpkExpErrors;
extern SymbolTable      * gSpkExpSymbolTable;
extern DOMDocument      * gSpkExpTree;
extern FILE             * yyin;
extern int                yydebug;


void NonmemExpXlatorTest::setUp()
{
}
void NonmemExpXlatorTest::tearDown()
{
  delete gSpkExpSymbolTable;
}
void NonmemExpXlatorTest::testSimplest()
{
  yydebug = 0;

  char errmess[128];

  // Populate the symbol table with pre-defined symbols.
  //
  char input[] = "simplest.in";
  file = fopen( input, "r" );

  
  sprintf( errmess, "Failed to open %s!", input );
  CPPUNIT_ASSERT_MESSAGE( errmess, file != NULL );
  yyin = file;
  CPPUNIT_ASSERT( yyin != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpTree = expTreeUtil.createTree("unit");
  gSpkExpSymbolTable = new SymbolTable;
  CPPUNIT_ASSERT( gSpkExpSymbolTable != NULL );

  yyparse();

  fclose( file );
  sprintf( errmess, "Syntax error found (%d)!", gSpkExpErrors );
  CPPUNIT_ASSERT_MESSAGE( errmess, gSpkExpErrors==0 );
  if( gSpkExpErrors == 0 )
  {
    //expTreeUtil.printToStdout( gSpkExpTree );
  }

  //cout << endl;
  //gSpkExpSymbolTable->dump();
}
void NonmemExpXlatorTest::testParse()
{
  yydebug = 0;

  char errmess[128];

  // Populate the symbol table with pre-defined symbols.
  //
  char input[] = "exp.in";
  file = fopen( input, "r" );

  sprintf( errmess, "Failed to open %s!\n", input );
  CPPUNIT_ASSERT_MESSAGE( errmess, input != NULL );
  yyin = file;
  CPPUNIT_ASSERT( yyin != NULL );

  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpTree = expTreeUtil.createTree("unit");
  gSpkExpSymbolTable = new SymbolTable;
  
  //
  // NONMEM keywords
  //
  Symbol theta( "THETA", Symbol::VECTOR, Symbol::DOUBLE, true );
  theta.size( 3 );
  gSpkExpSymbolTable->insert( theta );

  Symbol sigma( "SIGMA", Symbol::MATRIX, Symbol::DOUBLE, true );
  sigma.dim( 2, 2 );
  gSpkExpSymbolTable->insert( sigma );

  Symbol omega( "OMEGA", Symbol::MATRIX, Symbol::DOUBLE, true );
  omega.dim( 3, 3 );
  gSpkExpSymbolTable->insert( omega );

  Symbol eta( "ETA", Symbol::VECTOR, Symbol::DOUBLE, true );
  eta.size( 5 );
  gSpkExpSymbolTable->insert( eta );

  // 
  // As if these are from data file.
  //
  const int nMeasurements = 3;
  Symbol dose( "DOSE", Symbol::VECTOR, Symbol::DOUBLE, false );
  dose.size( nMeasurements );
  gSpkExpSymbolTable->insert( dose );

  Symbol wt( "WT", Symbol::VECTOR, Symbol::DOUBLE, false );
  wt.size( nMeasurements );
  gSpkExpSymbolTable->insert( wt );

  Symbol time( "TIME", Symbol::VECTOR, Symbol::DOUBLE, false );
  time.size( nMeasurements );
  gSpkExpSymbolTable->insert( time );

  Symbol ds( "DS", Symbol::VECTOR, Symbol::DOUBLE, false );
  ds.size( nMeasurements );
  gSpkExpSymbolTable->insert( ds );

  yyparse();

  fclose( file );
  sprintf( errmess, "Syntax error found (%d)!", gSpkExpErrors );
  CPPUNIT_ASSERT_MESSAGE( errmess, gSpkExpErrors==0 );
  if( gSpkExpErrors == 0 )
  {
    //expTreeUtil.printToStdout( gSpkExpTree );
  }

  //cout << endl;
  //gSpkExpSymbolTable->dump();

}
CppUnit::Test * NonmemExpXlatorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "NonmemExpXlatorTest" );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemExpXlatorTest>(
         "testSimplest", 
	 &NonmemExpXlatorTest::testSimplest ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemExpXlatorTest>(
         "testParse",
	 &NonmemExpXlatorTest::testParse ) );

   return suiteOfTests;
}

