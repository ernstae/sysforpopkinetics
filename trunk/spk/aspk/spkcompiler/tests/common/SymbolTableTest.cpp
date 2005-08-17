#include <iostream>
#include <string>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "../../spkcompiler/SymbolTable.h"
#include "SymbolTableTest.h"

using namespace std;
using namespace CppUnit;


void SymbolTableTest::setUp()
{
   datalabel  = Symbol::DATALABEL;
   nonmem     = Symbol::PREDEFINED;
   userdef    = Symbol::USERDEFINED;
   scalar     = Symbol::SCALAR;
   vec        = Symbol::VECTOR;
   matrix     = Symbol::MATRIX;
   full       = Symbol::FULL;
   diagonal   = Symbol::DIAGONAL;
   triangle   = Symbol::TRIANGLE;
   user       = Symbol::USER;
   system     = Symbol::SYSTEM;
   readonly   = Symbol::READONLY;
   readwrite  = Symbol::READWRITE;
}
void SymbolTableTest::tearDown()
{
}
void SymbolTableTest::testEnd()
{
  SymbolTable table;
  CPPUNIT_ASSERT( Symbol::empty() == Symbol::empty() );
}
void SymbolTableTest::testInsertUserVar()
{
   string empty_string( "" ); 
   SymbolTable table;
   string aaa_name( "aaa" );
   vector<int> aaa_dim( 1 );
   aaa_dim[0] = 1;
   Symbol *aaa = table.insertScalar( aaa_name, user, readwrite );
   CPPUNIT_ASSERT( table.find( aaa_name ) != Symbol::empty() );
   CPPUNIT_ASSERT( table.find( aaa_name ) == aaa );

   aaa->initial[0][0] = "3.0";
   CPPUNIT_ASSERT( "3.0" ==  aaa->initial[0][0] );
}
void SymbolTableTest::testInsertLabel()
{
   string empty_string( "" ); 
   SymbolTable table;
   string aaa_name( "aaa" );
   string aaa_alias( "AAA" );
   valarray<int> aaa_dim( 2 );
   aaa_dim[0] = 2;
   aaa_dim[1] = 1;
   Symbol *aaa = table.insertLabel( aaa_name, aaa_alias, aaa_dim );
   CPPUNIT_ASSERT( table.find( aaa_name ) != Symbol::empty() );
   CPPUNIT_ASSERT( table.find( aaa_name ) == aaa );

   aaa->initial[0][0] = "3.0";
   CPPUNIT_ASSERT( "3.0" == aaa->initial[0][0] );

}
void SymbolTableTest::testInsertNMVector()
{
   string empty_string( "" ); 
   SymbolTable table;
   string aaa_name( "aaa" );
   int aaa_dim = 2;
   Symbol *aaa = table.insertVector( aaa_name, aaa_dim, system, readonly );
   CPPUNIT_ASSERT( table.find( aaa_name ) != Symbol::empty() );
   CPPUNIT_ASSERT( table.find( aaa_name ) == aaa );

   aaa->initial[0][0] = "3.0";
   CPPUNIT_ASSERT( "3.0" == aaa->initial[0][0] );
}
void SymbolTableTest::testInsertNMMatrix()
{
   string empty_string( "" ); 
   SymbolTable table;
   string aaa_name( "aaa" );
   int aaa_dim = 2;
   Symbol *aaa = table.insertMatrix( aaa_name, diagonal, aaa_dim, system, readonly );
   CPPUNIT_ASSERT( table.find( aaa_name ) != Symbol::empty() );
   CPPUNIT_ASSERT( table.find( aaa_name ) == aaa );

   string bbb_name( "bbb" );
   int bbb_dim = 2;
   Symbol *bbb = table.insertMatrix( bbb_name, triangle, bbb_dim, system, readonly );
   CPPUNIT_ASSERT( table.find( bbb_name ) != Symbol::empty() );
   CPPUNIT_ASSERT( table.find( bbb_name ) == bbb );

   aaa->initial[0][0] = "3.0";
   CPPUNIT_ASSERT( "3.0" == aaa->initial[0][0] );

   bbb->initial[0][0] = "4.0";
   CPPUNIT_ASSERT( "4.0" == bbb->initial[0][0] );
}

CppUnit::Test * SymbolTableTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "SymbolTableTest" );

  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testEnd",
						    &SymbolTableTest::testEnd ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testInsertNMMatrix",
						    &SymbolTableTest::testInsertNMMatrix ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testInsertNMVector",
						    &SymbolTableTest::testInsertNMVector ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testInsertUserVar",
						    &SymbolTableTest::testInsertUserVar ) );

  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testInsertLabel",
						    &SymbolTableTest::testInsertLabel ) );

  return suiteOfTests;
}
