#include <iostream>
#include "SymbolTable.h"

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "SymbolTable.h"
#include "SymbolTableTest.h"
#include "client.h"

using namespace std;
using namespace CppUnit;

int errors = 0;

void SymbolTableTest::setUp()
{
  table = new SymbolTable( client::NONMEM );
}
void SymbolTableTest::tearDown()
{
  delete table; 
}
void SymbolTableTest::testRegister() {
 
  CPPUNIT_ASSERT_MESSAGE( "Failed to insert a name.",  table->insert( "theta" ) != NULL );
  CPPUNIT_ASSERT_MESSAGE( "Failed to find a successfully inserted name.", table->find( "theta" ) != NULL ); 

  CPPUNIT_ASSERT_MESSAGE( "Failed to insert a name.",  table->insert( "Sachiko" ) != NULL );
  CPPUNIT_ASSERT_MESSAGE( "Failed to find a successfully inserted name.", table->find( "Sachiko" ) != NULL ); 
}
void SymbolTableTest::testDefine() {
  CPPUNIT_ASSERT_MESSAGE( "Failed to insert a name.",  table->insert( "Sachiko" ) != NULL );
  Symbol * const s = table->find( "Sachiko" );
  CPPUNIT_ASSERT_MESSAGE( "Failed to find a successfully inserted name.", table->find( "Sachiko" ) != NULL );
  s->dataType( Symbol::DOUBLE );
  s->objectType( Symbol::SCALAR );
  CPPUNIT_ASSERT_MESSAGE( "The variable should be a scalar.", 
	    table->find("Sachiko")->objectType() == Symbol::SCALAR ); 
  CPPUNIT_ASSERT_MESSAGE( "The variable should be have type \'double\'.", 
	    table->find("Sachiko")->dataType() == Symbol::DOUBLE );
  CPPUNIT_ASSERT_MESSAGE( "The variable should be have 1 by 1 dimensions.", 
	    table->find("Sachiko")->dim().first == 1
	    && table->find("Sachiko")->dim().second == 1 );

  CPPUNIT_ASSERT_MESSAGE( "Failed to insert a name.",  table->insert( "Noriko" ) != NULL );
  Symbol * const ss = table->find( "Noriko" );
  CPPUNIT_ASSERT_MESSAGE( "Failed to find a successfully inserted name.", table->find( "Noriko" ) != NULL );
  ss->dataType( Symbol::INT );
  ss->objectType( Symbol::SCALAR );
  CPPUNIT_ASSERT_MESSAGE( "The variable should be a scalar.", 
	    table->find("Noriko")->objectType() == Symbol::SCALAR ); 
  CPPUNIT_ASSERT_MESSAGE( "The variable should be have type \'integer\'.", 
	    table->find("Noriko")->dataType() == Symbol::INT );
  CPPUNIT_ASSERT_MESSAGE( "The variable should be have 1 by 1 dimensions.", 
	    table->find("Noriko")->dim().first == 1
	    && table->find("Noriko")->dim().second == 1 );
}
void SymbolTableTest::testReRegister() 
{
  CPPUNIT_ASSERT_MESSAGE( "Failed to insert a name.",  table->insert( "Sachiko" ) != NULL );
  CPPUNIT_ASSERT_MESSAGE( "The name should have been registered already.",  table->insert( "Sachiko" ) == NULL );
}

CppUnit::Test * SymbolTableTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "SymbolTableTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testRegister",
						    &SymbolTableTest::testRegister ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testDefine",
						    &SymbolTableTest::testDefine ) );

  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTableTest>("testReRegister",
						    &SymbolTableTest::testReRegister ) );

  return suiteOfTests;
}
