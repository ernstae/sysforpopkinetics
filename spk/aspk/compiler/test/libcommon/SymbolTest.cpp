#include <iostream>
#include <string>
#include <vector>
#include "SymbolTable.h"

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "Symbol.h"
#include "SymbolTest.h"

using namespace std;
using namespace CppUnit;

int gSpkExpErrors = 0;

void SymbolTest::setUp()
{
}

void SymbolTest::tearDown()
{
}

void SymbolTest::testCreate()
{
  Symbol s1( "Sachiko" );
  CPPUNIT_ASSERT_MESSAGE( "The given name and the returned name were different", "Sachiko" == s1.name() );
  enum Symbol::SYMBOLTYPE objectType = s1.objectType();
  enum Symbol::SYMBOLTYPE dataType   = s1.dataType();
  int size                           = s1.size();
  pair<int,int> dimensions           = s1.dim();
  CPPUNIT_ASSERT_MESSAGE( "The default object type shall not be SCALAR.",      Symbol::SCALAR != objectType );
  CPPUNIT_ASSERT_MESSAGE( "The default object type shall not be DOUBLE.",      Symbol::DOUBLE != objectType );
  CPPUNIT_ASSERT_MESSAGE( "The default object type shall not be BOOL.",        Symbol::BOOL   != objectType );
  CPPUNIT_ASSERT_MESSAGE( "The default size shall not be a fair number.",      size < 1 );
  CPPUNIT_ASSERT_MESSAGE( "The default dimensions shall not be fair numbers.", dimensions.first < 1 );
  CPPUNIT_ASSERT_MESSAGE( "The default dimensions shall not be fair numbers.", dimensions.second < 1 );
}

void SymbolTest::testDefineScalar()
{
  Symbol s1( "Sachiko" );
  CPPUNIT_ASSERT_MESSAGE( "The given name and the returned name were different", "Sachiko" == s1.name() );
  s1.objectType( Symbol::SCALAR );
  s1.dataType( Symbol::DOUBLE );
  
  enum Symbol::SYMBOLTYPE objectType = s1.objectType();
  enum Symbol::SYMBOLTYPE dataType   = s1.dataType();
  int size                           = s1.size();
  pair<int,int> dimensions           = s1.dim();
  CPPUNIT_ASSERT_MESSAGE( "The object type shall be SCALAR.", Symbol::SCALAR    == objectType );
  CPPUNIT_ASSERT_MESSAGE( "The data type shall be SCALAR.",   Symbol::DOUBLE    == dataType );
  CPPUNIT_ASSERT_MESSAGE( "The size shall be 1.",             size              == 1 );
  CPPUNIT_ASSERT_MESSAGE( "The dimensions shall be 1 by 1.",  dimensions.first  == 1 );
  CPPUNIT_ASSERT_MESSAGE( "The dimensions shall be 1 by 1.",  dimensions.second == 1 );
}

void SymbolTest::testDefineVector()
{
  Symbol s1( "Sachiko" );
  CPPUNIT_ASSERT_MESSAGE( "The given name and the returned name were different", "Sachiko" == s1.name() );
  s1.objectType( Symbol::VECTOR );
  s1.dataType( Symbol::DOUBLE );
  s1.size( 3 );
  
  enum Symbol::SYMBOLTYPE objectType = s1.objectType();
  enum Symbol::SYMBOLTYPE dataType   = s1.dataType();
  int size                           = s1.size();
  pair<int,int> dimensions           = s1.dim();
  CPPUNIT_ASSERT_MESSAGE( "The object type shall be VECTOR.", Symbol::VECTOR    == objectType );
  CPPUNIT_ASSERT_MESSAGE( "The data type shall be DOUBLE.",   Symbol::DOUBLE    == dataType );
  CPPUNIT_ASSERT_MESSAGE( "The size shall be 3.",             size              == 3 );
  CPPUNIT_ASSERT_MESSAGE( "The dimensions shall be 3 by 1.",  dimensions.first  == 3 );
  CPPUNIT_ASSERT_MESSAGE( "The dimensions shall be 3 by 1.",  dimensions.second == 1 );
}

void SymbolTest::testDefineMatrix()
{
  Symbol s1( "Sachiko" );
  CPPUNIT_ASSERT_MESSAGE( "The given name and the returned name were different", "Sachiko" == s1.name() );
  s1.objectType( Symbol::MATRIX );
  s1.dataType( Symbol::BOOL );
  s1.dim( 3, 3 );
    
  enum Symbol::SYMBOLTYPE objectType = s1.objectType();
  enum Symbol::SYMBOLTYPE dataType   = s1.dataType();
  int size                           = s1.size();
  pair<int,int> dimensions           = s1.dim();
  CPPUNIT_ASSERT_MESSAGE( "The object type shall be MATRIX", Symbol::MATRIX    == objectType );
  CPPUNIT_ASSERT_MESSAGE( "The data type shall be BOOL.",    Symbol::BOOL      == dataType );
  CPPUNIT_ASSERT_MESSAGE( "The size shall be 3.",            size              == 3 * 3 );
  CPPUNIT_ASSERT_MESSAGE( "The dimensions shall be 3 by 3.", dimensions.first  == 3 );
  CPPUNIT_ASSERT_MESSAGE( "The dimensions shall be 3 by 3.", dimensions.second == 3 );
}

void SymbolTest::testToString()
{
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toString() does not match the enum.", 
			  Symbol::toString( Symbol::MATRIX ) == Symbol::STR_MATRIX );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toString() does not match the enum.", 
			  Symbol::toString( Symbol::VECTOR ) == Symbol::STR_VECTOR );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toString() does not match the enum.", 
			  Symbol::toString( Symbol::SCALAR ) == Symbol::STR_SCALAR );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toString() does not match the enum.", 
			  Symbol::toString( Symbol::UNKNOWN ) == Symbol::STR_UNKNOWN );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toString() does not match the enum.", 
			  Symbol::toString( Symbol::DOUBLE ) == Symbol::STR_DOUBLE );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toString() does not match the enum.", 
			  Symbol::toString( Symbol::INT )    == Symbol::STR_INT );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toString() does not match the enum.", 
			  Symbol::toString( Symbol::BOOL )   == Symbol::STR_BOOL );
}
void SymbolTest::testToEnum()
{
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toEnum() does not match the string.", 
			  Symbol::toEnum( Symbol::STR_MATRIX )  == Symbol::MATRIX  );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toEnum() does not match the string.", 
			  Symbol::toEnum( Symbol::STR_VECTOR )  == Symbol::VECTOR  );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toEnum() does not match the string.", 
			  Symbol::toEnum( Symbol::STR_SCALAR )  == Symbol::SCALAR  );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toEnum() does not match the string.", 
			  Symbol::toEnum( Symbol::STR_UNKNOWN ) == Symbol::UNKNOWN  );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toEnum() does not match the string.", 
			  Symbol::toEnum( Symbol::STR_DOUBLE )  == Symbol::DOUBLE  );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toEnum() does not match the string.", 
			  Symbol::toEnum( Symbol::STR_INT )     == Symbol::INT  );
  CPPUNIT_ASSERT_MESSAGE( "The string returned by toEnum() does not match the string.", 
			  Symbol::toEnum( Symbol::STR_BOOL )    == Symbol::BOOL  );
}

CppUnit::Test * SymbolTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "SymbolTableTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testCreate",
						    &SymbolTest::testCreate ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testDefineScalar",
						    &SymbolTest::testDefineScalar ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testDefineVector",
						    &SymbolTest::testDefineVector ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testDefineMatrix",
						    &SymbolTest::testDefineMatrix ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testToString",
						    &SymbolTest::testToString ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SymbolTest>("testToEnum",
						    &SymbolTest::testToEnum ) );
 return suiteOfTests;
}

