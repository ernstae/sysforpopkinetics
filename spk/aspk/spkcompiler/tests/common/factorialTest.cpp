#include <iostream>
#include <string>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include <spkcompiler/factorial.h>
#include "factorialTest.h"

using namespace std;
using namespace CppUnit;



void factorialTest::setUp()
{
}

void factorialTest::tearDown()
{
}

void factorialTest::testNonzero()
{
  unsigned int n = 10;
  CPPUNIT_ASSERT_EQUAL( static_cast<unsigned int>( 55 ), factorial(10) );
}
void factorialTest::testZero()
{
  CPPUNIT_ASSERT_EQUAL( static_cast<unsigned int>( 0 ), factorial(0) );
}

CppUnit::Test * factorialTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "factorialTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<factorialTest>
			 ("testNonzero", &factorialTest::testNonzero ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<factorialTest>
			 ("testZero", &factorialTest::testZero ) );

 return suiteOfTests;
}

