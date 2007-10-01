#include <iostream>
#include <string>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include "../../spkcompiler/series.h"
#include "seriesTest.h"

using namespace std;
using namespace CppUnit;



void seriesTest::setUp()
{
}

void seriesTest::tearDown()
{
}

void seriesTest::testNonzero()
{
  unsigned int n = 10;
  CPPUNIT_ASSERT_EQUAL( 55, series(1,1,10) );
}
void seriesTest::testZero()
{
  CPPUNIT_ASSERT_EQUAL( 0, series(1, 1, 0) );
}

CppUnit::Test * seriesTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "seriesTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<seriesTest>
			 ("testNonzero", &seriesTest::testNonzero ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<seriesTest>
			 ("testZero", &seriesTest::testZero ) );

 return suiteOfTests;
}

