#include <iostream>
#include <string>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include "../../spkcompiler/lower.h"
#include "lowerTest.h"

using namespace std;
using namespace CppUnit;



void lowerTest::setUp()
{
}

void lowerTest::tearDown()
{
}

void lowerTest::testMix()
{
  char * AAA[] = { "AAA", "AAa", "AaA", "Aaa", "aAA", "aAa", "aaA" };

  for( int i=0; i<7; i++ )
    {
      string aaa = lower( AAA[i] );
      CPPUNIT_ASSERT( aaa == string( "aaa" ) );
    }
 
}
void lowerTest::testEmpty()
{
  CPPUNIT_ASSERT( lower("") == string("") );
}

CppUnit::Test * lowerTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "lowerTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<lowerTest>
			 ("testMix", &lowerTest::testMix ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<lowerTest>
			 ("testEmpty", &lowerTest::testEmpty ) );

 return suiteOfTests;
}

