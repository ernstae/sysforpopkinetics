#include <iostream>
#include <string>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include <spkcompiler/upper.h>
#include "upperTest.h"

using namespace std;
using namespace CppUnit;



void upperTest::setUp()
{
}

void upperTest::tearDown()
{
}

void upperTest::testMix()
{
  char * aaa[] = { "AAA", "AAa", "AaA", "Aaa", "aAA", "aAa", "aaA" };

  for( int i=0; i<7; i++ )
    {
      string AAA = upper( aaa[i] );
      CPPUNIT_ASSERT( AAA == string( "AAA" ) );
    }
 
}
void upperTest::testEmpty()
{
  CPPUNIT_ASSERT( upper("") == string("") );
}

CppUnit::Test * upperTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "upperTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<upperTest>
			 ("testMix", &upperTest::testMix ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<upperTest>
			 ("testEmpty", &upperTest::testEmpty ) );

 return suiteOfTests;
}

