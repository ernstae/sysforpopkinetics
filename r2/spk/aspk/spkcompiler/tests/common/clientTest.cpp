#include <iostream>
#include <string>
#include <map>
#include <valarray>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include "../../spkcompiler/client.h"
#include "clientTest.h"

using namespace std;
using namespace CppUnit;



void clientTest::setUp()
{
}

void clientTest::tearDown()
{
}

void clientTest::testEnumulator()
{
  //
  // NONMEM and NOT_SUPPORTED should be the only valid values
  // currently (09/10/03) registered.
  //
  // If for some reason, these values are removed from the 
  // enum list, compilation, not the test itself, will fail.
  //
  enum client::type nonmem = client::NONMEM;
  enum client::type not_supported = client::NOT_SUPPORTED;

  //
  // Actual strings associated with the enum values do not matter,
  // but what matters is they are unique from each other.
  //
  CPPUNIT_ASSERT( strcmp( client::toString( nonmem ), client::toString( not_supported ) ) != 0 );
}


CppUnit::Test * clientTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "clientTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<clientTest>
			 ("test", &clientTest::testEnumulator ) );

 return suiteOfTests;
}

