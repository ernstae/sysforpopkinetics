#include <iostream>
#include <string>
#include <map>
#include <valarray>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include "SpkMLToCpp.h"
#include "SpkMLToCppTest.h"

using namespace std;
using namespace CppUnit;

void SpkMLToCppTest::setUp()
{
}

void SpkMLToCppTest::tearDown()
{
}

void SpkMLToCppTest::test()
{
  SpkMLToCpp nonmem2cpp( "SpkMLToCppTestInput.xml" );
  nonmem2cpp.translate();
}


CppUnit::Test * SpkMLToCppTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "SpkMLToCppTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<SpkMLToCppTest>
			 ("test", &SpkMLToCppTest::test ) );

 return suiteOfTests;
}

