#include <iostream>
#include <string>
#include <map>
#include <valarray>
#include <vector>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include <spkcompiler/SpkMLToCpp.h>
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
  vector<string> filenames = nonmem2cpp.getFilenameList();
  for ( int i=0; i<filenames.size(); i++ )
    cout << filenames[i] << endl;
}


CppUnit::Test * SpkMLToCppTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "SpkMLToCppTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<SpkMLToCppTest>
			 ("test", &SpkMLToCppTest::test ) );

 return suiteOfTests;
}

