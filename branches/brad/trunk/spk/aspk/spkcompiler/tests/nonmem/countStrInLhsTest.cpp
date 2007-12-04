#include <iostream>
#include <string>
#include <cassert>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include "../../spkcompiler/nonmem/countStrInLhs.h"
#include "countStrInLhsTest.h"

using namespace std;
using namespace CppUnit;

void countStrInLhsTest::setUp()
{
}

void countStrInLhsTest::tearDown()
{
}

void countStrInLhsTest::testSimple()
{
   char str1[] = "A = x;\n";
   assert( countStrInLhs( "A", str1 ) == 1 );
                                                                                
   char str2[] = "A = x;\nA=y;\n";
   assert( countStrInLhs( "A", str2 ) == 2 );
                                                                                
   char str3[] = "A = x;\nA=y;\nA=z;";
   assert( countStrInLhs( "A", str3 ) == 3 );
                                                                                
   char str4[] = "A = x;\nB=y;\nA=A + A;";
   assert( countStrInLhs( "A", str4 ) == 2 );
                                                                                
   char str5[] = "A[1] = x;\nB=y;\nA[2]=A[1];";
   assert( countStrInLhs( "A", str5 ) == 2 );

}

CppUnit::Test * countStrInLhsTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "countStrInLhsTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<countStrInLhsTest>
			 ("testSimple", &countStrInLhsTest::testSimple ) );

 return suiteOfTests;
}

