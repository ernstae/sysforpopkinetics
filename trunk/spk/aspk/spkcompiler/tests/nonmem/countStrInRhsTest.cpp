#include <iostream>
#include <string>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include "../../spkcompiler/nonmem/countStrInRhs.h"
#include "countStrInRhsTest.h"

using namespace std;
using namespace CppUnit;



void countStrInRhsTest::setUp()
{
}

void countStrInRhsTest::tearDown()
{
}

void countStrInRhsTest::testSimple()
{
   char str1[] = "x = A;\n";
   CPPUNIT_ASSERT_EQUAL( 1, countStrInRhs( "A", str1 ) );
                                                                                
   char str2[] = "x = A;\ny=A;\n";
   CPPUNIT_ASSERT_EQUAL( 2, countStrInRhs( "A", str2 ) );
                                                                                
   char str3[] = "x = A;\ny=A;\nA=A;";
   CPPUNIT_ASSERT_EQUAL( 3, countStrInRhs( "A", str3 ) );
                                                                                
   char str4[] = "x = A;\ny=B;\nA=A;";
   CPPUNIT_ASSERT_EQUAL( 2, countStrInRhs( "A", str4 ) );
                                                                                
   char str5[] = "A[1] = x;\nB=y;\nA[2]=A[1];";
   CPPUNIT_ASSERT_EQUAL( 1, countStrInRhs( "A", str5 ) );

   char str6[] = "CL=THETA(1)*EXP(ETA(1))\nV=THETA(2)\nK=CL/V\nS1=V\n";
   CPPUNIT_ASSERT_EQUAL( 0, countStrInRhs( "T", str6 ) );
}

CppUnit::Test * countStrInRhsTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "countStrInRhsTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<countStrInRhsTest>
			 ("testMix", &countStrInRhsTest::testSimple ) );

 return suiteOfTests;
}

