/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
/*************************************************************************
 *
 * File: SpkExceptionTest.cpp
 *
 *
 * Test cases for SpkException test
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkException.h"
#include "SpkExceptionTest.h"

using namespace std;
using namespace CppUnit;

void SpkExceptionTest::setUp()
{
    // initializations
}
void SpkExceptionTest::tearDown()
{
    // clean up
}

Test* SpkExceptionTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("SpkExceptionTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<SpkExceptionTest>(
                         "findTest", 
                         &SpkExceptionTest::findTest));
    suiteOfTests->addTest(new TestCaller<SpkExceptionTest>(
                         "defaultConstructorTest", 
                         &SpkExceptionTest::defaultConstructorTest));
    suiteOfTests->addTest(new TestCaller<SpkExceptionTest>(
                         "constructorTest", 
                         &SpkExceptionTest::constructorTest));
    suiteOfTests->addTest(new TestCaller<SpkExceptionTest>(
                         "copyConstructorTest",
                         &SpkExceptionTest::copyConstructorTest));
    suiteOfTests->addTest(new TestCaller<SpkExceptionTest>(
                         "assignmentTest", 
                         &SpkExceptionTest::assignmentTest));
    suiteOfTests->addTest(new TestCaller<SpkExceptionTest>(
                         "pushTest", 
                         &SpkExceptionTest::pushTest));
    suiteOfTests->addTest(new TestCaller<SpkExceptionTest>(
                         "catTest", 
                         &SpkExceptionTest::catTest));
    suiteOfTests->addTest(new TestCaller<SpkExceptionTest>(
                         "popTest",
                         &SpkExceptionTest::popTest));
    suiteOfTests->addTest(new TestCaller<SpkExceptionTest>(
                         "iterationTest", 
                         &SpkExceptionTest::iterationTest));
    suiteOfTests->addTest(new TestCaller<SpkExceptionTest>(
                         "serializeTest", 
                         &SpkExceptionTest::serializeTest));
    return suiteOfTests;
}

void SpkExceptionTest::defaultConstructorTest()
{
    try{
        SpkException e;
    }
    catch( ... )
    {
        CPPUNIT_ASSERT_MESSAGE( "Default constructor shall never throw.", false );
    }
}
void SpkExceptionTest::constructorTest()
{
    try{
        SpkException e( SpkError::SPK_UNKNOWN_ERR, "Constructor Test", __LINE__, __FILE__);
    }
    catch( ... )
    {
        CPPUNIT_ASSERT_MESSAGE( "Constructor shall never throw!", false );
    }
}
void SpkExceptionTest::copyConstructorTest()
{
    try{
        SpkException e1;
        SpkException e2(e1);
    }
    catch( ... )
    {
        CPPUNIT_ASSERT_MESSAGE( "Copy constructor threw an exception!.", false );
    }
}
void SpkExceptionTest::assignmentTest()
{
    try{
        SpkException e1;
        SpkException e2 = e1;
    }
    catch( ... )
    {
        CPPUNIT_ASSERT_MESSAGE( "Assignment operator threw an exception!", false);
    }
}
void SpkExceptionTest::pushTest()
{
    SpkError err(SpkError::SPK_UNKNOWN_ERR, "pushTest", __LINE__, __FILE__);
    SpkException e( err );
    try{
        e.push( err );
        e.push( err );
    }
    catch(...)
    {
        CPPUNIT_ASSERT_MESSAGE( "push() threw!", false );
    }
    CPPUNIT_ASSERT_MESSAGE("e.size()==3", e.size() == 3 );
    for( int i=3; i<SpkException::maxErrors(); i++ )
    {
        e.push(err);
    }
    CPPUNIT_ASSERT_MESSAGE( "e.full()==true", e.full() );
}
void SpkExceptionTest::catTest()
{
  int n = 4;
  SpkError e1(SpkError::SPK_UNKNOWN_ERR, "1st error ", 1, __FILE__);
  SpkError e2(SpkError::SPK_UNKNOWN_ERR, "2nd error ", 2, __FILE__);
  SpkError e3(SpkError::SPK_UNKNOWN_ERR, "3rd error ", 3, __FILE__);
  SpkError e4(SpkError::SPK_UNKNOWN_ERR, "4th error ", 4, __FILE__);
  SpkException exception1;
  exception1.push( e1 );
  exception1.push( e2 );
  SpkException exception2;
  exception2.push( e3 );
  exception2.push( e4 );
  
  try{
    exception1.cat( exception2 );
  }
  catch(...)
    {
      CPPUNIT_ASSERT_MESSAGE( "push() threw!", false );
    }

  CPPUNIT_ASSERT_EQUAL( n, (int)exception1.size() );
  for( int i=0; i<n; i++ )
    {
      CPPUNIT_ASSERT( exception1[i].linenum() == i+1 );
    }
}
void SpkExceptionTest::popTest()
{
    SpkError err(SpkError::SPK_UNKNOWN_ERR, "pushTest", __LINE__, __FILE__);
    SpkException e( err );
    try{
        e.push( err );
        e.push( err );
    }
    catch(...)
    {
        CPPUNIT_ASSERT_MESSAGE( "push() shall never throw.", false );
    }
    CPPUNIT_ASSERT_MESSAGE("e.size()==3", e.size() == 3 );
    for( int i=0; i<3; i++ )
    {
        e.pop();
    }
    CPPUNIT_ASSERT_MESSAGE("e.empty()==true", e.empty() );
}

void SpkExceptionTest::serializeTest()
{
    SpkException e1;
    SpkException e2;
    string s1, s2;
    s1 << e1;
    s2 << e2;

    CPPUNIT_ASSERT_MESSAGE( "s1==s2", s1==s2 );

    int line        = __LINE__;
    char filename[] = __FILE__;
    char message[]  = "Serialize Test";
    SpkException e3(SpkError::SPK_UNKNOWN_ERR, message, line, filename);
    SpkException e4(SpkError::SPK_UNKNOWN_ERR, message, line, filename);
    SpkException e5;

    string s3, s4;
    s3 << e3;
    s4 << e4;
    CPPUNIT_ASSERT_MESSAGE( "s3==s4", s3==s4 );
    SpkError error(SpkError::SPK_UNKNOWN_ERR, 
        "different message", line+1, filename);
    string s5,s6;

    e3.push(error);
    s5 << e3;

    s5 >> e5;
    s6 << e5;

    CPPUNIT_ASSERT_MESSAGE( "s5==s6", s5==s6 );


}

void SpkExceptionTest::iterationTest()
{
    try{
        int line        = __LINE__;
        char filename[] = __FILE__;
        char message[]  = "Serialize Test";
        SpkException e1(SpkError::SPK_UNKNOWN_ERR, message, line, filename);
        SpkError e;
        for( int i=0; i<e1.size(); i++ )
        {
            e = e1[i];
            CPPUNIT_ASSERT_MESSAGE("e.code() == SpkError::SPK_UNKNOWN_ERR", 
				   e.code() == SpkError::SPK_UNKNOWN_ERR );
        }
    }
    catch( ... )
    {
        CPPUNIT_ASSERT_MESSAGE( "operator[] shall never throw..", false );
    }

}
void SpkExceptionTest::findTest()
{
  char target[] = "target.cpp";
  int  linenum  = 999;
  char message[] = "This is a message.";

  SpkException e1(SpkError::SPK_UNKNOWN_ERR, message, linenum, "dummy");
  e1.push(SpkError::SPK_UNKNOWN_ERR, message, linenum, target);
  e1.push(SpkError::SPK_UNKNOWN_ERR, message, linenum, "dummy");

  CPPUNIT_ASSERT_EQUAL(1, e1.findFile(target));
}
