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
 * File: SpkCompilerExceptionTest.cpp
 *
 *
 * Test cases for SpkCompilerException test
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../spkcompiler/SpkCompilerException.h"
#include "SpkCompilerExceptionTest.h"

using namespace std;
using namespace CppUnit;

void SpkCompilerExceptionTest::setUp()
{
    // initializations
}
void SpkCompilerExceptionTest::tearDown()
{
    // clean up
}

Test* SpkCompilerExceptionTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("SpkCompilerExceptionTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<SpkCompilerExceptionTest>(
                         "findTest", 
                         &SpkCompilerExceptionTest::findTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerExceptionTest>(
                         "defaultConstructorTest", 
                         &SpkCompilerExceptionTest::defaultConstructorTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerExceptionTest>(
                         "constructorTest", 
                         &SpkCompilerExceptionTest::constructorTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerExceptionTest>(
                         "copyConstructorTest",
                         &SpkCompilerExceptionTest::copyConstructorTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerExceptionTest>(
                         "assignmentTest", 
                         &SpkCompilerExceptionTest::assignmentTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerExceptionTest>(
                         "pushTest", 
                         &SpkCompilerExceptionTest::pushTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerExceptionTest>(
                         "popTest",
                         &SpkCompilerExceptionTest::popTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerExceptionTest>(
                         "iterationTest", 
                         &SpkCompilerExceptionTest::iterationTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerExceptionTest>(
                         "serializeTest", 
                         &SpkCompilerExceptionTest::serializeTest));

    suiteOfTests->addTest(new TestCaller<SpkCompilerExceptionTest>(
                         "getXmlTest", 
                         &SpkCompilerExceptionTest::getXmlTest));

    return suiteOfTests;
}

void SpkCompilerExceptionTest::defaultConstructorTest()
{
    try{
        SpkCompilerException e;
    }
    catch( ... )
    {
        CPPUNIT_ASSERT_MESSAGE( "Default constructor shall never throw.", false );
    }
}
void SpkCompilerExceptionTest::constructorTest()
{
    try{
        SpkCompilerException e( SpkCompilerError::ASPK_UNKNOWN_ERR, "Constructor Test", __LINE__, __FILE__);
    }
    catch( ... )
    {
        CPPUNIT_ASSERT_MESSAGE( "Constructor shall never throw!", false );
    }
}
void SpkCompilerExceptionTest::copyConstructorTest()
{
    try{
        SpkCompilerException e1;
        SpkCompilerException e2(e1);
    }
    catch( ... )
    {
        CPPUNIT_ASSERT_MESSAGE( "Copy constructor threw an exception!.", false );
    }
}
void SpkCompilerExceptionTest::assignmentTest()
{
    try{
        SpkCompilerException e1;
        SpkCompilerException e2 = e1;
    }
    catch( ... )
    {
        CPPUNIT_ASSERT_MESSAGE( "Assignment operator threw an exception!", false);
    }
}
void SpkCompilerExceptionTest::pushTest()
{
    SpkCompilerError err(SpkCompilerError::ASPK_UNKNOWN_ERR, "pushTest", __LINE__, __FILE__);
    SpkCompilerException e( err );
    try{
        e.push( err );
        e.push( err );
    }
    catch(...)
    {
        CPPUNIT_ASSERT_MESSAGE( "push() threw!", false );
    }
    CPPUNIT_ASSERT_MESSAGE("e.size()==3", e.size() == 3 );
    for( int i=3; i<SpkCompilerException::maxErrors(); i++ )
    {
        e.push(err);
    }
    CPPUNIT_ASSERT_MESSAGE( "e.full()==true", e.full() );
}

void SpkCompilerExceptionTest::popTest()
{
    SpkCompilerError err(SpkCompilerError::ASPK_UNKNOWN_ERR, "pushTest", __LINE__, __FILE__);
    SpkCompilerException e( err );
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

void SpkCompilerExceptionTest::serializeTest()
{
    int line        = __LINE__;
    char filename[] = __FILE__;
    char message[]  = "Serialize Test";
    SpkCompilerException e(SpkCompilerError::ASPK_UNKNOWN_ERR, message, line, filename);
    ostringstream actual, expected;

    expected << "count" << endl;
    expected << "1" << endl;
    expected << SpkCompilerError::ERRORCODE_FIELD_NAME << endl;
    expected << e[0].code() << endl;

    expected << SpkCompilerError::ERRORCODE_DESCRIPTION_FIELD_NAME << endl;
    expected << SpkCompilerError::describe( e[0].code() ) << endl;

    expected << SpkCompilerError::LINENUM_FIELD_NAME   << endl;
    expected << e[0].linenum()   << endl;
    
    expected << SpkCompilerError::FILENAME_FIELD_NAME  << endl;
    expected << e[0].filename()  << endl;
    
    expected << SpkCompilerError::MESSAGE_FIELD_NAME   << endl;
    expected << e[0].message() << endl;
 
    actual << e;

    /*
    cout << "----------------------------------------" << endl;
    cout << expected.str();
    cout << "----------------------------------------" << endl;
    cout << actual.str();
    cout << "----------------------------------------" << endl;
    */

    CPPUNIT_ASSERT( expected.str() == actual.str() );

}

void SpkCompilerExceptionTest::iterationTest()
{
    try{
        int line        = __LINE__;
        char filename[] = __FILE__;
        char message[]  = "Serialize Test";
        SpkCompilerException e1(SpkCompilerError::ASPK_UNKNOWN_ERR, message, line, filename);
        SpkCompilerError e;
        for( int i=0; i<e1.size(); i++ )
        {
            e = e1[i];
            CPPUNIT_ASSERT_MESSAGE("e.code() == SpkCompilerError::ASPK_UNKNOWN_ERR", 
				   e.code() == SpkCompilerError::ASPK_UNKNOWN_ERR );
        }
    }
    catch( ... )
    {
        CPPUNIT_ASSERT_MESSAGE( "operator[] shall never throw..", false );
    }

}
void SpkCompilerExceptionTest::findTest()
{
  char target[] = "target.cpp";
  int  linenum  = 999;
  char message[] = "This is a message.";

  SpkCompilerException e1(SpkCompilerError::ASPK_UNKNOWN_ERR, message, linenum, "dummy");
  e1.push(SpkCompilerError::ASPK_UNKNOWN_ERR, message, linenum, target);
  e1.push(SpkCompilerError::ASPK_UNKNOWN_ERR, message, linenum, "dummy");

  CPPUNIT_ASSERT_EQUAL(1, e1.findFile(target));
}
void SpkCompilerExceptionTest::getXmlTest()
{
  char message[ SpkCompilerError::maxMessageLen() ];
  sprintf( message, "<&>" );
  SpkCompilerException e( SpkCompilerError::ASPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );

  ostringstream expected, actual;

  expected << "<error_list length=\"1\">" << endl;
  expected << "<error>" << endl;
  expected << "   <code>" << SpkCompilerError::describe( e[0].code() ) << "</code>" << endl;
  expected << "   <file_name>" << e[0].filename() << "</file_name>" << endl;
  expected << "   <line_number>" << e[0].linenum() << "</line_number>" << endl;
  expected << "   <message>" <<  "&lt;&amp;&gt;" << "</message>" << endl;
  expected << "</error>" << endl;
  expected << "</error_list>" << endl;

  actual << e.getXml();
  /*
  cout << "----------------------------------------" << endl;
  cout << expected.str();
  cout << "----------------------------------------" << endl;
  cout << actual.str();
  cout << "----------------------------------------" << endl;
  */

  CPPUNIT_ASSERT( expected.str() == actual.str() );
  return;
}
