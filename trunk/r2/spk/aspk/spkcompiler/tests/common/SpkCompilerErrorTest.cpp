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
 * File: SpkCompilerErrorTest.cpp
 *
 *
 * Test cases for SpkCompilerErrorTest
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <sstream>
#include <new>
#include <stdio.h>
#include <cstring>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../spkcompiler/SpkCompilerError.h"
#include "SpkCompilerErrorTest.h"

using namespace CppUnit;
using namespace std;

static void ImGonnaThrowBadException() 
{
  throw bad_exception();
}

void SpkCompilerErrorTest::setUp()
{
    // initializations
}
void SpkCompilerErrorTest::tearDown()
{
    // clean up
}

Test* SpkCompilerErrorTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("SpkCompilerErrorTest");

    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
			 "defaultConstructorTest", 
                         &SpkCompilerErrorTest::defaultConstructorTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                         "constructorTest", 
                         &SpkCompilerErrorTest::constructorTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                         "stdexceptionConstructorTest", 
                         &SpkCompilerErrorTest::stdexceptionConstructorTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                         "copyConstructorTest", 
                         &SpkCompilerErrorTest::copyConstructorTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                         "assignmentTest", 
                         &SpkCompilerErrorTest::assignmentTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                         "maxLinenumTest", 
                         &SpkCompilerErrorTest::maxLinenumTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                         "maxFilenameLenTest", 
                         &SpkCompilerErrorTest::maxFilenameLenTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                         "maxMessageLenTest", 
                         &SpkCompilerErrorTest::maxMessageLenTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                         "codeTest", 
                         &SpkCompilerErrorTest::codeTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                         "linenumTest", 
                         &SpkCompilerErrorTest::linenumTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                         "filenameTest", 
                         &SpkCompilerErrorTest::filenameTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                          "messageTest",
                          &SpkCompilerErrorTest::messageTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                          "serializeTest", 
                          &SpkCompilerErrorTest::serializeTest));
    suiteOfTests->addTest(new TestCaller<SpkCompilerErrorTest>(
                          "getXMLTest", 
                          &SpkCompilerErrorTest::getXmlTest));

    return suiteOfTests;
}

void SpkCompilerErrorTest::defaultConstructorTest()
{
    const int MAX_ERRORS = 500;

    for(int i=0; i<MAX_ERRORS; i++)
    {
        try{
            SpkCompilerError e;
        }
        catch(...)
        {
            CPPUNIT_ASSERT_MESSAGE("Default Constructor should never throw", false);
        }
    }
}
void SpkCompilerErrorTest::constructorTest()
{
    const int MAX_ERRORS = 500;

    for(int i=0; i<MAX_ERRORS; i++)
    {
        try{
            SpkCompilerError e(SpkCompilerError::ASPK_UNKNOWN_ERR, "constructorTest", __LINE__, __FILE__);
        }
        catch(...)
        {
            CPPUNIT_ASSERT_MESSAGE( "copyConstructorTest should never throw", false);
        }
    }
}

void SpkCompilerErrorTest::stdexceptionConstructorTest()
{
  try{
    ImGonnaThrowBadException();
  }
  catch( std::exception& e )
    {
      SpkCompilerError e1( e,
		   "standard library error", __LINE__, __FILE__);
      CPPUNIT_ASSERT_MESSAGE( "error code should be ASPK_STD_ERR!", 
			      e1.code() == SpkCompilerError::ASPK_STD_ERR );
      CPPUNIT_ASSERT_MESSAGE( "wrong error message", 
			      strstr(e1.message(), 
				     "standard library error") != NULL );
      return;
    }
  CPPUNIT_ASSERT_MESSAGE( "Shouldn't be here." , false );
}
void SpkCompilerErrorTest::copyConstructorTest()
{
    SpkCompilerError e1(SpkCompilerError::ASPK_UNKNOWN_ERR, 
		"copyConstructorTest", __LINE__, __FILE__);
    SpkCompilerError e2(e1);

    CPPUNIT_ASSERT_EQUAL( e1.code(), e2.code() );
    CPPUNIT_ASSERT_EQUAL( e1.linenum(), e2.linenum() );
    CPPUNIT_ASSERT_MESSAGE( "e1.filename()==e2.filename()", 
			    strcmp(e1.filename(), e2.filename())==0 );
    CPPUNIT_ASSERT_MESSAGE( "e1.message()==e2.message()", 
			    strcmp(e1.message(), e2.message())==0 );

    SpkCompilerError e3(e2);

    CPPUNIT_ASSERT_EQUAL( e1.code(), e3.code() );
    CPPUNIT_ASSERT_EQUAL( e1.linenum(), e3.linenum() );
    CPPUNIT_ASSERT_MESSAGE( "e1.filename()==e3.filename()", 
			    strcmp(e1.filename(), e3.filename())==0 );
    CPPUNIT_ASSERT_MESSAGE( "e1.message()==e3.message()", 
			    strcmp(e1.message(), e3.message())==0 );

}
void SpkCompilerErrorTest::assignmentTest()
{
    SpkCompilerError e1(SpkCompilerError::ASPK_UNKNOWN_ERR, "assignmentTest", __LINE__, __FILE__);
    SpkCompilerError e2 = e1;

    CPPUNIT_ASSERT_EQUAL( e1.code(), e2.code() );
    CPPUNIT_ASSERT_EQUAL( e1.linenum(), e2.linenum() );
    CPPUNIT_ASSERT_MESSAGE( "e1.filename()==e2.filename()",
                            strcmp(e1.filename(), e2.filename())==0 );
    CPPUNIT_ASSERT_MESSAGE( "e1.message()==e2.message()", 
                            strcmp(e1.message(), e2.message())==0 );

    SpkCompilerError e3(SpkCompilerError::ASPK_UNKNOWN_ERR, "assignmentTest", __LINE__, __FILE__);
    e3 = e2;

    CPPUNIT_ASSERT_EQUAL( e1.code(), e3.code() );
    CPPUNIT_ASSERT_EQUAL( e1.linenum(), e3.linenum() );
    CPPUNIT_ASSERT_MESSAGE( "e1.filename()==e3.filename()", 
                            strcmp(e1.filename(), e3.filename())==0 );
    CPPUNIT_ASSERT_MESSAGE( "e1.message()==e3.message()", 
                            strcmp(e1.message(), e3.message())==0 );
}
void SpkCompilerErrorTest::maxLinenumTest()
{
    unsigned int i, multiplier, max=0;

    for(i=1, multiplier=1; i<= SpkCompilerError::LINENUM_FIELD_LEN; i++, multiplier*=10)
        max += 9*multiplier;

    CPPUNIT_ASSERT_MESSAGE( "Max # of allowed lines", SpkCompilerError::maxLinenum() == max );
}
void SpkCompilerErrorTest::maxFilenameLenTest()
{
    CPPUNIT_ASSERT_MESSAGE( "SpkCompilerError::FILENAME_FIELD_LEN == SpkCompilerError::maxFilenameLen()",
                             SpkCompilerError::FILENAME_FIELD_LEN == SpkCompilerError::maxFilenameLen() );
}
void SpkCompilerErrorTest::maxMessageLenTest()
{
    CPPUNIT_ASSERT_MESSAGE( "SpkCompilerError::MESSAGE_FIELD_LEN == SpkCompilerError::maxMessageLen()",
                             SpkCompilerError::MESSAGE_FIELD_LEN == SpkCompilerError::maxMessageLen() );
}

void SpkCompilerErrorTest::codeTest()
{
    SpkCompilerError e(SpkCompilerError::ASPK_UNKNOWN_ERR, "codeTest", __LINE__, __FILE__);
    CPPUNIT_ASSERT_MESSAGE( "e.code()==SpkCompilerError::ASPK_UNKNOWN_ERR", 
                             e.code()==SpkCompilerError::ASPK_UNKNOWN_ERR );
}
void SpkCompilerErrorTest::linenumTest()
{
    unsigned int linenum = 999999;
    unsigned int actual;
    SpkCompilerError e(SpkCompilerError::ASPK_UNKNOWN_ERR, "linenumTest", linenum, __FILE__);
    try
    {
         actual = e.linenum();
    }
    catch(...)
    {
        CPPUNIT_ASSERT_MESSAGE( "linenum() should never throw", false);
    }
    CPPUNIT_ASSERT_MESSAGE( "e.linenum()==linenum", actual==linenum );

}
void SpkCompilerErrorTest::filenameTest()
{
    char filename[] = "X:\\abc\\efg\\xyz.cpp";
    SpkCompilerError e(SpkCompilerError::ASPK_UNKNOWN_ERR, "filenameTest", __LINE__, filename);
    char* s;
    try
    {
        s = const_cast<char*>(e.filename());
    }
    catch(...)
    {
        CPPUNIT_ASSERT_MESSAGE( "filename() should never throw", false );
    }
    CPPUNIT_ASSERT_MESSAGE( "strcmp(e.filename(),filename)==0", 
                            strcmp(s,filename)==0 );
}
void SpkCompilerErrorTest::messageTest()
{
    char message[] = "The sky is blue.\nThe water is clear.\n";
    SpkCompilerError e(SpkCompilerError::ASPK_UNKNOWN_ERR, message, __LINE__, __FILE__);
    char *s;
    try
    {
        s = const_cast<char*>(e.message());
    }
    catch(...)
    {
        CPPUNIT_ASSERT_MESSAGE( "message() should never throw", false );
    }
    CPPUNIT_ASSERT_MESSAGE( "strcmp(e.message(),message)==0", strcmp(s,message)==0 );
}

void SpkCompilerErrorTest::serializeTest()
{
    int i;
    unsigned int multiplier = 1;
    unsigned int errorcode = SpkCompilerError::ASPK_UNKNOWN_ERR;
    for( i=1; i<SpkCompilerError::ERRORCODE_FIELD_LEN; i++ )
    {
        multiplier *= 10;
        errorcode += 9*multiplier;
    }

    unsigned int linenum = 9;
    for( i=1, multiplier=1; i<SpkCompilerError::LINENUM_FIELD_LEN; i++ )
    {
        multiplier *= 10;
        linenum += 9*multiplier;
    }
    char filename[]        = "X:\\abc\\myfile.cpp";
    char message[]         = "This is the <first> line of a test message.\nSecond line.\n";


    SpkCompilerError e(static_cast<SpkCompilerError::ErrorCode>(errorcode), message, linenum, filename);
    ostringstream expected;
    expected << SpkCompilerError::ERRORCODE_FIELD_NAME << endl;
    expected << e.code() << endl;
    expected << SpkCompilerError::ERRORCODE_DESCRIPTION_FIELD_NAME << endl;
    expected << SpkCompilerError::describe( e.code() ) << endl;
    expected << SpkCompilerError::LINENUM_FIELD_NAME   << endl;
    expected << e.linenum()   << endl;
    expected << SpkCompilerError::FILENAME_FIELD_NAME  << endl;
    expected << e.filename()  << endl;
    expected << SpkCompilerError::MESSAGE_FIELD_NAME   << endl;
    expected << e.message() << endl; 

    string actual;
    actual << e;

    CPPUNIT_ASSERT( actual == expected.str() );
}
void SpkCompilerErrorTest::getXmlTest()
{
  char message[ SpkCompilerError::maxMessageLen() ];
  sprintf( message, "<&>\"\'" );
  SpkCompilerError e( SpkCompilerError::ASPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );

  ostringstream expected, actual;

  expected << "<error>" << endl;
  expected << "   <code>" << SpkCompilerError::describe( e.code() ) << "</code>" << endl;
  expected << "   <file_name>" << e.filename() << "</file_name>" << endl;
  expected << "   <line_number>" << e.linenum() << "</line_number>" << endl;
  expected << "   <message>" <<  "&lt;&amp;&gt;\"\'" << "</message>" << endl;
  expected << "</error>" << endl;

  actual << e.getXml();

  CPPUNIT_ASSERT( expected.str() == actual.str() );
  return;
}
