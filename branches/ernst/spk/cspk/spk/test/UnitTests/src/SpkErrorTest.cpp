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
 * File: SpkErrorTest.cpp
 *
 *
 * Test cases for SpkErrorTest
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <sstream>
#include <new>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkError.h"
#include "SpkErrorTest.h"

using namespace CppUnit;
using namespace std;

static void ImGonnaThrowBadException() 
{
  throw bad_exception();
}

void SpkErrorTest::setUp()
{
    // initializations
}
void SpkErrorTest::tearDown()
{
    // clean up
}

Test* SpkErrorTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("SpkErrorTest");

//    suiteOfTests->addTest(new TestCaller<SpkErrorTest>("mapTest", mapTest));

    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
			 "defaultConstructorTest", 
                         &SpkErrorTest::defaultConstructorTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                         "constructorTest", 
                         &SpkErrorTest::constructorTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                         "stdexceptionConstructorTest", 
                         &SpkErrorTest::stdexceptionConstructorTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                         "copyConstructorTest", 
                         &SpkErrorTest::copyConstructorTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                         "assignmentTest", 
                         &SpkErrorTest::assignmentTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                         "maxLinenumTest", 
                         &SpkErrorTest::maxLinenumTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                         "maxFilenameLenTest", 
                         &SpkErrorTest::maxFilenameLenTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                         "maxMessageLenTest", 
                         &SpkErrorTest::maxMessageLenTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                         "codeTest", 
                         &SpkErrorTest::codeTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                         "linenumTest", 
                         &SpkErrorTest::linenumTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                         "filenameTest", 
                         &SpkErrorTest::filenameTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                          "messageTest",
                          &SpkErrorTest::messageTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                          "serializeTest", 
                          &SpkErrorTest::serializeTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                          "formatLongErrorTest", 
                          &SpkErrorTest::formatLongErrorTest));
    suiteOfTests->addTest(new TestCaller<SpkErrorTest>(
                          "specialCharsTest", 
                          &SpkErrorTest::specialCharsTest));
    return suiteOfTests;
}
/*
void SpkErrorTest::mapTest()
{
  //
  // Just test whether the error code-description map has been properly initialized.
  //
  const char* mess = SpkError::describe(SpkError::SPK_UNKNOWN_ERR);

  assertImplementation( strcmp( mess, "Unknown" ) == 0 );
}
*/
void SpkErrorTest::defaultConstructorTest()
{
    const int MAX_ERRORS = 500;

    for(int i=0; i<MAX_ERRORS; i++)
    {
        try{
            SpkError e;
        }
        catch(...)
        {
            CPPUNIT_ASSERT_MESSAGE("Default Constructor should never throw", false);
        }
    }
}
void SpkErrorTest::constructorTest()
{
    const int MAX_ERRORS = 500;

    for(int i=0; i<MAX_ERRORS; i++)
    {
        try{
            SpkError e(SpkError::SPK_UNKNOWN_ERR, "constructorTest", __LINE__, __FILE__);
        }
        catch(...)
        {
            CPPUNIT_ASSERT_MESSAGE( "copyConstructorTest should never throw", false);
        }
    }
}

void SpkErrorTest::stdexceptionConstructorTest()
{
  try{
    ImGonnaThrowBadException();
  }
  catch( std::exception& e )
    {
      SpkError e1( e,
		   "standard library error", __LINE__, __FILE__);
      CPPUNIT_ASSERT_MESSAGE( "error code should be SPK_STD_ERR!", 
			      e1.code() == SpkError::SPK_STD_ERR );
      CPPUNIT_ASSERT_MESSAGE( "wrong error message", 
			      strstr(e1.message(), 
				     "standard library error") != NULL );
      return;
    }
  CPPUNIT_ASSERT_MESSAGE( "Shouldn't be here." , false );
}
void SpkErrorTest::copyConstructorTest()
{
    SpkError e1(SpkError::SPK_UNKNOWN_ERR, 
		"copyConstructorTest", __LINE__, __FILE__);
    SpkError e2(e1);

    CPPUNIT_ASSERT_EQUAL( e1.code(), e2.code() );
    CPPUNIT_ASSERT_EQUAL( e1.linenum(), e2.linenum() );
    CPPUNIT_ASSERT_MESSAGE( "e1.filename()==e2.filename()", 
			    strcmp(e1.filename(), e2.filename())==0 );
    CPPUNIT_ASSERT_MESSAGE( "e1.message()==e2.message()", 
			    strcmp(e1.message(), e2.message())==0 );

    SpkError e3(e2);

    CPPUNIT_ASSERT_EQUAL( e1.code(), e3.code() );
    CPPUNIT_ASSERT_EQUAL( e1.linenum(), e3.linenum() );
    CPPUNIT_ASSERT_MESSAGE( "e1.filename()==e3.filename()", 
			    strcmp(e1.filename(), e3.filename())==0 );
    CPPUNIT_ASSERT_MESSAGE( "e1.message()==e3.message()", 
			    strcmp(e1.message(), e3.message())==0 );

}
void SpkErrorTest::assignmentTest()
{
    SpkError e1(SpkError::SPK_UNKNOWN_ERR, "assignmentTest", __LINE__, __FILE__);
    SpkError e2 = e1;

    CPPUNIT_ASSERT_EQUAL( e1.code(), e2.code() );
    CPPUNIT_ASSERT_EQUAL( e1.linenum(), e2.linenum() );
    CPPUNIT_ASSERT_MESSAGE( "e1.filename()==e2.filename()",
                            strcmp(e1.filename(), e2.filename())==0 );
    CPPUNIT_ASSERT_MESSAGE( "e1.message()==e2.message()", 
                            strcmp(e1.message(), e2.message())==0 );

    SpkError e3(SpkError::SPK_UNKNOWN_ERR, "assignmentTest", __LINE__, __FILE__);
    e3 = e2;

    CPPUNIT_ASSERT_EQUAL( e1.code(), e3.code() );
    CPPUNIT_ASSERT_EQUAL( e1.linenum(), e3.linenum() );
    CPPUNIT_ASSERT_MESSAGE( "e1.filename()==e3.filename()", 
                            strcmp(e1.filename(), e3.filename())==0 );
    CPPUNIT_ASSERT_MESSAGE( "e1.message()==e3.message()", 
                            strcmp(e1.message(), e3.message())==0 );
}
void SpkErrorTest::maxLinenumTest()
{
    unsigned int i, multiplier, max=0;

    for(i=1, multiplier=1; i<= SpkError::LINENUM_FIELD_LEN; i++, multiplier*=10)
        max += 9*multiplier;

    CPPUNIT_ASSERT_MESSAGE( "Max # of allowed lines", SpkError::maxLinenum() == max );
}
void SpkErrorTest::maxFilenameLenTest()
{
    CPPUNIT_ASSERT_MESSAGE( "SpkError::FILENAME_FIELD_LEN == SpkError::maxFilenameLen()",
                             SpkError::FILENAME_FIELD_LEN == SpkError::maxFilenameLen() );
}
void SpkErrorTest::maxMessageLenTest()
{
    CPPUNIT_ASSERT_MESSAGE( "SpkError::MESSAGE_FIELD_LEN == SpkError::maxMessageLen()",
                             SpkError::MESSAGE_FIELD_LEN == SpkError::maxMessageLen() );
}

void SpkErrorTest::codeTest()
{
    SpkError e(SpkError::SPK_UNKNOWN_ERR, "codeTest", __LINE__, __FILE__);
    CPPUNIT_ASSERT_MESSAGE( "e.code()==SpkError::SPK_UNKNOWN_ERR", 
                             e.code()==SpkError::SPK_UNKNOWN_ERR );
}
void SpkErrorTest::linenumTest()
{
    unsigned int linenum = 999999;
    unsigned int actual;
    SpkError e(SpkError::SPK_UNKNOWN_ERR, "linenumTest", linenum, __FILE__);
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
void SpkErrorTest::filenameTest()
{
    char filename[] = "X:\\abc\\efg\\xyz.cpp";
    SpkError e(SpkError::SPK_UNKNOWN_ERR, "filenameTest", __LINE__, filename);
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
void SpkErrorTest::messageTest()
{
    char message[] = "The sky is blue.\nThe water is clear.\n";
    SpkError e(SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__);
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
/*
void SpkErrorTest::formatTest()
{
    SpkError e;
    CPPUNIT_ASSERT_MESSAGE(strcmp(e.formatString(), "errorcode\n%-4d\nlinenum\n%-6d\nfilename\n%-128s\nmessage\n%-256s\n")==0, "", __LINE__, __FILE__);
}
*/

void SpkErrorTest::serializeTest()
{
    int i;
    unsigned int multiplier = 1;
    unsigned int errorcode = SpkError::SPK_UNKNOWN_ERR;
    for( i=1; i<SpkError::ERRORCODE_FIELD_LEN; i++ )
    {
        multiplier *= 10;
        errorcode += 9*multiplier;
    }

    unsigned int linenum = 9;
    for( i=1, multiplier=1; i<SpkError::LINENUM_FIELD_LEN; i++ )
    {
        multiplier *= 10;
        linenum += 9*multiplier;
    }
    char filename[]        = "X:\\abc\\myfile.cpp";
    char message[]         = "This is the <first> line of a test message.\nSecond line.\n";

    SpkError e1(static_cast<SpkError::ErrorCode>(errorcode), message, linenum, filename);

    string str1;
    str1 << e1;
    
    SpkError e2;
    str1 >> e2;
    string str2;
    str2 << e2;

    CPPUNIT_ASSERT_MESSAGE( "e1.code()==e2.code()", e1.code()==e2.code() );
    CPPUNIT_ASSERT_MESSAGE( "e1.linenum()==e2.linenum()", e1.linenum()==e2.linenum() );
    CPPUNIT_ASSERT_MESSAGE( "e1.filename()==e2.filename()", strcmp(e1.filename(),e2.filename())==0 );
    CPPUNIT_ASSERT_MESSAGE( "e1.message()==e2.message()", strcmp(e1.message(),e2.message())==0 );
    CPPUNIT_ASSERT_MESSAGE( "str1==str2", str1==str2 );

    SpkError e3;
    fstream fs("dummy", std::ios::out);
    CPPUNIT_ASSERT_MESSAGE( "Failed to open a temporary file <dummy> as output", fs.good() );
    fs << e1;
    fs.close();

    fs.open("dummy", ios::in);
    CPPUNIT_ASSERT_MESSAGE( "Failed to open a temporary file <dummy> as input.", fs.good() );
    fs >> e3;
    fs.close();
    remove( "dummy" );

    CPPUNIT_ASSERT_MESSAGE( "e1.code()==e3.code()", e1.code()==e3.code() );
    CPPUNIT_ASSERT_MESSAGE( "e1.linenum()==e3.linenum()", e1.linenum()==e3.linenum() );
    CPPUNIT_ASSERT_MESSAGE( "e1.filename()==e3.filename()", strcmp(e1.filename(),e3.filename())==0 );
    CPPUNIT_ASSERT_MESSAGE( "e1.message()==e3.message()", strcmp(e1.message(),e3.message())==0 );
  
}
void SpkErrorTest::specialCharsTest()
{
  char message[ SpkError::maxMessageLen() ];
  sprintf( message, "<&>\"\'" );
  SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );

  ostringstream expected, actual;

  expected << "<error code=\"" << e.code() << "\">" << endl;
  expected << "<description>" << SpkError::describe( e.code() ) << "</description>" << endl;
  expected << "<file_name>" << e.filename() << "</file_name>" << endl;
  expected << "<line_number>" << e.linenum() << "</line_number>" << endl;
  expected << "<message>" <<  "&lt;&amp;&gt;\"\'" << "</message>" << endl;
  expected << "</error>" << endl;

  actual << e;

  CPPUNIT_ASSERT( expected.str() == actual.str() );
  return;
}
void SpkErrorTest::formatLongErrorTest()
{
    // Create an error message that is too long to be stored
    // in an SpkError object.
    ostringstream message;
    string messageStr;
    message << "An error occurred during individual level estimation." << endl;
    message << endl;
    message << "The analysis failed during the optimization of the parameters for" << endl;
    message << "individual 3." << endl;
    message << endl;
    message << "At the last successful iteration of the individual level optimizer the" << endl;
    message << "individual parameter and its gradient had the following values:" << endl;
    message << endl;
    message << "Index  Parameter  Bounds?    Gradient" << endl;
    message << "-----  ---------  -------    --------" << endl;
    message << endl;
    message << "1        3.40      Lower      -0.004 " << endl;
    message << "2        49.0      Free        0.088 " << endl;
    message << "3      -0.030      Both       -9.839 " << endl;
    messageStr = message.str();

    // This will be the message formatted the same way a shorter
    // message would be formatted by the serialize function.
    string formattedMessage;

    // Format the long error message.
    formatLongError(
        SpkError::SPK_OPT_ERR,
        messageStr,
        __LINE__,
        __FILE__,
        formattedMessage );

    // Check that the formatted message is not empty.
    CPPUNIT_ASSERT_MESSAGE(
        "The formatted long message string is empty. ",
        messageStr.empty() == false );

    // Uncomment these statements to see the error message.
    /*
    cout << endl;
    cout << "########################################" << endl;
    cout << formattedMessage;
    cout << "########################################" << endl;
    */

}
