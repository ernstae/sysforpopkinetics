#include <iostream>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>

#include "SpkCompilerUtil.h"
#include "SpkCompilerUtilTest.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

void SpkCompilerUtilTest::setUp()
{
  // Initialize the XML4C2 system
  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch(const XMLException &toCatch)
  {
    char buf[256];
    sprintf( buf, "Error during Xerces-c Initialization.\nException message: %s", C(toCatch.getMessage()) );
    CPPUNIT_ASSERT_MESSAGE( buf, false );
  }
}
void SpkCompilerUtilTest::tearDown()
{
  XMLPlatformUtils::Terminate();
}
void SpkCompilerUtilTest::testCreate()
{
  SpkCompilerUtil util;
#ifndef NDEBUG
  CPPUNIT_ASSERT_MESSAGE( "xml_strings vector should be empty!", util.debug_xml_strings().size() == 0 );
  CPPUNIT_ASSERT_MESSAGE( "xml_strings vector should be empty!", util.debug_c_strings().size() == 0 );
#endif
}
void SpkCompilerUtilTest::testStringConversion()
{
  SpkCompilerUtil util;
  char hello[] = "Hello";
  const XMLCh* xml_str = util.createXmlString( hello );

  CPPUNIT_ASSERT_MESSAGE( "xml_strings vector should have one entry!", util.debug_xml_strings().size() == 1 );

  const char* c_str = util.createCString( xml_str );
  CPPUNIT_ASSERT_MESSAGE( "The original XML string and generated C string do not match!", strcmp( hello, c_str )== 0 );

  CPPUNIT_ASSERT_MESSAGE( "c_strings vector should be empty!", util.debug_c_strings().size() == 1 );  

  CPPUNIT_ASSERT_MESSAGE( "Failed to release resources held by xml_strings vector", util.debug_call_releaseXmlStrings() == 1 );
  CPPUNIT_ASSERT_MESSAGE( "xml_strings vector should be empty!", util.debug_xml_strings().size() == 0 );

  CPPUNIT_ASSERT_MESSAGE( "Failed to release resources held by c_strings vector", util.debug_call_releaseCStrings() == 1 );
  CPPUNIT_ASSERT_MESSAGE( "c_strings vector should be empty!", util.debug_c_strings().size() == 0 );
}

CppUnit::Test * SpkCompilerUtilTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "SpkCompilerUtilTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<SpkCompilerUtilTest>("testCreate",
						    &SpkCompilerUtilTest::testCreate ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<SpkCompilerUtilTest>("testStringConversion",
						    &SpkCompilerUtilTest::testStringConversion ) );
   return suiteOfTests;

}

