#include <iostream>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>

#include "ExpTreeGenerator.h"
#include "ExpTreeGeneratorTest.h"
#include "SpkCompilerUtil.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

void ExpTreeGeneratorTest::setUp()
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
void ExpTreeGeneratorTest::tearDown()
{
  XMLPlatformUtils::Terminate();
}
void ExpTreeGeneratorTest::testCreate()
{
  ExpTreeGenerator tree;
  DOMDocument * getRoot = tree.getRoot();
  CPPUNIT_ASSERT_MESSAGE( "ExpTreeGenerator::handerl() returned NULL.", getRoot != NULL );

  DOMElement  * root = getRoot->getDocumentElement();
  CPPUNIT_ASSERT_MESSAGE( "DOMDocument::getDocumentElement() returned NULL.", root != NULL );
}

void ExpTreeGeneratorTest::testGetRoot()
{
  ExpTreeGenerator tree;
  DOMDocument * getRoot = tree.getRoot();
  CPPUNIT_ASSERT_MESSAGE( "ExpTreeGenerator::handerl() returned NULL.", getRoot != NULL );

  DOMElement  * root = getRoot->getDocumentElement();
  CPPUNIT_ASSERT_MESSAGE( "DOMDocument::getDocumentElement() returned NULL.", root != NULL );

  DOMElement * node1 = getRoot->createElement( X( "node1" ) );
  CPPUNIT_ASSERT_MESSAGE( "DOMDocument::createElement() returned NULL.", node1 != NULL );

}
void ExpTreeGeneratorTest::testNodeCarrier()
{
  ExpTreeGenerator tree;
  DOMDocument * getRoot = tree.getRoot();
  struct NodeCarrier *node1 = tree.createNodeCarrier();
  node1->node = getRoot->createElement( X("node1") );
  int n = tree.releaseNodeCarriers();
  CPPUNIT_ASSERT_MESSAGE( "ExpTreeGenerator:releaseNodeCarriers() should have released one NodeCarrier object.", n == 1 );
}

CppUnit::Test * ExpTreeGeneratorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ExpTreeGeneratorTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<ExpTreeGeneratorTest>("testCreate",
						    &ExpTreeGeneratorTest::testCreate ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ExpTreeGeneratorTest>("testGetRoot",
						    &ExpTreeGeneratorTest::testGetRoot ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ExpTreeGeneratorTest>("testNodeCarrier",
						    &ExpTreeGeneratorTest::testNodeCarrier ) );
   return suiteOfTests;

}

