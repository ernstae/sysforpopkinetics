#include <iostream>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>

#include "ParseTree.h"
#include "ParseTreeTest.h"
#include "SpkCompilerUtil.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

void ParseTreeTest::setUp()
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
void ParseTreeTest::tearDown()
{
  XMLPlatformUtils::Terminate();
}
void ParseTreeTest::testCreate()
{
  ParseTree tree;
  DOMDocument * handler = tree.handler();
  CPPUNIT_ASSERT_MESSAGE( "ParseTree::handerl() returned NULL.", handler != NULL );

  DOMElement  * root = handler->getDocumentElement();
  CPPUNIT_ASSERT_MESSAGE( "DOMDocument::getDocumentElement() returned NULL.", root != NULL );
}

void ParseTreeTest::testHandler()
{
  ParseTree tree;
  DOMDocument * handler = tree.handler();
  CPPUNIT_ASSERT_MESSAGE( "ParseTree::handerl() returned NULL.", handler != NULL );

  DOMElement  * root = handler->getDocumentElement();
  CPPUNIT_ASSERT_MESSAGE( "DOMDocument::getDocumentElement() returned NULL.", root != NULL );

  DOMElement * node1 = handler->createElement( X( "node1" ) );
  CPPUNIT_ASSERT_MESSAGE( "DOMDocument::createElement() returned NULL.", node1 != NULL );

}
void ParseTreeTest::testNodeCarrier()
{
  ParseTree tree;
  DOMDocument * handler = tree.handler();
  struct NodeCarrier *node1 = tree.createNodeCarrier();
  node1->node = handler->createElement( X("node1") );
  int n = tree.releaseNodeCarriers();
  CPPUNIT_ASSERT_MESSAGE( "ParseTree:releaseNodeCarriers() should have released one NodeCarrier object.", n == 1 );
}

CppUnit::Test * ParseTreeTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ParseTreeTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<ParseTreeTest>("testCreate",
						    &ParseTreeTest::testCreate ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ParseTreeTest>("testHandler",
						    &ParseTreeTest::testHandler ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ParseTreeTest>("testNodeCarrier",
						    &ParseTreeTest::testNodeCarrier ) );
   return suiteOfTests;

}

