#include <iostream>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>

#include "ExpTreeGenerator.h"
#include "ExpTreeGeneratorTest.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

void ExpTreeGeneratorTest::setUp()
{
}
void ExpTreeGeneratorTest::tearDown()
{
}
void ExpTreeGeneratorTest::testCreate()
{
  ExpTreeGenerator util;
  DOMDocument * tree = util.createTree("unit");
  CPPUNIT_ASSERT_MESSAGE( "ExpTreeGenerator::createTree() returned NULL.", tree != NULL );

  DOMElement  * root = tree->getDocumentElement();
  CPPUNIT_ASSERT_MESSAGE( "DOMDocument::getDocumentElement() returned NULL.", root != NULL );
}

void ExpTreeGeneratorTest::testGetRoot()
{
  ExpTreeGenerator util;
  DOMDocument * tree = util.createTree( "unit" );
  CPPUNIT_ASSERT_MESSAGE( "ExpTreeGenerator::createTree() returned NULL.", tree != NULL );

  DOMElement  * root = tree->getDocumentElement();
  CPPUNIT_ASSERT_MESSAGE( "DOMDocument::getDocumentElement() returned NULL.", root != NULL );

  DOMElement * node1 = tree->createElement( X( "node1" ) );
  CPPUNIT_ASSERT_MESSAGE( "DOMDocument::createElement() returned NULL.", node1 != NULL );

}
void ExpTreeGeneratorTest::testExpNodeCarrier()
{
  ExpTreeGenerator util;
  DOMDocument * tree = util.createTree("unit");
  struct ExpNodeCarrier *node1 = util.createExpNodeCarrier();
  node1->node = tree->createElement( X("node1") );
  int n = util.releaseExpNodeCarriers();
  CPPUNIT_ASSERT_MESSAGE( "ExpTreeGenerator:releaseExpNodeCarriers() should have released one ExpNodeCarrier object.", n == 1 );
}

CppUnit::Test * ExpTreeGeneratorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ExpTreeGeneratorTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<ExpTreeGeneratorTest>("testCreate",
						    &ExpTreeGeneratorTest::testCreate ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ExpTreeGeneratorTest>("testGetRoot",
						    &ExpTreeGeneratorTest::testGetRoot ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ExpTreeGeneratorTest>("testExpNodeCarrier",
						    &ExpTreeGeneratorTest::testExpNodeCarrier ) );
   return suiteOfTests;

}

