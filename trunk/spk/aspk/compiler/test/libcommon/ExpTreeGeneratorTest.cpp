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
}
void ExpTreeGeneratorTest::tearDown()
{
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
void ExpTreeGeneratorTest::testExpNodeCarrier()
{
  ExpTreeGenerator tree;
  DOMDocument * getRoot = tree.getRoot();
  struct ExpNodeCarrier *node1 = tree.createExpNodeCarrier();
  node1->node = getRoot->createElement( X("node1") );
  int n = tree.releaseExpNodeCarriers();
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

