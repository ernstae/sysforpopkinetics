#include <iostream>
#include <string>
#include <vector>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/framework/StdOutFormatTarget.hpp>
#include "client.h"
#include "SpkParameters.h"
#include "read_content.h"
#include "read_contentTest.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

void read_contentTest::setUp()
{
}

void read_contentTest::tearDown()
{
}

void read_contentTest::test()
{
    // Initialize the XML4C2 system.
    try
    {
        XMLPlatformUtils::Initialize();
    }

    catch(const XMLException& toCatch)
    {
        char *pMsg = XMLString::transcode(toCatch.getMessage());
        CPPUNIT_ASSERT_MESSAGE( pMsg, false );
        XMLString::release(&pMsg);
    }

    DOMImplementation* impl 
      =  DOMImplementationRegistry::getDOMImplementation(XMLString::transcode("Core"));
    
    DOMDocument* doc = impl->createDocument(
					    0,                    // root element namespace URI.
					    XMLString::transcode("spkinml"),         // root element name
					    0);                   // document type object (DTD).
    
    DOMElement* rootElem = doc->getDocumentElement();
    
    
    DOMElement* contentNode  = doc->createElement( XMLString::transcode( "content" ) );
    rootElem->appendChild( contentNode ); 
    contentNode->setAttribute( XMLString::transcode( "spkinml_ver" ), XMLString::transcode( "1.0" ) );
    contentNode->setAttribute( XMLString::transcode( "client" ), XMLString::transcode( client::STR_NONMEM ) );
    contentNode->setAttribute( XMLString::transcode( "analysis" ), XMLString::transcode( "population" ) );
    
    DOMWriter * writer = ((DOMImplementationLS*)impl)->createDOMWriter();
    StdOutFormatTarget destination;
    //    writer->writeNode( &destination, *rootElem );
    
    string verOut;
    enum client::type clientOut;
    enum SpkParameters::Analysis analysisOut;
    read_content( contentNode, verOut, clientOut, analysisOut );
    
    CPPUNIT_ASSERT_MESSAGE( verOut, verOut == "1.0" );
    CPPUNIT_ASSERT_MESSAGE( client::toString(clientOut), clientOut == client::NONMEM );
    CPPUNIT_ASSERT_MESSAGE( "should be population", analysisOut == SpkParameters::POPULATION );

    contentNode->setAttribute( XMLString::transcode( "spkinml_ver" ), XMLString::transcode( "1.1" ) );
    contentNode->setAttribute( XMLString::transcode( "client" ), XMLString::transcode( client::STR_NOT_SUPPORTED ) );
    contentNode->setAttribute( XMLString::transcode( "analysis" ), XMLString::transcode( "individual" ) );

    read_content( contentNode, verOut, clientOut, analysisOut );
    CPPUNIT_ASSERT_MESSAGE( verOut, verOut == "1.1" );
    CPPUNIT_ASSERT_MESSAGE( client::toString(clientOut), clientOut == client::NOT_SUPPORTED );
    CPPUNIT_ASSERT_MESSAGE( "should be population", analysisOut == SpkParameters::INDIVIDUAL );

    XMLPlatformUtils::Terminate();
}


CppUnit::Test * read_contentTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "read_contentTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<read_contentTest>
			 ("test", &read_contentTest::test ) );

 return suiteOfTests;
}

