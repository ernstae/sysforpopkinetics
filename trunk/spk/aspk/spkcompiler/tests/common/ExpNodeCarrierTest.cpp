#include <iostream>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include "ExpNodeCarrier.h"
#include "ExpNodeCarrierTest.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

void ExpNodeCarrierTest::setUp()
{
}
void ExpNodeCarrierTest::tearDown()
{
}
void ExpNodeCarrierTest::testExpNodeCarrier()
{
  try
    {
      XMLPlatformUtils::Initialize();
    }
  
  catch(const XMLException& toCatch)
    {
      char *pMsg = XMLString::transcode(toCatch.getMessage());
      cerr << "Error during Xerces-c Initialization.\n"
	   << "  Exception message:"
	   << pMsg;
      XMLString::release(&pMsg);
      throw;
    }
  const XMLCh* core = const_cast<XMLCh*>( XMLString::transcode("core") );
  assert( core != NULL );
  DOMImplementation * impl =  DOMImplementationRegistry::getDOMImplementation( core );
  assert( impl != NULL );
  DOMDocument * tree;
  try{
     tree = impl->createDocument(
				 0,               // root element namespace URI.
				 XMLString::transcode("Root"),            // root element name
				 0);              // document type object (DTD).
     assert( tree != NULL );
  }
  catch( const DOMException & e ) 
    {
      switch( e.code )
	{
	case DOMException::INVALID_CHARACTER_ERR: 
	  cerr << "The specified qualified name contains an illegal character." << endl;
	  break;

        case DOMException::NAMESPACE_ERR: 
	  cerr << "The qualifiedName is malformed, ";
	  cerr << "if the qualifiedName has a prefix and the namespaceURI is null, ";
          cerr << "or if the qualifiedName has a prefix that is xml and the ";
	  cerr << "namespaceURI is different from http://www.w3.org/XML/1998/namespace, ";
	  cerr << "or if the DOM implementation does not support the XML feature ";
	  cerr << "but a non-null namespace URI was provided, ";
	  cerr << "since namespaces were defined by XML." << endl;
	  break;

        case DOMException::WRONG_DOCUMENT_ERR: 
	  cerr << "Doctype has already been used with a different document or ";
	  cerr << "was created from a different implementation." << endl;
	  break;

        case DOMException::NOT_SUPPORTED_ERR: 
	  cerr << "Maybe the DOM implementations does support the XML feature, ";
	  cerr << "if they choose not to support this method. Other features ";
	  cerr << "introduced in the future, by the DOM WG or in extensions ";
	  cerr << "defined by other groups, may also demand support for this method; ";
	  cerr << "please consult the definition of the feature to see ";
	  cerr << "if it requires this method." << endl;
	  break;
	}
      CPPUNIT_ASSERT( false );
    }

  DOMElement * node = tree->createElement( XMLString::transcode("node1") );
  struct ExpNodeCarrier carrier;
  carrier.node = node; 

  CPPUNIT_ASSERT( strcmp( XMLString::transcode( carrier.node->getTagName() ), "node1" ) == 0 );
}

CppUnit::Test * ExpNodeCarrierTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "ExpNodeCarrierTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<ExpNodeCarrierTest>("testExpNodeCarrier",
						    &ExpNodeCarrierTest::testExpNodeCarrier ) );
   return suiteOfTests;

}

