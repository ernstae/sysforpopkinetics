#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include <spkcompiler/ClientTranslator.h>
#include "ClientTranslatorTest.h"
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <spkcompiler/SpkParameters.h>
#include <vector>
#include <string>

using namespace std;
using namespace CppUnit;
using namespace xercesc;

void ClientTranslatorTest::setUp()
{
}

void ClientTranslatorTest::tearDown()
{
}

//
// ClientTranslator is an abstract class.
// It has to be derived.
//
class MyTranslator : public ClientTranslator
{
  vector<string> filenames;
  SpkParameters spk;

public:

  //
  // Upon the completion of this process, a SpkParameter object
  // and some data structure returned by getClientParameters().
  // may be ready to be downloaded but not required.
  // A list of file paths must be ready to donwloaded via
  // getFilenameList().
  //
  virtual void translate( xercesc::DOMDocument * pTree )
  {
    filenames.push_back( "driver.cpp" );
    filenames.push_back( "model.h" );
    filenames.push_back( "model.cpp" );
    filenames.push_back( "data.h" );
    filenames.push_back( "data.cpp" );
  }
  virtual const struct SpkParameters * getSpkParameters() const
  {
    return &spk;
  }
  virtual const void * getClientParameters() const
  {
    return NULL;
  }
  virtual const std::vector< std::string > getFilenameList() const
  {
    return filenames;
  }
};
void ClientTranslatorTest::testInterface()
{
  XMLPlatformUtils::Initialize();
  
  DOMImplementation* impl 
    =  DOMImplementationRegistry::getDOMImplementation(XMLString::transcode("Core"));
  
  DOMDocument* pTree = impl->createDocument(
					    0,                    // root element namespace URI.
					    XMLString::transcode("spkinml"),         // root element name
					    0);                   // document type object (DTD).

  MyTranslator xlator;

  xlator.translate( pTree );

  // This doesn't have to be a valid data structure.
  const void* clientpara = xlator.getClientParameters(); 

  // This doesn't have to be a valid data structure either.
  const SpkParameters * spk = xlator.getSpkParameters();

  // This, however, has to contain a list of filenames.
  const vector<string> files = xlator.getFilenameList();
  CPPUNIT_ASSERT( files.size() > 0 );

  pTree->release();
  XMLPlatformUtils::Terminate();
}


CppUnit::Test * ClientTranslatorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "ClientTranslatorTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<ClientTranslatorTest>
			 ("test", &ClientTranslatorTest::testInterface ) );

 return suiteOfTests;
}

