#include <iostream>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>

#include "ExpTreeGenerator.h"
#include "ExpNodeCarrier.h"
#include "SpkCompilerUtil.h"

using namespace std;
using namespace xercesc;

extern "C"{
  int yylex(void);  
  int yyparse(void);
};
extern int                gSpkExpLines;
extern int                gSpkExpErrors;
extern ExpTreeGenerator * gSpkExpTreeGenerator;
extern SymbolTable      * gSpkExpSymbolTable;
extern FILE             * yyin;
extern int                yydebug;


ExpTreeGenerator::ExpTreeGenerator()
  : isToTerminate( true ) // Remember that this object initialized XMLPlatformUtils.
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

  //
  // The string passed to getDOMImplementation is a list of features.  
  // "Core"... I do not know what it really means but do never change it!
  // 
  const XMLCh* core = const_cast<XMLCh*>( X("core") );
  assert( core != NULL );
  impl =  DOMImplementationRegistry::getDOMImplementation( core );
  assert( impl != NULL );

  doc = createTree( X( "unit" ) );
  assert( doc != NULL );
  root = doc->getDocumentElement();
}
ExpTreeGenerator::ExpTreeGenerator( bool shouldTerminate )
  : isToTerminate( shouldTerminate )
{
  //
  // The string passed to getDOMImplementation is a list of features.  
  // "Core"... I do not know what it really means but do never change it!
  // 
  const XMLCh* core = X("core");
  assert( core != NULL );
  impl =  DOMImplementationRegistry::getDOMImplementation( core );
  assert( impl != NULL );

  doc = createTree( X( "unit" ) );
  assert( doc != NULL );
  root = doc->getDocumentElement();
}
DOMDocument* ExpTreeGenerator::createTree( const XMLCh* unit )
{
  assert( unit != NULL );

  try{
     DOMDocument * newTree = impl->createDocument(
                    0,               // root element namespace URI.
                    unit,            // root element name
                    0);              // document type object (DTD).
     assert( newTree != NULL );
     return newTree;
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
      return NULL;
    }
}
ExpTreeGenerator::~ExpTreeGenerator()
{
  releaseExpNodeCarriers();
  doc->release();

  if( isToTerminate )
    XMLPlatformUtils::Terminate();
}
DOMDocument* ExpTreeGenerator::getRoot() const
{
  return doc;
}
struct ExpNodeCarrier* ExpTreeGenerator::createExpNodeCarrier( )
{
  struct ExpNodeCarrier * n = new ExpNodeCarrier;
  nodes.push_back( n );
  return n;
}

int ExpTreeGenerator::releaseExpNodeCarriers()
{
  int n = nodes.size();
  for( int i=0; i<n; i++ )
  {
    delete nodes[i];
  }
  nodes.erase( nodes.begin(), nodes.end() );
  return n;
}
void ExpTreeGenerator::printToStdout() const
{
  XMLFormatTarget *myFormTarget = new StdOutFormatTarget();
  DOMWriter* writer = ((DOMImplementationLS*)impl)->createDOMWriter();
  
  if (writer->canSetFeature(XMLUni::fgDOMWRTFormatPrettyPrint, true))
    writer->setFeature(XMLUni::fgDOMWRTFormatPrettyPrint,true);

  writer->writeNode(myFormTarget, *root );

  delete writer;
  delete myFormTarget;

}
void ExpTreeGenerator::printToFile( const char* filename ) const
{
  XMLFormatTarget *myFormTarget; // = new StdOutFormatTarget();
  myFormTarget = new LocalFileFormatTarget( filename );
  DOMWriter* writer = ((DOMImplementationLS*)impl)->createDOMWriter();

  if (writer->canSetFeature(XMLUni::fgDOMWRTFormatPrettyPrint, true))
    writer->setFeature(XMLUni::fgDOMWRTFormatPrettyPrint,true);

  writer->writeNode(myFormTarget, *root );

  delete writer;
  delete myFormTarget;

}

