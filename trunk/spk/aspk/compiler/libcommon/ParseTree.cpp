#include <iostream>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>

#include "ParseTree.h"
#include "NodeCarrier.h"
#include "SpkCompilerUtil.h"

using namespace std;
using namespace xercesc;

ParseTree::ParseTree()
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
  core = const_cast<XMLCh*>( X("core") );
  assert( core != NULL );
  impl =  DOMImplementationRegistry::getDOMImplementation( core );
  assert( impl != NULL );

  unit = const_cast<XMLCh*>( X("unit" ) );
  assert( unit != NULL );
  try{
    doc = impl->createDocument(
                    0,                    // root element namespace URI.
                    unit,            // root element name
                    0);                   // document type object (DTD).
  }
  catch( const DOMException & e ) 
    {
      switch( e.code )
	{
	case DOMException::INVALID_CHARACTER_ERR: 
	  cerr << "The specified qualified name contains an illegal character." << endl;
	  break;

        case DOMException::NAMESPACE_ERR: 
	  cerr << "The qualifiedName is malformed, if the qualifiedName has a prefix and the namespaceURI is null, or if the qualifiedName has a prefix that is xml and the namespaceURI is different from http://www.w3.org/XML/1998/namespace , or if the DOM implementation does not support the XML feature but a non-null namespace URI was provided, since namespaces were defined by XML." << endl;
	  break;

        case DOMException::WRONG_DOCUMENT_ERR: 
	  cerr << "Doctype has already been used with a different document or was created from a different implementation." << endl;
	  break;

        case DOMException::NOT_SUPPORTED_ERR: 
	  cerr << "Maybe the DOM implementations does support the XML feature, if they choose not to support this method. Other features introduced in the future, by the DOM WG or in extensions defined by other groups, may also demand support for this method; please consult the definition of the feature to see if it requires this method." << endl;
	  break;
	}
    }
  assert( doc != NULL );

  root = doc->getDocumentElement();
}
ParseTree::~ParseTree()
{
  releaseNodeCarriers();
  doc->release();
  XMLPlatformUtils::Terminate();
}
DOMDocument* ParseTree::handler() const
{
  return doc;
}
struct NodeCarrier* ParseTree::createNodeCarrier( )
{
  struct NodeCarrier * n = new NodeCarrier;
  nodes.push_back( n );
  return n;
}

int ParseTree::releaseNodeCarriers()
{
  int n = nodes.size();
  for( int i=0; i<n; i++ )
  {
    delete nodes[i];
  }
  nodes.erase( nodes.begin(), nodes.end() );
  return n;
}
void ParseTree::printToStdout() const
{
  XMLFormatTarget *myFormTarget = new StdOutFormatTarget();
  DOMWriter* writer = ((DOMImplementationLS*)impl)->createDOMWriter();
  
  if (writer->canSetFeature(XMLUni::fgDOMWRTFormatPrettyPrint, true))
    writer->setFeature(XMLUni::fgDOMWRTFormatPrettyPrint,true);

  writer->writeNode(myFormTarget, *root );

  delete writer;
  delete myFormTarget;

}
void ParseTree::printToFile( const char* filename ) const
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

