#include <iostream>

#include "read_content.h"
#include <xercesc/dom/DOM.hpp>
#include "client.h"
#include "SpkParameters.h"
#include "SpkCompilerUtil.h"

using namespace std;
using namespace xercesc;

bool read_content( DOMDocument* tree, 
		   string & spkml_verOut, 
		   enum client::type & clientOut, 
		   enum SpkParameters::Analysis& analysisOut  )
{
  //
  // Get the version of SpkInML this document is supposed to comply with.
  //
  assert( tree != NULL );

  assert( tree->getElementsByTagName( X("content") ) != NULL );
  DOMElement * content_node
    = dynamic_cast<DOMElement*>(tree->getElementsByTagName( X("content") )->item(0));
  assert( content_node != NULL );

  //
  // Verify SpkInML version specification
  //
  const XMLCh* xml_spkinml_ver = content_node->getAttribute( X("spkinml_ver") );
  spkml_verOut = string( C( xml_spkinml_ver ) );
  
  //
  // Verify client specification
  //
  XMLCh* xml_client = XMLString::replicate( content_node->getAttribute( X("client") ) );
  XMLString::trim( xml_client );
  if( XMLString::equals( xml_client, X( client::STR_NONMEM ) ) )
    {
      clientOut = client::NONMEM;
    }
  else 
    {
      clientOut = client::NOT_SUPPORTED;
    }

  //
  // analysis level
  //
  const XMLCh* xml_analysis = content_node->getAttribute( X("analysis") );
  if( XMLString::equals( xml_analysis, X( "population" ) ) )
    analysisOut = SpkParameters::POPULATION;
  else if( XMLString::equals( xml_analysis, X( "individual" ) ) )
    analysisOut = SpkParameters::INDIVIDUAL;
  else
    return false;

  return true;
}
