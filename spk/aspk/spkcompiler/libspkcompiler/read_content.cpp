#include <iostream>
#include <map>

#include "read_content.h"
#include <xercesc/dom/DOM.hpp>
#include "client.h"
#include "SpkParameters.h"
#include "SpkCompilerUtil.h"

using namespace std;
using namespace xercesc;

pair<bool, bool> read_content( DOMElement * content_node, 
			       string & spkml_verOut, 
			       enum client::type & clientOut, 
			       enum SpkParameters::Analysis& analysisOut  )
{
  /*
  //
  // Get the version of SpkInML this document is supposed to comply with.
  //
  assert( tree != NULL );

  assert( tree->getElementsByTagName( X("content") ) != NULL );
  DOMElement * content_node
    = dynamic_cast<DOMElement*>(tree->getElementsByTagName( X("content") )->item(0));
  assert( content_node != NULL );
  */

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
  // checking analysis level
  //
  const XMLCh* xml_analysis = content_node->getAttribute( X("analysis") );
  if( XMLString::equals( xml_analysis, X( "population" ) ) )
    analysisOut = SpkParameters::POPULATION;
  else // if( XMLString::equals( xml_analysis, X( "individual" ) ) )
    analysisOut = SpkParameters::INDIVIDUAL;

  // checking if parameter estimation is requested.
  //
  bool isEstimation = false;
  const XMLCh* xml_estimation = content_node->getAttribute( X("estimation") );
  assert( xml_estimation != NULL );
  if( XMLString::equals( xml_estimation, X( "yes" ) ) )
    isEstimation = true;
  else 
    isEstimation = false;

  //
  // checking if data simulation is requested.
  //
  bool isSimulation = false;
  const XMLCh* xml_simulation = content_node->getAttribute( X("simulation") );
  assert( xml_simulation != NULL );
  if( XMLString::equals( xml_simulation, X( "yes" ) ) )
    isSimulation = true;
  else 
    isSimulation = false;

  return pair<bool, bool>( isEstimation, isSimulation );
}
