/**
 * @file NM_parseMonte.cpp
 * Define NonmemTranslator::parseMonte().
 */
#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

//=============================================================================
//
// parse <monte_carlo>
//
// Pre-conditions - (Nothing really)
//
// Post-condtions -  The following variables are set to valid values:
//                     myIntegMethod
//                     myIntegNEvals
//                     myIntegNumberEvals (vector)
//
//=============================================================================
void NonmemTranslator::parseMonte( const DOMElement* monte_carlo )
{
  assert( monte_carlo != NULL );
  if( monte_carlo->hasAttribute( XML.X_METHOD ) )
    {
      const XMLCh* x_temp = monte_carlo->getAttribute( XML.X_METHOD );
      if( XMLString::equals( x_temp, XML.X_ADAPT ) )
	myIntegMethod = ADAPT;
      else if( XMLString::equals( x_temp, XML.X_GRID ) )
	myIntegMethod = GRID;
      else if( XMLString::equals( x_temp, XML.X_MISER ) )
	myIntegMethod = MISER;
      else if( XMLString::equals( x_temp, XML.X_VEGAS ) )
	myIntegMethod = VEGAS;
      else if( XMLString::equals( x_temp, XML.X_PLAIN ) )
	myIntegMethod = PLAIN;
      else
      {  char method_in_file[21];
	 char mess[ SpkCompilerError::maxMessageLen() ];

         // convert from XMLString to string
         XMLString::transcode(x_temp, method_in_file, 20);

         // construct the error message
         snprintf( mess, 
	   SpkCompilerError::maxMessageLen(),
	   "<%s::%s> attribute = %s", 
           XML.C_MONTE_CARLO, 
           XML.C_METHOD, 
           method_in_file
         );
         SpkCompilerException e( 
           SpkCompilerError::ASPK_SOURCEML_ERR, 
           mess,
	   __LINE__,
           __FILE__
         );
         throw e;
      }
    }
  else
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s::%s> attribute.", XML.C_MONTE_CARLO, XML.C_METHOD );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess,
			      __LINE__, __FILE__ );
      throw e;
    }
  DOMNodeList * number_evals = monte_carlo->getElementsByTagName( XML.X_NUMBEREVAL );
  if( number_evals->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element!", XML.C_NUMBEREVAL );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
			      mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement  * number_eval  = dynamic_cast<DOMElement*>( number_evals->item(0) );
  DOMNodeList * value_list = number_eval->getElementsByTagName( XML.X_VALUE );
  myIntegNEvals = value_list->getLength();
  if( myIntegNEvals < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <%s> element!",
	       XML.C_VALUE );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
			      mess, __LINE__, __FILE__ );
      throw e;
    }

  // Only when the integration method is GRID, the number of function evaluations is
  // something other then 1.  Actually it is equal to the order of OMEGA (ie. the length of ETA).
  if( myIntegMethod == GRID )
    {
      if( myIntegNEvals != myEtaLen )
        {
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "The number <%s> elements must be equal to the length of ETA (%d) for grid and miser approximation!", 
		   XML.C_VALUE, myEtaLen );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
				  mess, __LINE__, __FILE__ );
	  throw e;
        }
    }
  else // adapt, plain, miser, vegas
    {
      // For these methods, ignore what the user says.
      // They take only one and the first occurence of <number_eval>.
      myIntegNEvals = 1;
    }
  myIntegNumberEvals.resize( myIntegNEvals );
  for( int i=0; i<myIntegNEvals; i++ )
    {
      DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
      const XMLCh * x_value = value->getFirstChild()->getNodeValue();
      unsigned int temp_value = 0;
      XMLString::textToBin( x_value, temp_value );
      myIntegNumberEvals[i] = temp_value;
    }
}
