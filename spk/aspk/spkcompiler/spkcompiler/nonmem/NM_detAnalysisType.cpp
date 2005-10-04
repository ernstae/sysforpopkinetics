#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  detAnalysisType()
//  Determines the type of analysis and the number of subjects.
//
//  Pre-conditions
// 
//  * source points to a valid XMLDocument
//
//  Post-conditions
//
//  * ClientTranslator::ourTarget is set to either POP or IND
//  * ClientTranslator::ourPopSize is set to the number of subjects
//
/////////////////////////////////////////////////////////////////////////////////////////////
int NonmemTranslator::detAnalysisType()
{
  DOMElement  * spksource      = getSourceTree()->getDocumentElement();
  DOMNodeList * pop_analysises = spksource->getElementsByTagName( X_POP_ANALYSIS );
  DOMNodeList * ind_analysises = spksource->getElementsByTagName( X_IND_ANALYSIS );
  DOMElement  * analysis;
  unsigned int popSize = 0;
  enum TARGET target;

  if( pop_analysises->getLength() > 0 )
    {
      if( ind_analysises->getLength() > 0 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, 
		   "<%s> and <%s> elements cannot be found together in a sourceML document.", 
		   C_POP_ANALYSIS, C_IND_ANALYSIS );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      target = POP;
      analysis = dynamic_cast<DOMElement*>( pop_analysises->item(0) );
      assert( analysis != NULL );

      //
      // Finding out the population size
      //
      if( !analysis->hasAttribute( X_POP_SIZE ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing <%s::%s> attribute specification.", C_POP_ANALYSIS, C_POP_SIZE );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      const XMLCh * xml_pop_size = analysis->getAttribute( X_POP_SIZE );
      if( !XMLString::textToBin( xml_pop_size, popSize ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, 
		   "Invalid <%s::%s> attribute value: \"%s\"", C_POP_ANALYSIS, C_POP_SIZE,
		   XMLString::transcode(xml_pop_size) );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
    }
  else //( ind_analysises->getLength() > 0 )
    {
      target = IND;
      popSize = 1;

    }
  setTarget ( target );
  setPopSize( popSize );
  return popSize;
}
