#include "read_nonmem_data.h"
#include "../SymbolTable.h"
#include "../SpkParameters.h"

#include <xercesc/dom/DOM.hpp>
#include <map>
#include <string>
#include <iostream>
#include <valarray>
#include <vector>

using namespace xercesc;
using namespace std;

void read_nonmem_data( 
	xercesc::DOMElement* dataNode, 
        int nIndividuals,
	SymbolTable & table,
	map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> &label_alias_mappingOut,
	map<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT> data_forOut[],
	string order_id_pairOut[],
        struct SpkParameters & spkOut 
      )
{
  assert( dataNode != NULL );
  spkOut.nMeasurementsAll.resize( nIndividuals );
  std::vector<double> y_temp;

  //
  // Get the list of <individual> nodes.  Each <individual> node is the root
  // of that individual's data subtree and determine the number of 
  // sets (= #individuals) of data.
  //
  DOMDocument * doc = dataNode->getOwnerDocument();
  DOMTreeWalker * walker = doc->createTreeWalker(  dataNode,
						   DOMNodeFilter::SHOW_ELEMENT,
						   NULL,
						   false );
       
  //=================================================================================
  //
  // Traversing the DATA tree to gather information.
  //
  //=================================================================================

  //
  // Iterate through the list of <individual> blocks to register
  // label-alias mappings and to populate the data_forOut table.
  //
  //
  // <individual> tag comes with the following attributes:
  //
  // order  --- (optional) the order at which the individual's data shall be processed.
  // id     --- (required) the alpha-numerical value identifying the individual.
  // length --- (required) the number of measurements for every observation.
  //
  // and contains x number of <item> tags.
  // <item> tag comes with the following attributes:
  //
  // label  --- (required) the title used to refer to the observation.
  // synonym--- (optional) an alias for the label.
  // 
  // Either of the label or the synonym in each pair MUST be one of 
  // NONMEM-reserved words 
  // (see "$INPUT" section, p56, NONMEM User's Guide VIII, for a complete list).
  // If either of them is "skip" or "drop", ignore the entire measurement vector.
  //
  DOMElement * individual = dynamic_cast<DOMElement*>( walker->firstChild() );
  for( int i=0, y_pos=0; i<nIndividuals; i++ )
    {
      //      DOMElement * individual = dynamic_cast<DOMElement*>( individualsList->item(i) );

      //
      // First, take care the <individual> tag's attributes.
      //
      int nMeasurements = atoi( C( individual->getAttribute( X("length") ) ) );
      const string id = string( C( individual->getAttribute( X("id") ) ) );

      int order = i;
      const XMLCh* xml_order = individual->getAttribute( X("order") );
      if( !XMLString::isAllWhiteSpace( xml_order ) || xml_order != NULL )
	{
	  order = atoi( C( xml_order ) ) - 1;
	  assert( order >= 0 );
	}

      //
      // Map the processing order and the individual's identifier.
      //
      order_id_pairOut[order] = id;

      //
      // Next, get the entire subtree of <individual> and traverse
      // the tree to collect a data set for this individual.
      // 
      //           <individual>
      //                 |
      //                 |
      //                \|/
      //              <item> ---> <item> ---> <item> ---> <item> ---> ...   --+
      //                 |           |
      //                 |          \|/                                      |
      //                 |        <value> ---> <value> ---> ... --+         NULL
      //                \|/                                       |
      //              <value> ---> <value> ---> ... --+         NULL
      //                                              |
      //                                             NULL
      //
      /*
      DOMTreeWalker * walker = dataTree->createTreeWalker( individual,
							   DOMNodeFilter::SHOW_ELEMENT,
							   NULL,
							   false );
      */
      int nItems = 0;
      DOMElement * item = dynamic_cast<DOMElement*>( walker->firstChild() );
      for( nItems=0; item != NULL; ++nItems )
	{
	  // 
	  // Retrieve the attributes (label and synonym) for this item/column
	  // and register the pair in the label-alias map.
	  // 
	  // - label is required.
	  // - synonym is optional.
	  //
	  // Make sure, if no alias is defined, the entry associated with
	  // the key (label) must be set to NULL because the later operations
	  // will assume it's NULL.
	  //
	  // If either of them says "skip" or "drop", ignore the column 
	  // completely.
	  //
	  const XMLCh * xml_label = item->getAttribute( X("label" ) );
	  assert( !XMLString::isAllWhiteSpace( xml_label ) );
	  const XMLCh * xml_synonym = item->getAttribute( X("synonym") );

	  const XMLCh* X_SKIP = X("skip");
	  const XMLCh* X_DROP = X("drop");
	  if( XMLString::equals( xml_label, X_SKIP ) || XMLString::equals( xml_label, X_DROP )
	      || XMLString::equals( xml_synonym, X_SKIP ) || XMLString::equals( xml_synonym, X_DROP ) )
	    {
	      // This record is to be ignored.
	    }
	  else
	    {
	      const string label   = string( C( xml_label ) );
	      const string synonym = string( C( xml_synonym ) );
	      
	      label_alias_mappingOut[ label ] = synonym;
	      //
	      // Now, go though the set (column) of measurement data.
	      //
	      // The <value> subtree is an interesting one.  In the xml document
	      // it appears like this:
	      //   <value>1.0</value>
	      // so, it looks like the Node Value of <value> is "1.0".
	      // Wrong!  The value "1.0" is stored as the value of a text node (DOMText) 
	      // in a deeper level.
	      // 
	      // NOTE by Sachiko:
	      // Thought specifying DOMNodeFilter::SHOW_ELEMENT when creating a DOMTreeWalker
	      // would supress this DOMText representation but doesn't.  Why?
	      //
	      int nValues = 0;
	      valarray<double> values(nMeasurements);
	      DOMElement * valueTag = dynamic_cast<DOMElement*>( walker->firstChild() );
	      for( nValues=0; valueTag != NULL; ++nValues )
		{
		  //
		  // <value> tag could be empty, implying the value is supposed to be 0.0.
		  //
		  DOMText * val_node = dynamic_cast<DOMText*>( valueTag->getFirstChild() );
		  if( val_node != NULL )
		    {
		      values[nValues] = atof( C( trim( val_node->getNodeValue() ) ) );
		    }
		  else
		    {
		      values[nValues] = 0.0;
		    }
		  valueTag = dynamic_cast<DOMElement*>( walker->nextSibling() );
		}
	      assert( nMeasurements == nValues );
	      
	      //
	      // Register the label-values pair in the data_forOut map.
	      //
	      data_forOut[order].insert( 
		   pair<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT >
		   (label, values) );

	      Symbol Label( label, Symbol::VECTOR, Symbol::DOUBLE, true );
	      Label.size( nMeasurements );
	      table.insert( Label );
              
              if( label == "dv" || synonym == "dv" )
		{
		  spkOut.nMeasurementsAll[order] = nMeasurements;
                  for( int j=0; j<nMeasurements; j++ )
		    y_temp.push_back( values[j] );
		}      

	      walker->parentNode();
	    } 
	  item = dynamic_cast<DOMElement*>( walker->nextSibling() );
	}
      
      walker->parentNode();
      individual = dynamic_cast<DOMElement*>( walker->nextSibling() );
    }

  spkOut.measurementsAll.resize( y_temp.size() );
  for( int i=0; i<y_temp.size(); i++ )
    spkOut.measurementsAll[i] = y_temp[i];
}
