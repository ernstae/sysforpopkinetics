#include "read_nonmem_data.h"
#include "../SpkCompilerUtil.h"
#include "../SymbolTable.h"
#include "../SpkParameters.h"
#include <spk/SpkValarray.h>

#include <xercesc/dom/DOM.hpp>
#include <map>
#include <string>
#include <iostream>
#include <valarray>
#include <vector>

using SPK_VA::valarray;
using namespace xercesc;
using namespace std;

void read_nonmem_data( 
	xercesc::DOMElement* dataNode, 
        int nIndividuals,
	SymbolTable & table,
	map<string, string> &label_alias_mappingOut,
	vector< map<string, valarray<double> > > & data_forOut,
	string order_id_pairOut[],
        struct SpkParameters & spkOut 
      )
{
  assert( dataNode != NULL );
  spkOut.nMeasurements.resize( nIndividuals );
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
       
  assert( doc->getElementsByTagName( X("individual"))->getLength() == nIndividuals );
  //=================================================================================
  //
  // Traversing the DATA tree to gather information.
  //
  //=================================================================================

  //
  // Iterate through the list of <individual> blocks to register
  // label-alias mappings and to populate the data_forOut table.
  //
  // EVID and MDV are required by NONMEM (not to be confused with NM-TRAN).
  // They are useful anyway to weed in/out different types of records.
  // So, if they don't appear in the data set (data file), insert them
  // with values, nonmem::EVID_OBSERVATIONS = 0 and nonmem::MDV_NOT_MISSING = 0,
  // respectively.
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
      bool isDVgiven = false;
      //      DOMElement * individual = dynamic_cast<DOMElement*>( individualsList->item(i) );
      
      //
      // First, take care the <individual> tag's attributes.
      //
      int nMeasurements = atoi( C( individual->getAttribute( X("length") ) ) );
      assert( nMeasurements > 0 );
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
	      // This set is to be ignored.
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
		   pair<string, valarray<double> >
		   (label, values) );

	      Symbol Label( label, Symbol::VECTOR, Symbol::DOUBLE, true );
	      Label.size( nMeasurements );
	      table.insert( Label );
              
              if( label == "dv" || synonym == "dv" )
		{
		  isDVgiven = true;
                  for( int j=0; j<nMeasurements; j++ )
		    y_temp.push_back( values[j] );
		}      
	      spkOut.nMeasurements[order] = nMeasurements;

	      walker->parentNode();
	    } 
	  item = dynamic_cast<DOMElement*>( walker->nextSibling() );
	}
      // DV must appear for each individual's data set.
      if( !isDVgiven )
	{
	  fprintf( stderr, "DV is missing in %s's <individual>!\n", id.c_str() );
	  exit(-1);
	}
      
      walker->parentNode();
      individual = dynamic_cast<DOMElement*>( walker->nextSibling() );
    }

  //
  // This is "y".
  //
  spkOut.measurementsAll.resize( y_temp.size() );
  for( int i=0; i<y_temp.size(); i++ )
    spkOut.measurementsAll[i] = y_temp[i];

  //
  // If EVID column was not in the data set, that means all records are for
  // observations.
  //
  if( table.find( "evid" ) == NULL )
    {
      Symbol evid( "evid", Symbol::VECTOR, Symbol::DOUBLE, true );
      table.insert( evid );
      string label = "evid";
      label_alias_mappingOut[label] = "";
      for( int i=0; i<nIndividuals; i++ )
	{
	  valarray<double> values( nonmem::EVID_OBSERVATION, spkOut.nMeasurements[i] );
	  data_forOut[i].insert( pair<string, valarray<double> >( label, values ) );
	} 
    }
  //
  // If MDV column was not in the data set, that means all records have
  // observations.
  //
  if( table.find( "mdv" ) == NULL )
    {
      Symbol mdv( "mdv", Symbol::VECTOR, Symbol::DOUBLE, true );
      table.insert( mdv );
      string label = "mdv";
      label_alias_mappingOut[label] = "";
      for( int i=0; i<nIndividuals; i++ )
	{
	  valarray<double> values( nonmem::MDV_NOT_MISSING, spkOut.nMeasurements[i] );
	  data_forOut[i].insert( pair<string, valarray<double> >( label, values ) );
	} 
    }
}
