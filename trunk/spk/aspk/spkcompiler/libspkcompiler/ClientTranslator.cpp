#include <iostream>
#include <cstdlib>
#include <string>
#include <map>
#include <vector>
#include "ClientTranslator.h"
#include "SymbolTable.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

class TestTranslator : public ClientTranslator
{
public:
  virtual void convertSource(){};
};
ClientTranslator::ClientTranslator()
{
}
ClientTranslator::ClientTranslator( const ClientTranslator& )
{
}
ClientTranslator & ClientTranslator::operator=( const ClientTranslator& )
{
}
ClientTranslator::ClientTranslator( DOMDocument* sourceIn, DOMDocument* dataIn )
  : source( sourceIn ), data( dataIn )
{
}
ClientTranslator::~ClientTranslator()
{
}
const SymbolTable* ClientTranslator::getSymbolTable() const
{
  return &stable;
}
//***************************************************************************************
//
// <!ELEMENT spkdata (table)*>
// <!ATTLIST spkdata version CDATA #REQUIRED>
//
// <!ELEMENT table (description? | row*)>
// <!ATTLIST table columns CDATA #REQUIRED>
// <!ATTLIST table rows CDATA #REQUIRED>
//
// <!ELEMENT description (#PCDATA)>
//
// <!ELEMENT row (value)*>
// <!ATTLIST row position CDATA #REQUIRED>
//
// <ELEMENT value (#PCDATA)>
// <!ATTLIST value type (numeric|string) #IMPLIED>
//
//***************************************************************************************
void ClientTranslator::parseData()
{
  //
  // The precondition: The symbol table has no entry yet for data labels.
  //
  assert( stable.getLabels().size() == 0 );

  DOMElement * spkdata = data->getDocumentElement();
  assert( XMLString::equals( spkdata->getNodeName(), XMLString::transcode("spkdata") ) );
  const XMLCh* version = spkdata->getAttribute( XMLString::transcode("version") );
  assert( XMLString::equals( version, XMLString::transcode("0.1") ) );

  //
  // Process through n number of <table>s, where n >= 0.
  // NOTE: For v0.1, n == 1.
  //
  DOMNodeList * tables = spkdata->getElementsByTagName( XMLString::transcode("table") );
  int nTables = tables->getLength();
  assert( nTables == 1 );

  for( int i=0, nIDs=0; i<nTables; i++ )
    {
      DOMElement * table = dynamic_cast<DOMElement*>( tables->item(i) );
      unsigned int nVals;
      XMLString::textToBin( table->getAttribute( XMLString::transcode("columns") ),
			    nVals );
      unsigned int nRows;
      XMLString::textToBin( table->getAttribute( XMLString::transcode("rows") ),
			    nRows );
      map< string, map<string, vector<string> > > tmp_values;
      vector<string> tmp_ids;
      string tmp_labels[ nVals ];
      string tmp_types [ nVals ];
      vector<int>    tmp_nDataRecords;

      DOMNodeList * description = table->getElementsByTagName( XMLString::transcode("description") );
      assert( description == NULL || description->getLength() == 1 );
      const XMLCh* xml_descript = dynamic_cast<DOMText*>(description->item(0)->getFirstChild())->getNodeValue();
      string descript = (xml_descript!=NULL? XMLString::transcode( xml_descript ) : "" );
      
      DOMNodeList * rows = table->getElementsByTagName( XMLString::transcode("row") );
      assert( rows->getLength() == nRows );
      for( int j=0; j<nRows; j++ )
	{
	  string id;
	  DOMElement  * row    = dynamic_cast<DOMElement*>( rows->item(j) );
	  const XMLCh* xml_position = row->getAttribute( XMLString::transcode("position") );
	  assert( xml_position != NULL );
	  unsigned int pos;
	  if( !XMLString::textToBin( xml_position, pos ) )
	    assert( false ); // pos = j
	  DOMNodeList * values = row->getElementsByTagName( XMLString::transcode("value") );
	  assert( values->getLength() == nVals );
	  for( int k=0; k<nVals; k++ )
	    {
	      if( pos==1 )
		{
		  const XMLCh* xml_label = values->item(k)->getFirstChild()->getNodeValue();
		  assert( xml_label != NULL );
		  tmp_labels[k] = XMLString::transcode( xml_label );
		  continue;
		}
	      if( pos==2 )
		{
		  const XMLCh* xml_type = 
		    dynamic_cast<DOMElement*>( 
					      values->item(k) )->getAttribute( XMLString::transcode("type") );
		  tmp_types[k] = ( xml_type==NULL? "numeric" : XMLString::transcode(xml_type) );
		}
	      const XMLCh* xml_type = dynamic_cast<DOMElement*>( 
								values->item(k) )->getAttribute( XMLString::transcode("type") );
	      assert( XMLString::transcode( xml_type ) == tmp_types[k] );
	      const XMLCh* xml_value = values->item(k)->getFirstChild()->getNodeValue();
              if( k == 0 )
	      {
		// This is the ID.
		id = XMLString::transcode( xml_value );
		if( find( tmp_ids.begin(), tmp_ids.end(), id ) == tmp_ids.end() )
		  {
		    tmp_ids.push_back( id );
		    ++nIDs;
		  }
	      }
	      tmp_values[id][tmp_labels[k]].push_back (xml_value==NULL? "" : XMLString::transcode(xml_value) );
	    }
	}
     
      assert( nIDs == tmp_ids.size() );

      //
      // Figure out the number of data records for each individual
      // and save them in a temporary array.
      //
      vector<string>::const_iterator id = tmp_ids.begin();
      for( int k=0; id != tmp_ids.end(); k++, id++ )
	{
	  tmp_nDataRecords.push_back( tmp_values[*id][tmp_labels[0]].size() );
	}

      //
      // Register the data labels without any attributes yet.
      //
      for( int k=0; k<nVals; k++ )
	{
	  stable.insertLabel( tmp_labels[k], "", tmp_nDataRecords );

	}

      //
      // Move the extracted (from the parse tree) info into the symbol table.
      //
      // NOTE: The actual values in tmp_ids are stored in the same order they appeared
      // in the table specification.  ie. tmp_ids[0] contains the first individual's ID.
      //
      int who=0;
      for( id = tmp_ids.begin(); id != tmp_ids.end(); id++, who++ )
	{
	  for( int k=0; k<nVals; k++ )
	    {
	      Symbol *s = stable.findi( tmp_labels[k] );
	      vector<string>::const_iterator itr = (tmp_values[*id][tmp_labels[k]]).begin();
	      for( int l=0; itr != tmp_values[*id][tmp_labels[k]].end(); l++, itr++ )
		{
		  s->initial[who][l] = tmp_values[*id][tmp_labels[k]][l];
		}
	    }
	}
    }
}

