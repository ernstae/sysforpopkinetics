#include <iostream>
#include <cstdlib>
#include <string>
#include <map>
#include <vector>
#include <fstream>
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
  : source( sourceIn ), 
    data( dataIn ),
    X_SPKDATA    ( XMLString::transcode( "spkdata" ) ),
    X_VERSION    ( XMLString::transcode( "version" ) ),
    X_POINTONE   ( XMLString::transcode( "0.1" ) ),
    X_TABLE      ( XMLString::transcode( "table" ) ),
    X_COLUMNS    ( XMLString::transcode( "columns" ) ),
    X_ROWS       ( XMLString::transcode( "rows" ) ),
    X_DESCRIPTION( XMLString::transcode( "description" ) ),
    X_ROW        ( XMLString::transcode( "row" ) ),
    X_POSITION   ( XMLString::transcode( "position" ) ),
    X_VALUE      ( XMLString::transcode( "value" ) ),
    X_TYPE       ( XMLString::transcode( "type" ) ),
    X_NUMERIC    ( XMLString::transcode( "numeric" ) )
{
}
ClientTranslator::~ClientTranslator()
{
  XMLString::release( &X_SPKDATA );
  XMLString::release( &X_VERSION );
  XMLString::release( &X_POINTONE );
  XMLString::release( &X_TABLE );
  XMLString::release( &X_COLUMNS );
  XMLString::release( &X_ROWS );
  XMLString::release( &X_DESCRIPTION );
  XMLString::release( &X_ROW );
  XMLString::release( &X_POSITION );
  XMLString::release( &X_VALUE );
  XMLString::release( &X_TYPE );
  XMLString::release( &X_NUMERIC );
}
const SymbolTable* ClientTranslator::getSymbolTable() const
{
  return &table;
}
SymbolTable* ClientTranslator::getSymbolTable()
{
  return &table;
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
  // Precondition: The symbol table has no entry yet for data labels.
  //
  assert( table.getLabels()->size() == 0 );

  DOMElement * spkdata = data->getDocumentElement();
  assert( XMLString::equals( spkdata->getNodeName(), X_SPKDATA ) );
  const XMLCh* version = spkdata->getAttribute( X_VERSION );

  assert( XMLString::equals( version, X_POINTONE ) );

  //
  // Process through n number of <table>s, where n >= 0.
  // NOTE: For v0.1, n == 1.
  //
  DOMNodeList * dataTables = spkdata->getElementsByTagName( X_TABLE );
  int nDataTables = dataTables->getLength();
  assert( nDataTables == 1 );

  int nIDs = 0;
  for( int i=0; i<nDataTables; i++ )
    {
      DOMElement * dataTable = dynamic_cast<DOMElement*>( dataTables->item(i) );
      unsigned int nVals;
      XMLString::textToBin( dataTable->getAttribute( X_COLUMNS ),
			    nVals );
      unsigned int nRows;
      XMLString::textToBin( dataTable->getAttribute( X_ROWS ),
			    nRows );
      map< string, map<string, vector<string> > > tmp_values;
      vector<string> tmp_ids;
      string tmp_labels[ nVals ];
      string tmp_types [ nVals ];
      valarray<int>  nDataRecords;

      /*
      DOMNodeList * description = dataTable->getElementsByTagName( X_DESCRIPTION );
      assert( description == NULL || description->getLength() == 1 );
      const XMLCh* xml_descript = dynamic_cast<DOMText*>(description->item(0)->getFirstChild())->getNodeValue();
      char * descript = (XMLString::stringLen( xml_descript ) > 0 ? XMLString::transcode( xml_descript ) : NULL );
      */
      
      DOMNodeList * rows = dataTable->getElementsByTagName( X_ROW );
      assert( rows->getLength() == nRows );
      for( int j=0; j<nRows; j++ )
	{
	  DOMElement  * row    = dynamic_cast<DOMElement*>( rows->item(j) );
	  const XMLCh* xml_position = row->getAttribute( X_POSITION );
	  assert( xml_position != NULL );
	  unsigned int pos;
	  if( !XMLString::textToBin( xml_position, pos ) )
	    assert( false ); // pos = j
	  DOMNodeList * values = row->getElementsByTagName( X_VALUE );
	  assert( values->getLength() == nVals );

	  //
	  // At the first iteration, k=0, the value of *id is set and it is used for the
	  // rest of iterations.
	  //
	  char * id = NULL;
	  for( int k=0; k<nVals; k++ )
	    {
	      //
	      // The values in the first row (ie. position=1) are data labels.
	      //
	      if( pos==1 )
		{
		  const XMLCh* xml_label = values->item(k)->getFirstChild()->getNodeValue();
		  assert( xml_label != NULL );
                  char * delme = XMLString::transcode( xml_label );
		  tmp_labels[k] = string( delme );
                  delete delme;
		  continue;
		}

	      const XMLCh* xml_type;
	      if( dynamic_cast<DOMElement*>( values->item(k) )->hasAttribute( X_TYPE ) )
		{
		  xml_type = dynamic_cast<DOMElement*>( values->item(k) )->getAttribute( X_TYPE );
		}
	      else
		{
		  xml_type = X_NUMERIC;
		}
	      //
	      // The value types in the second row (ie. position=2) are used as reference
	      // against which the value types in the subsequent rows will be compared.
	      //
	      if( pos==2 )
		{
		  char * delme_c = XMLString::transcode(xml_type);
		  string delme_s( delme_c );
		  delete delme_c;
		  tmp_types[k] = delme_s;
		}	      

	      //
	      // For the subsequent rows (>2), the value types should match the entries in tmp_types[].
	      //
	      char * delme_c = XMLString::transcode( xml_type );
              string delme_s( delme_c );
	      delete delme_c;
	      assert( delme_s == tmp_types[k] );
	      
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
	      char * c_value = ( XMLString::stringLen( xml_value )>0? XMLString::transcode( xml_value ) : NULL );
	      tmp_values[id][tmp_labels[k]].push_back( string(c_value) );
	      delete c_value;
	    }
	  delete id;
	}
     
      assert( nIDs == tmp_ids.size() );
      nDataRecords.resize( nIDs );
      //
      // Figure out the number of data records for each individual
      // and save them in a temporary array.
      //
      vector<string>::const_iterator id = tmp_ids.begin();
      for( int k=0; id != tmp_ids.end(), k<=nIDs; k++, id++ )
	{
	  nDataRecords[k] = tmp_values[*id][tmp_labels[0]].size();
	}

      //
      // Register the data labels without any attributes yet.
      //
      for( int k=0; k<nVals; k++ )
	{
	  table.insertLabel( tmp_labels[k], "", nDataRecords );

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
	      Symbol *s = table.findi( tmp_labels[k] );
	      vector<string>::const_iterator itr = (tmp_values[*id][tmp_labels[k]]).begin();
	      for( int l=0; itr != tmp_values[*id][tmp_labels[k]].end(); l++, itr++ )
		{
		  s->initial[who][l] = tmp_values[*id][tmp_labels[k]][l];
		}
	    }
	}
    }
}
