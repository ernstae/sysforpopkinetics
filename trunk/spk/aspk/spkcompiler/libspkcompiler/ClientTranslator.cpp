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
  : source       ( sourceIn ), 
    data         ( dataIn ),
    ourPopSize   ( 0 ),
    ourApproximation( FO ),
    ourTarget    ( POP ),
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
    X_NUMERIC    ( XMLString::transcode( "numeric" ) ),
    X_ID         ( XMLString::transcode( "ID" ) )
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
  XMLString::release( &X_ID );
}
const SymbolTable* ClientTranslator::getSymbolTable() const
{
  return &table;
}
SymbolTable* ClientTranslator::getSymbolTable()
{
  return &table;
}
void ClientTranslator::translate()
{
  //
  // First of all, determine the number of individuals in the population.
  // This routine sets the analysis type (population/individual) as well.
  //
  detAnalysisType();

  parseData();
  parseSource();
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
  DOMNodeList * dataSets = spkdata->getElementsByTagName( X_TABLE );
  int nDataSets = dataSets->getLength();
  assert( nDataSets <= 1 );

  int nSubjects = 0;
  for( int i=0; i<nDataSets; i++ )
    {
      bool isIDMissing = false;
      DOMElement * dataSet = dynamic_cast<DOMElement*>( dataSets->item(i) );
      unsigned int nFields;
      XMLString::textToBin( dataSet->getAttribute( X_COLUMNS ),
			    nFields );
      unsigned int nRecords;
      XMLString::textToBin( dataSet->getAttribute( X_ROWS ),
			    nRecords );
      if( nRecords == 0 )
      {
         // empty data set, skip to the next data set.
         continue;
      }

      map< string, map<string, vector<string> > > tmp_values;
      vector<string> tmp_ids;
      vector<string> tmp_labels(nFields);
      vector<string> tmp_types (nFields);
      valarray<int>  nDataRecords;

      // REVISIT RETRUN SACHIKO
      // PRE-PROCESS
      //
      // Look for the first (ie. position=1) record and see if
      // it has "ID" as a string value as the first entry.
      //
      const DOMNodeList * records = dataSet->getElementsByTagName( X_ROW );
      unsigned int pos;
      const DOMNodeList * values;
      for( int j=0; j<records->getLength(); j++ )
      {
	const XMLCh* x_position = dynamic_cast<DOMElement*>(records->item(j))->getAttribute( X_POSITION );
	XMLString::textToBin( x_position, pos );
        if( pos == 1 )
	  {
	    values = dynamic_cast<DOMElement*>(records->item(j))->getElementsByTagName( X_VALUE );
            if( values->getLength() > 0 )
	    {
	      const XMLCh* x_value = values->item(0)->getFirstChild()->getNodeValue();
	      if( XMLString::compareIString( x_value, X_ID ) != 0 )
	      {
		isIDMissing = true;
		tmp_ids.push_back( "1" );
		nSubjects = 1;
              }
	    }
	  }
      }
      
      /*
      DOMNodeList * description = dataSet->getElementsByTagName( X_DESCRIPTION );
      assert( description == NULL || description->getLength() == 1 );
      const XMLCh* xml_descript = dynamic_cast<DOMText*>(description->item(0)->getFirstChild())->getNodeValue();
      char * descript = (XMLString::stringLen( xml_descript ) > 0 ? XMLString::transcode( xml_descript ) : NULL );
      */
      
      DOMNodeList * rows = dataSet->getElementsByTagName( X_ROW );
      assert( rows->getLength() == nRecords );
      for( int j=0; j<nRecords; j++ )
	{
	  DOMElement  * row    = dynamic_cast<DOMElement*>( rows->item(j) );
	  const XMLCh* xml_position = row->getAttribute( X_POSITION );
	  assert( xml_position != NULL );
	  unsigned int pos;
	  if( !XMLString::textToBin( xml_position, pos ) )
	    assert( false ); // pos = j
	  DOMNodeList * values = row->getElementsByTagName( X_VALUE );
	  assert( values->getLength() == nFields );

	  //
	  // At the first iteration, k=0, the value of *id is set and it is used for the
	  // rest of iterations.
	  //
	  char * id = NULL;
	  for( int k=0; k<nFields; k++ )
	    {
	      //
	      // The values in the first row (ie. position=1) are data labels.
	      //
	      if( pos==1 )
		{
		  const XMLCh* xml_label = values->item(k)->getFirstChild()->getNodeValue();
		  assert( xml_label != NULL );
		  // At this point xml_label may contain white spaces.
		  // Get rid of them!
		  XMLCh* xml_label_noWS = XMLString::replicate( xml_label );
		  XMLString::removeWS( xml_label_noWS );
                  char * delme = XMLString::transcode( xml_label_noWS );
		  tmp_labels[k] = string( delme );
                  delete delme;
		  XMLString::release( &xml_label_noWS );
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
	      XMLCh* xml_value_noWS = XMLString::replicate( xml_value );
	      XMLString::removeWS( xml_value_noWS );
	      if( k == 0 )
              {
		if( !isIDMissing )
		{
		  
		  id = XMLString::transcode( xml_value_noWS );
		  if( find( tmp_ids.begin(), tmp_ids.end(), id ) == tmp_ids.end() )
		    {
		      tmp_ids.push_back( id );
		      ++nSubjects;
		    }
		}
		  else
		  {
		    id = new char[2];
		    strcpy( id, "1" );
		  }
	      }
              //
              // If a data value is ".", that means 0.0.
              //
	      char * c_value = 
                 ( XMLString::stringLen( xml_value )>0? XMLString::transcode( xml_value_noWS ) : NULL );
              if( strcmp( c_value, "." ) == 0 )
                 tmp_values[id][tmp_labels[k]].push_back( "0.0" );
              else
	         tmp_values[id][tmp_labels[k]].push_back( string(c_value) );
	      delete c_value;
	      XMLString::release( &xml_value_noWS );		  
	    }
	  delete id;
	}
     
      assert( nSubjects == tmp_ids.size() );
      nDataRecords.resize( nSubjects );


      vector<string>::const_iterator id = tmp_ids.begin();
      for( int k=0; id != tmp_ids.end(), k<nSubjects; k++, id++ )
	{
	  nDataRecords[k] = tmp_values[*id][tmp_labels[0]].size();
	}
      
      //
      // Figure out the number of data records for each individual
      // and save them in a temporary array.
      //
      if( isIDMissing )
	{
	  nDataRecords[0] = nRecords - 1; // The first row is for labels, so don't count that row.
	  tmp_labels.insert( tmp_labels.begin(), "ID" );
	  tmp_values["1"]["ID"].resize( nDataRecords[0] );
	  fill( tmp_values["1"]["ID"].begin(), tmp_values["1"]["ID"].end(), "1" );
	}

      //
      // Register the data labels without any attributes yet.
      //
      int nLabels = tmp_labels.size();
       for( int k=0; k<nLabels; k++ )
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
      for( vector<string>::const_iterator id = tmp_ids.begin(); id != tmp_ids.end(); id++, who++ )
	{
	  for( int k=0; k<nLabels; k++ )
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
