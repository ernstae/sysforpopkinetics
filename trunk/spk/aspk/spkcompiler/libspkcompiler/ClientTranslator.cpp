#include <iostream>
#include <cstdlib>
#include <string>
#include <map>
#include <vector>
#include <fstream>
#include "ClientTranslator.h"
#include "SymbolTable.h"
#include "SpkCompilerException.h"

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
//   -- The 1st row (ie. position = 1 ) contains labels.
//
// <!ELEMENT value (#PCDATA)>
// <!ATTLIST value type (numeric|string) #IMPLIED>
//
//***************************************************************************************

/*
 * Insert the ID field if the data set lacks the field.
 * Returns the location (>=0) in which the ID field can be found.
 */

int ClientTranslator::insertID()
{
  //
  // Precondition: The number of individuals has been determined.
  //
  assert( ourPopSize > 0 );

  //
  // Precondition: The type of analysis has been determined.
  //
  assert( ourTarget == POP || ourTarget == IND );

  //
  // Determine if there's the ID field in the data set or not.
  //
  DOMElement * spkdata   = data->getDocumentElement();
  DOMNodeList * datasets = spkdata->getElementsByTagName( X_TABLE );
  assert( datasets->getLength() == 1 );
  DOMElement  * dataset  = dynamic_cast<DOMElement*>( datasets->item(0) );
  DOMNodeList * records  = dataset->getElementsByTagName( X_ROW );
  unsigned int pos = 0;
  const XMLCh * x_id_val;
  for( int i=0; i<records->getLength(); i++ )
    {
      const XMLCh * x_position = dynamic_cast<DOMElement*>(records->item(i))->getAttribute( X_POSITION );
      XMLString::textToBin( x_position, pos );
      if( pos == 1 )
	{
	  DOMNodeList * values = dynamic_cast<DOMElement*>(records->item(i))->getElementsByTagName( X_VALUE );
	  for( int j=0; j<values->getLength(); j++ )
	    {
	      const XMLCh* x_value = values->item(j)->getFirstChild()->getNodeValue();

	      // If there's the label, ID, return immediately.
	      if( XMLString::compareIString( x_value, X_ID ) == 0 )
		{
		  // The ID field is found in the j-th column.
		  return j;
		}
	    }
	  break;
	}      
    }


  //
  // If the data set is a population data and lacks the ID field,
  // then every record should be assigned to a different ID.
  //
  char id[ 56 ];
  if( ourTarget == POP )
    {
      for( int i=0; i<records->getLength(); i++ )
	{
	  const XMLCh * x_position = dynamic_cast<DOMElement*>(records->item(i))->getAttribute( X_POSITION );
	  XMLString::textToBin( x_position, pos );
	  if( pos == 1 )
	    {
	      x_id_val = X_ID;
	    }
	  else
	    {
	      sprintf( id, "%d", pos-1 );
	      x_id_val = XMLString::transcode( id );
	    }
	  DOMNodeList * values          = dynamic_cast<DOMElement*>(records->item(i))->getElementsByTagName( X_VALUE );
	  DOMNode     * firstValueNode  = values->item(0);
	  DOMElement  * newValueNode    = data->createElement( X_VALUE );
	  DOMText     * newTerminalNode = data->createTextNode( x_id_val );
	  newValueNode->appendChild( newTerminalNode );
	  records->item(i)->insertBefore( newValueNode, firstValueNode );
	}
    }
  //
  // If the data set is an individual data set and lacks the ID field,
  // all the records belongs to a subject whose ID=1.
  //
  else // ourTarget == IND
    {
      const XMLCh* X_1 = XMLString::transcode( "1" );
      for( int i=0; i<records->getLength(); i++ )
	{
	  const XMLCh * x_position = dynamic_cast<DOMElement*>(records->item(i))->getAttribute( X_POSITION );
	  XMLString::textToBin( x_position, pos );
	  if( pos == 1 )
	    {
	      x_id_val = X_ID;
	    }
	  else
	    {
	      x_id_val = X_1;
	    }
	  DOMNodeList * values          = dynamic_cast<DOMElement*>(records->item(i))->getElementsByTagName( X_VALUE );
	  DOMNode     * firstValueNode  = values->item(0);
	  DOMElement  * newValueNode    = data->createElement( X_VALUE );
	  DOMText     * newTerminalNode = data->createTextNode( x_id_val );
	  newValueNode->appendChild( newTerminalNode );
	  records->item(i)->insertBefore( newValueNode, firstValueNode );
	}
    }
  
  unsigned int nItems = 0;
  char c_nItemsPlus1[ 56 ];
  XMLString::textToBin( dataset->getAttribute( X_COLUMNS ),
			    nItems );
  sprintf( c_nItemsPlus1, "%d", nItems + 1 );
  dataset->setAttribute( X_COLUMNS, XMLString::transcode( c_nItemsPlus1)  );

  // The ID is found in the 1st column.
  return 0;
}
void ClientTranslator::parseData()
{

  const int locID = insertID();  
  // Post condition: There's one and only one ID field in the data set parse tree.

  //
  // Precondition: The symbol table has no entry yet for data labels.
  //
  assert( table.getLabels()->size() == 0 );

  //
  // Precondition: The number of individuals has been determined.
  //
  assert( ourPopSize > 0 );

  //
  // Precondition: The type of analysis has been determined.
  //
  assert( ourTarget == POP || ourTarget == IND );

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
         // continue;
         // For ver 0.1, this is an error!
	char m[ SpkCompilerError::maxMessageLen() ];
	sprintf( m, "Empty data set!" );
	SpkCompilerException e( SpkCompilerError::ASPK_DATAML_ERR, m, __LINE__, __FILE__ );
	throw e;
      }

      map< string, map<string, vector<string> > > tmp_values;
      vector<string> tmp_ids;
      vector<string> tmp_labels(nFields);
      vector<string> tmp_types (nFields);
      valarray<int>  nDataRecords;

      unsigned int pos;      
      DOMNodeList * rows = dataSet->getElementsByTagName( X_ROW );
      assert( rows->getLength() == nRecords );
      for( int j=0; j<nRecords; j++ )
	{
	  DOMElement  * row    = dynamic_cast<DOMElement*>( rows->item(j) );
	  const XMLCh* xml_position = row->getAttribute( X_POSITION );
	  assert( xml_position != NULL );
	  if( !XMLString::textToBin( xml_position, pos ) )
	    assert( false ); // pos = j
	  DOMNodeList * values = row->getElementsByTagName( X_VALUE );
	  assert( values->getLength() == nFields );

	  //
	  // At the first iteration, k=0, the value of *id is set and it is used for the
	  // rest of iterations.
	  //
	  const XMLCh* xml_value = values->item(locID)->getFirstChild()->getNodeValue();
	  XMLCh* xml_value_noWS = XMLString::replicate( xml_value );
	  XMLString::removeWS( xml_value_noWS );
	  char * id = XMLString::transcode( xml_value_noWS );
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

              //
              // If this item is an ID, check if the value changed since the previous iteration.
              // If it does, increment the #of subjects and keep the new ID for future reference.
	      //
	      if( k == locID )
		{
		  id = XMLString::transcode( xml_value_noWS );
		  if( find( tmp_ids.begin(), tmp_ids.end(), id ) == tmp_ids.end() )
		    {
		      tmp_ids.push_back( id );
		      ++nSubjects;
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
      assert( nSubjects == ourPopSize ); 
      assert( nSubjects == tmp_ids.size() );
      nDataRecords.resize( nSubjects );


      vector<string>::const_iterator pID = tmp_ids.begin();
      for( int k=0; pID != tmp_ids.end(), k<nSubjects; k++, pID++ )
	{
	  nDataRecords[k] = tmp_values[*pID][tmp_labels[0]].size();
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
      for( vector<string>::const_iterator pID = tmp_ids.begin(); pID != tmp_ids.end(); pID++, who++ )
	{
	  for( int k=0; k<nLabels; k++ )
	    {
	      Symbol *s = table.findi( tmp_labels[k] );
	      vector<string>::const_iterator itr = (tmp_values[*pID][tmp_labels[k]]).begin();
	      for( int l=0; itr != tmp_values[*pID][tmp_labels[k]].end(); l++, itr++ )
		{
		  s->initial[who][l] = tmp_values[*pID][tmp_labels[k]][l];
		}
	    }
	}
    }
}
/*
void ClientTranslator::parseData()
{
  insertID();
  //
  // Precondition: The symbol table has no entry yet for data labels.
  //
  assert( table.getLabels()->size() == 0 );

  //
  // Precondition: The number of individuals has been determined.
  //
  assert( ourPopSize > 0 );

  //
  // Precondition: The type of analysis has been determined.
  //
  assert( ourTarget == POP || ourTarget == IND );

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
      assert( nSubjects == ourPopSize ); 
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
*/
