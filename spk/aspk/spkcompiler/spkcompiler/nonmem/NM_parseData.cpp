#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"
#include "explang.h"
#include "countStrInLhs.h"
#include "../upper.h"
#include "../lower.h"
#include "../series.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include "../DOMPrint.h"

using namespace std;
using namespace xercesc;

/*
 * Look for a label/item.  If the label is found, return the position from the left (>=0).
 * If not found, return -1.
 */
int NonmemTranslator::whereis( DOMElement * dataset, const XMLCh* x_label ) const
{

  DOMNodeList * records  = dataset->getElementsByTagName( X_ROW );
  unsigned int recordNum = 0;
  for( int i=0; i<records->getLength(); i++ )
    {
      const XMLCh * x_position = dynamic_cast<DOMElement*>(records->item(i))->getAttribute( X_POSITION );
      XMLString::textToBin( x_position, recordNum );
      if( recordNum == 1 )
	{
	  DOMNodeList * values = dynamic_cast<DOMElement*>(records->item(i))->getElementsByTagName( X_VALUE );
	  for( int j=0; j<values->getLength(); j++ )
	    {
	      const XMLCh* x_value = values->item(j)->getFirstChild()->getNodeValue();

	      // If there's the label, return immediately.
	      if( XMLString::compareIString( x_value, x_label ) == 0 )
		{
		  // The ID field is found in the j-th column.
		  return j;
		}
	    }
	  break;
	}      
    }
  return -1;
}
/*
 * Insert the ID field if the data set lacks the field.
 * Returns the location (>=0) in which the ID field can be found.
 */

int NonmemTranslator::insertID( DOMElement * dataset )
{
  // If ID is defined in the data set, don't need to do anything.
  int posID;
  assert( whereis( dataset, X_ID ) < 0 );

  DOMNodeList * records  = dataset->getElementsByTagName( X_ROW );
  //
  // If the data set is a population data and lacks the ID field,
  // then every record should be assigned to a different ID.
  //
  unsigned int recordNum = 0;
  const XMLCh * x_id_val;
  char id[ 56 ];
  if( getTarget() == POP )
    {
      for( int i=0; i<records->getLength(); i++ )
	{
	  const XMLCh * x_position = dynamic_cast<DOMElement*>(records->item(i))->getAttribute( X_POSITION );
	  XMLString::textToBin( x_position, recordNum );
	  if( recordNum == 1 )
	    {
	      x_id_val = X_ID;
	    }
	  else
	    {
	      snprintf( id, 56, "%d", recordNum-1 );
	      x_id_val = XMLString::transcode( id );
	    }
	  DOMNodeList * values          = dynamic_cast<DOMElement*>(records->item(i))->getElementsByTagName( X_VALUE );
	  DOMNode     * firstValueNode  = values->item(0);
	  DOMElement  * newValueNode    = getDataTree()->createElement( X_VALUE );
	  DOMText     * newTerminalNode = getDataTree()->createTextNode( x_id_val );
	  newValueNode->appendChild( newTerminalNode );
	  records->item(i)->insertBefore( newValueNode, firstValueNode );
	}
    }
  //
  // If the data set is an individual data set and lacks the ID field,
  // all the records belongs to a subject whose ID=1.
  //
  else // getTarget() == IND
    {
      const XMLCh* X_1 = XMLString::transcode( "1" );
      for( int i=0; i<records->getLength(); i++ )
	{
	  const XMLCh * x_position = dynamic_cast<DOMElement*>(records->item(i))->getAttribute( X_POSITION );
	  XMLString::textToBin( x_position, recordNum );
	  if( recordNum == 1 )
	    {
	      x_id_val = X_ID;
	    }
	  else
	    {
	      x_id_val = X_1;
	    }
	  DOMNodeList * values          = dynamic_cast<DOMElement*>(records->item(i))->getElementsByTagName( X_VALUE );
	  DOMNode     * firstValueNode  = values->item(0);
	  DOMElement  * newValueNode    = getDataTree()->createElement( X_VALUE );
	  DOMText     * newTerminalNode = getDataTree()->createTextNode( x_id_val );
	  newValueNode->appendChild( newTerminalNode );
	  records->item(i)->insertBefore( newValueNode, firstValueNode );
	}
    }
  
  unsigned int nItems = 0;
  char c_nItemsPlus1[ 56 ];

  if( !dataset->hasAttribute( X_COLUMNS ) )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Missing \"%s::%s\" attribute specification in data.xml!\n", C_TABLE, C_COLUMNS );
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
    }
  XMLString::textToBin( dataset->getAttribute( X_COLUMNS ),
			    nItems );
  snprintf( c_nItemsPlus1, 56,"%d", nItems + 1 );
  dataset->setAttribute( X_COLUMNS, XMLString::transcode( c_nItemsPlus1)  );

  // The ID was inserted into the 1st column.
  return 0;
}
/*
 * Insert the AMT field if the data set lacks the field.
 *
 * AMT(i) = 0
 *
 * Returns the location (>=0) in which the AMT field can be found.
 */
int NonmemTranslator::insertAMT( DOMElement * dataset )
{
  // If AMT is defined in the data set, don't need to do anything.
  assert( whereis( dataset, X_AMT ) < 0 );

  DOMNodeList * records  = dataset->getElementsByTagName( X_ROW );
  unsigned int recordNum=0;
  const XMLCh* x_amt_val;
  const XMLCh* X_0 = XMLString::transcode( "0.0" );
  for( int i=0; i<records->getLength(); i++ )
    {
      const XMLCh * x_position = dynamic_cast<DOMElement*>(records->item(i))->getAttribute( X_POSITION );
      XMLString::textToBin( x_position, recordNum );
      DOMNodeList * values     = dynamic_cast<DOMElement*>(records->item(i))->getElementsByTagName( X_VALUE );
      if( recordNum == 1 )
	{
	  x_amt_val = X_AMT;
	}
      else
	{
	  x_amt_val = X_0;
	}

      //  values  -> item(0)          
      //             <value>ID</value>
      //          -> item(1)
      //             <value>TIME</value>
      //          -> item(2)
      //             <value>DV</value>
      //          -> item(3)
      //             <value>AMT</value>
      DOMNode     * firstValueNode  = values->item(0);
      DOMElement  * newValueNode    = getDataTree()->createElement( X_VALUE );
      DOMText     * newTerminalNode = getDataTree()->createTextNode( x_amt_val );
      newValueNode->appendChild( newTerminalNode );
      records->item(i)->appendChild( newValueNode );
    }
  
  unsigned int nItems = 0;
  char c_nItemsPlus1[ 56 ];
  if( !dataset->hasAttribute( X_COLUMNS ) )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Missing \"%s::%s\" attribute specification in data.xml!\n", C_TABLE, C_COLUMNS );
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
    }
  XMLString::textToBin( dataset->getAttribute( X_COLUMNS ),
			    nItems );
  snprintf( c_nItemsPlus1, 56, "%d", nItems + 1 );
  dataset->setAttribute( X_COLUMNS, XMLString::transcode( c_nItemsPlus1 )  );

  // The AMT was appended at the last.
  return nItems;
}

/*
 * Insert the MDV field if the data set lacks the field.
 *
 * Initialize MDV in the following way:
 *
 *  If EVID is given:
 *    MDV(i) = 0 if EVID(i) == 0
 *           = 1 if EVID(i) > 0
 *  When AMT is given but not EVID:
 *    MDV(i) = 0 for AMT(i) == 0
 *           = 1 for AMT(i) == 1
 *  No AMT, no EVID:
 *    MDV(i) = 0
 *
 * Returns the location (>=0) at which the MDV field is inserted.
 */
int NonmemTranslator::insertMDV( DOMElement * dataset, int posAMT, int posEVID )
{
  assert( whereis( dataset, X_MDV ) < 0 );

  DOMNodeList * records  = dataset->getElementsByTagName( X_ROW );

  unsigned int recordNum=0;
  const XMLCh* x_mdv_val;
  const XMLCh* X_0 = XMLString::transcode( "0" );
  const XMLCh* X_1 = XMLString::transcode( "1" );
  for( int i=0; i<records->getLength(); i++ )
    {
      const XMLCh * x_position = dynamic_cast<DOMElement*>(records->item(i))->getAttribute( X_POSITION );
      XMLString::textToBin( x_position, recordNum );
      DOMNodeList * values     = dynamic_cast<DOMElement*>(records->item(i))->getElementsByTagName( X_VALUE );
      if( recordNum == 1 )
	{
	  x_mdv_val = X_MDV;
	}
      else
	{
	  const char *c_amt = XMLString::transcode( 
			      dynamic_cast<DOMElement*>(values->item(posAMT))->getFirstChild()->getNodeValue() );
	  double amt = atof( c_amt );
	  if( amt == 0.0 )
	    x_mdv_val = X_0;
	  else
	    x_mdv_val = X_1;

	  if( posEVID >= 0 )
	    {
	      unsigned int evid = 0;
	      // Converting the value of EVID on i-th record to an integer.
	      XMLString::textToBin( dynamic_cast<DOMElement*>(values->item(posEVID))->getFirstChild()->getNodeValue(), evid );
	      if( evid == 0 )
		x_mdv_val = X_0;
	      else
		x_mdv_val = X_1;
	    }
	  else if( posAMT >= 0 )
	    {
	      const char *c_amt = XMLString::transcode( 
				     dynamic_cast<DOMElement*>(values->item(posAMT))->getFirstChild()->getNodeValue() );
              double amt = atof( c_amt );
	      if( amt == 0.0 )
		x_mdv_val = X_0;
	      else
		x_mdv_val = X_1;
	    }
	  else
	    {
	      x_mdv_val = X_0;
	    }
	}

      //  values  -> item(0)          
      //             <value>ID</value>
      //          -> item(1)
      //             <value>TIME</value>
      //          -> item(2)
      //             <value>DV</value>
      //          -> item(3)
      //             <value>EVID</value>
      DOMNode     * firstValueNode  = values->item(0);
      DOMElement  * newValueNode    = getDataTree()->createElement( X_VALUE );
      DOMText     * newTerminalNode = getDataTree()->createTextNode( x_mdv_val );
      newValueNode->appendChild( newTerminalNode );
      records->item(i)->appendChild( newValueNode );
    }
  
  unsigned int nItems = 0;
  char c_nItemsPlus1[ 56 ];
  if( !dataset->hasAttribute( X_COLUMNS ) )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Missing \"%s::%s\" attribute specification in data.xml!\n", C_TABLE, C_COLUMNS );
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
    }
  XMLString::textToBin( dataset->getAttribute( X_COLUMNS ),
			    nItems );
  snprintf( c_nItemsPlus1, 56, "%d", nItems + 1 );
  dataset->setAttribute( X_COLUMNS, XMLString::transcode( c_nItemsPlus1) );

  // The MDV was appended at the last.
  return nItems;
}
/*
 * Insert the EVID field if the data set lacks the field.
 *
 * If MDV is given:
 *   EVID=MDV
 *
 * If AMT is given but not MDV:
 *   EVID=0 if AMT=0
 *       =1 if AMT>0
 * Fill with 0 if neighther AMT or MDV is given.
 *
 * Returns the location (>=0) in which the EVID field can be found.
 */
int NonmemTranslator::insertEVID( DOMElement * dataset, int posAMT, int posMDV )
{
  assert( whereis( dataset, X_EVID ) < 0 );

  DOMNodeList * records  = dataset->getElementsByTagName( X_ROW );
  unsigned int recordNum=0;
  const XMLCh* x_evid_val;
  const XMLCh* X_0 = XMLString::transcode( "0" );
  const XMLCh* X_1 = XMLString::transcode( "1" );
  for( int i=0; i<records->getLength(); i++ )
    {
      const XMLCh * x_position = dynamic_cast<DOMElement*>(records->item(i))->getAttribute( X_POSITION );
      XMLString::textToBin( x_position, recordNum );
      DOMNodeList * values     = dynamic_cast<DOMElement*>(records->item(i))->getElementsByTagName( X_VALUE );
      if( recordNum == 1 )
	{
	  x_evid_val = X_EVID;
	}
      else
	{
	  if( posMDV >= 0 )
	    {
	      unsigned int mdv = 0;
	      // Converting the value of EVID on i-th record to an integer.
	      XMLString::textToBin( dynamic_cast<DOMElement*>(values->item(posMDV))->getFirstChild()->getNodeValue(), mdv );
	      if( mdv == 0 )
		x_evid_val = X_0;
	      else
		x_evid_val = X_1;
	    }
	  else if( posAMT >= 0 )
	    {
	      const char *c_amt = XMLString::transcode( 
				     dynamic_cast<DOMElement*>(values->item(posAMT))->getFirstChild()->getNodeValue() );
              double amt = atof( c_amt );
	      if( amt == 0.0 )
		x_evid_val = X_0;
	      else
		x_evid_val = X_1;
	    }
	  else
	    {
	      x_evid_val = X_0;
	    }
	}

      //  values  -> item(0)          
      //             <value>ID</value>
      //          -> item(1)
      //             <value>TIME</value>
      //          -> item(2)
      //             <value>DV</value>
      //          -> item(3)
      //             <value>MDV</value>
      DOMNode     * firstValueNode  = values->item(0);
      DOMElement  * newValueNode    = getDataTree()->createElement( X_VALUE );
      DOMText     * newTerminalNode = getDataTree()->createTextNode( x_evid_val );
      newValueNode->appendChild( newTerminalNode );
      records->item(i)->appendChild( newValueNode );
    }
  
  unsigned int nItems = 0;
  char c_nItemsPlus1[ 56 ];
  if( !dataset->hasAttribute( X_COLUMNS ) )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Missing \"%s::%s\" attribute specification in data.xml!\n", C_TABLE, C_COLUMNS );
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
    }
  XMLString::textToBin( dataset->getAttribute( X_COLUMNS ),
			    nItems );
  snprintf( c_nItemsPlus1, 56, "%d", nItems + 1 );
  dataset->setAttribute( X_COLUMNS, XMLString::transcode( c_nItemsPlus1 )  );

  // The EVID was appended at the last.
  return nItems;
}

void NonmemTranslator::parseData()
{
  SymbolTable * table = getSymbolTable();

  //
  // Precondition: The number of individuals has been determined.
  //
  if( getPopSize() <= 0 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Programming error!  The population size must have been determined!" );
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
    }

  //
  // Precondition: The type of analysis has been determined.
  //
  if( getTarget() != POP && getTarget() != IND )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Programming error!  The analysis type (individual/population) must have been determined!" );
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
    }

  //
  // Precondition: The symbol table has no entry yet for data labels.
  //
  if( table->getLabels()->size() != 0 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Programming error!  No labels should have been registered in the symbol table!" );
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
     }

  valarray<int> N;
  DOMElement * spkdata = getDataTree()->getDocumentElement();
  assert( XMLString::equals( spkdata->getNodeName(), X_SPKDATA ) );
  const XMLCh* version = spkdata->getAttribute( X_VERSION );

  if( !XMLString::equals( version, X_POINTONE ) )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Version %s is not supported yet!", version );
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
     }

  //
  // Process through n number of <table>s, where n >= 0.
  // NOTE: For v0.1, n == 1.
  //
  DOMNodeList * datasets = spkdata->getElementsByTagName( X_TABLE );
  int nDataSets = datasets->getLength();
  if( nDataSets < 1 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Missing a data set!" );
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
    }
  if( nDataSets > 1 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Multiple data sets not supported!" );
      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
    }

  int nSubjects = 0;
  for( int i=0; i<nDataSets; i++ )
    {
      DOMElement * dataset = dynamic_cast<DOMElement*>( datasets->item(i) );

      // Warning: Do not change the order of the following three calls.
      int  posID, posAMT, posMDV, posEVID;
      bool isID   = ( ( posID   = whereis( dataset, X_ID )   ) >= 0 );
      bool isAMT  = ( ( posAMT  = whereis( dataset, X_AMT )  ) >= 0 );
      bool isMDV  = ( ( posMDV  = whereis( dataset, X_MDV )  ) >= 0 );
      bool isEVID = ( ( posEVID = whereis( dataset, X_EVID ) ) >= 0 );
     
      // If ID is missing, add it with a value = 1.
      if( !isID )
	{
	  posID = insertID( dataset );
	}

      if( detModelType() != PRED )
	{
	  // If MDV is missing, there are more than one way to determine the default value.
	  // Follow the following logic:
	  //
	  //          Input                     Output
	  // (-=missing, x=present)       (0=0, 1=1, x=as_is)
	  // 
	  //   AMT     MDV    EVID        AMT     MDV    EVID
	  //    -       -       -          0       0       0
	  //    -       -       x          0      EVID     x
	  //    -       x       -          0       x      MDV
	  //    -       x       x          0       x       x
	  //    x       -       -          x      AMT     AMT
	  //    x       -       x          x      EVID     x 
	  //    x       x       -          x       x      MDV 
	  //    x       x       x          x       x       x 
	  // If AMT is missing, add it with a value = 0.0.
	  if( !isAMT )
	    {
	      posAMT = insertAMT( dataset );
	    }
	  /*
	  if( !isMDV )
	    {
	      //
	      // use EVID if EVID is present.
              //   MDV=0 if EVID=0
              //      =1 if EVID!=0
              // use AMT if EVID is not present and AMT is present
	      //   AMT=0 -> MDV=0
	      //      =1 -> MDV=1
	      // fill with 0 if neither AMT or EVID is present
	      //
	      posMDV = insertMDV( dataset, posAMT, posEVID );
	    }
	  if( !isEVID )
	    {
	      //
	      // Insert the EVID field if the data set lacks the field.
	      //
	      // If MDV is given:
	      //   EVID=MDV
	      //
	      // If AMT is given but not MDV:
	      //   EVID=0 if AMT=0
	      //       =1 if AMT>0
	      // Fill with 0 if neighther AMT or MDV is given.
	      //
	      // Returns the location (>=0) in which the EVID field can be found.
	      //	     
	      posEVID = insertEVID( dataset, posAMT, posMDV );
	    }
	  */
	}

      unsigned int nFields;
      if( !dataset->hasAttribute( X_COLUMNS ) )
	{
	  char m[ SpkCompilerError::maxMessageLen() ];
	  snprintf( m, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing \"%s::%s\" attribute specification in data.xml!\n", C_TABLE, C_COLUMNS );
	  throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
	}     
      XMLString::textToBin( dataset->getAttribute( X_COLUMNS ), nFields );

      unsigned int nRecords;
      if( !dataset->hasAttribute( X_ROWS ) )
	{
	  char m[ SpkCompilerError::maxMessageLen() ];
	  snprintf( m, 
		    SpkCompilerError::maxMessageLen(),
		    "Missing \"%s::%s\" attribute specification in data.xml!\n", C_TABLE, C_ROWS );
	  throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
	} 
      XMLString::textToBin( dataset->getAttribute( X_ROWS ), nRecords );

      if( nRecords == 0 )
      {
         // empty data set, skip to the next data set.
         // continue;
         // For ver 0.1, this is an error!
	char m[ SpkCompilerError::maxMessageLen() ];
	snprintf( m, 
		  SpkCompilerError::maxMessageLen(),
		  "Empty data set!" );
	SpkCompilerException e( SpkCompilerError::ASPK_USER_ERR, m, __LINE__, __FILE__ );
	throw e;
      }

      map< string, map<string, vector<string> > > tmp_values;
      vector<string> tmp_ids;
      vector<string> tmp_labels(nFields);
      vector<string> tmp_types (nFields);

      unsigned int pos;      
      DOMNodeList * rows = dataset->getElementsByTagName( X_ROW );
      if( rows->getLength() != nRecords )
	{
	  char m[ SpkCompilerError::maxMessageLen() ];
	  snprintf( m, 
		    SpkCompilerError::maxMessageLen(),
		    "The number of &lt;row&gt;s does not match with the value of &lt;table::rows&gt; attribute!" );
	  throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
	}

      for( int j=0; j<nRecords; j++ )
	{
	  DOMElement  * row    = dynamic_cast<DOMElement*>( rows->item(j) );
	  if( !row->hasAttribute( X_POSITION ) )
	    {
	      char m[ SpkCompilerError::maxMessageLen() ];
	      snprintf( m, 
			SpkCompilerError::maxMessageLen(),
			"An attribute, %s, is missing from &lt;%s&gt; tag.", X_POSITION, X_ROW );
	      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );	      
	    }
	  const XMLCh* xml_position = row->getAttribute( X_POSITION );
	  if( !XMLString::textToBin( xml_position, pos ) )
	    {
	      char m[ SpkCompilerError::maxMessageLen() ];
	      snprintf( m, 
			SpkCompilerError::maxMessageLen(),
			"Failed to convert an alphanumeric value to an integer!" );
	      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
	    }
	  DOMNodeList * values = row->getElementsByTagName( X_VALUE );
	  if( values->getLength() != nFields )
	    {
	      char m[ SpkCompilerError::maxMessageLen() ];
	      snprintf( m, 
			SpkCompilerError::maxMessageLen(),
			"The number of &lt;%s&gt; elements does not match with the value of &lt;%s&gt; attribute!",
		       X_VALUE, X_COLUMNS );
	      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
	    }

	  //
	  // At the first iteration, k=0, the value of *id is set and it is used for the
	  // rest of iterations.
	  //
	  const XMLCh* xml_value = values->item(posID)->getFirstChild()->getNodeValue();
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
		  if( find( tmp_labels.begin(), tmp_labels.end(), string(delme) ) != tmp_labels.end() )
		    {
		      char m[ SpkCompilerError::maxMessageLen() ];
		      snprintf( m, 
				SpkCompilerError::maxMessageLen(),
				"%s is already defined!", delme );
		      throw SpkCompilerException( SpkCompilerError::ASPK_USER_ERR, m, __LINE__, __FILE__ );
		    }
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
	      if( k == posID )
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
      if( (nSubjects > 1) && (getTarget() == IND) )
        {
          throw SpkCompilerException( SpkCompilerError::ASPK_USER_ERR, 
			      "Individual analysis was requested with a population data set!", __LINE__, __FILE__ );
        }

      assert( nSubjects == getPopSize() ); 
      assert( nSubjects == tmp_ids.size() );
      N.resize( nSubjects );


      vector<string>::const_iterator pID = tmp_ids.begin();
      for( int k=0; pID != tmp_ids.end(), k<nSubjects; k++, pID++ )
	{
	  N[k] = tmp_values[*pID][tmp_labels[0]].size();
	}
      
      //
      // Register the data labels without any attributes yet.
      //
      int nLabels = tmp_labels.size();
      for( int k=0; k<nLabels; k++ )
	{
	  table->insertLabel( tmp_labels[k], "", N );  
	}

      // Store the data item values permanently.
      // A Symbol object for a label maintains three vectors: initial, upper and lower.
      // Use "initial" vector to store the data item values for the label. 
      //
      // label ---> initial[i] ---> data for the i-th individual's "label" data item.
      //
      int who=0;
      for( vector<string>::const_iterator pID = tmp_ids.begin(); pID != tmp_ids.end(); pID++, who++ )
	{
	  for( int k=0; k<nLabels; k++ )
	    {
	      Symbol *s = table->findi( tmp_labels[k] );
	      vector<string>::const_iterator itr = (tmp_values[*pID][tmp_labels[k]]).begin();
	      for( int l=0; itr != tmp_values[*pID][tmp_labels[k]].end(); l++, itr++ )
		{
		  s->initial[who][l] = tmp_values[*pID][tmp_labels[k]][l];
		}
	    }
	}
    }
}
