#include <iostream>
#include <valarray>
#include <fstream>

#include "read_nonmem_dataTest.h"

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <spk/printInMatrix.h>
#include <spk/SpkValarray.h>
#include <spkcompiler/nonmem/read_nonmem_data.h>

using SPK_VA::valarray;
using namespace std;
using namespace CppUnit;
using namespace xercesc;

void read_nonmem_dataTest::setUp()
{
  XMLPlatformUtils::Initialize();
}
void read_nonmem_dataTest::tearDown()
{
  XMLPlatformUtils::Terminate();
}
void read_nonmem_dataTest::createDataTree(
   DOMDocument* doc,
   int nIndividuals,  char * str_order[], char * id     [], char * str_len[],
   int nObservations, char * label    [], char * synonym[],
   int nMeasurements, char * str_value[] )
{  

  //
  // This initializer does not insert "evid" or "mdv" columns.
  //

  DOMElement * individuals [nIndividuals +1];
  DOMElement * observations[nObservations+1];
  DOMElement * measurements[nMeasurements+1];

  DOMElement* rootElem  = doc->getDocumentElement();  

  DOMElement* dataNode  = doc->createElement( XMLString::transcode( "data" ) );
  rootElem->appendChild( dataNode ); 

  for( int i=0; i<nIndividuals; i++ )
    {
      individuals[i] = doc->createElement( XMLString::transcode( "individual" ) );
      individuals[i]->setAttribute( 
			 XMLString::transcode("order"), 
			 XMLString::transcode( str_order[i] ) );
      individuals[i]->setAttribute( 
                         XMLString::transcode("id"),    
			 XMLString::transcode( id[i] ) );
      individuals[i]->setAttribute( 
                         XMLString::transcode("length"),
			 XMLString::transcode( str_len[i] ) );
      
      for( int j=0; j<nObservations; j++ )
	{
	  observations[j] = doc->createElement( XMLString::transcode( "item" ) );
	  observations[j]->setAttribute(
			      XMLString::transcode("label"),   
			      XMLString::transcode( label  [j] ) );
	  observations[j]->setAttribute( 
                              XMLString::transcode("synonym"), 
			      XMLString::transcode( synonym[j] ) );
	  
	  for( int k=0; k<nMeasurements; k++ )
	    {
	      measurements[k] = doc->createElement( XMLString::transcode( "value" ) );
	      measurements[k]->appendChild(
                                  doc->createTextNode( 
				          XMLString::transcode( 
					     str_value[i*nObservations*nMeasurements+j*nMeasurements+k] ) ) );
	      observations[j]->appendChild( measurements[k] );
	    }
	  individuals[i]->appendChild( observations[j] );
	}
      dataNode->appendChild( individuals[i] );
    }

  DOMWriter * writer = ((DOMImplementationLS*)impl)->createDOMWriter();
  writer->setNewLine( XMLString::transcode("\n") );
  StdOutFormatTarget destination;
  //  writer->writeNode( &destination, *rootElem );
}
//
// Tests if read_nonmem_data() can properly handles a data set
// in which individuals' data sets are displaced in order:
// In other words, each individual data set specifies a specific
// processing order which is supposed to override the
// order in which the data set appears in the document.
// 
void read_nonmem_dataTest::testOrderMixedUp()
{
  const int nIndividuals  = 4;
  const int nObservations = 3;
  const int nMeasurements = 2;

  char *str_order[ nIndividuals  + 1 ];
  char *id       [ nIndividuals  + 1 ];
  char *str_len  [ nIndividuals  + 1 ];

  char *label    [ nObservations + 1 ];
  char *synonym  [ nObservations + 1 ];

  char *str_value[ nIndividuals * nObservations * nMeasurements + 1 ];

  for( int j=0; j<nObservations; j++ )
    {
      label[j]   = new char[20];
      sprintf( label[j],   "label_%03d", j+1 );
      synonym[j] = new char[20];
      
      //
      // Data set must contain DV data.  So, set the first column to DV.
      //
      if( j == 0 )
	sprintf( synonym[j], "dv" );
      else
	sprintf( synonym[j], "alias_%03d", j+1 );
    }
  for( int i=0; i<nIndividuals; i++ )
    {
      // Flip around the order of process.
      str_order[i] = new char[20];
      sprintf( str_order[i], "%d",   nIndividuals-i );
      id[i]        = new char[20];
      sprintf( id[i],        "%03d", i+1 );

      str_len[i]   = new char[20];
      sprintf( str_len[i],   "%d",   nMeasurements );

      for( int j=0; j<nObservations; j++ )
	{
	  for( int k=0; k<nMeasurements; k++ )
	    {
	      str_value[i*nObservations*nMeasurements+j*nMeasurements+k] 
		= new char[20];
	      sprintf( str_value[i*nObservations*nMeasurements+j*nMeasurements+k], 
		       "%f", 
		       static_cast<double>(i*nObservations*nMeasurements+j*nMeasurements+k) );
	    }
	}
    }

  impl =  DOMImplementationRegistry::getDOMImplementation(XMLString::transcode("Core"));
  
  doc = impl->createDocument(
		0,                               // root element namespace URI.
		XMLString::transcode("spkinml"), // root element name
		0);                              // document type object (DTD).
  createDataTree( doc, 
		  nIndividuals, str_order, id, str_len, 
		  nObservations, 
		  label, synonym, 
		  nMeasurements, str_value );
  DOMElement * dataNode = dynamic_cast<DOMElement*>(
	     doc->getElementsByTagName( XMLString::transcode( "data" ) )->item(0) );

  /*
  DOMWriter * writer = ((DOMImplementationLS*)impl)->createDOMWriter();
  writer->setNewLine( XMLString::transcode("\n") );
  StdOutFormatTarget destination;
  writer->writeNode( &destination, *dataNode );
  */

  struct NonmemParameters nonmem;
  struct SpkParameters spk;

  spk.nIndividuals = nIndividuals;

  //
  // This table maps labels and corresponding aliases.
  // When no alias is defined for a label, the entry (alias) field shall contain
  // an empty string.
  //
  //     key          entry
  //   (label)       (alias)
  // +---------+   +---------+
  // | label_a |---| alias_a |
  // +---------+   +---------+
  // | label_b |---|   ""    |
  // +---------+   +---------+
  //      .             .
  //      .             .
  //      .             .
  // +---------+   +---------+
  // | label_x |   | alias_x |
  // +---------+   +---------+
  //
  map<string, string> label_alias_mapping;


  //
  // This table is used to record the following map:
  //
  //
  //     index          key                 entry
  // (individual#)    (label)           (measurements)
  //    +---+       +---------+    +-----+-----+-----+-----+
  //    | 0 |------>| label_a |--->| 0.0 | 1.0 | 2.0 | 2.3 |...
  //    +---+       +---------+    +-----+-----+-----+-----+
  //    | 1 |       | label_b |
  //    +---+       +---------+
  //      .         | label_c |
  //      .         +---------+
  //      .             ...
  //      .  
  //    +---+       +---------+    +-----+-----+-----+-----+
  //    | n |------>| label_a |--->| 0.0 | 1.2 | 1.9 | 2.3 |...
  //    +---+       +---------+    +-----+-----+-----+-----+
  //                | label_b |
  //                +---------+
  //                | label_c |
  //                +---------+
  //                    ...
  // 
  vector< map<string, valarray<double>  > > data_for( nIndividuals );

  //
  // This table records the processing order vs. the identifier pair of each
  // individual.
  //
  string order_id_pair[ nIndividuals +1 ];

  SymbolTable table;

  read_nonmem_data( dataNode,
		    nIndividuals,
		    table,
		    label_alias_mapping, 
		    data_for,
		    order_id_pair,
		    spk );

  for ( int i=0; i<nIndividuals; i++ )
    {
      CPPUNIT_ASSERT_EQUAL( nMeasurements, spk.nMeasurements[i] );
    }

  int nColumns = nObservations + 2; // plus the column for EVID

  CPPUNIT_ASSERT( table.find( "evid" ) != NULL );
  CPPUNIT_ASSERT( table.find( "mdv" ) != NULL );

  // EVID columns should've been added, so the size is plus one.
  CPPUNIT_ASSERT_EQUAL( nColumns, (int)label_alias_mapping.size() );
  
  //  cout << endl;
  for( int i=0; i<nIndividuals; i++ )
    {
      char id[20];
      sprintf( id, "%03d", nIndividuals-i );
      CPPUNIT_ASSERT_MESSAGE( order_id_pair[i], strcmp( id, order_id_pair[i].c_str()) == 0 );
      CPPUNIT_ASSERT_EQUAL( nColumns, (int)data_for[i].size() );

      //      cout << "order = " << i << ", ID = " << id << endl;
      map<string, valarray<double> >::const_iterator column 
	= data_for[i].begin();

      for( int j=0, real=0; j<nColumns, column != data_for[i].end(); j++, column++ )
	{
	  if( column->first == "evid" )
	    {
	      for( int k=0; k<nMeasurements; k++ )
		{
		  CPPUNIT_ASSERT_EQUAL( nonmem::EVID_OBSERVATION, column->second[k] );
		}
	    }
	  else if( column->first == "mdv" )
	    {
	      for( int k=0; k<nMeasurements; k++ )
		{
		  CPPUNIT_ASSERT_EQUAL( nonmem::MDV_NOT_MISSING, column->second[k] );
		}
	    }
	  else
	    {

	      char label[20];
	      char alias[20];
	      sprintf( label, "label_%03d", real+1 );
	      if( real == 0 )
		sprintf( alias, "dv" );
	      else
		sprintf( alias, "alias_%03d", real+1 );
	      CPPUNIT_ASSERT_MESSAGE( label_alias_mapping[label].c_str(), 
		 strcmp( label_alias_mapping[label].c_str(), alias ) == 0 );

	      for( int k=0; k<nMeasurements; k++ )
		{
		  CPPUNIT_ASSERT_EQUAL( static_cast<double>( 
		     (nIndividuals-i-1)*(nObservations)*nMeasurements+real*nMeasurements+k),
			column->second[k]);
		}
	      ++real;
	    }
	}
    }
  
  //cout << endl;
  for( int j=0; j<nObservations; j++ )
  {
    delete label[j];
    delete synonym[j];
  }
  for( int i=0; i<nIndividuals; i++ )
    {
      delete str_order[i];
      delete str_len[i];
      delete id[i];
      for( int j=0; j<nObservations; j++ )
	{
	  for( int k=0; k<nMeasurements; k++ )
	    {
	      delete str_value[i*(nObservations)*nMeasurements+j*nMeasurements+k];
	    }
	}
    }
}

CppUnit::Test * read_nonmem_dataTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "read_nonmem_dataTest" );

  suiteOfTests->addTest( new CppUnit::TestCaller<read_nonmem_dataTest>("testOrderMixedUp",
						    &read_nonmem_dataTest::testOrderMixedUp ) );

   return suiteOfTests;
}

