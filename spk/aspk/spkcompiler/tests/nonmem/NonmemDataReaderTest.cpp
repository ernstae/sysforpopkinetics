#include <iostream>
#include <valarray>
#include <fstream>
#include <map>

#include "NonmemDataReaderTest.h"

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include "SpkCompilerUtil.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

void NonmemDataReaderTest::setUp()
{  
  // This file contains only the <data> section of SpkInML.
  strcpy( input, "NonmemDataReaderTestInput.xml" );
  try
    {
      XMLPlatformUtils::Initialize();
    }
  catch( const XMLException & toCatch )
    {
      char buf[256];
      sprintf( buf, "Error during Xerces-c Initialization.\nException message: %s.\n",
	       C(toCatch.getMessage() ) );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
    }

  parser = new xercesc::XercesDOMParser;
  parser->setValidationScheme( XercesDOMParser::Val_Auto );
  parser->setDoNamespaces(true );
  parser->setDoSchema(true );
  parser->setValidationSchemaFullChecking(true);
  parser->setCreateEntityReferenceNodes(true);

  try
  {
    ifstream ifs( input );
    if( !ifs.good() )
      {
	XMLPlatformUtils::Terminate();
	char buf[256];
	sprintf( buf, "Failed to open %s!\n", input);
	CPPUNIT_ASSERT_MESSAGE( buf, false );
      }
    ifs.close();
    parser->parse(input);
  }
  catch( const XMLException& e )
  {
    XMLPlatformUtils::Terminate();
    char buf[256];
    sprintf( buf, "An error occurred during parsing\n   Message: %s\n", C(e.getMessage() ) );
    CPPUNIT_ASSERT_MESSAGE( buf, false );
  }
  catch( const DOMException& e )
  {
      const unsigned int maxChars = 2047;
      XMLCh errText[maxChars + 1];
      char buf[256];
      sprintf( buf, "DOM Error during parsing \"%s\"\nDOMException code is: %d\n", 
	       input, e.code );
      CPPUNIT_ASSERT_MESSAGE( buf, false );

      if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
      {
	XMLPlatformUtils::Terminate();
	char buf[256];
	sprintf( buf, "Message is: %s.\n %d, %s\n", C(errText), __LINE__, __FILE__ );
	CPPUNIT_ASSERT_MESSAGE( buf, false );
      }
  }
  catch( ... )
  {
    XMLPlatformUtils::Terminate();
    char buf[128];
    sprintf( buf, "An unknown error occurred during parsing.\n %d, %s\n", __LINE__, __FILE__ );
    CPPUNIT_ASSERT_MESSAGE( buf, false );
  }
  
  dataTree = parser->getDocument();
}
void NonmemDataReaderTest::tearDown()
{
  delete parser;
  XMLPlatformUtils::Terminate();
}
void NonmemDataReaderTest::testNoSkip()
{
  translate_data( dataTree );
}
std::vector<const char*> NonmemDataReaderTest::translate_data( xercesc::DOMDocument* dataTree )
{
  map<string, const char*> master_alias_table;

  DOMNodeList * individualsList = dataTree->getElementsByTagName(X("individual"));
  const int nIndividuals = static_cast<int>( (individualsList->getLength()) );
  assert( nIndividuals == 12 );

  vector< map< string, valarray<double> > > alias_maps(nIndividuals);
  for( int i=0; i<nIndividuals; i++ )
    {
      DOMElement * individual = dynamic_cast<DOMElement*>( individualsList->item(i) );

      int order = atoi( C( individual->getAttribute( X("order") ) ) );
      const char * const id = C( individual->getAttribute( X("id") ) );
      int nMeasurements = atoi( C( individual->getAttribute( X("length") ) ) );
      
      DOMTreeWalker * walker = dataTree->createTreeWalker( individual,
							   DOMNode::ELEMENT_NODE,
							   NULL,
							   false );
      int nItems = 0;
      DOMElement * item = dynamic_cast<DOMElement*>( walker->firstChild() );
      for( nItems=0; item != NULL; ++nItems )
	{
	  // synonym can be null but label has to be non empty.
	  const char * label = C( item->getAttribute( X("label" ) ) );
	  assert( label != NULL );
	  const XMLCh * x_synonym = item->getAttribute( X("synonym") );
	  
	  const char * const synonym 
	    = ( XMLString::isAllWhiteSpace( x_synonym )? NULL : C( x_synonym ) );

	  master_alias_table[ label ] = synonym;

	  int nValues = 0;
	  valarray<double> values(nMeasurements);
	  DOMElement * valueTag = dynamic_cast<DOMElement*>( walker->firstChild() );
	  for( nValues=0; valueTag != NULL; ++nValues )
	    {
	      DOMNode * val_node = valueTag->getFirstChild();
	      if( val_node != NULL )
		{
		  values[nValues] = atof( C( val_node->getNodeValue() ) );
		}
	      else
		{
		  values[nValues] = 0.0;
		}
	      valueTag = dynamic_cast<DOMElement*>( walker->nextSibling() );
	    }
	  assert( nMeasurements == nValues );
	  walker->parentNode();

	  alias_maps[i].insert( pair<string, valarray<double> >(label, values) );

	  item = dynamic_cast<DOMElement*>( walker->nextSibling() );
	}
      
      walker->parentNode();
    }

  //
  // Write the definition of "class IndRecords".
  //
  cout << "class IndRecords{" << endl;
  cout << "public:" << endl;
  cout << "\tIndRecords(" << endl;
  map<string, const char*>::const_iterator names = master_alias_table.begin();
  while( names != master_alias_table.end() )
    {
      if( names != master_alias_table.begin() )
	cout << ", " << endl;
      cout << "\t\tconst double * " << names->first << "In";
      ++names;
    }
  cout << endl;
  cout << "\t)" << endl;
  cout << "\t : ";
  names = master_alias_table.begin();
  while( names != master_alias_table.end() )
    {
      if( names != master_alias_table.begin() )
	cout << ", ";
      cout << names->first << "(" << names->first << "In" << ")";
      if( names->second != NULL )
	{
	  cout << ", " << names->second << "(" << names->first << ")";
	}
      ++names;
    }
  cout << endl;
  cout << "\t{ /* do nothing */ }" << endl;
  
  names = master_alias_table.begin();
  while( names != master_alias_table.end() )
    {
      cout << "\tconst double * " << names->first << ";" << endl;
      if( names->second != NULL )
	{
	  cout << "\tconst double * " << names->second << ";" << endl;
	}
      ++names;
    }
  cout << endl;
  cout << "};" << endl;

  //
  // Allocate memory
  //
  cout << "const int nIndividuals = " << nIndividuals << ";" << endl;
  cout << "IndRecords * data[nIndividuals+1]" << ";" << endl;
  cout << endl;

  //
  // Write the initialization code for IndRecords records for all individuals.
  //
  map< string, valarray<double> >::const_iterator map_records;
  for( int i=0; i<nIndividuals; i++ )
    {
      cout << "// " << i << " individual's data" << endl;
      map_records = alias_maps[i].begin();
      while( map_records != alias_maps[i].end() )
	{
	  cout << "const double " << map_records->first << "_" << i << "[] = ";
       	  cout << "{ ";
	  for( int j=0; j<map_records->second.size(); j++ )
	    {
	      if( j>0 )
		cout << ", ";
	      cout << map_records->second[j];
	    }
	  cout << " };" << endl;
	  ++map_records;
	}
      
      cout << "IndRecords * data" << "_" << i << " = new IndRecords( ";
      map_records = alias_maps[i].begin();
      while( map_records != alias_maps[i].end() )
	{
	  if( map_records != alias_maps[i].begin() )
	    cout << ", ";
	  cout << map_records->first << "_" << i;
	  ++map_records;
	}
      cout << " );" << endl;
      cout << "data[" << i << "] = " << "data_" << i << ";" << endl;
      cout << endl;
   }

  cout << "for( int i=0; i<nIndividuals; i++ )" << endl;
  cout << "{" << endl;
  cout << "   delete data[i];" << endl;
  cout << "}" << endl;

  vector<const char*> filenames;
  return filenames;
}
void NonmemDataReaderTest::testWithSkip()
{
}
CppUnit::Test * NonmemDataReaderTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "NonmemDataReaderTest" );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemDataReaderTest>(
         "testNoSkip", 
	 &NonmemDataReaderTest::testNoSkip ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<NonmemDataReaderTest>(
         "testWithSkip",
	 &NonmemDataReaderTest::testWithSkip ) );

   return suiteOfTests;
}

