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

bool isNonmemKeyword( const string var )
{
  if( var =="id" |
      var =="l1" |
      var =="l2" |
      var =="dv" |
      var =="mdv" |
      var =="time" |
      var =="data" |
      var =="dat1" |
      var =="dat2" |
      var =="dat3" |
      var =="drop" |
      var =="skip" |
      var =="evid" |
      var =="amt" |
      var =="rate" |
      var =="ss" |
      var =="ii" |
      var =="add1" |
      var =="cmt" |
      var =="pcmt" |
      var =="call" |
      var =="cont" )
    {
      return true;
    }
  else
    return false;
}
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
  int nIndividuals = 12;

  SymbolTable * symbolTable = new SymbolTable;

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
  map<LABEL, ALIAS> label_alias_mapping;


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
  map< LABEL, MEASUREMENT > data_for[ nIndividuals +1 ];

  //
  // This table records the processing order vs. the identifier pair of each
  // individual.
  //
  const char* order_id_pair[ nIndividuals +1 ];

  read_data( dataTree, nIndividuals, symbolTable, label_alias_mapping, data_for, order_id_pair );

  emit_data( nIndividuals, symbolTable, label_alias_mapping, data_for, order_id_pair );

  symbolTable->dump();
  delete symbolTable;
}
 
/**
 * Process <data> section of SpkInML document to gather information
 * needed to genreate the source code for the following entities:
 *
 * - the definition of IndRecords class
 * - the initialization of IndRecords objects for all individuals
 * 
 * IndRecords class
 *   This class lists all observation records associated with an individual.
 *   It allows accessing a set of observation records (column) by both
 *   "label" and "alias" (if an alias is given).
 */
void NonmemDataReaderTest::read_data( 
	xercesc::DOMDocument* tree, 
        int nIndividuals,
	SymbolTable * table,
	map<LABEL, ALIAS> label_alias_mapping,
	map< LABEL, MEASUREMENT > data_for[],
	const char* order_id_pair[]
      )
{
  assert( tree->getElementsByTagName( X("data") ) != NULL );
  DOMNode * dataTree = tree->getElementsByTagName( X("data") )->item(0);
  assert( dataTree != NULL );
  //
  // Get the list of <individual> nodes.  Each <individual> node is the root
  // of that individual's data subtree and determine the number of 
  // sets (= #individuals) of data.
  //
  DOMTreeWalker * walker = tree->createTreeWalker( dataTree,
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
  // label-alias mappings and to populate the data_for table.
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
  for( int i=0; i<nIndividuals; i++ )
    {
      //      DOMElement * individual = dynamic_cast<DOMElement*>( individualsList->item(i) );

      //
      // First, take care the <individual> tag's attributes.
      //
      int nMeasurements = atoi( C( individual->getAttribute( X("length") ) ) );
      const char * id = C( individual->getAttribute( X("id") ) );

      int order = i;
      const XMLCh* xml_order = individual->getAttribute( X("order") );
      if( !XMLString::isAllWhiteSpace( xml_order ) || xml_order != NULL )
	{
	  order = atoi( C( xml_order ) ) - 1;
	}

      //
      // Map the processing order and the individual's identifier.
      //
      order_id_pair[order] = id;

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
	      // This column is to be ignored.
	    }
	  else
	    {
	      const string label = string( C( xml_label ) );
	      const string synonym = string( C( xml_synonym ) );

	      bool isLabel_NonmemKeyword = isNonmemKeyword( label );	      
	      //Symbol name1( label, Symbol::VECTOR, Symbol::DOUBLE, isLabel_NonmemKeyword );
	      //name1.size( nMeasurements );
	      //table->insert( name1 );
	      if( synonym != "" )
		{
		  bool isAlias_NonmemKeyword = isNonmemKeyword( synonym );
		  
		  // When an alias is given, either the label or alias MUST be one of NONMEM keywords.
		  assert( isLabel_NonmemKeyword || isAlias_NonmemKeyword );
		  
		  //Symbol name2( synonym, Symbol::VECTOR, Symbol::DOUBLE, isAlias_NonmemKeyword );
		  //name2.size( nMeasurements );
		  //table->insert( name2 );
		}
	      label_alias_mapping[ label ] = synonym;

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
	      walker->parentNode();
	      
	      //
	      // Register the label-values pair in the data_for map.
	      //
	      data_for[i].insert( pair<string, valarray<double> >(label, values) );
	    } 
	  item = dynamic_cast<DOMElement*>( walker->nextSibling() );
	}
      
      walker->parentNode();
    }
}

std::vector<string> NonmemDataReaderTest::emit_data( 		
		 int nIndividuals,
		 SymbolTable* table,
		 const std::map<LABEL, ALIAS> label_alias_mapping,
		 const std::map< LABEL, MEASUREMENT > data_for[],
		 const char* const order_id_pair[]
		 )
{
  //=================================================================================
  //
  // Convert the gathered information into C++ source code.
  //
  //=================================================================================

  //
  // Write the definition of "class IndRecords".
  //
  const char* str_IndRecords = "IndRecords";

  cout << "class " << str_IndRecords << "{\n";
  cout << "public:\n";
  cout << "\t" << str_IndRecords << "(\n";
  map<LABEL, ALIAS>::const_iterator names = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
    {
      if( names != label_alias_mapping.begin() )
	{
	  cout << ", \n";
	}
      cout << "\t\t" << "const double * " << names->first << "In";
      ++names;
    }
  cout << "\n";
  cout << "\t)\n";
  cout << "\t : ";
  names = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
    {
      if( names != label_alias_mapping.begin() )
	{
	  cout << ",\n\t";
	}
      cout << names->first << "(" << names->first << "In" << ")";
      if( names->second != "" )
	{
	  cout << ", " << names->second << "(" << names->first << ")";
	}
      ++names;
    }
  cout << "\n";
  cout << "\t{ /* have nothing really to do */ }\n";
  cout << "\n";
  
  names = label_alias_mapping.begin();
  while( names != label_alias_mapping.end() )
    {
      cout << "\tconst double * " << names->first << ";\n";
      if( names->second != "" )
	{
	  cout << "\tconst double * " << names->second << ";\n";
	}
      ++names;
    }

  cout << "\n";
  cout << "protected:\n";
  cout << "\t" << str_IndRecords << "(){}\n";
  cout << "\t" << str_IndRecords << "( const " << str_IndRecords << "& ){}\n";
  cout << "\t" << str_IndRecords << "* operator=( const " << str_IndRecords << "& ){}\n";
  cout << "};\n";

  //
  // Declare an array of #nIndividual number of IndRecords objects.
  //
  cout << "const int nIndividuals = " << nIndividuals << ";\n";
  cout << str_IndRecords << " * data[nIndividuals+1]" << ";\n";
  cout << "\n";

  //
  // Write the initialization code for IndRecords records for each individual.
  //
  map< LABEL, MEASUREMENT >::const_iterator map_records;
  for( int i=0; i<nIndividuals; i++ )
    {
      cout << "// " << order_id_pair[i] << "'s data (process order = " << i << ")\n";
      map_records = data_for[i].begin();
      while( map_records != data_for[i].end() )
	{
	  cout << "const double " << map_records->first << "_" << i << "[] = ";
       	  cout << "{ ";
	  for( int j=0; j<map_records->second.size(); j++ )
	    {
	      if( j>0 )
		cout << ", ";
	      cout << map_records->second[j];
	    }
	  cout << " };\n";
	  ++map_records;
	}
      
      cout << str_IndRecords << " * data" << "_" << i << " = new " << str_IndRecords << "( ";
      map_records = data_for[i].begin();
      while( map_records != data_for[i].end() )
	{
	  if( map_records != data_for[i].begin() )
	    cout << ", ";
	  cout << map_records->first << "_" << i;
	  ++map_records;
	}
      cout << " );\n";
      cout << "data_for[" << i << "] = " << "data_" << i << ";\n";
      cout << endl;
   }

  //
  // Clean-up code
  //
  cout << "// Release memory allocated for " << str_IndRecords << " objects.\n";
  cout << "for( int i=0; i<nIndividuals; i++ )\n";
  cout << "{\n";
  cout << "   delete data[i];\n";
  cout << "}\n";

  vector<string> filenames;
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

