#include "nonmem/NonmemTranslator.h"
#include "SpkCompilerUtil.h"
#include "read_content.h"
#include "nonmem/read_nonmem_driver.h"
#include "nonmem/read_nonmem_data.h"
#include "read_nonmem_model.h"

#include <xercesc/dom/DOM.hpp>

#include <iostream>

using namespace xercesc;
using namespace std;

////////////////////////////////////////////////////////////////////////////////////
//
// LOCAL FUNCTIONS
//
////////////////////////////////////////////////////////////////////////////////////

//=======================================================
// isNonmemKeyword( const string var )
//
// Return true if "var" is one of the NONMEM reserved
// words.
//=======================================================
static bool isNonmemKeyword( const string var )
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


//=======================================================
// error( const char * message )
//
// Redirect "message" to the standard error and
// increment the error counter.
// 
// REVISIT: 06/03/03, Sachiko
// This routine should be replaced by 
// something more sophisticated.
//
//=======================================================
static int errors = 0;
static int error( const char * message, int line, const char* file )
{
  fprintf( stderr, "!!! ERROR !!! %s (%d: %s)\n", message, line, file );
  ++errors;
  return 1;
}
////////////////////////////////////////////////////////////////////////////////
//
//  NonmemTranslator class
//
////////////////////////////////////////////////////////////////////////////////
const char * const NonmemTranslator::STR_NONE    = "none";
const char * const NonmemTranslator::STR_ADVAN1  = "advan1";
const char * const NonmemTranslator::STR_ADVAN2  = "advan2";
const char * const NonmemTranslator::STR_ADVAN3  = "advan3";
const char * const NonmemTranslator::STR_ADVAN4  = "advan4";
const char * const NonmemTranslator::STR_ADVAN5  = "advan5";
const char * const NonmemTranslator::STR_ADVAN6  = "advan6";
const char * const NonmemTranslator::STR_ADVAN7  = "advan7";
const char * const NonmemTranslator::STR_ADVAN8  = "advan8";
const char * const NonmemTranslator::STR_ADVAN9  = "advan9";
const char * const NonmemTranslator::STR_ADVAN10 = "advan10";
const char * const NonmemTranslator::STR_ADVAN11 = "advan11";
const char * const NonmemTranslator::STR_ADVAN12 = "advan12";
const char * const NonmemTranslator::STR_DEFAULT = "default";
const char * const NonmemTranslator::STR_TRANS1  = "trans1";
const char * const NonmemTranslator::STR_TRANS2  = "trans2";
const char * const NonmemTranslator::STR_TRANS3  = "trans3";
const char * const NonmemTranslator::STR_TRANS4  = "trans4";
const char * const NonmemTranslator::STR_TRANS5  = "trans5";

NonmemTranslator::NonmemTranslator( )
  : nonmemModel( NONE ), nonmemParameterization( DEFAULT )
{
  //  gSpkExpSymbolTable = new SymbolTable;
  gSpkExpTree        = expTreeUtils.createTree( "unit" );
}
NonmemTranslator::~NonmemTranslator( )
{
  //delete symbol_checker;
  //  delete gSpkExpSymbolTable;
}
enum NonmemTranslator::NonmemModel
NonmemTranslator::toNonmemModelEnum( const char* str )
{
  if( strcmp( str, STR_NONE ) == 0 )
    return NONE;
  if( strcmp( str, STR_ADVAN1 ) == 0 )
    return ADVAN1;
  if( strcmp( str, STR_ADVAN2 ) == 0 )
    return ADVAN2;
  if( strcmp( str, STR_ADVAN3 ) == 0 )
    return ADVAN3;
  if( strcmp( str, STR_ADVAN4 ) == 0 )
    return ADVAN4;
  if( strcmp( str, STR_ADVAN5 ) == 0 )
    return ADVAN5;
  if( strcmp( str, STR_ADVAN6 ) == 0 )
    return ADVAN6;
  if( strcmp( str, STR_ADVAN7 ) == 0 )
    return ADVAN7;
  if( strcmp( str, STR_ADVAN8 ) == 0 )
    return ADVAN8;
  if( strcmp( str, STR_ADVAN9 ) == 0 )
    return ADVAN9;
  if( strcmp( str, STR_ADVAN10 ) == 0 )
    return ADVAN10;
  if( strcmp( str, STR_ADVAN11 ) == 0 )
    return ADVAN11;
  if( strcmp( str, STR_ADVAN12 ) == 0 )
    return ADVAN12;
}
enum NonmemTranslator::NonmemParameterization
NonmemTranslator::toNonmemParameterizationEnum( const char* str )
{
  if( strcmp( str, STR_DEFAULT) == 0 )
    return DEFAULT;
  if( strcmp( str, STR_TRANS1 ) == 0 )
    return TRANS1;
  if( strcmp( str, STR_TRANS2 ) == 0 )
    return TRANS2;
  if( strcmp( str, STR_TRANS3 ) == 0 )
    return TRANS3;
  if( strcmp( str, STR_TRANS4 ) == 0 )
    return TRANS4;
  if( strcmp( str, STR_TRANS5 ) == 0 )
    return TRANS5;
}

const char* const 
NonmemTranslator::toNonmemModelString( 
   enum NonmemTranslator::NonmemModel e )
{
  if( e == NONE )
    return STR_NONE;
  if( e == ADVAN1 )
    return STR_ADVAN1;
  if( e == ADVAN2 )
    return STR_ADVAN2;
  if( e == ADVAN3 )
    return STR_ADVAN3;
  if( e == ADVAN4 )
    return STR_ADVAN4;
  if( e == ADVAN5 )
    return STR_ADVAN5;
  if( e == ADVAN6 )
    return STR_ADVAN6;
  if( e == ADVAN7 )
    return STR_ADVAN7;
  if( e == ADVAN8 )
    return STR_ADVAN8;
  if( e == ADVAN9 )
    return STR_ADVAN9;
  if( e == ADVAN10 )
    return STR_ADVAN10;
  if( e == ADVAN11 )
    return STR_ADVAN11;
  if( e == ADVAN12 )
    return STR_ADVAN12;
}

const char* const 
NonmemTranslator::toNonmemParameterizationString( 
   enum NonmemTranslator::NonmemParameterization e )
{
  if( e == DEFAULT )
    return STR_DEFAULT;
  if( e == TRANS1 )
    return STR_TRANS1;
  if( e == TRANS2 )
    return STR_TRANS2;
  if( e == TRANS3 )
    return STR_TRANS3;
  if( e == TRANS4 )
    return STR_TRANS4;
  if( e == TRANS5 )
    return STR_TRANS5;
}

void NonmemTranslator::translate( DOMDocument* tree )
{
  assert( tree != NULL );

  //
  // Read <content> section, which has these attributes like:
  // spkinml_ver, client, analysis.
  //
  string spkinml_verOut;
  enum client::type client_typeOut;
  enum SpkParameters::Analysis analysis_typeOut;

  assert( tree->getElementsByTagName( X("content") ) != NULL );
  DOMElement * contentTree
    = dynamic_cast<DOMElement*>( tree->getElementsByTagName( X("content") )->item(0) );
  assert( contentTree != NULL );
  if( !read_content( contentTree, spkinml_verOut, client_typeOut, analysis_typeOut ) )
    {
      char buf[] = "Rough content checking failed!";
      exit( error(buf, __LINE__, __FILE__) );
    }
  if( client_typeOut != client::NONMEM )
    {
      char buf[] = "Client type must be NONMEM!";
      exit( error(buf, __LINE__, __FILE__) );
    }

  ourSpk.analysis = analysis_typeOut;


  //
  // Read <driver> section.
  //
  // Get the root of "driver" subtree.  Since there's one and only one
  // <driver> specification per document, the 1st element of the list
  // obtained by DOMDocument::getElementsByTagName() is undoubtedly
  // the one that is of our interest.
  //
  assert( tree->getElementsByTagName( X("driver") ) != NULL );
  DOMElement * driverNode = dynamic_cast<DOMElement*>( tree->getElementsByTagName( X("driver") )->item(0) );
  assert( driverNode != NULL );
  
  int nIndividuals = read_nonmem_driver( driverNode, ourSpk, ourNonmem );
  
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
  map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> label_alias_mapping;


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
  map< NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT > data_for[ nIndividuals +1 ];

  //
  // This table records the processing order vs. the identifier pair of each
  // individual.
  //
  string order_id_pair[ nIndividuals +1 ];

  assert( tree->getElementsByTagName( X("data") ) != NULL );
  DOMElement * dataNode = dynamic_cast<DOMElement*>( tree->getElementsByTagName( X("data") )->item(0) );
  assert( dataNode != NULL );

  read_nonmem_data( dataNode, nIndividuals, label_alias_mapping, data_for, order_id_pair );

  assert( tree->getElementsByTagName( X("model") ) != NULL );
  DOMElement * modelNode = dynamic_cast<DOMElement*>( tree->getElementsByTagName( X("model") )->item(0) );
  assert( modelNode != NULL );

  SymbolTable table;
  gSpkExpSymbolTable = &table;
  pair<enum NonmemModel, enum NonmemParameterization> model_type 
    = read_nonmem_model( modelNode, nIndividuals, gSpkExpSymbolTable );
  nonmemModel = model_type.first;
  nonmemParameterization = model_type.second;

  //emitData( nIndividuals, gSpkExpSymbolTable, label_alias_mapping, data_for, order_id_pair );
  
  return;
}

void NonmemTranslator::initSymbolTable( SymbolTable& )
{
}


std::vector<string> NonmemTranslator::emitData( 		
		 int nIndividuals,
		 SymbolTable* table,
		 const std::map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> & label_alias_mapping,
		 const std::map<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT> data_for[],
		 const string order_id_pair[]
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
  map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS>::const_iterator names
    = label_alias_mapping.begin();
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
  map< NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT >::const_iterator map_records;
  for( int i=0; i<nIndividuals; i++ )
    {
      cout << "// " << order_id_pair[i] << "'s data (process order = " << i+1 << ")\n";
      map_records = data_for[i].begin();
      while( map_records != data_for[i].end() )
	{
	  cout << "const double " << map_records->first << "_" << order_id_pair[i] << "[] = ";
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
      
      cout << str_IndRecords << " * data" << "_" << order_id_pair[i] << " = new " << str_IndRecords << "( ";
      map_records = data_for[i].begin();
      while( map_records != data_for[i].end() )
	{
	  if( map_records != data_for[i].begin() )
	    cout << ", ";
	  cout << map_records->first << "_" << order_id_pair[i];
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

void NonmemTranslator::emitDriver()
{
}
void NonmemTranslator::emitModel()
{
}
const struct SpkParameters * NonmemTranslator::getSpkParameters() const
{
  return &ourSpk;
}
const void * NonmemTranslator::getClientParameters() const
{
  return static_cast<const void*>( &ourNonmem );
}
const char * NonmemTranslator::getDriverFilename() const
{
  return NULL;
}
const std::vector< const char * > NonmemTranslator::getModelFilenameList() const
{
  std::vector<const char*> empty;
  return empty;
}

