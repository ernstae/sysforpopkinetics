#include <spk/Objective.h>
#include "nonmem/NonmemTranslator.h"
#include "SpkCompilerUtil.h"
#include "read_content.h"
#include "nonmem/read_nonmem_driver.h"
#include "nonmem/read_nonmem_data.h"
#include "nonmem/read_nonmem_model.h"
#include "emit_IndData.h"
#include "emit_driver.h"
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
  //  gSpkExpTree        = expTreeUtils.createTree( "unit" );
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
  string order_id_pair[ nIndividuals + 1 ];

  SymbolTable table;
  Symbol theta( "theta", Symbol::VECTOR, Symbol::DOUBLE, true );
  Symbol eta(   "eta",   Symbol::VECTOR, Symbol::DOUBLE, true );
  Symbol omega( "omega", Symbol::MATRIX, Symbol::DOUBLE, true );
  Symbol sigma( "sigma", Symbol::MATRIX, Symbol::DOUBLE, true );
  table.insert( theta );
  table.insert( eta );
  table.insert( omega );
  table.insert( sigma );

  assert( tree->getElementsByTagName( X("data") ) != NULL );
  DOMElement * dataNode = dynamic_cast<DOMElement*>( 
    tree->getElementsByTagName( X("data") )->item(0) );
  assert( dataNode != NULL );

  read_nonmem_data( dataNode, 
		    nIndividuals, 
		    table, 
		    label_alias_mapping, 
		    data_for, 
		    order_id_pair,
                    ourSpk );
  
  assert( tree->getElementsByTagName( X("model") ) != NULL );
  DOMElement * modelNode = dynamic_cast<DOMElement*>( 
     tree->getElementsByTagName( X("model") )->item(0) );
  assert( modelNode != NULL );

  gSpkExpSymbolTable = &table;
  pair<enum NonmemModel, enum NonmemParameterization> model_type 
    = read_nonmem_model( modelNode, nIndividuals, gSpkExpSymbolTable );
  nonmemModel = model_type.first;
  nonmemParameterization = model_type.second;

  ourGeneratedFileNames = emit(nIndividuals, 
			       gSpkExpSymbolTable, 
			       label_alias_mapping, 
			       data_for, 
			       order_id_pair );

  return;
}

std::vector<std::string> NonmemTranslator::emit(
		  int nIndividuals,
                  const SymbolTable * table,
                  const std::map<LABEL, ALIAS> & label_alias_mapping,
                  const std::map<LABEL, MEASUREMENT> data_for[],
                  const std::string order_id_pair[]
)
{
  char driver_cpp[]    = "main.cpp";
  char SpkModel_name[] = "SpkModel";

  FILE * pDriver_cpp = fopen( driver_cpp, "w" );
  emit_driver( pDriver_cpp, nIndividuals, SpkModel_name, ourSpk );
  fclose( pDriver_cpp );
 
  char IndData_h[] = "IndData.h";
  FILE * pIndData_h = fopen( IndData_h, "w" );
  emit_IndData(   pIndData_h, 
		  nIndividuals, 
		  gSpkExpSymbolTable, 
		  label_alias_mapping, 
		  data_for, 
		  order_id_pair );
  fclose( pIndData_h );

  char IndData_cpp[] = "IndData.cpp";
  FILE * pIndData_cpp = fopen( IndData_cpp, "w" );
  emit_initIndDataObjects( 
		  pIndData_cpp, 
		  nIndividuals, 
		  gSpkExpSymbolTable, 
		  label_alias_mapping, 
		  data_for, 
		  order_id_pair );
  
  emit_releaseIndDataObjects( 
		  pIndData_cpp, 
		  nIndividuals, 
		  gSpkExpSymbolTable, 
		  label_alias_mapping, 
		  data_for, 
		  order_id_pair );

  fclose( pIndData_cpp );
  
  vector<string> filenames(8);
  filenames.push_back( driver_cpp );
  filenames.push_back( "IndData.h" );
  filenames.push_back( "IndData.cpp" );
  filenames.push_back( "nonmem_model.h" );
  filenames.push_back( "pk.cpp" );
  filenames.push_back( "error.cpp" );
  filenames.push_back( "omega.cpp" );
  filenames.push_back( "sigma_cpp" );
  return filenames;
}
const struct SpkParameters * NonmemTranslator::getSpkParameters() const
{
  return &ourSpk;
}
const void * NonmemTranslator::getClientParameters() const
{
  return static_cast<const void*>( &ourNonmem );
}
const vector<string> NonmemTranslator::getFilenameList() const
{
  return ourGeneratedFileNames;
}

