#include <spk/Objective.h>
#include <spk/SpkValarray.h>

#include "nonmem.h"
#include "NonmemTranslator.h"
#include "SpkCompilerUtil.h"
#include "read_content.h"
#include "read_nonmem_driver.h"
#include "read_nonmem_model.h"
#include "read_nonmem_data.h"
#include "emit_IndData.h"
#include "emit_nonmem_driver.h"
#include <xercesc/dom/DOM.hpp>

#include <iostream>

using SPK_VA::valarray;
using namespace xercesc;
using namespace std;

////////////////////////////////////////////////////////////////////////////////////
//
// LOCAL FUNCTIONS
//
////////////////////////////////////////////////////////////////////////////////////

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

NonmemTranslator::NonmemTranslator( )
  : nonmemModel( nonmem::NONE ), nonmemTrans( nonmem::DEFAULT )
{
  //  gSpkExpSymbolTable = new SymbolTable;
  //  gSpkExpTree        = expTreeUtils.createTree( "unit" );
}
NonmemTranslator::~NonmemTranslator( )
{
  //delete symbol_checker;
  //  delete gSpkExpSymbolTable;
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
  pair<bool, bool> p = read_content( contentTree, spkinml_verOut, client_typeOut, analysis_typeOut );

  ourSpk.analysis = analysis_typeOut;
  ourSpk.isEstimation = p.first;
  ourSpk.isSimulation = p.second;

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
  vector< map< string, valarray<double>  > > data_for( nIndividuals );

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
  pair<enum nonmem::MODEL, enum nonmem::TRANS> model_type 
    = read_nonmem_model( modelNode, nIndividuals, gSpkExpSymbolTable );
  nonmemModel = model_type.first;
  nonmemTrans = model_type.second;

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
                  const std::map<string, string> & label_alias_mapping,
                  const std::vector< std::map<string, valarray<double>  > > & data_for,
                  const std::string order_id_pair[]
)
{
  char driver_cpp[]    = "main.cpp";
  char SpkModel_name[] = "NonmemModel";
  char modelObject_init_block[] = "NonmemModel model;";

  FILE * pDriver_cpp = fopen( driver_cpp, "w" );
  emit_nonmem_driver( pDriver_cpp, 
		      nIndividuals, 
		      SpkModel_name, 
		      modelObject_init_block,
		      ourSpk, 
		      ourNonmem );
  fclose( pDriver_cpp );
 
  char IndData_h[]    = "IndData.h";
  FILE * pIndData_h   = fopen( IndData_h, "w" );
  char IndData_cpp[]  = "IndData.cpp";
  FILE * pIndData_cpp = fopen( IndData_cpp, "w" );
  emit_IndData(   pIndData_h, 
		  pIndData_cpp,
		  label_alias_mapping, 
		  data_for, 
		  order_id_pair );
  fclose( pIndData_h );
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

