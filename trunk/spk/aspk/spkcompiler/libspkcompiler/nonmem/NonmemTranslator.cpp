#include <spk/Objective.h>
#include "nonmem/NonmemTranslator.h"
#include "SpkCompilerUtil.h"
#include "read_content.h"
#include "nonmem/read_nonmem_driver.h"
#include "nonmem/read_nonmem_data.h"
#include "nonmem/read_nonmem_model.h"
#include "emit_IndData.h"
#include "emit_nonmem_driver.h"
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
// isNonmemLabel( const string var )
//
// Return true if "var" is one of the NONMEM reserved
// words.
//=======================================================
 bool nonmem::isNonmemLabel( const string &var )
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
enum nonmem::MODEL
nonmem::toEnumMODEL( const char* str )
{
  if( strcmp( str, nonmem::STR_NONE ) == 0 )
    return nonmem::NONE;
  if( strcmp( str, nonmem::STR_ADVAN1 ) == 0 )
    return nonmem::ADVAN1;
  if( strcmp( str, nonmem::STR_ADVAN2 ) == 0 )
    return nonmem::ADVAN2;
  if( strcmp( str, nonmem::STR_ADVAN3 ) == 0 )
    return nonmem::ADVAN3;
  if( strcmp( str, nonmem::STR_ADVAN4 ) == 0 )
    return nonmem::ADVAN4;
  if( strcmp( str, nonmem::STR_ADVAN5 ) == 0 )
    return nonmem::ADVAN5;
  if( strcmp( str, nonmem::STR_ADVAN6 ) == 0 )
    return nonmem::ADVAN6;
  if( strcmp( str, nonmem::STR_ADVAN7 ) == 0 )
    return nonmem::ADVAN7;
  if( strcmp( str, nonmem::STR_ADVAN8 ) == 0 )
    return nonmem::ADVAN8;
  if( strcmp( str, nonmem::STR_ADVAN9 ) == 0 )
    return nonmem::ADVAN9;
  if( strcmp( str, nonmem::STR_ADVAN10 ) == 0 )
    return nonmem::ADVAN10;
}
enum nonmem::TRANS
nonmem::toEnumTRANS( const char* str )
{
  if( strcmp( str, nonmem::STR_DEFAULT) == 0 )
    return nonmem::DEFAULT;
  if( strcmp( str, nonmem::STR_TRANS1 ) == 0 )
    return nonmem::TRANS1;
  if( strcmp( str, nonmem::STR_TRANS2 ) == 0 )
    return nonmem::TRANS2;
  if( strcmp( str, nonmem::STR_TRANS3 ) == 0 )
    return nonmem::TRANS3;
  if( strcmp( str, nonmem::STR_TRANS4 ) == 0 )
    return nonmem::TRANS4;
  if( strcmp( str, nonmem::STR_TRANS5 ) == 0 )
    return nonmem::TRANS5;
}

const char* const 
nonmem::toStringMODEL( 
   enum nonmem::MODEL e )
{
  if( e == nonmem::NONE )
    return nonmem::STR_NONE;
  if( e == nonmem::ADVAN1 )
    return nonmem::STR_ADVAN1;
  if( e == nonmem::ADVAN2 )
    return nonmem::STR_ADVAN2;
  if( e == nonmem::ADVAN3 )
    return nonmem::STR_ADVAN3;
  if( e == nonmem::ADVAN4 )
    return nonmem::STR_ADVAN4;
  if( e == nonmem::ADVAN5 )
    return nonmem::STR_ADVAN5;
  if( e == nonmem::ADVAN6 )
    return nonmem::STR_ADVAN6;
  if( e == nonmem::ADVAN7 )
    return nonmem::STR_ADVAN7;
  if( e == nonmem::ADVAN8 )
    return nonmem::STR_ADVAN8;
  if( e == nonmem::ADVAN9 )
    return nonmem::STR_ADVAN9;
  if( e == nonmem::ADVAN10 )
    return nonmem::STR_ADVAN10;
}

const char* const 
nonmem::toStringTRANS( 
   enum nonmem::TRANS e )
{
  if( e == nonmem::DEFAULT )
    return nonmem::STR_DEFAULT;
  if( e == nonmem::TRANS1 )
    return nonmem::STR_TRANS1;
  if( e == nonmem::TRANS2 )
    return nonmem::STR_TRANS2;
  if( e == nonmem::TRANS3 )
    return nonmem::STR_TRANS3;
  if( e == nonmem::TRANS4 )
    return nonmem::STR_TRANS4;
  if( e == nonmem::TRANS5 )
    return nonmem::STR_TRANS5;
}
enum nonmem::PK_PARA nonmem::toEnumPK_PARA( const char* s )
{
  if( strcmp( s, STR_PK_Pn ) == 0 )
    return nonmem::Pn;
  if( strcmp( s, STR_PK_K ) == 0 )
    return nonmem::K;
  if( strcmp( s, STR_PK_KA ) == 0 )
    return nonmem::KA;
  if( strcmp( s, STR_PK_K12 ) == 0 )
    return nonmem::K12;
  if( strcmp( s, STR_PK_K21 ) == 0 )
    return nonmem::K21;
  if( strcmp( s, STR_PK_K23 ) == 0 )
    return nonmem::K23;
  if( strcmp( s, STR_PK_K32 ) == 0 )
    return nonmem::K32;
  if( strcmp( s, STR_PK_KM ) == 0 )
    return nonmem::KM;
  if( strcmp( s, STR_PK_CL ) == 0 )
    return nonmem::CL;
  if( strcmp( s, STR_PK_Q ) == 0 )
    return nonmem::Q;
  if( strcmp( s, STR_PK_V ) == 0 )
    return nonmem::V;
  if( strcmp( s, STR_PK_VSS ) == 0 )
    return nonmem::VSS;
  if( strcmp( s, STR_PK_V1 ) == 0 )
    return nonmem::V1;
  if( strcmp( s, STR_PK_V2 ) == 0 )
    return nonmem::V2;
  if( strcmp( s, STR_PK_V3 ) == 0 )
    return nonmem::V3;
  if( strcmp( s, STR_PK_VM ) == 0 )
    return nonmem::VM;
  if( strcmp( s, STR_PK_AOB ) == 0 )
    return nonmem::AOB;
  if( strcmp( s, STR_PK_ALPHA ) == 0 )
    return nonmem::ALPHA;
  if( strcmp( s, STR_PK_BETA ) == 0 )
    return nonmem::BETA;
  if( strcmp( s, STR_PK_Sn ) == 0 )
    return nonmem::Sn;
  if( strcmp( s, STR_PK_SC ) == 0 )
    return nonmem::SC;
  if( strcmp( s, STR_PK_S0 ) == 0 )
    return nonmem::S0;
  if( strcmp( s, STR_PK_S1 ) == 0 )
    return nonmem::S1;
  if( strcmp( s, STR_PK_S2 ) == 0 )
    return nonmem::S2;
  if( strcmp( s, STR_PK_S3 ) == 0 )
    return nonmem::S3;
  if( strcmp( s, STR_PK_S4 ) == 0 )
    return nonmem:: S4;
  if( strcmp( s, STR_PK_Fn ) == 0 )
    return nonmem::Fn;
  if( strcmp( s, STR_PK_F0 ) == 0 )
    return nonmem::F0; // f-zero
  if( strcmp( s, STR_PK_FO ) == 0 )
    return nonmem::FO; // f-oh
  if( strcmp( s, STR_PK_F1 ) == 0 )
    return nonmem::F1;
  if( strcmp( s, STR_PK_F2 ) == 0 )
    return nonmem::F2;
  if( strcmp( s, STR_PK_F3 ) == 0 )
    return nonmem::F3;
  if( strcmp( s, STR_PK_Rn ) == 0 )
    return nonmem::Rn;
  if( strcmp( s, STR_PK_R1 ) == 0 )
    return nonmem::R1;
  if( strcmp( s, STR_PK_R2 ) == 0 )
    return nonmem::R2;
  if( strcmp( s, STR_PK_R3 ) == 0 )
    return nonmem::R3;
  if( strcmp( s, STR_PK_Dn ) == 0 )
    return nonmem::Dn;
  if( strcmp( s, STR_PK_D1 ) == 0 )
    return nonmem::D1;
  if( strcmp( s, STR_PK_D2 ) == 0 )
    return nonmem::D2;
  if( strcmp( s, STR_PK_D3 ) == 0 )
    return nonmem::D3;
  if( strcmp( s, STR_PK_ALAGn ) == 0 )
    return nonmem::ALAGn;
  if( strcmp( s, STR_PK_ALAG1 ) == 0 )
    return nonmem::ALAG1;
  if( strcmp( s, STR_PK_ALAG2 ) == 0 )
    return nonmem::ALAG2;
  if( strcmp( s, STR_PK_ALAG3 ) == 0 )
    return nonmem::ALAG3;
  if( strcmp( s, STR_PK_XSCALE ) == 0 )
    return nonmem::XSCALE;
}
const char* const nonmem::toStringPK_PARA( enum nonmem::PK_PARA e )
{
  if( e == nonmem::Pn )
    return STR_PK_Pn;
  if( e == nonmem::K )
    return STR_PK_K;
  if( e == nonmem::KA )
    return STR_PK_KA;
  if( e == nonmem::K12 )
    return STR_PK_K12;
  if( e == nonmem::K21 )
    return STR_PK_K21;
  if( e == nonmem::K23 )
    return STR_PK_K23;
  if( e == nonmem::K32 )
    return STR_PK_K32;
  if( e == nonmem::KM )
    return STR_PK_KM;
  if( e == nonmem::CL )
    return STR_PK_CL;
  if( e == nonmem::Q )
    return STR_PK_Q;
  if( e == nonmem::V )
    return STR_PK_V;
  if( e == nonmem::VSS )
    return STR_PK_VSS;
  if( e == nonmem::V1 )
    return STR_PK_V1;
  if( e == nonmem::V2 )
    return STR_PK_V2;
  if( e == nonmem::V3 )
    return STR_PK_V3;
  if( e == nonmem::VM )
    return STR_PK_VM;
  if( e == nonmem::AOB )
    return STR_PK_AOB;
  if( e == nonmem::ALPHA )
    return STR_PK_ALPHA;
  if( e == nonmem::BETA )
    return STR_PK_BETA;
  if( e == nonmem::Sn )
    return STR_PK_Sn;
  if( e == nonmem::SC )
    return STR_PK_SC;
  if( e == nonmem::S0 )
    return STR_PK_S0;
  if( e == nonmem::S1 )
    return STR_PK_S1;
  if( e == nonmem::S2 )
    return STR_PK_S2;
  if( e == nonmem::S3 )
    return STR_PK_S3;
  if( e == nonmem::S4 )
    return STR_PK_S4;
  if( e == nonmem::Fn )
    return STR_PK_Fn;
  if( e == nonmem::F0 ) 
    return STR_PK_F0;
  if( e == nonmem::FO )
    return STR_PK_FO;
  if( e == nonmem::F1 )
    return STR_PK_F1;
  if( e == nonmem::F2 )
    return STR_PK_F2;
  if( e == nonmem:: F3 )
    return STR_PK_F3;
  if( e == nonmem::Rn )
    return STR_PK_Rn;
  if( e == nonmem::R1 )
    return STR_PK_R1;
  if( e == nonmem::R2 )
    return STR_PK_R2;
  if( e == nonmem::R3 )
    return STR_PK_R3;
  if( e == nonmem::Dn )
    return STR_PK_Dn;
  if( e == nonmem::D1 )
    return STR_PK_D1;
  if( e == nonmem::D2 )
    return STR_PK_D2;
  if( e == nonmem::D3 )
    return STR_PK_D3;
  if( e == nonmem::ALAGn )
    return STR_PK_ALAGn;
  if( e == nonmem::ALAG1 )
    return STR_PK_ALAG2;
  if( e == nonmem::ALAG2 )
    return STR_PK_ALAG2;
  if( e == nonmem::ALAG3 )
    return STR_PK_ALAG3;
  if( e == nonmem::XSCALE )
    return STR_PK_XSCALE;
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
  map<nonmem::LABEL, nonmem::ALIAS> label_alias_mapping;


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
  vector< map< nonmem::LABEL, nonmem::MEASUREMENT > > data_for( nIndividuals );

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
                  const std::map<nonmem::LABEL, nonmem::ALIAS> & label_alias_mapping,
                  const std::vector< std::map<nonmem::LABEL, nonmem::MEASUREMENT > > & data_for,
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

