#include <iostream>
#include <string>
#include <map>
#include <valarray>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include <spkcompiler/nonmem/NonmemTranslator.h>
#include <spkcompiler/nonmem/emit_nonmem_model.h>
#include "emit_nonmem_modelTest.h"
#include "read_nonmem_datafile_gamma_wt_time_y.h"
#include "../common/FitPopulationTestModel.h"

#include <spk/Objective.h>
#include <spk/SpkModel.h>
#include <spk/popStatistics.h>
#include <nag.h>
#include <nagg05.h>

using namespace std;
using namespace CppUnit;

void emit_nonmem_modelTest::setUp()
{
}

void emit_nonmem_modelTest::tearDown()
{
}

void emit_nonmem_modelTest::test()
{
  SymbolTable table;
  map<string, string> label_alias;

  // NONMEM predefined entities
  Symbol theta( "theta", Symbol::VECTOR, Symbol::DOUBLE, true );
  Symbol eta  ( "eta",   Symbol::VECTOR, Symbol::DOUBLE, true );
  Symbol eps  ( "eps",   Symbol::VECTOR, Symbol::DOUBLE, true );
  table.insert( theta );
  table.insert( eta );
  table.insert( eps );

  // Pretend these are lables on data from the data file.
  // These guys may be refered anywhere in the expressions.
  Symbol dose ( "dose" , Symbol::VECTOR, Symbol::DOUBLE, true );
  Symbol time ( "time" , Symbol::VECTOR, Symbol::DOUBLE, true );
  Symbol cp   ( "cp",    Symbol::VECTOR, Symbol::DOUBLE, true );
  Symbol dv   ( "dv",    Symbol::VECTOR, Symbol::DOUBLE, true );
  Symbol wt   ( "wt",    Symbol::VECTOR, Symbol::DOUBLE, true );
  table.insert( dose );
  table.insert( time );
  table.insert( cp );
  table.insert( dv );
  table.insert( wt );

  // These labels/data are required and used to
  // determine the return value of evalPred().
  Symbol evid ( "evid" , Symbol::VECTOR, Symbol::DOUBLE, true );
  Symbol mdv  ( "mdv",   Symbol::VECTOR, Symbol::DOUBLE, true );
  table.insert( evid );
  table.insert( mdv );

  // Pretend these are found in the expressions but not
  // from the data file, as user-defined arbitrary variables.
  Symbol ka( "ka", Symbol::VECTOR, Symbol::DOUBLE, false );
  Symbol ke( "ke", Symbol::VECTOR, Symbol::DOUBLE, false );
  Symbol cl( "cl", Symbol::VECTOR, Symbol::DOUBLE, false );
  Symbol d ( "d",  Symbol::VECTOR, Symbol::DOUBLE, false );
  Symbol e ( "e",  Symbol::VECTOR, Symbol::DOUBLE, false );
  table.insert( ka );
  table.insert( ke );
  table.insert( cl );
  table.insert( d );
  table.insert( e );

  // These ones which are also found in the symbol table
  // are data labels.
  label_alias[ "dose" ]   = "";
  label_alias[ "time" ]   = "";
  label_alias[ "cp"   ]   = "dv";
  label_alias[ "wt"   ]   = "";
  label_alias[ "mdv" ]    = "";
  label_alias[ "evid"   ] = "";

  // evalPred() needs IndData and IndData classes declared
  // in IndData.h.  For this test, just creat a minimal
  // code.
  FILE * inddata_h = fopen( "IndData.h", "w" );
  fprintf( inddata_h, "class IndData{\n" );
  fprintf( inddata_h, "  public:\n" );
  fprintf( inddata_h, "  IndData();\n" );
  fprintf( inddata_h, "  std::valarray<double> dose;\n" );
  fprintf( inddata_h, "  std::valarray<double> time;\n" );
  fprintf( inddata_h, "  std::valarray<double> cp;\n" );
  fprintf( inddata_h, "  std::valarray<double> dv;\n" );
  fprintf( inddata_h, "  std::valarray<double> wt;\n" );
  fprintf( inddata_h, "  std::valarray<double> evid;\n" );
  fprintf( inddata_h, "  std::valarray<double> mdv;\n" );
  fprintf( inddata_h, "};\n" );
  fprintf( inddata_h, "class IndDataSet{\n" );
  fprintf( inddata_h, "  public:\n" );
  fprintf( inddata_h, "  IndDataSet();\n" );
  fprintf( inddata_h, "  IndData& operator[](int);\n" );
  fprintf( inddata_h, "};\n" );
  fclose( inddata_h );

  // The expressions are stored here.
  FILE * pred = fopen( "pred.cpp", "w" );

  // Expressions to be inserted in the body of evalPred().
  fprintf( pred, "if( dose != 0 ){\n" );
  fprintf( pred, "  ds = dose * wt;\n" );
  fprintf( pred, "  w = wt;\n" );
  fprintf( pred, "}\n" );
  fprintf( pred, "ka = theta[0] + eta[0];\n" );
  fprintf( pred, "ke = theta[1] + eta[1];\n" );
  fprintf( pred, "cl = theta[2] * w * eta[2];\n" );
  fprintf( pred, "d = exp(-ke * time) - exp(-ka * time);\n" );
  fprintf( pred, "e = cl * (ka - ke);\n" );
  fprintf( pred, "f = ds * ke * ka / e * d;\n" );
  fprintf( pred, "y = f + eps[0];\n\n" );
  fclose( pred );

  // The generated code will be stored here.
  FILE * out  = fopen( "evalPred.cpp", "w" );

  // Open the file containing expressions as readable
  // to pass onto emit_nonmem_model().
  pred = fopen( "pred.cpp", "r" );

  // Let it genenate.
  emit_nonmem_model( out, pred, table, label_alias );

  fclose( pred );
  fclose( out );

  CPPUNIT_ASSERT( system( "g++ -c evalPred.cpp" ) == 0 );
}


CppUnit::Test * emit_nonmem_modelTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "emit_nonmem_modelTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<emit_nonmem_modelTest>
			 ("test", &emit_nonmem_modelTest::test ) );

 return suiteOfTests;
}

