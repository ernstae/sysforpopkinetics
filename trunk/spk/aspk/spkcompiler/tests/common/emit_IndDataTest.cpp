#include <iostream>
#include <string>
#include <map>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>
#include "emit_IndData.h"
#include "emit_IndDataTest.h"
#include "SymbolTable.h"
#include "Symbol.h"
#include "NonmemTranslator.h"

using namespace std;
using namespace CppUnit;

void emit_IndDataTest::setUp()
{
}

void emit_IndDataTest::tearDown()
{
}

void emit_IndDataTest::test()
{
  const int nIndividuals = 3;
  int nMeasurements[ nIndividuals + 1 ];
  SymbolTable table;
  std::map<nonmem::LABEL, nonmem::ALIAS> label_alias_mapping;
  std::map<nonmem::LABEL, nonmem::MEASUREMENT> data_for[ nIndividuals +1 ];
  string order_id_pair[ nIndividuals + 1 ];

  const int ALAN  = 0;
  const int JIAJI = 1;
  const int MITCH = 2;

  order_id_pair[ALAN]  = "Alan";
  order_id_pair[JIAJI] = "Jiaji";
  order_id_pair[MITCH] = "Mitch";

  nMeasurements[ALAN]  = 1;
  nMeasurements[JIAJI] = 2;
  nMeasurements[MITCH] = 3;
 
  Symbol XX( "x", Symbol::VECTOR, Symbol::DOUBLE, false );
  Symbol YY( "y", Symbol::VECTOR, Symbol::DOUBLE, false );
  Symbol ZZ( "z", Symbol::VECTOR, Symbol::DOUBLE, false );
  Symbol VV( "v", Symbol::VECTOR, Symbol::DOUBLE, false );
  Symbol WW( "w", Symbol::VECTOR, Symbol::DOUBLE, false );
  table.insert( XX );
  table.insert( YY );
  table.insert( ZZ );
  table.insert( VV );
  table.insert( WW );

  label_alias_mapping[ "x" ] = "y";
  label_alias_mapping[ "z" ] = "v";
  label_alias_mapping[ "w" ] = "";

  data_for[ALAN]["x"].resize( nMeasurements[ALAN] );
  data_for[ALAN]["z"].resize( nMeasurements[ALAN] );
  data_for[ALAN]["w"].resize( nMeasurements[ALAN] );
  for( int i=0; i<nMeasurements[ALAN]; i++ )
  {
    data_for[ALAN]["x"][i] = ALAN + i;
    data_for[ALAN]["z"][i] = ALAN + i * 100;
    data_for[ALAN]["w"][i] = ALAN + i * 1000;
  }
  
  data_for[JIAJI]["x"].resize( nMeasurements[JIAJI] );
  data_for[JIAJI]["z"].resize( nMeasurements[JIAJI] );
  data_for[JIAJI]["w"].resize( nMeasurements[JIAJI] );
  for( int i=0; i<nMeasurements[JIAJI]; i++ )
  {
    data_for[JIAJI]["x"][i] = JIAJI + i;
    data_for[JIAJI]["z"][i] = JIAJI + i * 100;
    data_for[JIAJI]["w"][i] = JIAJI + i * 1000;
  }

  data_for[MITCH]["x"].resize( nMeasurements[MITCH] );
  data_for[MITCH]["z"].resize( nMeasurements[MITCH] );
  data_for[MITCH]["w"].resize( nMeasurements[MITCH] );
  for( int i=0; i<nMeasurements[MITCH]; i++ )
  {
    data_for[MITCH]["x"][i] = MITCH + i;
    data_for[MITCH]["z"][i] = MITCH + i * 100;
    data_for[MITCH]["w"][i] = MITCH + i * 1000;
  }
  
  FILE * IndData_h = fopen( "IndData.h", "w" );
  emit_IndData( IndData_h, nIndividuals, &table, label_alias_mapping, data_for, order_id_pair );
  fclose( IndData_h );
  
  FILE * IndData_cpp = fopen( "IndData.cpp", "w" );
  emit_initIndDataObjects( IndData_cpp, nIndividuals, &table, label_alias_mapping, data_for, order_id_pair );
  emit_releaseIndDataObjects( IndData_cpp, nIndividuals, &table, label_alias_mapping, data_for, order_id_pair );
  fclose( IndData_cpp );

  CPPUNIT_ASSERT_MESSAGE( "Failed to compile IndData.cpp!", system( "g++ -c IndData.cpp" ) == 0 );

  char driver_filename[] = "temp_driver.cpp";
  FILE * pDriver = fopen( driver_filename, "w" );
  fprintf( pDriver, "#include \"IndData.h\"\n" );
  fprintf( pDriver, "int main()\n" );
  fprintf( pDriver, "{\n" );
  fprintf( pDriver, "   const int ALAN  = %d;\n", ALAN );
  fprintf( pDriver, "   const int JIAJI = %d;\n", JIAJI );
  fprintf( pDriver, "   const int MITCH = %d;\n", MITCH );
  fprintf( pDriver, "   int N[] = { 1, 2, 3 };\n" );
  
  fprintf( pDriver, "   IndData * data_for[ %d ];\n", nIndividuals );
  fprintf( pDriver, "   initIndDataObjects( %d, data_for );\n", nIndividuals );
  
  fprintf( pDriver, "   for( int j=0; j<3; j++ )\n" );
  fprintf( pDriver, "   {\n" );
  fprintf( pDriver, "      for( int i=0; i<N[j]; i++ )\n" );
  fprintf( pDriver, "      {\n" );
  fprintf( pDriver, "         if( data_for[j]->x[i] != data_for[j]->y[i] )\n" );
  fprintf( pDriver, "            return -1;\n" );
  fprintf( pDriver, "         if( data_for[j]->z[i] != data_for[j]->v[i] )\n" );
  fprintf( pDriver, "            return -1;\n" );
  fprintf( pDriver, "      }\n" );
  fprintf( pDriver, "   }\n" );
  
  fprintf( pDriver, "   releaseIndDataObjects( %d, data_for );\n", nIndividuals );
  fprintf( pDriver, "   return 0;\n" );
  fprintf( pDriver, "}\n" );
  fclose( pDriver );
  
  CPPUNIT_ASSERT_MESSAGE( "Failed to compile %s!", system( "g++ -c temp_driver.cpp" ) == 0 );
  CPPUNIT_ASSERT_MESSAGE( "Failed to link %s and %s!", system( "g++ temp_driver.o IndData.o -o temp_driver" ) == 0 );
  CPPUNIT_ASSERT( system( "./temp_driver" ) == 0 );
  remove( "IndData.cpp" );
  remove( "IndData.o" );
  remove( "temp_driver.cpp" );
  remove( "temp_driver.o " );
  remove( "temp_driver" );
}


CppUnit::Test * emit_IndDataTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "emit_IndDataTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<emit_IndDataTest>
			 ("test", &emit_IndDataTest::test ) );

 return suiteOfTests;
}

