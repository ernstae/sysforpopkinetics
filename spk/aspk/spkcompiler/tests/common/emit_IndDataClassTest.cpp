#include <iostream>
#include <string>
#include <map>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>
#include "emit_IndDataClass.h"
#include "emit_IndDataClassTest.h"
#include "SymbolTable.h"
#include "Symbol.h"
#include "NonmemTranslator.h"

using namespace std;
using namespace CppUnit;

void emit_IndDataClassTest::setUp()
{
}

void emit_IndDataClassTest::tearDown()
{
}

void emit_IndDataClassTest::test()
{
  const int nIndividuals = 3;
  int nMeasurements[ nIndividuals + 1 ];
  SymbolTable table;
  std::map<NonmemTranslator::LABEL, NonmemTranslator::ALIAS> label_alias_mapping;
  std::map<NonmemTranslator::LABEL, NonmemTranslator::MEASUREMENT> data_for[ nIndividuals +1 ];
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
  Symbol ZZ( "z", Symbol::SCALAR, Symbol::DOUBLE, false );
  Symbol VV( "v", Symbol::SCALAR, Symbol::DOUBLE, false );
  table.insert( XX );
  table.insert( YY );
  table.insert( ZZ );
  table.insert( VV );

  label_alias_mapping[ "x" ] = "y";
  label_alias_mapping[ "z" ] = "v";

  data_for[ALAN]["x"].resize( nMeasurements[ALAN] );
  data_for[ALAN]["z"].resize( 1 );
  for( int i=0; i<nMeasurements[ALAN]; i++ )
    data_for[ALAN]["x"][i] = (double)i;
  data_for[ALAN]["z"][0] = (double)ALAN;

  data_for[JIAJI]["x"].resize( nMeasurements[JIAJI] );
  data_for[JIAJI]["z"].resize( 1 );
  for( int i=0; i<nMeasurements[JIAJI]; i++ )
    data_for[JIAJI]["x"][i] = (double)i;
  data_for[JIAJI]["z"][0] = (double)JIAJI;

  data_for[MITCH]["x"].resize( nMeasurements[MITCH] );
  data_for[MITCH]["z"].resize( 1 );
  for( int i=0; i<nMeasurements[MITCH]; i++ )
    data_for[MITCH]["x"][i] = (double)i;
  data_for[MITCH]["z"][0] = (double)MITCH;

  FILE * IndData_h = fopen( "IndData.h", "w" );
  emit_IndDataClass( IndData_h, nIndividuals, &table, label_alias_mapping, data_for, order_id_pair );
  fclose( IndData_h );
  
  FILE * IndData_cpp = fopen( "IndData.cpp", "w" );
  emit_initIndDataObjects( IndData_cpp, nIndividuals, &table, label_alias_mapping, data_for, order_id_pair );
  emit_releaseIndDataObjects( IndData_cpp, nIndividuals, &table, label_alias_mapping, data_for, order_id_pair );
  fclose( IndData_cpp );

  if( system( "g++ -c IndData.cpp" ) != 0 )
  {
     cerr << "Failed to compile IndData.cpp!" << endl;
  }
}


CppUnit::Test * emit_IndDataClassTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "emit_IndDataClassTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<emit_IndDataClassTest>
			 ("test", &emit_IndDataClassTest::test ) );

 return suiteOfTests;
}

