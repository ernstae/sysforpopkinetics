/**
 * @file emit_IndDataTest.cpp
 * Declares a unit test for emit_IndData() function.
 */

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
  //===================================================================
  //  IndData.h generated by emit_IndData() called from this test
  //===================================================================
  // #ifndef INDDATA_H
  // #define INDDATA_H
  //
  // #include <vector>
  // #include <spk/SpkValarray.h>
  //
  // class IndData{
  //   public:
  //      IndData( const std::string & ID, 
  //         const SPK_VA::valarray<double>& wIn, 
  //         const SPK_VA::valarray<double>& xIn, 
  //         const SPK_VA::valarray<double>& zIn );
  //      const SPK_VA::valarray<double> w;
  //      const SPK_VA::valarray<double> x;
  //      const SPK_VA::valarray<double> y;
  //      const SPK_VA::valarray<double> z;
  //      const SPK_VA::valarray<double> v;
  //      const std::string ID;
  //      ~IndData();
  //
  //   protected:
  //      IndData();
  //      IndData( const IndData & );
  //      const IndData& operator=( const IndData& );
  // };
  //
  // class IndDataSet{
  //   public:
  //      IndDataSet();
  //     ~IndDataSet();
  //      const IndData& operator[]( int i ) const;
  //   private:
  //      std::vector<IndData*> all;
  //      const int n;
  //   protected:
  //      IndDataSet( const IndDataSet & );
  //      const IndDataSet& operator=( const IndDataSet& );
  // };
  // #endif
  //====================================================================
  //====================================================================
  //  IndData.cpp generated by emit_IndData() called from this test
  //====================================================================
  // #include "IndData.h"
  // #include <spk/SpkValarray.h>
  // #include <string>
  // #include <vector>
  // #include <map>
  //
  // using SPK_VA::valarray;
  // using namespace std;
  //
  // IndData::IndData( const std::string& IDIn, 
  //          const SPK_VA::valarray<double>& wIn, 
  //          const SPK_VA::valarray<double>& xIn, 
  //          const SPK_VA::valarray<double>& zIn )
  //         :
  //         ID(IDIn), 
  //         w(wIn),
  //         x(xIn), y(xIn),
  //         z(zIn), v(zIn)
  // {}
  // IndData::~IndData(){}
  // IndData::IndData(){}
  // IndData::IndData( const IndData & ){}
  // const IndData& IndData::operator=( const IndData& ){}
  // 
  // IndDataSet::IndDataSet()
  //       : n(3), all(3)
  // {
  //    double raw_w_Alan[] = { 0.000000 };
  //    valarray<double> w_Alan( raw_w_Alan, 1 );
  //    double raw_x_Alan[] = { 0.000000 };
  //    valarray<double> x_Alan( raw_x_Alan, 1 );
  //    double raw_z_Alan[] = { 0.000000 };
  //    valarray<double> z_Alan( raw_z_Alan, 1 );
  //    all[0] = new IndData( "Alan", w_Alan, x_Alan, z_Alan );
  //
  //    double raw_w_Jiaji[] = { 1.000000, 1001.000000 };
  //    valarray<double> w_Jiaji( raw_w_Jiaji, 2 );
  //    double raw_x_Jiaji[] = { 1.000000, 2.000000 };
  //    valarray<double> x_Jiaji( raw_x_Jiaji, 2 );
  //    double raw_z_Jiaji[] = { 1.000000, 101.000000 };
  //    valarray<double> z_Jiaji( raw_z_Jiaji, 2 );
  //    all[1] = new IndData( "Jiaji", w_Jiaji, x_Jiaji, z_Jiaji );
  // 
  //    double raw_w_Mitch[] = { 2.000000, 1002.000000, 2002.000000 };
  //    valarray<double> w_Mitch( raw_w_Mitch, 3 );
  //    double raw_x_Mitch[] = { 2.000000, 3.000000, 4.000000 };
  //    valarray<double> x_Mitch( raw_x_Mitch, 3 );
  //    double raw_z_Mitch[] = { 2.000000, 102.000000, 202.000000 };
  //    valarray<double> z_Mitch( raw_z_Mitch, 3 );
  //    all[2] = new IndData( "Mitch", w_Mitch, x_Mitch, z_Mitch );
  // 
  // }
  // IndDataSet::~IndDataSet()
  // {
  //    for( int i=0; i<n; i++ )
  //       delete all[i];
  // }
  // const IndData & IndDataSet::operator[]( int i ) const
  // {
  //    return *all[i];
  // }
  // IndDataSet::IndDataSet( const IndDataSet & ){}
  // const IndDataSet & IndDataSet::operator=( const IndDataSet& ){}
  //=================================================================
  const int nIndividuals = 3;
  int nMeasurements[ nIndividuals + 1 ];
  std::map<string, string> label_alias_mapping;
  std::vector< std::map<string, valarray<double> > > data_for( nIndividuals );
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
  CPPUNIT_ASSERT( IndData_h != NULL );
  FILE * IndData_cpp = fopen( "IndData.cpp", "w" );
  CPPUNIT_ASSERT( IndData_cpp != NULL );

  //=================================================================
  //
  emit_IndData( IndData_h, 
		IndData_cpp,
		label_alias_mapping, 
		data_for, 
		order_id_pair );
  //
  //=================================================================
  fclose( IndData_h );
  fclose( IndData_cpp );

  CPPUNIT_ASSERT_MESSAGE( "Failed to compile IndData.cpp!", 
			  system( "g++ -c IndData.cpp" ) == 0 );
  CPPUNIT_ASSERT_MESSAGE( "IndData.o cannot be found!", 
			  fopen( "IndData.o", "r" ) != NULL );


  //=================================================================
  // Generating a driver on fly to be able to link to
  // IndData.o which is just compiled above.
  //
  // temp_driver.cpp
  //=================================================================
  // #include "IndData.h"
  // int main()
  // {
  //    const int ALAN  = 0;
  //    const int JIAJI = 1;
  //    const int MITCH = 2;
  //    int N[] = { 1, 2, 3 };
  //    IndDataSet data_for;
  //    assert( data_for[ALAN] .ID == "Alan" );
  //    assert( data_for[JIAJI].ID == "Jiaji" );
  //    assert( data_for[MITCH].ID == "Mitch" );
  //    for( int j=0; j<3; j++ )
  //    {
  //       for( int i=0; i<N[j]; i++ )
  //       {
  //          if( data_for[j].x[i] != data_for[j].y[i] )
  //             return -1;
  //          if( data_for[j].z[i] != data_for[j].v[i] )
  //             return -1;
  //       }
  //    }
  //    return 0;
  // }
  //=================================================================
  char driver_filename[] = "temp_driver.cpp";
  FILE * pDriver = fopen( driver_filename, "w" );
  CPPUNIT_ASSERT( pDriver != NULL );

  fprintf( pDriver, "#include \"IndData.h\"\n" );
  fprintf( pDriver, "int main()\n" );
  fprintf( pDriver, "{\n" );
  fprintf( pDriver, "   const int ALAN  = %d;\n", ALAN );
  fprintf( pDriver, "   const int JIAJI = %d;\n", JIAJI );
  fprintf( pDriver, "   const int MITCH = %d;\n", MITCH );
  fprintf( pDriver, "   int N[] = { 1, 2, 3 };\n" );
  
  fprintf( pDriver, "   IndDataSet data_for;\n", nIndividuals );
  
  fprintf( pDriver, "   assert( data_for[ALAN] .ID == \"%s\" );\n", 
	   order_id_pair[ALAN] .c_str() );
  fprintf( pDriver, "   assert( data_for[JIAJI].ID == \"%s\" );\n", 
	   order_id_pair[JIAJI].c_str() );
  fprintf( pDriver, "   assert( data_for[MITCH].ID == \"%s\" );\n", 
	   order_id_pair[MITCH].c_str() );

  fprintf( pDriver, "   for( int j=0; j<3; j++ )\n" );
  fprintf( pDriver, "   {\n" );
  fprintf( pDriver, "      for( int i=0; i<N[j]; i++ )\n" );
  fprintf( pDriver, "      {\n" );
  fprintf( pDriver, "         if( data_for[j].x[i] != data_for[j].y[i] )\n" );
  fprintf( pDriver, "            return -1;\n" );
  fprintf( pDriver, "         if( data_for[j].z[i] != data_for[j].v[i] )\n" );
  fprintf( pDriver, "            return -1;\n" );
  fprintf( pDriver, "      }\n" );
  fprintf( pDriver, "   }\n" );
  
  fprintf( pDriver, "   return 0;\n" );
  fprintf( pDriver, "}\n" );
  fclose( pDriver );
  
  CPPUNIT_ASSERT_MESSAGE( "Failed to compile temp_driver.cpp!", 
			  system( "g++ -c temp_driver.cpp" ) == 0 );
  CPPUNIT_ASSERT_MESSAGE( "Failed to link temp_driver.cpp and IndData.cpp!", 
			  system( "g++ temp_driver.o IndData.o -o temp_driver" ) == 0 );
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

