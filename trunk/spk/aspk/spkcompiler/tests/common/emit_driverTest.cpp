#include <iostream>
#include <string>
#include <map>
#include <valarray>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include "emit_driver.h"
#include "emit_driverTest.h"
#include "SpkParameters.h"
#include <spk/Objective.h>
#include <spk/SpkModel.h>
#include <spk/popStatistics.h>

using namespace std;
using namespace CppUnit;



void emit_driverTest::setUp()
{
}

void emit_driverTest::tearDown()
{
}

void emit_driverTest::test()
{
  const int nIndividuals       = 3;
  const int nMeasurements[]    = { 1, 2, 3 };
  const int nTotalMeasurements = 1 + 2 + 3;
  const int nPopPar            = 2;
  const int nIndPar            = 3;
  const char model_name[]      = "UserModel";

  // Set up a SpkParameter data structure object.
  struct SpkParameters spk;

  spk.analysis                  = SpkParameters::POPULATION;
  spk.objective                 = FIRST_ORDER;
  spk.nIndividuals              = 3;
  spk.nMeasurementsAll.resize( nIndividuals );
  for( int i=0; i<nIndividuals; i++ )
    spk.nMeasurementsAll[i] = nMeasurements[i];
  spk.measurementsAll.resize( nTotalMeasurements );
  for( int i=0; i<nTotalMeasurements; i++ )
    spk.measurementsAll[i] = i * 100.0;
  spk.popParIn  .resize( nPopPar );
  spk.popParUp  .resize( nPopPar );
  spk.popParLow .resize( nPopPar );
  spk.popParStep.resize( nPopPar );
  for( int i=0; i<nPopPar; i++ )
    {
      spk.popParIn  [i] = i;
      spk.popParUp  [i] = i + 10;
      spk.popParLow [i] = i - 10;
      spk.popParStep[i] = 0.1;
    }
  spk.popEpsilon                = 0.001;
  spk.popMaxItr                 = 100;
  spk.popTrace                  = 1;
  spk.isPopWarmStart            = true;
  spk.isPopParOut               = true;
  spk.isPopObjOut               = true;
  spk.isPopObj_popParOut        = true;
  spk.isPopObj_popPar_popParOut = true;

  spk.indParIn  .resize( nIndPar * nIndividuals );
  spk.indParUp  .resize( nIndPar );
  spk.indParLow .resize( nIndPar );
  spk.indParStep.resize( nIndPar );
  for( int j=0; j<nIndividuals; j++ )
    {
      for( int i=0; i<nIndPar; i++ )
	spk.indParIn  [nIndPar * j + i] = i;
    }
  for( int i=0; i<nIndPar; i++ )
    {
      spk.indParUp  [i] = i + 10;
      spk.indParLow [i] = i - 10;
      spk.indParStep[i] = 0.1;
    }
  spk.indEpsilon                = 0.001;
  spk.indMaxItr                 = 100;
  spk.indTrace                  = 1;
  spk.isIndWarmStart            = true;
  spk.isIndParOut               = true;
  spk.isIndObjOut               = true;
  spk.isIndObj_indParOut        = true;
  spk.isIndObj_indPar_indParOut = true;


  spk.popCovarianceForm         = R;
  spk.isPopStderrorOut          = true;
  spk.isPopCorrelationOut       = true;
  spk.isPopCovarianceOut        = true;
  spk.isPopCoefficientOut       = true;
  spk.isPopConfidenceOut        = true;

  spk.isIndStderrorOut          = true;
  spk.isIndCorrelationOut       = true;
  spk.isIndCovarianceOut        = true;
  spk.isIndCoefficientOut       = true;
  spk.isIndConfidenceOut        = true;

  // Let emit_driver() process the data structure.
  FILE * hDriver = fopen( "driver.cpp", "w" );
  fprintf( hDriver, "#include <spk/SpkModel.h>\n" );
  fprintf( hDriver, "#include <valarray>\n" );
  fprintf( hDriver, "\n" );
  fprintf( hDriver, "using namespace std;\n" );
  fprintf( hDriver, "\n" );
  fprintf( hDriver, "class UserModel : public SpkModel\n" );
  fprintf( hDriver, "{\n" );
  fprintf( hDriver, "   valarray<double> indPar, popPar;\n" );
  fprintf( hDriver, "   int who;\n" );
  fprintf( hDriver, "\n" );
  fprintf( hDriver, "   void doSelectIndividual( int whoIn )\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "      who = whoIn;\n" );
  fprintf( hDriver, "   }\n" );
  fprintf( hDriver, "   void doSetPopPar( const valarray<double> & popParIn )\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "      popPar.resize( popParIn.size() );\n" );
  fprintf( hDriver, "      popPar = popParIn;\n" );
  fprintf( hDriver, "   }\n" );
  fprintf( hDriver, "   void doSetIndPar( const valarray<double> & indParIn )\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "      indPar.resize( indParIn.size() );\n" );
  fprintf( hDriver, "      indPar = indParIn;\n" );
  fprintf( hDriver, "   }\n" );
  fprintf( hDriver, "\n" );
  fprintf( hDriver, "   void doDataMean( valarray<double> & fOut ) const\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "   }\n" );
  fprintf( hDriver, "   bool doDataMean_popPar( valarray<double>& f_alpOut ) const\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "   }\n" );
  fprintf( hDriver, "   bool doDataMean_indPar( valarray<double> & f_bOut ) const\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "   }\n" );
  fprintf( hDriver, "   void doDataVariance( valarray<double> & ROut ) const\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "   }\n" );
  fprintf( hDriver, "   bool doDataVariance_popPar( valarray<double> & R_alpOut ) const\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "   }\n" );
  fprintf( hDriver, "   bool doDataVariance_indPar( valarray<double> & R_bOut ) const\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "   }\n" );
  fprintf( hDriver, "   void doIndParVariance( valarray<double> & DOut ) const\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "   }\n" );
  fprintf( hDriver, "   bool doIndParVariance_popPar( valarray<double> & D_alpOut ) const\n" );
  fprintf( hDriver, "   {\n" );
  fprintf( hDriver, "   }\n" ); 
  fprintf( hDriver, "};\n" );
  fprintf( hDriver, "\n" );
 
  emit_driver( hDriver, nIndividuals, model_name, spk );
  fclose( hDriver );

  // Try compiling it and see if it succeeds.
  CPPUNIT_ASSERT_MESSAGE( "Compilation failed!", 
		  system( "g++ driver.cpp -lspk /usr/local/lib/libnagc.a -lpthread -lm -g" ) 
			  == 0 );
  remove( "driver.cpp" );
  remove( "driver.o" );
  remove( "driver" );
}


CppUnit::Test * emit_driverTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "emit_driverTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<emit_driverTest>
			 ("test", &emit_driverTest::test ) );

 return suiteOfTests;
}

