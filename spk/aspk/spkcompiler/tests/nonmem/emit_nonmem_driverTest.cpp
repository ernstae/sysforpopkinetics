#include <iostream>
#include <string>
#include <map>
#include <valarray>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>


#include "emit_nonmem_driver.h"
#include "emit_nonmem_driverTest.h"
#include "read_nonmem_datafile_gamma_wt_time_y.h"
#include "../common/FitPopulationTestModel.h"

#include <libspkcompiler/NonmemTranslator.h>
#include <spk/Objective.h>
#include <spk/SpkModel.h>
#include <spk/popStatistics.h>
#include <nag.h>
#include <nagg05.h>

using namespace std;
using namespace CppUnit;

void emit_nonmem_driverTest::setUp()
{
}

void emit_nonmem_driverTest::tearDown()
{
}

void emit_nonmem_driverTest::test()
{
  struct SpkParameters    spkInfo;
  struct NonmemParameters nonmemInfo;
  prepInfo( spkInfo, nonmemInfo );

  char model_init_block[ 512 ];

  testPopSimOnly( "sim_driver", 
		  "FitPopulationTestModel",
                  "FitPopulationTestModel model( nPopPar, nIndPar, nMeasurementsAll );\n",
		  spkInfo,
	          nonmemInfo
		);
  testPopEstOnly( "est_driver", 
		  "FitPopulationTestModel",
                  "FitPopulationTestModel model( nPopPar, nIndPar, nMeasurementsAll );\n",
		  spkInfo,
		  nonmemInfo );

  testPopSimEst( "simest_driver", 
		  "FitPopulationTestModel",
                  "FitPopulationTestModel model( nPopPar, nIndPar, nMeasurementsAll );\n",
		  spkInfo,
		  nonmemInfo );

  testPopSimEstStat( "simeststat_driver", 
		  "FitPopulationTestModel",
                  "FitPopulationTestModel model( nPopPar, nIndPar, nMeasurementsAll );\n",
		  spkInfo,
		  nonmemInfo );

}


void emit_nonmem_driverTest::testPopSimOnly( char driver_name[],
				      char modelClass_name[],
				      char modelObject_init_block[],
				      struct SpkParameters & spk,
	                              struct NonmemParameters & nonmem
				      )
{
  char model_h[ 256 ];
  char model_cpp[ 256 ];
  char driver_cpp[ 256 ];
  char driver_obj[ 256 ];
  char driver_exe[ 256 ];
  sprintf( model_h,    "%s.h",   modelClass_name );
  sprintf( model_cpp,  "%s.cpp", modelClass_name );
  sprintf( driver_cpp, "%s.cpp", driver_name );
  sprintf( driver_obj, "%s.o",   driver_name );
  sprintf( driver_exe, "%s",     driver_name );

  spk.analysis                  = SpkParameters::POPULATION;
  spk.isEstimation              = false;
  spk.isSimulation              = true;

  spk.isPopWarmStart            = false;
  spk.isPopParOut               = false;
  spk.isPopObjOut               = false;
  spk.isPopObj_popParOut        = false;
  spk.isPopObj_popPar_popParOut = false;

  spk.isPopStderrorOut          = false;
  spk.isPopCorrelationOut       = false;
  spk.isPopCovarianceOut        = false;
  spk.isPopCoefficientOut       = false;
  spk.isPopConfidenceOut        = false;

  spk.isIndWarmStart            = false;
  spk.isIndParOut               = false;
  spk.isIndObjOut               = false;
  spk.isIndObj_indParOut        = false;
  spk.isIndObj_indPar_indParOut = false;

  spk.isIndStderrorOut          = false;
  spk.isIndCorrelationOut       = false;
  spk.isIndCovarianceOut        = false;
  spk.isIndCoefficientOut       = false;
  spk.isIndConfidenceOut        = false;

  FILE * out = fopen( driver_cpp, "w" );

  fprintf( out, "#include \"%s\"\n", model_h );
  emit_nonmem_driver( 
		  out, 
		  spk.nIndividuals, 
		  modelClass_name, 
		  modelObject_init_block, 
		  spk, nonmem );
  fclose( out );

  //
  // Try compiling it and see if it succeeds.
  // This is pretty much all I can do to test whether the generated code
  // is correct or not.
  //
  char command[ 256 ];
  sprintf( command, "g++ -g %s %s -lspk /usr/local/lib/libnagc.a -lpthread -lm -o %s", 
	   driver_cpp, model_cpp, driver_exe );
  CPPUNIT_ASSERT_MESSAGE( "Compilation failed!", system( command ) == 0 );


  sprintf( command, "./%s", driver_exe );
  CPPUNIT_ASSERT_MESSAGE( "Execution failed!", system( command ) == 0 );

  remove( driver_cpp );
  remove( driver_obj );
  remove( driver_exe );
}

void emit_nonmem_driverTest::testPopEstOnly( char driver_name[],
				      char modelClass_name[],
				      char modelObject_init_block[],
				      struct SpkParameters & spk,
	                              struct NonmemParameters & nonmem
					     )
{
  char model_h[ 256 ];
  char model_cpp[ 256 ];
  char driver_cpp[ 256 ];
  char driver_obj[ 256 ];
  char driver_exe[ 256 ];
  sprintf( model_h,    "%s.h",   modelClass_name );
  sprintf( model_cpp,  "%s.cpp", modelClass_name );
  sprintf( driver_cpp, "%s.cpp", driver_name );
  sprintf( driver_obj, "%s.o",   driver_name );
  sprintf( driver_exe, "%s",     driver_name );

  spk.analysis                  = SpkParameters::POPULATION;
  spk.isEstimation              = true;
  spk.isSimulation              = false;

  spk.isPopWarmStart            = true;
  spk.isPopParOut               = true;
  spk.isPopObjOut               = true;
  spk.isPopObj_popParOut        = true;
  spk.isPopObj_popPar_popParOut = true;

  spk.isPopStderrorOut          = false;
  spk.isPopCorrelationOut       = false;
  spk.isPopCovarianceOut        = false;
  spk.isPopCoefficientOut       = false;
  spk.isPopConfidenceOut        = false;

  spk.isIndWarmStart            = false;
  spk.isIndParOut               = false;
  spk.isIndObjOut               = false;
  spk.isIndObj_indParOut        = false;
  spk.isIndObj_indPar_indParOut = false;

  spk.isIndStderrorOut          = false;
  spk.isIndCorrelationOut       = false;
  spk.isIndCovarianceOut        = false;
  spk.isIndCoefficientOut       = false;
  spk.isIndConfidenceOut        = false;

  FILE * out = fopen( driver_cpp, "w" );

  fprintf( out, "#include \"%s\"\n", model_h );
  emit_nonmem_driver( 
		  out, 
		  spk.nIndividuals, 
		  modelClass_name, 
		  modelObject_init_block, 
		  spk, nonmem );
  fclose( out );

  //
  // Try compiling it and see if it succeeds.
  // This is pretty much all I can do to test whether the generated code
  // is correct or not.
  //
  char command[ 256 ];
  sprintf( command, "g++ -g %s %s -lspk /usr/local/lib/libnagc.a -lpthread -lm -o %s", 
	   driver_cpp, model_cpp, driver_exe );
  CPPUNIT_ASSERT_MESSAGE( "Compilation failed!", system( command ) == 0 );


  sprintf( command, "./%s", driver_exe );
  CPPUNIT_ASSERT_MESSAGE( "Execution failed!", system( command ) == 0 );

  remove( driver_cpp );
  remove( driver_obj );
  remove( driver_exe );
 
}
void emit_nonmem_driverTest::testPopSimEst( char driver_name[],
				      char modelClass_name[],
				      char modelObject_init_block[],
				      struct SpkParameters & spk,
	                              struct NonmemParameters & nonmem
					     )
{
  char model_h[ 256 ];
  char model_cpp[ 256 ];
  char driver_cpp[ 256 ];
  char driver_obj[ 256 ];
  char driver_exe[ 256 ];
  sprintf( model_h,    "%s.h",   modelClass_name );
  sprintf( model_cpp,  "%s.cpp", modelClass_name );
  sprintf( driver_cpp, "%s.cpp", driver_name );
  sprintf( driver_obj, "%s.o",   driver_name );
  sprintf( driver_exe, "%s",     driver_name );

  spk.analysis                  = SpkParameters::POPULATION;
  spk.isEstimation              = true;
  spk.isSimulation              = true;

  spk.isPopWarmStart            = true;
  spk.isPopParOut               = true;
  spk.isPopObjOut               = true;
  spk.isPopObj_popParOut        = true;
  spk.isPopObj_popPar_popParOut = true;

  spk.isPopStderrorOut          = false;
  spk.isPopCorrelationOut       = false;
  spk.isPopCovarianceOut        = false;
  spk.isPopCoefficientOut       = false;
  spk.isPopConfidenceOut        = false;

  spk.isIndWarmStart            = false;
  spk.isIndParOut               = true;
  spk.isIndObjOut               = false;
  spk.isIndObj_indParOut        = false;
  spk.isIndObj_indPar_indParOut = false;

  spk.isIndStderrorOut          = false;
  spk.isIndCorrelationOut       = false;
  spk.isIndCovarianceOut        = false;
  spk.isIndCoefficientOut       = false;
  spk.isIndConfidenceOut        = false;

  FILE * out = fopen( driver_cpp, "w" );

  fprintf( out, "#include \"%s\"\n", model_h );
  emit_nonmem_driver( 
		  out, 
		  spk.nIndividuals, 
		  modelClass_name, 
		  modelObject_init_block, 
		  spk, nonmem );
  fclose( out );

  //
  // Try compiling it and see if it succeeds.
  // This is pretty much all I can do to test whether the generated code
  // is correct or not.
  //
  char command[ 256 ];
  sprintf( command, "g++ -g %s %s -lspk /usr/local/lib/libnagc.a -lpthread -lm -o %s", 
	   driver_cpp, model_cpp, driver_exe );
  CPPUNIT_ASSERT_MESSAGE( "Compilation failed!", system( command ) == 0 );


  sprintf( command, "./%s", driver_exe );
  CPPUNIT_ASSERT_MESSAGE( "Execution failed!", system( command ) == 0 );

  remove( driver_cpp );
  remove( driver_obj );
  remove( driver_exe );
 
}
void emit_nonmem_driverTest::testPopSimEstStat( 
			   char driver_name[],
			   char modelClass_name[],
			   char modelObject_init_block[],
			   struct SpkParameters & spk,
			   struct NonmemParameters & nonmem )
{
  char model_h[ 256 ];
  char model_cpp[ 256 ];
  char driver_cpp[ 256 ];
  char driver_obj[ 256 ];
  char driver_exe[ 256 ];
  sprintf( model_h,    "%s.h",   modelClass_name );
  sprintf( model_cpp,  "%s.cpp", modelClass_name );
  sprintf( driver_cpp, "%s.cpp", driver_name );
  sprintf( driver_obj, "%s.o",   driver_name );
  sprintf( driver_exe, "%s",     driver_name );

  spk.analysis                  = SpkParameters::POPULATION;
  spk.isEstimation              = true;
  spk.isSimulation              = true;

  spk.isPopWarmStart            = true;
  spk.isPopParOut               = true;
  spk.isPopObjOut               = true;
  spk.isPopObj_popParOut        = true;
  spk.isPopObj_popPar_popParOut = true;

  spk.isPopStderrorOut          = false;
  spk.isPopCorrelationOut       = false;
  spk.isPopCovarianceOut        = false;
  spk.isPopCoefficientOut       = false;
  spk.isPopConfidenceOut        = false;

  spk.isIndWarmStart            = true;
  spk.isIndParOut               = true;
  spk.isIndObjOut               = true;
  spk.isIndObj_indParOut        = true;
  spk.isIndObj_indPar_indParOut = true;

  spk.isIndStderrorOut          = true;
  spk.isIndCorrelationOut       = false;
  spk.isIndCovarianceOut        = false;
  spk.isIndCoefficientOut       = false;
  spk.isIndConfidenceOut        = false;

  FILE * out = fopen( driver_cpp, "w" );

  fprintf( out, "#include \"%s\"\n", model_h );
  emit_nonmem_driver( 
		  out, 
		  spk.nIndividuals, 
		  modelClass_name, 
		  modelObject_init_block, 
		  spk, nonmem );
  fclose( out );

  //
  // Try compiling it and see if it succeeds.
  // This is pretty much all I can do to test whether the generated code
  // is correct or not.
  //
  char command[ 256 ];
  sprintf( command, "g++ -g %s %s -lspk /usr/local/lib/libnagc.a -lpthread -lm -o %s", 
	   driver_cpp, model_cpp, driver_exe );
  CPPUNIT_ASSERT_MESSAGE( "Compilation failed!", system( command ) == 0 );


  sprintf( command, "./%s", driver_exe );
  CPPUNIT_ASSERT_MESSAGE( "Execution failed!", system( command ) == 0 );

  remove( driver_cpp );
  remove( driver_obj );
  remove( driver_exe );
}
void emit_nonmem_driverTest::prepInfo( struct SpkParameters & spkInfo,
				struct NonmemParameters & nonmemInfo )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------
  // Objective
  enum Objective whichObjective = MODIFIED_LAPLACE;

  // Number of individuals.
  const int nInd = 10;
  spkInfo.nIndividuals = nInd;

  // Number of measurements per individual (same for all)
  valarray<int> N( 1, nInd );
  spkInfo.nMeasurementsAll.resize( nInd );
  spkInfo.nMeasurementsAll = N;

  // Number of measurements in total
  const int nY = N.sum();

  const int nAlp = 2;

  const int nB = 1;

  //------------------------------------------------------------
  // Quantities that define the problem.
  //------------------------------------------------------------

  // Mean and variance of the true transfer rate, betaTrue.
  double meanBetaTrue = 1.0;
  double varBetaTrue  = 5.0;

  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Measurement values, y.
  valarray<double> y( nY );

  // These will hold the generated values for the true measurement 
  // noise, eTrue, and the true random population parameters, bTrue.
  double eTrue;
  double bTrue;

  // Mean, variance, and standard deviation of eTrue and bTrue.
  double meanETrue = 0.0;
  double varETrue  = 1.0;
  double sdETrue   = sqrt( varETrue );
  double meanBTrue = 0.0;
  double varBTrue  = varBetaTrue;
  double sdBTrue   = sqrt( varBTrue );

  // Compute the measurements for each individual.
  Integer seed = 0;
  g05cbc(seed);
  for ( int i = 0; i < nInd; i++ )
  {
    eTrue = nag_random_normal( meanETrue, sdETrue );
    bTrue = nag_random_normal( meanBTrue, sdBTrue );

    y[ i ] = meanBetaTrue + bTrue + eTrue;
  }

  spkInfo.measurementsAll.resize( nY );
  spkInfo.measurementsAll = y;

  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------

  valarray<double> alpTrue( nAlp );
  valarray<double> alpLow ( nAlp );
  valarray<double> alpUp  ( nAlp );
  valarray<double> alpIn  ( nAlp );
  valarray<double> alpOut ( nAlp );
  valarray<double> alpStep( nAlp );

  // Set the values associated with alp(1).
  alpTrue[ 0 ] = meanBetaTrue;
  alpLow [ 0 ] = -10.0;
  alpUp  [ 0 ] = 10.0;
  alpIn  [ 0 ] = -1.0;
  alpStep[ 0 ] = 1.0e-2;

  // Set the values associated with alp(2).
  alpTrue[ 1 ] = varBetaTrue;
  alpLow [ 1 ] = 1.0e-3;
  alpUp  [ 1 ] = 100.0;
  alpIn  [ 1 ] = 0.5;
  alpStep[ 1 ] = 1.0e-2;
  
  spkInfo.popParIn.resize( nAlp );
  spkInfo.popParIn = alpIn;

  spkInfo.popParLow.resize( nAlp );
  spkInfo.popParLow = alpLow;

  spkInfo.popParUp.resize( nAlp );
  spkInfo.popParUp = alpUp;
  
  spkInfo.popParStep.resize( nAlp );
  spkInfo.popParStep = alpStep;

  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  valarray<double> bLow ( -1.5e+1, nB );
  valarray<double> bUp  ( +1.0e+1, nB );
  valarray<double> bStep(  1.0e-2, nB );

  valarray<double> bIn ( 1., nB * nInd );
  valarray<double> bOut(     nB * nInd );

  spkInfo.indParIn.resize( nB * nInd );
  spkInfo.indParIn = bIn;

  spkInfo.indParUp.resize( nB );
  spkInfo.indParUp = bUp;

  spkInfo.indParLow.resize( nB );
  spkInfo.indParLow = bLow;

  spkInfo.indParStep.resize( nB );
  spkInfo.indParStep = bStep;
  //------------------------------------------------------------
  // Remaining inputs to fitPopulation.
  //------------------------------------------------------------

  // Set the values associated with the individual objective function.
  spkInfo.indEpsilon = 1.0e-6;
  spkInfo.indMaxItr  = 40;
  spkInfo.indTrace   = 0;

  // Set the values associated with the population objective function.
  spkInfo.popEpsilon = 1.0e-6;
  spkInfo.popMaxItr  = 10;
  spkInfo.popTrace   = 0;
  spkInfo.isPopWarmStart = true;
}


CppUnit::Test * emit_nonmem_driverTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "emit_nonmem_driverTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<emit_nonmem_driverTest>
			 ("test", &emit_nonmem_driverTest::test ) );

 return suiteOfTests;
}

