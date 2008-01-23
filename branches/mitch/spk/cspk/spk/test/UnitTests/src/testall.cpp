#include <iostream>
#include <string>
#include <map>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>
#include <pvm3.h>

#include "ABA_xTest.h"
#include "addTest.h"
#include "AkronBtimesCTest.h"
#include "AkronItimesCTest.h"
#include "allTrueTest.h"
#include "BlockAllocTest.h"
#include "blockDiagonalDerivativeTest.h"
#include "calcMeanTest.h"
#include "centdiffModelTest.h"
#include "centdiffTest.h"
#include "choleskyTest.h"
#include "countTruesTest.h"
#include "CovarianceTest.h"
#include "derParStatisticsTest.h"
#include "detTest.h"
#include "divByScalarTest.h"
#include "elementwiseAndTest.h"
#include "elementwiseOrTest.h"
#include "elsq_xBlockDiagTest.h"
#include "elsq_xDiagTest.h"
#include "elsq_xTest.h"
#include "elsqTest.h"
#include "EqIndModelTest.h"
#include "estimateBTest.h"
#include "expectedHessianTest.h"
#include "firstOrderOptTest.h"
#include "fitIndividualErrorTest.h"
#include "fitIndividualTest.h"
#include "fitPopulationTest.h"
#include "FunctionTest.h"
#include "getColTest.h"
#include "getMulColsTest.h"
#include "getMulRowsTest.h"
#include "getRowTest.h"
#include "getSubblockTest.h"
#include "identityTest.h"
#include "IkronBtimesCTest.h"
#include "IndInputDataPackageTest.h"
#include "IndOutputDataPackageTest.h"
#include "indResidualsTest.h"
#include "IndResultsTest.h"
#include "indStatisticsTest.h"
#include "IndVarsTest.h"
#include "inverseTest.h"
#include "inxToMaxTest.h"
#include "intToOrdinalStringTest.h"
#include "isDblEpsEqualTest.h"
#include "isDmatEpsEqualTest.h"
#include "isGreaterThanOrEqualToTest.h"
#include "isLessThanOrEqualToTest.h"
#include "isNotANumberTest.h"
#include "isSymmetricTest.h"
#include "isUnnormNumberTest.h"
#include "lambda2diffTest.h"
#include "lambdaTest.h"
#include "linearInterpolateTest.h"
#include "lTildeTest.h"
#include "lTildePvmTest.h"
#include "mapObjDiffTest.h"
#include "mapObjTest.h"
#include "mapOptTest.h"
#include "mapTildeTest.h"
#include "matabsTest.h"
#include "matmaxTest.h"
#include "matminTest.h"
#include "MatrixTest.h"
#include "mulByScalarTest.h"
#include "multiplyTest.h"
#include "NaiveFoModelTest.h"
#include "normTest.h"
#include "OptimizerTest.h"
#include "placeRowsTest.h"
#include "PopConstValsTest.h"
#include "popResidualsTest.h"
#include "popStatisticsTest.h"
#include "PopVarsTest.h"
#include "ppkaOptTest.h"
#include "printInMatrixTest.h"
#include "quasiNewtonAnyBoxTest.h"
#include "randNormalTest.h"
#include "replaceIthTest.h"
#include "replaceJthTest.h"
#include "replaceSubblockTest.h"
#include "wresTest.h"
#include "rvecInverseTest.h"
#include "rvecTest.h"
#include "simulateTest.h"
#include "SpkErrorTest.h"
#include "SpkExceptionTest.h"
#include "spk_non_par_test.h"
#include "SpkModelErrorTest.h"
#include "subtractTest.h"
#include "symmetrizeTest.h"
#include "transposeDerivativeTest.h"
#include "transposeRowBlocksTest.h"
#include "transposeTest.h"
#include "twoStageMethodTest.h"
#include "UTranTimesSymKronSymTimesU_xTest.h"
#include "UTranTimesSymKronSymTimesUTest.h"
#include "WarningsManagerTest.h"

using namespace std;
using namespace CppUnit;

void usage()
{
  printf( "Usage: ./testall <LIST>\n" );
  printf( "\n" );
  printf( "<LIST> is optional.\n" );
  printf( "If it is omitted, all of the known test suites will be executed.\n" );
  printf( "Otherwise, it will be a space-separated list of CppUnit tests to be executed.\n" );
  return;
}

int main( int argc, const char * argv[] )
{
  char* arg[1];
  arg[0] = "localhost";
  pvm_start_pvmd(1, arg, 1);

  //  Turning on the floating point error detection mechanism
  FpErrorChecker checkerON;

  map<string, CppUnit::Test*> master_list_of_tests;
  vector<CppUnit::Test*> tests_to_be_executed;

  master_list_of_tests[ "ABA_xTest" ]                        = ABA_xTest::suite();
  master_list_of_tests[ "addTest" ]                          = addTest::suite();
  master_list_of_tests[ "AkronBtimesCTest" ]                 = AkronBtimesCTest::suite();
  master_list_of_tests[ "AkronItimesCTest" ]                 = AkronItimesCTest::suite();
  master_list_of_tests[ "allTrueTest" ]                      = allTrueTest::suite();
  master_list_of_tests[ "BlockAllocTest" ]                   = BlockAllocTest::suite();
  master_list_of_tests[ "blockDiagonalDerivativeTest" ]      = blockDiagonalDerivativeTest::suite();
  master_list_of_tests[ "calcMeanTest" ]                     = calcMeanTest::suite();
  master_list_of_tests[ "centdiffModelTest" ]                = centdiffModelTest::suite();
  master_list_of_tests[ "centdiffTest" ]                     = centdiffTest::suite();
  master_list_of_tests[ "choleskyTest" ]                     = choleskyTest::suite();
  master_list_of_tests[ "countTruesTest" ]                   = countTruesTest::suite();
  master_list_of_tests[ "CovarianceTest" ]                   = CovarianceTest::suite();
  master_list_of_tests[ "derParStatisticsTest" ]             = derParStatisticsTest::suite();
  master_list_of_tests[ "detTest" ]                          = detTest::suite();
  master_list_of_tests[ "divByScalarTest" ]                  = divByScalarTest::suite();
  master_list_of_tests[ "elementwiseAndTest" ]               = elementwiseAndTest::suite();
  master_list_of_tests[ "elementwiseOrTest" ]                = elementwiseOrTest::suite();
  master_list_of_tests[ "elsq_xBlockDiagTest" ]              = elsq_xBlockDiagTest::suite();
  master_list_of_tests[ "elsq_xDiagTest" ]                   = elsq_xDiagTest::suite();
  master_list_of_tests[ "elsq_xTest" ]                       = elsq_xTest::suite();
  master_list_of_tests[ "elsqTest" ]                         = elsqTest::suite();
  master_list_of_tests[ "EqIndModelTest" ]                   = EqIndModelTest::suite();
  master_list_of_tests[ "estimateBTest" ]                    = estimateBTest::suite();
  master_list_of_tests[ "expectedHessianTest" ]              = expectedHessianTest::suite();
  master_list_of_tests[ "firstOrderOptTest" ]                = firstOrderOptTest::suite();
  master_list_of_tests[ "fitIndividualErrorTest" ]           = fitIndividualErrorTest::suite();
  master_list_of_tests[ "fitIndividualTest" ]                = fitIndividualTest::suite();
  master_list_of_tests[ "fitPopulationTest" ]                = fitPopulationTest::suite();
  master_list_of_tests[ "FunctionTest" ]                     = FunctionTest::suite();
  master_list_of_tests[ "getColTest" ]                       = getColTest::suite();
  master_list_of_tests[ "getMulColsTest" ]                   = getMulColsTest::suite();
  master_list_of_tests[ "getMulRowsTest" ]                   = getMulRowsTest::suite();
  master_list_of_tests[ "getRowTest" ]                       = getRowTest::suite();
  master_list_of_tests[ "getSubblockTest" ]                  = getSubblockTest::suite();
  master_list_of_tests[ "identityTest" ]                     = identityTest::suite();
  master_list_of_tests[ "IkronBtimesCTest" ]                 = IkronBtimesCTest::suite();
  master_list_of_tests[ "IndInputDataPackageTest" ]          = IndInputDataPackageTest::suite();
  master_list_of_tests[ "IndOutputDataPackageTest" ]         = IndOutputDataPackageTest::suite();
  master_list_of_tests[ "indResidualsTest" ]                 = indResidualsTest::suite();
  master_list_of_tests[ "IndResultsTest" ]                   = IndResultsTest::suite();
  master_list_of_tests[ "indStatisticsTest" ]                = indStatisticsTest::suite();
  master_list_of_tests[ "IndVarsTest" ]                      = IndVarsTest::suite();
  master_list_of_tests[ "inverseTest" ]                      = inverseTest::suite();
  master_list_of_tests[ "inxToMaxTest" ]                     = inxToMaxTest::suite();
  master_list_of_tests[ "intToOrdinalStringTest" ]           = intToOrdinalStringTest::suite();
  master_list_of_tests[ "isDblEpsEqualTest" ]                = isDblEpsEqualTest::suite();
  master_list_of_tests[ "isDmatEpsEqualTest" ]               = isDmatEpsEqualTest::suite();
  master_list_of_tests[ "isGreaterThanOrEqualToTest" ]       = isGreaterThanOrEqualToTest::suite();
  master_list_of_tests[ "isLessThanOrEqualToTest" ]          = isLessThanOrEqualToTest::suite();
  master_list_of_tests[ "isNotANumberTest" ]                 = isNotANumberTest::suite();
  master_list_of_tests[ "isSymmetricTest" ]                  = isSymmetricTest::suite();
  master_list_of_tests[ "isUnnormNumberTest" ]               = isUnnormNumberTest::suite();
  master_list_of_tests[ "lambda2diffTest" ]                  = lambda2diffTest::suite();
  master_list_of_tests[ "lambdaTest" ]                       = lambdaTest::suite();
  master_list_of_tests[ "linearInterpolateTest" ]            = linearInterpolateTest::suite();
  master_list_of_tests[ "lTildeTest" ]                       = lTildeTest::suite();
  master_list_of_tests[ "lTildePvmTest" ]                    = lTildePvmTest::suite();
  master_list_of_tests[ "mapObjDiffTest" ]                   = mapObjDiffTest::suite();
  master_list_of_tests[ "mapObjTest" ]                       = mapObjTest::suite();
  master_list_of_tests[ "mapOptTest" ]                       = mapOptTest::suite();
  master_list_of_tests[ "mapTildeTest" ]                     = mapTildeTest::suite();
  master_list_of_tests[ "matabsTest" ]                       = matabsTest::suite();
  master_list_of_tests[ "matmaxTest" ]                       = matmaxTest::suite();
  master_list_of_tests[ "matminTest" ]                       = matminTest::suite();
  master_list_of_tests[ "MatrixTest" ]                       = MatrixTest::suite();
  master_list_of_tests[ "mulByScalarTest" ]                  = mulByScalarTest::suite();
  master_list_of_tests[ "multiplyTest" ]                     = multiplyTest::suite();
  master_list_of_tests[ "NaiveFoModelTest" ]                 = NaiveFoModelTest::suite();
  master_list_of_tests[ "normTest" ]                         = normTest::suite();
  master_list_of_tests[ "OptimizerTest" ]                    = OptimizerTest::suite();
  master_list_of_tests[ "placeRowsTest" ]                    = placeRowsTest::suite();
  master_list_of_tests[ "PopConstValsTest" ]                 = PopConstValsTest::suite();
  master_list_of_tests[ "popResidualsTest" ]                 = popResidualsTest::suite();
  master_list_of_tests[ "popStatisticsTest" ]                = popStatisticsTest::suite();
  master_list_of_tests[ "PopVarsTest" ]                      = PopVarsTest::suite();
  master_list_of_tests[ "ppkaOptTest" ]                      = ppkaOptTest::suite();
  master_list_of_tests[ "printInMatrixTest" ]                = printInMatrixTest::suite();
  master_list_of_tests[ "quasiNewtonAnyBoxTest" ]            = quasiNewtonAnyBoxTest::suite();
  master_list_of_tests[ "randNormalTest" ]                   = randNormalTest::suite();
  master_list_of_tests[ "replaceIthTest" ]                   = replaceIthTest::suite();
  master_list_of_tests[ "replaceJthTest" ]                   = replaceJthTest::suite();
  master_list_of_tests[ "replaceSubblockTest" ]              = replaceSubblockTest::suite();
  master_list_of_tests[ "wresTest" ]                         = wresTest::suite();
  master_list_of_tests[ "rvecInverseTest" ]                  = rvecInverseTest::suite();
  master_list_of_tests[ "rvecTest" ]                         = rvecTest::suite();
  master_list_of_tests[ "simulateTest" ]                     = simulateTest::suite();
  master_list_of_tests[ "SpkErrorTest" ]                     = SpkErrorTest::suite();
  master_list_of_tests[ "SpkExceptionTest" ]                 = SpkExceptionTest::suite();
  master_list_of_tests[ "SpkModelErrorTest" ]                = SpkModelErrorTest::suite();
  master_list_of_tests[ "spk_non_par_test" ]                 = spk_non_par_test::suite();
  master_list_of_tests[ "subtractTest" ]                     = subtractTest::suite();
  master_list_of_tests[ "symmetrizeTest" ]                   = symmetrizeTest::suite();
  master_list_of_tests[ "transposeDerivativeTest" ]          = transposeDerivativeTest::suite();
  master_list_of_tests[ "transposeRowBlocksTest" ]           = transposeRowBlocksTest::suite();
  master_list_of_tests[ "transposeTest" ]                    = transposeTest::suite();
  master_list_of_tests[ "twoStageMethodTest" ]               = twoStageMethodTest::suite();
  master_list_of_tests[ "UTranTimesSymKronSymTimesU_xTest" ] = UTranTimesSymKronSymTimesU_xTest::suite();
  master_list_of_tests[ "UTranTimesSymKronSymTimesU_xTest" ] = UTranTimesSymKronSymTimesU_xTest::suite();
  master_list_of_tests[ "UTranTimesSymKronSymTimesUTest" ]   = UTranTimesSymKronSymTimesUTest::suite();
  master_list_of_tests[ "WarningsManagerTest" ]              = WarningsManagerTest::suite();
  
  //
  // This is the case where user didn't select specific unit tests.
  // That means, run them all in the single process mode.
  //
  if( argc == 1 )
    {
      // push all existing unit test classes into the list
# if 0
Brad: 2006-05-09: It appears to me this should have never been here.
      tests_to_be_executed.resize( master_list_of_tests.size() );
# endif
      map<string, CppUnit::Test*>::const_iterator p = master_list_of_tests.begin();
      for( p; p != master_list_of_tests.end(); p++ )
	tests_to_be_executed.push_back( p->second );
    }
  //
  // This is the case where user either specified a list of unit tests to run
  // or requested the parallel execution.
  //
  if( argc >= 2 )
    {
      if( strcmp( argv[1], "--help" ) == 0 || strcmp( argv[1], "?" ) == 0 )
	{
	  usage();
	  return 0;
	}
      for( int i=1; i<argc; i++ )
	{
	  map<string, CppUnit::Test*>::const_iterator p = master_list_of_tests.find( argv[i] );
	  if( p != master_list_of_tests.end() )
	    tests_to_be_executed.push_back( p->second );
	  else
	    fprintf( stderr, "!!! %s is not found in the master list (typo?) !!!\n", argv[i] );
	}
    }

  CppUnit::TextUi::TestRunner runner;

  int n = tests_to_be_executed.size();
  for( int i=0; i<n; i++ )
    {
      runner.addTest( tests_to_be_executed[i] );
    }

  runner.run();
  pvm_halt();
  return 0;
}
