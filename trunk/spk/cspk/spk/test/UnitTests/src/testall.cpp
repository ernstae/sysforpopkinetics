#include <iostream>
#include <string>
#include <map>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "src/ABA_xTest.h"
#include "src/addTest.h"
#include "src/AkronBtimesCTest.h"
#include "src/AkronItimesCTest.h"
#include "src/allTrueTest.h"
#include "src/BlockAllocTest.h"
#include "src/blockDiagonalDerivativeTest.h"
#include "src/calcMeanTest.h"
#include "src/centdiffModelTest.h"
#include "src/centdiffTest.h"
#include "src/choleskyTest.h"
#include "src/countTruesTest.h"
#include "src/CovarianceTest.h"
#include "src/detTest.h"
#include "src/divByScalarTest.h"
#include "src/elementwiseAndTest.h"
#include "src/elementwiseOrTest.h"
#include "src/elsq_xBlockDiagTest.h"
#include "src/elsq_xDiagTest.h"
#include "src/elsq_xTest.h"
#include "src/elsqTest.h"
#include "src/EqIndModelTest.h"
#include "src/estimateBTest.h"
#include "src/expectedHessianTest.h"
#include "src/firstOrderOptTest.h"
#include "src/fitIndividualErrorTest.h"
#include "src/fitIndividualTest.h"
#include "src/FunctionTest.h"
#include "src/getColTest.h"
#include "src/getMulColsTest.h"
#include "src/getMulRowsTest.h"
#include "src/getRowTest.h"
#include "src/getSubblockTest.h"
#include "src/identityTest.h"
#include "src/IkronBtimesCTest.h"
#include "src/IndInputDataPackageTest.h"
#include "src/IndOutputDataPackageTest.h"
#include "src/IndResultsTest.h"
#include "src/indStatisticsTest.h"
#include "src/IndVarsTest.h"
#include "src/inverseTest.h"
#include "src/inxToMaxTest.h"
#include "src/isDblEpsEqualTest.h"
#include "src/isDmatEpsEqualTest.h"
#include "src/isGreaterThanOrEqualToTest.h"
#include "src/isLessThanOrEqualToTest.h"
#include "src/isSymmetricTest.h"
#include "src/lambda2diffTest.h"
#include "src/lambdaTest.h"
#include "src/lTildeTest.h"
#include "src/mapObjDiffTest.h"
#include "src/mapObjTest.h"
#include "src/mapOptTest.h"
#include "src/mapTildeTest.h"
#include "src/matabsTest.h"
#include "src/matmaxTest.h"
#include "src/matminTest.h"
#include "src/MatrixTest.h"
#include "src/mulByScalarTest.h"
#include "src/multiplyTest.h"
#include "src/NaiveFoModelTest.h"
#include "src/normTest.h"
#include "src/OptimizerTest.h"
#include "src/placeRowsTest.h"
#include "src/PopConstValsTest.h"
#include "src/popStatisticsTest.h"
#include "src/PopVarsTest.h"
#include "src/ppkaOptTest.h"
#include "src/printInMatrixTest.h"
#include "src/randNormalTest.h"
#include "src/replaceIthTest.h"
#include "src/replaceJthTest.h"
#include "src/replaceSubblockTest.h"
#include "src/residualsTest.h"
#include "src/rvecInverseTest.h"
#include "src/rvecTest.h"
#include "src/simulateTest.h"
#include "src/SpkErrorTest.h"
#include "src/SpkExceptionTest.h"
#include "src/SpkModelErrorTest.h"
#include "src/sqpAnyBoxTest.h"
#include "src/subtractTest.h"
#include "src/symmetrizeTest.h"
#include "src/transposeDerivativeTest.h"
#include "src/transposeRowBlocksTest.h"
#include "src/transposeTest.h"
#include "src/UTranTimesSymKronSymTimesU_xTest.h"
#include "src/UTranTimesSymKronSymTimesUTest.h"

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
  master_list_of_tests[ "IndResultsTest" ]                   = IndResultsTest::suite();
  master_list_of_tests[ "indStatisticsTest" ]                = indStatisticsTest::suite();
  master_list_of_tests[ "IndVarsTest" ]                      = IndVarsTest::suite();
  master_list_of_tests[ "inverseTest" ]                      = inverseTest::suite();
  master_list_of_tests[ "inxToMaxTest" ]                     = inxToMaxTest::suite();
  master_list_of_tests[ "isDblEpsEqualTest" ]                = isDblEpsEqualTest::suite();
  master_list_of_tests[ "isDmatEpsEqualTest" ]               = isDmatEpsEqualTest::suite();
  master_list_of_tests[ "isGreaterThanOrEqualToTest" ]       = isGreaterThanOrEqualToTest::suite();
  master_list_of_tests[ "isLessThanOrEqualToTest" ]          = isLessThanOrEqualToTest::suite();
  master_list_of_tests[ "isSymmetricTest" ]                  = isSymmetricTest::suite();
  master_list_of_tests[ "lambda2diffTest" ]                  = lambda2diffTest::suite();
  master_list_of_tests[ "lambdaTest" ]                       = lambdaTest::suite();
  master_list_of_tests[ "lTildeTest" ]                       = lTildeTest::suite();
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
  master_list_of_tests[ "popStatisticsTest" ]                = popStatisticsTest::suite();
  master_list_of_tests[ "PopVarsTest" ]                      = PopVarsTest::suite();
  master_list_of_tests[ "ppkaOptTest" ]                      = ppkaOptTest::suite();
  master_list_of_tests[ "printInMatrixTest" ]                = printInMatrixTest::suite();
  master_list_of_tests[ "randNormalTest" ]                   = randNormalTest::suite();
  master_list_of_tests[ "replaceIthTest" ]                   = replaceIthTest::suite();
  master_list_of_tests[ "replaceJthTest" ]                   = replaceJthTest::suite();
  master_list_of_tests[ "replaceSubblockTest" ]              = replaceSubblockTest::suite();
  master_list_of_tests[ "residualsTest" ]                    = residualsTest::suite();
  master_list_of_tests[ "rvecInverseTest" ]                  = rvecInverseTest::suite();
  master_list_of_tests[ "rvecTest" ]                         = rvecTest::suite();
  master_list_of_tests[ "simulateTest" ]                     = simulateTest::suite();
  master_list_of_tests[ "SpkErrorTest" ]                     = SpkErrorTest::suite();
  master_list_of_tests[ "SpkExceptionTest" ]                 = SpkExceptionTest::suite();
  master_list_of_tests[ "SpkModelErrorTest" ]                = SpkModelErrorTest::suite();
  master_list_of_tests[ "sqpAnyBoxTest" ]                    = sqpAnyBoxTest::suite();
  master_list_of_tests[ "subtractTest" ]                     = subtractTest::suite();
  master_list_of_tests[ "symmetrizeTest" ]                   = symmetrizeTest::suite();
  master_list_of_tests[ "transposeDerivativeTest" ]          = transposeDerivativeTest::suite();
  master_list_of_tests[ "transposeRowBlocksTest" ]           = transposeRowBlocksTest::suite();
  master_list_of_tests[ "transposeTest" ]                    = transposeTest::suite();
  master_list_of_tests[ "UTranTimesSymKronSymTimesU_xTest" ] = UTranTimesSymKronSymTimesU_xTest::suite();
  master_list_of_tests[ "UTranTimesSymKronSymTimesUTest" ]   = UTranTimesSymKronSymTimesUTest::suite();
  
  //
  // This is the case where user didn't select specific unit tests.
  // That means, run them all in the single process mode.
  //
  if( argc == 1 )
    {
      // push all existing unit test classes into the list
      tests_to_be_executed.resize( master_list_of_tests.size() );
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

  return 0;
}
