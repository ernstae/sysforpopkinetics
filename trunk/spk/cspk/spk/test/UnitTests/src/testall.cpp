#include <iostream>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "src/SpkErrorTest.h"
#include "src/SpkExceptionTest.h"
#include "src/BlockAllocTest.h"
#include "src/MatrixTest.h"
#include "src/addTest.h"
#include "src/subtractTest.h"
#include "src/multiplyTest.h"
#include "src/divByScalarTest.h"
#include "src/mulByScalarTest.h"
#include "src/backDivTest.h"
#include "src/allTrueTest.h"
#include "src/detTest.h"
#include "src/elementwiseAndTest.h"
#include "src/elementwiseOrTest.h"
#include "src/identityTest.h"
#include "src/countTruesTest.h"
#include "src/isDblEpsEqualTest.h"
#include "src/isGreaterThanOrEqualToTest.h"
#include "src/isLessThanOrEqualToTest.h"
#include "src/isSymmetricTest.h"
#include "src/matabsTest.h"
#include "src/matmaxTest.h"
#include "src/matminTest.h"
#include "src/getColTest.h"
#include "src/getMulColsTest.h"
#include "src/inverseTest.h"
#include "src/printInMatrixTest.h"
#include "src/transposeTest.h"
#include "src/symmetrizeTest.h"
#include "src/rvecTest.h"
#include "src/rvecInverseTest.h"
#include "src/replaceJthTest.h"
#include "src/replaceIthTest.h"
#include "src/replaceSubblockTest.h"
#include "src/transposeRowBlocksTest.h"
#include "src/transposeDerivativeTest.h"
#include "src/SpkModelErrorTest.h"
#include "src/placeRowsTest.h"
#include "src/printInMatrixTest.h"
#include "src/AkronBtimesCTest.h"
#include "src/AkronItimesCTest.h"
#include "src/IkronBtimesCTest.h"
#include "src/elsqTest.h"
#include "src/elsq_xTest.h"
#include "src/centdiffTest.h"
#include "src/elsq_xDiagTest.h"
#include "src/elsq_xBlockDiagTest.h"
#include "src/choleskyTest.h"
#include "src/CovarianceTest.h"
#include "src/ABA_xTest.h"
#include "src/blockDiagonalDerivativeTest.h"
#include "src/centdiffModelTest.h"
#include "src/IndResultsTest.h"
#include "src/OptimizerTest.h"
#include "src/UTranTimesSymKronSymTimesUTest.h"
#include "src/UTranTimesSymKronSymTimesU_xTest.h"
#include "src/FunctionTest.h"
#include "src/FullDataCovarianceTest.h"
#include "src/FullIndParCovarianceTest.h"
#include "src/sqpAnyBoxTest.h"
#include "src/mapObjTest.h"
#include "src/mapObjDiffTest.h"
#include "src/mapOptTest.h"
#include "src/mapTildeTest.h"
#include "src/fitIndividualTest.h"
#include "src/indStatisticsTest.h"
#include "src/fitIndividualErrorTest.h"
#include "src/lambdaTest.h"
#include "src/lambda2diffTest.h"
#include "src/NaiveFoModelTest.h"
#include "src/estimateBTest.h"
#include "src/expectedHessianTest.h"
#include "src/lTildeTest.h"
#include "src/centdiffTest.h"
#include "src/EqIndModelTest.h"
#include "src/ppkaOptTest.h"
#include "src/firstOrderOptTest.h"
#include "src/fitPopulationTest.h"
#include "src/fitPopulationErrorTest.h"
#include "src/popStatisticsTest.h"
 
//#include "src/fitPopulationTest.h"

using namespace std;
using namespace CppUnit;

int main( int argc, const char * argv[] )
{
  CppUnit::TextUi::TestRunner runner;

  runner.addTest( SpkErrorTest::suite() );
  runner.addTest( SpkExceptionTest::suite() );
  runner.addTest( BlockAllocTest::suite() );
  runner.addTest( MatrixTest::suite() );
  runner.addTest( addTest::suite() );
  runner.addTest( subtractTest::suite() );
  runner.addTest( multiplyTest::suite() );
  runner.addTest( divByScalarTest::suite() );
  runner.addTest( mulByScalarTest::suite() );
  runner.addTest( allTrueTest::suite() );
  runner.addTest( elementwiseAndTest::suite() );
  runner.addTest( elementwiseOrTest::suite() );
  runner.addTest( identityTest::suite() );
  runner.addTest( countTruesTest::suite() );
  runner.addTest( isDblEpsEqualTest::suite() );
  runner.addTest( isGreaterThanOrEqualToTest::suite() );
  runner.addTest( isLessThanOrEqualToTest::suite() );
  runner.addTest( isSymmetricTest::suite() );
  runner.addTest( matabsTest::suite() );
  runner.addTest( matmaxTest::suite() );
  runner.addTest( matminTest::suite() );
  runner.addTest( getColTest::suite() );
  runner.addTest( getMulColsTest::suite() );
  runner.addTest( detTest::suite() );
  runner.addTest( inverseTest::suite() );
  runner.addTest( transposeTest::suite() );
  runner.addTest( symmetrizeTest::suite() );
  runner.addTest( rvecTest::suite() );
  runner.addTest( rvecInverseTest::suite() );
  runner.addTest( replaceJthTest::suite() );
  runner.addTest( replaceIthTest::suite() );
  runner.addTest( replaceSubblockTest::suite() );
  runner.addTest( transposeRowBlocksTest::suite() );
  runner.addTest( transposeDerivativeTest::suite() );
  runner.addTest( SpkModelErrorTest::suite() );   
  runner.addTest( placeRowsTest::suite() );
  //runner.addTest( printInMatrixTest::suite() );
  
  runner.addTest( AkronBtimesCTest::suite() );
  runner.addTest( AkronItimesCTest::suite() );
  runner.addTest( IkronBtimesCTest::suite() );
  runner.addTest( elsqTest::suite() );
  runner.addTest( elsq_xTest::suite() );
  runner.addTest( elsq_xDiagTest::suite() );
  runner.addTest( elsq_xBlockDiagTest::suite() );
  runner.addTest( centdiffTest::suite() );
  runner.addTest( choleskyTest::suite() );
  runner.addTest( CovarianceTest::suite() );
  runner.addTest( ABA_xTest::suite() );
  runner.addTest( blockDiagonalDerivativeTest::suite() );
  runner.addTest( centdiffModelTest::suite() );
  runner.addTest( IndResultsTest::suite() );
  runner.addTest( OptimizerTest::suite() );
  runner.addTest( UTranTimesSymKronSymTimesUTest::suite() );
  runner.addTest( UTranTimesSymKronSymTimesU_xTest::suite() );
  runner.addTest( FunctionTest::suite() );
  //FpErrorCheckerTest?
  runner.addTest( FullDataCovarianceTest::suite() );
  runner.addTest( FullIndParCovarianceTest::suite() );
  runner.addTest( sqpAnyBoxTest::suite() );
  runner.addTest( mapObjTest::suite() );
  runner.addTest( mapObjDiffTest::suite() );
  runner.addTest( mapOptTest::suite() );
  runner.addTest( mapTildeTest::suite() );
  runner.addTest( fitIndividualTest::suite() );
  runner.addTest( indStatisticsTest::suite() );
  runner.addTest( fitIndividualErrorTest::suite() );
  runner.addTest( lambdaTest::suite() );
  runner.addTest( lambda2diffTest::suite() );
  runner.addTest( NaiveFoModelTest::suite() );
  runner.addTest( estimateBTest::suite() );
  runner.addTest( expectedHessianTest::suite() );
  runner.addTest( lTildeTest::suite() );
  runner.addTest( centdiffTest::suite() );
  runner.addTest( ppkaOptTest::suite() );
  runner.addTest( EqIndModelTest::suite() );
  runner.addTest( firstOrderOptTest::suite() );
  runner.addTest( fitPopulationTest::suite() );
  runner.addTest( fitPopulationErrorTest::suite() );
  runner.addTest( popStatisticsTest::suite() );

  runner.run();

  return 0;
}
