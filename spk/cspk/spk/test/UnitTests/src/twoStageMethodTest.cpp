/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
/*************************************************************************
 *
 * File: twoStageMethodTest.cpp
 *
 *
 * Unit test for the function twoStageMethod.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>
#include <string>
#include <cmath>
#include <cstdlib>
#include <cassert>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/add.h"
#include "../../../spk/divByScalar.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/twoStageMethod.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/getCol.h"
#include "../../../spk/identity.h"
#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/mapOpt.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/multiply.h"
#include "../../../spk/pi.h"
#include "../../../spk/replaceJth.h"
#include "../../../spk/subtract.h"
#include "../../../spk/transpose.h"
#include "../../../spk/inverse.h"

#include "twoStageMethodTest.h"

using namespace std;
using namespace CppUnit;


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

  //**********************************************************************
  //
  // Class:  RailExampleModel
  //
  //
  // This class is an SpkModel subclasse with model functions that
  // correspond to the Rail Example that is included in the NLME
  // distribution.
  //
  // The PRED block for the Rail Example after it has been converted
  // to individual notation for this two-stage method test is
  //
  //     $PRED 
  //     F = THETA(1)
  //     Y = F + ETA(1)
  //
  //**********************************************************************

  class RailExampleModel : public SpkModel
  {
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
  public:
    RailExampleModel(int nA, int nB, int nYi)
      : _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {}; 
    ~RailExampleModel(){};
  private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& aval)
    {
      assert(aval.size() == _nA);
        _a = aval;
    }
    void doSetIndPar(const  valarray<double>& bval)
    {
      assert(bval.size() == _nB);
        _b = bval;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        //
        //     D(alp) = I  .
        //
        ret.resize(_nB * _nB);
        identity( _nB, ret );
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        //    D_alp(alp) = 0  .
        //
        ret.resize(_nB * _nB * _nA);
        ret = 0.0;
        return false;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        //
        //                 / b(1) \ 
        //     f(alp, b) = | b(1) |  ,
        //                 \ b(1) /
        //
        // where
        //                          
        //         / theta(1) \
        //     b = |          |  .
        //         \ omegaPar /
        //                          
        ret.resize(_nYi);
        ret = _b[1];
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        //
        //     f_alp(alp, b) = 0  .
        //
        ret.resize(_nYi * _nA);
        ret = 0.0;
        return false;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        //
        //                   / 1   0 \ 
        //     f_b(alp, b) = | 1   0 |  .
        //                   \ 1   0 /
        //
        ret.resize(_nYi * _nB);
        ret = 0.0;
        ret[0] = 1.0;
        ret[1] = 1.0;
        ret[2] = 1.0;
        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        //
        //                 / exp[2 b(2)]      0           0      \ 
        //     R(alp, b) = |      0      exp[2 b(2)]      0      |  ,
        //                 \      0           0      exp[2 b(2)] / 
        //
        // where
        //                          
        //         / theta(1) \
        //     b = |          |  .
        //         \ omegaPar /
        //                          
        ret.resize(_nYi * _nYi);
        ret = 0.0;
        ret[0] = exp( 2.0 * _b[1] );
        ret[4] = exp( 2.0 * _b[1] );
        ret[8] = exp( 2.0 * _b[1] );
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        //
        //    R_alp(alp, b) = 0  .
        //
        ret.resize(_nYi * _nYi * _nA);
        ret = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        //
        //                   /  0      2 exp[2 b(2)] \ 
        //                   |  0             0      | 
        //                   |  0             0      | 
        //                   |  0             0      | 
        //     R_b(alp, b) = |  0      2 exp[2 b(2)] |   .
        //                   |  0             0      | 
        //                   |  0             0      | 
        //                   |  0             0      | 
        //                   \  0      2 exp[2 b(2)] / 
        //
        ret.resize(_nYi * _nYi * _nB);
        ret = 0.0;
        ret[0 + _nYi * _nYi] = 2.0 * exp( 2.0 * _b[1] );
        ret[4 + _nYi * _nYi] = 2.0 * exp( 2.0 * _b[1] );
        ret[8 + _nYi * _nYi] = 2.0 * exp( 2.0 * _b[1] );
        return true;
    }   
  };


} // [End: unnamed namespace]


/*************************************************************************
 *
 * CppUnit framework functions.
 *
 *************************************************************************/

void twoStageMethodTest::setUp()
{
    // initializations
}
void twoStageMethodTest::tearDown()
{
    // clean up
}

Test* twoStageMethodTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite("twoStageMethodTest");

    suiteOfTests->addTest(new TestCaller<twoStageMethodTest>(
      "railExampleTest", &twoStageMethodTest::railExampleTest));

    return suiteOfTests;
}


/*************************************************************************
 *
 * Function: railExampleTest
 *
 *
 * This test uses SpkModel subclasses with model functions that
 * correspond to the Rail Example that is included in the NLME
 * distribution.
 *
 * The PRED block for the Rail Example after it has been converted
 * to individual notation for this two-stage method test is
 *
 *     $PRED 
 *     F = THETA(1)
 *     Y = F + ETA(1)
 *
 *************************************************************************/

void twoStageMethodTest::railExampleTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  int j;


  //------------------------------------------------------------
  // Quantities related to the population.
  //------------------------------------------------------------

  // Set the number of individuals.
  const int nInd = 6;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Set the number of data values per individual.
  const int nY_i = 3;

  // Set the number of data values for all of the individuals. 
  DoubleMatrix dvecN( nInd, 1 );
  dvecN.fill( (double) nY_i );

  // Set the data values for all of the individuals.
  DoubleMatrix dvecY( nInd * nY_i, 1 );
  double* pdYData = dvecY.data();
  pdYData[ 0] = 5.5000E+01;
  pdYData[ 1] = 5.3000E+01;
  pdYData[ 2] = 5.4000E+01;
  pdYData[ 3] = 2.6000E+01;
  pdYData[ 4] = 3.7000E+01;
  pdYData[ 5] = 3.2000E+01;
  pdYData[ 6] = 7.8000E+01;
  pdYData[ 7] = 9.1000E+01;
  pdYData[ 8] = 8.5000E+01;
  pdYData[ 9] = 9.2000E+01;
  pdYData[10] = 1.0000E+02;
  pdYData[11] = 9.6000E+01;
  pdYData[12] = 4.9000E+01;
  pdYData[13] = 5.1000E+01;
  pdYData[14] = 5.0000E+01;
  pdYData[15] = 8.0000E+01;
  pdYData[16] = 8.5000E+01;
  pdYData[17] = 8.3000E+01;


  //------------------------------------------------------------
  // Quantities related to the population parameter, alp.
  //------------------------------------------------------------

  // There are no population parameters for this test.
  const int nAlp = 0;


  //------------------------------------------------------------
  // Prepare the individual model variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of individual model independent variables.
  const int nTheta = 1;
  const int nEta   = 1;

  // Set the initial value for theta.
  valarray<double> theta( nTheta );
  theta[0] = 72.0;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = 7.2;
  thetaUp[0]  = 720.0;

  // Set the number elements for the parameterization of omega, the
  // covariance matrix for eta.
  int nOmegaPar = 1;

  // Set the initial minimal representation.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 32.0;

  // Set the initial omega parameters.
  valarray<double> omegaPar( nOmegaPar );
  omegaPar[0] = std::log( omegaMinRep[0] ) / 2.0;

  // Set the limits for omega.
  valarray<double> omegaParLow( nOmegaPar );
  valarray<double> omegaParUp ( nOmegaPar );
  omegaParLow[0] = omegaMinRep[0] / 100.0;
  omegaParUp[0]  = omegaMinRep[0] * 100.0;


  //------------------------------------------------------------
  // Quantities related to the individual parameters, b.
  //------------------------------------------------------------

  // Set the number of individual parameters.
  const int nB = nTheta + nOmegaPar;

  DoubleMatrix dvecBLow ( nB, 1 );
  DoubleMatrix dvecBUp  ( nB, 1 );
  DoubleMatrix dvecBStep( nB, 1 );
  DoubleMatrix dmatBIn  ( nB, nInd );

  double* pdBLowData  = dvecBLow .data();
  double* pdBUpData   = dvecBUp  .data();
  double* pdBStepData = dvecBStep.data();
  double* pdBInData   = dmatBIn  .data();

  // Set the initial values for the individual parameters for all of
  // the individuals.
  for ( i = 0; i < nInd; i++ )
  {
    pdBInData[0 + i * nB] = theta[0];
    pdBInData[1 + i * nB] = omegaPar[0];
  }

  // Set the limits for the individual parameters.
  pdBLowData[0] = thetaLow[0];
  pdBUpData [0] = thetaUp[0];
  pdBLowData[1] = omegaParLow[0];
  pdBUpData [1] = omegaParUp[0];

  // Set the step sizes for the individual parameters.
  pdBStepData[0] = ( thetaUp[0] - thetaLow[0] ) / 1000.0;
  pdBStepData[1] = ( omegaParUp[0] - omegaParLow[0] ) / 1000.0;


  //------------------------------------------------------------
  // Construct the SPK model.
  //------------------------------------------------------------

  // Construct the individual level model that will be applied to all
  // of the individuals' data sets.
  RailExampleModel model( nAlp, nB, nY_i );


  //------------------------------------------------------------
  // Prepare the output variables for the two-stage method.
  //------------------------------------------------------------

  DoubleMatrix dmatBOut    ( nB, nB );
  DoubleMatrix dvecBMeanOut( nB, 1 );
  DoubleMatrix dmatBCovOut ( nB, nB );


  //------------------------------------------------------------
  // Remaining inputs to twoStageMethod.
  //------------------------------------------------------------

  // Choose the two-stage method to use.
  enum Objective method = STANDARD_TWO_STAGE;

  // Set the flag that indiciates if the Map Bayesian terms should be
  // included in the individual objective function MapObj(b).
  bool withD;
  if( method == STANDARD_TWO_STAGE  ||
      method == ITERATIVE_TWO_STAGE ||
      method == GLOBAL_TWO_STAGE )
  {
    withD = false;
  }
  else
  {
    withD = true;
  }

  // Set the values for optimization of the individual objective
  // functions.
  double indEpsilon = 1.e-3; 
  int indNMaxIter   = 50; 
  int indLevel      = 0;
  Optimizer indOptInfo( indEpsilon, indNMaxIter, indLevel ); 

  // Set the values for optimization of the population objective
  // function.
  double popEpsilon = 1.e-3; 
  int popNMaxIter   = 50; 
  int popLevel      = 0;
  Optimizer popOptInfo( popEpsilon, popNMaxIter, popLevel ); 


  //------------------------------------------------------------
  // Perform the two-stage method.
  //------------------------------------------------------------

  try
  {
    twoStageMethod( model,
                    method,
                    dvecN,
                    dvecY,
                    popOptInfo,
                    indOptInfo,
                    dvecBLow,
                    dvecBUp,
                    dmatBIn,
                    &dmatBOut,
                    dvecBStep,
                    &dvecBMeanOut,
                    &dmatBCovOut );
  }
  catch( const SpkException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "twoStageMethod failed!", false );
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE( "twoStageMethod failed for unknown reasons!", false);
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  DoubleMatrix dvecBKnown_i           ( nB, 1 );
  DoubleMatrix dmatBKnown             ( nB, nInd );
  DoubleMatrix dvecBKnown_iSum        ( nB, 1 );
  DoubleMatrix dvecBMeanKnown         ( nB, 1 );
  DoubleMatrix dvecBDiff_i            ( nB, 1 );
  DoubleMatrix dvecBDiff_iTrans       ( nB, 1 );
  DoubleMatrix dmatBDiff_iCrossProd   ( nB, nB );
  DoubleMatrix dmatBDiff_iCrossProdSum( nB, nB );
  DoubleMatrix dmatBCovKnown          ( nB, nB );

  DoubleMatrix dvecY_i  ( nY_i, 1 );
  DoubleMatrix dvecBIn_i( nB,   1 );

  double* pdvecY_iData = dvecY_i.data();
  double* pdvecYData   = dvecY.data();

  // Initially set these all equal to zero.
  dvecBKnown_iSum        .fill( 0.0 );
  dmatBDiff_iCrossProdSum.fill( 0.0 );

  double* pdNull = 0;
  DoubleMatrix* pDmatNull = 0;

  // Calculate the known values for the means for the individuals'
  // parameters,
  //
  //                       nInd
  //                       ----
  //                 1     \    
  //     bMean  =  ------  /      b   .
  //                nInd   ----    i
  //                       i = 1 
  //
  for ( i = 0; i < nInd; i++ )
  {
    // Get this individual's data values.
    pdvecY_iData[0] = pdvecYData[0 + i * nY_i];
    pdvecY_iData[1] = pdvecYData[1 + i * nY_i];
    pdvecY_iData[2] = pdvecYData[2 + i * nY_i];
    
    // Get this individual's initial parameter value.
    dvecBIn_i = getCol( dmatBIn, i );

    // Calculate this individual's optimal parameter value.
    mapOpt(
      model,
      dvecY_i,
      indOptInfo,
      dvecBLow,
      dvecBUp,
      dvecBIn_i,
      &dvecBKnown_i,
      dvecBStep,
      pdNull,
      pDmatNull,
      pDmatNull,
      withD );

    // Set this individual's optimal parameter value in the matrix.
    replaceJth( dmatBKnown, i, dvecBKnown_i );

    // Add in this individual's parameter value.
    dvecBKnown_iSum = add( dvecBKnown_iSum, dvecBKnown_i );

  }
  divByScalar( dvecBKnown_iSum, nInd, dvecBMeanKnown );

  // Calculate the known value for the covariance of the individuals'
  // parameters,
  //
  //                       nInd
  //                       ----
  //                 1     \                                     T
  //     bCov   =  ------  /      ( b  -  bMean ) ( b  -  bMean )   .
  //                nInd   ----      i               i
  //                       i = 1 
  //
  for ( i = 0; i < nInd; i++ )
  {
    // Get this individual's optimal parameter value.
    dvecBKnown_i = getCol( dmatBKnown, i );

    // Calculate the difference of between this individual's parameter
    // value and the mean value.
    subtract( dvecBKnown_i, dvecBMeanKnown, dvecBDiff_i );

    // Calculate this individual's cross-product.
    dvecBDiff_iTrans     = transpose( dvecBDiff_i );
    dmatBDiff_iCrossProd = multiply( dvecBDiff_i, dvecBDiff_iTrans );

    // Add in this individual's cross-product value.
    dmatBDiff_iCrossProdSum = add( dmatBDiff_iCrossProdSum, dmatBDiff_iCrossProd );
  }
  divByScalar( dmatBDiff_iCrossProdSum, nInd, dmatBCovKnown );


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest( dmatBOut,
             dmatBKnown,
             dvecBMeanOut,
             dvecBMeanKnown,
             dmatBCovOut,
             dmatBCovKnown );
  
}


/*************************************************************************
 *
 * Function: doTheTest
 *
 *************************************************************************/

void twoStageMethodTest::doTheTest(
  const DoubleMatrix& dmatBOut,
  const DoubleMatrix& dmatBKnown,
  const DoubleMatrix& dvecBMeanOut,
  const DoubleMatrix& dvecBMeanKnown,
  const DoubleMatrix& dmatBCovOut,
  const DoubleMatrix& dmatBCovKnown )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Check the values.
  //------------------------------------------------------------

  CPPUNIT_ASSERT( isDmatEpsEqual( dmatBKnown, dmatBOut, dmatBKnown ) );
  
  CPPUNIT_ASSERT( isDmatEpsEqual( dvecBMeanKnown, dvecBMeanOut, dvecBMeanKnown ) );
  
  CPPUNIT_ASSERT( isDmatEpsEqual( dmatBCovKnown, dmatBCovOut, dmatBCovKnown ) );
  
}


