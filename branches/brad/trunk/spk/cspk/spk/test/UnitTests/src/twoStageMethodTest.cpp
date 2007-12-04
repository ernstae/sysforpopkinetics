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
#include "../../../spk/matabs.h"
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

  class RailExampleModel : public SpkModel<double>
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
        ret = _b[0];
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
      "railExampleSTSTest", &twoStageMethodTest::railExampleSTSTest));

    suiteOfTests->addTest(new TestCaller<twoStageMethodTest>(
      "railExampleITSTest", &twoStageMethodTest::railExampleITSTest));

    suiteOfTests->addTest(new TestCaller<twoStageMethodTest>(
      "railExampleGTSTest", &twoStageMethodTest::railExampleGTSTest));

    return suiteOfTests;
}


/*************************************************************************
 *
 * Function: railExampleSTSTest
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

void twoStageMethodTest::railExampleSTSTest()
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
  omegaParLow[0] = std::log( omegaMinRep[0] / 100.0 ) / 2.0;
  omegaParUp[0]  = std::log( omegaMinRep[0] * 100.0 ) / 2.0;


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

  doTheTest( popEpsilon,
             dvecBLow,
             dvecBUp,
             dmatBOut,
             dmatBKnown,
             dvecBMeanOut,
             dvecBMeanKnown,
             dmatBCovOut,
             dmatBCovKnown );
  
}


/*************************************************************************
 *
 * Function: railExampleITSTest
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

void twoStageMethodTest::railExampleITSTest()
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
  //
  // Note that the data values for the first and fifth individuals
  // were modified from their original values.
  DoubleMatrix dvecY( nInd * nY_i, 1 );
  double* pdYData = dvecY.data();
  pdYData[ 0] = 4.0000E+01;
  pdYData[ 1] = 5.1000E+01;
  pdYData[ 2] = 4.6000E+01;
  pdYData[ 3] = 2.6000E+01;
  pdYData[ 4] = 3.7000E+01;
  pdYData[ 5] = 3.2000E+01;
  pdYData[ 6] = 7.8000E+01;
  pdYData[ 7] = 9.1000E+01;
  pdYData[ 8] = 8.5000E+01;
  pdYData[ 9] = 9.2000E+01;
  pdYData[10] = 1.0000E+02;
  pdYData[11] = 9.6000E+01;
  pdYData[12] = 7.0000E+01;
  pdYData[13] = 7.5000E+01;
  pdYData[14] = 7.3000E+01;
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
  omegaParLow[0] = std::log( omegaMinRep[0] / 100.0 ) / 2.0;
  omegaParUp[0]  = std::log( omegaMinRep[0] * 100.0 ) / 2.0;


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
  enum Objective method = ITERATIVE_TWO_STAGE;

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
  double indEpsilon = 1.e-6; 
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

  DoubleMatrix dmatBKnown    ( nB, nInd );
  DoubleMatrix dvecBMeanKnown( nB, 1 );
  DoubleMatrix dmatBCovKnown ( nB, nB );

  double* pdBKnownData     = dmatBKnown    .data();
  double* pdBMeanKnownData = dvecBMeanKnown.data();
  double* pdBCovKnownData  = dmatBCovKnown .data();

  // Set the known values.
  //
  // Note that the following values were calculated using R.
  //     
  //     [1] "DD"  (bCov)
  //
  //               [,1]        [,2]
  //     [1,] 512.855962 -2.48689687
  //     [2,]  -2.486897  0.03563247
  //     
  //     theta_i    (b_i_1): 45.90834 32.12237 84.45908 95.81735 72.67466 82.60276
  //     omegaPar_i (b_i_2): 1.421344 1.479556 1.299462 1.168821 1.233321 1.187630
  //     
  //     [1] "omegaParMean =  1.29835582063139"  (bMean_2)
  //     
  //     [1] "THETA1 =  68.930759775187"         (bMean_1)
  //     [1] "OMEGA11 =  508.307879473506"
  //     [1] "SIGMA =  13.7975614035972"
  //
  pdBKnownData[0 + 0 * nB] = 45.90834;
  pdBKnownData[0 + 1 * nB] = 32.12237;
  pdBKnownData[0 + 2 * nB] = 84.45908;
  pdBKnownData[0 + 3 * nB] = 95.81735;
  pdBKnownData[0 + 4 * nB] = 72.67466;
  pdBKnownData[0 + 5 * nB] = 82.60276;
  pdBKnownData[1 + 0 * nB] = 1.421344;
  pdBKnownData[1 + 1 * nB] = 1.479556;
  pdBKnownData[1 + 2 * nB] = 1.299462;
  pdBKnownData[1 + 3 * nB] = 1.168821;
  pdBKnownData[1 + 4 * nB] = 1.233321;
  pdBKnownData[1 + 5 * nB] = 1.187630;

  pdBMeanKnownData[0] = 68.930759775187;
  pdBMeanKnownData[1] =  1.298355820631;

  pdBCovKnownData[0 + 0 * nB] = 512.855962;
  pdBCovKnownData[0 + 1 * nB] =  -2.486897;
  pdBCovKnownData[1 + 0 * nB] =  -2.48689687;
  pdBCovKnownData[1 + 1 * nB] =   0.03563247;


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest( popEpsilon,
             dvecBLow,
             dvecBUp,
             dmatBOut,
             dmatBKnown,
             dvecBMeanOut,
             dvecBMeanKnown,
             dmatBCovOut,
             dmatBCovKnown );
  
}



/*************************************************************************
 *
 * Function: railExampleGTSTest
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

void twoStageMethodTest::railExampleGTSTest()
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
  omegaParLow[0] = std::log( omegaMinRep[0] / 100.0 ) / 2.0;
  omegaParUp[0]  = std::log( omegaMinRep[0] * 100.0 ) / 2.0;


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
  enum Objective method = GLOBAL_TWO_STAGE;

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
  double indEpsilon = 1.e-6; 
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

  DoubleMatrix dmatBKnown    ( nB, nInd );
  DoubleMatrix dvecBMeanKnown( nB, 1 );
  DoubleMatrix dmatBCovKnown ( nB, nB );

  double* pdBKnownData     = dmatBKnown    .data();
  double* pdBMeanKnownData = dvecBMeanKnown.data();
  double* pdBCovKnownData  = dmatBCovKnown .data();

  // Set the known values.
  //
  // Note that the following values were calculated using R.
  //     
  //     [1] D ( bCov )
  //
  //               [,1]      [,2]
  //     [1,] 510.56611 5.0945202
  //     [2,]   5.09452 0.4072278
  //
  //     [1] mu ( bMean )
  //
  //                [,1]
  //     [1,] 66.5239521
  //     [2,]  0.7786256
  //
  //          theta_i      omegaPar_i  omega_i
  //
  //          (b    )       (b    )
  //            i(1)          i(2)
  //
  //     [1] 54.00181991  0.07015188   1.15062327
  //     [1] 32.256252    1.163502    10.247198
  //     [1] 84.463966    1.443051    17.923306
  //     [1] 95.803746    1.147627     9.926954
  //     [1] 50.00372897  0.05744019   1.12173925
  //     [1] 82.6164265   0.7899683    4.8546478
  //
  //         thetaMean    omegaParMean  omegaMean
  //
  //     [1] 66.5243233   0.7786234    7.5374114
  //
  pdBKnownData[0 + 0 * nB] = 54.00181991;
  pdBKnownData[0 + 1 * nB] = 32.256252;
  pdBKnownData[0 + 2 * nB] = 84.463966;
  pdBKnownData[0 + 3 * nB] = 95.803746;
  pdBKnownData[0 + 4 * nB] = 50.00372897;
  pdBKnownData[0 + 5 * nB] = 82.6164265;
  pdBKnownData[1 + 0 * nB] = 0.07015188;
  pdBKnownData[1 + 1 * nB] = 1.163502;
  pdBKnownData[1 + 2 * nB] = 1.443051;
  pdBKnownData[1 + 3 * nB] = 1.147627;
  pdBKnownData[1 + 4 * nB] = 0.05744019;
  pdBKnownData[1 + 5 * nB] = 0.7899683;

  pdBMeanKnownData[0] = 66.5239521;
  pdBMeanKnownData[1] =  0.7786256;

  pdBCovKnownData[0 + 0 * nB] = 510.56611;
  pdBCovKnownData[0 + 1 * nB] = 5.0945202;
  pdBCovKnownData[1 + 0 * nB] = 5.0945202;
  pdBCovKnownData[1 + 1 * nB] = 0.4072278;


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest( popEpsilon,
             dvecBLow,
             dvecBUp,
             dmatBOut,
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
  double              epsilon,
  const DoubleMatrix& dvecBLow,
  const DoubleMatrix& dvecBUp,
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

  const double* pdBLowData = dvecBLow.data();
  const double* pdBUpData  = dvecBUp.data();

  const double* pdBOutData     = dmatBOut    .data();
  const double* pdBMeanOutData = dvecBMeanOut.data();
  const double* pdBCovOutData  = dmatBCovOut .data();

  const double* pdBKnownData     = dmatBKnown    .data();
  const double* pdBMeanKnownData = dvecBMeanKnown.data();
  const double* pdBCovKnownData  = dmatBCovKnown .data();

  int nB   = dmatBOut.nr();
  int nInd = dmatBOut.nc();

  bool isConverged;

  int i;
  int j;
  int k;


  //------------------------------------------------------------
  // Check the individual parameter values.
  //------------------------------------------------------------

  // For each pair of vectors bOut_i and bKnown_i, i.e., for each column 
  // of bOut and bKnown, check to see that the convergence criteria,
  // 
  //      abs(bOut  - bKnown ) <= epsilon * (bUp - bLow)  ,
  //              i         i
  //
  // is satisfied.
  isConverged = true;
  for ( i = 0; i < nInd; i++ )
  {
    for ( j = 0; j < nB; j++ )
    {
      if ( fabs(pdBOutData[ j + i * nB ] - pdBKnownData[ j + i * nB  ]) > 
        epsilon * (pdBUpData[ j ] - pdBLowData[ j ]) )
      {
        isConverged = false;
      }
    }
  }
  
  CPPUNIT_ASSERT( isConverged);


  //------------------------------------------------------------
  // Check the mean individual parameter values.
  //------------------------------------------------------------

  // Check to see if any elements of bMeanOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(bMeanOut - bMeanKnown) <= epsilon * (bUp - bLow)  .
  //
  isConverged = true;
  for ( i = 0; i < nB; i++ )
  {
    if ( fabs(pdBMeanOutData[ i ] - pdBMeanKnownData[ i ]) > 
            epsilon * (pdBUpData[ i ] - pdBLowData[ i ]) )
    {
      isConverged = false;
    }
  }

  CPPUNIT_ASSERT( isConverged );


  //------------------------------------------------------------
  // Check the individual parameter covariance values.
  //------------------------------------------------------------

  // For each pair of vectors bCovOut_j_k and bCovKnown_j_k, i.e., for each column 
  // of bOut and bKnown, check to see that the convergence criteria,
  // 
  //                                                                                                 1/2
  //      abs(bCovOut      - bCovKnown     ) <= epsilon * [ (bUp    - bLow   ) * (bUp    - bLow   ) ]    ,
  //                 (j,k)            (j,k)                     (j)       (j)        (k)       (k)
  //
  // is satisfied.
  isConverged = true;
  for ( j = 0; j < nB; j++ )
  {
    for ( k = 0; k < nB; k++ )
    {
      if ( fabs(pdBCovOutData[ j + k * nB ] - pdBCovKnownData[ j + k * nB  ]) > 
	   epsilon * sqrt( (pdBUpData[ j ] - pdBLowData[ j ]) * (pdBUpData[ k ] - pdBLowData[ k ]) ) )
      {
        isConverged = false;
      }
    }
  }
  
  CPPUNIT_ASSERT( isConverged);
}


