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
 * File: predNonparamMethodTest.cpp
 *
 *
 * Unit test for the function predNonparamMethod.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "predNonparamMethodTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include "../../../spkpred/DiagCov.h"
#include "../../../spkpred/FullCov.h"
#include "../../../spkpred/IndPredModel.h"
#include "../../../spkpred/IndPredModelBase.h"
#include "../../../spkpred/PopPredModel.h"
#include "../../../spkpred/predNonparamMethod.h"

// SPK library header files.
#include <spk/DoubleMatrix.h>
#include <spk/fitIndividual.h>
#include <spk/multiply.h>
#include <spk/SpkValarray.h>
#include <spk/transpose.h>

// CppAD header files.
#include <CppAD/CppAD.h>

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <string>
#include <cmath>

using namespace CppAD;
using namespace CppUnit;
using SPK_VA::valarray;


/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

void p_y_given_b(
        IndPredModel             &model   ,
        DoubleMatrix             &y       ,
        DoubleMatrix             &N       ,
        DoubleMatrix             &B       ,
        DoubleMatrix             &pout    )
{       typedef std::valarray<double> vector;

        size_t n  = B.nr();
        size_t J  = B.nc();
        size_t M  = N.nr();
        assert( pout.nr() == M );
        assert( pout.nc() == J );
        double pi = 4. * std::atan(1.);

        size_t i, j, k, ell;
        double residual, variance;

        double *p_ptr = pout.data();
        double *B_ptr = B.data();

        for(j = 0; j < J; j++)
        {       // move data from B for this discrete measurement point
                vector b(n);
                for(k = 0; k < n; k++)
                        b[k] = B_ptr[k + j * n];

                double *y_ptr = y.data();
                for(i = 0; i < M; i++)
                {       model.selectIndividual(i);
                        model.setIndPar(b);
                        size_t Ni = size_t( *(N.data() + i) );
                        vector f(Ni);
                        model.dataMean(f);
                        vector R(Ni * Ni);
                        model.dataVariance(R);
                        double sum = 0.;
                        for(k = 0; k < Ni; k++)
                        {       for(ell = 0; ell < Ni; ell++)
                                if( k != ell )
                                        assert( R[ k * Ni + ell] == 0. );
                                residual = (*y_ptr++) - f[k];
                                variance = R[ k * Ni + k ];
                                sum     += residual * residual / variance;
                                sum     += std::log( 2 * pi * variance); 
                        }
                        p_ptr[ i + j * M ] = std::exp( - sum / 2. );
                }
        }
}

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Local variable declarations
 *------------------------------------------------------------------------*/

namespace railexample_prednonparammethodtest
{
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
  valarray<int> N( nY_i, nInd );

  // Create a C style array with the data values for all of the
  // individuals.
  double YCArray[nInd * nY_i] = {
      5.5000E+01,
      5.3000E+01,
      5.4000E+01,
      2.6000E+01,
      3.7000E+01,
      3.2000E+01,
      7.8000E+01,
      9.1000E+01,
      8.5000E+01,
      9.2000E+01,
      1.0000E+02,
      9.6000E+01,
      4.9000E+01,
      5.1000E+01,
      5.0000E+01,
      8.0000E+01,
      8.5000E+01,
      8.3000E+01 };

  // Set the data values for all of the individuals.
  valarray<double> Y( YCArray, nY_i * nInd  );

  // This will contain the data values for a particular individual.
  valarray<double> Y_i( nY_i );
}


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

  //**********************************************************************
  //
  // Class:  RailExample_Pred
  //
  //
  // This class evaluates Pred block expressions that correspond to
  // the Rail Example that is included in the NLME distribution.
  //
  // The PRED block for the Rail Example after it has been converted
  // to individual notation for this nonparametric method test is
  //
  //     $PRED 
  //     F = THETA(1)
  //     Y = F + ETA(1)
  //
  //**********************************************************************

  template<class Value>
  class RailExample_Pred : public PredBase<Value>
  {
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    RailExample_Pred( int nY_iIn )
    :
    nY_i ( nY_iIn )
    {}

    ~RailExample_Pred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;


    //**********************************************************
    // 
    // Function: eval
    //
    //**********************************************************

    bool eval(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int epsOffset,   int epsLen,
      int fOffset,     int fLen,
      int yOffset,     int yLen,
      int i,
      int j,
      const std::vector<Value>& indepVar,
      std::vector<Value>& depVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace railexample_prednonparammethodtest;


      //--------------------------------------------------------
      // Evaluate the mean of the data and its predicted value.
      //--------------------------------------------------------

      // Calculate
      //           
      //    f  =  theta(0)  ,
      //
      // and
      //
      //    y  =  f + eta(0)  .
      //
      depVar[fOffset + j] = indepVar[thetaOffset + 0];
      depVar[yOffset + j] = depVar[fOffset + j] + indepVar[etaOffset + 0]; 


      //--------------------------------------------------------
      // Finish up.
      //--------------------------------------------------------

      // Return true to indicate that this is not a Missing Data 
      // Variable (MDV).
      return true;
    }


    //**********************************************************
    // 
    // Function: getNObservs
    //
    //**********************************************************

    int getNObservs( int i ) const
    {
      return nY_i;
    }


    //------------------------------------------------------------
    // Disallowed, implicitly generated member functions.
    //------------------------------------------------------------

  protected:
    RailExample_Pred(){}
    RailExample_Pred( const RailExample_Pred& ){}
    RailExample_Pred & operator=( const RailExample_Pred& ){}
  };


} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void predNonparamMethodTest::setUp()
{
    // initializations
}


/*************************************************************************
 *
 * Function: tearDown
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void predNonparamMethodTest::tearDown()
{
    // clean up
}


/*************************************************************************
 *
 * Function: suite
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

Test* predNonparamMethodTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite("predNonparamMethodTest");

    suiteOfTests->addTest(new TestCaller<predNonparamMethodTest>(
      "RailExample_Test", &predNonparamMethodTest::RailExample_Test));

    return suiteOfTests;
}


/*************************************************************************
 *
 * Function: RailExample_Test
 *
 *
 * This test uses a Pred block expression evaluator that corresponds
 * to the Rail Example that is included in the NLME distribution.
 *
 * The PRED block for the Rail Example after it has been converted
 * to individual notation for this nonparametric method test is
 *
 *     $PRED 
 *     F = THETA(1)
 *     Y = F + ETA(1)
 *
 *************************************************************************/

void predNonparamMethodTest::RailExample_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace railexample_prednonparammethodtest;

  int i;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
  //------------------------------------------------------------

  RailExample_Pred< double > predEvaluator( nY_i );

  RailExample_Pred< AD<double> > predEvaluatorAD( nY_i );

  RailExample_Pred< AD< AD<double> > > predEvaluatorADAD( nY_i );

  RailExample_Pred< AD< AD< AD<double> > > > predEvaluatorADADAD( nY_i );


  //------------------------------------------------------------
  // Prepare the population model variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of population model independent variables.
  //
  // Note that for the population models used with the nonparametric
  // methods, the number of theta and eta values must be equal.
  const int nThetaPop = 1;
  const int nEtaPop   = nThetaPop;
  const int nEpsPop   = 1;

  // Set the initial value for theta.
  valarray<double> thetaPopIn( nThetaPop );
  thetaPopIn[0] = 72.0;

  // Set the limits for theta.
  valarray<double> thetaPopLow( nThetaPop );
  valarray<double> thetaPopUp ( nThetaPop );
  thetaPopLow[0] = 20.0;
  thetaPopUp[0]  = 115.0;

  // Set the initial value for eta equal to all zeros.
  valarray<double> etaPopIn( 0.0, nEtaPop );


  //------------------------------------------------------------
  // Initialize quantities related to the population covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  //
  // Note that for the population models used with the nonparametric
  // methods, the Omega matrix must be full.
  covStruct omegaPopStruct = FULL;

  // Construct omega.
  FullCov omegaPop( nEtaPop );

  // Set the number elements for this parameterization.
  int nOmegaPopPar = omegaPop.getNPar();

  // Set the initial lower triangle elements for the initial value for
  // omega equal to those for an identity matrix.
  valarray<double> omegaPopMinRep( nOmegaPopPar );
  omegaPopMinRep[0] = 1.0;
  assert( nOmegaPopPar == 1 );

  // Set the structure of sigma, the covariance matrix for eps.
  covStruct sigmaPopStruct = DIAGONAL;

  // Construct sigma.
  DiagCov sigmaPop( nEpsPop );

  // Set the number elements for this parameterization.
  int nSigmaPopPar = sigmaPop.getNPar();

  // Set the diagonal elements for the initial value for Sigma.
  valarray<double> sigmaPopMinRep( nSigmaPopPar );
  sigmaPopMinRep[0] = 12.0;
  assert( nSigmaPopPar == 1 );


  //------------------------------------------------------------
  // Prepare the population level Pred model.
  //------------------------------------------------------------

  // Construct the population level Pred model.
  PopPredModel popModel(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nThetaPop,
    thetaPopLow,
    thetaPopUp,
    thetaPopIn,
    nEtaPop,
    etaPopIn,
    nEpsPop,
    omegaPopStruct,
    omegaPopMinRep,
    sigmaPopStruct,
    sigmaPopMinRep );


  //------------------------------------------------------------
  // Prepare the individual model variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of individual model independent variables.
  //
  // The Pred library specific nonparametric method wrapper determines
  // population means and covariances for individual parameters.
  //
  // It does that by performing nonparametric method analysis' using this
  // individual model, which should contain the same data and model as
  // the population model.
  //
  // Therefore, the following must be true:
  //
  //     nThetaPop       =  nThetaInd  ,
  //
  //     nEtaPop         =  nThetaInd  ,
  //
  //     nEpsPop         =  nEtaInd  ,
  //
  //     SigmaStructPop  =  OmegaStructInd  ,
  //
  //     OmegaPop        =  nEtaPop by nEtaPop ,
  //
  //     SigmaPop        =  nEpsPop by nEpsPop ,
  //
  //     OmegaInd        =  nEtaInd by nEtaInd  .
  //
  const int nThetaInd = nThetaPop;
  const int nEtaInd   = nEpsPop;


  //------------------------------------------------------------
  // Initialize quantities related to the individual covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  //
  // Note that for the individual models used with the nonparametric
  // methods, the Omega matrix must have the same structure as the
  // population Sigma matrix.
  covStruct omegaIndStruct = sigmaPopStruct;

  // Set the number elements for this parameterization.
  int nOmegaIndPar = nSigmaPopPar;

  // Set the initial minimal representation.
  valarray<double> omegaIndMinRep( nOmegaIndPar );
  omegaIndMinRep = sigmaPopMinRep;


  //------------------------------------------------------------
  // Construct the individual level Pred models with the population data.
  //------------------------------------------------------------

  // Construct the double version of the individual model that will be
  // applied to the same sets of individuals' data sets as for the
  // population model.
  //
  // Note that the population model's eps and Sigma information is
  // used for the individual model's eta and Omega information.
  IndPredModel indModelWithPopData(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nThetaPop,
    thetaPopLow,
    thetaPopUp,
    thetaPopIn,
    nEtaInd,
    omegaIndStruct,
    omegaIndMinRep );

  // Construct the AD< double > version of the individual model
  // that will be applied to the same sets of individuals' data sets
  // as for the population model.
  //
  // Note that the population model's eps and Sigma information is
  // used for the individual model's eta and Omega information.
  IndPredModelBase< AD< double > > indModelWithPopDataAD(
    predEvaluatorAD,
    predEvaluatorADAD,
    predEvaluatorADADAD,
    nThetaPop,
    thetaPopLow,
    thetaPopUp,
    thetaPopIn,
    nEtaInd,
    omegaIndStruct,
    omegaIndMinRep );


  //------------------------------------------------------------
  // Quantities related to the population parameter, alp.
  //------------------------------------------------------------

  // Get the number of population parameters.
  const int nAlp = popModel.getNPopPar();


  //------------------------------------------------------------
  // Quantities related to the individual parameters, b.
  //------------------------------------------------------------

  // Get the number of individual parameters.
  const int nB = indModelWithPopData.getNIndPar();
  assert( nB == nThetaInd + nOmegaIndPar );

  valarray<double> bLow( nB );
  valarray<double> bUp ( nB );

  // Get the limits for the individual parameters.
  indModelWithPopData.getIndParLimits( bLow, bUp );


  //------------------------------------------------------------
  // Set the initial atomic measure points.
  //------------------------------------------------------------

  int nPointPerSide = 5;
  int nMeasurePointIn = pow( nPointPerSide, nB );

  valarray<double> bAllIn   ( nMeasurePointIn * nB );
  valarray<double> bGridStep( nB );

  int p;
  int q;

  int k;

  // Calculate the grid size that will give points at the upper and
  // lower bounds of the box.
  for ( k = 0; k < nB; k++ )
  {
    bGridStep[k] = ( bUp[k] - bLow[k] ) / ( nPointPerSide - 1);
  }

  // Set the value for the initial atomic measure points.
  int j = 0;
  for ( p = 0; p < nPointPerSide; p++ )
  {
    for ( q = 0; q < nPointPerSide; q++ )
    {
      bAllIn[0 + j * nB] = bLow[0] + p * bGridStep[0];
      bAllIn[1 + j * nB] = bLow[1] + q * bGridStep[1];

      j++;
    }
  }

  // Check that the proper number of grid points were set.
  assert( nMeasurePointIn == j );


  //------------------------------------------------------------
  // Remaining inputs to predNonparamMethod.
  //------------------------------------------------------------

  // These will hold all of the calculated values.
  double           alpObjOut;
  valarray<double> bAllOut          ( nB * nMeasurePointIn );
  valarray<double> lambdaOut        ( nMeasurePointIn );
  valarray<double> probDensityAllOut( nB * nMeasurePointIn );
  valarray<double> bPostMeanAllOut  ( nB * nInd );

  // Set the values for the nonparametric relaxed interior point
  // subproblem.
  //
  // Note that this unit test uses zero iterations for this level of
  // optimization in order to remove issues of convergence and because
  // there is not a known solution for this model.
  double indEpsilon = 1.e-4; 
  int indNMaxIter   = 0; 
  int indLevel      = 0;
  Optimizer indOptInfo( indEpsilon, indNMaxIter, indLevel ); 

  // Set the values for optimization of the nonparametric population
  // objective function.
  //
  // Note that this unit test uses zero iterations for this level of
  // optimization in order to remove issues of convergence and because
  // there is not a known solution for this model.
  double popEpsilon = 1.e-4; 
  int popNMaxIter   = 0; 
  int popLevel      = 0;
  Optimizer popOptInfo( popEpsilon, popNMaxIter, popLevel ); 


  //------------------------------------------------------------
  // Perform the nonparametric method.
  //------------------------------------------------------------

  try
  {
    predNonparamMethod( popModel,
                        indModelWithPopData,
                        indModelWithPopDataAD,
                        N,
                        Y,
                        popOptInfo,
                        indOptInfo,
                        bLow,
                        bUp,
                        bAllIn,
                        &alpObjOut,
                        &bAllOut,
                        &lambdaOut,
                        &probDensityAllOut,
                        &bPostMeanAllOut );
  }
  catch( const SpkException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "predNonparamMethod failed!", false );
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE( "predNonparamMethod failed for unknown reasons!", false);
  }

  valarray<double> thetaPopNonparam      ( nThetaPop );
  valarray<double> omegaPopMinRepNonparam( nOmegaPopPar );
  valarray<double> omegaPopNonparam      ( nEtaPop * nEtaPop );
  valarray<double> sigmaPopMinRepNonparam( nSigmaPopPar );
  valarray<double> sigmaPopNonparam      ( nEpsPop * nEpsPop );
  assert( nThetaPop == nEtaPop );

  // Get the values for the population parameters that were set for
  // the population model at the end of the pred library wrapper for
  // the nonparametric method.
  popModel.getTheta( thetaPopNonparam );
  popModel.getOmega( omegaPopMinRepNonparam );
  popModel.getSigma( sigmaPopMinRepNonparam );

  // Expand the minimal representations for the nonparametric values for
  // the covariance matrices.
  omegaPop.expandCovMinRep( omegaPopMinRepNonparam, omegaPopNonparam );
  sigmaPop.expandCovMinRep( sigmaPopMinRepNonparam, sigmaPopNonparam );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  // Set the final atomic measure points equal to their initial values
  // because this unit test uses zero iterations for both levels of
  // optimizations in order to remove issues of convergence and
  // because there is not a known solution for this model.
  valarray<double> bAllKnown( bAllIn );

  // Set the final number of atomic measure points.
  int nMeasurePointOut = bAllKnown.size() / nB;

  DoubleMatrix dmatBAllKnown( bAllKnown, nMeasurePointOut );

  // These will hold the rest of the known values.
  valarray<double> lambdaKnown        ( nMeasurePointOut );
  valarray<double> probDensityAllKnown( nInd * nMeasurePointOut );
  valarray<double> bPostMeanAllKnown  ( nB * nInd );

  DoubleMatrix dmatProbDensityAllKnown( nInd, nMeasurePointOut );

  valarray<double> bKnown_j ( nB );

  valarray<double> thetaPopKnown( nThetaPop );
  valarray<double> omegaPopKnown( nEtaPop * nEtaPop );
  valarray<double> sigmaPopKnown( nEpsPop * nEpsPop );

  valarray<double> thetaIndKnown_j        ( nThetaInd );
  valarray<double> thetaIndMeanKnown      ( nThetaInd );
  valarray<double> thetaIndDiff_j         ( nThetaInd );
  valarray<double> thetaIndDiff_jTrans    ( nThetaInd );
  valarray<double> thetaIndDiff_jCrossProd( nThetaInd * nThetaInd );
  valarray<double> thetaIndCov            ( nThetaInd * nThetaInd );
  valarray<double> thetaIndCovKnown       ( nThetaInd * nThetaInd );
  valarray<double> omegaIndKnown_j        ( nEtaInd   * nEtaInd );
  valarray<double> omegaIndMeanKnown      ( nEtaInd   * nEtaInd );

  // Initially set these all equal to zero.
  thetaIndMeanKnown = 0.0;
  omegaIndMeanKnown = 0.0;
  thetaIndCov       = 0.0;

  double* pdNull = 0;
  valarray<double>* pVANull = 0;

  // Set the known values for the weights for the atomic measure
  // points.
  //
  // Because no iterations were performed, each atomic measure point
  // should have the same weight and all of the weights should add up
  // to one.
  lambdaKnown = 1.0 / nMeasurePointOut;

  // Convert this valarray to DoubleMatrix.
  DoubleMatrix dvecN( nInd, 1 );
  double* pNData = dvecN.data();
  for ( i = 0; i < nInd; i++ )
  {
    pNData[i] = N[i];
  }

  // Convert this valarray to DoubleMatrix.
  DoubleMatrix dvecY( Y );

  // Calculate the probability density for y_i given the individual
  // parameter is equal to the j-th atomic measure point b_j,
  //
  //     p( y  | b  )  .
  //         i    j
  //
  // Use the known value for the atomic measure points and the known
  // weights to perform the calculation.
  //
  // The code used to calculate the known values was copied from the
  // unit test for spk_non_par.
  p_y_given_b(
    indModelWithPopData,
    dvecY,
    dvecN,
    dmatBAllKnown,        
    dmatProbDensityAllKnown );
  probDensityAllKnown = dmatProbDensityAllKnown.toValarray();

  double alpObjKnown = 0.0;
  double denom_i;
  valarray<double> bPostMean_i( nB );

  // Calculate the known value for the posterior mean for the i-th
  // individual
  //
  //                        nMeasurePoint
  //                            ----
  //                            \    
  //                            /     lambda  * p (y | b ) * b
  //                            ----        j       i   j     j
  //                            j = 1 
  //
  //     bPostMean   =  ------------------------------------------  ,
  //              i
  //                            nMeasurePoint
  //                                ----
  //                                \    
  //                                /     lambda  * p (y | b )
  //                                ----        j       i   j
  //                                j = 1 
  //
  // At the same time, calculate the known value for the population
  // objective function,
  //
  //                         -                                 -
  //              nInd      |   nMeasurePoint                   |
  //              ----      |       ----                        |
  //              \         |       \                           |
  //     F  =  -  /     log |       /     lambda  * p (y | b )  |  .
  //              ----      |       ----        j       i   j   |
  //              i = 1     |       j = 1                       |
  //                         -                                 -
  //
  for ( i = 0; i < nInd; i++ )
  {
    // Zero these before calculating each individual's value.
    denom_i = 0.0;
    bPostMean_i = 0.0;

    // Calculate this individual's denominator.
    for ( j = 0; j < nMeasurePointOut; j++ )
    {
      denom_i += lambdaKnown[j] * probDensityAllKnown[i + j * nInd];
    }

    // Add this individual's contribution to the population objective
    // function.
    alpObjKnown -= std::log( denom_i );

    // Calculate this individual's posterior mean.
    for ( j = 0; j < nMeasurePointOut; j++ )
    {
      // Get this atomic measure point's optimal parameter value.
      bKnown_j = bAllKnown[ slice( j * nB, nB, 1 ) ];

      // Add in this atomic measure point multiplied by its weight and
      // its probability density all divided by the denominator.
      bPostMean_i += bKnown_j *
        ( ( lambdaKnown[j] * probDensityAllKnown[i + j * nInd] ) / denom_i );
    }

    // Set this individual's known posterior mean in the matrix of all
    // of the individuals' values.
    bPostMeanAllKnown[ slice( i * nB, nB, 1 ) ] = bPostMean_i;
  }

  // Calculate the known values for the means for the atomic measure
  // points' values for theta and Omega,
  //
  //                      nMeasurePoint
  //                          ----
  //                          \    
  //     thetaIndMean  =      /     lambda  * thetaInd   ,
  //                          ----        j           j
  //                          j = 1 
  //
  //
  //                      nMeasurePoint
  //                          ----
  //                          \    
  //     OmegaIndMean  =      /     lambda  * OmegaInd   .
  //                          ----        j           j
  //                          j = 1 
  //
  for ( j = 0; j < nMeasurePointOut; j++ )
  {
    // Get this atomic measure point's optimal parameter value.
    bKnown_j = bAllKnown[ slice( j * nB, nB, 1 ) ];

    // Set the model's individual parameter value equal to this atomic
    // measure point.
    indModelWithPopData.setIndPar( bKnown_j );

    // Get this atomic measure point's theta and Omega values.
    indModelWithPopData.getTheta( thetaIndKnown_j );
    indModelWithPopData.getOmega( omegaIndKnown_j );

    // Add in this atomic measure point's theta and Omega values
    // multiplied by their weight.
    thetaIndMeanKnown += lambdaKnown[j] * thetaIndKnown_j;
    omegaIndMeanKnown += lambdaKnown[j] * omegaIndKnown_j;
  }

  // Set the known value for the population theta equal to the mean
  // for the atomic measure points' values for theta,
  //
  //     thetaPop  =  thetaIndMean  .
  //
  thetaPopKnown = thetaIndMeanKnown;

  // Set the known value for the populaton sigma equal to the mean for
  // the atomic measure points' values for Omega,
  //
  //     sigmaPop  =  ometaIndMean  .
  //
  sigmaPopKnown = omegaIndMeanKnown;

  // Calculate the known value for the covariance of the atomic
  // measure points' values for theta,
  //
  //     thetaIndCov
  //
  //            nMeasurePoint
  //                ----
  //                \                                                                           T
  //         =      /      lambda  * ( thetaInd  -  thetaIndMean ) ( thetaInd  -  thetaIndMean )   .
  //                ----         j             j                             j       
  //                j = 1 
  //
  for ( j = 0; j < nMeasurePointOut; j++ )
  {
    // Get this atomic measure point's optimal parameter value.
    bKnown_j = bAllKnown[ slice( j * nB, nB, 1 ) ];

    // Set the model's individual parameter value equal to this atomic
    // measure point.
    indModelWithPopData.setIndPar( bKnown_j );

    // Get this atomic measure point's theta value.
    indModelWithPopData.getTheta( thetaIndKnown_j );

    // Calculate the difference of between this atomic measure point's
    // theta value and the mean value.
    thetaIndDiff_j = thetaIndKnown_j - thetaIndMeanKnown;

    // Calculate this atomic measure point's cross-product.
    thetaIndDiff_jTrans     = transpose( thetaIndDiff_j, 1 );
    thetaIndDiff_jCrossProd = multiply( thetaIndDiff_j, 1, thetaIndDiff_jTrans, nThetaInd );

    // Add in this atomic measure point's cross-product value
    // multiplied by its weight.
    thetaIndCov += lambdaKnown[j] * thetaIndDiff_jCrossProd;
  }

  // Set the known value for the population Omega equal to the
  // covariance for the atomic measure points' values for theta,
  //
  //     OmegaPop  =  thetaIndCov  .
  //
  omegaPopKnown = thetaIndCov;


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  CPPUNIT_ASSERT_DOUBLES_EQUAL(
          alpObjOut,
          alpObjKnown,
          tol );

  compareToKnown( 
    bAllOut,
    bAllKnown,
    "bAll",
    tol );

  compareToKnown( 
    lambdaOut,
    lambdaKnown,
    "lambda",
    tol );

  tol = 1.0e-12;

  compareToKnown( 
    probDensityAllOut,
    probDensityAllKnown,
    "probDensityAll",
    tol );

  tol = 1.0e-14;

  compareToKnown( 
    bPostMeanAllOut,
    bPostMeanAllKnown,
    "bPostMeanAll",
    tol );

  compareToKnown( 
    thetaPopNonparam,
    thetaPopKnown,
    "thetaPop",
    tol );

  compareToKnown( 
    omegaPopNonparam,
    omegaPopKnown,
    "omegaPop",
    tol );

  compareToKnown( 
    sigmaPopNonparam,
    sigmaPopKnown,
    "sigmaPop",
    tol );

}

