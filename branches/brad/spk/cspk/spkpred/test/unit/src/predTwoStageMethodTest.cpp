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
 * File: predTwoStageMethodTest.cpp
 *
 *
 * Unit test for the function predTwoStageMethod.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "predTwoStageMethodTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include "../../../spkpred/DiagCov.h"
#include "../../../spkpred/FullCov.h"
#include "../../../spkpred/IndPredModel.h"
#include "../../../spkpred/PopPredModel.h"
#include "../../../spkpred/predTwoStageMethod.h"

// SPK library header files.
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
 * Local variable declarations
 *------------------------------------------------------------------------*/

namespace railexample_predtwostagemethodtest
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


namespace railexample_modifieddata_predtwostagemethodtest
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
  //
  // Note that the data values for the first and fifth individuals
  // were modified from their original values.
  double YCArray[nInd * nY_i] = {
    4.0000E+01,
    5.1000E+01,
    4.6000E+01,
    2.6000E+01,
    3.7000E+01,
    3.2000E+01,
    7.8000E+01,
    9.1000E+01,
    8.5000E+01,
    9.2000E+01,
    1.0000E+02,
    9.6000E+01,
    7.0000E+01,
    7.5000E+01,
    7.3000E+01,
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
  // to individual notation for this two-stage method test is
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

      using namespace railexample_predtwostagemethodtest;


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

void predTwoStageMethodTest::setUp()
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

void predTwoStageMethodTest::tearDown()
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

Test* predTwoStageMethodTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite("predTwoStageMethodTest");

    suiteOfTests->addTest(new TestCaller<predTwoStageMethodTest>(
      "RailExampleSTS_Test", &predTwoStageMethodTest::RailExampleSTS_Test));

    suiteOfTests->addTest(new TestCaller<predTwoStageMethodTest>(
      "RailExampleITS_Test", &predTwoStageMethodTest::RailExampleITS_Test));

    suiteOfTests->addTest(new TestCaller<predTwoStageMethodTest>(
      "RailExampleGTS_Test", &predTwoStageMethodTest::RailExampleGTS_Test));

    return suiteOfTests;
}


/*************************************************************************
 *
 * Function: RailExampleSTS_Test
 *
 *
 * This test uses a Pred block expression evaluator that corresponds
 * to the Rail Example that is included in the NLME distribution.
 *
 * The PRED block for the Rail Example after it has been converted
 * to individual notation for this two-stage method test is
 *
 *     $PRED 
 *     F = THETA(1)
 *     Y = F + ETA(1)
 *
 *************************************************************************/

void predTwoStageMethodTest::RailExampleSTS_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace railexample_predtwostagemethodtest;

  int i;
  int j;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
  //------------------------------------------------------------

  RailExample_Pred< double > predEvaluator( nY_i );

  RailExample_Pred< AD<double> > predEvaluatorAD( nY_i );

  RailExample_Pred< AD< AD<double> > > predEvaluatorADAD( nY_i );


  //------------------------------------------------------------
  // Prepare the population model variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of population model independent variables.
  //
  // Note that for the population models used with the two-stage
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
  thetaPopLow[0] = 7.2;
  thetaPopUp[0]  = 720.0;

  // Set the initial value for eta equal to all zeros.
  valarray<double> etaPopIn( 0.0, nEtaPop );


  //------------------------------------------------------------
  // Initialize quantities related to the population covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  //
  // Note that for the population models used with the two-stage
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
  sigmaPopMinRep[0] = 32.0;
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
  // The Pred library specific two-stage method wrapper determines
  // population means and covariances for individual parameters.
  //
  // It does that by performing two-stage method analysis' using this
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
  // Note that for the individual models used with the two-stage
  // methods, the Omega matrix must have the same structure as the
  // population Sigma matrix.
  covStruct omegaIndStruct = sigmaPopStruct;

  // Set the number elements for this parameterization.
  int nOmegaIndPar = nSigmaPopPar;

  // Set the initial minimal representation.
  valarray<double> omegaIndMinRep( nOmegaIndPar );
  omegaIndMinRep = sigmaPopMinRep;


  //------------------------------------------------------------
  // Construct the individual level Pred model with the population data.
  //------------------------------------------------------------

  // Construct the individual model that will be applied to the same
  // sets of individuals' data sets as for the population model.
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


  //------------------------------------------------------------
  // Prepare the set of individual level Pred models with individual data.
  //------------------------------------------------------------

  // This will contain pointers to an individual level Pred model for
  // each individual.
  std::vector<IndPredModel*> pIndModelWithIndData( nInd );

  // Construct the set of individual models that will each be used
  // with a single individual's data set.
  for ( i = 0; i < nInd; i++ )
  {
    // Note that the population model's eps and Sigma information is
    // used for the individual model's eta and Omega information.
    pIndModelWithIndData[i] = new IndPredModel(
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
  }


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

  valarray<double> bLow ( nB );
  valarray<double> bUp  ( nB );
  valarray<double> bStep( nB );
  valarray<double> bIn_i( nB );

  // Get the current value for the individual parameters.
  indModelWithPopData.getIndPar( bIn_i );

  // Get the limits for the individual parameters.
  indModelWithPopData.getIndParLimits( bLow, bUp );

  // Get the step sizes for the individual parameters.
  indModelWithPopData.getIndParStep( bStep );

  // This will hold all of the individuals' parameter values.  
  valarray<double> bAllOut( nB * nInd );


  //------------------------------------------------------------
  // Remaining inputs to predTwoStageMethod.
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
    predTwoStageMethod( popModel,
                        indModelWithPopData,
                        method,
                        N,
                        Y,
                        popOptInfo,
                        indOptInfo,
                        &bAllOut );
  }
  catch( const SpkException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "predTwoStageMethod failed!", false );
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE( "predTwoStageMethod failed for unknown reasons!", false);
  }

  valarray<double> thetaPopTwoStage      ( nThetaPop );
  valarray<double> omegaPopMinRepTwoStage( nOmegaPopPar );
  valarray<double> omegaPopTwoStage      ( nEtaPop * nEtaPop );
  valarray<double> sigmaPopMinRepTwoStage( nSigmaPopPar );
  valarray<double> sigmaPopTwoStage      ( nEpsPop * nEpsPop );
  assert( nThetaPop == nEtaPop );

  // Get the values for the population parameters that were set for
  // the population model at the end of the pred library wrapper for
  // the two-stage method.
  popModel.getTheta( thetaPopTwoStage );
  popModel.getOmega( omegaPopMinRepTwoStage );
  popModel.getSigma( sigmaPopMinRepTwoStage );

  // Expand the minimal representations for the two-stage values for
  // the covariance matrices.
  omegaPop.expandCovMinRep( omegaPopMinRepTwoStage, omegaPopTwoStage );
  sigmaPop.expandCovMinRep( sigmaPopMinRepTwoStage, sigmaPopTwoStage );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> bKnown_i ( nB );
  valarray<double> bAllKnown( nB * nInd );

  valarray<double> thetaPopKnown( nThetaPop );
  valarray<double> omegaPopKnown( nEtaPop * nEtaPop );
  valarray<double> sigmaPopKnown( nEpsPop * nEpsPop );

  valarray<double> thetaIndKnown_i           ( nThetaInd );
  valarray<double> thetaIndKnown_iSum        ( nThetaInd );
  valarray<double> thetaIndMeanKnown         ( nThetaInd );
  valarray<double> thetaIndDiff_i            ( nThetaInd );
  valarray<double> thetaIndDiff_iTrans       ( nThetaInd );
  valarray<double> thetaIndDiff_iCrossProd   ( nThetaInd * nThetaInd );
  valarray<double> thetaIndDiff_iCrossProdSum( nThetaInd * nThetaInd );
  valarray<double> thetaIndCovKnown          ( nThetaInd * nThetaInd );
  valarray<double> omegaIndKnown_i           ( nEtaInd   * nEtaInd );
  valarray<double> omegaIndKnown_iSum        ( nEtaInd   * nEtaInd );
  valarray<double> omegaIndMeanKnown         ( nEtaInd   * nEtaInd );

  // Initially set these all equal to zero.
  thetaIndKnown_iSum         = 0.0;
  omegaIndKnown_iSum         = 0.0;
  thetaIndDiff_iCrossProdSum = 0.0;

  double* pdNull = 0;
  valarray<double>* pVANull = 0;

  // Calculate the known values for the means for the individuals'
  // values for theta and Omega,
  //
  //                              nInd
  //                              ----
  //                        1     \    
  //     thetaIndMean  =  ------  /     thetaInd   ,
  //                       nInd   ----          i
  //                              i = 1 
  //
  //                              nInd
  //                              ----
  //                        1     \    
  //     OmegaIndMean  =  ------  /     OmegaInd   .
  //                       nInd   ----          i
  //                              i = 1 
  //
  for ( i = 0; i < nInd; i++ )
  {
    // Get this individual's data values.
    Y_i = Y[ slice( i * nY_i, nY_i, 1 ) ];

    // Calculate this individual's optimal parameter value.
    fitIndividual(
      *( pIndModelWithIndData[i] ),
      Y_i,
      indOptInfo,
      bLow,
      bUp,
      bIn_i,
      bStep,
      &bKnown_i,
      pdNull,
      pVANull,
      pVANull,
      withD );

    // Set this individual's optimal parameter value in the matrix.
    bAllKnown[ slice( i * nB, nB, 1 ) ] = bKnown_i;

    // Set this individual's optimal parameter value in their model.
    pIndModelWithIndData[i]->setIndPar( bKnown_i );

    // Get this individual's theta and Omega values.
    pIndModelWithIndData[i]->getTheta( thetaIndKnown_i );
    pIndModelWithIndData[i]->getOmega( omegaIndKnown_i );

    // Add in this individual's theta and Omega values.
    thetaIndKnown_iSum += thetaIndKnown_i;
    omegaIndKnown_iSum += omegaIndKnown_i;
  }
  thetaIndMeanKnown = thetaIndKnown_iSum / static_cast<double>( nInd );
  omegaIndMeanKnown = omegaIndKnown_iSum / static_cast<double>( nInd );

  // Set the known value for the population theta equal to the mean
  // for the individuals' values for theta,
  //
  //     thetaPop  =  thetaIndMean  .
  //
  thetaPopKnown = thetaIndMeanKnown;

  // Set the known value for the populaton sigma equal to the mean for
  // the individuals' values for Omega,
  //
  //     sigmaPop  =  ometaIndMean  .
  //
  sigmaPopKnown = omegaIndMeanKnown;

  // Calculate the known value for the covariance of the individuals'
  // values for theta,
  //
  //                              nInd
  //                              ----
  //                        1     \                                                                 T
  //     thetaIndCov   =  ------  /      ( thetaInd  -  thetaIndMean ) ( thetaInd  -  thetaIndMean )   ,
  //                       nInd   ----             i                             i
  //                              i = 1 
  //
  for ( i = 0; i < nInd; i++ )
  {
    // Get this individual's optimal parameter value.
    bKnown_i = bAllKnown[ slice( i * nB, nB, 1 ) ];

    // Set the current individual's parameter value.
    pIndModelWithIndData[i]->setIndPar( bKnown_i );

    // Get this individual's theta value.
    pIndModelWithIndData[i]->getTheta( thetaIndKnown_i );

    // Calculate the difference of between this individual's theta
    // value and the mean value.
    thetaIndDiff_i = thetaIndKnown_i - thetaIndMeanKnown;

    // Calculate this individual's cross-product.
    thetaIndDiff_iTrans     = transpose( thetaIndDiff_i, 1 );
    thetaIndDiff_iCrossProd = multiply( thetaIndDiff_i, 1, thetaIndDiff_iTrans, nThetaInd );

    // Add in this individual's cross-product value.
    thetaIndDiff_iCrossProdSum += thetaIndDiff_iCrossProd;
  }
  thetaIndCovKnown = thetaIndDiff_iCrossProdSum / static_cast<double>( nInd );

  // Set the known value for the population Omega equal to the
  // covariance for the individuals' values for theta,
  //
  //     OmegaPop  =  thetaIndCov  .
  //
  omegaPopKnown = thetaIndCovKnown;


  //------------------------------------------------------------
  // Free the memory allocated for the set of individual models.
  //------------------------------------------------------------

  // Call the destructor for each of the individual models that were
  // used with a single individual's data set.
  for ( i = 0; i < nInd; i++ )
  {
    delete pIndModelWithIndData[i];
  }


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    thetaPopTwoStage,
    thetaPopKnown,
    "thetaPop",
    tol );

  compareToKnown( 
    omegaPopTwoStage,
    omegaPopKnown,
    "omegaPop",
    tol );

  compareToKnown( 
    sigmaPopTwoStage,
    sigmaPopKnown,
    "sigmaPop",
    tol );

}


/*************************************************************************
 *
 * Function: RailExampleITS_Test
 *
 *
 * This test uses a Pred block expression evaluator that corresponds
 * to the Rail Example that is included in the NLME distribution.
 *
 * The PRED block for the Rail Example after it has been converted
 * to individual notation for this two-stage method test is
 *
 *     $PRED 
 *     F = THETA(1)
 *     Y = F + ETA(1)
 *
 *************************************************************************/

void predTwoStageMethodTest::RailExampleITS_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // This test uses a modified data set.
  using namespace railexample_modifieddata_predtwostagemethodtest;

  int i;
  int j;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
  //------------------------------------------------------------

  RailExample_Pred< double > predEvaluator( nY_i );

  RailExample_Pred< AD<double> > predEvaluatorAD( nY_i );

  RailExample_Pred< AD< AD<double> > > predEvaluatorADAD( nY_i );


  //------------------------------------------------------------
  // Prepare the population model variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of population model independent variables.
  //
  // Note that for the population models used with the two-stage
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
  thetaPopLow[0] = 7.2;
  thetaPopUp[0]  = 720.0;

  // Set the initial value for eta equal to all zeros.
  valarray<double> etaPopIn( 0.0, nEtaPop );


  //------------------------------------------------------------
  // Initialize quantities related to the population covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  //
  // Note that for the population models used with the two-stage
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
  sigmaPopMinRep[0] = 32.0;
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
  // The Pred library specific two-stage method wrapper determines
  // population means and covariances for individual parameters.
  //
  // It does that by performing two-stage method analysis' using this
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
  // Note that for the individual models used with the two-stage
  // methods, the Omega matrix must have the same structure as the
  // population Sigma matrix.
  covStruct omegaIndStruct = sigmaPopStruct;

  // Set the number elements for this parameterization.
  int nOmegaIndPar = nSigmaPopPar;

  // Set the initial minimal representation.
  valarray<double> omegaIndMinRep( nOmegaIndPar );
  omegaIndMinRep = sigmaPopMinRep;


  //------------------------------------------------------------
  // Construct the individual level Pred model with the population data.
  //------------------------------------------------------------

  // Construct the individual model that will be applied to the same
  // sets of individuals' data sets as for the population model.
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


  //------------------------------------------------------------
  // Prepare the set of individual level Pred models with individual data.
  //------------------------------------------------------------

  // This will contain pointers to an individual level Pred model for
  // each individual.
  std::vector<IndPredModel*> pIndModelWithIndData( nInd );

  // Construct the set of individual models that will each be used
  // with a single individual's data set.
  for ( i = 0; i < nInd; i++ )
  {
    // Note that the population model's eps and Sigma information is
    // used for the individual model's eta and Omega information.
    pIndModelWithIndData[i] = new IndPredModel(
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
  }


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

  valarray<double> bLow ( nB );
  valarray<double> bUp  ( nB );
  valarray<double> bStep( nB );
  valarray<double> bIn_i( nB );

  // Get the current value for the individual parameters.
  indModelWithPopData.getIndPar( bIn_i );

  // Get the limits for the individual parameters.
  indModelWithPopData.getIndParLimits( bLow, bUp );

  // Get the step sizes for the individual parameters.
  indModelWithPopData.getIndParStep( bStep );

  // This will hold all of the individuals' parameter values.  
  valarray<double> bAllOut( nB * nInd );


  //------------------------------------------------------------
  // Remaining inputs to predTwoStageMethod.
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
    predTwoStageMethod( popModel,
                        indModelWithPopData,
                        method,
                        N,
                        Y,
                        popOptInfo,
                        indOptInfo,
                        &bAllOut );
  }
  catch( const SpkException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "predTwoStageMethod failed!", false );
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE( "predTwoStageMethod failed for unknown reasons!", false);
  }

  valarray<double> thetaPopTwoStage      ( nThetaPop );
  valarray<double> omegaPopMinRepTwoStage( nOmegaPopPar );
  valarray<double> omegaPopTwoStage      ( nEtaPop * nEtaPop );
  valarray<double> sigmaPopMinRepTwoStage( nSigmaPopPar );
  valarray<double> sigmaPopTwoStage      ( nEpsPop * nEpsPop );
  assert( nThetaPop == nEtaPop );

  // Get the values for the population parameters that were set for
  // the population model at the end of the pred library wrapper for
  // the two-stage method.
  popModel.getTheta( thetaPopTwoStage );
  popModel.getOmega( omegaPopMinRepTwoStage );
  popModel.getSigma( sigmaPopMinRepTwoStage );

  // Expand the minimal representations for the two-stage values for
  // the covariance matrices.
  omegaPop.expandCovMinRep( omegaPopMinRepTwoStage, omegaPopTwoStage );
  sigmaPop.expandCovMinRep( sigmaPopMinRepTwoStage, sigmaPopTwoStage );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> thetaPopKnown( nThetaPop );
  valarray<double> omegaPopKnown( nEtaPop * nEtaPop );
  valarray<double> sigmaPopKnown( nEpsPop * nEpsPop );

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
  //     
  thetaPopKnown[0] =  68.930759775187;
  omegaPopKnown[0] = 512.855962;
  sigmaPopKnown[0] =  13.7975614035972;


  //------------------------------------------------------------
  // Free the memory allocated for the set of individual models.
  //------------------------------------------------------------

  // Call the destructor for each of the individual models that were
  // used with a single individual's data set.
  for ( i = 0; i < nInd; i++ )
  {
    delete pIndModelWithIndData[i];
  }


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  // Do this test with more accuracy than the values have.
  double tol = popEpsilon * popEpsilon;

  compareToKnown( 
    thetaPopTwoStage,
    thetaPopKnown,
    "thetaPop",
    tol );

  tol = popEpsilon;

  compareToKnown( 
    omegaPopTwoStage,
    omegaPopKnown,
    "omegaPop",
    tol );

  compareToKnown( 
    sigmaPopTwoStage,
    sigmaPopKnown,
    "sigmaPop",
    tol );

}


/*************************************************************************
 *
 * Function: RailExampleGTS_Test
 *
 *
 * This test uses a Pred block expression evaluator that corresponds
 * to the Rail Example that is included in the NLME distribution.
 *
 * The PRED block for the Rail Example after it has been converted
 * to individual notation for this two-stage method test is
 *
 *     $PRED 
 *     F = THETA(1)
 *     Y = F + ETA(1)
 *
 *************************************************************************/

void predTwoStageMethodTest::RailExampleGTS_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace railexample_predtwostagemethodtest;

  int i;
  int j;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
  //------------------------------------------------------------

  RailExample_Pred< double > predEvaluator( nY_i );

  RailExample_Pred< AD<double> > predEvaluatorAD( nY_i );

  RailExample_Pred< AD< AD<double> > > predEvaluatorADAD( nY_i );


  //------------------------------------------------------------
  // Prepare the population model variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of population model independent variables.
  //
  // Note that for the population models used with the two-stage
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
  thetaPopLow[0] = 7.2;
  thetaPopUp[0]  = 720.0;

  // Set the initial value for eta equal to all zeros.
  valarray<double> etaPopIn( 0.0, nEtaPop );


  //------------------------------------------------------------
  // Initialize quantities related to the population covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  //
  // Note that for the population models used with the two-stage
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
  sigmaPopMinRep[0] = 32.0;
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
  // The Pred library specific two-stage method wrapper determines
  // population means and covariances for individual parameters.
  //
  // It does that by performing two-stage method analysis' using this
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
  // Note that for the individual models used with the two-stage
  // methods, the Omega matrix must have the same structure as the
  // population Sigma matrix.
  covStruct omegaIndStruct = sigmaPopStruct;

  // Set the number elements for this parameterization.
  int nOmegaIndPar = nSigmaPopPar;

  // Set the initial minimal representation.
  valarray<double> omegaIndMinRep( nOmegaIndPar );
  omegaIndMinRep = sigmaPopMinRep;


  //------------------------------------------------------------
  // Construct the individual level Pred model with the population data.
  //------------------------------------------------------------

  // Construct the individual model that will be applied to the same
  // sets of individuals' data sets as for the population model.
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


  //------------------------------------------------------------
  // Prepare the set of individual level Pred models with individual data.
  //------------------------------------------------------------

  // This will contain pointers to an individual level Pred model for
  // each individual.
  std::vector<IndPredModel*> pIndModelWithIndData( nInd );

  // Construct the set of individual models that will each be used
  // with a single individual's data set.
  for ( i = 0; i < nInd; i++ )
  {
    // Note that the population model's eps and Sigma information is
    // used for the individual model's eta and Omega information.
    pIndModelWithIndData[i] = new IndPredModel(
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
  }


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

  valarray<double> bLow ( nB );
  valarray<double> bUp  ( nB );
  valarray<double> bStep( nB );
  valarray<double> bIn_i( nB );

  // Get the current value for the individual parameters.
  indModelWithPopData.getIndPar( bIn_i );

  // Get the limits for the individual parameters.
  indModelWithPopData.getIndParLimits( bLow, bUp );

  // Get the step sizes for the individual parameters.
  indModelWithPopData.getIndParStep( bStep );

  // This will hold all of the individuals' parameter values.  
  valarray<double> bAllOut( nB * nInd );


  //------------------------------------------------------------
  // Remaining inputs to predTwoStageMethod.
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
    predTwoStageMethod( popModel,
                        indModelWithPopData,
                        method,
                        N,
                        Y,
                        popOptInfo,
                        indOptInfo,
                        &bAllOut );
  }
  catch( const SpkException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "predTwoStageMethod failed!", false );
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE( "predTwoStageMethod failed for unknown reasons!", false);
  }

  valarray<double> thetaPopTwoStage      ( nThetaPop );
  valarray<double> omegaPopMinRepTwoStage( nOmegaPopPar );
  valarray<double> omegaPopTwoStage      ( nEtaPop * nEtaPop );
  valarray<double> sigmaPopMinRepTwoStage( nSigmaPopPar );
  valarray<double> sigmaPopTwoStage      ( nEpsPop * nEpsPop );
  assert( nThetaPop == nEtaPop );

  // Get the values for the population parameters that were set for
  // the population model at the end of the pred library wrapper for
  // the two-stage method.
  popModel.getTheta( thetaPopTwoStage );
  popModel.getOmega( omegaPopMinRepTwoStage );
  popModel.getSigma( sigmaPopMinRepTwoStage );

  // Expand the minimal representations for the two-stage values for
  // the covariance matrices.
  omegaPop.expandCovMinRep( omegaPopMinRepTwoStage, omegaPopTwoStage );
  sigmaPop.expandCovMinRep( sigmaPopMinRepTwoStage, sigmaPopTwoStage );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> thetaPopKnown( nThetaPop );
  valarray<double> omegaPopKnown( nEtaPop * nEtaPop );
  valarray<double> sigmaPopKnown( nEpsPop * nEpsPop );

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
  thetaPopKnown[0] = 66.5243233;
  omegaPopKnown[0] = 510.56611;
  sigmaPopKnown[0] = 7.5374114;


  //------------------------------------------------------------
  // Free the memory allocated for the set of individual models.
  //------------------------------------------------------------

  // Call the destructor for each of the individual models that were
  // used with a single individual's data set.
  for ( i = 0; i < nInd; i++ )
  {
    delete pIndModelWithIndData[i];
  }


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  // Do this test with more accuracy than the values have.
  double tol = popEpsilon * popEpsilon;

  compareToKnown( 
    thetaPopTwoStage,
    thetaPopKnown,
    "thetaPop",
    tol );

  tol = popEpsilon;

  compareToKnown( 
    omegaPopTwoStage,
    omegaPopKnown,
    "omegaPop",
    tol );

  compareToKnown( 
    sigmaPopTwoStage,
    sigmaPopKnown,
    "sigmaPop",
    tol );

}


