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
 * File: popStatisticsTest.cpp
 *
 *
 * Unit test for the function popStatistics.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/
#include <iostream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "popStatisticsTest.h"

#include "../../../spk/SpkException.h"
#include "../../../spk/Objective.h"
#include "../../../spk/NaiveFoModel.h"
#include "../../../spk/DirBasedParallelControls.h"
#include "../../../spk/fitPopulation.h"
#include "../../../spk/firstOrderOpt.h"

using namespace std;
using namespace CppUnit;

void popStatisticsTest::setUp()
{
  // initializations
}
void popStatisticsTest::tearDown()
{
  // clean up
}

Test* popStatisticsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("popStatisticsTest");

  suiteOfTests->addTest(new TestCaller<popStatisticsTest>("modifiedLaplaceTest", &popStatisticsTest::modifiedLaplaceTest));

  suiteOfTests->addTest(new TestCaller<popStatisticsTest>("expectedHessianTest",
							  &popStatisticsTest::expectedHessianTest));

  suiteOfTests->addTest(new TestCaller<popStatisticsTest>("firstOrderTest",      
							  &popStatisticsTest::firstOrderTest));
  suiteOfTests->addTest(new TestCaller<popStatisticsTest>("naiveFirstOrderTest", 
							  &popStatisticsTest::naiveFirstOrderTest));
  suiteOfTests->addTest(new TestCaller<popStatisticsTest>("popMaskTest", 
							  &popStatisticsTest::popMaskTest));

  return suiteOfTests;
}

void popStatisticsTest::modifiedLaplaceTest()
{
  statisticsExampleTest(MODIFIED_LAPLACE);
  coreStatTest(MODIFIED_LAPLACE);
}
void popStatisticsTest::expectedHessianTest()
{
  statisticsExampleTest(EXPECTED_HESSIAN);
  coreStatTest(EXPECTED_HESSIAN);
}
void popStatisticsTest::firstOrderTest()
{
  statisticsExampleTest(FIRST_ORDER);
  coreStatTest(FIRST_ORDER);
}
void popStatisticsTest::naiveFirstOrderTest()
{
  statisticsExampleTest(NAIVE_FIRST_ORDER);
}

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/
#include <spk/popStatistics.h>
#include <spk/lTilde.h>
#include <spk/pi.h>
#include <spk/SpkModel.h>
#include <spk/inverse.h>
#include <spk/randNormal.h>
#include <iomanip>
#include <cmath>
#include <spk/printInMatrix.h>
#include <spk/scalarToDouble.h>


/*------------------------------------------------------------------------
 * Namespace Declarations
 *------------------------------------------------------------------------*/

using namespace std;


/*************************************************************************
 *
 * Function: statisticsExampleTest
 *
 *
 * This test implements the example problem from the popStatistics specification. 
 *
 *************************************************************************/

template <class Scalar>
class UserModelPopStatisticsExampleTest : public SpkModel<Scalar>
{
    valarray<Scalar> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    UserModelPopStatisticsExampleTest(int nA, int nB, int nYi)
      :
      _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {};    
    ~UserModelPopStatisticsExampleTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<Scalar>& aval)
    {
        _a = aval;
    }
    void doSetIndPar(const valarray<Scalar>& bval)
    {
        _b = bval;
    }
    void doIndParVariance( valarray<Scalar>& ret ) const
    {
        //
        // D = [ alp[1] ]
        //
        ret.resize(_nYi);
        ret[0] = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        // D_alp = [ 0  1 ]
        //
        ret.resize(_nYi * _nA);
        ret[0] = 0.0;
        ret[1] = 1.0;
        return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
        //
        // Dinv = [ 1.0 / alp[1] ]
        //
        assert(_a[1] != 0.0);
        ret.resize(_nB * _nB);
        double a1Double;
        scalarToDouble( _a[1], a1Double );
        ret[0] = 1.0 / a1Double;
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Dinv_alp = [ 0    -alp[1]^(-2) ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        double a1Double;
        scalarToDouble( _a[1], a1Double );
        ret[1] = -1.0 / ( a1Double * a1Double );
        return true;
    }
    void doDataMean( valarray<Scalar>& ret ) const
    {
        //
        // f = [ alp[0]+b[0] ]
        //
        ret.resize(_nYi);
        ret[0] = ( _a[0] + _b[0] );
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        //
        // f_alp = [ 1   0 ]
        //
        ret.resize(_nYi * _nA);
        ret[0] = 1.0;
        ret[1] = 0.0;
        return true;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        //
        // f_b = [ 1 ]
        //
        ret.resize(_nYi * _nB);
        ret[0] = 1.0;
        return true;
    }
    void doDataVariance( valarray<Scalar>& ret ) const
    {
        //
        // R = [ 1 ]
        //
        ret.resize(_nB*_nB);
        ret[0] = 1.0;
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        //
        // R_alp = [ 0   0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        //
        // R_b = [ 0 ]
        //
        ret.resize(_nB *_nB);
        ret[0] = 0.0;
        return false;
    }
    void doDataVarianceInv( valarray<Scalar>& ret ) const
    {
        //
        // Rinv = [ 1 ]
        //
        ret.resize(_nB * _nB);
        ret[0] = 1.0;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Rinv_alp = [ 0  0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        //
        // Rinv_b = [ 0 ]
        //
        ret.resize(_nB * _nB * _nB);
        ret[0] = 0.0;
        return false;
    }   

};

#include <spk/multiply.h>
void popStatisticsTest::statisticsExampleTest(enum Objective whichObjective)
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int i;

  //preTestPrinting( "Specification Example" );


  // Number of individuals.
  const int nInd = 10;

  // Number of measurements per individual (same for all)
  const int nYi = 1;

  // Number of measurements in total
  const int nY = nInd * nYi;

  const int nAlp = 2;

  const int nB = 1;

  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelPopStatisticsExampleTest<double> model( nAlp, nB, nYi );

  UserModelPopStatisticsExampleTest< CppAD::AD<double> > modelAD( nAlp, nB, nYi );


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
  valarray<double> Y( nY );

  // Number of measurements for each individual. 
  valarray<int> N( 1, nInd );

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

  // Set the measurements for each individual.
  //
  // Note: these values were generated on a 32-bit Pentium machine
  // using the following code.
  //
  //     int seed = 2;
  //     srand(seed);
  //
  //     valarray<double> sdECov(nY*nY);
  //     sdECov[ slice( 0, nY, nY+1 ) ] = sdETrue;
  //
  //     valarray<double> sdBCov(nY*nY);
  //     sdBCov[ slice( 0, nY, nY+1 ) ] = sdBTrue;
  //
  //     Y = meanBTrue + randNormal( sdBCov, nY ) + randNormal( sdECov, nY );
  //
  // The values generated on a 64-bit Athalon machine were different
  // and their optimal paramter values could not be calculated.  So,
  // these values have been set explicitly here to ensure they're the
  // same on all machines.
  //
  Y[0] = 1.88758;
  Y[1] = -1.03471;
  Y[2] = 1.18851;
  Y[3] = -0.476253;
  Y[4] = -1.45167;
  Y[5] = -0.797979;
  Y[6] = -0.0825739;
  Y[7] = 3.04214;
  Y[8] = 1.48168;
  Y[9] = -1.29312;

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
  alpLow [ 1 ] = 0.1;
  alpUp  [ 1 ] = 100.0;
  alpIn  [ 1 ] = 0.5;
  alpStep[ 1 ] = 1.0e-2;

  // Set the mask for trancating the alp vector.
  valarray<bool> alpMask( true, nAlp );


  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  valarray<double> bLow ( -1.5e+1, nB );
  valarray<double> bUp  ( +1.0e+1, nB );
  valarray<double> bStep(  1.0e-2, nB );

  valarray<double> bIn ( 1., nB * nInd );
  valarray<double> bOut(     nB * nInd );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  valarray<double> lTilde_alpOut    ( nAlp );
  valarray<double> lTilde_alp_alpOut( nAlp * nAlp );


  //------------------------------------------------------------
  // Remaining inputs to fitPopulation.
  //------------------------------------------------------------

  // Set the values associated with the individual objective function.
  Optimizer indOptimizer( 1.0e-6, 40, 0 );

  // Set the values associated with the population objective function.
  Optimizer popOptimizer( 1.0e-6, 40, 0 );

  // Set the parallel controls object
  DirBasedParallelControls parallelControls( false, 0, 0 );


  //------------------------------------------------------------
  // Optimize the population objective function.
  //------------------------------------------------------------

  bool ok;
  try{
    fitPopulation(
		  model, 
		  modelAD, 
		  whichObjective,
		  N,
		  Y,
		  popOptimizer,
		  alpLow,
		  alpUp,
		  alpIn,
		  alpStep,
		  &alpOut,
		  indOptimizer,
		  bLow,
		  bUp,
		  bIn,            
		  bStep,
		  &bOut,
		  &dLTildeOut,
		  &lTilde_alpOut,
		  &lTilde_alp_alpOut,
                  false,
                  false,
		  parallelControls 
		  );
    ok = true;
  }
  catch(...)
    {
      CPPUNIT_ASSERT(false);
    }

  // Set the mask equal to false for any parameter elements that are
  // at their bounds to indicate these elements should not be included
  // in the statistics calculation.
  for ( i = 0; i < nAlp; i++ )
  {
    alpMask[i] = !( alpLow[i] == alpUp[i]
                    ||
                    ( fabs( ( alpOut[i] - alpLow[i] ) / alpLow[i] ) <  popOptimizer.getEpsilon() )
                    ||
                    ( fabs( ( alpUp[i]  - alpOut[i] ) / alpUp[i]  ) <  popOptimizer.getEpsilon() ) );
  }

  /*
    cout << endl;
    cout << endl;
    cout << "=======================" << endl;
    cout << "objective = " << whichObjective << endl;
    cout << "LTilde = " << dLTildeOut << endl;
    cout << "popPar = " << endl;
    printInMatrix( alpOut, 1 );
    cout << "lTilde_alp_alpOut = " << endl;
    printInMatrix( lTilde_alp_alpOut, nAlp );
    cout << "-----------------------" << endl;
  */
  //------------------------------------------------------------
  // Compute statistics of population parameter estimates.
  //------------------------------------------------------------
  valarray<double> popParCovOut( nAlp * nAlp );
  valarray<double> popParSEOut( nAlp );
  valarray<double> popParCorOut( nAlp * nAlp );
  valarray<double> popParCVOut( nAlp );
  valarray<double> popParCIOut( nAlp * 2 );

  for( int form = 1; form < 4; form++ )
    {
      try{
	
	popStatistics(
		      model,
		      modelAD,
		      whichObjective,
		      N,
		      Y,
		      alpOut,
		      alpMask,
		      lTilde_alp_alpOut,
		      bOut,
		      bLow,
		      bUp,				 
		      bStep,
		      static_cast<PopCovForm>( form ),
		      &popParCovOut,
		      &popParSEOut,
		      &popParCorOut,
		      &popParCVOut,
		      &popParCIOut
		      );
      }
      catch( const SpkException& e )
	{
	  cerr << e << endl;
	  CPPUNIT_ASSERT_MESSAGE( "popStatistics failed!", false );
	}
      catch(...)
	{
	  CPPUNIT_ASSERT_MESSAGE( "popStatistics failed for unknown reasons!", false);
	}
      
      /*
	cout << "formulation = " << form << endl;
	cout << "popParCovOut = " << endl;
	printInMatrix( popParCovOut, nAlp );
	cout << "-----------------------" << endl;
      */
      double eps = 1e-5;
      
      //------------------------------------------------------------
      // Test popParCovOut formulation R.
      //------------------------------------------------------------
      
      if( form == R )             
	{
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 0 ] * popParCovOut[ 0 ] +
					lTilde_alp_alpOut[ 2 ] * popParCovOut[ 1 ], 1.0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 1 ] * popParCovOut[ 0 ] +
					lTilde_alp_alpOut[ 3 ] * popParCovOut[ 1 ], 0.0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 0 ] * popParCovOut[ 2 ] +
					lTilde_alp_alpOut[ 2 ] * popParCovOut[ 3 ], 0.0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 1 ] * popParCovOut[ 2 ] +
					lTilde_alp_alpOut[ 3 ] * popParCovOut[ 3 ], 1.0, eps );

	}
      //------------------------------------------------------------
      // Compute S.
      //------------------------------------------------------------
      
      if( form == RSR || form == S )
	{
	  DoubleMatrix dvecN( nInd, 1 );
	  double * pN = dvecN.data();
	  for( int i=0; i<nInd; i++ )
	    pN[i] = N[i];
	  DoubleMatrix dvecY( Y );
	  DoubleMatrix dvecAlpLow( alpLow );
	  DoubleMatrix dvecAlpUp( alpUp );
	  DoubleMatrix dvecAlpIn( alpOut );
	  DoubleMatrix dvecAlpStep( alpStep );
	  DoubleMatrix dvecBLow( bLow );
	  DoubleMatrix dvecBUp( bUp );
	  DoubleMatrix dmatBIn( bOut, nInd );
	  DoubleMatrix dvecBStep( bStep );
	  DoubleMatrix dmatLambdaTilde_alpOut( nAlp, nInd );
	  
          // Calculate the derivatives of each individual's
          // contribution to the population objective function.
	  if( whichObjective != FIRST_ORDER )
	    {
	      lTilde( model, 
		      whichObjective, 
		      dvecY, 
		      dvecN,
		      indOptimizer,
		      dvecAlpIn,
		      dvecBLow,
		      dvecBUp,
		      dvecBStep,
		      dmatBIn,
		      0,
		      0, 
		      0, 
		      &dmatLambdaTilde_alpOut );
	    }
	  else
	    {
	      firstOrderOpt(
			    model,
			    modelAD,
			    dvecN,
			    dvecY,
			    popOptimizer,
			    dvecAlpLow,
			    dvecAlpUp,
			    dvecAlpIn,
			    0,
			    dvecAlpStep,
			    indOptimizer,
			    dvecBLow,
			    dvecBUp,
			    dmatBIn,
			    0,
			    dvecBStep,
			    0,
			    0,
			    0,
			    &dmatLambdaTilde_alpOut );
	    }
	  valarray<double> lambdaTilde_alpOut = dmatLambdaTilde_alpOut.toValarray();

	  valarray<double> s( 0.0, nAlp * nAlp );
	  double* pdmatLambdaTilde_alpOut = dmatLambdaTilde_alpOut.data();
          for( int j=0; j<nInd; j++ )
          {
             for( int i=0; i<nAlp; i++ )
             {
                for( int k=0; k<nAlp; k++ )
                {
                   s[ k+i*nAlp ] += pdmatLambdaTilde_alpOut[ i+j*nAlp ] * pdmatLambdaTilde_alpOut[ k + j*nAlp ];
                }
             }
          }

	  //------------------------------------------------------------
	  // Test popParCovOut formulation S.
	  //------------------------------------------------------------
	  
	  if( form == S )
	    {
	      CPPUNIT_ASSERT_DOUBLES_EQUAL( s[ 0 ] * popParCovOut[ 0 ] +
			                    s[ 2 ] * popParCovOut[ 1 ], 1.0, eps );
	      CPPUNIT_ASSERT_DOUBLES_EQUAL( s[ 1 ] * popParCovOut[ 0 ] +
			                    s[ 3 ] * popParCovOut[ 1 ], 0.0, eps );
	      CPPUNIT_ASSERT_DOUBLES_EQUAL( s[ 0 ] * popParCovOut[ 2 ] +
			                    s[ 2 ] * popParCovOut[ 3 ], 0.0, eps );
	      CPPUNIT_ASSERT_DOUBLES_EQUAL( s[ 1 ] * popParCovOut[ 2 ] +
			                    s[ 3 ] * popParCovOut[ 3 ], 1.0, eps );
	    }
	  
	  //------------------------------------------------------------
	  // Test popParCovOut formulation RSR.
	  //------------------------------------------------------------
	  
	  if( form == RSR )
	    {
	      double detR = lTilde_alp_alpOut[ 0 ] * lTilde_alp_alpOut[ 3 ] -
		lTilde_alp_alpOut[ 2 ] * lTilde_alp_alpOut[ 1 ];
	      
	      CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 0 ] * popParCovOut[ 0 ] + 
			                    lTilde_alp_alpOut[ 2 ] * popParCovOut[ 1 ], 
					    ( s[ 0 ] * lTilde_alp_alpOut[ 3 ] - 
					      s[ 2 ] * lTilde_alp_alpOut[ 1 ] ) / detR, eps );
	      
	      CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 1 ] * popParCovOut[ 0 ] +
			                    lTilde_alp_alpOut[ 3 ] * popParCovOut[ 1 ], 
					    ( s[ 1 ] * lTilde_alp_alpOut[ 3 ] - 
					      s[ 3 ] * lTilde_alp_alpOut[ 1 ] ) / detR, eps );
	      
	      CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 0 ] * popParCovOut[ 2 ] +
			                    lTilde_alp_alpOut[ 2 ] * popParCovOut[ 3 ], 
					    ( -s[ 0 ] * lTilde_alp_alpOut[ 2 ] + 
					      s[ 2 ] * lTilde_alp_alpOut[ 0 ] ) / detR, eps );
	      
	      CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 1 ] * popParCovOut[ 2 ] +
			                    lTilde_alp_alpOut[ 3 ] * popParCovOut[ 3 ], 
					    ( -s[ 1 ] * lTilde_alp_alpOut[ 2 ] + 
					      s[ 3 ] * lTilde_alp_alpOut[ 0 ] ) / detR, eps );
	    }
	  
	  //------------------------------------------------------------
	  // Test popParSeOut.
	  //------------------------------------------------------------
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParSEOut[ 0 ], sqrt( popParCovOut[ 0 ] ), eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParSEOut[ 1 ], sqrt( popParCovOut[ 3 ] ), eps );
	  
	  //------------------------------------------------------------
	  // Test popParCorOut.
	  //------------------------------------------------------------
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCorOut[ 0 ], 1.0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCorOut[ 1 ], popParCovOut[ 1 ] /
			                popParSEOut[ 0 ] / popParSEOut[ 1 ], eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCorOut[ 2 ], popParCorOut[ 1 ], eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCorOut[ 3 ], 1.0, eps );
	  
	  //------------------------------------------------------------
	  // Test popParCVOut.
	  //------------------------------------------------------------
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCVOut[ 0 ], popParSEOut[ 0 ] / alpOut[ 0 ] * 100., eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCVOut[ 1 ], popParSEOut[ 1 ] / alpOut[ 1 ] * 100., eps );
	  
	  //------------------------------------------------------------
	  // Test popParCIOut.
	  //------------------------------------------------------------
	  double d0 = 2.306 * popParSEOut[ 0 ];
	  double d1 = 2.306 * popParSEOut[ 1 ];
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCIOut[ 0 ], alpOut[ 0 ] - d0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCIOut[ 1 ], alpOut[ 1 ] - d1, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCIOut[ 2 ], alpOut[ 0 ] + d0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCIOut[ 3 ], alpOut[ 1 ] + d1, eps );      
	}
    }
}
void popStatisticsTest::coreStatTest(enum Objective whichObjective)
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int i;

  //preTestPrinting( "Specification Example" );


  // Number of individuals.
  const int nInd = 10;

  // Number of measurements per individual (same for all)
  const int nYi = 1;

  // Number of measurements in total
  const int nY = nInd * nYi;

  const int nAlp = 2;

  const int nB = 1;

  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelPopStatisticsExampleTest<double> model( nAlp, nB, nYi );

  UserModelPopStatisticsExampleTest< CppAD::AD<double> > modelAD( nAlp, nB, nYi );


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
  valarray<double> Y( nY );

  // Number of measurements for each individual. 
  valarray<int> N( 1, nInd );

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

  // Set the measurements for each individual.
  //
  // Note: these values were generated on a 32-bit Pentium machine
  // using the following code.
  //
  //     int seed = 2;
  //     srand(seed);
  //
  //     valarray<double> sdECov(nY*nY);
  //     sdECov[ slice( 0, nY, nY+1 ) ] = sdETrue;
  //
  //     valarray<double> sdBCov(nY*nY);
  //     sdBCov[ slice( 0, nY, nY+1 ) ] = sdBTrue;
  //
  //     Y = meanBTrue + randNormal( sdBCov, nY ) + randNormal( sdECov, nY );
  //
  // The values generated on a 64-bit Athalon machine were different
  // and their optimal paramter values could not be calculated.  So,
  // these values have been set explicitly here to ensure they're the
  // same on all machines.
  //
  Y[0] = 1.88758;
  Y[1] = -1.03471;
  Y[2] = 1.18851;
  Y[3] = -0.476253;
  Y[4] = -1.45167;
  Y[5] = -0.797979;
  Y[6] = -0.0825739;
  Y[7] = 3.04214;
  Y[8] = 1.48168;
  Y[9] = -1.29312;

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
  
  // Set the mask for trancating the alp vector.
  valarray<bool> alpMask( true, nAlp );


  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  valarray<double> bLow ( -1.5e+1, nB );
  valarray<double> bUp  ( +1.0e+1, nB );
  valarray<double> bStep(  1.0e-2, nB );

  valarray<double> bIn ( 1., nB * nInd );
  valarray<double> bOut(     nB * nInd );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  valarray<double> lTilde_alpOut    ( nAlp );
  valarray<double> lTilde_alp_alpOut( nAlp * nAlp );


  //------------------------------------------------------------
  // Remaining inputs to fitPopulation.
  //------------------------------------------------------------

  // Set the values associated with the individual objective function.
  Optimizer indOptimizer( 1.0e-6, 40, 0 );

  // Set the values associated with the population objective function.
  Optimizer popOptimizer( 1.0e-6, 40, 0 );

  //------------------------------------------------------------
  // Optimize the population objective function.
  //------------------------------------------------------------

  bool ok;
  try{
    fitPopulation(
		  model, 
		  modelAD, 
		  whichObjective,
		  N,
		  Y,
		  popOptimizer,
		  alpLow,
		  alpUp,
		  alpIn,
		  alpStep,
		  &alpOut,
		  indOptimizer,
		  bLow,
		  bUp,
		  bIn,            
		  bStep,
		  &bOut,
		  &dLTildeOut,
		  &lTilde_alpOut,
		  &lTilde_alp_alpOut,
                  false,
                  false
		  );
    ok = true;
  }
  catch(...)
    {
      CPPUNIT_ASSERT(false);
    }

  // Set the mask equal to false for any parameter elements that are
  // at their bounds to indicate these elements should not be included
  // in the statistics calculation.
  for ( i = 0; i < nAlp; i++ )
  {
    alpMask[i] = !( alpLow[i] == alpUp[i]
                    ||
                    ( fabs( ( alpOut[i] - alpLow[i] ) / alpLow[i] ) <  popOptimizer.getEpsilon() )
                    ||
                    ( fabs( ( alpUp[i]  - alpOut[i] ) / alpUp[i]  ) <  popOptimizer.getEpsilon() ) );
  }


  //------------------------------------------------------------
  // Obtain the partial derivative of the individual
  // objective with respect to alp.
  //------------------------------------------------------------
  DoubleMatrix dvecN   ( nInd, 1 );
  for( int i=0; i<nInd; i++ )
    dvecN.data()[i] = N[i];
  DoubleMatrix dvecY      ( Y,       1 );
  DoubleMatrix dmatBIn   ( bOut,    nInd );
  DoubleMatrix dvecBLow   ( bLow,    1 );
  DoubleMatrix dvecBUp    ( bUp,     1 );
  DoubleMatrix dvecBStep  ( bStep,   1 );
  DoubleMatrix dvecAlpIn  ( alpOut,  1 );
  DoubleMatrix dvecAlpUp  ( alpUp,   1 );
  DoubleMatrix dvecAlpLow ( alpLow,  1 );
  DoubleMatrix dvecAlpStep( alpStep, 1 );
  DoubleMatrix dmatLambdaTilde_alpOut( nAlp, nInd );

  valarray<double> lambdaTilde_alpOut( nAlp*nInd );

  try{
    // Calculate the derivatives of each individual's
    // contribution to the population objective function.
    if( whichObjective != FIRST_ORDER )
      {
	lTilde( model,
		whichObjective,
		dvecY,
		dvecN,
		indOptimizer,
		dvecAlpIn,
		dvecBLow,
		dvecBUp,
		dvecBStep,
		dmatBIn,
		0,
		0,
		0,
		&dmatLambdaTilde_alpOut );
      }
    else
      {	
	firstOrderOpt(
		      model,
		      modelAD,
		      dvecN,
		      dvecY,
		      popOptimizer,
		      dvecAlpLow,
		      dvecAlpUp,
		      dvecAlpIn,
		      0,
		      dvecAlpStep,
		      indOptimizer,
		      dvecBLow,
		      dvecBUp,
		      dmatBIn,
		      0,
		      dvecBStep,
		      0,
		      0,
		      0,
		      &dmatLambdaTilde_alpOut );
      }
  }
  catch( ... )
    {
      CPPUNIT_ASSERT(false);
    }

  lambdaTilde_alpOut = dmatLambdaTilde_alpOut.toValarray();

  //------------------------------------------------------------
  // Compute statistics of population parameter estimates.
  //------------------------------------------------------------
  valarray<double> popParCovOut( nAlp * nAlp );
  valarray<double> popParSEOut ( nAlp );
  valarray<double> popParCorOut( nAlp * nAlp );
  valarray<double> popParCVOut ( nAlp );
  valarray<double> popParCIOut ( nAlp * 2 );

  for( int form = 1; form < 4; form++ )
    {
      try{
	
	popStatistics(alpMask,
		      Y,
		      alpOut,
                      lambdaTilde_alpOut,
		      lTilde_alp_alpOut,
		      static_cast<PopCovForm>( form ),
		      &popParCovOut,
		      &popParSEOut,
		      &popParCorOut,
		      &popParCVOut,
		      &popParCIOut
		      );
      }
      catch( const SpkException& e )
	{
	  cerr << e << endl;
	  CPPUNIT_ASSERT_MESSAGE( "popStatistics failed!", false );
	}
      catch(...)
	{
	  CPPUNIT_ASSERT_MESSAGE( "popStatistics failed for unknown reasons!", false);
	}
      
      double eps = 1e-5;
      
      //------------------------------------------------------------
      // Test popParCovOut formulation R.
      //------------------------------------------------------------
      
      if( form == R )             
	{
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 0 ] * popParCovOut[ 0 ] +
					lTilde_alp_alpOut[ 2 ] * popParCovOut[ 1 ], 1.0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 1 ] * popParCovOut[ 0 ] +
					lTilde_alp_alpOut[ 3 ] * popParCovOut[ 1 ], 0.0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 0 ] * popParCovOut[ 2 ] +
					lTilde_alp_alpOut[ 2 ] * popParCovOut[ 3 ], 0.0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 1 ] * popParCovOut[ 2 ] +
					lTilde_alp_alpOut[ 3 ] * popParCovOut[ 3 ], 1.0, eps );
	}
      
      //------------------------------------------------------------
      // Compute S.
      //------------------------------------------------------------
      valarray<double> s( 0.0, nAlp * nAlp );
      for( int i = 0; i < nInd * 2; i += 2 )
	{
	  s[ 0 ] += lambdaTilde_alpOut[ i ]     * lambdaTilde_alpOut[ i ];
	  s[ 1 ] += lambdaTilde_alpOut[ i ]     * lambdaTilde_alpOut[ i + 1 ];
	  s[ 2 ] += lambdaTilde_alpOut[ i + 1 ] * lambdaTilde_alpOut[ i ];
	  s[ 3 ] += lambdaTilde_alpOut[ i + 1 ] * lambdaTilde_alpOut[ i + 1 ];
	}
      //------------------------------------------------------------
      // Test popParCovOut formulation S.
      //------------------------------------------------------------
	  
      if( form == S )
	{
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( s[ 0 ] * popParCovOut[ 0 ] +
					s[ 2 ] * popParCovOut[ 1 ], 1.0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( s[ 1 ] * popParCovOut[ 0 ] +
					s[ 3 ] * popParCovOut[ 1 ], 0.0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( s[ 0 ] * popParCovOut[ 2 ] +
					s[ 2 ] * popParCovOut[ 3 ], 0.0, eps );
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( s[ 1 ] * popParCovOut[ 2 ] +
					s[ 3 ] * popParCovOut[ 3 ], 1.0, eps );
	}
	  
      //------------------------------------------------------------
      // Test popParCovOut formulation RSR.
      //------------------------------------------------------------
	  
      if( form == RSR )
	{
	  double detR = lTilde_alp_alpOut[ 0 ] * lTilde_alp_alpOut[ 3 ] -
	    lTilde_alp_alpOut[ 2 ] * lTilde_alp_alpOut[ 1 ];
	      
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 0 ] * popParCovOut[ 0 ] + 
					lTilde_alp_alpOut[ 2 ] * popParCovOut[ 1 ], 
					( s[ 0 ] * lTilde_alp_alpOut[ 3 ] - 
					  s[ 2 ] * lTilde_alp_alpOut[ 1 ] ) / detR, eps );
	      
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 1 ] * popParCovOut[ 0 ] +
					lTilde_alp_alpOut[ 3 ] * popParCovOut[ 1 ], 
					( s[ 1 ] * lTilde_alp_alpOut[ 3 ] - 
					  s[ 3 ] * lTilde_alp_alpOut[ 1 ] ) / detR, eps );
	      
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 0 ] * popParCovOut[ 2 ] +
					lTilde_alp_alpOut[ 2 ] * popParCovOut[ 3 ], 
					( -s[ 0 ] * lTilde_alp_alpOut[ 2 ] + 
					  s[ 2 ] * lTilde_alp_alpOut[ 0 ] ) / detR, eps );
	      
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 1 ] * popParCovOut[ 2 ] +
					lTilde_alp_alpOut[ 3 ] * popParCovOut[ 3 ], 
					( -s[ 1 ] * lTilde_alp_alpOut[ 2 ] + 
					  s[ 3 ] * lTilde_alp_alpOut[ 0 ] ) / detR, eps );
	}
      //------------------------------------------------------------
      // Test popParSeOut.
      //------------------------------------------------------------
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParSEOut[ 0 ], sqrt( popParCovOut[ 0 ] ), eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParSEOut[ 1 ], sqrt( popParCovOut[ 3 ] ), eps );
	  
      //------------------------------------------------------------
      // Test popParCorOut.
      //------------------------------------------------------------
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCorOut[ 0 ], 1.0, eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCorOut[ 1 ], popParCovOut[ 1 ] /
				    popParSEOut[ 0 ] / popParSEOut[ 1 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCorOut[ 2 ], popParCorOut[ 1 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCorOut[ 3 ], 1.0, eps );
	  
      //------------------------------------------------------------
      // Test popParCVOut.
      //------------------------------------------------------------
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCVOut[ 0 ], popParSEOut[ 0 ] / alpOut[ 0 ] * 100., eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCVOut[ 1 ], popParSEOut[ 1 ] / alpOut[ 1 ] * 100., eps );
	  
      //------------------------------------------------------------
      // Test popParCIOut.
      //------------------------------------------------------------
      double d0 = 2.306 * popParSEOut[ 0 ];
      double d1 = 2.306 * popParSEOut[ 1 ];
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCIOut[ 0 ], alpOut[ 0 ] - d0, eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCIOut[ 1 ], alpOut[ 1 ] - d1, eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCIOut[ 2 ], alpOut[ 0 ] + d0, eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCIOut[ 3 ], alpOut[ 1 ] + d1, eps );      
    }
}

#include <spk/fitPopulation.h>
#include <spk/firstOrderOpt.h>
#include <spk/NaiveFoModel.h>
#include <spk/lTilde.h>
#include <spk/randNormal.h>

template <class Scalar>
class PopStatWrapperTestModel : public SpkModel<Scalar>
{
    valarray<Scalar> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    PopStatWrapperTestModel(int nA, int nB, int nYi)
      :
      _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {};    
    ~PopStatWrapperTestModel(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<Scalar>& aval)
    {
        _a = aval;
    }
    void doSetIndPar(const valarray<Scalar>& bval)
    {
        _b = bval;
    }
    void doIndParVariance( valarray<Scalar>& ret ) const
    {
        //
        // D = [ alp[1] ]
        //
        ret.resize(_nYi);
        ret[0] = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        // D_alp = [ 0  1 ]
        //
        ret.resize(_nYi * _nA);
        ret[0] = 0.0;
        ret[1] = 1.0;
        return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
        //
        // Dinv = [ 1.0 / alp[1] ]
        //
        assert(_a[1] != 0.0);
        ret.resize(_nB * _nB);
        double a1Double;
        scalarToDouble( _a[1], a1Double );
        ret[0] = 1.0 / a1Double;
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Dinv_alp = [ 0    -alp[1]^(-2) ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        double a1Double;
        scalarToDouble( _a[1], a1Double );
        ret[1] = -1.0 / ( a1Double * a1Double );
        return true;
    }
    void doDataMean( valarray<Scalar>& ret ) const
    {
        //
        // f = [ alp[0]+b[0] ]
        //
        ret.resize(_nYi);
        ret[0] = ( _a[0] + _b[0] );
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        //
        // f_alp = [ 1   0 ]
        //
        ret.resize(_nYi * _nA);
        ret[0] = 1.0;
        ret[1] = 0.0;
        return true;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        //
        // f_b = [ 1 ]
        //
        ret.resize(_nYi * _nB);
        ret[0] = 1.0;
        return true;
    }
    void doDataVariance( valarray<Scalar>& ret ) const
    {
        //
        // R = [ 1 ]
        //
        ret.resize(_nB*_nB);
        ret[0] = 1.0;
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        //
        // R_alp = [ 0   0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        //
        // R_b = [ 0 ]
        //
        ret.resize(_nB *_nB);
        ret[0] = 0.0;
        return false;
    }
    void doDataVarianceInv( valarray<Scalar>& ret ) const
    {
        //
        // Rinv = [ 1 ]
        //
        ret.resize(_nB * _nB);
        ret[0] = 1.0;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Rinv_alp = [ 0  0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        //
        // Rinv_b = [ 0 ]
        //
        ret.resize(_nB * _nB * _nB);
        ret[0] = 0.0;
        return false;
    }   

};

void popStatisticsTest::popMaskTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int i;

  //preTestPrinting( "Specification Example" );

  Objective method = EXPECTED_HESSIAN;

  // Number of individuals.
  const int nInd = 10;

  // Number of measurements per individual (same for all)
  const int nYi = 1;

  // Number of measurements in total
  const int nY = nInd * nYi;

  // The last element will be fixed (ie. dummy).
  const int nAlp = 2 + 1;

  const int nB = 1;

  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  PopStatWrapperTestModel<double> model( nAlp, nB, nYi );

  PopStatWrapperTestModel< CppAD::AD<double> > modelAD( nAlp, nB, nYi );


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
  valarray<double> Y( nY );

  // Number of measurements for each individual. 
  valarray<int> N( 1, nInd );

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

  // Set the measurements for each individual.
  //
  // Note: these values were generated on a 32-bit Pentium machine
  // using the following code.
  //
  //     int seed = 2;
  //     srand(seed);
  //
  //     valarray<double> sdECov(nY*nY);
  //     sdECov[ slice( 0, nY, nY+1 ) ] = sdETrue;
  //
  //     valarray<double> sdBCov(nY*nY);
  //     sdBCov[ slice( 0, nY, nY+1 ) ] = sdBTrue;
  //
  //     Y = meanBTrue + randNormal( sdBCov, nY ) + randNormal( sdECov, nY );
  //
  // The values generated on a 64-bit Athalon machine were different
  // and their optimal paramter values could not be calculated.  So,
  // these values have been set explicitly here to ensure they're the
  // same on all machines.
  //
  Y[0] = 1.88758;
  Y[1] = -1.03471;
  Y[2] = 1.18851;
  Y[3] = -0.476253;
  Y[4] = -1.45167;
  Y[5] = -0.797979;
  Y[6] = -0.0825739;
  Y[7] = 3.04214;
  Y[8] = 1.48168;
  Y[9] = -1.29312;

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
 
  // Set the values associated with alp(3) which is fixed.
  alpTrue[ 2 ] = 0.5;
  alpLow [ 2 ] = 0.5;
  alpUp  [ 2 ] = 0.5;
  alpIn  [ 2 ] = 0.5;
  alpStep[ 2 ] = 0.0;

  // Set the mask for trancating the alp vector.
  // true  - keep it
  // false - fixed (ignore)
  valarray<bool> mask( true, nAlp );
  mask[ 2 ] = false;

  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  valarray<double> bLow ( -1.5e+1, nB );
  valarray<double> bUp  ( +1.0e+1, nB );
  valarray<double> bStep(  1.0e-2, nB );

  valarray<double> bIn ( 1., nB * nInd );
  valarray<double> bOut(     nB * nInd );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  valarray<double> lTilde_alpOut    ( nAlp );
  valarray<double> lTilde_alp_alpOut( nAlp * nAlp );


  //------------------------------------------------------------
  // Remaining inputs to fitPopulation.
  //------------------------------------------------------------

  // Set the values associated with the individual objective function.
  Optimizer indOptimizer( 1.0e-6, 40, 0 );

  // Set the values associated with the population objective function.
  Optimizer popOptimizer( 1.0e-6, 40, 0 );

  //------------------------------------------------------------
  // Optimize the population objective function.
  //------------------------------------------------------------

  bool ok;
  try{
    fitPopulation(
		  model, 
		  modelAD, 
		  method,
		  N,
		  Y,
		  popOptimizer,
		  alpLow,
		  alpUp,
		  alpIn,
		  alpStep,
		  &alpOut,
		  indOptimizer,
		  bLow,
		  bUp,
		  bIn,            
		  bStep,
		  &bOut,
		  &dLTildeOut,
		  &lTilde_alpOut,
		  &lTilde_alp_alpOut,
                  false,
                  false
		  );
    ok = true;
  }
  catch(...)
    {
      CPPUNIT_ASSERT(false);
    }


  //------------------------------------------------------------
  // Obtain the partial derivative of the individual
  // objective with respect to alp.
  //------------------------------------------------------------
  DoubleMatrix dvecN   ( nInd, 1 );
  for( int i=0; i<nInd; i++ )
    dvecN.data()[i] = N[i];
  DoubleMatrix dvecY      ( Y,       1 );
  DoubleMatrix dmatBIn   ( bOut,    nInd );
  DoubleMatrix dvecBLow   ( bLow,    1 );
  DoubleMatrix dvecBUp    ( bUp,     1 );
  DoubleMatrix dvecBStep  ( bStep,   1 );
  DoubleMatrix dvecAlpIn ( alpOut,  1 );
  DoubleMatrix dvecAlpUp  ( alpUp,   1 );
  DoubleMatrix dvecAlpLow ( alpLow,  1 );
  DoubleMatrix dvecAlpStep( alpStep, 1 );
  DoubleMatrix dmatLambdaTilde_alpOut( nAlp, nInd );

  valarray<double> lambdaTilde_alpOut( nAlp*nInd );

  try{
	lTilde( model,
		method,
		dvecY,
		dvecN,
		indOptimizer,
		dvecAlpIn,
		dvecBLow,
		dvecBUp,
		dvecBStep,
		dmatBIn,
		0,
		0,
		0,
		&dmatLambdaTilde_alpOut );
  }
  catch( ... )
    {
      CPPUNIT_ASSERT(false);
    }

  lambdaTilde_alpOut = dmatLambdaTilde_alpOut.toValarray();

  //------------------------------------------------------------
  // Compute statistics of population parameter estimates.
  //------------------------------------------------------------
  valarray<double> popParCovOut( nAlp * nAlp );
  valarray<double> popParSEOut ( nAlp );
  valarray<double> popParCorOut( nAlp * nAlp );
  valarray<double> popParCVOut ( nAlp );
  valarray<double> popParCIOut ( nAlp * 2 );

  for( int form = 1; form < 4; form++ )
    {
      try{
	popStatistics( mask,
		       Y,
		       alpOut,
		       lambdaTilde_alpOut,
		       lTilde_alp_alpOut,
		       static_cast<PopCovForm>( form ),
		       &popParCovOut,
		       &popParSEOut,
		       &popParCorOut,
		       &popParCVOut,
		       &popParCIOut
		       );
      }
      catch( const SpkException& e )
	{
	  cerr << e << endl;
	  CPPUNIT_ASSERT_MESSAGE( "statWrapper failed!", false );
	}
      catch(...)
	{
	  CPPUNIT_ASSERT_MESSAGE( "statWrapper failed for unknown reasons!", false);
	}
      
      double eps = 1e-5;
      
      //------------------------------------------------------------
      // Test popParCovOut formulation R.
      //------------------------------------------------------------
    
      // 
      //          /  1.0   0.0   0.0  \
      // Expect:  |  0.0   1.0   0.0  |
      //          \  0.0   0.0   0.0  /
      //                            
      if( form == R )             
	{

          CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, lTilde_alp_alpOut[ 0 ] * popParCovOut[ 0 ] +
                                             lTilde_alp_alpOut[ 3 ] * popParCovOut[ 1 ] +
                                             lTilde_alp_alpOut[ 6 ] * popParCovOut[ 2 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, lTilde_alp_alpOut[ 1 ] * popParCovOut[ 0 ] +
                                             lTilde_alp_alpOut[ 4 ] * popParCovOut[ 1 ] +
                                             lTilde_alp_alpOut[ 7 ] * popParCovOut[ 2 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, lTilde_alp_alpOut[ 2 ] * popParCovOut[ 0 ] +
                                             lTilde_alp_alpOut[ 5 ] * popParCovOut[ 1 ] +
                                             lTilde_alp_alpOut[ 8 ] * popParCovOut[ 2 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, lTilde_alp_alpOut[ 0 ] * popParCovOut[ 3 ] +
                                             lTilde_alp_alpOut[ 3 ] * popParCovOut[ 4 ] +
                                             lTilde_alp_alpOut[ 6 ] * popParCovOut[ 5 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, lTilde_alp_alpOut[ 1 ] * popParCovOut[ 3 ] +
                                             lTilde_alp_alpOut[ 4 ] * popParCovOut[ 4 ] +
                                             lTilde_alp_alpOut[ 7 ] * popParCovOut[ 5 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, lTilde_alp_alpOut[ 2 ] * popParCovOut[ 3 ] +
                                             lTilde_alp_alpOut[ 5 ] * popParCovOut[ 4 ] +
                                             lTilde_alp_alpOut[ 8 ] * popParCovOut[ 5 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, lTilde_alp_alpOut[ 0 ] * popParCovOut[ 6 ] +
                                             lTilde_alp_alpOut[ 3 ] * popParCovOut[ 7 ] +
                                             lTilde_alp_alpOut[ 6 ] * popParCovOut[ 8 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, lTilde_alp_alpOut[ 1 ] * popParCovOut[ 6 ] +
                                             lTilde_alp_alpOut[ 4 ] * popParCovOut[ 7 ] +
                                             lTilde_alp_alpOut[ 7 ] * popParCovOut[ 8 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, lTilde_alp_alpOut[ 2 ] * popParCovOut[ 6 ] +
                                             lTilde_alp_alpOut[ 5 ] * popParCovOut[ 7 ] +
                                             lTilde_alp_alpOut[ 8 ] * popParCovOut[ 8 ], eps );
        } 

      //------------------------------------------------------------
      // Compute S.
      //------------------------------------------------------------
      valarray<double> s( 0.0, nAlp * nAlp );
      for( int j=0; j<nInd; j++ )
      {
         for( int i=0; i<nAlp; i++ )
         {
            for( int k=0; k<nAlp; k++ )
               s[ k + i*nAlp ] += lambdaTilde_alpOut[ i + j*nAlp ] * lambdaTilde_alpOut[ k + j*nAlp ];
         }
      }
      //------------------------------------------------------------
      // Test popParCovOut formulation S.
      //------------------------------------------------------------
	  
      if( form == S )
	{

          CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, s[ 0 ] * popParCovOut[ 0 ] +
                                             s[ 3 ] * popParCovOut[ 1 ] +
                                             s[ 6 ] * popParCovOut[ 2 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, s[ 1 ] * popParCovOut[ 0 ] +
                                             s[ 4 ] * popParCovOut[ 1 ] +
                                             s[ 7 ] * popParCovOut[ 2 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, s[ 2 ] * popParCovOut[ 0 ] +
                                             s[ 5 ] * popParCovOut[ 1 ] +
                                             s[ 8 ] * popParCovOut[ 2 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, s[ 0 ] * popParCovOut[ 3 ] +
                                             s[ 3 ] * popParCovOut[ 4 ] +
                                             s[ 6 ] * popParCovOut[ 5 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, s[ 1 ] * popParCovOut[ 3 ] +
                                             s[ 4 ] * popParCovOut[ 4 ] +
                                             s[ 7 ] * popParCovOut[ 5 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, s[ 2 ] * popParCovOut[ 3 ] +
                                             s[ 5 ] * popParCovOut[ 4 ] +
                                             s[ 8 ] * popParCovOut[ 5 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, s[ 0 ] * popParCovOut[ 6 ] +
                                             s[ 3 ] * popParCovOut[ 7 ] +
                                             s[ 6 ] * popParCovOut[ 8 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, s[ 1 ] * popParCovOut[ 6 ] +
                                             s[ 4 ] * popParCovOut[ 7 ] +
                                             s[ 7 ] * popParCovOut[ 8 ], eps );
          CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, s[ 2 ] * popParCovOut[ 6 ] +
                                             s[ 5 ] * popParCovOut[ 7 ] + 
                                             s[ 8 ] * popParCovOut[ 5 ], eps );

	}

      //------------------------------------------------------------
      // Test popParCovOut formulation RSR.
      //------------------------------------------------------------
	  
      if( form == RSR )
	{
	  double detR = lTilde_alp_alpOut[ 0 ] * lTilde_alp_alpOut[ 4 ] -
	                lTilde_alp_alpOut[ 3 ] * lTilde_alp_alpOut[ 1 ];
	      
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( ( s[ 0 ] * lTilde_alp_alpOut[ 4 ] - 
					  s[ 3 ] * lTilde_alp_alpOut[ 1 ] ) / detR, 
	                                lTilde_alp_alpOut[ 0 ] * popParCovOut[ 0 ] + 
					lTilde_alp_alpOut[ 3 ] * popParCovOut[ 1 ], eps );
	      
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( ( s[ 1 ] * lTilde_alp_alpOut[ 4 ] - 
					  s[ 4 ] * lTilde_alp_alpOut[ 1 ] ) / detR, 
	                                lTilde_alp_alpOut[ 1 ] * popParCovOut[ 0 ] +
					lTilde_alp_alpOut[ 4 ] * popParCovOut[ 1 ], eps );
	      
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( (-s[ 0 ] * lTilde_alp_alpOut[ 3 ] + 
					  s[ 3 ] * lTilde_alp_alpOut[ 0 ] ) / detR, 
	                                lTilde_alp_alpOut[ 0 ] * popParCovOut[ 3 ] +
					lTilde_alp_alpOut[ 3 ] * popParCovOut[ 4 ], eps );
	      
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( (-s[ 1 ] * lTilde_alp_alpOut[ 3 ] + 
					  s[ 4 ] * lTilde_alp_alpOut[ 0 ] ) / detR, 
	                                lTilde_alp_alpOut[ 1 ] * popParCovOut[ 3 ] +
					lTilde_alp_alpOut[ 4 ] * popParCovOut[ 4 ], eps );	
        }

      //------------------------------------------------------------
      // Test popParSeOut.
      //------------------------------------------------------------
      CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( popParCovOut[ 0 ] ), popParSEOut[ 0 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( popParCovOut[ 4 ] ), popParSEOut[ 1 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( popParCovOut[ 7 ] ), popParSEOut[ 2 ], eps );
	  
      //------------------------------------------------------------
      // Test popParCorOut.
      //------------------------------------------------------------
      CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, popParCorOut[ 0 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCovOut[ 1 ] / popParSEOut[ 0 ] / popParSEOut[ 1 ], 
                                    popParCorOut[ 1 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, popParCorOut[ 2 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParCorOut[ 1 ], popParCorOut[ 3 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, popParCorOut[ 4 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, popParCorOut[ 5 ], eps );
	  
      //------------------------------------------------------------
      // Test popParCVOut.
      //------------------------------------------------------------
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParSEOut[ 0 ] / alpOut[ 0 ] * 100., popParCVOut[ 0 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( popParSEOut[ 1 ] / alpOut[ 1 ] * 100., popParCVOut[ 1 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                                   popParCVOut[ 2 ], eps );
	  
      //------------------------------------------------------------
      // Test popParCIOut.
      //------------------------------------------------------------
      double d0 = 2.306 * popParSEOut[ 0 ];
      double d1 = 2.306 * popParSEOut[ 1 ];
      CPPUNIT_ASSERT_DOUBLES_EQUAL( alpOut[ 0 ] - d0, popParCIOut[ 0 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( alpOut[ 1 ] - d1, popParCIOut[ 1 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,              popParCIOut[ 2 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( alpOut[ 0 ] + d0, popParCIOut[ 3 ], eps );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( alpOut[ 1 ] + d1, popParCIOut[ 4 ], eps );    
      CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,              popParCIOut[ 5 ], eps );    

    }
}

