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

#include <spk/SpkException.h>
#include <spk/Objective.h>
#include <spk/NaiveFoModel.h>
#include <spk/DirBasedParallelControls.h>
#include <spk/fitPopulation.h>

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
    suiteOfTests->addTest(new TestCaller<popStatisticsTest>("expectedHessianTest", &popStatisticsTest::expectedHessianTest));
    suiteOfTests->addTest(new TestCaller<popStatisticsTest>("firstOrderTest",      &popStatisticsTest::firstOrderTest));
    suiteOfTests->addTest(new TestCaller<popStatisticsTest>("naiveFirstOrderTest", &popStatisticsTest::naiveFirstOrderTest));

    return suiteOfTests;
}

void popStatisticsTest::modifiedLaplaceTest()
{
    statisticsExampleTest(MODIFIED_LAPLACE);
}
void popStatisticsTest::expectedHessianTest()
{
    statisticsExampleTest(EXPECTED_HESSIAN);
}
void popStatisticsTest::firstOrderTest()
{
    statisticsExampleTest(FIRST_ORDER);
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
#include <nag.h>
#include <nagg05.h>
#include <iomanip>
#include <cmath>


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

class UserModelStatisticsExampleTest : public SpkModel
{
    valarray<double> _a, _b;
	const int _nA;
    const int _nB;
    const int _nYi;
    int _i;
public:
    UserModelStatisticsExampleTest(int nA, int nB, int nYi)
      :_nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
	{};    
    ~UserModelStatisticsExampleTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& aval)
    {
        _a = aval;
    }
    void doSetIndPar(const valarray<double>& bval)
    {
        _b = bval;
    }
    void doIndParVariance( valarray<double>& ret ) const
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
        ret[0] = ( 1.0 / _a[1] );
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Dinv_alp = [ 0    -alp[1]^(-2) ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = -1.0 / (_a[1]*_a[1]);
        return true;
    }
    void doDataMean( valarray<double>& ret ) const
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
    void doDataVariance( valarray<double>& ret ) const
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
    void doDataVarianceInv( valarray<double>& ret ) const
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

  UserModelStatisticsExampleTest model( nAlp, nB, nYi );


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

  // Compute the measurements for each individual.
  Integer seed = 0;
  g05cbc(seed);
  for ( i = 0; i < nInd; i++ )
  {
    eTrue = nag_random_normal( meanETrue, sdETrue );
    bTrue = nag_random_normal( meanBTrue, sdBTrue );

    Y[ i ] = meanBetaTrue + bTrue + eTrue;
  }


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
					 parallelControls 
                   );
      ok = true;
  }
  catch(...)
  {
    CPPUNIT_ASSERT(false);
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

  try
  {
	for( int form = 1; form < 4; form++ )
	{
      popStatistics(
		             model,
				     whichObjective,
				     N,
				     Y,
				     alpOut,
	                 lTilde_alp_alpOut,
				     bOut,
			         bLow,
				     bUp,				 
				     bStep,
				     enum PopCovForm( form ),
				     &popParCovOut,
				     &popParSEOut,
				     &popParCorOut,
                     &popParCVOut,
                     &popParCIOut
			       );

	  if( whichObjective == FIRST_ORDER && form != 2 )
		  continue;
/*
	  cout << "formulation = " << form << endl;
	  cout << "popParCovOut = " << endl;
      printInMatrix( popParCovOut, nAlp );
      cout << "-----------------------" << endl;
*/
	  double eps = 1e-9;

  //------------------------------------------------------------
  // Symmetrize the matrix of second derivatives.
  //------------------------------------------------------------

  // Because the function popStatistics symmetrizes the matrix of
  // second derivatives of the population objective function that 
  // is passed in to it, this test must do the same.
  valarray<double> lTilde_alp_alpSymm( nAlp * nAlp );

  // Calculate the value for the symmetrized off diagonal elements
  // by taking their average.
  double lTilde_alp_alpOffDiag = 
    ( lTilde_alp_alpOut[ 1 ] + lTilde_alp_alpOut[ 2 ] ) / 2.0;

   // Set the elements of the symmetrized matrix.
  lTilde_alp_alpSymm[ 0 ] = lTilde_alp_alpOut[0];
  lTilde_alp_alpSymm[ 1 ] = lTilde_alp_alpOffDiag;
  lTilde_alp_alpSymm[ 2 ] = lTilde_alp_alpOffDiag;
  lTilde_alp_alpSymm[ 3 ] = lTilde_alp_alpOut[3];

  // Replace this with its symmetrized version.
  lTilde_alp_alpOut = lTilde_alp_alpSymm;


      //------------------------------------------------------------
      // Test popParCovOut formulation 2.
      //------------------------------------------------------------

	  if( form == 2 )             
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

	  if( form == 1 || form == 3 )
	  {
		DoubleMatrix dvecN( nInd, 1 );
        double * pN = dvecN.data();
        for( int i=0; i<nInd; i++ )
        pN[i] = N[i];
        DoubleMatrix dvecY( Y );
        DoubleMatrix dvecAlp( alpOut );
        DoubleMatrix dvecBLow( bLow );
        DoubleMatrix dvecBUp( bUp );
        DoubleMatrix dmatBIn( bOut, nInd );
        DoubleMatrix dvecBStep( bStep );
		DoubleMatrix dmatLTilde_alpOut( nAlp, nAlp );

		lTilde( model, 
			    whichObjective, 
				dvecY, 
				dvecN,
                indOptimizer,
				dvecAlp,
				dvecBLow,
				dvecBUp,
				dvecBStep,
				dmatBIn,
                0,
				0, 
				0, 
				&dmatLTilde_alpOut );

		valarray<double> S( 0.0, nAlp * nAlp );
		double* pdmatLTilde_alpOut = dmatLTilde_alpOut.data();
        for( int i = 0; i < nInd * 2; i += 2 )
		{
		    S[ 0 ] += pdmatLTilde_alpOut[ i ]     * pdmatLTilde_alpOut[ i ];
		    S[ 1 ] += pdmatLTilde_alpOut[ i ]     * pdmatLTilde_alpOut[ i + 1 ];
		    S[ 2 ] += pdmatLTilde_alpOut[ i + 1 ] * pdmatLTilde_alpOut[ i ];
            S[ 3 ] += pdmatLTilde_alpOut[ i + 1 ] * pdmatLTilde_alpOut[ i + 1 ];
		}

		//------------------------------------------------------------
        // Test popParCovOut formulation 3.
        //------------------------------------------------------------

		if( form == 3 )
		{
		    CPPUNIT_ASSERT_DOUBLES_EQUAL( S[ 0 ] * popParCovOut[ 0 ] +
			                    S[ 2 ] * popParCovOut[ 1 ], 1.0, eps );
	        CPPUNIT_ASSERT_DOUBLES_EQUAL( S[ 1 ] * popParCovOut[ 0 ] +
			                    S[ 3 ] * popParCovOut[ 1 ], 0.0, eps );
		    CPPUNIT_ASSERT_DOUBLES_EQUAL( S[ 0 ] * popParCovOut[ 2 ] +
			                    S[ 2 ] * popParCovOut[ 3 ], 0.0, eps );
		    CPPUNIT_ASSERT_DOUBLES_EQUAL( S[ 1 ] * popParCovOut[ 2 ] +
			                    S[ 3 ] * popParCovOut[ 3 ], 1.0, eps );
		}

        //------------------------------------------------------------
        // Test popParCovOut formulation 1.
        //------------------------------------------------------------

		if( form == 1 )
		{
			double detR = lTilde_alp_alpOut[ 0 ] * lTilde_alp_alpOut[ 3 ] -
				          lTilde_alp_alpOut[ 2 ] * lTilde_alp_alpOut[ 1 ];

            CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 0 ] * popParCovOut[ 0 ] + 
			                    lTilde_alp_alpOut[ 2 ] * popParCovOut[ 1 ], 
								( S[ 0 ] * lTilde_alp_alpOut[ 3 ] - 
								  S[ 2 ] * lTilde_alp_alpOut[ 1 ] ) / detR, eps );

	        CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 1 ] * popParCovOut[ 0 ] +
			                    lTilde_alp_alpOut[ 3 ] * popParCovOut[ 1 ], 
								( S[ 1 ] * lTilde_alp_alpOut[ 3 ] - 
								  S[ 3 ] * lTilde_alp_alpOut[ 1 ] ) / detR, eps );

		    CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 0 ] * popParCovOut[ 2 ] +
			                    lTilde_alp_alpOut[ 2 ] * popParCovOut[ 3 ], 
								( -S[ 0 ] * lTilde_alp_alpOut[ 2 ] + 
								  S[ 2 ] * lTilde_alp_alpOut[ 0 ] ) / detR, eps );

		    CPPUNIT_ASSERT_DOUBLES_EQUAL( lTilde_alp_alpOut[ 1 ] * popParCovOut[ 2 ] +
			                    lTilde_alp_alpOut[ 3 ] * popParCovOut[ 3 ], 
								( -S[ 1 ] * lTilde_alp_alpOut[ 2 ] + 
								  S[ 3 ] * lTilde_alp_alpOut[ 0 ] ) / detR, eps );
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
  catch(...)
  {
    CPPUNIT_ASSERT(false);
  }
}

