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
 * File: indStatisticsTest.cpp
 *
 *
 * Unit test for the function indStatistics.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/
#include <iomanip>
#include <cmath>
#include <iostream>
#include <string>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/SpkException.h"
#include "../../../spk/fitIndividual.h"
#include "../../../spk/indStatistics.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/inverse.h"
#include "../../../spk/multiply.h"
#include "../../../spk/transpose.h"
//#include "printInMatrix.h"

#include "indStatisticsTest.h"

/*------------------------------------------------------------------------
 * Namespace Declarations
 *------------------------------------------------------------------------*/

using namespace std;
using namespace CppUnit;

void indStatisticsTest::setUp()
{
    // initializations
}
void indStatisticsTest::tearDown()
{
    // clean up
}

Test* indStatisticsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "indStatisticsTest" );
  
  suiteOfTests->addTest( new TestCaller<indStatisticsTest>(
			 "statisticsExampleTest", &indStatisticsTest::statisticsExampleTest) );
  suiteOfTests->addTest( new TestCaller<indStatisticsTest>(
			 "indMaskTest", &indStatisticsTest::indMaskTest) );
  suiteOfTests->addTest( new TestCaller<indStatisticsTest>(
			 "modelBasedInterface_R_formulation_withD_falseTest",
                         &indStatisticsTest::modelBasedInterface_R_formulation_withD_falseTest) );
  suiteOfTests->addTest( new TestCaller<indStatisticsTest>(
			 "modelBasedInterface_H_formulation_withD_falseTest",
                         &indStatisticsTest::modelBasedInterface_H_formulation_withD_falseTest) );
  suiteOfTests->addTest( new TestCaller<indStatisticsTest>(
			 "modelBasedInterface_S_formulation_withD_falseTest",
                         &indStatisticsTest::modelBasedInterface_S_formulation_withD_falseTest) );
  suiteOfTests->addTest( new TestCaller<indStatisticsTest>(
			 "modelBasedInterface_R_formulation_withD_trueTest",
                         &indStatisticsTest::modelBasedInterface_R_formulation_withD_trueTest) );
  suiteOfTests->addTest( new TestCaller<indStatisticsTest>(
			 "modelBasedInterface_H_formulation_withD_trueTest",
                         &indStatisticsTest::modelBasedInterface_H_formulation_withD_trueTest) );
  suiteOfTests->addTest( new TestCaller<indStatisticsTest>(
			 "modelBasedInterface_S_formulation_withD_trueTest",
                         &indStatisticsTest::modelBasedInterface_S_formulation_withD_trueTest) );
  
  return suiteOfTests;
}

/*************************************************************************
 *
 * Function: statisticsExampleTest
 *
 *
 * This test implements the example problem from the popStatistics specification. 
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Definition
 *------------------------------------------------------------------------*/

class UserModelIndStatisticsExampleTest : public SpkModel<double>
{
    valarray<double> _b;
    int _nY;
public:
    UserModelIndStatisticsExampleTest(int nB, int nY ): _b(nB), _nY(nY){};    
    ~UserModelIndStatisticsExampleTest(){};
private:
    void doSetIndPar(const valarray<double>& bval)
    {
        _b = bval;
    }
    void doDataMean( valarray<double>& ret ) const
    {
//
//            / b(2) \ 
//     f(b) = | b(2) |   
//            \ b(2) /
//
        ret.resize( 3, _b[1] );
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
//
//              / 0   1 \ 
//     f_b(b) = | 0   1 |   
//              \ 0   1 /
//
        ret.resize( 6, 0.0 );
        ret[3] = 1.0;
        ret[4] = 1.0;
        ret[5] = 1.0;

        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
//
//            /  exp[b(1)]     0         0     \ 
//     R(b) = |      0     exp[b(1)]     0     |   
//            \      0         0     exp[b(1)] / 
//
	ret.resize( 9, 0.0 );
        ret[0] = exp( _b[0] );
        ret[4] = exp( _b[0] );
        ret[8] = exp( _b[0] );
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
//
//              /  exp[b(1)]     0  \ 
//     R_b(b) = |  0             0  |   
//              |  0             0  | 
//              |  0             0  |
//              |  exp[b(1)]     0  | 
//              |  0             0  |   
//              |  0             0  | 
//		        |  0             0  |
//              \  exp[b(1)]     0  / 
//
        ret.resize( 18, 0.0 );
        ret[0] = exp( _b[0] );
        ret[4] = exp( _b[0] );
        ret[8] = exp( _b[0] );

        return true;
    }   
    void doIndParVariance( valarray<double>& ret ) const
    {
//
//         /  1.0     0   \ 
//     D = |              |   
//         \  0       0.5 / 
//
	ret.resize( 4 );
        ret[0] = 1.0;
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 0.5;
    }
  UserModelIndStatisticsExampleTest(){};
};

void indStatisticsTest::statisticsExampleTest()
{
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    using namespace std;

    //------------------------------------------------------------
    // Quantities related to the data vector, y.
    //------------------------------------------------------------

    int nY = 3;
    valarray<double> Y( nY );
    Y[ 0 ] = 1.8;
    Y[ 1 ] = 2.0;
    Y[ 2 ] = 2.2;

    //------------------------------------------------------------
    // Quantities related to the objective function parameter, b.
    //------------------------------------------------------------

    int nB = 2;

    valarray<double> indParLow ( -4.0, nB );
    valarray<double> indParUp  (  4.0, nB );
    valarray<double> indParIn  (  2.0, nB );
    valarray<double> indParOut (       nB );
    valarray<double> indParStep( .001, nB );


    //------------------------------------------------------------
    // Quantities related to the user-provided model.
    //------------------------------------------------------------

    UserModelIndStatisticsExampleTest model( nB, nY );

    //------------------------------------------------------------
    // Quantities related to the objective function, MapObj(b).
    //------------------------------------------------------------

    double MapObjOut;

    valarray<double> MapObj_bOut  ( nB );
    valarray<double> MapObj_b_bOut( nB * nB );


    //------------------------------------------------------------
    // Remaining inputs to fitIndividual.
    //------------------------------------------------------------

    Optimizer indOptimizer( 1.e-3, 40, 0 );
    bool withD      = true;

    //------------------------------------------------------------
    // Optimize MapObj(b).
    //------------------------------------------------------------

    try
	{
		fitIndividual( model,
                       Y,
                       indOptimizer,
                       indParLow,
                       indParUp,
                       indParIn,
                       indParStep,
                       &indParOut,            
                       &MapObjOut,
                       &MapObj_bOut,
                       &MapObj_b_bOut,
                       withD );
	}
    catch(...)
	{
	  CPPUNIT_ASSERT(false);
	}

    //------------------------------------------------------------
    // Compute statistics of individual parameter estimates.
    //------------------------------------------------------------

    valarray<double> indParFinalEstimate = indParOut;
    valarray<double> dataMean_indParFinalEstimate( nY * nB );
    valarray<double> dataVariance_indParFinalEstimate( nY * nY * nB );
    valarray<double> dataVarianceInvFinalEstimate( nY * nY );

    model.setIndPar( indParFinalEstimate );
    model.dataMean_indPar( dataMean_indParFinalEstimate );
    model.dataVariance_indPar( dataVariance_indParFinalEstimate );
    model.dataVarianceInv( dataVarianceInvFinalEstimate );

    valarray<double> indParCovOut( nB * nB );
    valarray<double> indParSEOut ( nB      );
    valarray<double> indParCorOut( nB * nB );
    valarray<double> indParCVOut ( nB      );
    valarray<double> indParCIOut ( nB *  2 ); 

    try
      {
	indStatistics( indParOut,
		       dataMean_indParFinalEstimate,
		       dataVariance_indParFinalEstimate,
		       dataVarianceInvFinalEstimate,
		       &indParCovOut,
                       &indParSEOut,                          
                       &indParCorOut,
                       &indParCVOut,
	               &indParCIOut );

    /*
    cout << "indParOut = " << endl;
    printInMatrix( indParOut, 1 );
    cout << "indParCovOut = " << endl;
    printInMatrix( indParCovOut, nB );
    cout << "indParSEOut = " << endl;
    printInMatrix( indParSEOut, 1 );
    cout << "indParCorOut = " << endl;
    printInMatrix( indParCorOut, nB );
    cout << "indParCVOut = " << endl;
    printInMatrix( indParCVOut, 1 );
    cout << "indParCIOut = " << endl;
    printInMatrix( indParCIOut, 2 );
    cout << "-----------------------" << endl;
    */
    }
    catch( const SpkException& e )
      {
	cerr << e << endl;
	CPPUNIT_ASSERT_MESSAGE(false, "indStatistics");
      }
    catch(...)
      {
	CPPUNIT_ASSERT_MESSAGE(false, "Unknown error from indStatistics()" );
      }
    
    double eps = 0.000000001;

	//------------------------------------------------------------
    // Test indParCovOut.
    //------------------------------------------------------------

	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCovOut[ 0 ], 2./3., eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCovOut[ 1 ], 0., eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCovOut[ 2 ], 0., eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCovOut[ 3 ], 1./3.*exp( indParOut[ 0 ] ), eps );

    //------------------------------------------------------------
    // Test indParSEOut.
    //------------------------------------------------------------

	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParSEOut[ 0 ], sqrt( indParCovOut[ 0 ] ), eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParSEOut[ 1 ], sqrt( indParCovOut[ 3 ] ), eps );

    //------------------------------------------------------------
    // Test indParCVOut.
    //------------------------------------------------------------

	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCVOut[ 0 ], fabs(indParSEOut[ 0 ] / indParOut[ 0 ]) * 100., eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCVOut[ 1 ], fabs(indParSEOut[ 1 ] / indParOut[ 1 ]) * 100., eps );

    //------------------------------------------------------------
    // Test indParCorOut.
    //------------------------------------------------------------

	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCorOut[ 0 ], 1., eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCorOut[ 1 ], 0., eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCorOut[ 2 ], 0., eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCorOut[ 3 ], 1., eps );

    //------------------------------------------------------------
    // Test indParCIOut.
    //------------------------------------------------------------

    double d0 = 12.706 * indParSEOut[ 0 ];
    double d1 = 12.706 * indParSEOut[ 1 ];
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCIOut[ 0 ], indParOut[ 0 ] - d0, eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCIOut[ 1 ], indParOut[ 1 ] - d1, eps );
    CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCIOut[ 2 ], indParOut[ 0 ] + d0, eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCIOut[ 3 ], indParOut[ 1 ] + d1, eps );
}


/*************************************************************************
 *
 * Function: indMaskTest
 *
 *
 * This test checks that the mask for individual parameters works.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Definition
 *------------------------------------------------------------------------*/

class IndMaskTestUserModel : public SpkModel<double>
{
  valarray<double> b;
  const int nY;  // 4
  const int nB;  // 3
public:
  IndMaskTestUserModel(): nB(3), b(3), nY(4)
  {}    
  ~IndMaskTestUserModel()
  {}
private:
  void doSetIndPar( const valarray<double>& bval )
  {
    assert( bval.size() == nB );
    b = bval;
  }
  void doDataMean( valarray<double>& fOut ) const
  {
    //
    //            / b(2) \ 
    //     f(b) = | b(2) |   
    //            \ b(2) /
    //
    fOut.resize( nY, b[1] );
  }
  bool doDataMean_indPar( valarray<double>& f_bOut ) const
  {
    //
    //              / 0   1   0 \ 
    //     f_b(b) = | 0   1   0 |   
    //              \ 0   1   0 /
    //
    f_bOut.resize( nY*nB, 0.0 );
    f_bOut[ slice( nY, nY, 1 ) ] = 1.0;
    return true;
  }
  void doDataVariance( valarray<double>& ROut ) const
  {
    //
    //            /  exp[b(1)]     0         0      \ 
    //     R(b) = |      0     exp[b(1)]     0      |   
    //            \      0         0     exp[b(1)]  / 
    //
    ROut.resize( nY*nY, 0.0 );
    ROut[ slice( 0, nY, nY + 1 ) ] = exp( b[0] );
  }
  bool doDataVariance_indPar( valarray<double>& R_bOut ) const
  {
    //
    //              /  exp[b(1)]     0    0  \ 
    //     R_b(b) = |  0             0    0  |   
    //              |  0             0    0  | 
    //              |  0             0    0  |
    //              |  exp[b(1)]     0    0  | 
    //              |  0             0    0  |   
    //              |  0             0    0  | 
    //                    |  0             0    0  |
    //              \  exp[b(1)]     0    0  / 
    //
    R_bOut.resize( nY*nY*nB, 0.0 );
    R_bOut[ slice( 0, nY, nY+1 ) ] = exp( b[0] );
    return true;
  }   
};

void indStatisticsTest::indMaskTest()
{
  const int nB = 3;
  const int nY = 4;

  const int nFreedom = nY - nB;

  double yIn[] = { 1.8, 2.0, 2.2, 2.4 };
  valarray<double> y( yIn, nY );

  // Fix the last element of b.
  bool maskIn[] = { true, true, false };
  valarray<bool> mask( maskIn, nB );

  valarray<double> bLow ( -4.0, nB );
  valarray<double> bUp  (  4.0, nB );
  valarray<double> bIn  (  2.0, nB );
  valarray<double> bOut (       nB );
  valarray<double> bStep( .001, nB );

  // Fix the 3rd (last) element of b.
  bLow[2] = bIn[2];
  bUp [2] = bIn[2];

  IndMaskTestUserModel model;
  
  double MapObjOut;
  valarray<double> MapObj_bOut  ( nB );
  valarray<double> MapObj_b_bOut( nB * nB );
  Optimizer indOptimizer( 1.e-3, 40, 0 );

  try{
    fitIndividual( model,
                   y,
                   indOptimizer,
                   bLow,
                   bUp,
                   bIn,
                   bStep,
                   &bOut,            
                   &MapObjOut,
                   &MapObj_bOut,
                   &MapObj_b_bOut,
                   false );
  }
  catch( const SpkException & e )
    {
      cerr << e << endl;
      throw;
    }

  valarray<double> f_b( nY * nB );
  valarray<double> R_b( nY * nY * nB );
  valarray<double> RInv( nY * nY );

  model.setIndPar( bOut );
  model.dataMean_indPar( f_b );
  model.dataVariance_indPar( R_b );
  model.dataVarianceInv( RInv );

  valarray<double> bCovOut( nB * nB );
  valarray<double> bSEOut ( nB );
  valarray<double> bCorOut( nB * nB );
  valarray<double> bCVOut ( nB );
  valarray<double> bCIOut ( nB * 2 );

  indStatistics( mask, bOut, f_b, R_b, RInv, &bCovOut, &bSEOut, &bCorOut, &bCVOut, &bCIOut );

  double eps = 0.000000001;

  //------------------------------------------------------------
  // Test bCovOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0/2.0,                  bCovOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0/4.0*exp( bOut[ 0 ] ), bCovOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bSEOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 0 ] ), bSEOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 4 ] ), bSEOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                  bSEOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCVOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 0 ] / bOut[ 0 ] ) * 100., bCVOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 1 ] / bOut[ 1 ] ) * 100., bCVOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                                    bCVOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCorOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 1 ] / ( bSEOut[ 1 ] * bSEOut[ 0 ] ), bCorOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 3 ] / ( bSEOut[ 0 ] * bSEOut[ 1 ] ), bCorOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bCIOut.
  //------------------------------------------------------------
  // The degree of freedom for this particular problem is
  // 2 because one of the element in b is fixed so that
  // from indStatistics()'s point of view, there's only 3-1=2
  // elements in b.  
  // For degree of freedom = 2, the t-critical value for the 
  // 95% confidence level is 4.303.
  double d0 = 4.303 * bSEOut[ 0 ];
  double d1 = 4.303 * bSEOut[ 1 ];

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] - d0, bCIOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] - d1, bCIOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 2 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] + d0, bCIOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] + d1, bCIOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 5 ], eps );
}


/*************************************************************************
 *
 * Class: modelBasedInterfaceTestUserModel
 *
 *
 * This classs is used to test the new interface to indStatistics.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Definition
 *------------------------------------------------------------------------*/

class ModelBasedInterfaceTestUserModel : public SpkModel<double>
{
  valarray<double> b;
  const int nY;  // 4
  const int nB;  // 3
public:
  ModelBasedInterfaceTestUserModel(): nB(3), b(3), nY(4)
  {}    
  ~ModelBasedInterfaceTestUserModel()
  {}
private:
  void doSetIndPar( const valarray<double>& bval )
  {
    assert( bval.size() == nB );
    b = bval;
  }
  void doDataMean( valarray<double>& fOut ) const
  {
    //
    //            / b(2) \ 
    //     f(b) = | b(2) |   
    //            \ b(2) /
    //
    fOut.resize( nY, b[1] );
  }
  bool doDataMean_indPar( valarray<double>& f_bOut ) const
  {
    //
    //              / 0   1   0 \ 
    //     f_b(b) = | 0   1   0 |   
    //              \ 0   1   0 /
    //
    f_bOut.resize( nY*nB, 0.0 );
    f_bOut[ slice( nY, nY, 1 ) ] = 1.0;
    return true;
  }
  void doDataVariance( valarray<double>& ROut ) const
  {
    //
    //            /  exp[b(1)]     0         0      \ 
    //     R(b) = |      0     exp[b(1)]     0      |   
    //            \      0         0     exp[b(1)]  / 
    //
    ROut.resize( nY*nY, 0.0 );
    ROut[ slice( 0, nY, nY + 1 ) ] = exp( b[0] );
  }
  bool doDataVariance_indPar( valarray<double>& R_bOut ) const
  {
    //
    //              /  exp[b(1)]     0    0  \ 
    //     R_b(b) = |  0             0    0  |   
    //              |  0             0    0  | 
    //              |  0             0    0  |
    //              |  exp[b(1)]     0    0  | 
    //              |  0             0    0  |   
    //              |  0             0    0  | 
    //                    |  0             0    0  |
    //              \  exp[b(1)]     0    0  / 
    //
    R_bOut.resize( nY*nY*nB, 0.0 );
    R_bOut[ slice( 0, nY, nY+1 ) ] = exp( b[0] );
    return true;
  }   
  void doIndParVariance( valarray<double>& ret ) const
  {
    //
    //         /     1.0        0         0      \ 
    //     D = |      0        0.5        0      |   
    //         \      0         0        7.39    / 
    //
    ret.resize( nB * nB );
    ret = 0.0;
    ret[0 + 0 * nB] = 1.0;
    ret[1 + 1 * nB] = 0.5;
    ret[2 + 2 * nB] = 7.39;
  }
};


/*************************************************************************
 *
 * Function: modelBasedInterface_R_formulation_withD_falseTest
 *
 *
 * This test checks the new interface to indStatistics.
 *
 *************************************************************************/

void indStatisticsTest::modelBasedInterface_R_formulation_withD_falseTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Set the formulation used to calculate the covariance of the
  // parameter estimates.
  IndCovForm formulation = R;

  // Set this flag to indicate that the Map Bayesian objective
  // function was not used to calculate the parameter estimates.
  bool withD = false;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  const int nY = 4;

  double yIn[] = { 1.8, 2.0, 2.2, 2.4 };
  valarray<double> y( yIn, nY );


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  const int nB = 3;

  // Fix the last element of b.
  bool maskIn[] = { true, true, false };
  valarray<bool> mask( maskIn, nB );

  valarray<double> bLow ( -4.0, nB );
  valarray<double> bUp  (  4.0, nB );
  valarray<double> bIn  (  2.0, nB );
  valarray<double> bOut (       nB );
  valarray<double> bStep( .001, nB );

  // Fix the 3rd (last) element of b.
  bLow[2] = bIn[2];
  bUp [2] = bIn[2];


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  ModelBasedInterfaceTestUserModel model;
  

  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double MapObjOut;
  valarray<double> MapObj_bOut  ( nB );
  valarray<double> MapObj_b_bOut( nB * nB );

  //------------------------------------------------------------
  // Remaining inputs to fitIndividual.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.e-3, 40, 0 );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try{
    fitIndividual( model,
                   y,
                   indOptimizer,
                   bLow,
                   bUp,
                   bIn,
                   bStep,
                   &bOut,            
                   &MapObjOut,
                   &MapObj_bOut,
                   &MapObj_b_bOut,
                   withD );
  }
  catch( const SpkException & e )
    {
      cerr << e << endl;
      throw;
    }


  //------------------------------------------------------------
  // Compute statistics of individual parameter estimates.
  //------------------------------------------------------------

  valarray<double> bCovOut( nB * nB );
  valarray<double> bSEOut ( nB );
  valarray<double> bCorOut( nB * nB );
  valarray<double> bCVOut ( nB );
  valarray<double> bCIOut ( nB * 2 );

  indStatistics(
    model,
    y,
    bOut,
    mask,
    MapObj_b_bOut,
    formulation,
    &bCovOut,
    &bSEOut,
    &bCorOut,
    &bCVOut,
    &bCIOut,
    withD );


  //------------------------------------------------------------
  // Test bCovOut.
  //------------------------------------------------------------

  double eps = 0.000000001;

  // The covariance matrix of the individual parameter estimates is
  // calculated using one of the following formulations:
  // 
  //                                       -1     -1
  //     formulation "RSR":  cov[ b ]  =  R   S  R   ;
  // 
  //                                       -1     -1
  //     formulation "HSH":  cov[ b ]  =  H   S  H   ;
  // 
  //                                       -1
  //     formulation "R":    cov[ b ]  =  R   ;
  // 
  //                                       -1
  //     formulation "H":    cov[ b ]  =  H   ;
  // 
  //                                       -1
  //     formulation "S":    cov[ b ]  =  S   .
  // 
  // 
  // The first approximation that can be made for the information matrix is
  //
  //                 -                                     -
  //             1  |                                    T  | 
  //      R  =  --- | d  d  MapObj(b)  +  d  d  MapObj(b)   |  ,
  //             2  |  b  b                b  b             | 
  //                 -                                     -
  //
  // which is a symmetrized version of the Hessian of the individual
  // objective function MapObj(b).
  // 
  // The second approximation that can be made for the information
  // matrix is
  // 
  //             -1            T  -1                1        T        -1      -1
  //      H  =  D    +  d  f(b)  R  (b) d  f(b)  +  - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  ,
  //                     b               b          2  b                               b
  // 
  // which is the expected value with respect to the data y of the
  // Hessian of the individual objective function.
  // See Section (7.)  of Bell (2001) for its derivation.
  //
  // Note that the R(b) on the right hand side of this equation is the
  // model for the covariance of an individual's data, is different
  // than the information matrix R.  The term involving the covariance
  // of the individual parameters D can be dropped from this
  // calculation if the Map Bayesian objective function was not used
  // to obtain the individual parameter estimates.
  // 
  // The cross-product gradient matrix is defined as
  // 
  //             nY                  nY + nB
  //             ---                   ---
  //             \       T             \        T
  //      S  =   /    { s   s  }  +    /     { s   s  }  .
  //             ---     i   i         ---      i   i
  //            i = 1               i = nY + 1
  //        
  // The second sum is dropped from this calculation if the Map
  // Bayesian objective function was not used to obtain the individual
  // parameter estimates.  For i values less than or equal to nY,
  // 
  //                     -                                                                      -
  //            - 1     |                                           T     -1                     |
  //     s   =   --- d  | log[ 2 pi R     (b) ]  +  [y    - f   (b)]  R     (b) [y    - f   (b)] |  . 
  //      i       2   b |            (i,i)            (i)    (i)       (i,i)      (i)    (i)     | 
  //                     -                                                                      -
  // 
  // For i values greater than nY,
  // 
  //                     -                                                   -
  //            - 1     |                                        -1        2  |
  //     s   =   --- d  | log[ 2 pi D            ]  +  D             b        |  . 
  //      i       2   b |            (i-nY,i-nY)        (i-nY,i-nY)   (i-nY)  | 
  //                     -                                                   -
  // 
  // The model functions for this test are:
  //
  //            / b(2) \ 
  //     f(b) = | b(2) |   ,
  //            \ b(2) /
  //
  //            /  exp[b(1)]     0         0      \ 
  //     R(b) = |      0     exp[b(1)]     0      |   ,
  //            \      0         0     exp[b(1)]  / 
  //
  //            /     1.0        0         0      \ 
  //     D    = |      0        0.5        0      |   .
  //            \      0         0        7.39    / 
  //
  int nX = nB - 1;
  valarray<double> MapObj_x_x ( nX * nX );
  valarray<double> bCovReduced( nX * nX );
  MapObj_x_x[ 0 + 0 * nX ] = MapObj_b_bOut[ 0 + 0 * nB ];
  MapObj_x_x[ 1 + 0 * nX ] = MapObj_b_bOut[ 1 + 0 * nB ];
  MapObj_x_x[ 0 + 1 * nX ] = MapObj_b_bOut[ 0 + 1 * nB ];
  MapObj_x_x[ 1 + 1 * nX ] = MapObj_b_bOut[ 1 + 1 * nB ];
  bCovReduced = inverse( ( MapObj_x_x + transpose( MapObj_x_x, nX ) ) * 0.5, nX );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 0 + 0 * nX ],  bCovOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 1 + 0 * nX ],  bCovOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 0 + 1 * nX ],  bCovOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 1 + 1 * nX ],  bCovOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 5 ], eps );
				    				    
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bSEOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 0 ] ), bSEOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 4 ] ), bSEOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                  bSEOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCVOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 0 ] / bOut[ 0 ] ) * 100., bCVOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 1 ] / bOut[ 1 ] ) * 100., bCVOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                                    bCVOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCorOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 1 ] / ( bSEOut[ 1 ] * bSEOut[ 0 ] ), bCorOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 3 ] / ( bSEOut[ 0 ] * bSEOut[ 1 ] ), bCorOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bCIOut.
  //------------------------------------------------------------
  // The degree of freedom for this particular problem is
  // 2 because one of the element in b is fixed so that
  // from indStatistics()'s point of view, there's only 3-1=2
  // elements in b.  
  // For degree of freedom = 2, the t-critical value for the 
  // 95% confidence level is 4.303.
  double d0 = 4.303 * bSEOut[ 0 ];
  double d1 = 4.303 * bSEOut[ 1 ];

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] - d0, bCIOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] - d1, bCIOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 2 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] + d0, bCIOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] + d1, bCIOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 5 ], eps );
}


/*************************************************************************
 *
 * Function: modelBasedInterface_H_formulation_withD_falseTest
 *
 *
 * This test checks the new interface to indStatistics.
 *
 *************************************************************************/

void indStatisticsTest::modelBasedInterface_H_formulation_withD_falseTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Set the formulation used to calculate the covariance of the
  // parameter estimates.
  IndCovForm formulation = H;

  // Set this flag to indicate that the Map Bayesian objective
  // function was not used to calculate the parameter estimates.
  bool withD = false;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  const int nY = 4;

  double yIn[] = { 1.8, 2.0, 2.2, 2.4 };
  valarray<double> y( yIn, nY );


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  const int nB = 3;

  // Fix the last element of b.
  bool maskIn[] = { true, true, false };
  valarray<bool> mask( maskIn, nB );

  valarray<double> bLow ( -4.0, nB );
  valarray<double> bUp  (  4.0, nB );
  valarray<double> bIn  (  2.0, nB );
  valarray<double> bOut (       nB );
  valarray<double> bStep( .001, nB );

  // Fix the 3rd (last) element of b.
  bLow[2] = bIn[2];
  bUp [2] = bIn[2];


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  ModelBasedInterfaceTestUserModel model;
  

  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double MapObjOut;
  valarray<double> MapObj_bOut  ( nB );
  valarray<double> MapObj_b_bOut( nB * nB );

  //------------------------------------------------------------
  // Remaining inputs to fitIndividual.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.e-3, 40, 0 );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try{
    fitIndividual( model,
                   y,
                   indOptimizer,
                   bLow,
                   bUp,
                   bIn,
                   bStep,
                   &bOut,            
                   &MapObjOut,
                   &MapObj_bOut,
                   &MapObj_b_bOut,
                   withD );
  }
  catch( const SpkException & e )
    {
      cerr << e << endl;
      throw;
    }


  //------------------------------------------------------------
  // Compute statistics of individual parameter estimates.
  //------------------------------------------------------------

  valarray<double> bCovOut( nB * nB );
  valarray<double> bSEOut ( nB );
  valarray<double> bCorOut( nB * nB );
  valarray<double> bCVOut ( nB );
  valarray<double> bCIOut ( nB * 2 );

  indStatistics(
    model,
    y,
    bOut,
    mask,
    MapObj_b_bOut,
    formulation,
    &bCovOut,
    &bSEOut,
    &bCorOut,
    &bCVOut,
    &bCIOut,
    withD );


  //------------------------------------------------------------
  // Test bCovOut.
  //------------------------------------------------------------

  double eps = 0.000000001;

  // The covariance matrix of the individual parameter estimates is
  // calculated using one of the following formulations:
  // 
  //                                       -1     -1
  //     formulation "RSR":  cov[ b ]  =  R   S  R   ;
  // 
  //                                       -1     -1
  //     formulation "HSH":  cov[ b ]  =  H   S  H   ;
  // 
  //                                       -1
  //     formulation "R":    cov[ b ]  =  R   ;
  // 
  //                                       -1
  //     formulation "H":    cov[ b ]  =  H   ;
  // 
  //                                       -1
  //     formulation "S":    cov[ b ]  =  S   .
  // 
  // 
  // The first approximation that can be made for the information matrix is
  //
  //                 -                                     -
  //             1  |                                    T  | 
  //      R  =  --- | d  d  MapObj(b)  +  d  d  MapObj(b)   |  ,
  //             2  |  b  b                b  b             | 
  //                 -                                     -
  //
  // which is a symmetrized version of the Hessian of the individual
  // objective function MapObj(b).
  // 
  // The second approximation that can be made for the information
  // matrix is
  // 
  //             -1            T  -1                1        T        -1      -1
  //      H  =  D    +  d  f(b)  R  (b) d  f(b)  +  - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  ,
  //                     b               b          2  b                               b
  // 
  // which is the expected value with respect to the data y of the
  // Hessian of the individual objective function.
  // See Section (7.)  of Bell (2001) for its derivation.
  //
  // Note that the R(b) on the right hand side of this equation is the
  // model for the covariance of an individual's data, is different
  // than the information matrix R.  The term involving the covariance
  // of the individual parameters D can be dropped from this
  // calculation if the Map Bayesian objective function was not used
  // to obtain the individual parameter estimates.
  // 
  // The cross-product gradient matrix is defined as
  // 
  //             nY                  nY + nB
  //             ---                   ---
  //             \       T             \        T
  //      S  =   /    { s   s  }  +    /     { s   s  }  .
  //             ---     i   i         ---      i   i
  //            i = 1               i = nY + 1
  //        
  // The second sum is dropped from this calculation if the Map
  // Bayesian objective function was not used to obtain the individual
  // parameter estimates.  For i values less than or equal to nY,
  // 
  //                     -                                                                      -
  //            - 1     |                                           T     -1                     |
  //     s   =   --- d  | log[ 2 pi R     (b) ]  +  [y    - f   (b)]  R     (b) [y    - f   (b)] |  . 
  //      i       2   b |            (i,i)            (i)    (i)       (i,i)      (i)    (i)     | 
  //                     -                                                                      -
  // 
  // For i values greater than nY,
  // 
  //                     -                                                   -
  //            - 1     |                                        -1        2  |
  //     s   =   --- d  | log[ 2 pi D            ]  +  D             b        |  . 
  //      i       2   b |            (i-nY,i-nY)        (i-nY,i-nY)   (i-nY)  | 
  //                     -                                                   -
  // 
  // The model functions for this test are:
  //
  //            / b(2) \ 
  //     f(b) = | b(2) |   ,
  //            \ b(2) /
  //
  //            /  exp[b(1)]     0         0      \ 
  //     R(b) = |      0     exp[b(1)]     0      |   ,
  //            \      0         0     exp[b(1)]  / 
  //
  //            /     1.0        0         0      \ 
  //     D    = |      0        0.5        0      |   .
  //            \      0         0        7.39    / 
  //
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0/2.0,                  bCovOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0/4.0*exp( bOut[ 0 ] ), bCovOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                        bCovOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bSEOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 0 ] ), bSEOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 4 ] ), bSEOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                  bSEOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCVOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 0 ] / bOut[ 0 ] ) * 100., bCVOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 1 ] / bOut[ 1 ] ) * 100., bCVOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                                    bCVOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCorOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 1 ] / ( bSEOut[ 1 ] * bSEOut[ 0 ] ), bCorOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 3 ] / ( bSEOut[ 0 ] * bSEOut[ 1 ] ), bCorOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bCIOut.
  //------------------------------------------------------------
  // The degree of freedom for this particular problem is
  // 2 because one of the element in b is fixed so that
  // from indStatistics()'s point of view, there's only 3-1=2
  // elements in b.  
  // For degree of freedom = 2, the t-critical value for the 
  // 95% confidence level is 4.303.
  double d0 = 4.303 * bSEOut[ 0 ];
  double d1 = 4.303 * bSEOut[ 1 ];

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] - d0, bCIOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] - d1, bCIOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 2 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] + d0, bCIOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] + d1, bCIOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 5 ], eps );
}


/*************************************************************************
 *
 * Function: modelBasedInterface_S_formulation_withD_falseTest
 *
 *
 * This test checks the new interface to indStatistics.
 *
 *************************************************************************/

void indStatisticsTest::modelBasedInterface_S_formulation_withD_falseTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Set the formulation used to calculate the covariance of the
  // parameter estimates.
  IndCovForm formulation = S;

  // Set this flag to indicate that the Map Bayesian objective
  // function was not used to calculate the parameter estimates.
  bool withD = false;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  const int nY = 4;

  double yIn[] = { 1.8, 2.0, 2.2, 2.4 };
  valarray<double> y( yIn, nY );


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  const int nB = 3;

  // Fix the last element of b.
  bool maskIn[] = { true, true, false };
  valarray<bool> mask( maskIn, nB );

  valarray<double> bLow ( -4.0, nB );
  valarray<double> bUp  (  4.0, nB );
  valarray<double> bIn  (  2.0, nB );
  valarray<double> bOut (       nB );
  valarray<double> bStep( .001, nB );

  // Fix the 3rd (last) element of b.
  bLow[2] = bIn[2];
  bUp [2] = bIn[2];


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  ModelBasedInterfaceTestUserModel model;
  

  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double MapObjOut;
  valarray<double> MapObj_bOut  ( nB );
  valarray<double> MapObj_b_bOut( nB * nB );

  //------------------------------------------------------------
  // Remaining inputs to fitIndividual.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.e-3, 40, 0 );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try{
    fitIndividual( model,
                   y,
                   indOptimizer,
                   bLow,
                   bUp,
                   bIn,
                   bStep,
                   &bOut,            
                   &MapObjOut,
                   &MapObj_bOut,
                   &MapObj_b_bOut,
                   withD );
  }
  catch( const SpkException & e )
    {
      cerr << e << endl;
      throw;
    }


  //------------------------------------------------------------
  // Compute statistics of individual parameter estimates.
  //------------------------------------------------------------

  valarray<double> bCovOut( nB * nB );
  valarray<double> bSEOut ( nB );
  valarray<double> bCorOut( nB * nB );
  valarray<double> bCVOut ( nB );
  valarray<double> bCIOut ( nB * 2 );

  indStatistics(
    model,
    y,
    bOut,
    mask,
    MapObj_b_bOut,
    formulation,
    &bCovOut,
    &bSEOut,
    &bCorOut,
    &bCVOut,
    &bCIOut,
    withD );


  //------------------------------------------------------------
  // Test bCovOut.
  //------------------------------------------------------------

  double eps = 0.000000001;

  // The covariance matrix of the individual parameter estimates is
  // calculated using one of the following formulations:
  // 
  //                                       -1     -1
  //     formulation "RSR":  cov[ b ]  =  R   S  R   ;
  // 
  //                                       -1     -1
  //     formulation "HSH":  cov[ b ]  =  H   S  H   ;
  // 
  //                                       -1
  //     formulation "R":    cov[ b ]  =  R   ;
  // 
  //                                       -1
  //     formulation "H":    cov[ b ]  =  H   ;
  // 
  //                                       -1
  //     formulation "S":    cov[ b ]  =  S   .
  // 
  // The first approximation that can be made for the information matrix is
  //
  //                 -                                     -
  //             1  |                                    T  | 
  //      R  =  --- | d  d  MapObj(b)  +  d  d  MapObj(b)   |  ,
  //             2  |  b  b                b  b             | 
  //                 -                                     -
  //
  // which is a symmetrized version of the Hessian of the individual
  // objective function MapObj(b).
  // 
  // The second approximation that can be made for the information
  // matrix is
  // 
  //             -1            T  -1                1        T        -1      -1
  //      H  =  D    +  d  f(b)  R  (b) d  f(b)  +  - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  ,
  //                     b               b          2  b                               b
  // 
  // which is the expected value with respect to the data y of the
  // Hessian of the individual objective function.
  // See Section (7.)  of Bell (2001) for its derivation.
  //
  // Note that the R(b) on the right hand side of this equation is the
  // model for the covariance of an individual's data, is different
  // than the information matrix R.  The term involving the covariance
  // of the individual parameters D can be dropped from this
  // calculation if the Map Bayesian objective function was not used
  // to obtain the individual parameter estimates.
  // 
  // The cross-product gradient matrix is defined as
  // 
  //             nY                  nY + nB
  //             ---                   ---
  //             \       T             \        T
  //      S  =   /    { s   s  }  +    /     { s   s  }  .
  //             ---     i   i         ---      i   i
  //            i = 1               i = nY + 1
  //        
  // The second sum is dropped from this calculation if the Map
  // Bayesian objective function was not used to obtain the individual
  // parameter estimates.  For i values less than or equal to nY,
  // 
  //                     -                                                                      -
  //            - 1     |                                           T     -1                     |
  //     s   =   --- d  | log[ 2 pi R     (b) ]  +  [y    - f   (b)]  R     (b) [y    - f   (b)] |  . 
  //      i       2   b |            (i,i)            (i)    (i)       (i,i)      (i)    (i)     | 
  //                     -                                                                      -
  // 
  // For i values greater than nY,
  // 
  //                     -                                                   -
  //            - 1     |                                        -1        2  |
  //     s   =   --- d  | log[ 2 pi D            ]  +  D             b        |  . 
  //      i       2   b |            (i-nY,i-nY)        (i-nY,i-nY)   (i-nY)  | 
  //                     -                                                   -
  // 
  // The model functions for this test are:
  //
  //            / b(2) \ 
  //     f(b) = | b(2) |   ,
  //            \ b(2) /
  //
  //            /  exp[b(1)]     0         0      \ 
  //     R(b) = |      0     exp[b(1)]     0      |   ,
  //            \      0         0     exp[b(1)]  / 
  //
  //            /     1.0        0         0      \ 
  //     D    = |      0        0.5        0      |   .
  //            \      0         0        7.39    / 
  //
  int nX   = nB - 1;
  int nS_i = nY;
  if ( withD )
  {
    nS_i += nX;
  }
  valarray<double> s_iReduced                    ( nX );
  valarray<double> s_iReducedTrans               ( nX );
  valarray<double> s_iReducedTransTimesS_iReduced( nX * nX );
  valarray<double> SReduced                      ( nX * nX );
  valarray<double> bCovReduced                   ( nX * nX );
  SReduced = 0.0;
  int i;
  for ( i = 0; i < nY; i++ )
  {
    s_iReduced[0] = 1.0/2.0 * ( pow( y[ i ] - bOut[ 1 ], 2.0)*exp( -bOut[ 0 ] ) - 1.0 );
    s_iReduced[1] = ( y[ i ] - bOut[ 1 ] )*exp( -bOut[ 0 ] );

    s_iReducedTrans                = transpose( s_iReduced, nX );
    s_iReducedTransTimesS_iReduced = multiply( s_iReducedTrans, 1, s_iReduced, nX );

    SReduced += s_iReducedTransTimesS_iReduced;
  }
  bCovReduced = inverse( SReduced, nX );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 0 + 0 * nX ],  bCovOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 1 + 0 * nX ],  bCovOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 0 + 1 * nX ],  bCovOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 1 + 1 * nX ],  bCovOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bSEOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 0 ] ), bSEOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 4 ] ), bSEOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                  bSEOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCVOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 0 ] / bOut[ 0 ] ) * 100., bCVOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 1 ] / bOut[ 1 ] ) * 100., bCVOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                                    bCVOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCorOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 1 ] / ( bSEOut[ 1 ] * bSEOut[ 0 ] ), bCorOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 3 ] / ( bSEOut[ 0 ] * bSEOut[ 1 ] ), bCorOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bCIOut.
  //------------------------------------------------------------
  // The degree of freedom for this particular problem is
  // 2 because one of the element in b is fixed so that
  // from indStatistics()'s point of view, there's only 3-1=2
  // elements in b.  
  // For degree of freedom = 2, the t-critical value for the 
  // 95% confidence level is 4.303.
  double d0 = 4.303 * bSEOut[ 0 ];
  double d1 = 4.303 * bSEOut[ 1 ];

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] - d0, bCIOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] - d1, bCIOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 2 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] + d0, bCIOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] + d1, bCIOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 5 ], eps );
}


/*************************************************************************
 *
 * Function: modelBasedInterface_R_formulation_withD_trueTest
 *
 *
 * This test checks the new interface to indStatistics.
 *
 *************************************************************************/

void indStatisticsTest::modelBasedInterface_R_formulation_withD_trueTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Set the formulation used to calculate the covariance of the
  // parameter estimates.
  IndCovForm formulation = R;

  // Set this flag to indicate that the Map Bayesian objective
  // function was used to calculate the parameter estimates.
  bool withD = true;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  const int nY = 4;

  double yIn[] = { 1.8, 2.0, 2.2, 2.4 };
  valarray<double> y( yIn, nY );


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  const int nB = 3;

  // Fix the last element of b.
  bool maskIn[] = { true, true, false };
  valarray<bool> mask( maskIn, nB );

  valarray<double> bLow ( -4.0, nB );
  valarray<double> bUp  (  4.0, nB );
  valarray<double> bIn  (  2.0, nB );
  valarray<double> bOut (       nB );
  valarray<double> bStep( .001, nB );

  // Fix the 3rd (last) element of b.
  bLow[2] = bIn[2];
  bUp [2] = bIn[2];


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  ModelBasedInterfaceTestUserModel model;
  

  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double MapObjOut;
  valarray<double> MapObj_bOut  ( nB );
  valarray<double> MapObj_b_bOut( nB * nB );

  //------------------------------------------------------------
  // Remaining inputs to fitIndividual.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.e-3, 40, 0 );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try{
    fitIndividual( model,
                   y,
                   indOptimizer,
                   bLow,
                   bUp,
                   bIn,
                   bStep,
                   &bOut,            
                   &MapObjOut,
                   &MapObj_bOut,
                   &MapObj_b_bOut,
                   withD );
  }
  catch( const SpkException & e )
    {
      cerr << e << endl;
      throw;
    }


  //------------------------------------------------------------
  // Compute statistics of individual parameter estimates.
  //------------------------------------------------------------

  valarray<double> bCovOut( nB * nB );
  valarray<double> bSEOut ( nB );
  valarray<double> bCorOut( nB * nB );
  valarray<double> bCVOut ( nB );
  valarray<double> bCIOut ( nB * 2 );

  indStatistics(
    model,
    y,
    bOut,
    mask,
    MapObj_b_bOut,
    formulation,
    &bCovOut,
    &bSEOut,
    &bCorOut,
    &bCVOut,
    &bCIOut,
    withD );


  //------------------------------------------------------------
  // Test bCovOut.
  //------------------------------------------------------------

  double eps = 0.000000001;

  // The covariance matrix of the individual parameter estimates is
  // calculated using one of the following formulations:
  // 
  //                                       -1     -1
  //     formulation "RSR":  cov[ b ]  =  R   S  R   ;
  // 
  //                                       -1     -1
  //     formulation "HSH":  cov[ b ]  =  H   S  H   ;
  // 
  //                                       -1
  //     formulation "R":    cov[ b ]  =  R   ;
  // 
  //                                       -1
  //     formulation "H":    cov[ b ]  =  H   ;
  // 
  //                                       -1
  //     formulation "S":    cov[ b ]  =  S   .
  // 
  // The first approximation that can be made for the information matrix is
  //
  //                 -                                     -
  //             1  |                                    T  | 
  //      R  =  --- | d  d  MapObj(b)  +  d  d  MapObj(b)   |  ,
  //             2  |  b  b                b  b             | 
  //                 -                                     -
  //
  // which is a symmetrized version of the Hessian of the individual
  // objective function MapObj(b).
  // 
  // The second approximation that can be made for the information
  // matrix is
  // 
  //             -1            T  -1                1        T        -1      -1
  //      H  =  D    +  d  f(b)  R  (b) d  f(b)  +  - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  ,
  //                     b               b          2  b                               b
  // 
  // which is the expected value with respect to the data y of the
  // Hessian of the individual objective function.
  // See Section (7.)  of Bell (2001) for its derivation.
  //
  // Note that the R(b) on the right hand side of this equation is the
  // model for the covariance of an individual's data, is different
  // than the information matrix R.  The term involving the covariance
  // of the individual parameters D can be dropped from this
  // calculation if the Map Bayesian objective function was not used
  // to obtain the individual parameter estimates.
  // 
  // The cross-product gradient matrix is defined as
  // 
  //             nY                  nY + nB
  //             ---                   ---
  //             \       T             \        T
  //      S  =   /    { s   s  }  +    /     { s   s  }  .
  //             ---     i   i         ---      i   i
  //            i = 1               i = nY + 1
  //        
  // The second sum is dropped from this calculation if the Map
  // Bayesian objective function was not used to obtain the individual
  // parameter estimates.  For i values less than or equal to nY,
  // 
  //                     -                                                                      -
  //            - 1     |                                           T     -1                     |
  //     s   =   --- d  | log[ 2 pi R     (b) ]  +  [y    - f   (b)]  R     (b) [y    - f   (b)] |  . 
  //      i       2   b |            (i,i)            (i)    (i)       (i,i)      (i)    (i)     | 
  //                     -                                                                      -
  // 
  // For i values greater than nY,
  // 
  //                     -                                                   -
  //            - 1     |                                        -1        2  |
  //     s   =   --- d  | log[ 2 pi D            ]  +  D             b        |  . 
  //      i       2   b |            (i-nY,i-nY)        (i-nY,i-nY)   (i-nY)  | 
  //                     -                                                   -
  // 
  // The model functions for this test are:
  //
  //            / b(2) \ 
  //     f(b) = | b(2) |   ,
  //            \ b(2) /
  //
  //            /  exp[b(1)]     0         0      \ 
  //     R(b) = |      0     exp[b(1)]     0      |   ,
  //            \      0         0     exp[b(1)]  / 
  //
  //            /     1.0        0         0      \ 
  //     D    = |      0        0.5        0      |   .
  //            \      0         0        7.39    / 
  //
  int nX = nB - 1;
  valarray<double> MapObj_x_x ( nX * nX );
  valarray<double> bCovReduced( nX * nX );
  MapObj_x_x[ 0 + 0 * nX ] = MapObj_b_bOut[ 0 + 0 * nB ];
  MapObj_x_x[ 1 + 0 * nX ] = MapObj_b_bOut[ 1 + 0 * nB ];
  MapObj_x_x[ 0 + 1 * nX ] = MapObj_b_bOut[ 0 + 1 * nB ];
  MapObj_x_x[ 1 + 1 * nX ] = MapObj_b_bOut[ 1 + 1 * nB ];
  bCovReduced = inverse( ( MapObj_x_x + transpose( MapObj_x_x, nX ) ) * 0.5, nX );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 0 + 0 * nX ],  bCovOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 1 + 0 * nX ],  bCovOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 0 + 1 * nX ],  bCovOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 1 + 1 * nX ],  bCovOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 5 ], eps );
				    				    
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bSEOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 0 ] ), bSEOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 4 ] ), bSEOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                  bSEOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCVOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 0 ] / bOut[ 0 ] ) * 100., bCVOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 1 ] / bOut[ 1 ] ) * 100., bCVOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                                    bCVOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCorOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 1 ] / ( bSEOut[ 1 ] * bSEOut[ 0 ] ), bCorOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 3 ] / ( bSEOut[ 0 ] * bSEOut[ 1 ] ), bCorOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bCIOut.
  //------------------------------------------------------------
  // The degree of freedom for this particular problem is
  // 2 because one of the element in b is fixed so that
  // from indStatistics()'s point of view, there's only 3-1=2
  // elements in b.  
  // For degree of freedom = 2, the t-critical value for the 
  // 95% confidence level is 4.303.
  double d0 = 4.303 * bSEOut[ 0 ];
  double d1 = 4.303 * bSEOut[ 1 ];

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] - d0, bCIOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] - d1, bCIOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 2 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] + d0, bCIOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] + d1, bCIOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 5 ], eps );
}


/*************************************************************************
 *
 * Function: modelBasedInterface_H_formulation_withD_trueTest
 *
 *
 * This test checks the new interface to indStatistics.
 *
 *************************************************************************/

void indStatisticsTest::modelBasedInterface_H_formulation_withD_trueTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Set the formulation used to calculate the covariance of the
  // parameter estimates.
  IndCovForm formulation = H;

  // Set this flag to indicate that the Map Bayesian objective
  // function was used to calculate the parameter estimates.
  bool withD = true;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  const int nY = 4;

  double yIn[] = { 1.8, 2.0, 2.2, 2.4 };
  valarray<double> y( yIn, nY );


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  const int nB = 3;

  // Fix the last element of b.
  bool maskIn[] = { true, true, false };
  valarray<bool> mask( maskIn, nB );

  valarray<double> bLow ( -4.0, nB );
  valarray<double> bUp  (  4.0, nB );
  valarray<double> bIn  (  2.0, nB );
  valarray<double> bOut (       nB );
  valarray<double> bStep( .001, nB );

  // Fix the 3rd (last) element of b.
  bLow[2] = bIn[2];
  bUp [2] = bIn[2];


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  ModelBasedInterfaceTestUserModel model;
  

  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double MapObjOut;
  valarray<double> MapObj_bOut  ( nB );
  valarray<double> MapObj_b_bOut( nB * nB );

  //------------------------------------------------------------
  // Remaining inputs to fitIndividual.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.e-3, 40, 0 );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try{
    fitIndividual( model,
                   y,
                   indOptimizer,
                   bLow,
                   bUp,
                   bIn,
                   bStep,
                   &bOut,            
                   &MapObjOut,
                   &MapObj_bOut,
                   &MapObj_b_bOut,
                   withD );
  }
  catch( const SpkException & e )
    {
      cerr << e << endl;
      throw;
    }


  //------------------------------------------------------------
  // Compute statistics of individual parameter estimates.
  //------------------------------------------------------------

  valarray<double> bCovOut( nB * nB );
  valarray<double> bSEOut ( nB );
  valarray<double> bCorOut( nB * nB );
  valarray<double> bCVOut ( nB );
  valarray<double> bCIOut ( nB * 2 );

  indStatistics(
    model,
    y,
    bOut,
    mask,
    MapObj_b_bOut,
    formulation,
    &bCovOut,
    &bSEOut,
    &bCorOut,
    &bCVOut,
    &bCIOut,
    withD );


  //------------------------------------------------------------
  // Test bCovOut.
  //------------------------------------------------------------

  double eps = 0.000000001;

  // The covariance matrix of the individual parameter estimates is
  // calculated using one of the following formulations:
  // 
  //                                       -1     -1
  //     formulation "RSR":  cov[ b ]  =  R   S  R   ;
  // 
  //                                       -1     -1
  //     formulation "HSH":  cov[ b ]  =  H   S  H   ;
  // 
  //                                       -1
  //     formulation "R":    cov[ b ]  =  R   ;
  // 
  //                                       -1
  //     formulation "H":    cov[ b ]  =  H   ;
  // 
  //                                       -1
  //     formulation "S":    cov[ b ]  =  S   .
  // 
  // The first approximation that can be made for the information matrix is
  //
  //                 -                                     -
  //             1  |                                    T  | 
  //      R  =  --- | d  d  MapObj(b)  +  d  d  MapObj(b)   |  ,
  //             2  |  b  b                b  b             | 
  //                 -                                     -
  //
  // which is a symmetrized version of the Hessian of the individual
  // objective function MapObj(b).
  // 
  // The second approximation that can be made for the information
  // matrix is
  // 
  //             -1            T  -1                1        T        -1      -1
  //      H  =  D    +  d  f(b)  R  (b) d  f(b)  +  - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  ,
  //                     b               b          2  b                               b
  // 
  // which is the expected value with respect to the data y of the
  // Hessian of the individual objective function.
  // See Section (7.)  of Bell (2001) for its derivation.
  //
  // Note that the R(b) on the right hand side of this equation is the
  // model for the covariance of an individual's data, is different
  // than the information matrix R.  The term involving the covariance
  // of the individual parameters D can be dropped from this
  // calculation if the Map Bayesian objective function was not used
  // to obtain the individual parameter estimates.
  // 
  // The cross-product gradient matrix is defined as
  // 
  //             nY                  nY + nB
  //             ---                   ---
  //             \       T             \        T
  //      S  =   /    { s   s  }  +    /     { s   s  }  .
  //             ---     i   i         ---      i   i
  //            i = 1               i = nY + 1
  //        
  // The second sum is dropped from this calculation if the Map
  // Bayesian objective function was not used to obtain the individual
  // parameter estimates.  For i values less than or equal to nY,
  // 
  //                     -                                                                      -
  //            - 1     |                                           T     -1                     |
  //     s   =   --- d  | log[ 2 pi R     (b) ]  +  [y    - f   (b)]  R     (b) [y    - f   (b)] |  . 
  //      i       2   b |            (i,i)            (i)    (i)       (i,i)      (i)    (i)     | 
  //                     -                                                                      -
  // 
  // For i values greater than nY,
  // 
  //                     -                                                   -
  //            - 1     |                                        -1        2  |
  //     s   =   --- d  | log[ 2 pi D            ]  +  D             b        |  . 
  //      i       2   b |            (i-nY,i-nY)        (i-nY,i-nY)   (i-nY)  | 
  //                     -                                                   -
  // 
  // The model functions for this test are:
  //
  //            / b(2) \ 
  //     f(b) = | b(2) |   ,
  //            \ b(2) /
  //
  //            /  exp[b(1)]     0         0      \ 
  //     R(b) = |      0     exp[b(1)]     0      |   ,
  //            \      0         0     exp[b(1)]  / 
  //
  //            /     1.0        0         0      \ 
  //     D    = |      0        0.5        0      |   .
  //            \      0         0        7.39    / 
  //
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0/3.0,                          bCovOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                                bCovOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                                bCovOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                                bCovOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0/(2.0+4.0*exp( - bOut[ 0 ] )), bCovOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                                bCovOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                                bCovOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                                bCovOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                                bCovOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bSEOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 0 ] ), bSEOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 4 ] ), bSEOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                  bSEOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCVOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 0 ] / bOut[ 0 ] ) * 100., bCVOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 1 ] / bOut[ 1 ] ) * 100., bCVOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                                    bCVOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCorOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 1 ] / ( bSEOut[ 1 ] * bSEOut[ 0 ] ), bCorOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 3 ] / ( bSEOut[ 0 ] * bSEOut[ 1 ] ), bCorOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bCIOut.
  //------------------------------------------------------------
  // The degree of freedom for this particular problem is
  // 2 because one of the element in b is fixed so that
  // from indStatistics()'s point of view, there's only 3-1=2
  // elements in b.  
  // For degree of freedom = 2, the t-critical value for the 
  // 95% confidence level is 4.303.
  double d0 = 4.303 * bSEOut[ 0 ];
  double d1 = 4.303 * bSEOut[ 1 ];

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] - d0, bCIOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] - d1, bCIOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 2 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] + d0, bCIOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] + d1, bCIOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 5 ], eps );
}


/*************************************************************************
 *
 * Function: modelBasedInterface_S_formulation_withD_trueTest
 *
 *
 * This test checks the new interface to indStatistics.
 *
 *************************************************************************/

void indStatisticsTest::modelBasedInterface_S_formulation_withD_trueTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Set the formulation used to calculate the covariance of the
  // parameter estimates.
  IndCovForm formulation = S;

  // Set this flag to indicate that the Map Bayesian objective
  // function was used to calculate the parameter estimates.
  bool withD = true;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  const int nY = 4;

  double yIn[] = { 1.8, 2.0, 2.2, 2.4 };
  valarray<double> y( yIn, nY );


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  const int nB = 3;

  // Fix the last element of b.
  bool maskIn[] = { true, true, false };
  valarray<bool> mask( maskIn, nB );

  valarray<double> bLow ( -4.0, nB );
  valarray<double> bUp  (  4.0, nB );
  valarray<double> bIn  (  2.0, nB );
  valarray<double> bOut (       nB );
  valarray<double> bStep( .001, nB );

  // Fix the 3rd (last) element of b.
  bLow[2] = bIn[2];
  bUp [2] = bIn[2];


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  ModelBasedInterfaceTestUserModel model;
  

  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double MapObjOut;
  valarray<double> MapObj_bOut  ( nB );
  valarray<double> MapObj_b_bOut( nB * nB );

  //------------------------------------------------------------
  // Remaining inputs to fitIndividual.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.e-3, 40, 0 );


  //------------------------------------------------------------
  // Optimize the objective function.
  //------------------------------------------------------------

  try{
    fitIndividual( model,
                   y,
                   indOptimizer,
                   bLow,
                   bUp,
                   bIn,
                   bStep,
                   &bOut,            
                   &MapObjOut,
                   &MapObj_bOut,
                   &MapObj_b_bOut,
                   withD );
  }
  catch( const SpkException & e )
    {
      cerr << e << endl;
      throw;
    }


  //------------------------------------------------------------
  // Compute statistics of individual parameter estimates.
  //------------------------------------------------------------

  valarray<double> bCovOut( nB * nB );
  valarray<double> bSEOut ( nB );
  valarray<double> bCorOut( nB * nB );
  valarray<double> bCVOut ( nB );
  valarray<double> bCIOut ( nB * 2 );

  indStatistics(
    model,
    y,
    bOut,
    mask,
    MapObj_b_bOut,
    formulation,
    &bCovOut,
    &bSEOut,
    &bCorOut,
    &bCVOut,
    &bCIOut,
    withD );


  //------------------------------------------------------------
  // Test bCovOut.
  //------------------------------------------------------------

  double eps = 0.000000001;

  // The covariance matrix of the individual parameter estimates is
  // calculated using one of the following formulations:
  // 
  //                                       -1     -1
  //     formulation "RSR":  cov[ b ]  =  R   S  R   ;
  // 
  //                                       -1     -1
  //     formulation "HSH":  cov[ b ]  =  H   S  H   ;
  // 
  //                                       -1
  //     formulation "R":    cov[ b ]  =  R   ;
  // 
  //                                       -1
  //     formulation "H":    cov[ b ]  =  H   ;
  // 
  //                                       -1
  //     formulation "S":    cov[ b ]  =  S   .
  // 
  // The first approximation that can be made for the information matrix is
  //
  //                 -                                     -
  //             1  |                                    T  | 
  //      R  =  --- | d  d  MapObj(b)  +  d  d  MapObj(b)   |  ,
  //             2  |  b  b                b  b             | 
  //                 -                                     -
  //
  // which is a symmetrized version of the Hessian of the individual
  // objective function MapObj(b).
  // 
  // The second approximation that can be made for the information
  // matrix is
  // 
  //             -1            T  -1                1        T        -1      -1
  //      H  =  D    +  d  f(b)  R  (b) d  f(b)  +  - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  ,
  //                     b               b          2  b                               b
  // 
  // which is the expected value with respect to the data y of the
  // Hessian of the individual objective function.
  // See Section (7.)  of Bell (2001) for its derivation.
  //
  // Note that the R(b) on the right hand side of this equation is the
  // model for the covariance of an individual's data, is different
  // than the information matrix R.  The term involving the covariance
  // of the individual parameters D can be dropped from this
  // calculation if the Map Bayesian objective function was not used
  // to obtain the individual parameter estimates.
  // 
  // The cross-product gradient matrix is defined as
  // 
  //             nY                  nY + nB
  //             ---                   ---
  //             \       T             \        T
  //      S  =   /    { s   s  }  +    /     { s   s  }  .
  //             ---     i   i         ---      i   i
  //            i = 1               i = nY + 1
  //        
  // The second sum is dropped from this calculation if the Map
  // Bayesian objective function was not used to obtain the individual
  // parameter estimates.  For i values less than or equal to nY,
  // 
  //                     -                                                                      -
  //            - 1     |                                           T     -1                     |
  //     s   =   --- d  | log[ 2 pi R     (b) ]  +  [y    - f   (b)]  R     (b) [y    - f   (b)] |  . 
  //      i       2   b |            (i,i)            (i)    (i)       (i,i)      (i)    (i)     | 
  //                     -                                                                      -
  // 
  // For i values greater than nY,
  // 
  //                     -                                                   -
  //            - 1     |                                        -1        2  |
  //     s   =   --- d  | log[ 2 pi D            ]  +  D             b        |  . 
  //      i       2   b |            (i-nY,i-nY)        (i-nY,i-nY)   (i-nY)  | 
  //                     -                                                   -
  // 
  // The model functions for this test are:
  //
  //            / b(2) \ 
  //     f(b) = | b(2) |   ,
  //            \ b(2) /
  //
  //            /  exp[b(1)]     0         0      \ 
  //     R(b) = |      0     exp[b(1)]     0      |   ,
  //            \      0         0     exp[b(1)]  / 
  //
  //            /     1.0        0         0      \ 
  //     D    = |      0        0.5        0      |   .
  //            \      0         0        7.39    / 
  //
  int nX   = nB - 1;
  int nS_i = nY;
  if ( withD )
  {
    nS_i += nX;
  }
  valarray<double> s_iReduced                    ( nX );
  valarray<double> s_iReducedTrans               ( nX );
  valarray<double> s_iReducedTransTimesS_iReduced( nX * nX );
  valarray<double> SReduced                      ( nX * nX );
  valarray<double> bCovReduced                   ( nX * nX );
  SReduced = 0.0;
  int i;
  for ( i = 0; i < nY; i++ )
  {
    s_iReduced[0] = 1.0/2.0 * ( pow( y[ i ] - bOut[ 1 ], 2.0)*exp( -bOut[ 0 ] ) - 1.0 );
    s_iReduced[1] = ( y[ i ] - bOut[ 1 ] )*exp( -bOut[ 0 ] );

    s_iReducedTrans                = transpose( s_iReduced, nX );
    s_iReducedTransTimesS_iReduced = multiply( s_iReducedTrans, 1, s_iReduced, nX );

    SReduced += s_iReducedTransTimesS_iReduced;
  }
  for ( i = nY; i < nY + nX; i++ )
  {
    s_iReduced[0] = - 1.0 * bOut[ 0 ];
    s_iReduced[1] = - 2.0 * bOut[ 1 ];

    s_iReducedTrans                = transpose( s_iReduced, nX );
    s_iReducedTransTimesS_iReduced = multiply( s_iReducedTrans, 1, s_iReduced, nX );

    SReduced += s_iReducedTransTimesS_iReduced;
  }
  bCovReduced = inverse( SReduced, nX );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 0 + 0 * nX ],  bCovOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 1 + 0 * nX ],  bCovOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 0 + 1 * nX ],  bCovOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovReduced[ 1 + 1 * nX ],  bCovOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0,                          bCovOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bSEOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 0 ] ), bSEOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( sqrt( bCovOut[ 4 ] ), bSEOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                  bSEOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCVOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 0 ] / bOut[ 0 ] ) * 100., bCVOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( fabs( bSEOut[ 1 ] / bOut[ 1 ] ) * 100., bCVOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,                                    bCVOut[ 2 ], eps );

  //------------------------------------------------------------
  // Test bCorOut.
  //------------------------------------------------------------

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 1 ] / ( bSEOut[ 1 ] * bSEOut[ 0 ] ), bCorOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 2 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bCovOut[ 3 ] / ( bSEOut[ 0 ] * bSEOut[ 1 ] ), bCorOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.,                                           bCorOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 5 ], eps );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 6 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 7 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.,                                           bCorOut[ 8 ], eps );


  //------------------------------------------------------------
  // Test bCIOut.
  //------------------------------------------------------------
  // The degree of freedom for this particular problem is
  // 2 because one of the element in b is fixed so that
  // from indStatistics()'s point of view, there's only 3-1=2
  // elements in b.  
  // For degree of freedom = 2, the t-critical value for the 
  // 95% confidence level is 4.303.
  double d0 = 4.303 * bSEOut[ 0 ];
  double d1 = 4.303 * bSEOut[ 1 ];

  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] - d0, bCIOut[ 0 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] - d1, bCIOut[ 1 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 2 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 0 ] + d0, bCIOut[ 3 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( bOut[ 1 ] + d1, bCIOut[ 4 ], eps );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0,            bCIOut[ 5 ], eps );
}


