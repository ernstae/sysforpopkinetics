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

#include <spk/SpkException.h>
#include <spk/fitIndividual.h>
#include <spk/indStatistics.h>
#include <spk/SpkModel.h>
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

    suiteOfTests->addTest(new TestCaller<indStatisticsTest>(
                  "statisticsExampleTest", &indStatisticsTest::statisticsExampleTest));

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

class UserModelIndStatisticsExampleTest : public SpkModel
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
//            /  1.0     0   \ 
//     D(b) = |              |   
//            \  0       0.5 / 
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

	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCVOut[ 0 ], indParSEOut[ 0 ] / indParOut[ 0 ] * 100., eps );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( indParCVOut[ 1 ], indParSEOut[ 1 ] / indParOut[ 1 ] * 100., eps );

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

