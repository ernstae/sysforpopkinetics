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
 * File: fitIndividualTest.cpp
 *
 *
 * Unit test for the function fitIndividual.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include <spk/fitIndividual.h>
#include "fitIndividualTest.h"

#include <iomanip>
#include <cmath>
#include <cstdlib>
#include <cassert>

#include <spk/mapObj.h>
#include <spk/SpkModel.h>
#include <spk/pi.h>
#include <spk/DoubleMatrix.h>
#include <spk/SpkValarray.h>
#include <spk/allZero.h>
#include <spk/identity.h>

using namespace CppUnit;

void fitIndividualTest::setUp()
{
    // initializations
}
void fitIndividualTest::tearDown()
{
    // clean up
}

Test* fitIndividualTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite("fitIndividualTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<fitIndividualTest>(
                         "fitIndividualExampleTest", 
                         &fitIndividualTest::fitIndividualExampleTest));
    suiteOfTests->addTest(new TestCaller<fitIndividualTest>(
                         "fitIndividualQuadraticTest", 
                         &fitIndividualTest::fitIndividualQuadraticTest));
    suiteOfTests->addTest(new TestCaller<fitIndividualTest>(
                         "fitIndividualZeroIterationsTest", 
                         &fitIndividualTest::fitIndividualZeroIterationsTest));


    return suiteOfTests;
}

/*------------------------------------------------------------------------
 * Namespace Declarations
 *------------------------------------------------------------------------*/

using namespace std;


/*------------------------------------------------------------------------
 * Local Function Declarations
 *------------------------------------------------------------------------*/

namespace fitindividualexample
{
  static valarray<double> funF  (const valarray<double> &dvecB );
  static valarray<double> funF_b(const valarray<double> &dvecFb, const valarray<double> &dvecB );
  static valarray<double> funR  (const valarray<double> &dvecB );
  static valarray<double> funR_b(const valarray<double> &dmatRb, const valarray<double> &dvecB );
}

namespace fitindividualquadratictest
{
  static valarray<double> funF  (const valarray<double> &dvecB );
  static valarray<double> funF_b(const valarray<double> &dvecFb, const valarray<double> &dvecB );
  static valarray<double> funR  (const valarray<double> &dvecB );
  static valarray<double> funR_b(const valarray<double> &dmatRb, const valarray<double> &dvecB );

  int nY;
  int nB;
}


namespace fitindividualzeroiterations
{
  static valarray<double> funF  (const valarray<double> &dvecB );
  static valarray<double> funF_b(const valarray<double> &dvecFb, const valarray<double> &dvecB );
  static valarray<double> funR  (const valarray<double> &dvecB );
  static valarray<double> funR_b(const valarray<double> &dmatRb, const valarray<double> &dvecB );
}

/*------------------------------------------------------------------------
 * Class Definition
 *------------------------------------------------------------------------*/

class UserModelFitIndividualExampleTest : public SpkModel
{
    valarray<double> _b;
    int _nY;

public:
    UserModelFitIndividualExampleTest( int nB, int nY ) : _b(nB), _nY(nY) {};    
    ~UserModelFitIndividualExampleTest(){};
private:
    void doSetIndPar(const valarray<double>& bval)
    {
        _b = bval;
    }
    void doDataMean( valarray<double>& fOut ) const
    {
        using namespace fitindividualexample;
	fOut.resize( _nY );
        fOut = funF(_b);
    }
    bool doDataMean_indPar( valarray<double>& f_bOut ) const
    {
        using namespace fitindividualexample;
	valarray<double> fOut( _nY );
        doDataMean(fOut);

	f_bOut.resize( _nY * _b.size() );
        f_bOut = funF_b(fOut, _b);
        return !allZero(f_bOut);
    }
    void doDataVariance( valarray<double>& ROut ) const
    {
        using namespace fitindividualexample;
	ROut.resize( _nY * _nY );
        ROut = funR(_b);
    }
    bool doDataVariance_indPar( valarray<double>& R_bOut ) const
    {
        using namespace fitindividualexample;
        valarray<double> ROut( _nY * _nY );
	doDataVariance(ROut);
        
	R_bOut.resize( _nY * _nY * _b.size() );
	R_bOut = funR_b(ROut, _b);
        return !allZero(R_bOut);
    }   
    void doIndParVariance( valarray<double>& DOut ) const
    {
        using namespace fitindividualexample;

	DOut.resize( _b.size() * _b.size() );
        DOut[0] = 1.0;
        DOut[1] = 0.0;
        DOut[2] = 0.0;
        DOut[3] = 0.5;
    }
    UserModelFitIndividualExampleTest() {};
};

class UserModelFitIndividualQuadraticTest : public SpkModel
{
    valarray<double> _b;
    int _nY;
public:
    UserModelFitIndividualQuadraticTest(int nB, int nY)
      : _b(nB), _nY(nY)
    {};  
    ~UserModelFitIndividualQuadraticTest(){};
private:
    void doSetIndPar(const valarray<double>& bval)
    {
      _b = bval;
    }

    void doDataMean( valarray<double>& fOut ) const
    {
        using namespace fitindividualquadratictest;
        fOut.resize( _nY );
        fOut = funF(_b);   
    }
    bool doDataMean_indPar( valarray<double>& f_bOut ) const
    {
        using namespace fitindividualquadratictest;
	valarray<double> fOut(_b.size());
        doDataMean(fOut);

	f_bOut.resize( _nY * _b.size() );
        f_bOut = funF_b(fOut, _b);
        return !allZero(f_bOut);
    }
    void doDataVariance( valarray<double>& ROut ) const
    {
        using namespace fitindividualquadratictest;
	ROut.resize( _nY * _nY );
        ROut = funR(_b);
    }
    bool doDataVariance_indPar( valarray<double>& R_bOut ) const
    {
        using namespace fitindividualquadratictest;
	valarray<double> ROut( _nY * _nY );
        doDataVariance(ROut);

	R_bOut.resize( _nY * _nY * _b.size() );
        R_bOut = funR_b(ROut, _b);
        return !allZero(R_bOut);
    }   
    void doIndParVariance( valarray<double>& DOut ) const
    {
        using namespace fitindividualquadratictest;
	DOut.resize( _b.size() * _b.size() );
        identity( _b.size(), DOut );
    }

    UserModelFitIndividualQuadraticTest(){};
};

class UserModelFitIndividualZeroIterationsTest : public SpkModel
{
    valarray<double> _b;
    int _nY;
public:
    UserModelFitIndividualZeroIterationsTest(int nB, int nY) : _b(nB), _nY(nY){}; 
    ~UserModelFitIndividualZeroIterationsTest(){};
private:
    void doSetIndPar(const valarray<double>& bval)
    {
        _b = bval;
    }
    void doDataMean( valarray<double>& fOut ) const
    {
        using namespace fitindividualzeroiterations;
	fOut.resize( _nY );
        fOut = funF(_b);
    }
    bool doDataMean_indPar( valarray<double>& f_bOut ) const
    {
        using namespace fitindividualzeroiterations;
	valarray<double> fOut( _nY );
        doDataMean(fOut);
	f_bOut.resize( _nY * _b.size() );
        f_bOut = funF_b(fOut, _b);
        return !allZero(f_bOut);
    }
    void doDataVariance( valarray<double>& ROut ) const
    {
        using namespace fitindividualzeroiterations;
	ROut.resize( _nY * _nY );
        ROut = funR(_b);
    }
    bool doDataVariance_indPar( valarray<double>& R_bOut ) const
    {
        using namespace fitindividualzeroiterations;
	valarray<double> ROut( _nY * _nY );
        doDataVariance(ROut);
	R_bOut.resize( _nY * _nY * _b.size() );
        R_bOut = funR_b(ROut, _b);
        return !allZero(R_bOut);
    }   
    void doIndParVariance( valarray<double>& DOut ) const
    {
        using namespace fitindividualzeroiterations;
	DOut.resize( _b.size() * _b.size() );
        identity( _b.size(), DOut );
    }
    UserModelFitIndividualZeroIterationsTest(){};
};


/*************************************************************************
 *
 * Function: fitIndividualExampleTest
 *
 *
 * This test implements the example problem from the fitIndividual specification. 
 *
 *************************************************************************/

void fitIndividualTest::fitIndividualExampleTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace fitindividualexample;

  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  int nY = 2;
  valarray<double> dvecY( 2.0, nY );

  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  int nB = 2;

  valarray<double> dmatD( nB * nB );
  dmatD[0] = 1.0;
  dmatD[1] = 0.0;
  dmatD[2] = 0.0;
  dmatD[3] = 0.5;

  valarray<double> dvecBLow ( -4.0, nB );
  valarray<double> dvecBUp  (  4.0, nB );
  valarray<double> dvecBIn  (  2.0, nB );
  valarray<double> dvecBOut (       nB );
  valarray<double> dvecBStep( .001, nB );


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

    UserModelFitIndividualExampleTest model( nB, nY );

  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double dMapObjOut;

  valarray<double> drowMapObj_bOut  ( nB );
  valarray<double> dmatMapObj_b_bOut( nB * nB );


  //------------------------------------------------------------
  // Remaining inputs to fitIndividual.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.e-3, 5, 0 );
  bool withD      = true;
  indOptimizer.setupWarmStart( nB );

  //------------------------------------------------------------
  // Optimize MapObj(b).
  //------------------------------------------------------------

  try{
	  while( true )
	  {
          fitIndividual( model,
						 dvecY,
						 indOptimizer,
						 dvecBLow,
						 dvecBUp,
						 dvecBIn,
						 dvecBStep,
						 &dvecBOut,            
						 &dMapObjOut,
						 &drowMapObj_bOut,
						 &dmatMapObj_b_bOut,
						 withD );
          if( !indOptimizer.getIsTooManyIter() )
		      // Finished
			  break;

		  // Turn on warm start.
          indOptimizer.setIsWarmStart( true );
	  }
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }

  //------------------------------------------------------------
  // Known values.
  //------------------------------------------------------------

  valarray<double> dvecBKnown( nB );
  dvecBKnown[0] = 0.0;
  dvecBKnown[1] = 1.0;

  double dMapObjKnown = 2.0 * log( 2.0 * PI ) - 0.5 * log( 2.0 ) + 2.0;

  valarray<double> dmatMapObj_b_bKnown( nB * nB );
  dmatMapObj_b_bKnown[0] = 2.0;
  dmatMapObj_b_bKnown[1] = 2.0;
  dmatMapObj_b_bKnown[2] = 2.0;
  dmatMapObj_b_bKnown[3] = 4.0;


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(  dMapObjOut, 
              dMapObjKnown, 
              indOptimizer.getEpsilon(), 
			  dvecBLow,
              dvecBUp, 
              dvecBOut, 
              dvecBKnown, 
              dmatMapObj_b_bOut, 
              dmatMapObj_b_bKnown );
  
}


/*************************************************************************
 *
 * Function: funR
 *
 *
 * Calculates
 *
 *            /  exp[b(1)]     0  \ 
 *     R(b) = |                   |   .
 *            \  0      exp[b(1)] / 
 *
 *************************************************************************/

static valarray<double> fitindividualexample::funR( const valarray<double> &dvecB )
{
  valarray<double> dmatR( 2 * 2 );
  dmatR[0] = exp( dvecB[0] );
  dmatR[1] = 0.0;
  dmatR[2] = 0.0;
  dmatR[3] = exp( dvecB[0] );
  
  return dmatR;
}


/*************************************************************************
 *
 * Function: funR_b
 *
 *
 * Calculates
 *
 *              /  exp[b(1)]     0  \ 
 *     R_b(b) = |  0             0  |   .
 *              |  0             0  | 
 *              \  exp[b(1)]     0  / 
 *
 *************************************************************************/

static valarray<double> fitindividualexample::funR_b( const valarray<double> &dmatR, 
                                                      const valarray<double> &dvecB )
{
  valarray<double> dmatR_b( 0.0, dmatR.size() * dvecB.size() );
  dmatR_b[0] = exp( dvecB[0] );
  dmatR_b[3] = exp( dvecB[0] );
  
  return dmatR_b;
}


/*************************************************************************
 *
 * Function: funF
 *
 *
 * Calculates
 *
 *            / b(2) \ 
 *     f(b) = |      |   .
 *            \ b(2) /
 *
 *************************************************************************/

static valarray<double> fitindividualexample::funF( const valarray<double> &dvecB )
{
  valarray<double> dvecF( 2 );
  dvecF[0] = dvecB[1];
  dvecF[1] = dvecB[1];

  return dvecF;
}


/*************************************************************************
 *
 * Function: funF_b
 *
 *
 * Calculates
 *
 *              / 0   1 \ 
 *     f_b(b) = |       |   .
 *              \ 0   1 /
 *
 *************************************************************************/

static valarray<double> fitindividualexample::funF_b( const valarray<double> &dvecF, 
                                                      const valarray<double> &dvecB )
{
  valarray<double> dmatF_b( 2 * 2 );
  dmatF_b[0] = 0.0;
  dmatF_b[1] = 0.0;
  dmatF_b[2] = 1.0;
  dmatF_b[3] = 1.0;
  
  return dmatF_b;
}


/*************************************************************************
 *
 * Function: fitIndividualQuadraticTest
 *
 *
 * This test uses a form for mapObj(b) that is quadratic in b: 
 *
 *                  1             
 *     mapObj(b) =  -  ( nY + nB )  log(2 PI)
 *                  2  
 * 
 *                         nB    -                         -
 *                     1  ----  |              2         2  |
 *                  +  -  >     |  [ j - b    ]   +  b      |  ,
 *                     2  ----  |         (j)         (j)   |
 *                        j = 1  -                         -
 *
 * where
 *
 *     nY  =  number of elements in y,
 *
 *     nB  =  number of elements in b,
 *
 * and
 *
 *     nB  <  nY  .
 *
 *
 * It accomplishes this by setting 
 *
 *                                   T
 *     y     =  ( 1, 2, 3, ... , nY )   ,
 *
 *                                                                   T
 *     f(b)  =  ( b   , b   , ... , b    , nB + 1, nB + 2, ... , nY )   ,
 *                 (1)   (2)         (nB)
 *
 *     R(b)  =  I     ,
 *               nY
 *
 *     D(b)  =  I     ,
 *               nB
 *
 * where
 *
 *     I   =  an m by m identity matrix.
 *      m
 *
 *************************************************************************/

void fitIndividualTest::fitIndividualQuadraticTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace fitindividualquadratictest;

  int i;

  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  nY = 50;

  valarray<double> dvecY( nY );

  for ( i = 0; i < nY; i++ )
  {
    dvecY[i] = i + 1;
  }


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  nB = 25;
  CPPUNIT_ASSERT( nB < nY );

  valarray<double> dmatD( nB * nB );
  identity( nB, dmatD );

  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelFitIndividualQuadraticTest model( nB, nY );

  // This is the known value for the (nB - 1)th element of the
  // b value that minimizes the objective function.
  double val = 0.5 * ( nB - 1 );

  valarray<double> dvecBLow ( -val, nB );
  valarray<double> dvecBUp  ( +val, nB );
  valarray<double> dvecBIn  (  0.0, nB );
  valarray<double> dvecBOut (       nB );
  valarray<double> dvecBStep( .001, nB );


  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double dMapObjOut;

  valarray<double> drowMapObj_bOut  ( nB );
  valarray<double> dmatMapObj_b_bOut( nB * nB );


  //------------------------------------------------------------
  // Remaining inputs to fitIndividual.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.e-3, 100, 0 );
  bool withD      = true;


  //------------------------------------------------------------
  // Optimize MapObj(b).
  //------------------------------------------------------------

  try{
      fitIndividual( model,
                     dvecY,
                     indOptimizer,
                     dvecBLow,
                     dvecBUp,
                     dvecBIn,
					 dvecBStep,
                     &dvecBOut,            
                     &dMapObjOut,
                     &drowMapObj_bOut,
                     &dmatMapObj_b_bOut,
                     withD );
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }


  //------------------------------------------------------------
  // Known values.
  //------------------------------------------------------------

  valarray<double> dvecBKnown( nB );

  // Compute the bKnown and mapObj(bKnown).  Note that the last 
  // element of bKnown is constrained by its upper bound, i.e.,
  // it is not equal to the last element of bTrue.
  double dConstTerm   = 0.5 * ( nY + nB ) * log( 2.0 * PI );
  double dMapObjKnown = dConstTerm;
  for ( i = 0; i < nB - 1; i++ )
  {
    dvecBKnown[i]  = 0.5  * ( i + 1 );
    dMapObjKnown    += 0.25 * ( i + 1 ) * ( i + 1 );
  }
  dvecBKnown[nB - 1]  = dvecBUp[nB - 1];
  dMapObjKnown         += 0.25 * ( nB * nB + 1 );
  valarray<double> dmatMapObj_b_bKnown( nB * nB );
  identity( nB, dmatMapObj_b_bKnown );
  dmatMapObj_b_bKnown *= 2.0;


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(  dMapObjOut, 
              dMapObjKnown, 
              indOptimizer.getEpsilon(), 
              dvecBLow, 
              dvecBUp, 
              dvecBOut, 
              dvecBKnown, 
              dmatMapObj_b_bOut, 
              dmatMapObj_b_bKnown );
  
}


/*************************************************************************
 *
 * Function: funR
 *
 *
 * Calculates 
 *
 *     R(b) = I    ,
 *             nY
 * where
 *
 *     nY  =  number of elements in y.
 *
 *************************************************************************/

static valarray<double> fitindividualquadratictest::funR( const valarray<double> &dvecB )
{
  valarray<double> I( nY * nY );
  identity( nY, I );
  return I;
}


/*************************************************************************
 *
 * Function: funR_b
 *
 *
 * Calculates 
 *
 *     R_b(b) = 0  .
 *
 *************************************************************************/

static valarray<double> fitindividualquadratictest::funR_b( const valarray<double> &dmatR, 
                            const valarray<double> &dvecB )
{
  valarray<double> dmatR_b( 0.0, dmatR.size() * dvecB.size() );
  return dmatR_b;
}


/*************************************************************************
 *
 * Function: funF
 *
 *
 * Calculates
 *                                                                   T
 *     f(b)  =  ( b   , b   , ... , b    , nB + 1, nB + 2, ... , nY )   .
 *                 (1)   (2)         (nB)
 *
 *************************************************************************/

static valarray<double> fitindividualquadratictest::funF( const valarray<double> &dvecB )
{
  int i;

  assert( dvecB.size() == nB );
  assert( nB < nY );

  valarray<double> dvecF( nY );

  for ( i = 0; i < nB; i++ )
  {
    dvecF[i] = dvecB[i];
  }
  for ( i = nB; i < nY; i++ )
  {
    dvecF[i] = i + 1;
  }

  return dvecF;
}


/*************************************************************************
 *
 * Function: funF_b
 *
 *
 * Calculates
 *
 *                 -        -
 *                | I      0 |
 *     f_b(b)  =  |  nB      |  ,
 *                |          |
 *                | 0      0 |
 *                 -        -
 *
 * where
 *
 *     nB  =  number of elements in b.
 *
 *************************************************************************/

static valarray<double> fitindividualquadratictest::funF_b( const valarray<double> &dvecF, 
                                                            const valarray<double> &dvecB )
{
  valarray<double> dvecF_b( 0.0, nY * nB );

  for ( int i = 0; i < nB; i++ )
  {
    dvecF_b[i + i * nY ] = 1.0;
  }

  return dvecF_b;

}


/*************************************************************************
 *
 * Function: fitIndividualZeroIterationsTest
 *
 *
 * This test calls fitIndividual with zero iterations.
 *
 *************************************************************************/

void fitIndividualTest::fitIndividualZeroIterationsTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace fitindividualzeroiterations;

  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  int nY = 2;
  valarray<double> dvecY( 2., nY );


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  int nB = 2;
  valarray<double> dmatD( nB * nB );
  identity( nB, dmatD );

  valarray<double> dvecBLow ( -4.0, nB );
  valarray<double> dvecBUp  (  4.0, nB );
  valarray<double> dvecBIn  (  2.0, nB );
  valarray<double> dvecBOut (       nB );
  valarray<double> dvecBStep( .001, nB );


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

    UserModelFitIndividualZeroIterationsTest model( nB, nY );

  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double dMapObjOut;

  valarray<double> drowMapObj_bOut  ( nB );
  valarray<double> dmatMapObj_b_bOut( nB * nB );


  //------------------------------------------------------------
  // Remaining inputs to fitIndividual.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.e-3, 0, 0 );
  bool withD      = true;


  //------------------------------------------------------------
  // Optimize MapObj(b).
  //------------------------------------------------------------

  try{
      fitIndividual( model,
                     dvecY,
                     indOptimizer,
                     dvecBLow,
                     dvecBUp,
                     dvecBIn,
					 dvecBStep,
                     &dvecBOut,            
                     &dMapObjOut,
                     &drowMapObj_bOut,
                     &dmatMapObj_b_bOut,
                     withD );
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }


  //------------------------------------------------------------
  // Known values.
  //------------------------------------------------------------

  DoubleMatrix Y( dvecY );

  // Since the number of iterations for the objective function is
  // zero, bOut and bHat should both be equal to bIn.
  DoubleMatrix BKnown( dvecBIn );
  valarray<double> dvecBKnown = dvecBIn;
  double dMapObjKnown;
  DoubleMatrix MapObj_bKnown( 1, nB );

  // Compute the known values for mapObj and mapObj_b.
  bool okMapObj;
  try{
      mapObj( model, 
              Y, 
              BKnown,
              &dMapObjKnown,
              &MapObj_bKnown,
              withD,
              false,
              NULL
              );
      okMapObj = true;
  }
  catch(...)
  {
      CPPUNIT_ASSERT(false);
  }
      valarray<double> drowMapObj_bKnown;
      MapObj_bKnown.toValarray( drowMapObj_bKnown );
      
  // The second derivative matrix should be an identity matrix
  // since only the last term in MapObj(b), 
  //
  //     1  T  -1
  //     - b  D   b  ,
  //     2
  //
  // depends on b.
      valarray<double> dmatMapObj_b_bKnown( nB * nB );
  identity( nB, dmatMapObj_b_bKnown );

  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(  dMapObjOut, 
              dMapObjKnown, 
              indOptimizer.getEpsilon(), 
              dvecBLow, 
              dvecBUp, 
              dvecBOut, 
              dvecBKnown, 
              dmatMapObj_b_bOut, 
              dmatMapObj_b_bKnown );
  
}


/*************************************************************************
 *
 * Function: funR
 *
 *
 * Calculates
 *
 *            /  1     0  \ 
 *     R(b) = |           |  .
 *            \  0     1  / 
 *
 *************************************************************************/

static valarray<double> fitindividualzeroiterations::funR( const valarray<double> &dvecB )
{
  valarray<double> I( 2 * 2 );
  identity( 2, I );
  return I;
}


/*************************************************************************
 *
 * Function: funR_b
 *
 *
 * Calculates
 *
 *              /  0     0  \ 
 *     R_b(b) = |  0     0  |  .
 *              |  0     0  | 
 *              \  0     0  / 
 *
 *************************************************************************/

static valarray<double> fitindividualzeroiterations::funR_b( const valarray<double> &dmatR, 
                                                             const valarray<double> &dvecB )
{
  valarray<double> dmatR_b( 0.0, dmatR.size() * dvecB.size() );
  return dmatR_b;
}


/*************************************************************************
 *
 * Function: funF
 *
 *
 * Calculates
 *
 *            / 2 \ 
 *     f(b) = |   |   .
 *            \ 2 /
 *
 *************************************************************************/

static valarray<double> fitindividualzeroiterations::funF( const valarray<double> &dvecB )
{
  valarray<double> dvecF( 2.0, 2 );
  return dvecF;
}


/*************************************************************************
 *
 * Function: funF_b
 *
 *
 * Calculates
 *
 *              / 0   0 \ 
 *     f_b(b) = |       |   .
 *              \ 0   0 /
 *
 *************************************************************************/

static valarray<double> fitindividualzeroiterations::funF_b( const valarray<double> &dvecF, 
                                                             const valarray<double> &dvecB )
{
  valarray<double> dmatF_b( 0.0, 4 );
  return dmatF_b;
}


/*************************************************************************
 *
 * Function: doTheTest
 *
 *************************************************************************/

void fitIndividualTest::doTheTest(
								   double dMapObjOut,
								   double dMapObjKnown,
								   double epsilon,
								   const valarray<double>& dvecBLow,
								   const valarray<double>& dvecBUp,
								   const valarray<double>& dvecBOut,
								   const valarray<double>& dvecBKnown,
								   const valarray<double>& dmatMapObj_b_bOut,
								   const valarray<double>& dmatMapObj_b_bKnown
                                 )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nB = dvecBOut.size();

  //------------------------------------------------------------
  // Check the final parameter value.
  //------------------------------------------------------------

  // Check to see if any elements of bOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(bOut - bKnown) <= epsilon (bUp - bLow)
  //
  bool isConverged = true;
  for (int i = 0; i < nB; i++ )
  {
    if( fabs(dvecBOut[i] - dvecBKnown[i]) > 
            epsilon * (dvecBUp[i] - dvecBLow[i]) )
    {
      isConverged = false;
    }
  }
  CPPUNIT_ASSERT(isConverged);
  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------
/*
  cout << setiosflags(ios::scientific) << setprecision(15);
  cout << endl;
  cout << "bOut = " << endl;
  DoubleMatrix BOut( dvecBOut );
  BOut.print(); 
  cout << "bKnown = " << endl;
  DoubleMatrix BKnown( dvecBKnown );
  BKnown.print(); 
  cout << endl;
  cout << "mapObj(bOut) = " << dMapObjOut << endl;
  cout << "MapObjKnown  = " << dMapObjKnown << endl;
  cout << endl;

  if ( nB < 5 )
  {
    cout << "MapObj_b_bOut  = " << endl;
	DoubleMatrix MapObj_b_bOut( dmatMapObj_b_bOut, nB );
    MapObj_b_bOut.print();
    cout << "MapObj_b_bKnown  = " << endl;
	DoubleMatrix MapObj_b_bKnown( dmatMapObj_b_bKnown, nB );
    MapObj_b_bKnown.print();
    cout << endl;
  }

  cout << "epsilon = " << epsilon  << endl;
  cout << endl;
*/ 
}

