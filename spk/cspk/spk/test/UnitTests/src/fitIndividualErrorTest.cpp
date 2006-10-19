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
 * File: fitIndividualErrorTest.cpp
 *
 *
 * Test cases that force fitIndividual() to generate errors
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/fitIndividual.h"
#include "fitIndividualErrorTest.h"

#include <cmath>

#include "../../../spk/SpkValarray.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/SpkException.h"
#include "../../../spk/mulByScalar.h"

using SPK_VA::valarray;
using namespace CppUnit;

namespace // [Begin: unnamed namespace]
{
const int    nY = 2;
const int    nB = 4;

double epsilon;
int    maxItr;
int    level;
valarray<double> y;
valarray<double> bLow;
valarray<double> bUp;
valarray<double> bIn;
valarray<double> bStep;
valarray<double> bOut;
double mapObjOut;
valarray<double> mapObj_bOut;
valarray<double> mapObj_b_bOut;
bool withD;

} // [End: unnamed namespace]

void fitIndividualErrorTest::setUp()
{
    // initializations
}
void fitIndividualErrorTest::tearDown()
{
    // clean up
}

Test* fitIndividualErrorTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite("fitIndividualErrorTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<fitIndividualErrorTest>(
                         "fitIndividualInputValidationTest", 
                         &fitIndividualErrorTest::fitIndividualInputValidationTest));
    suiteOfTests->addTest(new TestCaller<fitIndividualErrorTest>(
                         "fitIndividualOptimizerStressTest",
                         &fitIndividualErrorTest::fitIndividualOptimizerStressTest));

    return suiteOfTests;
}


class fitIndividualErrorTestModel : public SpkModel<double>
{
    valarray<double> _b;
    const int _nY;
    const int _nB;

public:
    fitIndividualErrorTestModel(int nB, int nY) : _nB(nB), _nY(nY), _b(nB) {};
    ~fitIndividualErrorTestModel(){};
private:
    void doSetIndPar(const valarray<double>& bval)
    {
        assert(bval.size() == _nB);
        _b = bval;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        //
        //            / b(2) \ 
        //     f(b) = |      |   .
        //            \ b(2) /
        //
        ret.resize(_nY);
        ret[0] = _b[1];
        ret[1] = _b[1];
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        //              / 0   1 \ 
        //     f_b(b) = |       |   .
        //              \ 0   1 /
        ret.resize(_nY * _nB);
 
        ret[0] = 0.0;
        ret[1] = 0.0;
        ret[2] = 1.0;
        ret[3] = 1.0;

        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
      //
      //            /  exp[b(1)]     0  \ 
      //     R(b) = |                   |   .
      //            \  0      exp[b(1)] / 
      //
      ret.resize( _nY * _nY );
  
      ret[0] = exp( _b[0] );
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = exp( _b[0] );
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        //
        //              /  exp[b(1)]     0  \ 
        //     R_b(b) = |  0             0  |   .
        //              |  0             0  | 
        //              \  exp[b(1)]     0  / 
        //
        ret.resize( _nY * _nY * _nB );

        for( int i=0; i<_nY*_nY*_nB; i++ )
          ret[ i ]  = 0.0;

        ret[0] = exp( _b[0] );
        ret[3] = exp( _b[0] );
        return true;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      //
      //               /  1.0 / exp[b(1)]     0  \ 
      //     R(b)^-1 = |                         |   .
      //               \  0      1.0 / exp[b(1)] / 
      //
      ret.resize( _nY * _nY );
  
      ret[0] = 1.0 / exp( _b[0] );
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 1.0 / exp( _b[0] );
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        //
        //                   /  -1.0 / exp[b(1)]^2     0  \ 
        //     R^(-1)_b(b) = |  0                      0  |   .
        //                   |  0                      0  | 
        //                   \  -1.0 / exp[b(1)]^2     0  / 
        //
        ret.resize( _nY * _nY * _nB );

        for( int i=0; i<_nY*_nY*_nB; i++ )
          ret[ i ]  = 0.0;

        ret[0] = -1.0 / ( exp( _b[0] ) * exp( _b[0] ) );
        ret[3] = -1.0 / ( exp( _b[0] ) * exp( _b[0] ) );
        return true;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        //
        //          /  1.0    0.0  \
        //   DOut = |              |
        //          \  0.0    1.0  /
        //
        ret.resize(_nB * _nB);
        for(int j=0; j<_nB; j++ )
        {
          for(int i=0; i<_nB; i++)
          {
            ret[i+j*_nB] = (i==j? 1.0 : 0.0);
          }
        }
    }
};

static fitIndividualErrorTestModel* pModel;

void fitIndividualErrorTest::fitIndividualInputValidationTest()
{
    // This unit test tests whether fitIndividual() rejects invalid input values properly.
    // All test cases will send invalid values to see if an exception gets thrown.
    // The input data validations are done at the very top of the routine (fitIndividual).
    // Therefore, it shouldn't matter whether valid data really make sense scientifically
    // or mathematically at all.  They don't get evaluated anyway.

    pModel = new fitIndividualErrorTestModel(nB,nY);

    validateAllLegal();
    validateEpsilon();
    validateMaxItr();
    validateLevel();    
    //validateY();        
    validateBLow();     
    validateBUp();      
    validateBIn();      
    validateBStep();    

    delete pModel;
}
void fitIndividualErrorTest::fitIndividualOptimizerStressTest()
{
    pModel = new fitIndividualErrorTestModel(nB,nY);

    tooManyIterationError();

    delete pModel;
}
/*
 *********************************************************************************
 *
 *   Test cases for fitIndividualOptimizerStressTest()
 *
 *********************************************************************************
 */
void fitIndividualErrorTest::tooManyIterationError()
{
    setAllValid();

    //
    // Set the maximum number of iterations allowed before convergence to 
    // a unrealistically small so that the optimizer complains.
    //

	maxItr = 1;
    Optimizer optimizer( epsilon, maxItr, 0 );

    try
    {
        //
        // Limit the maximum number of iteration by iteself is not 
        // enough to cause "too many itr" error with this particular model.
        // So, re-adjust the initial value so that it requires more
        // iterations to converge.
        //
        bIn = bIn * 1.2;
        pModel->setIndPar( bIn );

	// Set the optimizer flag so that it won't throw an exception
	// when the maximum number of iterations is exceeded.
        optimizer.setThrowExcepIfMaxIter( false );

        fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( SpkException& e )
    {
      using std::cerr;
      using std::endl;

        int index = e.find(SpkError::SPK_TOO_MANY_ITER);
        CPPUNIT_ASSERT( index < 0 );
        cerr << e;
        CPPUNIT_ASSERT( false );
        return;
    }

    CPPUNIT_ASSERT_MESSAGE( "fitIndividual() should have reported Too Many Iteration# in a graceful manner.", 
			    optimizer.getIsTooManyIter() );

    CPPUNIT_ASSERT_MESSAGE( "fitIndividual() should have consumed the max# of iterations.", 
			    optimizer.getNIterCompleted() == maxItr );
}

/*
 *********************************************************************************
 *
 *   Test cases for fitIndividualInputValidationTest()
 *
 *********************************************************************************
 */

void fitIndividualErrorTest::validateAllLegal()
{
    setAllValid();
    try
    {
		Optimizer optimizer( epsilon, maxItr, level );
        pModel->setIndPar( bIn );
        fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( ... )
    {
        CPPUNIT_ASSERT(false);
    }
    CPPUNIT_ASSERT(true);

}
void fitIndividualErrorTest::validateEpsilon()
{
    
    setAllValid();

    // epsilon has to be greater than 0 and less than or equal to 1.0.
    // set a value violating the lower boundary
    epsilon = 0.0;
    try
    {
        Optimizer optimizer( epsilon, maxItr, level );
        pModel->setIndPar( bIn );
        fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( const SpkException& )
    {
        // Good.  This is expected.
        CPPUNIT_ASSERT(true);
        return;
    }
    CPPUNIT_ASSERT(false);

    // set a value violating the upper boundary
    epsilon = 1.1;
    try
    {
        Optimizer optimizer( epsilon, maxItr, level );
        pModel->setIndPar( bIn );
        fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( const SpkException& )
    {
        // Good. This is expected.
        return;
    }
    CPPUNIT_ASSERT(false);
}

void fitIndividualErrorTest::validateMaxItr()
{
    // maxItr >= 0

    setAllValid();

    // set an illigal value
    maxItr = -1;
    try
    {
		Optimizer optimizer( epsilon, maxItr, level );
        pModel->setIndPar( bIn );
        fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( const SpkException& )
    {
        CPPUNIT_ASSERT(true);
        return;
    }
    CPPUNIT_ASSERT(false);
}
void fitIndividualErrorTest::validateLevel()
{
    // level >= 0

    setAllValid();

    // set an illigal value
    level = -1;
    try
    {
		Optimizer optimizer( epsilon, maxItr, level );
        pModel->setIndPar( bIn );
        fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( const SpkException& )
    {
        CPPUNIT_ASSERT(true);
        return;
    }
    CPPUNIT_ASSERT(false);
}

//
// [ Commented out by Sachiko, 10/10/2002 ]
// This test used to check if y is a column vector.
// It is no longer relevant because the data type holding y is now valarray.
//
// Further more, the length of Y cannot be derived from any other arguments passed to fitIndividual().
// Therefore, fitIndividual() cannot really validate the vector length at its level
// unless it executes a user implemented SpkModel function such as f().
//
// So, I disabled it.
//
/*
void fitIndividualErrorTest::validateY()
{
    setAllValid();

    // The length of y must be equal to the number of measurements (y).
    // Set it to violate the rule.
    y.resize(nY + 1);
    try
    {
		Optimizer optimizer( epsilon, maxItr, level );
        pModel->setIndPar( bIn );
        fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( const SpkException& )
    {
        // Good.  This is expected.
        CPPUNIT_ASSERT(true);
        return;
    }
    CPPUNIT_ASSERT(false);
}
*/
void fitIndividualErrorTest::validateBLow()
{
    setAllValid();

    // The length of bLow must be equal to the length of b.
    // Set it to violate the rule.
    bLow.resize(nB + 1);
    try
    {
		Optimizer optimizer( epsilon, maxItr, level );
        pModel->setIndPar( bIn );
        fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( const SpkException& )
    {
        // Good.  This is expected.
        CPPUNIT_ASSERT(true);
        return;
    }
    CPPUNIT_ASSERT(false);
}
void fitIndividualErrorTest::validateBUp()
{
    setAllValid();

    // The length of bUp must be equal to the length of b.
    // Set it to violate the rule.
    bUp.resize(nB + 1);
    try
    {
	   Optimizer optimizer( epsilon, maxItr, level );
       pModel->setIndPar( bIn );
       fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( const SpkException& )
    {
        // Good.  This is expected.
        CPPUNIT_ASSERT(true);
        return;
    }
    CPPUNIT_ASSERT(false);
}
void fitIndividualErrorTest::validateBIn()
{
    setAllValid();

    // The length of bUp must be equal to the length of b.
    // Set it to violate the rule.
    bIn.resize(nB + 1);
    try
    {
		Optimizer optimizer( epsilon, maxItr, level );
        fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( const SpkException& )
    {
        // Good.  This is expected.
        CPPUNIT_ASSERT(true);
        return;
    }
    CPPUNIT_ASSERT(false);
}
void fitIndividualErrorTest::validateBStep()
{
    setAllValid();

    // The length of bUp must be equal to the length of b.
    // Set it to violate the rule.
    bStep.resize(nB + 1);
    try
    {
		Optimizer optimizer( epsilon, maxItr, level );
        pModel->setIndPar( bIn );
       fitIndividual(*pModel, y, optimizer, bLow, bUp, bIn, bStep, &bOut, &mapObjOut, &mapObj_bOut, &mapObj_b_bOut, withD);
    }
    catch( const SpkException& )
    {
        // Good.  This is expected.
        CPPUNIT_ASSERT(true);
        return;
    }
    CPPUNIT_ASSERT(false);
}

void fitIndividualErrorTest::setAllValid()
{
    //------------------------------------------------------------
    // Quantities related to the data vector, y.
    //------------------------------------------------------------

    y.resize( nY );
    y = 2.0;

    //------------------------------------------------------------
    // Quantities related to the objective function parameter, b.
    //------------------------------------------------------------

    //nB = 2;

    bLow .resize( nB );
    bUp  .resize( nB );
    bIn  .resize( nB );
    bOut .resize( nB );
    bStep.resize( nB );

    bLow  = -4.0;
    bUp   =  4.0;
    bIn   =  2.0;
    bStep =  0.001;


    //------------------------------------------------------------
    // Remaining inputs to fitIndividual.
    //------------------------------------------------------------

    epsilon  = 1.e-3; 
    maxItr   = 0; 
    level    = 0;
    withD    = false;

    bOut.resize( nB );
    mapObjOut = 0.0;
    mapObj_bOut.resize( nB );
    mapObj_b_bOut.resize( nB * nB );
}
