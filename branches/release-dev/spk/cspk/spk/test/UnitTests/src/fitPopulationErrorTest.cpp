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
 * File: fitPopulationErrorTest.cpp
 *
 *
 * Test cases for forcing fitPopulation() to generate errors
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>
#include <string>
#include <cmath>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include <spk/randNormal.h>
#include <spk/DoubleMatrix.h>
#include <spk/fitPopulation.h>
#include <spk/SpkValarray.h>
#include <spk/namespace_population_analysis.h>
#include <spk/SpkException.h>

#include "fitPopulationErrorTest.h"

using SPK_VA::valarray;
using namespace std;
using namespace CppUnit;

using namespace population_analysis;

// Number of individuals.
static const int nInd = 10;

// Number of measurements for each individual
static const int nYi = 1;

// Number of measurements in total
static const int nY = nInd * nYi;

// Number of population parameter
static const int nAlp = 2;

// Number of individual parameter
static int nB = 1;

/*************************************************************************
 *
 * Function: fitPopulationExampleTest
 *
 *
 * This test implements the example problem from the fitPopulation specification. 
 *
 *************************************************************************/

class fitPopulationErrorTestModel : public SpkModel<double>
{
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    fitPopulationErrorTestModel(int nA, int nB, int nYi)
      :
      _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {};    
    ~fitPopulationErrorTestModel(){};
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

void fitPopulationErrorTest::setUp()
{
    // initializations
}
void fitPopulationErrorTest::tearDown()
{
    // clean up
}

Test* fitPopulationErrorTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite("fitPopulationErrorTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<fitPopulationErrorTest>(
                 "fitPopulationUserInputValidatations", 
                 &fitPopulationErrorTest::fitPopulationUserInputValidatations));
    suiteOfTests->addTest(new TestCaller<fitPopulationErrorTest>(
                 "fitPopulationOptimizerStressTest", 
                 &fitPopulationErrorTest::fitPopulationOptimizerStressTest));

    return suiteOfTests;
}

void fitPopulationErrorTest::fitPopulationUserInputValidatations()
{
    validateAllLegal();

    validateN();
    validateEpsilon();
    validateY();
    validateMaxItr();
    validateLevel();
    validateAlpLow();
    validateAlpUp();
    validateAlpIn();
    validateAlpStep();
    validateBLow();
    validateBUp();
    validateBIn();
    validateBStep();
}
void fitPopulationErrorTest::fitPopulationOptimizerStressTest()
{
    tooManyIter();
}

/****************************************************************************
 *
 *  private members : actual bodies of test cases
 *
 ****************************************************************************/
void fitPopulationErrorTest::tooManyIter()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);
    
    //
    // Reduce the maximum #of iterations allowed before convergence
    // so small that the population-level optimization complains "too much!"
    // The second element of the array below is the value
    // set for the individual level.
    //
//     info.indOptimizer.setNMaxIter( 1 );
    //
    // Reduce the maximum #of iterations allowed before convergence
    // so small that the population-level optimization complains "too much!"
    // The second element of the array below is the value
    // set for the population level.
    //
    info.popOptimizer.setNMaxIter( 2 );
		//
		// [ Revisit --- Jiaji, 10/25/2002 ]
		// If popOptimizer.setIsWarmStart() is called , Too-Many-Iter at population
	    // level optimization is not treated as an error.  The state info for the 
	    // later warm start will be stored in the popOptimizer object and returned 
	    // by fitpopulation().  However, if Too-Many-Iter occurs at the individual
	    // level ( sub-level ) optimization, it is still treated as an error.
		//
    try{
		// Population level test
        runFitPopulation(MODIFIED_LAPLACE, info);
	CPPUNIT_ASSERT_MESSAGE("It should have reached the max iteration# before convergence in poplation level test.", 
			       info.popOptimizer.getIsTooManyIter() );
        
	CPPUNIT_ASSERT_MESSAGE( "The number of iterations consumed should have been equal to the max #iterations in population level test.", 
				info.popOptimizer.getNIterCompleted() == info.popOptimizer.getNMaxIter( ) );

		// Individual level test
        info.popOptimizer.setNMaxIter( 40 );
        info.indOptimizer.setNMaxIter(  1 );
        runFitPopulation(MODIFIED_LAPLACE, info);
/*	    CPPUNIT_ASSERT_MESSAGE(         
	    "It should have reached the max iteration# before convergence in individual level test.", 
	    info.indOptimizer.getIsTooManyIter() );

	    CPPUNIT_ASSERT_MESSAGE( "The number of iterations consumed should have been equal to the max #iterations in indvidual level test.", 
	    info.indOptimizer.getNIterCompleted() == info.indOptimizer.getNMaxIter( ) );

*/	}
    catch( SpkException& e )
    {
        SpkError err = e[ 0 ];
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_TOO_MANY_ITER, err.code());
//        CPPUNIT_ASSERT( false );
        return;
    }
}


void fitPopulationErrorTest::validateAllLegal()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);
    
    try{
        runFitPopulation(MODIFIED_LAPLACE, info);
    }
    catch(...)
    {
        CPPUNIT_ASSERT(true);
    }
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(...)
    {
        CPPUNIT_ASSERT(true);
    }
    CPPUNIT_ASSERT(true);
}
//
// [ Commented out by Sachiko ]
// Ever since Objective is enumulated, the validation is done
// at the compilation time without us intervine.
// So, this test case is unnecessary.
//
/*
void fitPopulationErrorTest::validateObjective()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);
    
    try{
        runFitPopulation("Borgus", info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }
    catch(...)
    {
        CPPUNIT_ASSERT(true);
    }
    CPPUNIT_ASSERT(true);
}
*/
void fitPopulationErrorTest::validateN()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);

    // Make the length of N vector larger than a correct length.
    info.N.resize(nInd + 1);
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateEpsilon()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);
/*
    // The vector for epsilons has to be a 2 dimensional column vector.
    // Make it violate the requirement.
    info.dvecEpsilon.resize(3,1);
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
    }

    // The vector for epsilons has to be a column vector.
    // Make it violate the requirement.
    info.dvecEpsilon.resize(2,2);
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
    }
*/
    // Every element must be greater than zero.
    // Make it violate the requirement.

    info.indOptimizer.setEpsilon( -1.0);
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }

    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateY()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);

    //
    // Make the length of Y vector larger than a legal value.
    info.Y.resize(nY * nY);
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }
    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateMaxItr()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);
/*
    // This vector has to be a column vector.
    // Make it violate the requirement.
    info.dvecNMaxIter.resize(1,2);
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
    }

    // This vector has to be 2 dimensional.
    // Make it violate the requirement.
    info.dvecNMaxIter.resize(3,1);
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
    }
*/
    // Every element of this vector has to be greater than or equal to zero.
    // Make it violate the requirement.

    info.indOptimizer.setNMaxIter( -1 );

    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }

    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateLevel()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);
/*
    // This vector has to be a column vector.
    // Make it violate the requirement.
    info.dvecLevel.resize(1,2);
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
    }

    // This vector has to be 2 dimensional.
    // Make it violate the requirement.
    info.dvecLevel.resize(3,1);
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
    }
*/
    // Every element of this vector has to be greater than or equal to zero.
    // Make it violate the requirement.

    info.indOptimizer.setLevel( -1 );

    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }

    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateAlpLow()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);

    //
    // This vector must have the same number of rows as nAlp.
    // Make it violate the requirement.
    //
    info.alpLow.resize( nAlp + 1 );
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }
    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateAlpUp()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);

    //
    // This vector must have the same number of rows as nAlp.
    // Make it violate the requirement.
    //
    info.alpUp.resize( nAlp + 1 );
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }
    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateAlpIn()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);

    //
    // This vector must have the same number of rows as nAlp.
    // Make it violate the requirement.
    //
    info.alpIn.resize( nAlp + 1 );
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
    }

    // Every element of this vector must lay between the lower
    // boundary and upper boundary values (inclusive).
    info.alpIn.resize( nAlp );
    info.alpIn[0] = info.alpLow[0] - 1.0;
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
    }  
    info.alpIn[0] = info.alpUp[0] + 1.0;
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }  

    
    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //    
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateAlpStep()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);

    //
    // This vector must have the same number of rows as nAlp.
    // Make it violate the requirement.
    //
    info.alpStep.resize( nAlp + 1 );
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }
    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateBLow()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);

    //
    // This vector must have the same number of rows as nB.
    // Make it violate the requirement.
    //
    info.bLow.resize( nB + 1 );
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }
    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateBUp()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);

    //
    // This vector must have the same number of rows as nB.
    // Make it violate the requirement.
    //
    info.bUp.resize( nB + 1 );
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }
    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(true);
}
void fitPopulationErrorTest::validateBIn()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);

    // The length of the vector must be the product of nB * nInd.
    // Make it violate the requirement.
    info.bIn.resize( nB * nInd + 1 );
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
    }

    // Every element of this vector must lay between the lower
    // boundary and upper boundary values (inclusive).
    info.bIn.resize( nInd * nB );
    info.bIn[0] = info.bLow[0] - 1.0;
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
    }  
    info.bIn[0] = info.bUp[0] + 1.0;
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }  

    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(false);
}
void fitPopulationErrorTest::validateBStep()
{
    fitpopulationerrortest::Info info;
    setAllValid(info);

    //
    // This vector must have the same number of rows as nB.
    // Make it violate the requirement.
    //
    info.bStep.resize( nB + 1 );
    try{
        runFitPopulation(EXPECTED_HESSIAN, info);
    }
    catch(SpkException& e)
    {
        SpkError err = e.pop();
        CPPUNIT_ASSERT_EQUAL(SpkError::SPK_USER_INPUT_ERR, err.code());
        return;
    }
    //
    // All of the above cases should have caught an exception and
    // the last catch statement should let this function go out of scope.
    // If the control reaches here, that means exceptions did not
    // get thrown as planned.
    //
    CPPUNIT_ASSERT(false);
}
/**********************************************************************************/
void fitPopulationErrorTest::runFitPopulation(enum Objective whichObjective, fitpopulationerrortest::Info &info)
{
    fitPopulationErrorTestModel model( nAlp, nB, nYi );
    fitPopulation(
        model,
        whichObjective,
        info.N,
        info.Y,
        info.popOptimizer,
        info.alpLow,
        info.alpUp,
        info.alpIn,
        info.alpStep,
        &(info.alpOut),
		info.indOptimizer,
        info.bLow,
        info.bUp,
        info.bIn,
        info.bStep,
        &(info.bOut),
        &(info.dLTildeOut),
        &(info.lTilde_alpOut),
        &(info.lTilde_alp_alpOut)
    );
}
    
void fitPopulationErrorTest::setAllValid(fitpopulationerrortest::Info& info)
{
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    int i;

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
    info.Y.resize( nY );

    // Number of measurements for each individual. 
    info.N.resize( nInd );
    info.N = 1;

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
    info.alpLow.resize( nAlp );
    info.alpUp.resize( nAlp );
    info.alpIn.resize( nAlp );
    info.alpOut.resize( nAlp );
    info.alpStep.resize( nAlp );

    // Set the values associated with alp(1).
    alpTrue[ 0 ] = meanBetaTrue;
    info.alpLow [ 0 ] = -10.0;
    info.alpUp  [ 0 ] = 10.0;
    info.alpIn  [ 0 ] = -1.0;
    info.alpStep[ 0 ] = 1.0e-2;

    // Set the values associated with alp(2).
    alpTrue[ 1 ] = varBetaTrue;
    info.alpLow [ 1 ] = 1.0e-3;
    info.alpUp  [ 1 ] = 100.0;
    info.alpIn  [ 1 ] = 0.5;
    info.alpStep[ 1 ] = 1.0e-2;


    //------------------------------------------------------------
    // Quantities related to the random population parameters, b.
    //------------------------------------------------------------

    info.bLow.resize( nB );
    info.bUp.resize( nB );
    info.bStep.resize( nB );

    info.bLow = -1.5e+1;
    info.bUp  = +1.0e+1;
    info.bStep = 1.0e-2;

    info.bIn.resize ( nB * nInd );
    info.bOut.resize( nB * nInd );

    info.bIn = 1.0;


    //------------------------------------------------------------
    // Quantities related to the population objective function.
    //------------------------------------------------------------

    info.lTilde_alpOut.resize( nAlp );
    info.lTilde_alp_alpOut.resize( nAlp * nAlp );


    //------------------------------------------------------------
    // Remaining inputs to fitPopulation.
    //------------------------------------------------------------

    Optimizer indOptimizer( 1.0e-6, 40, 0 );
	Optimizer popOptimizer( 1.0e-6, 40, 0 );
    info.indOptimizer = indOptimizer;
    info.popOptimizer = popOptimizer;

}
