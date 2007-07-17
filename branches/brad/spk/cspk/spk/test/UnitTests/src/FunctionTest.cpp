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
 * File: FunctionTest.cpp
 *
 *
 * Test cases for Function class.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>

#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/Function.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/identity.h"
#include "../../../spk/allZero.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/SpkValarray.h"

#include "FunctionTest.h"

using SPK_VA::valarray;
using namespace CppUnit;
using namespace std;

//
// global values
//
static valarray<double> Y;
static const int nY      = 3;
static const int nPopPar = 4;
static const int nIndPar = 2;
static valarray<double> POP;
static valarray<double> IND;

class FunctionTest::PopModel : public SpkModel<double>
{
private:
    valarray<double> _a, _b;
    int _i;

private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& x)
    {
        _a = x;
    }
    void doSetIndPar(const valarray<double>& x)
    {
        _b = x;
    }
public:
    PopModel( int nPopParIn, int nIndParIn ) : _a(nPopParIn), _b(nIndParIn) {};
    ~PopModel() throw() {};
    PopModel(const PopModel& right) : _a(right._a), _b(right._b)
    {
    }
    PopModel operator=(const PopModel& right)
    {
        _a = right._a;
        _b = right._b;
        return *this;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        // vector valued function
        // 
        // odd when the index is _a[0]^2
        // even when the index is _b[0]^2
        //
        ret.resize(Y.size());
        for(int i=0; i<Y.size(); i++)
        {
            ret[i] = (i%2==0? _b[0]*_b[0] : _a[0]* _a[0]);
        }
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        bool isAllZero = (_b[0] == 0.0? true : false);

        ret.resize(Y.size() * _b.size());
        for(int j=0; j<_b.size(); j++)
        {
            for(int i=0; i<Y.size(); i++)
            {
                ret[i] = (i%2==0? 2.0*_b[0] : 0);
            }
        }
        return !isAllZero;
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        bool isAllZero = (_b[0] == 0.0? true : false);

        ret.resize(Y.size() * _a.size());
        for(int j=0; j<_a.size(); j++)
        {
            for(int i=0; i<Y.size(); i++)
            {
                ret[i] = (i%2==0? 0 :  2.0*_a[0]);
            }
        }
        return !isAllZero;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        // matrix valued function
        ret.resize(Y.size() * Y.size());
        for(int j=0; j<Y.size(); j++)
        {
            for(int i=0; i<Y.size(); i++)
                ret[i+j*Y.size()] = (i==j? _b[1]* _b[1] : _a[i]*_a[i]);
        }
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        bool isAllZero = (_b[1] == 0.0? true : false);

        int n = Y.size()*Y.size() * _b.size();
        ret.resize(n);

        for(int j=0; j<_b.size(); j++)
        {
            for(int i=0; i<Y.size()*Y.size(); i++)
            {
                ret[i+j*Y.size()*Y.size()] = (j==1? _b[1] : 0);        
            }
        }
        return !isAllZero;
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        int n = Y.size()*Y.size() * _a.size();
        ret.resize(n);

        for(int j=0; j<_a.size(); j++)
        {
            for(int i=0; i<Y.size()*Y.size(); i++)
            {
                ret[i+j*Y.size()*Y.size()] = (j==1? 0 : _a[j]);        
            }
        }
        return !allZero(_a);
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        ret.resize( _b.size() );
        ret = identity(_b.size()).toValarray();
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        int n = _b.size()*_b.size() * _a.size();
        ret.resize(n);
        for( int i=0; i<n; i++ )
          ret[i] = 0.0;
        return false;
    }
};

void FunctionTest::setUp()
{
    int i;

    Y.resize(nY);
    Y = 0.1;

    IND.resize(nIndPar);
    for(i=1; i<=nIndPar; i++)
    {
        IND[i-1] = 1.0/i;
    }

    POP.resize(nPopPar);
    for(i=1; i<=nPopPar; i++)
    {
        POP[i-1] = i;
    }

}
void FunctionTest::tearDown()
{
    // clean up
}

Test* FunctionTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite("FunctionTest");

    suiteOfTests->addTest(new TestCaller<FunctionTest>(
			 "testDataMean", 
                         &FunctionTest::testDataMean));

    suiteOfTests->addTest(new TestCaller<FunctionTest>(
                         "testDataMean_indPar",
                         &FunctionTest::testDataMean_indPar));

    suiteOfTests->addTest(new TestCaller<FunctionTest>(
                         "testDataMean_popPar", 
                         &FunctionTest::testDataMean_popPar));


    suiteOfTests->addTest(new TestCaller<FunctionTest>(
                         "testDataCovariance", 
                         &FunctionTest::testDataCovariance));

    suiteOfTests->addTest(new TestCaller<FunctionTest>(
                         "testDataCovariance_indPar", 
                         &FunctionTest::testDataCovariance_indPar));

    suiteOfTests->addTest(new TestCaller<FunctionTest>(
                         "testDataCovariance_popPar", 
                         &FunctionTest::testDataCovariance_popPar));

    suiteOfTests->addTest(new TestCaller<FunctionTest>(
                         "testIndParCovariance", 
                         &FunctionTest::testIndParCovariance));

    suiteOfTests->addTest(new TestCaller<FunctionTest>(
                         "testIndParCovariance_popPar", 
                         &FunctionTest::testIndParCovariance_popPar));

    return suiteOfTests;
}


void FunctionTest::testDataMean()
{
    using namespace std;

    int i,j;

    //
    // testing f()
    //
    for( j=0; j<3; j++ )
    {
        // Because this loop changes the length of the individual
        // parameter vector, construct the model object here so that 
        // it will be destructed and then re-constructed each time.
        PopModel model( nPopPar, nIndPar+j );
        ModelFunctionValarray f(&PopModel::dataMean, &model);

        model.selectIndividual(0);
        valarray<double> ind( j, nIndPar+j );

	valarray<double> expected( nY );        
	valarray<double> actual = f(POP, ind);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.dataMean(expected);

        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);


        binder1st<ModelFunctionValarray> fPop = bind1st(f,POP);

        actual   = fPop(ind);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.dataMean(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder2nd<ModelFunctionValarray> fInd = bind2nd(f,ind);
        actual   = fInd(POP);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.dataMean(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);
    }
}
void FunctionTest::testDataMean_indPar()
{
    using namespace std;

    int i,j;

    //
    // testing f_b()
    //
    for( j=0; j<3; j++ )
    {
        // Because this loop changes the length of the individual
        // parameter vector, construct the model object here so that 
        // it will be destructed and then re-constructed each time.
        PopModel model( nPopPar, nIndPar+j );
        model.selectIndividual(0);

        ModelDerivativeValarray f_b(&PopModel::dataMean_indPar, &model);
        valarray<double> ind(j, nIndPar+j);

	valarray<double> expected( nY * ind.size() );
	valarray<double> actual = f_b(POP, ind);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.dataMean_indPar(expected);

        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder1st<ModelDerivativeValarray> f_bPop = bind1st(f_b,POP);
        actual   = f_bPop(ind);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.dataMean_indPar(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder2nd<ModelDerivativeValarray> f_bInd = bind2nd(f_b,ind);
        actual   = f_bInd(POP);

        model.setPopPar(POP);

        model.setIndPar(ind);

        model.dataMean_indPar(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);
    }
}

void FunctionTest::testDataMean_popPar()
{
    using namespace std;

    int i,j;
    //
    // testing f_a()
    //
    for( j=0; j<3; j++ )
    {
        // Because this loop changes the length of the population
        // parameter vector, construct the model object here so that 
        // it will be destructed and then re-constructed each time.
        PopModel model( nPopPar+j, nIndPar );
        model.selectIndividual(0);

        ModelDerivativeValarray f_a(&PopModel::dataMean_popPar, &model);
    
        valarray<double> pop(j, nPopPar+j);

	valarray<double> expected( nY * pop.size() );
	valarray<double> actual   = f_a(pop, IND);

        model.setPopPar(pop);
        model.setIndPar(IND);
        model.dataMean_popPar(expected);

        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder1st<ModelDerivativeValarray> f_aPop = bind1st(f_a, pop);
        actual   = f_aPop(IND);

        model.setPopPar(pop);
        model.setIndPar(IND);
        model.dataMean_popPar(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder2nd<ModelDerivativeValarray> f_aInd = bind2nd(f_a,IND);
        actual   = f_aInd(pop);

        model.setPopPar(pop);
        model.setIndPar(IND);
        model.dataMean_popPar(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);
    }
}
void FunctionTest::testDataCovariance()
{
    using namespace std;

    int i,j;

    //
    // testing R()
    //
    for( j=0; j<3; j++ )
    {
        // Because this loop changes the length of the individual
        // parameter vector, construct the model object here so that 
        // it will be destructed and then re-constructed each time.
        PopModel model( nPopPar, nIndPar );
        model.selectIndividual(0);

        ModelFunctionValarray R(&PopModel::dataVariance, &model);
        valarray<double> ind(j, nIndPar+j);

	valarray<double> expected( nY * nY );
        valarray<double> actual   = R(POP, ind);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.dataVariance(expected);

        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder1st<ModelFunctionValarray> RPop = bind1st(R, POP);
        actual   = RPop(ind);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.dataVariance(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder2nd<ModelFunctionValarray> RInd = bind2nd(R, ind);
        actual   = RInd(POP);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.dataVariance(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);
    }
}
void FunctionTest::testDataCovariance_indPar()
{
    using namespace std;

    int i,j;

    //
    // testing R_b()
    //
    for( j=0; j<3; j++ )
    {
        // Because this loop changes the length of the individual
        // parameter vector, construct the model object here so that 
        // it will be destructed and then re-constructed each time.
        PopModel model( nPopPar, nIndPar+j );
        model.selectIndividual(0);

        ModelDerivativeValarray R_b(&PopModel::dataVariance_indPar, &model);

        valarray<double> ind(j, nIndPar+j);

	valarray<double> expected( nY * nY * ind.size() );
        valarray<double> actual   = R_b(POP, ind);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.dataVariance_indPar(expected);

        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder1st<ModelDerivativeValarray> R_bPop = bind1st(R_b, POP);
        actual   = R_bPop(ind);

        model.setPopPar(POP);

        model.setIndPar(ind);

        model.dataVariance_indPar(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder2nd<ModelDerivativeValarray> R_bInd = bind2nd(R_b, ind);
        actual   = R_bInd(POP);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.dataVariance_indPar(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);
    }
}

void FunctionTest::testDataCovariance_popPar()
{
    using namespace std;

    int i,j;

    //
    // testing R_a()
    //
    for( j=0; j<3; j++ )
    {
        // Because this loop changes the length of the population
        // parameter vector, construct the model object here so that 
        // it will be destructed and then re-constructed each time.
        PopModel model( nPopPar+j, nIndPar );
        model.selectIndividual(0);

        ModelDerivativeValarray R_a(&PopModel::dataVariance_popPar, &model);
        valarray<double> pop(j, nPopPar+j);
        
        valarray<double> expected( nY * nY * pop.size() );
        valarray<double> actual  = R_a(pop, IND);

        model.setPopPar(pop);
        model.setIndPar(IND);
        model.dataVariance_popPar(expected);

        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
        {
            if( expected[i] != actual[i] )
            {
                cerr << "expected = " << expected[i] << endl;
                cerr << "actual   = " << actual[i] << endl;
                CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);
            }
        }

        binder1st<ModelDerivativeValarray> R_aPop = bind1st(R_a, pop);
        actual   = R_aPop(IND);

        model.setPopPar(pop);
        model.setIndPar(IND);
        model.dataVariance_popPar(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder2nd<ModelDerivativeValarray> R_aInd = bind2nd(R_a, IND);
        actual   = R_aInd(pop);

        model.setPopPar(pop);
        model.setIndPar(IND);
        model.dataVariance_popPar(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);
    }
}
void FunctionTest::testIndParCovariance()
{
    using namespace std;

    int i,j;

    //
    // testing D()
    //
    for( j=0; j<3; j++ )
    {
        // Because this loop changes the length of the individual
        // parameter vector, construct the model object here so that 
        // it will be destructed and then re-constructed each time.
        PopModel model( nPopPar, nIndPar+j );
        model.selectIndividual(0);

        ModelFunctionValarray D(&PopModel::indParVariance, &model);
        valarray<double> ind(j, nIndPar+j);

	valarray<double> expected( ind.size() * ind.size() );
        valarray<double> actual = D( POP, ind );

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.indParVariance(expected);

        //CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
	for( i=0; i<expected.size(); i++ )
	     CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder1st<ModelFunctionValarray> DPop = bind1st(D, POP);
        actual   = DPop(ind);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.indParVariance(expected);
    
	//        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder2nd<ModelFunctionValarray> DInd = bind2nd(D, ind);
        actual   = DInd(POP);

        model.setPopPar(POP);
        model.setIndPar(ind);
        model.indParVariance(expected);
    
	//        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);
    }
}

void FunctionTest::testIndParCovariance_popPar()
{
    using namespace std;

    int i,j;

    //
    // testing D_a()
    //
    for( j=0; j<3; j++ )
    {
        // Because this loop changes the length of the population
        // parameter vector, construct the model object here so that 
        // it will be destructed and then re-constructed each time.
        PopModel model( nPopPar+j, nIndPar );
        model.selectIndividual(0);

        ModelDerivativeValarray doIndParVariance_popPar(&PopModel::indParVariance_popPar, &model);
        valarray<double> pop(j, nPopPar+j);

	valarray<double> expected( nIndPar * nIndPar * pop.size() );
        valarray<double> actual   = doIndParVariance_popPar(pop, IND);

        model.setPopPar(pop);
        model.setIndPar(IND);
        model.indParVariance_popPar(expected);

        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder1st<ModelDerivativeValarray> D_aPop = bind1st(doIndParVariance_popPar, pop);
        actual   = D_aPop(IND);

        model.setPopPar(pop);
        model.setIndPar(IND);
        model.indParVariance_popPar(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);

        binder2nd<ModelDerivativeValarray> D_aInd = bind2nd(doIndParVariance_popPar, IND);
        actual   = D_aInd(pop);

        model.setPopPar(pop);
        model.setIndPar(IND);
        model.indParVariance_popPar(expected);
    
        CPPUNIT_ASSERT_EQUAL(expected.size(), actual.size());
        for( i=0; i<expected.size(); i++ )
            CPPUNIT_ASSERT_EQUAL(expected[i], actual[i]);
    }
}
