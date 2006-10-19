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
 * File: centdiffForSpkModelMembersTest.cpp
 *
 *
 * Test centdiff() that evaluates SpkModel member functions.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#pragma warning( disable : 4786 )
#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>
#include <functional>
#include <cmath>
#include <cfloat>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/centdiff.h"
#include "centdiffModelTest.h"

#include "../../../spk/SpkValarray.h"
#include "../../../spk/inverse.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/det.h"
#include "../../../spk/elsq.h"
#include "../../../spk/elsq_x.h"
#include "../../../spk/identity.h"
#include "../../../spk/subtract.h"
#include "../../../spk/transposeRowBlocks.h"
#include "../../../spk/mapObj.h"
#include "../../../spk/mapObjDiff.h"

#include "../../../spk/lambda.h"
#include "../../../spk/Function.h"

using SPK_VA::valarray;
using namespace CppUnit;

void centdiffModelTest::setUp()
{
    // initializations
}
void centdiffModelTest::tearDown()
{
    // clean up
}

Test* centdiffModelTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite("centdiffModelTest");

    suiteOfTests->addTest(new TestCaller<centdiffModelTest>(
                         "vectorValuedModelCentdiffTest", 
                         &centdiffModelTest::vectorValuedModelCentdiffTest));
    /*
    suiteOfTests->addTest(new TestCaller<centdiffModelTest>(
                         "matrixValuedModelCentdiffTest", 
                         &centdiffModelTest::matrixValuedModelCentdiffTest));
    suiteOfTests->addTest(new TestCaller<centdiffModelTest>(
                         "matrixValuedDerivativeCentdiffTest", 
                         &centdiffModelTest::matrixValuedDerivativeCentdiffTest));
    */
    return suiteOfTests;
}
/**************************************************************************
 * modelMemberCentdiffTest:
 * 
 * central difference approximation for a model member function
 **************************************************************************/

namespace modelCentdiffTestNamespace
{

};
    class ModelCentdiffTestModel : public SpkModel<double>
    {
        valarray<double> _a, _b;
        int _i;
        const int _nAlp;
        const int _nB;
        const int _nY;
    public:
        ModelCentdiffTestModel( int nAlp, int nB, int nY)
          : _nAlp(nAlp), _nB(nB), _nY(nY), _a(nAlp), _b(nB)
        {};    
        ~ModelCentdiffTestModel(){};

        //
        // For debugging approximation for 2nd derivative
        //
        bool dataVariance_popPar_popPar( valarray<double>& ret ) const
        {
        
          //
          // R_a_a = [ 0       0 ]
          //         [ 0       0 ]
          //         [ 0       0 ]
          //         [ 0       0 ]
          //         [ 0       0 ]
          //         [ 0       0 ]
          //         [ 2       0 ]
          //         [ 0       0 ]
          //
          ret.resize( _nY * _nY * _nAlp * _nAlp );
          ret = 0.0;
          ret[6] = 2;

          return true;
        }
        //
        // For debugging approximation for 2nd derivative
        //
        bool dataVariance_indPar_indPar( valarray<double>& ret ) const
        {
        
          //
          // R_b_b = [ 2       0       0 ]
          //         [ 0       0       0 ]
          //         [ 0       0       0 ]
          //         [ 0       0       0 ]
          //         [ 0       0       0 ]
          //         [ 0       0       0 ]
          //         [ 0       0       0 ]
          //         [ 0       0       0 ]
          //         [ 0       0       0 ]
          //         [ 0       0       0 ]
          //         [ 0       0       0 ]
          //         [ 0       0       0 ]
          //
          ret.resize( _nY * _nY * _nB * _nB );
          ret = 0.0;
          ret[0] = 2;
          return true;
        }
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

        void doDataMean( valarray<double>& ret ) const
        {
            //
            // f(a, b) = [ a(2)^2 + b(2)^2 ]
            //           [ a(2)^2 + b(2)^2 ]
            //

            ret.resize( _nY );

            ret[0] = _a[1]*_a[1] + _b[1]*_b[1];
            ret[1] = _a[1]*_a[1] + _b[1]*_b[1];
        }
        bool doDataMean_popPar( valarray<double>& ret ) const
        {
            //
            // f(a, b)_a = [ 0  2*a(2) ]
            //             [ 0  2*a(2) ]
            //
            ret.resize( _nY * _nAlp);

            ret[0] = 0.0;
            ret[1] = 0.0;
            ret[2] = 2.0 * _a[1];
            ret[3] = 2.0 * _a[1];
            return true;
        }
        bool doDataMean_indPar( valarray<double>& ret ) const
        {
            //
            // f(a, b)_b = [ 0  2*b(2)  0 ]
            //             [ 0  2*b(2)  0 ]
            //
            ret.resize(_nY * _nB);

            ret[0] = 0.0;
            ret[1] = 0.0;
            ret[2] = 2.0 * _b[1];
            ret[3] = 2.0 * _b[1];
            ret[4] = 0.0;
            ret[5] = 0.0;
            return true;
        }
        void doDataVariance( valarray<double>& ret ) const
        {
            //
            // R(a, b) = [ b(1)^2     0    ]
            //           [   0      a(1)^2 ]
            //
            ret.resize(_nY * _nY);

            ret[0] = _b[0] * _b[0];
            ret[1] = 0.0;
            ret[2] = 0.0;
            ret[3] = _a[0] * _a[0];
        }
        bool doDataVariance_popPar( valarray<double>& ret ) const
        {
            //
            // R_a = [ 0       0 ]
            //       [ 0       0 ]
            //       [ 0       0 ]
            //       [ 2*a(1)  0 ]
            //
            ret.resize(_nY * _nY * _nAlp);
            ret = 0.0;

            ret[3] = 2.0 * _a[0];
            return true;

        }
        bool doDataVariance_indPar( valarray<double>& ret ) const
        {
            //
            // R_b   = [ 2*b(1)  0      0 ]
            //         [ 0       0      0 ]
            //         [ 0       0      0 ]
            //         [ 0       0      0 ]
            //
            ret.resize(_nY * _nY * _nB);
            ret = 0.0;

            ret[0] = 2.0 * _b[0];
            return true;
        } 
        void doIndParVariance( valarray<double>& ret ) const
        {
            //
            // D = [a(0)   0   ]
            //     [   0   a(0)]
            //
            ret.resize( _nB * _nB );

            ret[0] = _a[0];
            ret[1] = 0.0;
            ret[2] = 0.0;
            ret[3] = _a[0];
        }
        bool doIndParVariance_popPar( valarray<double>& ret ) const
        {
            //
            // D(a)a = [ 1  0 ]
            //         [ 0  0 ]
            //         [ 0  0 ]
            //         [ 1  0 ]
            //
            int n = _nB * _nB * _nAlp;
            ret.resize( n );
            for( int i=0; i<n; i++ )
              ret[i] = 0.0;

            ret[0] = 1.0;
            ret[3] = 1.0;

            return true;
        }
    };
//*************************************************************************
// modelMemberCentdiffTest:
// 
// central difference approximation for a model member function
//*************************************************************************


void centdiffModelTest::vectorValuedModelCentdiffTest()
{
    using namespace std;
    using namespace modelCentdiffTestNamespace;

    int i,j;
    const int nAlp = 2;
    const int nB = 3;
    const int nY = 2;
    const double step = 0.3;

    valarray<double> pop(2.0, nAlp);
    valarray<double> popStep(step, nAlp);

    valarray<double> ind(1.0, nB);
    valarray<double> indStep(step, nB);

    ModelCentdiffTestModel model( nAlp, nB, nY );
     
    ModelFunctionValarray fOb(&ModelCentdiffTestModel::dataMean, &model);

    valarray<double> f_bApprox(nY*nB);
    f_bApprox = centdiff< binder1st<ModelFunctionValarray> > ( bind1st(fOb, pop), 1, ind, indStep );
    valarray<double> f_aApprox = centdiff< binder2nd<ModelFunctionValarray> > (bind2nd(fOb, ind), 1, pop, popStep);

    model.setPopPar(pop);
    model.setIndPar(ind);
    
    valarray<double> f_bExact;
    model.dataMean_indPar(f_bExact);

    CPPUNIT_ASSERT_EQUAL(f_bExact.size(), f_bApprox.size() );
    
    valarray<double> f_aExact;
    model.dataMean_popPar(f_aExact);
    
    CPPUNIT_ASSERT_EQUAL(f_aExact.size(), f_aApprox.size() );

    for(j=0; j<nB; j++ )
    {
        for( i = 0; i<nY; i++ )
        {
	  double expected = f_bExact[i + j * nY];
	  double actual   = f_bApprox[i + j * nY];
	  CPPUNIT_ASSERT_DOUBLES_EQUAL( expected, actual, (expected==0.0? 0.001 : fabs(expected)*0.001) );
        }
    }

    for(j=0; j<nAlp; j++ )
    {
        for( i = 0; i<nY; i++ )
        {
	  double expected = f_aExact[i + j * nY];
	  double actual   = f_aApprox[i + j * nY];

	  CPPUNIT_ASSERT_DOUBLES_EQUAL( expected, actual, (expected==0.0? 0.001 : fabs(expected)*0.001) );
       }
    }

    //
    // Compare the f_bApprox against a previously obtained result.
    //
    // The benchmark used here was taken on 06/20/2002 with DoubleMatrix version of centdiff().
    //
    // 
    // [ 0.0000000000000000e+000 2.0000000000000004e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 2.0000000000000004e+000 0.0000000000000000e+000 ]

    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, f_bApprox[0] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, f_bApprox[1] );
    CPPUNIT_ASSERT_EQUAL( 2.0000000000000004e+000, f_bApprox[2] );
    CPPUNIT_ASSERT_EQUAL( 2.0000000000000004e+000, f_bApprox[3] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, f_bApprox[4] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, f_bApprox[5] );

    //
    // Compare the f_aApprox against a previously obtained result.
    //
    // The benchmark used here was taken on 06/20/2002 with DoubleMatrix version of centdiff().
    //
    // 
    // [ 0.0000000000000000e+000 3.9999999999999991e+000 ]
    // [ 0.0000000000000000e+000 3.9999999999999991e+000 ] 
    //
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, f_aApprox[0] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, f_aApprox[1] );
    CPPUNIT_ASSERT_EQUAL( 3.9999999999999991e+000, f_aApprox[2] );
    CPPUNIT_ASSERT_EQUAL( 3.9999999999999991e+000, f_aApprox[3] );
}

void centdiffModelTest::matrixValuedModelCentdiffTest()
{
    using namespace std;
    using namespace modelCentdiffTestNamespace;

    int i,j;
    const int nAlp = 2;
    const int nB = 3;
    const int nY = 2;
    const double step = 0.3;

    valarray<double> pop(2.0, nAlp);
    valarray<double> popStep(step, nAlp);

    valarray<double> ind(1.0, nB);
    valarray<double> indStep(step, nB);

    ModelCentdiffTestModel model( nAlp, nB, nY );

    ModelFunctionValarray ROb(&ModelCentdiffTestModel::dataVariance, &model);

    valarray<double> R_bApprox = centdiff< binder1st<ModelFunctionValarray> > (bind1st(ROb, pop), nY, ind, indStep);
    valarray<double> R_aApprox = centdiff< binder2nd<ModelFunctionValarray> > (bind2nd(ROb, ind), nY, pop, popStep);

    model.setPopPar(pop);
    model.setIndPar(ind);
    
    valarray<double> R_bExact;
    model.dataVariance_indPar( R_bExact );
    valarray<double> R_aExact;
    model.dataVariance_popPar( R_aExact );

    for( j=0; j<nB; j++ )
    {
      for(i=0; i<nY * nY; i++ )
      {
	double expected = R_bExact[i + j * nY * nY];
	double actual   = R_bApprox[i + j * nY * nY];
	
	CPPUNIT_ASSERT_DOUBLES_EQUAL( expected, actual, (expected==0.0? 0.001 : fabs(expected)*0.001) );
      }
    }
    for( j=0; j<nAlp; j++ )
    {
      for(i=0; i<nY * nY; i++ )
      {
	double expected = R_aExact[i + j * nY * nY];
	double actual   = R_aApprox[i + j * nY * nY];
	
	CPPUNIT_ASSERT_DOUBLES_EQUAL( expected, actual, (expected==0.0? 0.001 : fabs(expected)*0.001) );
      }
    }
    //
    // Compare the R_bApprox against a previously obtained result.
    //
    // The benchmark used here was taken on 06/20/2002 with DoubleMatrix version of centdiff().
    //
    // 
    // [ 2.0000000000000004e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //
    CPPUNIT_ASSERT_EQUAL( 2.0000000000000004e+000, R_bApprox[0] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_bApprox[1] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_bApprox[2] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_bApprox[3] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_bApprox[4] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_aApprox[5] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_bApprox[6] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_bApprox[7] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_bApprox[4] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_bApprox[5] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_bApprox[6] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_bApprox[7] );

    //
    // Compare the R_aApprox against a previously obtained result.
    //
    // The benchmark used here was taken on 06/20/2002 with DoubleMatrix version of centdiff().
    //
    // 
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 3.9999999999999991e+000 0.0000000000000000e+000 ]
    //
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_aApprox[0] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_aApprox[1] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_aApprox[2] );
    CPPUNIT_ASSERT_EQUAL( 3.9999999999999991e+000, R_aApprox[3] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_aApprox[4] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_aApprox[5] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_aApprox[6] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_aApprox[7] );
}
void centdiffModelTest::matrixValuedDerivativeCentdiffTest()
{
    using namespace std;
    using namespace modelCentdiffTestNamespace;

    int i,j;
    const int nAlp = 2;
    const int nB = 3;
    const int nY = 2;
    const double step = 0.3;

    valarray<double> pop(2.0, nAlp);
    valarray<double> popStep(step, nAlp);

    valarray<double> ind(1.0, nB);
    valarray<double> indStep(step, nB);

    ModelCentdiffTestModel model( nAlp, nB, nY );

    ModelDerivativeValarray R_bOb(&ModelCentdiffTestModel::dataVariance_indPar, &model);
    ModelDerivativeValarray R_aOb(&ModelCentdiffTestModel::dataVariance_popPar, &model);

    valarray<double> R_b_bApprox = centdiff< binder1st<ModelDerivativeValarray> > (bind1st(R_bOb, pop), nY, ind, indStep);
    valarray<double> R_a_aApprox = centdiff< binder2nd<ModelDerivativeValarray> > (bind2nd(R_aOb, ind), nY, pop, popStep);

    model.setPopPar(pop);
    model.setIndPar(ind);
    
    valarray<double> R_b_bExact;
    model.dataVariance_indPar_indPar( R_b_bExact );
    valarray<double> R_a_aExact;
    model.dataVariance_popPar_popPar( R_a_aExact );

    for( j=0; j<nB; j++ )
    {
      for(i=0; i<nY * nY * nB; i++ )
      {
	double expected = R_b_bExact[i + j * nY * nY * nB ];
	double actual   = R_b_bApprox[i + j * nY * nY * nB];
	CPPUNIT_ASSERT_DOUBLES_EQUAL( expected, actual, (expected==0.0? 0.001 : fabs(expected)*0.001 ) );
      }
    }
    for( j=0; j<nAlp; j++ )
    {
      for(i=0; i<nY * nY * nAlp; i++ )
      {
	double expected = R_a_aExact[i + j * nY * nY * nAlp ];
	double actual   =  R_a_aApprox[i + j * nY * nY * nAlp];
	CPPUNIT_ASSERT_DOUBLES_EQUAL( expected, actual, (expected==0.0? 0.001 : fabs(expected)*0.001 ) );
      }
    }
    //
    // Compare the R_b_bApprox against a previously obtained result.
    //
    // The benchmark used here was taken on 06/20/2002 with DoubleMatrix version of centdiff().
    //
    // 
    //  [ 2.0000000000000004e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //  [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //
    CPPUNIT_ASSERT_EQUAL( 2.0000000000000004e+000, R_b_bApprox[0] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[1] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[2] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[3] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[4] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[5] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[6] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[7] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[8] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[9] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[10] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[11] );

    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[12] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[13] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[14] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[15] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[16] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[17] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[18] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[19] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[20] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[21] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[22] );

    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[23] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[24] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[25] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[26] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[27] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[28] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[29] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[30] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[31] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[32] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_b_bApprox[33] );

    //
    // Compare the R_a_aApprox against a previously obtained result.
    //
    // The benchmark used here was taken on 06/20/2002 with DoubleMatrix version of centdiff().
    //
    // 
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 ]
    // [ 1.9999999999999996e+000 0.0000000000000000e+000 ]
    // [ 0.0000000000000000e+000 0.0000000000000000e+000 ]
    //

    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[0] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[1] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[2] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[3] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[4] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[5] );
    CPPUNIT_ASSERT_EQUAL( 1.9999999999999996e+000, R_a_aApprox[6] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[7] );

    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[8] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[9] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[10] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[11] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[12] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[13] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[14] );
    CPPUNIT_ASSERT_EQUAL( 0.0000000000000000e+000, R_a_aApprox[15] );
}
