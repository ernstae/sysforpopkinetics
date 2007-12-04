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
 * File: lTildePvmTest.cpp
 *
 *
 * Unit test for lTildePvm based on lTildeTest.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/
#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <cmath>
#include <limits>
#include <cassert>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/divByScalar.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/add.h"
#include "../../../spk/subtract.h"
#include "../../../spk/pi.h"
#include "../../../spk/matabs.h"
#include "../../../spk/isLessThanOrEqualTo.h"
#include "../../../spk/allTrue.h"
#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/isDblEpsEqual.h"
#include "../../../spk/Objective.h"
#include "../../../spk/NaiveFoModel.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/lTildePvm.h"

#include "lTildePvmTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

/*==========================================================================
 *
 * Commonly accessed variables/constants
 *
 *==========================================================================*/
namespace {
  const int nIndividuals = 2;
  const int nAlp         = 1;
  const int nB           = 1;

  const double eps = 1e-5;
  const int mitr   = 40;
  const int level  = 0;

  valarray<double> alp(nAlp);
  valarray<double> bIn(nB * nIndividuals);
  valarray<double> bLow(nB);
  valarray<double> bUp(nB);
  valarray<double> bStep(nB);
  valarray<double> N( nIndividuals );
  valarray<double> y;

  DoubleMatrix bOutExpected;
  double       LOutExpected;
  DoubleMatrix L_alpOutExpected;

  DoubleMatrix laplaceBOut;
  double       laplaceLOut;
  DoubleMatrix laplaceL_alpOut;
  DoubleMatrix laplaceLi_alpOut;

  DoubleMatrix foceBOut;
  double       foceLOut;
  DoubleMatrix foceL_alpOut;
  DoubleMatrix foceLi_alpOut;

  DoubleMatrix foBOut;
  double       foLOut;
  DoubleMatrix foL_alpOut;
  DoubleMatrix foLi_alpOut;
};

/*==========================================================================
 *
 * User-defined Model
 *
 *   In this example,
 *   the model function is linear,
 *   and the data variance does not depend on the random effects.
 *   Thus the three objective functions actually have the same value.
 *
 *==========================================================================*/
class DiagDLTildePvmTest : public SpkModel<double>
{
    valarray<double> _alp, _b;
    int _i;
public:
    DiagDLTildePvmTest()
    {};    


    ~DiagDLTildePvmTest(){};
private:
    void doSelectIndividual(int i)
    {
        _i = i;
    }
    void doSetPopPar(const valarray<double>& alp)
    {
        //
        // In this example, alp is a single element vector.
        //
        // alp = [ alp(1) ]
        //
        assert( alp.size() == 1 );
        _alp.resize( alp.size() );
        _alp = alp;
    }
    void doSetIndPar(const valarray<double>& b)
    {
        //
        // In this example, b is a single element vector.
        //
        // b = [ b(1) ]
        //
        assert( b.size() == 1 );
	_b.resize( b.size() );
        _b = b;
    }
    void doIndParVariance( valarray<double>& DOut ) const
    {
        //
        // D(alp) = alp
        //        = [ alp(1) ]
        //
      DOut.resize( _b.size() * _b.size() );
      DOut = _alp;

    }
    bool doIndParVariance_popPar( valarray<double>& D_alpOut ) const
    {
        //
        // D(alp)_alp = [ 1 ];
        //
        D_alpOut.resize( _b.size() * _b.size() * _alp.size() );
        D_alpOut = 1.0;
        return true;
    }
    void doDataMean( valarray<double>& fOut ) const
    {
        //
        // f(alp,b) = [ b(1) ]
        //            [ b(1) ]
        //
        fOut.resize( 2 );
        fOut = _b[0];
    }
    bool doDataMean_popPar( valarray<double>& f_alpOut ) const
    {
        //
        // f(alp,b)_alp = [ 0 ]
        //                [ 0 ]
        f_alpOut.resize( 2 );
        f_alpOut = 0.0;
        return false;
    }
    bool doDataMean_indPar( valarray<double>& f_bOut ) const
    {
        //
        // f(alp,b)_b = [ 1 ]
        //              [ 1 ]
        f_bOut.resize( 2 );
        f_bOut = 1.0;
        return true;
    }
    void doDataVariance( valarray<double>& ROut ) const
    {
        //
        // R(alp,b) = [ 1  0 ]
        //            [ 0  2 ]
        //
        ROut.resize( 4 );
        ROut[0] = 1.0;
        ROut[1] = 0.0;
        ROut[2] = 0.0;
        ROut[3] = 2.0;
    }
    bool doDataVariance_popPar( valarray<double>& R_alpOut ) const
    {
        //
        // R(alp,b)_alp = [ 0 ]
        //                [ 0 ]
        //                [ 0 ]
        //                [ 0 ]
        R_alpOut.resize( 4 );
        R_alpOut = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& R_bOut ) const
    {
        //
        // R(alp,b)_b   = [ 0 ]
        //                [ 0 ]
        //                [ 0 ]
        //                [ 0 ]
        R_bOut.resize( 4 );
        R_bOut = 0.0;
        return false;
    } 
};
class FullDUserModelLTildePvmTest : public SpkModel<double>
{
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    FullDUserModelLTildePvmTest(int nA, int nB, int nYi)
      : _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {};    


    ~FullDUserModelLTildePvmTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& aval)
    {
      _a.resize( aval.size() );
        _a = aval;
    }
    void doSetIndPar(const valarray<double>& bval)
    {
      _b.resize( bval.size() );
        _b = bval;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        // DOut =
        //
        // [ _a[0]  _a[1] ]
        // [ _a[1]  _a[0] ]
        //
        ret.resize(_nB * _nB);
        ret[0] = _a[0];
        ret[1] = _a[1];
        ret[2] = _a[1];
        ret[3] = _a[0];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        // D_aOut = 
        // 
        // [ 1   0 ]
        // [ 0   1 ]
        // [ 0   1 ]
        // [ 1   0 ]
        //
        ret.resize(_nB * _nB * _nA);
        ret = 0.0;
        ret[0] = 1.0;
        ret[3] = 1.0;
        ret[5] = 1.0;
        ret[6] = 1.0;

        return true;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        // fOut =
        // 
        // [ b[0] ]
        // [ b[0] ]
        //
        ret.resize(_nYi);
        ret[0] = _b[0];
        ret[1] = _b[0];
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        // f_aOut =
        //
        // [ 0  0 ]
        // [ 0  0 ]
        //
        ret.resize(_nYi * _nA);
        ret = 0.0;
        return false;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        // f_bOut =
        //
        // [ 1  0 ]
        // [ 1  0 ]
        //
        ret.resize(_nYi * _nB);
        ret = 0.0;
        ret[0] = 1.0;
        ret[1] = 1.0;
        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        // ROut =
        //
        // [ 1  0 ]
        // [ 0  2 ]
        //
        ret.resize(_nYi * _nYi);
        ret[0] = 1.0;
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 2.0;
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        // R_aOut =
        //
        // [ 0  0 ]
        // [ 0  0 ]
        // [ 0  0 ]
        // [ 0  0 ]
        //
        ret.resize(_nYi * _nYi * _nA);
        ret = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        // R_bOut =
        //
        // [ 0  0 ]
        // [ 0  0 ]
        // [ 0  0 ]
        // [ 0  0 ]
        //
        ret.resize(_nYi * _nYi * _nB);
        ret = 0.0;
        return false;
    }
 
};
/*************************************************************************
 *
 * File: lTildePvmTest.cpp
 *
 *
 * Test cases for lTildePvm
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

Test* lTildePvmTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("lTildePvmTest");

  suiteOfTests->addTest(new TestCaller<lTildePvmTest>("testLaplace",         &lTildePvmTest::testLaplace));
  suiteOfTests->addTest(new TestCaller<lTildePvmTest>("testHessian",         &lTildePvmTest::testHessian));
  suiteOfTests->addTest(new TestCaller<lTildePvmTest>("testNaiveFirstOrder", &lTildePvmTest::testNaiveFirstOrder));
  suiteOfTests->addTest(new TestCaller<lTildePvmTest>("cmpAllObjectives",    &lTildePvmTest::cmpAllObjectives));

    return suiteOfTests;
}

void lTildePvmTest::tearDown()
{
  system("rm indDriver");
}
//
// This test verifies the three objective values are all the same.
// 
void lTildePvmTest::testLaplace()
{
  diagDTest(MODIFIED_LAPLACE, laplaceBOut, laplaceLOut, laplaceL_alpOut, laplaceLi_alpOut);
}
void lTildePvmTest::testHessian()
{
  diagDTest(EXPECTED_HESSIAN, foceBOut, foceLOut, foceL_alpOut, foceLi_alpOut);
}
void lTildePvmTest::testNaiveFirstOrder()
{
  diagDTest(NAIVE_FIRST_ORDER, foBOut,foLOut, foL_alpOut, foLi_alpOut);
}
void lTildePvmTest::cmpAllObjectives()
{ 
  int i,j;

  //
  // Compare bOut
  //
  double tolB;
  double tolBMax = 0.0;
  for( j=0; j<nIndividuals; j++ )
  {
    for( i=0; i<nB; i++ )
    {
      // The optimizer called by lTildePvm will accept a value bOut as
      // an estimate for bHat if 
      //
      //     abs( bOut - bHat )  <=  epsilon ( bUp - bLow )  ,
      //
      // where abs is the element-by-element absolute value function
      // and bHat is a local minimizer of the objective function.
      tolB = eps * fabs( bUp[i] - bLow[i] );

      CPPUNIT_ASSERT_DOUBLES_EQUAL( bOutExpected.data()[i+j*nB], laplaceBOut.data()[i+j*nB], tolB );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( bOutExpected.data()[i+j*nB], foceBOut.data()[i+j*nB], tolB );
      CPPUNIT_ASSERT_DOUBLES_EQUAL( bOutExpected.data()[i+j*nB], foBOut.data()[i+j*nB], tolB );

      if ( tolB > tolBMax )
      {
        tolBMax = tolB;
      }
    }
  }
  //
  // Compare LOut
  //
  CPPUNIT_ASSERT_DOUBLES_EQUAL( LOutExpected, laplaceLOut, 1.0e-10 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( LOutExpected, foceLOut, 1.0e-10 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( LOutExpected, foLOut, 1.0e-10 );
  //
  // Compare L_alpOut
  //
  for( i=0; i<nAlp; i++ )
  {
    CPPUNIT_ASSERT_DOUBLES_EQUAL( L_alpOutExpected.data()[i], laplaceL_alpOut.data()[i], tolBMax );
    CPPUNIT_ASSERT_DOUBLES_EQUAL( L_alpOutExpected.data()[i], foceL_alpOut.data()[i], tolBMax );
    CPPUNIT_ASSERT_DOUBLES_EQUAL( L_alpOutExpected.data()[i], foL_alpOut.data()[i], tolBMax );
  }
}
void lTildePvmTest::setUp()
{
  using namespace std;

  // Build indDriver
  if(system("g++ indOptPvmDriver.cpp -o indDriver -I../../../ -L../../../spk -L/usr/local/lib/spktest -L/usr/lib/atlas -I/usr/share/pvm3/include -L/usr/share/pvm3/lib/LINUX -L/usr/share/pvm3/lib/LINUX64 -lpvm3 -lspk -lxerces-c -latlas -lginac -lQN01Box -lgsl -llapack -llapack_atlas -lcblas") != 0)
  {
    cout << "indDriver failed to build" << endl;
    abort();
  }

  //
  // set the vector containing the number of parameters for each individual's data.
  //
  //   N = [ 2 ]  // 1st individual's #measurements
  //       [ 2 ]  // 2nd individual's #measurements
  //
  // NOTE: this reads the first two elements of _y are for the 1st patient and
  //       the next two elements (3-4) of _y are for the 2nd patient.
  //valarray<double> N( nIndividuals );
  N.resize( nIndividuals );
  N = 2;


  //
  //set individuals' data to:
  //
  //   y = [ 1 ]  // 1st individual's 1st data
  //       [ 1 ]  // 1st individual's 2nd data
  //       [ 1 ]  // 2nd individual's 1st data
  //       [ 1 ]  // 2nd individual's 2nd data
  //
  y.resize( static_cast<int>( N.sum() ) );
  y = 1.0;
  //
  // set alp to:
  //  alp = [ 2.1 ]
  //
  //valarray<double> alp(nAlp); 
  alp.resize(1);
  alp = 2.1;

  // 
  // set bIn to:
  //  bIn = [ 0.0  0.0 ]
  //
  //valarray<double> bIn(nB * nIndividuals); 
  bIn.resize( nB * nIndividuals );
  bIn = 0.0;

  // set bLow to:
  //  bLow = [ -2.0 ]
  //
  //valarray<double> bLow(nB);
  bLow.resize( nB );
  bLow = -2.0;

  // set bUp to:
  //  bUp  = [ +2.0 ]
  //
  //valarray<double> bUp(nB); 
  bUp.resize( nB );
  bUp  = +2.0;

  // set _bStep to:
  //  [ 0.01 ]
  //
  //valarray<double> bStep(nB); 
  bStep.resize( nB );
  bStep = 0.01;

  // allocate resource to _bOut
  bOutExpected.resize(nB, nIndividuals);

  // allocate resouce _lTilde_alpOut
  L_alpOutExpected.resize(1, nAlp);

  //
  // The above initialization block set the variables to the
  // following values:
  //
  //                    / 1    0 \
  //     R_i (alp, b) = |        |
  //                    \ 0    2 /
  // 
  //                    / b \
  //     f_i(alp, b)  = |   |
  //                    \ b /
  // 
  //                    / 1 \
  //     y_i          = |   |
  //                    \ 1 /
  // 
  //     D(alp)       = alp
  // 
  //   Given that, it follows that
  //     Lambda_i (alp, b) = (1/2) #log{8 pi^2}   + (3/4)(1 - b)^2
  //                       + (1/2) #log{2 pi alp} + (1/2) b^2 / alp
  //   It follows that the Hessian of Lambda_i (alp, b)
  //   with respect to b is
  // 
  //     3 / 2 + 1 / alp
  // 
  //   In addition, the optimal value of $math%b%$$ solves the equation
  // 
  //     0   = -2 (3/4)(1 - b) +  b / alp
  //     0   = 1 - b - 2 b / (3 alp)
  //     1   = [1 + 2 / (3 alp)] b
  //     b   = 3 alp / (3 alp + 2)
  // 
  // 
  //   Note that in this example,
  //   the model function is linear,
  //   and the data variance does not depend on the random effects.
  //   Thus the three objective functions actually have the same value.
  // 
  //   The expected objectives can be computed as followings:
  // 
  //     H        = 3. / 2. + 1. / alp
  //     H_alp    =         - 1. / alp^2
  //     b        = 3. * alp / (3. * alp + 2.)
  //     b_alp    = 3. / (3. * alp + 2.) ...
  //                - 9. * alp / (3. * alp + 2.)^2
  //     Lambda   = .5 * log(8. * pi^2) ...
  //                + .75 * (1. - b)^2 ...
  //                    + .5 * log(2. * pi * alp) ...
  //                +  .5 * b^2 / alp
  //     Lambda_alp = - 1.5 * (1. - b) * b_alp ...
  //                  + .5 / alp ...
  //                  + b * b_alp / alp ...
  //                  - .5 * b^2 / alp^2
  //     bOut       = [b, b]
  //     LOut       = 2. * Lambda + log(H/(2 * pi))
  //     L_alpOut   = 2. * Lambda_alp + H_alp / H
  //
  valarray<double> H        = 3.0 / 2.0 + 1.0 / alp;
  valarray<double> H_alp    = - 1.0 / (alp * alp);
  valarray<double> b        = 3.0 * alp / (3.0 * alp + 2.0);
  valarray<double> b_alp    = 3.0 / (3.0 * alp + 2.0)
                            - 9.0 *  alp / ( (3.0 * alp + 2.0)*(3.0 * alp + 2.0) );

  // Lambda is a scalar but a vector is used to hold the scalar 
  // as the first element because the right hand side calculation
  // uses b and alp which are valarray objects.
  valarray<double> Lambda   = 0.5 * log(8.0 * PI * PI)
                            + 0.75 * (1.0 - b) * (1.0 - b)
                            + 0.5 * log(2.0 * PI * alp)
                            + 0.5 * b * b / alp;
  valarray<double> Lambda_alp = - 1.5 * (1.0 - b) * b_alp
                              + 0.5 / alp
                              + b * b_alp / alp
                              - 0.5 * b * b / (alp * alp);

  //
  // The remaining block of code is setting the global (static) 
  // place holders with expected values.
  //
  bOutExpected.resize( nB, nIndividuals );
  bOutExpected.fill( b[0] );

  // L (Objective) is a scalar but a vector is used to hold the scalar 
  // as the first element because the right hand side calculation
  // uses H and Lambda which are valarray objects.
  LOutExpected       = (2.0 * Lambda + log(H/(2.0 * PI)))[0];

  L_alpOutExpected.resize( 1, nAlp );
  L_alpOutExpected   = 2.0 * Lambda_alp + H_alp / H;
}
void lTildePvmTest::diagDTest(enum Objective whichObjective,
                      DoubleMatrix &bOut,
                      double &LOut,
                      DoubleMatrix &L_alpOut, 
                      DoubleMatrix &Li_alpOut)
{
  using namespace std;

  bOut.resize( nB, nIndividuals );
  L_alpOut.resize( 1, nAlp );
  Li_alpOut.resize( nIndividuals, nAlp );

  DiagDLTildePvmTest model;
  Optimizer optimizer( eps, mitr, level );

  if( whichObjective == NAIVE_FIRST_ORDER )
  {
    NaiveFoModel foModel( &model, bStep );
    lTildePvm( nIndividuals,
      foModel,
      whichObjective, 
      DoubleMatrix(y,1), 
      DoubleMatrix(N,1), 
      optimizer, 
      DoubleMatrix(alp,1), 
      DoubleMatrix(bLow, 1), 
      DoubleMatrix(bUp,1), 
      DoubleMatrix(bStep,1), 
      DoubleMatrix(bIn,nIndividuals), 
      &bOut, &LOut, &L_alpOut, &Li_alpOut);
  }
  {
    lTildePvm( nIndividuals,
      model,
      whichObjective, 
      DoubleMatrix(y,1), 
      DoubleMatrix(N,1), 
      optimizer, 
      DoubleMatrix(alp,1), 
      DoubleMatrix(bLow, 1), 
      DoubleMatrix(bUp,1), 
      DoubleMatrix(bStep,1), 
      DoubleMatrix(bIn,nIndividuals), 
      &bOut, &LOut, &L_alpOut, &Li_alpOut);
  }
/*
  cout << endl;
  cout << "---------------------------------------------------------------" << endl;
  cout << "      " << whichObjective << endl;
  cout << "---------------------------------------------------------------" << endl;
  cout << "bOut   = " << bOutExpected << endl;
  cout << "actual = " << bOut << endl;

  cout << "Objective" << LOutExpected << endl;
  cout << "actual="   << LOut << endl;

  cout << "Derivative with respect to alp" << endl;
  cout << "L_alpOutExpected=" << L_alpOutExpected << endl;
  cout << "L_alpOutActual  =" << L_alpOut << endl;
*/
  int i,j;
  valarray<double> tolB(nB);
  for( j=0; j< nIndividuals; j++)
  {
      for(i = 0; i<nB; i++)
      {
          // The optimizer called by lTildePvm will accept a value bOut as
          // an estimate for bHat if 
          //
          //     abs( bOut - bHat )  <=  epsilon ( bUp - bLow )  ,
          //
          // where abs is the element-by-element absolute value function
          // and bHat is a local minimizer of the objective function.
          tolB[i] = eps * fabs( bUp[i] - bLow[i] );
      }
  }
  for( j=0; j< nIndividuals; j++ )
  {
      for( i=0; i<nB; i++ )
      {
          CPPUNIT_ASSERT_DOUBLES_EQUAL(bOutExpected.data()[i], bOut.data()[i+j*nB], tolB[i]);
      }
  }
  CPPUNIT_ASSERT_DOUBLES_EQUAL(LOutExpected, LOut, tolB[0]);

  for( j=0; j<nAlp; j++ )
  {
      CPPUNIT_ASSERT_DOUBLES_EQUAL(L_alpOutExpected.data()[j], L_alpOut.data()[j], tolB[j]);
	  for( int k = 0; k < nIndividuals; k++ )
	  {
          CPPUNIT_ASSERT_DOUBLES_EQUAL( L_alpOutExpected.data()[ j ] / 2, 
			                  Li_alpOut.data()[ k * nAlp + j ], tolB[ j ] );
	  }
  }
}
