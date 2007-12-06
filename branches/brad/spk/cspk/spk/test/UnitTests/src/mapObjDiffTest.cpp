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
 * File: mapObjDiff.cpp
 *
 *
 * Test cases for mapObjDiff
 *
 * Author: sachiko honda
 *
 *************************************************************************/
#pragma warning( disable : 4786 )
#include <iostream>
#include <fstream>
#include <string>
#include <cassert>
#include <iomanip>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <cfloat>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/mapObj.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/matmax.h"
#include "../../../spk/matabs.h"
#include "../../../spk/subtract.h"
#include "../../../spk/divByScalar.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/transpose.h"
#include "../../../spk/AkronBtimesC.h"
#include "../../../spk/rvec.h"
#include "../../../spk/add.h"
#include "../../../spk/det.h"
#include "../../../spk/multiply.h"
#include "../../../spk/identity.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/mapObjDiff.h"
#include "mapObjDiffTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

void mapObjDiffTest::setUp()
{
    // initializations
}
void mapObjDiffTest::tearDown()
{
    // clean up
}

Test* mapObjDiffTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite( "mapObjDiffTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<mapObjDiffTest>("testMapObjDiff", &mapObjDiffTest::testMapObjDiff));


    return suiteOfTests;
}

// returns n*n by n
static valarray<double> funF_b_b( const valarray<double> &f_b, const valarray<double> &b )
{
    int n = b.size();
    valarray<double> f_b_bOut(0.0, 2 * 2 * n);
    return f_b_bOut;
}

// returns n*n*n by n
static valarray<double> funR_b_b( const valarray<double> &R_b, const valarray<double> &b )
{
    int n = b.size();

     valarray<double> R_b_bOut(0.0, 2 * 2 * n * n);
     return R_b_bOut;
}

// returns n*n*n by n
static valarray<double> funRinv_b_b( const valarray<double> &R_b, const valarray<double> &b )
{
    
    int n = b.size();
     valarray<double> R_b_bOut(0.0, 2 * 2 * n * n);

     R_b_bOut[0] = 2.0*pow(b[0], -3.0);
     R_b_bOut[6] = 2.0*pow(b[0], -3.0);
     return R_b_bOut;
}

static DoubleMatrix elsq_x_x( 
    const DoubleMatrix &dvecX,          // parameter
    const DoubleMatrix &dvecZ,          // data
    const DoubleMatrix &dvecH,          // h(x)
    const DoubleMatrix &dmatH_x,    
    const DoubleMatrix &dmatH_x_x,
    const DoubleMatrix &dmatQ,          // Q(x)
    const DoubleMatrix &dmatQ_x,
    const DoubleMatrix &dmatQinv,        // Q(x)inv
    const DoubleMatrix &dmatQinv_x,
    const DoubleMatrix &dmatQinv_x_x
    )
{
    using namespace std;

    DoubleMatrix    term1, term2, term1sub2div2, term3, term4, term5, term4add5, term6, term6div2, 
                    term7, term8, term9, term7add8div2, term7add8div2Trans;
    DoubleMatrix ZsubH, tranZsubH, identX;
    DoubleMatrix dmatElsq_x_x;

    ZsubH     = subtract(dvecZ, dvecH);
    tranZsubH = transpose(ZsubH);
    identX    = identity(dvecX.nr());

    term1 = AkronBtimesC(transpose(rvec(dmatQ)), identX, dmatQinv_x_x);
    term2 = multiply(transpose(dmatQinv_x), dmatQ_x);
    term1sub2div2 = divByScalar( add(term1, term2), -2.0 );

    term3 = mulByScalar( AkronBtimesC(multiply(tranZsubH,dmatQinv), identX, dmatH_x_x), -1.0);

    term4 = AkronBtimesC(tranZsubH, transpose(dmatH_x), dmatQinv_x);
    term5 = multiply(multiply(transpose(dmatH_x),dmatQinv),dmatH_x);
    term4add5 = subtract( term5, term4 );

    term6 = AkronBtimesC( transpose(rvec(multiply(ZsubH,tranZsubH))), identX, dmatQinv_x_x );
    term6div2 = divByScalar(term6, 2.0);


    term7 = AkronBtimesC( transpose(dmatH_x), transpose(ZsubH), dmatQinv_x );
    term8 = AkronBtimesC( transpose(ZsubH), transpose(dmatH_x), dmatQinv_x );
    term7add8div2 = divByScalar( add(term7, term8), -2.0 );
    term7add8div2Trans = transpose(term7add8div2);
    
    dmatElsq_x_x = add( term1sub2div2, add( term3, add(term4add5, add(term6div2, term7add8div2Trans))));
    
    return dmatElsq_x_x;
}

static DoubleMatrix gradientMapObj(SpkModel<double> &model, const DoubleMatrix &dvecB, const DoubleMatrix &dvecY, const DoubleMatrix &dmatD ){

    DoubleMatrix mapObj_bOut( 1, 2 );
    DoubleMatrix retVal;

    try{
        mapObj( model, dvecY, dvecB, 0, &mapObj_bOut, true, false, NULL );
    }
    catch(...)
    {
        assert(false);
    }
    retVal = mapObj_bOut;
    
    return retVal;
}
static DoubleMatrix secondDerivMapObj(SpkModel<double> & model, const DoubleMatrix &dvecB, const DoubleMatrix &dvecY, const DoubleMatrix &dmatD)
{
    valarray<double> b = dvecB.toValarray();
    const int nB = b.size();

    model.setIndPar( b );

    valarray<double>    f;      
    model.dataMean(f);
    const int nY = f.size();
    
    valarray<double>    f_b;    
    model.dataMean_indPar(f_b);

    valarray<double>    f_b_b   = funF_b_b(f_b, b);

    valarray<double>    R;      
    model.dataVariance(R);

    valarray<double>    R_b;    
    model.dataVariance_indPar(R_b);

    valarray<double>    R_b_b   = funR_b_b(R_b, b);

    valarray<double>    Rinv;   
    model.dataVarianceInv(Rinv);

    valarray<double>    Rinv_b; 
    model.dataVarianceInv_indPar(Rinv_b);

    valarray<double>    Rinv_b_b = funRinv_b_b(Rinv_b, b);

    valarray<double>    Dinv;   
    model.indParVarianceInv(Dinv);

    return add( 
        elsq_x_x( dvecB, 
                  dvecY, 
                  DoubleMatrix( f, 1 ), 
                  DoubleMatrix( f_b, nB ),
                  DoubleMatrix( f_b_b, nB ),
                  DoubleMatrix( R, nY ), 
                  DoubleMatrix( R_b, nB ),
                  DoubleMatrix( Rinv, nY ), 
                  DoubleMatrix( Rinv_b, nB ),
                  DoubleMatrix( Rinv_b_b , nB )
                ),

        DoubleMatrix( Dinv, nB )
       );
}

class UserModelMapObjDiffTest : public SpkModel<double>
{
    valarray<double> _b;
    int          _i;
    const int _nB;
    const int _nY;
public:
  UserModelMapObjDiffTest(int nB, int nY)
    : _nB(nB), _nY(nY), _b(nB)
  {};
    ~UserModelMapObjDiffTest(){};
private:
    void doSetIndPar(const valarray<double>& bval)
    {
      assert( bval.size() == _nB );
        _b = bval;
    }
    void doDataMean( valarray<double>& ret ) const
    {
      //
      // f(b) = [ b(2) ]
      //        [ b(2) ]
      //
      ret.resize(_nY);
      ret[0] = _b[1];
      ret[1] = _b[1];
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      //
      // f_b(b) = [ 0  1 ]
      //          [ 0  1 ]
      //
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
      // R(b) = [ b(1)  0   ]
      //        [  0   b(1) ]
      //
      ret.resize(_nY * _nY);
      ret[0] = _b[0];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = _b[0];
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      //
      // R_b(b) = [ 1  0 ]
      //          [ 0  0 ]
      //          [ 0  0 ]
      //          [ 1  0 ]
      //
      ret.resize(_nY*_nY * _nB);
      for( int i=0; i<_nY*_nY*_nB; i++ )
        ret[i] = 0.0;

      ret[0] = 1.0;
      ret[3] = 1.0;
      return true;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
      //
      // R(b)^-1 = [ 1 / b(1)      0   ]
      //           [  0       1 / b(1) ]
      //
      ret.resize(_nY * _nY);
      ret[0] = 1.0 / _b[0];
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 1.0 / _b[0];
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
      //
      // R^(-1)_b(b) = [ -1 / b(1)^2  0 ]
      //               [ 0            0 ]
      //               [ 0            0 ]
      //               [ -1 / b(1)^2  0 ]
      //
      ret.resize(_nY*_nY * _nB);
      for( int i=0; i<_nY*_nY*_nB; i++ )
        ret[i] = 0.0;

      ret[0] = -1.0 / (_b[0]*_b[0]);
      ret[3] = -1.0 / (_b[0]*_b[0]);
      return true;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
       //
       // D(alp) = [ 1  0 ]
       //          [ 0  1 ]
       //
       ret.resize(_nB * _nB);
       ret[0] = 1.0;
       ret[1] = 0.0;
       ret[2] = 0.0;
       ret[3] = 1.0;
    }
};


void mapObjDiffTest::testMapObjDiff()
{
    using namespace std;
    cout << setiosflags(ios::scientific) << setprecision(15);

    int m = 2;

    DoubleMatrix  y(m,1),
                  bStep(m,1),
                  b(m,1),
                  D(m,m);
    double       *pdY = y.data(),
                 *pdD = D.data(),
                 *pdBStep = bStep.data(),
                 *pdB = b.data();
    double       dtemp;
    DoubleMatrix mapObj_bOut(1,m),
                 mapObj_b_bOut(m,m),
                 exactMapObj_bOut,
                 exactMapObj_b_bOut;
    int i;

    UserModelMapObjDiffTest model( m, m );

    // Set y to a vector:
    for( i=0; i<m; i++ )
        pdY[i] = rand()/(pow(2.0,32.0)) * 100.0 + 2.0;

    // Set b
    // WARNING: each value needs to be fairly large; otherwise trancation error grows
    for( i=0; i<m; i++ ){
        pdB[i] = rand()/(pow(2.0,32.0)) * 100.0 + 2.0;
    }

    // Set bStep to a vector:
    for( i=0; i<m; i++ ){
# if 0
        dtemp = pow( DBL_EPSILON / (2.0/6.0), 1/3.0) + pdB[i];
        pdBStep[i] = 10.0 * ( dtemp - pdB[i] );
# else
	// changed by Brad on 2007-11-30
	pdBStep[i] = 1e-5;
# endif
    }

    // Set D to an identity matrix:
    D = identity(2);

    try{
        mapObjDiff(model, y, bStep, b, &mapObj_bOut, &mapObj_b_bOut, true, false);
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }

    exactMapObj_bOut   = gradientMapObj(model, b, y, D);
    pdBStep = bStep.data(); 

    for(i=0; i<m; i++ ){
        CPPUNIT_ASSERT_DOUBLES_EQUAL(exactMapObj_bOut.data()[i], mapObj_bOut.data()[i],
                ( pow(pdBStep[i], 2.0 ) / ( 1.0*((4.0*3.0*2.0/pow(pdB[0],5.0) * (3.0*2.0/pow(pdB[0],4.0))))) )
                + (DBL_EPSILON*10.0/pdBStep[i]) );

    }
    exactMapObj_b_bOut = secondDerivMapObj(model, b, y, D);
    pdBStep = bStep.data();

    for( i=0; i<m; i++ )
    {
        CPPUNIT_ASSERT_DOUBLES_EQUAL(exactMapObj_b_bOut.data()[i], mapObj_b_bOut.data()[i], 
                ( pow(pdBStep[i], 2.0 ) / ( 2.0*((4.0*3.0*2.0/pow(pdB[0],5.0) * (3.0*2.0/pow(pdB[0],4.0))))) )
                + (DBL_EPSILON*100.0/pdBStep[i]) );
    }
}
