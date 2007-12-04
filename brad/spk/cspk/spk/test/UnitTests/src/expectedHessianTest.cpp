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
 * File: expectedHessianTest.cpp
 *
 *
 * Test cases for expectedHessian
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/expectedHessian.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/multiply.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/transpose.h"
#include "../../../spk/mulByScalar.h"
#include "../../../spk/AkronBtimesC.h"
#include "../../../spk/add.h"
#include "../../../spk/centdiff.h"
#include "../../../spk/Function.h"
#include "../../../spk/lambda2diff.h"

#include "expectedHessianTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

class expectedHessianTest::exampleModel : public SpkModel<double>
{
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nY;
public:
    exampleModel() : _a(2), _b(2), _i(0), _nA(2), _nB(2), _nY(2) {};
    ~exampleModel(){};
private:
    void doSelectIndividual(int i)
    {
        _i = i;
    }
    void doSetPopPar(const valarray<double>& a)
    {
        assert(a.size() == _nA);
        _a = a;
    }
    void doSetIndPar(const valarray<double>& b)
    {
        assert(b.size() == _nB);
        _b = b;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        //fOut = {alp(2) + b(2), alp(2) + b(2)}

        ret.resize( _nY );
        ret[0] = _a[1]+_b[1];
        ret[1] = _a[1]+_b[1];
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
		//f_alpOut = 
		///[ 0 , 1 ]
		// [ 0 , 1 ]
        ret.resize(_nY * _nA);
        ret[0] = 0;
        ret[1] = 0;
        ret[2] = 1;
        ret[3] = 1;
        return true;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
		//f_bOut =
		//[ 0 , 1 ]
		//[ 0 , 1 ]
        ret.resize(_nY * _nB);
        ret[0] = 0;
        ret[1] = 0;
        ret[2] = 1;
        ret[3] = 1;
        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
		//ROut = 
		//[ b(1) ,   0   ]
		//[ 0    ,  b(1) ]
        ret.resize(_nY * _nY);
       
        ret[0] = _b[0];
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = _b[0];
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
		//R_alpOut =
		//[ 0 , 0 ]
		//[ 0 , 0 ]
		//[ 0 , 0 ]
		//[ 0 , 0 ]
        ret.resize(_nY*_nY * _nA);
        for( int i=0; i<_nY*_nY*_nA; i++ )
          ret[i] = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
		//R_bOut = 
		//[ 1 , 0 ]
		//[ 0 , 0 ]
		//[ 0 , 0 ]
		//[ 1 , 0 ]
        ret.resize(_nY*_nY * _nB);
        for( int i=0; i<_nY*_nY*_nB; i++ )
          ret[i] = 0.0;
        ret[0] = 1.0;
        ret[3] = 1.0;
        return true;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
        assert(_b[0] != 0);
        //RinvOut = 
        // [ 1/ b(1)    0   ]
        // [   0     1/b(1) ]
        ret.resize(_nY * _nY);

        ret[0] = 1.0/_b[0];
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 1.0/_b[0];
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        //Rinv_aOut =
        //
        // [ 0    0 ]
        // [ 0    0 ]
        // [ 0    0 ]
        // [ 0    0 ]
        ret.resize(_nY*_nY * _nA);
        for( int i=0; i<_nY*_nY*_nA; i++ )
          ret[i] = 0.0;
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        // Rinv_bOut =
        //
        // [ -1/b(1)^2    0 ]
        // [    0         0 ]
        // [    0         0 ]
        // [ -1/b(1)^2    0 ]
        ret.resize(_nY*_nY * _nB);
        for( int i=0; i<_nY*_nY*_nB; i++ )
          ret[i] = 0.0;

        ret[0] = -1.0/(_b[0]*_b[0]);
        ret[3] = -1.0/(_b[0]*_b[0]);

        return true;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        assert(_a[0] != 0);
		//DOut = 
		//[ alp(1)^(-1) ,   0   ]
		//[ 0    ,  alp(1)^(-1) ]
        ret.resize(_nB * _nB);
        ret[0] = 1.0/_a[0];
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 1.0/_a[0];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        assert(_a[0] != 0 );
		//D_alpOut = 
		//[ -alp(1)^(-2) , 0 ]
		//[ 0 , 0 ]
		//[ 0 , 0 ]
		//[ -alp(1)^(-2) , 0 ]
        ret.resize(_nB*_nB, _nA);
        for( int i=0; i<_nB*_nB*_nA; i++ )
          ret[i] = 0.0;
        ret[0] = -1.0/(_a[0]*_a[0]);
        ret[3] = -1.0/(_a[0]*_a[0]);
        return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
		//DinvOut = 
		//[ alp(1) ,            0   ]
		//[ 0    ,           alp(1) ]
        ret.resize(_nB * _nB);
        ret[0] = _a[0];
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = _a[0];
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        // Dinv_aOut =
        // [ 1     0 ]
        // [ 0     0 ]
        // [ 0     0 ]
        // [ 1     0 ]
        ret.resize(_nB*_nB * _nA);
        for( int i=0; i<_nB*_nB*_nA; i++ )
          ret[i] = 0.0;
        ret[0] = 1;
        ret[3] = 1;
        return true;
    }
};

// This class is a bit more complex than exampleModel in a way
// that D() returns a full symmetric matrix instead of diagonal.
class expectedHessianTest::fullDModel : public SpkModel<double>
{
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nY;
public:
    fullDModel() : _a(2), _b(2), _i(0), _nA(2), _nB(2), _nY(2) {};    
    ~fullDModel(){};
private:
    void doSelectIndividual(int i)
    {
        _i = i;
    }
    void doSetPopPar(const valarray<double>& a)
    {
        assert(a.size() == _nA);
        _a = a;
    }
    void doSetIndPar(const valarray<double>& b)
    {
        assert(b.size() == _nB);
        _b = b;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        //fOut = {alp(2) + b(2), alp(2) + b(2)}

        ret.resize(_nY);
        ret[0] = _a[1]+_b[1];
        ret[1] = _a[1]+_b[1];
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
		//f_alpOut = 
		///[ 0 , 1 ]
		// [ 0 , 1 ]
        ret.resize(_nY * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        ret[2] = 1.0;
        ret[3] = 1.0;
        return true;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
		//f_bOut =
		//[ 0 , 1 ]
		//[ 0 , 1 ]
        ret.resize(_nY * _nB);
        ret[0] = 0.0;
        ret[1] = 0.0;
        ret[2] = 1.0;
        ret[3] = 1.0;
        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
		//ROut = 
		//[ b(1) ,   0   ]
		//[ 0    ,  b(1) ]
        ret.resize(_nY * _nY);
        ret[0] = _b[0];
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = _b[0];
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
		//R_alpOut =
		//[ 0 , 0 ]
		//[ 0 , 0 ]
		//[ 0 , 0 ]
		//[ 0 , 0 ]
        ret.resize(_nY * _nY * _nA);
        for( int i=0; i<_nY*_nY*_nA; i++ )
          ret[i] = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
		//R_bOut = 
		//[ 1 , 0 ]
		//[ 0 , 0 ]
		//[ 0 , 0 ]
		//[ 1 , 0 ]
        ret.resize(_nY * _nY * _nB);
        for( int i=0; i<_nY*_nY*_nB; i++ )
          ret[i] = 0.0;
        ret[0] = 1.0;
        ret[3] = 1.0;
        return true;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
        assert( _b[0] != 0 );
        //RinvOut = 
        // [ 1/ b(1)    0   ]
        // [   0     1/b(1) ]
        ret.resize(_nY * _nY);
        ret[0] = 1.0/_b[0];
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 1.0/_b[0];
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        //Rinv_aOut =
        //
        // [ 0    0 ]
        // [ 0    0 ]
        // [ 0    0 ]
        // [ 0    0 ]
        ret.resize(_nY * _nY * _nA);
        for( int i=0; i<_nY*_nY*_nA; i++ )
          ret[i] = 0.0;
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        assert( _b[0] != 0 );
        // Rinv_bOut =
        //
        // [ -1/b(1)^2    0 ]
        // [    0         0 ]
        // [    0         0 ]
        // [ -1/b(1)^2    0 ]
        ret.resize( _nY * _nY * _nB );
        for( int i=0; i<_nY*_nY*_nB; i++ )
          ret[i] = 0.0;
        ret[0] = -1.0/(_b[0]*_b[0]);
        ret[3] = -1.0/(_b[0]*_b[0]);

        return true;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        assert( _a[0] != 0 );
        assert( _a[1] != 0 );
		//DOut = 
		//[ alp(1)^(-1),   alp(2)^(-1) ]
		//[ alp(2)^(-1),   alp(1)^(-1) ]

        ret.resize(_nB * _nB);
        ret[0] = 1.0/_a[0];
        ret[1] = 1.0/_a[1];
        ret[2] = 1.0/_a[1];
        ret[3] = 1.0/_a[0];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        assert( _a[0] != 0 );
        assert( _a[1] != 0 );
		//D_alpOut = 
		//[ -alp(1)^(-2) , 0            ]
		//[ 0            , -alp(2)^(-2) ]
		//[ 0 ,            -alp(2)^(-2) ]
		//[ -alp(1)^(-2) , 0            ]
        ret.resize(_nB * _nB * _nA);
        for( int i=0; i<_nB*_nB*_nA; i++ )
          ret[i] = 0.0;

        ret[0] = -1.0/(_a[0]*_a[0]);
        ret[3] = -1.0/(_a[0]*_a[0]);
        ret[5] = -1.0/(_a[1]*_a[1]);
        ret[6] = -1.0/(_a[1]*_a[1]);
        return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {

		//DinvOut = 
        // 1.0 / (a*d - b*c) 
		// * [ d , -b ]
		//   [ -c,  a ]
        //
        // = 1.0 / (a*d - b*c) *
        //   [  1.0/_pa[0]  -1.0/_pa[1] ]
        //   [ -1.0/_pa[1]   1.0/_pa[0] ]
        //
        double a = 1.0/_a[0];
        double c = 1.0/_a[1];
        double b = 1.0/_a[1];
        double d = 1.0/_a[0];
        double determinantOfTwoByTwo = 1.0 / (a*d - b*c);

        ret.resize(_nB * _nB);
        ret[0] =  d * determinantOfTwoByTwo;
        ret[1] = -c * determinantOfTwoByTwo;
        ret[2] = -b * determinantOfTwoByTwo;
        ret[3] =  a * determinantOfTwoByTwo;
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {

        // Dinv_aOut =
        // [ 1.0 / (a*d - b*c)    0                 ]
        // [ 0                    1.0 / (a*d - b*c) ]
        // [ 0                    1.0 / (a*d - b*c) ]
        // [ 1.0 / (a*d - b*c)    0                 ]
        double a = 1.0/_a[0];
        double b = 1.0/_a[1];
        double c = 1.0/_a[1];
        double d = 1.0/_a[0];
        double determinantOfTwoByTwo = 1.0 / (a*d - b*c);
        ret.resize(_nB *_nB * _nA);
        
        for( int i=0; i<_nB*_nB*_nA; i++ )
          ret[i] = 0.0;

        ret[0] = determinantOfTwoByTwo;
        ret[3] = determinantOfTwoByTwo;
        ret[5] = determinantOfTwoByTwo;
        ret[6] = determinantOfTwoByTwo;
        
        return true;
    }
};

static const int NUM_INDS = 3;
static const int NUM_ALP  = 2;
static const int NUM_B    = 4;
static const int IND_DATA_LEN[] = {1,2,3};

class expectedHessianTest::ExpectedHessianModel : public SpkModel<double>
{
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    int _nYi;
public:
    ExpectedHessianModel() : _a(NUM_ALP), _b(NUM_B), _i(0), _nA(NUM_ALP), _nB(NUM_B), _nYi(0) {};    
    ~ExpectedHessianModel(){};
private:
    void doSelectIndividual(int i)
    {
        _i = i;
        _nYi = IND_DATA_LEN[_i];
    }
    void doSetPopPar(const valarray<double>& a)
    {
      assert( a.size() == _nA );
        _a = a;
    }
    void doSetIndPar(const valarray<double>& b)
    {
      assert( b.size() == _nB );
        _b = b;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        assert(_nYi > 0);
        // mean of data
        ret.resize(_nYi);
        switch(_i+1)
        {
        case 1: 
            // For 1st individual, set matrix to:
            // 
            //    [ _b[0] ]
            //
            assert(_nYi==1);
            ret[0] = _b[0];
            break;
        case 2:
            // For 2nd individual, set matrix to:
            // 
            //    [ _b[0]   ]
            //    [ _b[0]*2 ]
            //
            assert(_nYi==2);
            ret[0] = _b[0];
            ret[1] = _b[0]*2;
            break;
        case 3:
            // For 3rd individual, set matrix to:
            // 
            //    [ _b[0]   ]
            //    [ _b[0]*2 ]
            //    [ _b[0]*3 ]
            //
            assert(_nYi==3);
            ret[0] = _b[0];
            ret[1] = _b[0]*2;
            ret[2] = _b[0]*3;
            break;
        default:
            assert(false);
            break;
        }
        
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        ret.resize(_nYi * _nA);
        for( int i=0; i< _nYi * _nA; i++ )
          ret[i] = 0.0;

        return false;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        assert(_nYi > 0);
        assert(_nB > 0);
        int i;
        ret.resize(_nYi * _nB);
        for( i=0; i< _nYi * _nB; i++ )
          ret[i] = 0.0;

        // For 1st individual, set matrix to:
        // 
        //    [ 1 0 0 0 ]
        //
        // For 2nd individual, set matrix to:
        // 
        //    [ 1 0 0 0 ]
        //    [ 2 0 0 0 ]
        //
        // For 3rd individual, set matrix to:
        // 
        //    [ 1 0 0 0 ]
        //    [ 2 0 0 0 ]
        //    [ 3 0 0 0 ]
        //
        for( i=0; i<_nYi; i++ )
            ret[i+0*_nYi] = i+1;

        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        // covariance of data
        assert(_nYi > 0);
        
        ret.resize(_nYi * _nYi);
        for( int i=0; i<_nYi*_nYi; i++ )
          ret[i] = 0.0;
        switch(_i+1)
        {
            // Must be symmetric!
            //
        case 1:
            // For 1st individual
            //   [ b[0] ] 
            //
            ret[0] = _b[0];
            break;
        case 2:
            // For 2nd individual
            //   [ b[0]   0  ]
            //   [   0  a[1] ]
            //
            ret[0] = _b[0];
            ret[3] = _a[1];
            break;
        case 3:
            // For 3rd individual
            //   [ b[0]   0    0  ]
            //   [   0  a[1]   0  ]
            //   [   0    0  b[2] ]
            //
            ret[0] = _b[0];
            ret[4] = _a[1];
            ret[8] = _b[2];
            break;
        default:
            assert(false);
            break;

        }
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        assert(_nYi > 0);
        assert(_nA > 0);
        ret.resize(_nYi*_nYi * _nA);
        for( int i=0; i<_nYi*_nYi*_nA; i++ )
          ret[i] = 0.0;

        switch(_i+1)
        {
        case 1:
            // Set it to a matrix:
            //
            //  [ 0 0 ]
            //
            ret[0] = 0;
            return false;
            break;
        case 2:
            // Set it to a matrix:
            //
            //  [ 0 0 ]
            //  [ 0 0 ]
            //  [ 0 0 ]
            //  [ 0 1 ]
            //
            ret[7] = 1;
            return true;
            break;
        case 3:
            // Set it to a matrix:
            //
            //  [ 0 0 ]
            //  [ 0 0 ]
            //  [ 0 0 ]
            //  [ 0 0 ]
            //  [ 0 1 ]
            //  [ 0 0 ]
            //  [ 0 0 ]
            //  [ 0 0 ]
            //  [ 0 0 ]
            //
            ret[13] = 1;
            return true;
            break;
        default:
            assert(false);
            break;
        }
        return true;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        assert(_nYi > 0);
        assert(_nB > 0);
        ret.resize(_nYi*_nYi * _nB);
        for( int i=0; i<_nYi*_nYi*_nB; i++ )
          ret[i] = 0.0;

        switch(_i+1)
        {
        case 1:
            // Set it to a matrix:
            //
            //  [ 1 0 0 0 ]
            //
            ret[0] = 1;
            break;
        case 2:
            // Set it to a matrix:
            //
            //  [ 1 0 0 0 ]
            //  [ 0 0 0 0 ]
            //  [ 0 0 0 0 ]
            //  [ 0 0 0 0 ]
            //
            ret[0] = 1;
            break;
        case 3:
            // Set it to a matrix:
            //
            //  [ 1 0 0 0 ]
            //  [ 0 0 0 0 ]
            //  [ 0 0 0 0 ]
            //  [ 0 0 0 0 ]
            //  [ 0 0 0 0 ]
            //  [ 0 0 0 0 ]
            //  [ 0 0 0 0 ]
            //  [ 0 0 0 0 ]
            //  [ 0 0 1 0 ]
            //
            ret[0] = 1;
            ret[26]= 1;
            break;
        default:
            assert(false);
            break;
        }

        return true;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
        assert(_nYi > 0);

        assert(_b[0] != 0);
        assert(_b[2] != 0);
        assert(_a[1] != 0);

        ret.resize(_nYi * _nYi);
        for( int i=0; i<_nYi*_nYi; i++ )
          ret[i] = 0.0;

        switch(_i+1)
        {
        case 1:
            // For 1st individual
            //   [ 1/b[0] ] 
            //
            ret[0] = 1.0 / _b[0];
            break;
        case 2:
            // For 2nd individual
            //   [ 1/b[0]     0  ]
            //   [     0  1/a[1] ]
            //
            ret[0] = 1.0 / _b[0];
            ret[3] = 1.0 / _a[1];
            break;
        case 3:
            // For 3rd individual
            //   [ 1/b[0]     0      0  ]
            //   [     0  1/a[1]     0  ]
            //   [     0      0  1/b[2] ]
            //
            ret[0] = 1.0 / _b[0];
            ret[4] = 1.0 / _a[1];
            ret[8] = 1.0 / _b[2];
            break;
        default:
            break;
        }

    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        assert(_a[1] != 0);

        ret.resize(_nYi*_nYi * _nA);
        for( int i=0; i<_nYi*_nYi*_nA; i++ )
          ret[i] = 0.0;

        switch(_i+1)
        {
        case 1:
            return false;
            break;
        case 2:
            ret[7] = -1.0/(_a[1]*_a[1]);
            return true;
            break;
        case 3:
            ret[13] = -1.0/(_a[1]*_a[1]);
            return true;
            break;
        default:
            assert(false);
            break;
        }
        return true;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        assert(_nYi > 0);
        assert(_nB > 0);

        assert(_b[0] != 0);
        assert(_b[2] != 0);

        ret.resize(_nYi*_nYi * _nB);
        for( int i=0; i<_nYi*_nYi*_nB; i++ )
          ret[i] = 0.0;

        switch(_i+1)
        {
        case 1:
            // For 1st individual
            //   [ -1/b[0]^2      0         0        0 ] 
            //
            ret[0] = -1.0 / (_b[0]*_b[0]);
            break;
        case 2:
            // For 2nd individual
            //   [ -1/b[0]^2      0         0        0 ]
            //   [      0         0         0        0 ]
            //   [      0         0         0        0 ]
            //   [      0         0         0        0 ]
            //
            ret[0] = -1.0 / (_b[0]*_b[0]);
            break;
        case 3:
            // For 3rd individual
            //   [ -1/b[0]^2      0         0        0 ]
            //   [      0         0         0        0 ]
            //   [      0         0         0        0 ]
            //   [      0         0         0        0 ]
            //   [      0         0         0        0 ]
            //   [      0         0         0        0 ]
            //   [      0         0         0        0 ]
            //   [      0         0         0        0 ]
            //   [      0         0    -1/b[2]^2     0 ]
            ret[0]  = -1.0 / (_b[0]*_b[0]);
            ret[26] = -1.0 / (_b[2]*_b[2]);
            break;
        default:
            assert(false);
            break;
        }
        return true;
    }   
    void doIndParVariance( valarray<double>& ret ) const
    {
        // variance between an inndividual's parameters
        assert(_nB > 0);
        ret.resize(_nB * _nB);

        // Set it to a matrix:
        //
        //   [ _a[0]      0      0      0  ]
        //   [    0    _a[0]     0      0  ]
        //   [    0       0   _a[0]     0  ]
        //   [    0       0      0   _a[0] ]
        //
        for( int j=0; j<_nB; j++ )
        {
          for( int i=0; i<_nB; i++ )
          {
            if( i==j )
              ret[i+j*_nB] = _a[0];
            else
              ret[i+j*_nB] = 0.0;

          }
        }
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        assert(_nB > 0);
        assert(_nA > 0);
        ret.resize(_nB*_nB * _nA);
        for( int i=0; i<_nB*_nB * _nA; i++ )
          ret[i] = 0.0;

        ret[0]  = 1.0;
        ret[5]  = 1.0;
        ret[10] = 1.0;
        ret[15] = 1.0;
        
        return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
        // Set it to a matrix:
        //
        //   [ 1/(_a[0])          0            0            0   ]
        //   [         0    1/(_a[0])          0            0   ]
        //   [         0            0    1/(_a[0])          0   ]
        //   [         0            0            0    1/(_a[0]) ]
        //
        assert(_nB > 0);
        ret.resize(_nB * _nB);
        for( int j=0; j<_nB; j++ )
        {
          for( int i=0; i<_nB; i++ )
          {
            if( i==j )
              ret[i+j*_nB] = 1.0 / _a[0];
            else
              ret[i+j*_nB] = 0.0;
          }
        }

    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        // Set it to a matrix:
        //
        //   [ -1/(_a[0]^2)    0 ]
        //   [ 0               0 ]
        //   [ 0               0 ]
        //   [ 0               0 ]
        //   [ 0               0 ]
        //   [ -1/(_a[0]^2)    0 ]
        //   [ 0               0 ]
        //   [ 0               0 ]
        //   [ 0               0 ]
        //   [ 0               0 ]
        //   [ -1/(_a[0]^2)    0 ]
        //   [ 0               0 ]
        //   [ 0               0 ]
        //   [ 0               0 ]
        //   [ 0               0 ]
        //   [ -1/(_a[0]^2)    0 ]
        //

        assert(_nB > 0);
        assert(_nA > 0);
        ret.resize(_nB*_nB * _nA);
        for( int i=0; i<_nB*_nB*_nA; i++ )
          ret[i] = 0.0;

        ret[0]  = -1.0/(_a[0]*_a[0]);
        ret[5]  = -1.0/(_a[0]*_a[0]);
        ret[10] = -1.0/(_a[0]*_a[0]);
        ret[15] = -1.0/(_a[0]*_a[0]);
        return true;
    }
};
void expectedHessianTest::setUp()
{
    // initializations
}
void expectedHessianTest::tearDown()
{
    // clean up
}

Test* expectedHessianTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite("expectedHessianTest");

    suiteOfTests->addTest(new TestCaller<expectedHessianTest>(
                  "testExample", &expectedHessianTest::testExample));
    suiteOfTests->addTest(new TestCaller<expectedHessianTest>(
                  "testModel",   &expectedHessianTest::testModel));
    suiteOfTests->addTest(new TestCaller<expectedHessianTest>(
                  "testFunction",&expectedHessianTest::testFunction));
    suiteOfTests->addTest(new TestCaller<expectedHessianTest>(
                  "testFullD", &expectedHessianTest::testFullD));

    return suiteOfTests;
}


void expectedHessianTest::testFunction()
{
    using namespace std;

    int i,j,cnt;

    // set Model
    ExpectedHessianModel model;

    DoubleMatrix alpDM    (NUM_ALP, 1);
    DoubleMatrix alpStepDM(NUM_ALP, 1);
    DoubleMatrix bDM      (NUM_B,   1);
    DoubleMatrix bStepDM  (NUM_B, 1);

    valarray<double> alp(NUM_ALP);
    valarray<double> alpStep(NUM_ALP);

    valarray<double> b(NUM_B);
    valarray<double> bStep(NUM_B);

    for(i=0; i<NUM_INDS; i++)
    {
        for( j=0; j<NUM_B; j++ )
        {
            b[j] = j+1.;
            bStep[j] = 0.01;
        }
        
         for(j=0; j<NUM_ALP; j++)
        {
            alp[j] = j+2;
            alpStep[j] = 0.01;
        }

        model.selectIndividual(i);

        model.setPopPar(alp);
        model.setIndPar(b);

        valarray<double> Dinv( NUM_B * NUM_B );
        model.indParVarianceInv(Dinv);
        DoubleMatrix Dinv_DM( Dinv, NUM_B );

        DoubleMatrix expectedHessianTilde(NUM_B, NUM_B);
        DoubleMatrix expectedHessianTilde_b(NUM_B*NUM_B, NUM_B);
        DoubleMatrix expectedHessianTilde_a(NUM_B*NUM_B, NUM_ALP);

        expectedHessian(model, DoubleMatrix( alp, 1 ), DoubleMatrix( b, 1 ), DoubleMatrix( bStep, 1 ), 
          &expectedHessianTilde, &expectedHessianTilde_a, &expectedHessianTilde_b );
        {
            CPPUNIT_ASSERT_EQUAL(NUM_B, expectedHessianTilde.nr());
            CPPUNIT_ASSERT_EQUAL(NUM_B, expectedHessianTilde.nc());

            CPPUNIT_ASSERT_EQUAL(NUM_B*NUM_B, expectedHessianTilde_b.nr());
            CPPUNIT_ASSERT_EQUAL(NUM_B, expectedHessianTilde_b.nc());

            // Prepare for computing expected values based on true derivatives
            valarray<double> f;
            model.dataMean(f);
            const int NUM_Yi = f.size();

            valarray<double> f_b;
            model.dataMean_indPar(f_b);
            
            valarray<double> f_a;
            model.dataMean_popPar(f_a);
                      
            valarray<double> R_b;
            model.dataVariance_indPar(R_b);
            
            valarray<double> Rinv;
            model.dataVarianceInv(Rinv);

            // Compute an expected expectedHessianTilde answer
            {
                DoubleMatrix term1 = multiply(multiply(transpose( DoubleMatrix( f_b, NUM_B ) ), DoubleMatrix( Rinv, NUM_Yi ) ), DoubleMatrix( f_b, NUM_B ));
                DoubleMatrix term2 = mulByScalar(multiply(transpose( DoubleMatrix( R_b, NUM_B ) ),
                          AkronBtimesC(DoubleMatrix( Rinv, NUM_Yi ), DoubleMatrix( Rinv, NUM_Yi ), DoubleMatrix( R_b, NUM_B ))), 0.5);
                DoubleMatrix Hi    = add(add( DoubleMatrix( Dinv, NUM_B ), term1), term2);
                

                valarray<double> tol(NUM_B);
                for( j = 0; j<NUM_B; j++ )
                {
                    //
                    // The true derivative of f(x) can be expressed in a talor's series:
                    // f'(x) = 1 + x/2 + x^3/3 + ... for -1 < x < 1
                    //
                    // The central difference approximation of f(x) with h as step size is expanded:
                    // {f(x+h) - f(x-h)}/(2.0*h) = 1 + 2x + (6x^2y + 2y^3) + ... for -1 < x < 1
                    //
                    // So, the difference between the two is:
                    // f'(x) - {f(x+h) - f(x-h)}/(2.0*h) = (x/2 -2x) + (x^3/3 - 6x^2y + 2y^3)...
                    // 
                    //
                    assert(bStep[j] <= 1.0  );
                    assert(bStep[j] >= -1.0 );
                    double x = b[j];
                    double h = bStep[j];
                    tol[j] = fabs(2.0*(x/2.0 - 2.0*x) + (x*x*x/3.0 - 6.0*x*x*h + 2*h*h*h));// + DBL_EPSILON*DBL_EPS_EQUAL_MULT;
                }

                for(int j=0; j<Hi.nc(); j++ )
                {
                    for( cnt = 0; cnt<Hi.nr(); cnt++ )
                    {
                        int inx = cnt+j*Hi.nr();
                        CPPUNIT_ASSERT_DOUBLES_EQUAL(Hi.data()[inx], expectedHessianTilde.data()[inx], tol[j]);
                    }
                }
            }

            // compute an expected expectedHessianTilde_b answer
            {
                ExpectedHessianValarray ExpectedHessianOb(&model, bStep);
                valarray<double> expectedHessianTilde_bApprox 
                    = centdiff< binder1st<ExpectedHessianValarray> >(bind1st(ExpectedHessianOb, alp ), 1, b, bStep);

                valarray<double> tol(NUM_B);

                for( j = 0; j<NUM_B; j++ )
                {
                    //
                    // The true derivative of f(x) can be expressed in a talor's series:
                    // f'(x) = 1 + x/2 + x^3/3 + ... for -1 < x < 1
                    //
                    // The central difference approximation of f(x) with h as step size is expanded:
                    // {f(x+h) - f(x-h)}/(2.0*h) = 1 + 2x + (6x^2y + 2y^3) + ... for -1 < x < 1
                    //
                    // So, the difference between the two is:
                    // f'(x) - {f(x+h) - f(x-h)}/(2.0*h) = (x/2 -2x) + (x^3/3 - 6x^2y + 2y^3)...
                    // 
                    //
                    assert(bStep[j] <= 1.0  );
                    assert(bStep[j] >= -1.0 );
                    double x = b[j];
                    double h = bStep[j];
                    tol[j] = fabs(2.0*(x/2.0 - 2.0*x) + (x*x*x/3.0 - 6.0*x*x*h + 2*h*h*h));// + DBL_EPSILON*DBL_EPS_EQUAL_MULT;
                }

                for(int j=0; j<NUM_B; j++ )
                {
                    for( cnt = 0; cnt<NUM_B*NUM_B; cnt++ )
                    {
                        int inx = cnt+j*NUM_B*NUM_B;
                        CPPUNIT_ASSERT_DOUBLES_EQUAL(expectedHessianTilde_bApprox[inx], expectedHessianTilde_b.data()[inx], tol[j]);
                    }
                }
            }

            // compute an expected expectedHessianTilde_a answer
            {
                ExpectedHessian ExpectedHessianOb(&model, DoubleMatrix( bStep, 1 ) );
                DoubleMatrix expectedHessianTilde_alpApprox 
                    = centdiff< binder2nd<ExpectedHessian> >(bind2nd(ExpectedHessianOb, DoubleMatrix( b, 1 ) ), DoubleMatrix( alp, 1 ), DoubleMatrix( alpStep, 1 ) );
                
                CPPUNIT_ASSERT_EQUAL(NUM_B*NUM_B, expectedHessianTilde_alpApprox.nr());
                CPPUNIT_ASSERT_EQUAL(NUM_ALP, expectedHessianTilde_alpApprox.nc());

/*
cout << "Expected Hessian: actual_alp = " << endl;
expectedHessianTilde_a.print();
cout << "Expected Hessian: expected_alp=" << endl;
expectedHessianTilde_alpApprox.print();
*/
                valarray<double> tol(NUM_ALP);
                for( j = 0; j<NUM_ALP; j++ )
                {
                    //
                    // The true derivative of f(x) can be expressed in a talor's series:
                    // f'(x) = 1 + x/2 + x^3/3 + ... for -1 < x < 1
                    //
                    // The central difference approximation of f(x) with h as step size is expanded:
                    // {f(x+h) - f(x-h)}/(2.0*h) = 1 + 2x + (6x^2y + 2y^3) + ... for -1 < x < 1
                    //
                    // So, the difference between the two is:
                    // f'(x) - {f(x+h) - f(x-h)}/(2.0*h) = (x/2 -2x) + (x^3/3 - 6x^2y + 2y^3)...
                    // 
                    //
                    assert(alpStep[j] <= 1.0  );
                    assert(alpStep[j] >= -1.0 );
                    double x = alp[j];
                    double h = alpStep[j];
                    tol[j] = fabs(2.0*(x/2.0 - 2.0*x) - (x*x*x/3.0 - 6.0*x*x*h + 2.0*h*h*h));// + DBL_EPSILON*DBL_EPS_EQUAL_MULT;
                }
                for(int j=0; j<expectedHessianTilde_alpApprox.nc(); j++ )
                {
                    for( cnt = 0; cnt<expectedHessianTilde_alpApprox.nr(); cnt++ )
                    {
                        int inx = cnt+j*expectedHessianTilde_alpApprox.nr();
                        CPPUNIT_ASSERT_DOUBLES_EQUAL(expectedHessianTilde_alpApprox.data()[inx], expectedHessianTilde_a.data()[inx], tol[j]);
                    }
                }
                
            }
        }

    }
}



void expectedHessianTest::testExample()
{
    using namespace std;
    DoubleMatrix alp(2,1);
    alp.fill(1.0);
    
    DoubleMatrix b(2,1);
    b.fill(1.0);

    DoubleMatrix bStep(2,1);
    bStep.fill(0.1);

    exampleModel model;

    DoubleMatrix expectedHessianTilde(2, 2);
    DoubleMatrix expectedHessianTilde_b(2, 2*2);
    DoubleMatrix expectedHessianTilde_a(2, 2*2);

    expectedHessian(model, alp, b, bStep, 
        &expectedHessianTilde, &expectedHessianTilde_a, &expectedHessianTilde_b);
	    
    /*
    <Expected values>
    Htilde =
	[ 2 , 0 ],
	[ 0 , 3 ]
	}
	Htilde_alpOut = 
    [ 1   0 ]
    [ 0   0 ]
    [ 0   0 ]
    [ 1   0 ]
    Htilde_bOut =
    [ -2  0 ]
    [  0  0 ]
    [  0  0 ]
    [ -2  0 ]
    */

    DoubleMatrix Htilde(2,2);
    Htilde.fill(0);
    Htilde.data()[0] = 2.0;
    Htilde.data()[3] = 3.0;

    DoubleMatrix Htilde_alpOut(4,2);
    Htilde_alpOut.fill(0);
    Htilde_alpOut.data()[0] = 1.0;
    Htilde_alpOut.data()[3] = 1.0;

    DoubleMatrix Htilde_bOut(4,2);
    Htilde_bOut.fill(0);
    Htilde_bOut.data()[0] = -2.0;
    Htilde_bOut.data()[3] = -2.0;

    int i,j;
    DoubleMatrix tolB(b.nr(), 1);
    for( i = 0; i<b.nr(); i++ )
    {
        //
        // The true derivative of f(x) can be expressed in a talor's series:
        // f'(x) = 1 + x/2 + x^3/3 + ... for -1 < x < 1
        //
        // The central difference approximation of f(x) with h as step size is expanded:
        // {f(x+h) - f(x-h)}/(2.0*h) = 1 + 2x + (6x^2y + 2y^3) + ... for -1 < x < 1
        //
        // So, the difference between the two is:
        // f'(x) - {f(x+h) - f(x-h)}/(2.0*h) = (x/2 -2x) + (x^3/3 - 6x^2y + 2y^3)...
        // 
        //
        assert(bStep.data()[i] <= 1.0  );
        assert(bStep.data()[i] >= -1.0 );
        double x = b.data()[i];
        double h = bStep.data()[i];
        tolB.data()[i] = fabs(2.0*(x/2.0 - 2.0*x) + (x*x*x/3.0 - 6.0*x*x*h + 2*h*h*h));// + DBL_EPSILON*DBL_EPS_EQUAL_MULT;
    }
    for( j=0; j<2; j++ )
    {
        for( i=0; i<2; i++ )
            CPPUNIT_ASSERT_DOUBLES_EQUAL(Htilde.data()[i+j*2], expectedHessianTilde.data()[i+j*2], 0.001 /*tolB.data()[j]*/);
    }
    for( j=0; j<2; j++ )
    {
        for( i=0; i<4; i++ )
            CPPUNIT_ASSERT_DOUBLES_EQUAL(Htilde_bOut.data()[i+j*2], expectedHessianTilde_b.data()[i+j*2], 0.001/*tolB.data()[j]*/);
    }
    /*
    cout << "HtildeOut = " << endl;
    expectedHessianTilde.print();
    cout << "Htilde_alpOut = " << endl;
    expectedHessianTilde_a.print();
    cout << "Htilde_bOut = " << endl;
    expectedHessianTilde_b.print();
    */
    
}
void expectedHessianTest::testFullD()
{
    using namespace std;
    DoubleMatrix alp(2,1);
    alp.data()[0] = 1.0;
    alp.data()[1] = 2.0;
    
    DoubleMatrix b(2,1);
    b.fill(1.0);

    DoubleMatrix bStep(2,1);
    bStep.fill(0.1);

    fullDModel model;

    DoubleMatrix expectedHessianTilde(2, 2);
    DoubleMatrix expectedHessianTilde_b(2, 2*2);
    DoubleMatrix expectedHessianTilde_a(2, 2*2);

    expectedHessian(model, alp, b, bStep, 
        &expectedHessianTilde, &expectedHessianTilde_a, &expectedHessianTilde_b);
	    
/*
HtildeOut      = {
[ 2.33333 , -0.666667 ]
[ -0.666667 , 3.33333 ]
}

Htilde_alpOut  = {
[ 2.22222 , -0.444444 ]
[ -1.77778 , 0.555556 ]
[ -1.77778 , 0.555556 ]
[ 2.22222 , -0.444444 ]
}

Htilde_bOut    = {
[ -2 , 0 ]
[ 0 , 0 ]
[ 0 , 0 ]
[ -2 , 0 ]
}
}*/

    DoubleMatrix Htilde(2,2);
    Htilde.fill(0);
    Htilde.data()[0] = 2.33333;
    Htilde.data()[1] = -0.666667;
    Htilde.data()[2] = -0.666667;
    Htilde.data()[3] = 3.33333;

    DoubleMatrix Htilde_alpOut(4,2);
    Htilde_alpOut.fill(0);
    Htilde_alpOut.data()[0] = 2.22222;
    Htilde_alpOut.data()[1] = -1.77778;
    Htilde_alpOut.data()[2] = -1.77778;
    Htilde_alpOut.data()[3] = 2.22222;
    Htilde_alpOut.data()[4] = -0.444444;
    Htilde_alpOut.data()[5] = 0.555556;
    Htilde_alpOut.data()[6] = 0.555556;
    Htilde_alpOut.data()[7] = -0.444444;

    DoubleMatrix Htilde_bOut(4,2);
    Htilde_bOut.fill(0);
    Htilde_bOut.data()[0] = -2.0;
    Htilde_bOut.data()[1] =  0.0;
    Htilde_bOut.data()[2] =  0.0;
    Htilde_bOut.data()[3] = -2.0;
    Htilde_bOut.data()[4] =  0.0;
    Htilde_bOut.data()[5] =  0.0;
    Htilde_bOut.data()[6] =  0.0;
    Htilde_bOut.data()[7] =  0.0;

    int i,j;
    DoubleMatrix tolB(b.nr(), 1);
    for( i = 0; i<b.nr(); i++ )
    {
        //
        // The true derivative of f(x) can be expressed in a talor's series:
        // f'(x) = 1 + x/2 + x^3/3 + ... for -1 < x < 1
        //
        // The central difference approximation of f(x) with h as step size is expanded:
        // {f(x+h) - f(x-h)}/(2.0*h) = 1 + 2x + (6x^2y + 2y^3) + ... for -1 < x < 1
        //
        // So, the difference between the two is:
        // f'(x) - {f(x+h) - f(x-h)}/(2.0*h) = (x/2 -2x) + (x^3/3 - 6x^2y + 2y^3)...
        // 
        //
        assert(bStep.data()[i] <= 1.0  );
        assert(bStep.data()[i] >= -1.0 );
        double x = b.data()[i];
        double h = bStep.data()[i];
        tolB.data()[i] = fabs(2.0*(x/2.0 - 2.0*x) + (x*x*x/3.0 - 6.0*x*x*h + 2*h*h*h));// + DBL_EPSILON*DBL_EPS_EQUAL_MULT;
    }
    for( j=0; j<2; j++ )
    {
        for( i=0; i<2; i++ )
            CPPUNIT_ASSERT_DOUBLES_EQUAL(Htilde.data()[i+j*2], expectedHessianTilde.data()[i+j*2], 0.001 /*tolB.data()[j]*/);
    }
    for( j=0; j<2; j++ )
    {
        for( i=0; i<4; i++ )
            CPPUNIT_ASSERT_DOUBLES_EQUAL(Htilde_bOut.data()[i+j*2], expectedHessianTilde_b.data()[i+j*2], 0.001/*tolB.data()[j]*/);
    }
    
}
void expectedHessianTest::testModel()
{
    using namespace std;

    const double STEP = 0.01;
    int i,j, nr, nc;

    // set Model
    ExpectedHessianModel model;

    DoubleMatrix alpDM    (NUM_ALP, 1);
    DoubleMatrix alpStepDM(NUM_ALP, 1);
    DoubleMatrix alpTolDM (NUM_ALP, 1);

    valarray<double> alp(NUM_ALP);
    valarray<double> alpStep(NUM_ALP);
    valarray<double> alpTol(NUM_ALP);
    for( j = 0; j<NUM_ALP; j++ )
    {
        //
        // The true derivative of f(x) can be expressed in a talor's series:
        // f'(x) = 1 + x/2 + x^3/3 + ... for -1 < x < 1
        //
        // The central difference approximation of f(x) with h as step size is expanded:
        // {f(x+h) - f(x-h)}/(2.0*h) = 1 + 2x + (6x^2y + 2y^3) + ... for -1 < x < 1
        //
        // So, the difference between the two is:
        // f'(x) - {f(x+h) - f(x-h)}/(2.0*h) = (x/2 -2x) + (x^3/3 - 6x^2y + 2y^3)...
        // 
        //
        alp[j] = j+1;
        alpStep[j] = STEP;
        assert(alpStep[j] <= 1.0  );
        assert(alpStep[j] >= -1.0 );
        double x = alp[j];
        double h = alpStep[j];
        alpTol[j] = 0.001;// fabs(2.0*(x/2.0 - 2.0*x) + (x*x*x/3.0 - 6.0*x*x*h + 2*h*h*h));
    }
    
    DoubleMatrix bDM(NUM_B,1);
    DoubleMatrix bStepDM(NUM_B,1);
    DoubleMatrix bTolDM(NUM_B,1);

    valarray<double> b(NUM_B);
    valarray<double> bStep(NUM_B);
    valarray<double> bTol(NUM_B);
    for( j = 0; j<NUM_B; j++ )
    {
        //
        // The true derivative of f(x) can be expressed in a talor's series:
        // f'(x) = 1 + x/2 + x^3/3 + ... for -1 < x < 1
        //
        // The central difference approximation of f(x) with h as step size is expanded:
        // {f(x+h) - f(x-h)}/(2.0*h) = 1 + 2x + (6x^2y + 2y^3) + ... for -1 < x < 1
        //
        // So, the difference between the two is:
        // f'(x) - {f(x+h) - f(x-h)}/(2.0*h) = (x/2 -2x) + (x^3/3 - 6x^2y + 2y^3)...
        // 
        //
        b[j] = j+1;
        bStep[j] = STEP;
        assert(bStep[j] <= 1.0  );
        assert(bStep[j] >= -1.0 );
        double x = b[j];
        double h = bStep[j];
        bTol[j] = fabs(2.0*(x/2.0 - 2.0*x) + (x*x*x/3.0 - 6.0*x*x*h + 2*h*h*h));
    }

    model.setPopPar(alp);
    model.setIndPar(b);

    valarray<double> Dinv_a;
    model.indParVarianceInv_popPar(Dinv_a);

    ModelFunctionValarray DinvOb(&SpkModel<double>::indParVarianceInv, &model);
    {
        valarray<double> Dinv_aApprox = centdiff< binder2nd<ModelFunctionValarray> >(bind2nd(DinvOb,b), NUM_B, alp, alpStep);
/*
std::cout << "Dinv_a = " << std::endl;
Dinv_a.print();
std::cout << "Dinv_aApprox = " << std::endl;
Dinv_aApprox.print();
std::cout << "--------------" << endl;
*/
        for(nc=0; nc<NUM_ALP; nc++)
        {
            for(nr=0; nr<NUM_B * NUM_B; nr++)
            {
                CPPUNIT_ASSERT_DOUBLES_EQUAL(Dinv_a[nr+nc*NUM_B*NUM_B], Dinv_aApprox[nr+nc*NUM_B*NUM_B], alpTol[nc]);
            }
        }
    }

    for(i=0; i<NUM_INDS; i++)
    {
        const int NUM_Yi = IND_DATA_LEN[i];

        model.selectIndividual(i);
        model.setIndPar(b);

        valarray<double> f;
        model.dataMean(f);

        CPPUNIT_ASSERT_EQUAL(NUM_Yi, static_cast<int>( f.size() ) );
        
        ModelFunctionValarray fOb(&SpkModel<double>::dataMean, &model);

        valarray<double> f_b;
        model.dataMean_indPar(f_b);
        {
            valarray<double> f_bApprox = centdiff<binder1st<ModelFunctionValarray> >(bind1st(fOb,alp), 1, b, bStep);

            for(nc=0; nc<NUM_B; nc++)
            {
                for(nr=0; nr<NUM_Yi; nr++)
                   CPPUNIT_ASSERT_DOUBLES_EQUAL(f_b[nr+nc*NUM_Yi], f_bApprox[nr+nc*NUM_Yi], bTol[nc]);
            }
        }

        valarray<double> f_a;
        model.dataMean_popPar(f_a);
        {
            valarray<double> f_aApprox = centdiff<binder2nd<ModelFunctionValarray> >(bind2nd(fOb,b), 1, alp, alpStep);
            for(nc=0; nc<NUM_ALP; nc++)
            {
                for(nr=0; nr<NUM_Yi; nr++)
                {
                    CPPUNIT_ASSERT_DOUBLES_EQUAL(f_a[nr+nc*NUM_Yi], f_aApprox[nr+nc*NUM_Yi], alpTol[nc]);
                }
            }
        }

        ModelFunctionValarray ROb(&SpkModel<double>::dataVariance, &model);

        valarray<double> R_b;
        model.dataVariance_indPar(R_b);
        {
            valarray<double> R_bApprox = centdiff<binder1st<ModelFunctionValarray> >(bind1st(ROb,alp), NUM_Yi, b, bStep);

            for(nc=0; nc<NUM_B; nc++)
            {
                for(nr=0; nr<NUM_Yi * NUM_Yi; nr++)
                   CPPUNIT_ASSERT_DOUBLES_EQUAL(R_b[nr+nc*NUM_Yi], R_bApprox[nr+nc*NUM_Yi], bTol[nc]);
            }
        }
        valarray<double> R_a;
        model.dataVariance_popPar(R_a);
        {
            valarray<double> R_aApprox = centdiff<binder2nd<ModelFunctionValarray> >(bind2nd(ROb,b), NUM_Yi, alp, alpStep);
            for(nc=0; nc<NUM_ALP; nc++)
            {
                for(nr=0; nr<NUM_Yi * NUM_Yi; nr++)
                {
                    CPPUNIT_ASSERT_DOUBLES_EQUAL(R_a[nr+nc*NUM_Yi], R_aApprox[nr+nc*NUM_Yi], alpTol[nc]);
                }
            }
        }

        ModelFunctionValarray RinvOb(&SpkModel<double>::dataVarianceInv, &model);

        valarray<double> Rinv_b;
        model.dataVarianceInv_indPar( Rinv_b );
        {
            valarray<double> Rinv_bApprox = centdiff<binder1st<ModelFunctionValarray> >(bind1st(RinvOb,alp), NUM_Yi, b, bStep);
            for(nc=0; nc<NUM_B; nc++)
            {
                for(nr=0; nr<NUM_Yi * NUM_Yi; nr++)
                   CPPUNIT_ASSERT_DOUBLES_EQUAL(Rinv_b[nr+nc*NUM_Yi], Rinv_bApprox[nr+nc*NUM_Yi], bTol[nc]);
            }
        }

        valarray<double> Rinv_a;
        model.dataVarianceInv_popPar(Rinv_a);
        {
            valarray<double> Rinv_aApprox = centdiff<binder2nd<ModelFunctionValarray> >(bind2nd(RinvOb,b), NUM_Yi, alp, alpStep);
            for(nc=0; nc<NUM_ALP; nc++)
            {
                for(nr=0; nr<NUM_Yi * NUM_Yi; nr++)
                {
                    CPPUNIT_ASSERT_DOUBLES_EQUAL(Rinv_a[nr+nc*NUM_Yi], Rinv_aApprox[nr+nc*NUM_Yi], alpTol[nc]);
                }
            }
        }
    }
}

