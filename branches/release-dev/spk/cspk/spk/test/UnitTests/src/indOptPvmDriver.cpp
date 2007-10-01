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
 * File: indDriverTest.cpp
 *
 *
 * Unit test driver for indOptPvm.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cassert>
#include <pvm3.h>

#include "../../../spk/pi.h"
#include "../../../spk/Objective.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/indOptPvm.h"
#include "../../../spk/spkpvm.h"

using SPK_VA::valarray;

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

static void setUp()
{
  using namespace std;
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

int main( int argc, const char* argv[] )
{
    if(chdir(argv[2]) != 0)
    {
        int ptid = pvm_parent();
        pvm_pkstr("could not change working directory");
        pvm_send(ptid, SpkPvmErrorMessage);
        pvm_exit();
        return 100;  // FILE_ACCESS_FAILURE
    }

    const char* stderrFileName = "software_error";
    freopen( stderrFileName, "w", stderr );

    setUp();

    DiagDLTildePvmTest model;

    int exit_value = indOptPvm(argv[1], model);
    fclose( stderr );
    return exit_value;
}
//g++ indDriverTest.cpp -o indDriver -L/usr/local/lib -L/usr/local/lib/spktest -I/usr/local/include/spktest -L/usr/lib/atlas -I/usr/share/pvm3/include -L/usr/share/pvm3/lib/LINUX -lspk -lpvm3 -lxerces-c -latlas -lginac -lQN01Box -lgsl -llapack -llapack_atlas -lcblas
