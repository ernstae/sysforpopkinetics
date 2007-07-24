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
 * File: DiffEqnModelF_x_Analytic.cpp
 *
 *
 * Analytic version of doDataMean_indPar.
 *
 * This function implements the derivatives of the model F that 
 * appears on page 40 of the NONMEM Users Guide -- Part I, 1989. 
 *
 * Author: Mitch Watrous
 * Extended by: Sachiko Honda
 *
 *************************************************************************/

#include <cmath>
#include "../../../../spk/SpkValarray.h"
#include "../../../../spk/allZero.h"

#include "DiffEqnModel.h"
#include "gval.h"

using SPK_VA::valarray;
using SPK_VA::slice;

static const valarray<double> fi_k( 
    enum DiffEqnModel::PK_TYPE pk, 
    const valarray<double> &alp, 
    const valarray<double> &b,
    const valarray<double> &ti,
    double w, 
    double gamma,
    valarray<double> &fiOut
    ) 
{
  valarray<double> minusAiTimesT( 1 );
  valarray<double> minusSiTimesT( 1 );

  valarray<double> gOut ( 1 );
  valarray<double> dgOut( 1 );

  valarray<double> zero( 0.0, 1 );
  valarray<double> one ( 1.0, 1 );

  const int n = ti.size();
  valarray<double> fi_kOut( n * 3 );
  fiOut.resize( n );

  double gij;
  double aij_alp;
  double gij_b;
  double fij_k1;
  double fij_k2;
  double fij_k3;
  double factor1;
  double factor2;
  double ai, si, ci;

  switch( pk )
  {
  case DiffEqnModel::PK_ADDITIVE:
    ai = alp[0] + b[0];
    si = alp[1] + b[1];
    ci = alp[2] * w + b[2];
    break;

  case DiffEqnModel::PK_PROPORTIONAL:
    ai = alp[0] * (1.0 + b[0]);
    si = alp[1] * (1.0 + b[1]);
    ci = alp[2] * w * (1.0 + b[2]);
    break;

  case DiffEqnModel::PK_EXPONENTIAL:
    ai = alp[0] * exp( b[0] );
    si = alp[1] * exp( b[1] );
    ci = alp[2] * w * exp( b[2] );
    break;
  }

  for( int j = 0; j < n; j++ )
  {
    double tij = ti[ j ];

    minusAiTimesT = -ai * tij;
    minusSiTimesT = -si * tij;

    //
    // gval() evaluates g(A, B) = (exp(A) - exp(B)) / (A - B) for each element pair
    // in the input matrices A and B.
    //
    // Let gval() compute g(A,B) and the partial derivative of g with respect to A.
    //
    gval(
      minusSiTimesT,    // A
      minusAiTimesT,    // B
      one,              // dA/dt
      zero,             // dB/dt
      gOut,
      dgOut );

    gij     = gOut [ 0 ]; 
    aij_alp = dgOut[ 0 ]; 

    //
    // Let gval() compute the partial derivative of g with respect to B.
    //
    gval(
      minusSiTimesT,
      minusAiTimesT,
      zero,
      one,
      gOut,
      dgOut );

    gij_b = dgOut[ 0 ]; 

    factor1 = gamma * tij * ai * si / ci;
    factor2 = tij * factor1;

    fiOut[j] = factor1 * gij;

    fij_k1 =  fiOut[j] / ai - factor2 * gij_b;
    fij_k2 =  fiOut[j] / si - factor2 * aij_alp;
    fij_k3 = -fiOut[j] / ci;

    fi_kOut[j + 0 * n] = fij_k1;
    fi_kOut[j + 1 * n] = fij_k2;
    fi_kOut[j + 2 * n] = fij_k3;
  }
/*
  //
  // This commented block contains the implementation of fi_k that does not use gval().
  //
  int j;
  const int n = ti.size();
  valarray<double> fi_kOut( n * 3 );
  double ai, si, ci;
  double fij, tij;
  double fij_k1, fij_k2, fij_k3;
  double factor0;
  double factor1;
  double factor2;
  double factor3;
  double factor4;

  switch( pk )
  {
  case DiffEqnModel::PK_ADDITIVE:
    ai = alp[0] + b[0];
    si = alp[1] + b[1];
    ci = alp[2] * w + b[2];
    break;

  case DiffEqnModel::PK_PROPORTIONAL:
    ai = alp[0] * (1.0 + b[0]);
    si = alp[1] * (1.0 + b[1]);
    ci = alp[2] * w * (1.0 + b[2]);
    break;

  case DiffEqnModel::PK_EXPONENTIAL:
    ai = alp[0] * exp( b[0] );
    si = alp[1] * exp( b[1] );
    ci = alp[2] * w * exp( b[2] );
    break;
  }

  // ai - si
  factor0 = ai - si;

  // ai * si * gammai
  factor1 = ai * si * gamma;

  // ci * (ai - si)
  factor2 = ci * factor0;

  for( j=0; j<n; j++ )
  {
    fij = fi[j];
    tij = ti[j];
 
    // fij / (ai - si)
    factor3 = fij / factor0;

    factor4 = ( factor1 * tij ) / factor2;
  
    //
    // Given k1 = alp(1) + b(1),
    //
    // fij_k1 = fij * [ 1 / k1i - 1 / ( k1i - k2i ) ] + [ k1i * k2i * gammai * tij * exp^(-k1i * tij) ] / [ k3i * (k1i - k2i) ]
    //        
    fij_k1 =  fij / ai - factor3 + factor4 *  exp( -ai * tij );

    //
    // Given k2 = alp(2) + b(2),
    //
    // fij_k2 = fij * [ 1 / k2i + 1 / ( k1i - k2i ) ] - [ k1i * k2i * gammai * tij * exp^(-k2i * tij) ] / [ k3i * (k1i - k2i) ]
    //        
    fij_k2 =  fij / si + factor3 - factor4 *  exp( -si * tij );

    //
    // Given k3 = alp(3) * wi + b(3)
    //
    // fij_k3 = -fij / k3i
    //
    fij_k3 = -fij / ci;

    fi_kOut[j + 0 * n] = fij_k1;
    fi_kOut[j + 1 * n] = fij_k2;
    fi_kOut[j + 2 * n] = fij_k3;
  }
*/
  return fi_kOut;
}

//
//    gamma_i
//      ||
//      \/
//    ------  ai   ------
//    | Ai | ----> | Si |
//    ------       ------
//                   || si
//                   \/
//
//                   ai * si * gamma_i    
//  fi(alp,bi)    = ------------------- * [ exp^(-si * ti) - exp^(-ai * ti) ]
//                   ci * ( ai - si )
//
//
//  where
//  ai(alp,b) = alp(1) + bi(1)          : (mass transfer rate from aborption to sample compartment)
//  si(alp,b) = alp(2) + bi(2)          : (mass transfer rate from sample to outside ) 
//  ci(alp,b) = alp(3) * weighti + bi(3): 
//
//
//                        [alp(1)+bi(1)] * [alp(2)+bi(2)] * gamma_i    
//  fi(alp,bi)    = -------------------------------------------------------- 
//                   [alp(3)+wi*bi(3)] * { [alp(1)+bi(1)] - [alp(2)+bi(2)] }
//               
//                * [ exp^-{ [alp(2)+bi(2)] * ti} - exp^-{ [alp(1)+bi(1)] * ti] }
//
//
//  fi_b(1)(alp,bi) = 
//                
// 
#include <spk/centdiff.h>
#include <spk/Function.h>
#include <functional>

bool DiffEqnModel::doDataMean_indPar( valarray<double>& fi_bOut ) const 
{
  fi_bOut.resize( _Ni * _b.size() );
  if ( isCachedFibValid )
  {
    fi_bOut = cachedFib;
    return !allZero(fi_bOut);
  }

  // If the cached value for the mean model is not valid, 
  // then it will be computed here 
  if ( !isCachedFiValid )
  {
    cachedFi.resize(_Ni);
  }

  cachedFib.resize(_Ni * _Q); 

#ifndef DERIV_TEST
  
  valarray<double> fiOut(_Ni);
  //dataMean(fiOut);
  valarray<double> fi_kOut = fi_k( _pk, _alp, _b, _t[ slice( _ti, _Ni, 1 ) ], _w[_who], _gamma[_who], fiOut );
  cachedFi = fiOut;
  isCachedFiValid = true;

  switch( _pk )
  {
  case PK_ADDITIVE:
    cachedFib = fi_kOut;
    break;

  case PK_PROPORTIONAL:
    cachedFib[ slice(     0, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice(     0, _Ni, 1 ) ]) * _alp[0];
    cachedFib[ slice(   _Ni, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice(   _Ni, _Ni, 1 ) ]) * _alp[1];
    cachedFib[ slice( 2*_Ni, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice( 2*_Ni, _Ni, 1 ) ]) * _alp[2] * _w[_who];
    break;

  case PK_EXPONENTIAL:
    double ai = _alp[0] * exp( _b[0] );
    double si = _alp[1] * exp( _b[1] );
    double ci = _alp[2] * _w[_who] * exp( _b[2] );
    cachedFib[ slice(     0, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice(     0, _Ni, 1 ) ]) * ai;
    cachedFib[ slice(   _Ni, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice(   _Ni, _Ni, 1 ) ]) * si;
    cachedFib[ slice( 2*_Ni, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice( 2*_Ni, _Ni, 1 ) ]) * ci;
    break;
  }

#else
    //////////////////////////////////////////////////////////////////////////////////////////////////
    // This block implements the approximation of the partial using central difference routine.
    //
    using namespace std;
    valarray<double> h(0.0001, _b.size());
    DiffEqnModel model( _alp.size(), _b.size(), _odeStep, _N, _gamma, _w, _t, _y, _pk, _er, _omega, _exp );
    model.selectIndividual( _who );
    ModelFunctionValarray fOb( &DiffEqnModel::dataMean, &model );

    valarray<double> f_bApprox = centdiff< binder1st< ModelFunctionValarray > >
    ( bind1st(fOb, _alp), 1, _b, h );
    cachedFib = f_bApprox;
    isCachedFiValid = false;
    //
    //////////////////////////////////////////////////////////////////////////////////////////////////
#endif

  isCachedFibValid = true;
  fi_bOut          = cachedFib;
  return !allZero(fi_bOut);
}

bool DiffEqnModel::doDataMean_popPar( valarray<double>& fi_alpOut ) const 
{
  fi_alpOut.resize( _Ni * _P );

  if ( isCachedFiaValid )
    {
      fi_alpOut = cachedFia;
      return !allZero(fi_alpOut);
    }

  cachedFia.resize( _Ni * _P );
  cachedFia = 0.0;  
  cachedFi.resize( _Ni );

#ifndef DERIV_TEST

  valarray<double> fiOut(_Ni);
  //dataMean(fiOut);
  valarray<double> fi_kOut = fi_k( _pk, _alp, _b, _t[ slice( _ti, _Ni, 1 ) ], _w[_who], _gamma[_who], fiOut );
  cachedFi = fiOut;
  isCachedFiValid = true;

  switch( _pk )
  {
  case PK_ADDITIVE:
    cachedFia[ slice(     0, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice(     0, _Ni, 1 ) ]);
    cachedFia[ slice(   _Ni, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice(   _Ni, _Ni, 1 ) ]);
    cachedFia[ slice( 2*_Ni, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice( 2*_Ni, _Ni, 1 ) ]) * _w[_who];
   break;

  case PK_PROPORTIONAL:
    cachedFia[ slice(     0, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice(     0, _Ni, 1 ) ]) * ( 1.0 + _b[0] );
    cachedFia[ slice(   _Ni, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice(   _Ni, _Ni, 1 ) ]) * ( 1.0 + _b[1] );
    cachedFia[ slice( 2*_Ni, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice( 2*_Ni, _Ni, 1 ) ]) * ( _w[_who] + _w[_who] * _b[2] );
    break;

  case PK_EXPONENTIAL:
    cachedFia[ slice(     0, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice(     0, _Ni, 1 ) ]) * exp(_b[0] );
    cachedFia[ slice(   _Ni, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice(   _Ni, _Ni, 1 ) ]) * exp(_b[1] );
    cachedFia[ slice( 2*_Ni, _Ni, 1 ) ] = valarray<double>(fi_kOut[ slice( 2*_Ni, _Ni, 1 ) ]) * _w[_who] * exp(_b[2]);
    break;
  }

#else
    //////////////////////////////////////////////////////////////////////////////////////////////////
    // This block implements the approximation of the partial using central difference routine.
    //
    using namespace std;
    valarray<double> h(0.0001, _alp.size());
    DiffEqnModel model( _alp.size(), _b.size(), _odeStep, _N, _gamma, _w, _t, _y, _pk, _er, _omega, _exp );
    model.selectIndividual( _who );
    ModelFunctionValarray fOb( &DiffEqnModel::dataMean, &model );

    valarray<double> f_alpApprox = centdiff< binder2nd< ModelFunctionValarray > > ( bind2nd(fOb, _b), 1, _alp, h );
    cachedFia = f_alpApprox;
    //
    //////////////////////////////////////////////////////////////////////////////////////////////////

#endif
   fi_alpOut        = cachedFia;
   isCachedFiaValid = true;
   return !allZero(fi_alpOut);
}
