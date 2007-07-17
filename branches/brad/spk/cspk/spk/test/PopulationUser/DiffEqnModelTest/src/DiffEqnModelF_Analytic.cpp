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
 * File: DiffEqnModelF_Analytic.cpp
 *
 *
 * Analytic version of doDataMean.
 *
 * This function implements the model F that appears on page 40 of 
 * the NONMEM Users Guide -- Part I, 1989. 
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/
#pragma warning (disable: 4786)

#include <cmath>
#include <limits>
#include "../../../../spk/SpkValarray.h"
#include "../../../../spk/mulByScalar.h"
#include "gval.h"

#include "DiffEqnModel.h"

using SPK_VA::valarray;

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
//  fi(j)(alp,bi) = ------------------- * [ exp^(-si * ti(j)) - exp^(-ai * ti(j)) ]
//                   ci * ( ai - si )
//
//  where ai, si and ci vary depending on the type of model.
//
void DiffEqnModel::doDataMean( valarray<double>& fiOut ) const 
{
  using namespace std;
  fiOut.resize(_Ni);

  if(isCachedFiValid)
  {
    fiOut = cachedFi;
    return;
  }

  cachedFi.resize(_Ni); 
  double ai, si, ci;

  switch( _pk )
  {
  case PK_ADDITIVE:
    //
    //  ai(alp,b) = alp(1) + bi(1)
    //  si(alp,b) = alp(2) + bi(2)
    //  ci(alp,b) = alp(3) * weighti + bi(3)
    //
    ai = _alp[0] + _b[0];
    si = _alp[1] + _b[1];
    ci = _alp[2] * _w[_who] + _b[2];
    break;
  case PK_PROPORTIONAL:
    //
    //  ai(alp,b) = alp(1) * bi(1)
    //  si(alp,b) = alp(2) * bi(2)
    //  ci(alp,b) = alp(3) * weighti * bi(3)
    //
    ai = _alp[0] * (1.0 + _b[0]);
    si = _alp[1] * (1.0 + _b[1]);
    ci = _alp[2] * _w[_who] * (1.0 + _b[2]);
    break;

  case PK_EXPONENTIAL:
    //
    //  ai(alp,b) = alp(1) * exp( bi(1) )
    //  si(alp,b) = alp(2) * exp( bi(2) )
    //  ci(alp,b) = alp(3) * weighti * exp( bi(3) )
    //
    ai = _alp[0] * exp( _b[0] );
    si = _alp[1] * exp( _b[1] );
    ci = _alp[2] * _w[_who] * exp( _b[2] );
    break;

  default:
    cerr << "ERROR!!! " << endl;
    break;
  }

  // Get the current individual's time values.
  valarray<double> ti   ( _t[ std::slice( _ti, _Ni, 1 ) ] );

  valarray<double> gOut ( _Ni );
  valarray<double> dgOut( _Ni );
  valarray<double> zero ( 0.0, _Ni );

  //
  // gval() evaluates g(a, b) = (exp(a) - exp(b)) / (a - b) for each element pair
  // in the input matrices A and B.
  //
  // Thus, g( A, B ) = ( exp( A ) - exp( B ) ) / ( A - B )
  // 
  // where A = - si * ti
  //       B = - ai * ti
  //
  gval(
    -si * ti,         // si * t : mass moved from absorption to sample compartment
    -ai * ti,         // ai * t : mass moved from sample to outside
    zero,             // d(si * t) /dt
    zero,             // d(ai * t) /dt
    gOut,
    dgOut );

  cachedFi = ( ai * si * _gamma[_who] / ci ) * gOut * ti;
  for( int i=0; i<_Ni; i++ )
  {
    if( cachedFi[i] == numeric_limits<double>::infinity() )
    {
      const int len = SpkError::maxMessageLen();
      char message [ len ];
      sprintf(  message,
                "Inidiviudal index (0-): %d\n \
                Vector fi() index (0-): %d\n \
                The value, %f, is not a valid number!\n",
                _who, i, cachedFi[i] );

      throw SpkException( SpkError::SPK_FP_INVALID_ERR, message, __LINE__, __FILE__ );
    }
  }

  isCachedFiValid = true;
  fiOut           = cachedFi;
}
