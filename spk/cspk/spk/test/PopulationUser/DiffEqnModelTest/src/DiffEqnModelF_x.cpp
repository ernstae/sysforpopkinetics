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
// This source code is stored in the file DiffEqnModelF_x.cpp
# include <spk/SpkValarray.h>
# include "DiffEqnModel.h"
# include <spk/allZero.h>
# include "ode4rk.h"

using SPK_VA::valarray;

void DiffEqnModel::dG( void *info, const double z[], double g[] )
{    
     DiffEqnModel *m = (DiffEqnModel *) info;
     const double ai = m->_alp[0] + m->_b[0];
     const double si = m->_alp[1] + m->_b[1];
     g[0]      = -ai * z[0];
     g[1]      = +ai * z[0] - si * z[1];
     g[2]      =     - z[0] - ai * z[2];
     g[3]      =     + z[0] + ai * z[2] - si * z[3];
     g[4]      =     - z[1] - si * z[4];
} 
bool DiffEqnModel::doDataMean_indPar( valarray<double>& fi_bOut ) const 
{    
     if ( isCachedFibValid )
     {
       fi_bOut = cachedFib;
       return !allZero(fi_bOut);
     }

     // If the cached value for the mean model is not valid, 
     // then it will be computed here 
     if ( !isCachedFiValid )
     {
         cachedFi.resize(_Ni, 0.0);
     }

     valarray<double> z(0.0, 5);
     const double si = _alp[1] + _b[1];
     const double ci = _alp[2] * _w[_who] + _b[2];
     const double sidivci = si / ci;
     const double sidivcisqd = si / ( ci * ci );
     cachedFib.resize(_Ni * _Q, 0.0 ); 
     z[0] = _gamma[_who];
     double r0  = 0.;
     for( int j = 0; j < _Ni; j++)
     {   
         z = ode4rk((void *) this, dG, r0, _t[_ti+j], _odeStep, z);
         cachedFib[j + 0 * _Ni] = z[3] * sidivci;
         cachedFib[j + 1 * _Ni] = z[4] * sidivci + z[1] / ci;
         cachedFib[j + 2 * _Ni] = - z[1] * sidivcisqd;
         r0 = _t[_ti+j];
         if ( !isCachedFiValid )
         {
             cachedFi[j] = z[1] * sidivci;
         }
     }
     isCachedFiValid = true;
     isCachedFibValid = true;
     fi_bOut = cachedFib;
     return !allZero(fi_bOut);
}
bool DiffEqnModel::doDataMean_popPar( valarray<double>& fi_alpOut ) const 
{
     if ( isCachedFiaValid )
     {
       fi_alpOut = cachedFia;
       return !allZero(fi_alpOut);
     }
     valarray<double> fi_bOut(_Ni * _Q);
     doDataMean_indPar(fi_bOut);

     cachedFia.resize(_Ni * _P, 0.0 );
     for( int j = 0; j < _Ni; j++)
     {   
         cachedFia[j + 0 * _Ni] = fi_bOut[j + 0 * _Ni];
         cachedFia[j + 1 * _Ni] = fi_bOut[j + 1 * _Ni];
         cachedFia[j + 2 * _Ni] = fi_bOut[j + 2 * _Ni] * _w[_who];
     }

     isCachedFiaValid = true;
     fi_alpOut = cachedFia;
     return !allZero(fi_alpOut);
}
