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
// This source code is stored in the file DiffEqnModelF.cpp
# include <spk/SpkValarray.h>
# include "DiffEqnModel.h"
# include "ode4rk.h"

using SPK_VA::valarray;

//
// This is a static method of DiffEqnModel class that
// places the rate of mass change in each compartment in a vector, g.
//
void DiffEqnModel::G(void *info, const double z[], double g[])
{
    //
    // The DiffEqnModel object is used as a carrier of 
    // the current population and individual parameters.
    //
    DiffEqnModel *m = (DiffEqnModel *) info;
    
    //
    // Compute the 
    //
    //  ai(alp,b) = alp(1) + b(1)
    //  si(alp,b) = alp(2) + b(2)
    //
    double ai = m->_alp[0] + m->_b[0];
    double si = m->_alp[1] + m->_b[1];

    //
    // Compute:
    //                          /                          \
    //  partial_t Zi(t,alp,b) = |  -ai(alp,b)       0      |  *  Zi(t,alp,b)
    //                          |  +ai(alp,b)  -si(alp,b)  |
    //                          \                          /
    //
    g[0] = -ai * z[0];
    g[1] = +ai * z[0] - si * z[1];
}

void DiffEqnModel::doDataMean( valarray<double>& fiOut ) const 
{    
    if(isCachedFiValid)
    {
        fiOut = cachedFi;
        return;
    }

    cachedFi.resize(_Ni); 

    //
    // Compute the disposal rate and the reciprocal of clearance (si * vi)^1 from
    // the sampling compartment.
    //
    //  si(alp,b) = alp(2) + b(2)
    //  ci(alp,b) = alp(3) * weight + b(3)
    //
    const double si = _alp[1] + _b[1];
    const double ci = _alp[2] * _w[_who] + _b[2];

    //
    // Set the initial mass in each compartment:
    //                /           \
    //  Zi(0,alp,b) = |  gamma_i  |
    //                |    0.0    |
    //                \           /
    //
    valarray<double> z1(2), z2(2);
    z1[0] = _gamma[_who];
    z1[1] = 0.0;

    //
    // Predict the concentration of the drug in the sampling compartment for
    // each sampling point, assuming the time variable starts from 0.
    // 
    //  |                
    //  |                
    //  |                
    //  |        +--+    
    //  |        |  |    
    //  |     +--+  |    
    //  |     |  |  |    
    //  |  +--+  |  |    +--+
    //  +--+  |  |  |    |  |
    //  +-------------------------
    //  0  1  2  ...j ... m        0 <= j < m, where m is the number of samples
    //
    double preSamplingPoint = 0.0;
    for(int j = 0; j < _Ni; j++)
    {   
        //       
        // Estimate the mass at time=ti(j) by integrating
        // the differential equation placed by G function over ti(j-1) to ti(j).
        //
        //       /t1            /t2
        //       |              |
        //  z2 = | dg(t)/dt  +  | dg(t)/dt
        //       |              |
        //       /t1            /t1
        //
        // where dg(t)/dt ~= ode4rk( dg(t)/dt, t1, t2, h )
        //
        z2 = ode4rk((void *) this, DiffEqnModel::G, preSamplingPoint, _t[_ti+j], _odeStep, z1);

        // 
        // The predicated value for j-th measurement is the concentration of the drug
        // in the sampling compartment.
        // 
        //             /t2
        //             |
        // fi(alp,b) = |  dg(t)/dt / volume 
        //             |
        //             /t0
        // 
        //
        // f(j) =: mass / volume
        // z(2) =: mass
        // si   =: mass / t
        // ci   =: 1 / (si * vi)
        // si/ci=: 1 / v
        //
        cachedFi[j] = z2[1] * si / ci;
        preSamplingPoint  = _t[_ti+j];
        z1 = z2;
    }
    isCachedFiValid = true;
    fiOut = cachedFi;
}

