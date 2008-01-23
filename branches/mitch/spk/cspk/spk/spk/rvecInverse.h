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
#ifndef RVECINVERSE_H
#define RVECINVERSE_H

#include "DoubleMatrix.h"
#include "rvecInverse.h"
//const DoubleMatrix rvecInverse(const DoubleMatrix &a,  int m);

inline const DoubleMatrix rvecInverse(const DoubleMatrix &a, int m)
{
    // m will be used as a denominator.  so, must test it.
    assert(m > 0);

    // a's gotta be a column vector
    assert(a.nc() == 1);
    assert(a.nr() % m == 0);
    const int k = a.nr() / m;

    // test if the size of a is divisible by m.
    assert(k*m == a.nr());

    DoubleMatrix C(k,m);

    const double* source = a.data();
    double* destination = C.data();

    // 
    // C = rvecInverse(a,m)
    //
    // m > 0
    //
    // a: (k*m) dimensional column vector
    //    transpose([ a(1,1), a(1,2)..., a(1,m), a(2,1), a(2,2)..., a(2,m).......a(k,m) ])
    //
    // C: k by m matrix
    //    [ a(1,1) a(1,2)... a(1,m) ]
    //    [ a(2,1) a(2,2)... a(2,m) ]
    //    [ ...                     ]
    //    [ a(k,1) a(k,2)... a(k,m) ]
    //
    for( int j=0; j<m; j++ )
    {
        for( int i=0; i<k; i++ )
        {
            destination[i+j*k] = source[i*m+j];
        }
    }
    return C;
}
inline void rvecInverse(const DoubleMatrix &a, int m, DoubleMatrix &A)
{
    // m will be used as a denominator.  so, must test it.
    assert(m > 0);

    // a's gotta be a column vector
    assert(a.nc() == 1);
    assert(a.nr() % m == 0);
    const int k = a.nr() / m;

    // test if the size of a is divisible by m.
    assert(k*m == a.nr());

    A.resize(k,m);

    const double* source = a.data();
    double* destination = A.data();

    // 
    // A = rvecInverse(a,m)
    //
    // m > 0
    //
    // a: (k*m) dimensional column vector
    //    transpose([ a(1,1), a(1,2)..., a(1,m), a(2,1), a(2,2)..., a(2,m).......a(k,m) ])
    //
    // A: k by m matrix
    //    [ a(1,1) a(1,2)... a(1,m) ]
    //    [ a(2,1) a(2,2)... a(2,m) ]
    //    [ ...                     ]
    //    [ a(k,1) a(k,2)... a(k,m) ]
    //
    for( int j=0; j<m; j++ )
    {
        for( int i=0; i<k; i++ )
        {
            destination[i+j*k] = source[i*m+j];
        }
    }
}
#endif
