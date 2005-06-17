/*
-----------------------------------------------------------------------
From:   Resource Facility for Population Kinetics
          Department of Bioengineering Box 352255
          University of Washington
          Seattle, WA 98195-2255

This file is part of the System for Population Kinetics (SPK), which
was developed with support from NIH grants RR-12609 and P41-
EB001975. Please cite these grants in any publication for which this
software is used and send a notification to the address given above.

SPK is Copyright (C) 1998-2003, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution.
-----------------------------------------------------------------------
Software:   Brad Bell (brad@apl.washington.edu)
Mathematics: Brad Bell & Jim Burke (burke@math.washington.edu)

$begin Residual$$
$spell
	std
	cassert
	cmath
	iostream
	da
	bool
	sqrt
	const
	Gnorm
	Fnorm
$$

$mindex central path residual$$
$section Central Path Residual$$

$table
$bold Syntax$$ $cnext
$syntax%double Residual(
	// Input Arguments
	size_t  %n%,
	const double  *%Q%,    // length n * n
	const double  *%r%,    // length n
	const double  *%l%,    // ...
	const double  *%u%,    // ...
	const double  *%x%,    // ...
	const double  *%a%,    // ...
	const double  *%b%,    // ...
	// Output Arguments
	double  *%FOut%)          // length 3 * n

%$$
$tend

$fend 20$$

$head Exceptions$$
The routine $code Residual$$ is $xref/glossary/Exception Safe/exception safe/$$.

$head Input Arguments$$
We define 
$latex n \in \Z_+$$,
$latex Q \in \R^{n \times n}$$,
$latex r \in \R^n$$,
$latex l \in \R^n$$,
$latex u \in \R^n$$,
$latex x \in \R^n$$,
$latex a \in \R^n$$,
$latex b \in \R^n$$,
as the values of the corresponding input arguments.
The matrix $latex Q$$ must be positive definite
$latex l < x < u$$,
$latex l < x < u$$,
$latex 0 < a $$,
$latex 0 < b $$.

$head Notation$$
We use $latex e \in \R^n$$ for the
$xref/glossary/e: Vector of Ones/vector of ones/$$.
We use $latex D(x)$$ for the
$xref/glossary/D: Diagonal Matrix of a Vector/diagonal matrix of a vector/$$.
We use $latex R( x , a , b )$$ for the
$xref/glossary/Residual Function/residual function/$$.

$head Output Arguments$$
The value $latex R( x , a , b )$$ will be placed in the
output argument $latex FOut$$
which must be a vector of length at $latex 3 n$$.
The return value of $code Residual$$ is the Euclidean norm
of $latex R( x , a , b )$$; i.e., the square root of the sum
of the squares of the elements of $latex FOut$$.

$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$codep */

# include "Residual.h"
# include "NearEqual.h"

# include <cassert>
# include <cmath>
# include <iostream>
# include <string>

bool ResidualTest(std::string &msg)
{
	bool   ok = true;
	int     n = 2;
	double  Q[] = {1., -1., -1., 2.};
	double  r[] = {1., 2.};
	double  l[] = {-1., -2.};
	double  u[] = {1., 1.};
	double  x[] = {.5, -.5};
	double  a[] = {1., 2.};
	double  b[] = {3., 4.};
	double  FOut[6];
	double  t;
	double  Fnorm;
	double  sum;
	size_t  i;

	// call to Residual with dx, da, db
	Fnorm = Residual(n, Q, r, l, u, x, a, b, FOut);

	ok &= NearEqual( FOut[0], 
		(u[0] - x[0]) * b[0] , 1e-12, 1e-12 );  
	ok &= NearEqual( FOut[1], 
		(u[1] - x[1]) * b[1] , 1e-12, 1e-12 );  
	ok &= NearEqual( FOut[2], 
		(x[0] - l[0]) * a[0] , 1e-12, 1e-12 );  
	ok &= NearEqual( FOut[3], 
		(x[1] - l[1]) * a[1] , 1e-12, 1e-12 );  
	ok &= NearEqual(FOut[4], 
		Q[0]*x[0] + Q[2]*x[1] + r[0] - a[0] + b[0] , 1e-12, 1e-12 );
	ok &= NearEqual(FOut[5], 
		Q[1]*x[0] + Q[3]*x[1] + r[1] - a[1] + b[1] , 1e-12, 1e-12 );
	sum = 0.;
	for(i = 0; i < 6; i++)
		sum += FOut[i] * FOut[i];
	ok &= NearEqual( Fnorm,  sqrt( sum ) , 1e-12, 1e-12 );

	msg = "";

	return ok;
}

/* $$
$end
--------------------------------------------------------------------------
*/

double Residual(
	// Input Arguments
	size_t  n,
	const double  *Q,    // length n * n
	const double  *r,    // length n
	const double  *l,    // ...
	const double  *u,    // ...
	const double  *x,    // ...
	const double  *a,    // ...
	const double  *b,    // ...
	// Output Arguments
	double  *FOut) // length 3 * n
{	
	size_t          i;
	size_t          j;
	double        tmp;
	double        sum;
	double        Qxi;

	for(i = 0; i < n; i++)
	{	if( x[i] <= l[i] || u[i] <= x[i] )
	       	{	std::cout << "Residual: " 
				<< "i = " << i
				<< ", l = " << l[i]
				<< ", x = " << x[i]
				<< ", u = " << u[i]
				<< std::endl;
		}

		assert( u[i] > x[i] );
		assert( x[i] > l[i] );
	}

	sum = 0.;
	for(i = 0; i < n; i++)
	{
		// Qxi = ( Q * x )[i]
		Qxi  = 0.;
		for(j = 0; j < n; j++)
			Qxi += Q[i + j * n] * x[j];

		FOut[i]         = (u[i] - x[i]) * b[i];
		FOut[i + n]     = (x[i] - l[i]) * a[i];
		FOut[i + 2 * n] = Qxi + r[i] - a[i] + b[i];

		sum += FOut[i] * FOut[i];
		sum += FOut[i + n] * FOut[i + n];
		sum += FOut[i + 2 * n] * FOut[i + 2 * n];
	}
	return sqrt( sum );
}
