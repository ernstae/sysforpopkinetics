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
Version: 03-11-21

$begin QuadTrust$$
$spell
	fabs
	complementarity
	ostream
	cassert
	Lagragian
	sqrt
	apxnorm
	std
	cout
	endl
	Qxi
	cstddef
	iostream
	bool
	strcmp
	namespace
	ntmp
	const
	blk
$$

$section Quadratic Problem with Box Constraints: Exact Complementarity$$

$table
$bold Syntax$$ $cnext
$syntax%
const char * QuadTrust(
	// Input Arguments
	std::ostream   &%os%,
	size_t        %kMax%,
	size_t       %level%,
	size_t        %n%,
	double        %delta%,
	const double *%Q%, // length n * n
	const double *%r%, // length n 
	const double *%l%, // ...
	const double *%u%, // ...
	// Output Arguments
	size_t       &%k%,
	double       *%x%, // length n
	double       *%a%, // ...
	double       *%b%  // ...
)%$$
$tend

$fend 20$$

$head Description$$
Uses the Interior Point method to find an approximation solution
to the problem
$xref/glossary/P: Quadratic Problem with Box Constraints/P(Q, r, l, u)/$$.
The approximate solution exactly solves the complementarity conditions
(see $xref/QuadTrust/Output Arguments/output arguments/$$).

$head Exceptions$$
The routine $code QuadTrust$$ is $xref/glossary/Exception Safe/exception safe/$$.

$head Return Value$$
If the return value of $code QuadTrust$$ is the (const char *) "ok",
it succeeded in meeting its specifications.
Otherwise, the return value is an error message and the specifications
have not been met.
Provided that all the input and output arguments have their specified lengths,
such a failure should not have any side effects.

$head Input Arguments$$
The input value $italic kMax$$ is the maximum number of iterations
of the interior point method to use before giving up on solving the problem.
We define 
$latex n \in \Z_+$$,
$latex \delta \in \R_+$$,
$latex Q \in \R^{n \times n}$$,
$latex r \in \R^n$$,
$latex l \in \R^n$$,
$latex u \in \R^n$$,
as the values of the corresponding input arguments.
The matrix $latex Q$$ must be positive definite,
$latex \delta > 0$$.

$subhead level$$
If $italic level = 0$$, no output is written by this routine.
If $latex level \geq 1$$, 
each time the set of binding constraints changes,
the following values are
written to the output stream specified by $italic os$$:
$table
$latex k$$  $cnext
the number of interior point iterations used so far
$rnext
$latex l$$ $cnext
the lower limit for the $italic x$$
$rnext
$latex x$$ $cnext
the approximate solution corresponding to this binding set
$rnext  
$latex u$$ $cnext
the upper limit for $italic x$$
$rnext
$latex Lx$$ $cnext
the gradient of the Lagragian corresponding to this binding set
$rnext
$tend

$head Problem$$
The input parameters define the following problem
$latex \[
\begin{array}{lrl}
{\rm minimize} 
	& \frac{1}{2} x^T Q x + r^T x  
	& \wrt \; x \in \R^n
\\
\st
	& l \leq  x \leq u 
\end{array}
\] $$

$head Output Arguments$$
The output value $italic k$$ is the number of iterations
of the interior point method used to obtain the approximate solution.
The output values 
$latex x \in \R^n$$, 
$latex a \in \R^n$$, and
$latex b \in \R^n$$ satisfy the following conditions:
$latex l \leq x \leq u$$,
$latex 0 \leq a$$,
$latex 0 \leq b$$,
and
$latex \[
\begin{array}{rcl}
	\delta & \geq & \left| \D{L}{x} L ( x, a, b ) \right|_\infty  \\
	0           & =    & C_l ( x, a, b )  \\
	0           & =    & C_u ( x, a, b ) 
\end{array}
\] $$
where $latex L$$ is the 
$xref/glossary/L: The Lagrangian/Lagrangian/$$,
$latex | \cdot |_\infty$$
is the 
$xref/glossary/Infinity Norm/infinity norm/$$,
the functions $latex C_l$$ and $latex C_u$$ are the
$xref/glossary/C: The Complementarity Conditions/complementarity conditions/$$.


$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$codep */

# include "QuadTrust.h"
# include "QuadBox.h"
# include "Memory.h"
# include "max.h"
# include "min.h"
# include "MaxAbs.h"

# include <cstddef>
# include <iostream>
# include <cassert>
# include <string>
# include <float.h>
# include <math.h>


bool QuadTrustTest(std::string &Msg)
{	bool ok = true;

	using namespace std;

	size_t i1;
	size_t i2;
	size_t i, j;
	double Qxi;

	size_t n   = 20;
	size_t m   = 10;

	size_t ntmp = n * m  +  n * n + 6 * n;
	Memory<double> dMemory(ntmp); 
	double *A = dMemory(n * m);
	double *Q = dMemory(n * n);
	double *r = dMemory(n);
	double *l = dMemory(n);
	double *u = dMemory(n);
	double *x = dMemory(n);
	double *a = dMemory(n);
	double *b = dMemory(n);

	// A is a random n x m matrix
	for(j = 0; j < n * m; j++)
		A[j] = rand() / (double) RAND_MAX - .5;

	// r is the linear coefficient
	// l is lower limit
	// u is the upper limit
	double sum = 0.;
	for(j = 0; j < n; j++)
	{	r[j] = rand() / (double) RAND_MAX - .5;
		l[j] = -.5;
		u[j] = +.5;

		sum += r[j] * r[j];
	}

	double apxnorm = sqrt(sum / double(n));
	for(j = 0; j < n; j++)
		r[j] = .5 * r[j] / apxnorm;

	// Q = A * A^T
	sum     = 0.;
	apxnorm = 0.;
	for(i1 = 0; i1 < n; i1++)
	{	for(i2 = 0; i2 < n; i2++)
		{	Q[i1 * n + i2] = 0.;
			for(j = 0; j < m; j++)
				Q[i1 * n + i2] += A[i1 * m + j] * A[i2 * m + j];
			sum += Q[i1 * n + i2] * Q[i1 * n + i2];
		}
		apxnorm += sqrt(sum / double(n) );
	}
	apxnorm /= double(n);
	for(i = 0; i < n*n; i++)
		Q[i] = Q[i] / apxnorm;

	// maximum number of iterations
	size_t kMax = 40;

	// convergence criteria
	double delta = 1e-10;

	// iteration counter
	size_t k;

	// level of tracing
	size_t level = 0;

	Msg = QuadTrust(cout, kMax, level, n, delta, Q, r, l, u, k, x, a, b );
	if( Msg == "ok" )
		Msg = "";
	else	ok = false;

	for(i = 0; i < n; i++)
	{	Qxi = 0;
		for(j = 0; j < n; j++)
			Qxi += Q[i * n + j] * x[j];

		ok &= fabs( Qxi + r[i] - a[i] + b[i] ) <= delta;
		ok &= a[i] * (x[i] - l[i]) == 0.;
		ok &= b[i] * (u[i] - x[i]) == 0.;
	}

	return ok;
}
/* $$
$end
-------------------------------------------------------------------------------
*/

const char * QuadTrust(
	// Input Arguments
	std::ostream   &os,
	size_t        kMax,
	size_t       level,
	size_t        n,
	double        delta,
	const double *Q, // length n * n
	const double *r, // length n 
	const double *l, // ...
	const double *u, // ...
	// Output Arguments
	size_t       &k,
	double       *x, // length n
	double       *a, // ...
	double       *b // ...
)
{

	// temporary indices
	size_t i;
	size_t j;

	// allocate local memory 
	Memory<size_t> sMemory(n);
	size_t *index  = sMemory(n);

	Memory<bool>   bMemory(n);
	bool *binding = bMemory(n);

	Memory<double> dMemory( n * n + 8 * n );
	double *Qf = dMemory(n * n);
	double *rf = dMemory(n);
	double *lf = dMemory(n);
	double *uf = dMemory(n);
	double *xf = dMemory(n);
	double *af = dMemory(n);
	double *bf = dMemory(n);
	double *Fx = dMemory(n);
	double *Lx = dMemory(n);


	// initial iteration counter
	k = 0;

	// set the initial face and x value
	for(i = 0; i < n; i++)
	{	assert( l[i] <= u[i] );

		// are we optimizing over this component
		if( l[i] < u[i] )
		{	binding[i] = false;
			x[i]       = ( l[i] + u[i] ) / 2.;
		}
		else
		{	binding[i] = true;
			x[i]       = l[i];
		}
	}

	// compute of gradient of the objective function
	double sum = 0.;
	for(i = 0; i < n; i++)
	{ 	// Fx  = Q * x + r
		Fx[i] = r[i];
		for(j = 0; j < n; j++)
			Fx[i] += Q[i * n + j] * x[j];
	}

	// initialize a, b so complementarity conditions Cl and Cu have 
	// about the same residual as gradient of the objective
	for(i = 0; i < n; i++)
	{	double bnd = max(fabs(l[i]) , fabs(u[i]) );
		bnd        = max( bnd, fabs( Fx[i] ) );
		if( binding[i] )
			a[i] = b[i] = 0.;
		else	a[i] = b[i] = bnd;
	}

	// Begin main loop over which face to optimizer w.r.t.
	while( k < kMax )
	{
		// define problem corresponding to the face defined by binding 
		size_t nf = 0;
		for(i = 0; i < n; i++)
		{	if( ! binding[i] )
			{	double Qxi = 0.;
				for(j = 0; j < n; j++)
				{	if( binding[j] )
						Qxi += Q[i * n + j] * x[j]; 
				}
				rf[nf]    = Qxi + r[i];
				lf[nf]    = l[i];
				uf[nf]    = u[i];
				xf[nf]    = x[i];
				af[nf]    = a[i];
				bf[nf]    = b[i];

				assert( lf[nf] < xf[nf] );
				assert( xf[nf] < uf[nf] );

				double bnd = max(fabs(l[i]) , fabs(u[i]) );
				if( af[nf] == 0. )
					af[nf] = bnd;
				if( bf[nf] == 0. )
					bf[nf] = bnd;

				index[nf] = i;
				nf++;
			}
		}
		for(i = 0; i < nf; i++)
			for(j = 0; j < nf; j++)
				Qf[ i * nf + j] = Q[ index[i] * n + index[j] ]; 

		// solve the face problem
		std::ostream &os = std::cout;
		size_t sublevel   = 0;
		double epsilon = 1e-2 * delta;
		const char * msg = QuadBox(
			// Input Arguments
			os,
			kMax,
			sublevel,
			nf,
			epsilon,
			Qf,
			rf,
			lf,
			uf,
			// Output Arguments
			k,
			xf,
			af,
			bf 
		);

		if( strcmp(msg, "ok") != 0 )
			return msg;

		// move solution to originl indices
		for(i = 0; i < n; i++)
		{	a[i] = 0.;
			b[i] = 0.;
		}
		for(i = 0; i < nf; i++)
		{	x[ index[i] ] = xf[i];
			a[ index[i] ] = af[i];
			b[ index[i] ] = bf[i];
		}

		// gradient of objective, Lagragian corresponding to x,
		// and the infinity norm of the Lagrangian
		double LxNorm = 0.;
		for(i = 0; i < n; i++)
		{ 	// Fx = Q * x + r 
			// Lx = Fx - a + b
			Fx[i] = r[i];
			for(j = 0; j < n; j++)
				Fx[i] += Q[i * n + j] * x[j];

			if( Fx[i] > 0. )
			{	if( x[i] == l[i] )
					Lx[i] = 0.;
				else	Lx[i] = Fx[i];
			}
			else
			{	if( x[i] == u[i] )
					Lx[i] = 0.;
				else	Lx[i] = Fx[i];
			}

			LxNorm = max( LxNorm , fabs( Lx[i] ) );
		}
		if( level > 0 )
		{	os << "k  = " << k << std::endl;
			os << "l = ";
			for(i = 0; i < n; i++)
			{	os.width(12);
				if( l[i] == x[i] )
					os << "x[i]";
				else	os << l[i];
				if( i == n-1 )
					os << std::endl;
				else
				{	os << ", ";
					if( i % 5 == 4 ) os << "..." 
						   << std::endl << "     ";
				}
			}
			os << "x  = ";
			for(i = 0; i < n; i++)
			{	os.width(12);
				os << x[i];
				if( i == n-1 )
					os << std::endl;
				else
				{	os << ", ";
					if( i % 5 == 4 ) os << "..." 
						   << std::endl << "     ";
				}
			}
			os << "u  = ";
			for(i = 0; i < n; i++)
			{	os.width(12);
				if( u[i] == x[i] )
					os << "x[i]";
				else	os << u[i];
				if( i == n-1 )
					os << std::endl;
				else
				{	os << ", ";
					if( i % 5 == 4 ) os << "..." 
						   << std::endl << "     ";
				}
			}
			os << "Lx = ";
			for(i = 0; i < n; i++)
			{	os.width(12);
				os << Lx[i];
				if( i == n-1 )
					os << std::endl;
				else
				{	os << ", ";
					if( i % 5 == 4 ) os << "..." 
						   << std::endl << "     ";
				}
			}
		}

		// check for convergence
		if( LxNorm <= delta )
		{	// convert complimentarity to exact form
			for(i = 0; i < n; i++)
			{	if( Fx[i] > 0. && x[i] == l[i] )
					a[i] = Fx[i];
				else	a[i] = 0.;
				if( Fx[i] < 0. && x[i] == u[i] )
					b[i] = - Fx[i];
				else	b[i] = 0.;
			}
			return (const char *)("ok");
		}

		// adjust the constraint set
		bool change = false;
		epsilon = min( 1e1 * delta, 1e-1 * LxNorm );
		for(i = 0; i < n; i++)
		{	if( fabs( Lx[i] ) > epsilon )
			{	if( binding[i] )
				{	change     = true;
					binding[i] = false;
					x[i]       = (l[i] + u[i]) / 2.;
					nf++;  // increase free set
				}
				else if( a[i] > b[i] && a[i] > epsilon )
				{	change     = true;
					binding[i] = true;
					x[i]       = l[i];
					assert( nf > 0 );
					nf--;  // decrease free set
				}
				else if( b[i] > a[i] && b[i] > epsilon )
				{	change     = true;
					binding[i] = true;
					x[i]       = u[i];
					assert( nf > 0 );
					nf--;  // decrease free set
				}
			}
		}
		if( ! change ) (const char *) 
			("QuadTrust: could not obtain desired convergence");
	}
	return (const char *)("QuadTrust: Maximum number of iterations reached");
}
