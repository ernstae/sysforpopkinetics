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

$begin QuadFixed$$
$spell
	Itr
	sstream
	ostringstream
	buf
	str
	fabs
	blk
	apxnorm
	sqrt
	cout
	endl
	Qxi
	ntmp
	namespace
	bool
	strcmp
	cassert
	iostream
	cstddef
	std
	ostream
	complementarity
	Lagrangian
	Lagrange
	const
	os
$$

$section Quadratic Problem with Box Constraints: Approximate Complementarity$$

$table
$bold Syntax$$ $cnext
$syntax%
const char * QuadFixed(
	// Input Arguments
	std::ostream &%os%,
	size_t        %kMax%,
	size_t        %level%,
	size_t        %n%,
	double        %epsilon%,
	const double *%Q%, // length n * n
	const double *%r%, // length n 
	const double *%l%, // ...
	const double *%u%, // ...
	// Input and Output Arguments
	size_t       &%k%,
	double       *%x%, // length n
	double       *%a%, // ...
	double       *%b%  // ...
)%$$
$tend

$fend 20$$

$head Exceptions$$
The routine $code QuadFixed$$ is 
$xref/glossary/Exception Safe/exception safe/$$.

$head Description$$
This routine finds a solution to the problem
$latex \[
\begin{array}{lrl}
{\rm minimize} 
	& \frac{1}{2} x^T Q x + r^T x  
	& \wrt \; x \in \R^n
\\
\st            & L_i \leq x_i \leq U_i  & {\rm for} \; i = 1 , \cdots , n
\end{array}
\] $$
where $latex L_i = U_i = latex x_i$$
for components where the scaled projected gradient is near zero and
$latex L_i = l_i$$ and $latex U_i = u_i$$ for the other components.
To be precise,
let $latex p \in \R^n$$ be the 
$xref/Glossary/p: Scaled Projected Gradient/scaled projected gradient/$$
corresponding to the problem above with $latex a$$ replaced by $latex l$$
and $latex b$$ replaced by $latex u$$. 
The $latex L_i$$ and $latex U_i$$ are given by
$latex \[
\begin{array}{rcl}
L_i & = & 
\left\{ \begin{array}{ll}
	l_i & {\rm if} \; | p_i | > 1000 \delta |p|  \\
	x_i & {\rm otherwise} 
\end{array} \right.
\\
U_i & = & 
\left\{ \begin{array}{ll}
	u_i & {\rm if} \; | p_i | > 1000 \delta |p|   \\
	x_i & {\rm otherwise} 
\end{array} \right.
\end{array}
\] $$
where $latex \delta$$ is machine epsilon; i.e. $code DBL_EPSILON$$.

$head Return Value$$
If the return value of $code QuadFixed$$ is the (const char *) "ok",
it succeeded in meeting its specifications.
Otherwise, the return value is an error message and the specifications
have not been met.
Provided that all the input and output arguments have their specified lengths,
such a failure should not have any side effects.

$head Input Arguments$$
We define 
$latex n \in \Z_+$$,
$latex \varepsilon \in \R_+$$,
$latex Q \in \R^{n \times n}$$,
$latex r \in \R^n$$,
$latex l \in \R^n$$,
$latex u \in \R^n$$,
as the values of the corresponding input arguments.
The matrix $latex Q$$ must be positive definite,
$latex \varepsilon > 0$$,
and $latex l < u$$.
$pre

$$
In the special case where $italic n$$ is zero,
$code QuadFixed$$ returns with out making any changes to its arguments
and with its return value equal to the (const char *) "ok".

$subhead level$$
If $italic level = 0$$, no output is written by this routine.
If $latex level \geq 1$$, the following values are
written to the output stream specified by $italic os$$:
$table
$latex QuadFixed removed$$
	$cnext a list of components for which
	$latex L_i = U_i = x_i$$ $rnext
$latex k$$
	$cnext iteration number $rnext
$latex \alpha$$
	$cnext value of $latex \alpha$$ used call to $xref/Next/$$
	(except for corrector steps) $rnext
$latex \lambda$$
	$cnext value of $latex \lambda$$ returned by $xref/Next/$$ $rnext
$latex fk$$
	$cnext value of the norm of the residual of 
	$xref/
		glossary/
		C: The Complementarity Conditions/
		complementarity conditions/$$
	$rnext
$latex | C_u |_\infty $$
	$cnext the maximum upper complementarity component $rnext
$latex | C_l |_\infty $$
	$cnext the maximum lower complementarity component $rnext
$latex | L_x |_\infty $$
	$cnext the maximum component in gradient of Lagrangian w.r.t. $latex x$$ 
$tend
If $latex level \geq 2$$, 
for each iteration, and after the values listed above, 
the vectors $italic x$$, $latex a$$ and $latex b$$ are also
written to the output stream specified by $italic os$$.

$subhead os$$
In the event that $latex level > 0$$,
$italic os$$ specifies the output stream that
the output is written to.


$head Input and Output Arguments$$
The input value of $italic k$$ specifies the number
of previous iterations that have been made
($italic kMax$$ is a bound on previous plus current iterations.)
The output value of $italic k$$ is the total number of
iterations (previous plus current).
The input and output values 
$latex x \in \R^n$$, 
$latex a \in \R^n$$, and
$latex b \in \R^n$$ satisfy the following conditions:
$latex l < x < u$$,
$latex 0 < a$$,
$latex 0 < b$$.
There is an exception to this namely $latex a_i = b_i = 0$$
for all the components that are fixed.
The input values are an initial approximate solution of the
approximate first order conditions for the original problem
in terms of $latex l$$ and $latex u$$.
The output values satisfy the following approximate first order conditions:
$latex \[
\begin{array}{rcl}
	\varepsilon & \geq & | L_x ( x, a, b ) |_\infty \\
	\varepsilon & \geq & | C_l ( x, a, b ) |_\infty  \\
	\varepsilon & \geq & | C_u ( x, a, b ) |_\infty  
\end{array}
\] $$
for the derived problem in terms of $latex L$$ and $latex U$$.
Here $latex | \cdot |_\infty$$
is the 
$xref/glossary/Infinity Norm/infinity norm/$$.

$head Method$$
The routine $code QuadFixed$$ removes any fixed components and then
it uses the routine $xref/QuadBox/$$ to do the actual work.

$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$codep */

# include "QuadFixed.h"
# include "QuadBox.h"
# include "Memory.h"
# include "max.h"
# include "min.h"
# include "MaxAbs.h"

# include <cstddef>
# include <iostream>
# include <sstream>
# include <cassert>
# include <string>
# include <float.h>
# include <math.h>

bool QuadFixedTest(std::string &Msg)
{	bool ok = true;

	using namespace std;

	size_t i, j, k;
	double Qxi;

	size_t n   = 3;

	size_t ntmp = n * n  +  n * n + 6 * n;
	Memory<double> dMemory(ntmp); 
	double *A = dMemory(n * n);
	double *Q = dMemory(n * n);
	double *r = dMemory(n);
	double *l = dMemory(n);
	double *u = dMemory(n);
	double *x = dMemory(n);
	double *a = dMemory(n);
	double *b = dMemory(n);

	// A is any n x m matrix
	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
			A[ i * n + j ] = 0.;
		A[i * n + i] = 1.;
	}

	// l is lower limit
	// u is the upper limit
	// and solution point (x) is on the boundary
	for(j = 0; j < n; j++)
	{	l[j] = -.5;
		u[j] = +.5;
		if( j % 3 == 0 )
			x[j] = l[j];   // note x[0] == l[0]
		if( j % 3 == 1 )
			x[j] = l[j] + (u[j] - l[j]) / 3.;
		if( j % 3 == 2 )
			x[j] = u[j];
	}

	// Q = A * A^T
	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
		{	Q[i * n + j] = 0.;
			for(k = 0; k < n; k++)
				Q[i * n + j] += A[i * n + k] * A[j * n + k];
		}
	}
	double norm = MaxAbs(n * n, Q);
	for(i = 0; i < n * n; i++)
		Q[i] = Q[i] / norm;

	// r = - Q * x
	for(i = 0; i < n; i++)
	{	r[i] = 0.;
		for(j = 0; j < n; j++)
			r[i] -= Q[i * n + j] * x[j];
	}

	// initial x point
	for(i = 0; i < n; i++)
		x[i] = l[i] + (u[i] - l[i]) / 2.;

	// start x[0] so that corresponding constraint is degenerate
	x[0] = l[0] + 100. * DBL_EPSILON * (u[0] - l[0]);

	// maximum number of iterations
	size_t ItrMax = 40;

	// convergence criteria
	double epsilon = 1e-10;

	// iteration counter
	size_t ItrCur = 0;

	// level of tracing
	size_t level = 0;

	Msg = QuadFixed(	
		cout, 
		ItrMax, 
		level, 
		n, 
		epsilon, 
		Q, 
		r, 
		l, 
		u, 
		ItrCur, 
		x, 
		a, 
		b 
	);
	if( Msg == "ok" )
	{	std::ostringstream buf;
		buf << " :QuadFixed Iteration Count = " << ItrCur;
		Msg = buf.str();
	}
	else	ok = false;

	ok &= (a[0] == 0. && b[0] == 0.);
	for(i = 1; i < n; i++)
	{	ok &= (a[i] > 0. && b[i] > 0.);
		Qxi = 0;
		for(j = 0; j < n; j++)
			Qxi += Q[i * n + j] * x[j];

		double pi = Qxi + r[i] - a[i] + b[i];
		if( pi > 0 )
			ok &= fabs( pi ) * (x[i] - l[i]) <= epsilon;
		else	ok &= fabs( pi ) * (u[i] - x[i]) <= epsilon;
		ok &= a[i] * ( x[i] - l[i] ) <= epsilon;
		ok &= b[i] * ( u[i] - x[i] ) <= epsilon;
	}

	return ok;
}
/* $$
$end
-------------------------------------------------------------------------------
*/

namespace {
	void ScaleProjectGradient(
		size_t        n, 
		const double *l,
		const double *u,
		const double *x, 
		const double *g, 
		double       *p)
	{	while(n)
		{	--n;
			if( g[n] >= 0. )
				p[n] = (x[n] - l[n]) * g[n];
			else 	p[n] = (u[n] - x[n]) * g[n];
		}
	}
}


const char * QuadFixed(
	// Input Arguments
	std::ostream  &os     ,
	size_t        kMax    ,
	size_t        level   ,
	size_t        n       ,
	double        epsilon ,
	const double *Q       , // length n * n
	const double *r       , // length n 
	const double *l       , // ...
	const double *u       , // ...
	// Output Arguments
	size_t       &k       ,
	double       *x       , // length n
	double       *a       , // ...
	double       *b         // ...
)
{ 	// temporary indices
	size_t i, j;

	if( n == 0 )
		return (const char *) "ok";

	// allocate local memory 
	Memory<size_t> sMemory(n);
	size_t *index  = sMemory(n);

	Memory<bool>   bMemory(n);
	bool *fixed = bMemory(n);

	Memory<double> dMemory( n * n + 9 * n );
	double *Qf = dMemory(n * n);
	double *rf = dMemory(n);
	double *lf = dMemory(n);
	double *uf = dMemory(n);
	double *xf = dMemory(n);
	double *af = dMemory(n);
	double *bf = dMemory(n);
	double *Lx = dMemory(n);
	double *g  = dMemory(n);
	double *p  = dMemory(n);

	// compute of gradient of the objective function
	double sum = 0.;
	for(i = 0; i < n; i++)
	{ 	// g  = Q * x + r
		g[i] = r[i];
		for(j = 0; j < n; j++)
			g[i] += Q[i * n + j] * x[j];
	}

	// determine the scaled projected gradient
	ScaleProjectGradient(n, l, u, x, g, p);
	double pNorm = MaxAbs(n, p);

	// determine which components to fix
	for(i = 0; i < n; i++)
	{	double eps = DBL_EPSILON;
		fixed[i] =  fabs( p[i] ) <= 1e3 * eps * pNorm;
	}
	size_t nf =  0;
	for(i = 0; i < n; i++)
	{	if( ! fixed[i] )
		{	double Qxi = 0.;
			for(j = 0; j < n; j++)
			{	if( fixed[j] )
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
	assert( nf > 0 );
	for(i = 0; i < nf; i++)
		for(j = 0; j < nf; j++)
			Qf[ i * nf + j] = Q[ index[i] * n + index[j] ]; 

	if( level > 0 )
	{	os << "QuadFixed none removed: ";
		for(i = 0; i < n; i++)
			if( fixed[i] )
				os << "x[" << i << "] ";
		os << std::endl;
	}
	// solve the problem with fixed components removed
	double eps = epsilon;
	const char * msg = QuadBox(
		// Input Arguments
		os,
		kMax,
		level,
		nf,
		Qf,
		rf,
		lf,
		uf,
		// Output Arguments
		k,
		eps,
		xf,
		af,
		bf 
	);
	if( epsilon != eps )
		msg = "QuadFixed: could not obtain desired accuracy";
	if( strcmp(msg, "ok") != 0 )
		return msg;

	// value of a and b for fixed components
	for(i = 0; i < n; i++)
	{	a[i] = 0.;
		b[i] = 0.;
	}

	// value of a and b for other components
	for(i = 0; i < nf; i++)
	{	x[ index[i] ] = xf[i];
		a[ index[i] ] = af[i];
		b[ index[i] ] = bf[i];
	}
	return msg;
}
