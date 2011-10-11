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

$begin QuadBox$$
$spell
	cmath
	eps
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
const char * QuadBox(
	// Input Arguments
	std::ostream &%os%     ,
	size_t        %kMax%   ,
	size_t        %level%  ,
	size_t        %n%      ,
	double       %eIn%     ,
	const double *%Q%      , // length n * n
	const double *%r%      , // length n 
	const double *%l%      , // ...
	const double *%u%      , // ...
	// Input and Output Arguments
	size_t       &%k%      ,
	double       *%x%      , // length n
	double       *%a%      , // ...
	double       *%b%      , // ...
	double       %eOut%
)%$$
$tend

$fend 20$$

$head Exceptions$$
The routine $code QuadBox$$ is $xref/glossary/Exception Safe/exception safe/$$.

$head Description$$
Uses the Interior Point method to find an approximation solution
to the problem
$xref/glossary/P: Quadratic Problem with Box Constraints/P(Q, r, l, u)/$$.
The approximate solution approximately satisfies the complementarity conditions
(see $xref/QuadBox/Input and Output Arguments/input and output arguments/$$).

$head Return Value$$
If the return value of $code QuadBox$$ is the (const char *) "ok",
it succeeded in meeting its specifications.
Otherwise, the return value is an error message and the specifications
have not been met.
Provided that all the input and output arguments have their specified lengths,
such a failure should not have any side effects.

$head Input Arguments$$

$head n, Q, r, l, u$$
We define 
$latex n \in \Z_+$$,
$latex Q \in \R^{n \times n}$$,
$latex r \in \R^n$$,
$latex l \in \R^n$$,
$latex u \in \R^n$$,
as the values of the corresponding input arguments.
The matrix $latex Q$$ must be positive definite
and $latex l < u$$.
$pre

$$
In the special case where $italic n$$ is zero,
$code QuadBox$$ returns with $italic eOut$$ equal to zero and
with its return value equal to "ok".

$subhead level$$
If $italic level = 0$$, no output is written by this routine.
If $latex level \geq 1$$, the following values are
written to the output stream specified by $italic os$$:
$table
$latex k$$
	$cnext iteration number $rnext
$latex \alpha$$
	$cnext value of $latex \alpha$$ used call to $xref/Next/$$
	(except for corrector steps) $rnext
$latex \lambda$$
	$cnext value of $latex \lambda$$ returned by $xref/Next/$$ $rnext
$latex fk$$
	$cnext square of the norm of the residual of 
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

$subhead eIn$$
The value $italic eIn$$ is the requested 
convergence criteria as a maximum for 
$latex \[
| L_x ( x, a, b ) |_\infty  \; ,\;
| C_l ( x, a, b ) |_\infty  \; ,\;
| C_u ( x, a, b ) |_\infty  
\] $$
at the output value corresponding
to the arguments $italic x$$, $italic a$$ and $italic b$$.
Here and below $latex | \cdot |_\infty$$ is the
$xref/glossary/Infinity Norm/infinity norm/$$.


$head Input and Output Arguments$$

$subhead k$$
The input value of $italic k$$ specifies the number
of previous iterations that have been made
($italic kMax$$ is a bound on previous plus current iterations.)
The output value of $italic k$$ is the total number of
iterations (previous plus current).

$subhead x,a,b$$
The input and output values 
$latex x \in \R^n$$, 
$latex a \in \R^n$$, and
$latex b \in \R^n$$ satisfy the following conditions:
$latex l < x < u$$,
$latex 0 < a$$,
$latex 0 < b$$.
The input values are an initial approximate solution of the
approximate first order conditions.
The output values satisfy the following approximate first order conditions:
$latex \[
\begin{array}{rcl}
	eOut & \geq & | L_x ( x, a, b ) |_\infty \\
	eOut & \geq & | C_l ( x, a, b ) |_\infty  \\
	eOut & \geq & | C_u ( x, a, b ) |_\infty  
\end{array}
\] $$
at the output value corresponding
to the arguments $italic x$$, $italic a$$ and $italic b$$.

$head eOut$$
The return value $italic eOut$$ is the maximum of
$latex \[
| L_x ( x, a, b ) |_\infty  \; ,\;
| C_l ( x, a, b ) |_\infty  \; ,\;
| C_u ( x, a, b ) |_\infty  
\] $$
at the output value corresponding
to the arguments $italic x$$, $italic a$$ and $italic b$$.
The routine $code QuadBox$$ will attempt to obtain the 
inequality $latex eOut \leq eIn$$.
Even if $code QuadBox$$ returns "ok",
you must check $italic eOut$$ to see if you have obtained
a residual less than or equal the requested value $italic eIn$$.

$head Subroutines$$
$children%
	lib/Next.cpp%
	lib/Residual.cpp
%$$
$table
$rref Next$$
$rref Residual$$
$tend


$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$codep */

# include <QN01Box/Error.h>
# include <QN01Box/Memory.h>
# include <QN01Box/QuadBox.h>
# include <QN01Box/Residual.h>
# include <QN01Box/MaxAbs.h>
# include <QN01Box/Next.h>

# include <climits>
# include <cstddef>
# include <cstdlib>
# include <cstring>
# include <iostream>
# include <string>
# include <sstream>
# include <cmath>
# include <float.h>

static bool Ok(const char * msg)
{	return strcmp(msg, "ok") == 0;
}

bool QuadBoxTest(std::string &Msg)
{	bool ok = true;
	using namespace QN01Box;
	using std::cout;
	using std::endl;

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
	// x is initial value for function argument
	// a is initial value for lower limit Lagrange multipliers
	// b is initial value for upper limit Lagrange multipliers
	double sum = 0.;
	for(j = 0; j < n; j++)
	{	r[j] = rand() / (double) RAND_MAX - .5;
		l[j] = -.5;
		u[j] = +.5;
		x[j] = 0.;
		a[j] = 1.;
		b[j] = 1.;

		sum += r[j] * r[j];
	}
	double apxnorm = std::sqrt(sum / double(n));
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
		apxnorm += std::sqrt(sum / double(n) );
	}
	apxnorm /= double(n);
	for(i = 0; i < n*n; i++)
		Q[i] = Q[i] / apxnorm;

	// maximum number of iterations
	size_t kMax = 20;

	// convergence criteria
	double eIn = 1e-10;

	// level of tracing
	size_t level = 0;

	// iteration counter
	size_t k = 0;

	double eOut;
	Msg = QuadBox(cout, kMax, level, n, eIn, Q, r, l, u, k, x, a, b, eOut);
	if( eOut > eIn  )
		Msg = " :QuadBox could not obtain desired accuracy";
	if( Msg == "ok" )
	{	std::ostringstream buf;
		buf << " :QuadBox Iteration Count = " << k;
		Msg = buf.str();
	}
	else	ok = false;

	for(i = 0; i < n; i++)
	{	Qxi = 0;
		for(j = 0; j < n; j++)
			Qxi += Q[i * n + j] * x[j];

		ok &= l[i] < x[i];
		ok &= x[i] < u[i];
		ok &= 0. < a[i]; 
		ok &= 0. < b[i]; 

		// account for possible round off error 
		ok &= fabs( Qxi + r[i] - a[i] + b[i] ) <= 10  * eOut;
		ok &= a[i] * (x[i] - l[i])             <= 10. * eOut;
		ok &= b[i] * (u[i] - x[i])             <= 10. * eOut;
	}

	return ok;
}
/* $$
$end
-------------------------------------------------------------------------------
*/

namespace QN01Box {
const char * QuadBox(
	// Input Arguments
	std::ostream &os,
	size_t        kMax,
	size_t        level,
	size_t        n,
	double      eIn,
	const double *Q, // length n * n
	const double *r, // length n 
	const double *l, // ...
	const double *u, // ...
	// Input and Output Arguments
	size_t       &k      ,
	double       *x      , // length n
	double       *a      , // ...
	double       *b      ,  // ...
	// Output Arugments
	double    &eOut      )
{
	using namespace std;

	size_t i;

	double alpha, alphak;
	double f,     fk;

	if( n == 0 )
	{	eOut = 0.;
		return (const char *)("ok");
	}

	size_t ntmp = 9 * n;
	Memory<double> dMemory(ntmp);
	double *xk   = dMemory(n);
	double *ak   = dMemory(n);
	double *bk   = dMemory(n);
	double *Fk   = dMemory(3 * n);
	double *F    = dMemory(3 * n);

	if( level >= 1 )
	{	os << "QuadBox:" << endl;
		for(i = 0; i < n; i++)
			os << "i = " << i 
			   << ", r = " << r[i]
			   << ", x = " << x[i]
			   << ", l = " << l[i]
			   << ", u = " << u[i]
			   << endl;
	}

	// check initial limits
	for(i = 0; i < n; i++)
	{	QN01BoxUsageError( l[i] < x[i], "QuadBox", "x not in [l, u]" );
		QN01BoxUsageError( x[i] < u[i], "QuadBox", "x not in [l, u]" );
		QN01BoxUsageError( 0.   < a[i], "QuadBox", "a not >= zero"   );
		QN01BoxUsageError( 0.   < b[i], "QuadBox", "b not >= zero"   );
	}
	
	// initial Residual in complementarity condition
	f    = Residual(n, Q, r, l, u, x, a, b, F);

	// initial alpha
	alpha = .005;

	// iterate until convergence, maximum number of iterations, or an error
	double lambda  = 1.;
	const char * msg = "ok";
	while( MaxAbs(3 * n,  F) > eIn && k < kMax )
	{
		// tracing
		if( level >= 1 )
		{	os << "k = " << k;
			os << ", alpha  = " << alpha;
			os << ", lambda  = " << lambda;
			os << ", f = " << f;
			os << ", Max(Ca) = " << MaxAbs(n, F);
			os << ", Max(Cb) = " << MaxAbs(n, F + n);
			os << ", Max(Lx) = " << MaxAbs(n, F + 2 * n);
			os << endl;
		}
		if( level >= 2 )
		{	for(i = 0; i < n; i++)
				os << "i = " << i 
				   << ", x = " << x[i]
				   << ", a = " << a[i]
				   << ", b = " << b[i]
				   << ", Ca = " << F[i]
				   << ", Cb = " << F[i + n]
				   << ", Lx = " << F[i + 2 * n]
				   << endl;
		}

		// iteration counter
		++k;

		// is the next step a corrector step
		bool corrector = (k % 3 == 0);

		// place new candidate in xk, ak, bk 
		if( corrector )
			alphak = 1.;
		else	alphak = alpha;
		msg = Next(
			n, alphak, Q, r, l, u, x, a, b, xk, ak, bk, lambda);
		if( ! Ok(msg) )
		{	// can not obtain more accuracy 
			msg     = "ok";
			eOut    = MaxAbs(3 * n , F);
			return (const char *) msg;
		}

		// check limits and set same flag
		bool same = true;
		for(i = 0; i < n; i++)
		{	QN01BoxUnknownError( l[i]  < xk[i], "QuadBox" );
			QN01BoxUnknownError( xk[i] < u[i], "QuadBox" );
			QN01BoxUnknownError( 0.    < ak[i], "QuadBox" );
			QN01BoxUnknownError( 0.    < bk[i], "QuadBox" );
			same &= (x[i]==xk[i] && a[i]==ak[i] && b[i]==bk[i]); 
		}
		if( same && (! corrector) )
		{	msg = "Quadbox: cannot achieve requested accuracy";
			if( level >= 1 )
				os << msg << std::endl;
			msg     = "ok";
			eOut = MaxAbs(3 * n , F);
			return (const char *) msg;
		}
		// next Residual relative to central path
		if( same )
		{
			fk = f;
			for(i = 0; i < 3*n; i++)
				Fk[i] = F[i];
		}
		else	fk = Residual(n, Q, r, l, u, xk, ak, bk, Fk);

		// did we get descent in the residual
		bool descent = (1. - 10. * DBL_EPSILON) * f >= fk;

		// update alpha
		if( ! corrector )
		{	alpha =  f / fk;
			alpha =  alpha * alpha;
			if( alpha >= .05 )
				alpha = .05;
			if( alpha < .005 )
				alpha = .005;
		}
		if( (! corrector) && ( ! descent) )
		{	msg = "QuadBox: central path residual not decreasing";
			if( level >= 1 )
			{	os << msg << std::endl 
				   << "lambda = "   << lambda
				   << ", f = "      << f 
				   << ", fk = "     << fk 
				   << ", fk - f = " << fk - f 
				   << std::endl;
			}
			msg     = "ok";
			eOut    = MaxAbs(3 * n , F);
			return (const char *) msg;
		}
		if( descent )
		{	for(i = 0; i < n; i++)
			{	x[i]         = xk[i];
				a[i]         = ak[i];
				b[i]         = bk[i];
				F[i]         = Fk[i];
				F[i + n]     = Fk[i + n];
				F[i + 2 * n] = Fk[i + 2 * n];
			}
			f = fk;
		}
	}
	eOut = MaxAbs(3 * n, F);

	// return with msg = "ok" and actual accuacy achieved.
	return msg;
}
} // End QN01Box namespace
