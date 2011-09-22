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

$begin  Next$$
$spell
	fs
	ftmp
	Util
	std
	Dvector
	Lu
	cmath
	cstddef
	Rp Rp
	Rhs
	logdet
	Cpp
	complementarity
	const
	stddef
	bool
	da
$$

$mindex Next interior point iterate$$
$section Next Interior Point Iterate$$

$table
$bold Syntax$$ $cnext
$syntax%
const char * Next(
	// Input Arguments
	size_t %n%,
	double %alpha%,
	const double *%Q%,  // length n * n
	const double *%r%,  // length n
	const double *%l%,  // ...
	const double *%u%,  // ...
	const double *%x%,  // ...
	const double *%a%,  // ...
	const double *%b%,  // ...
	// Output Arguments
	double *%xOut%,     // length n 
	double *%aOut%,     // ...
	double *%bOut%,     // ...
	double &%lambda% )
%$$
$tend

$fend 20$$

$head Exceptions$$
The routine $code Next$$ is $xref/glossary/Exception Safe/exception safe/$$.

$head Return Value$$
If the return value of $code Next$$ is either the (const char *) "ok",
it succeeded in meeting its specifications.
Otherwise, the return value is an error message and the specifications
have not been met.
Provided that all the input and output arguments have their specified lengths,
such a failure should not have any side effects.

$head Input Arguments$$
We define 
$latex n \in \Z_+$$,
$latex \alpha \in \R_+$$,
$latex Q \in \R^{n \times n}$$,
$latex r \in \R^n$$,
$latex l \in \R^n$$,
$latex u \in \R^n$$,
$latex x \in \R^n$$,
$latex a \in \R_+^n$$,
$latex b \in \R_+^n$$,
$latex da \in \R^n$$,
$latex db \in \R^n$$
as the values of the corresponding input arguments.
The matrix $latex Q$$ must be positive definite
$latex 0 < \alpha$$ 
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

$head Newton Step$$
The values 
$latex \Delta x \in \R^n$$, 
$latex \Delta a \in \R^n$$,
and
$latex \Delta b \in \R^n$$
are defined by:
$latex \[
R( x , a , b ) + 
R^{(1)} (x, a, b)
\left( \begin{array}{c}
	\Delta x \\
	\Delta a \\
	\Delta b
\end{array} \right)
=
\alpha
\left( \begin{array}{c}
avg [ C_u ( x , a , b ) ] e
\\
avg [ C_l ( x , a , b ) ] e
\\
0
\end{array} \right)
\]$$
where $latex R^{(1)}$$ is the derivative of $latex R$$. 

$head Output Arguments$$
We define $latex \beta \in \R$$ as the 
maximum value, greater than zero and less than or equal one,
such that the following conditions hold:
$latex \[
\begin{array}{rclc}
x_i + \beta \Delta x_i - l_i  & \geq & ( x_i - l_i ) / 1000 \\
u_i - x_i - \beta \Delta x_i  & \geq & ( u_i - x_i ) / 1000 \\
a_i + \beta \Delta a_i        & \geq & a_i / 1000     \\
b_i + \beta \Delta b_i        & \geq & b_i / 1000
\end{array}
\] $$
The output values are defined by
$latex \[
\begin{array}{rclc}
xOut & = &  x + \lambda \Delta x \\
aOut & = &  a + \lambda \Delta a \\
bOut & = &  b + \lambda \Delta b 
\end{array}
\] $$
where $latex lambda = \beta / 2^i$$ and $latex i$$ is the smallest
non-negative integer such that the descent criteria below holds.
$pre

$$
If $italic msg$$ is "ok",
$latex xOut = x$$, $latex aOut = a$$ and $latex bOut = b$$,
then an more accurate answer cannot be obtained 
(due to numerical round off error).

$head Descent Criteria$$
We define the function $latex f : \R_+ \rightarrow \R_+$$ by:
$latex \[
\begin{array}{rcl}
f( \theta ) 
& = &
| \;
	R ( x + \theta  \Delta x , a + \theta \Delta a , b + \theta \Delta b )
\; |_2
\\
& = &
| \;
	R ( x , a , b ) 
	+
	\theta
	R^{(1)} ( x , a , b  ) 
	( \Delta x^T , \Delta a^T ,  \Delta b^T  )^T
\; |_2
+
O( \theta^2 )
\\
& = &
| \;
	(1 - \theta ) R ( x , a , b ) 
	+ \theta \alpha  \left( 
		avg [ C_u ( x , a , b ) ] e^T,
		avg [ C_l ( x , a , b ) ] e^T,
		0
	\right)
\; |_2
+
O( \theta^2 )
\\
& \leq &
[ 1 - \theta (1 - \alpha) ] f( 0 ) + O( \theta^2 )
\end{array}
\] $$
It follows that
$latex \[
\frac{d}{d \theta} f ( 0 ) \leq  - f( 0 ) (1 - \alpha)
\] $$
The value $latex \lambda$$ is chosen so that 
$latex \[
	f ( \lambda ) \leq f(0) - \lambda \frac{1}{2} f (0) (1 - \alpha )
\] $$

$head Corrector Steps$$
If $latex \alpha = 1$$, the current step is a corrector step.
In this case $latex \lambda$$ is one if $latex f(1) \leq f(0)$$
and zero otherwise.
In addition, if $latex \alpha = 1$$, $code Next$$ always returns "ok".

$head Staying Interior$$
If $latex x_i$$ is with in 
$latex \[
	100 * MachineEpsilon *  (u_i - l_i )
\] $$
of the boundary, it is pushed back to that distance from the boundary.
This is done on a component by component basis. 
$pre

$$
If $latex a_i$$ or $latex b_i$$ is with in 
$latex \[
	100 * MachineEpsilon *  ( | Q |_\infty + | r |_\infty )
\] $$
of zero, it is pushed back to that minimum value.
This is done on a component by component basis. 

$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$codep */

# include "Memory.h"
# include "Next.h"
# include "Delta.h"
# include "Residual.h"
# include "min.h"
# include "max.h"
# include "NearEqual.h"
# include "MaxAbs.h"

// CppAD utilities used by QuasiNewton01Box
# include "CppADUtil.h"

# include <float.h>
# include <cstddef>
# include <cmath>
# include <string>

# define StepBound .999

bool NextTest(std::string &msg)
{	bool         ok    = true;
	const int    n     = 1;
	double       alpha = .2;
	const double Q[]   = {1.};
	const double r[]   = {5.};
	const double l[]   = {-.5};
	const double u[]   = {1.};
	const double x[]   = {0.};
	double       a[]   = {1.};
	double       b[]   = {2.};
	double  xOut[1];
	double  aOut[1];
	double  bOut[1];
	double  ftmp[3];
	size_t       i;

	// derivative of R, right hand side, and solution
	// use CppADvector because LuSolve expects it
	CppADvector<double> Rp(9);
	CppADvector<double> Rhs(3);
	CppADvector<double> Result(3);

	// first row of Rp
	Rp[0]  = - *b;
	Rp[1]  = 0.;
	Rp[2]  = *u - *x;
	Rhs[0] = (alpha - 1)*(*u - *x)*(*b); 

	// second row of Rp
	Rp[3]  = *a;
	Rp[4]  = *x - *l;
	Rp[5]  = 0.;
	Rhs[1] = (alpha - 1)*(*x - *l)*(*a); 

	// third row of Rp
	Rp[6]  = *Q;
	Rp[7]  = -1.;
	Rp[8]  = 1.;
	Rhs[2] = - (*Q)*(*x) - *r + *a - *b;

	// solve to Delta x, Delta a, and Delta b
	double logdet;
	double sign = CppAD::LuSolve(3, 1, Rp, Rhs, Result, logdet);

	// determine step size
	double lambda = 1.;
	if( lambda * Result[0] > StepBound * (*u - *x) )
		lambda = StepBound * (*u - *x) / Result[0];
	if( lambda * Result[0] < StepBound * (*l - *x) )
		lambda = StepBound * (*l - *x) / Result[0];
	if( lambda * Result[1] < - StepBound * (*a) )
		lambda = - StepBound * (*a) / Result[1];
	if( lambda * Result[2] < - StepBound * (*b) )
		lambda = - StepBound * (*b) / Result[2];

      	double f0 = Residual(n, Q, r, l, u, x, a, b, ftmp);
	double fs = f0;
	lambda = 2. * lambda;
	while( fs >=  f0 * ( 1. - .5 * lambda*(1 - alpha) ) )
	{	lambda = lambda / 2.;
		*xOut = *x + lambda * Result[0];	
		*aOut = *a + lambda * Result[1];	
		*bOut = *b + lambda * Result[2];	
      		fs = Residual(n, Q, r, l, u, xOut, aOut, bOut, ftmp);
	}
	Result[0] = *xOut;
	Result[1] = *aOut;
	Result[2] = *bOut;

	double lamOut;
	msg = Next(n, alpha, Q, r, l, u, x, a, b, 
		xOut, aOut, bOut, lamOut);
	if( msg == "ok" )
		msg = "";
	else	ok = false;

	ok &= NearEqual(lambda,   lamOut, 1e-12, 1e-12);
	ok &= NearEqual(Result[0], *xOut, 1e-12, 1e-12);
	ok &= NearEqual(Result[1], *aOut, 1e-12, 1e-12);
	ok &= NearEqual(Result[2], *bOut, 1e-12, 1e-12);

	return ok;
}
/* $$
$end
------------------------------------------------------------------------------
*/

static bool NearZero(
	size_t n, 
	const double *l, 
	const double *u, 
	const double *x, 
	const double *a, 
	const double *b
)
{	size_t i;
	double dmax = 0.;
	for(i = 0; i < n; i++)
	{
		dmax = max( dmax, fabs( x[i] / (u[i] - l[i]) ) );
		dmax = max( dmax, fabs( a[i] / (u[i] - l[i]) ) / DBL_EPSILON );
		dmax = max( dmax, fabs( b[i] / (u[i] - l[i]) ) / DBL_EPSILON );
	}
	return dmax <= 1e+2 * DBL_EPSILON;
}


const char * Next(
	size_t n,
	double alpha,
	const double *Q,  // length n * n
	const double *r,  // length n
	const double *l,  // ...
	const double *u,  // ...
	const double *x,  // ...
	const double *a,  // ...
	const double *b,  // ...
	double *xOut,     // ... 
	double *aOut,     // ...
	double *bOut,     // ...
	double &lambda ) 
{	
	size_t         i;
	const char * mOut;

	// split out temporary memory
	Memory<double> dMemory(7 * n);
	double *ftmp  = dMemory(3 * n);
	double *rhs   = dMemory(n);
	double *dx    = dMemory(n);
	double *da    = dMemory(n);
	double *db    = dMemory(n);

	// solve for the Newton step (dx, da, db)
	mOut = Delta(n, alpha, Q, r, l, u, x, a, b, dx, da, db);
	
	// check for zero step case
	bool nearZero = NearZero(n, l, u, dx, da, db);
	if( nearZero )
	{	for(i = 0; i < n; i++)
		{	dx[i] = 0.;
			da[i] = 0.;
			db[i] = 0.;
		}
	}

	// determine bound on step in this direction
	lambda = 1.;
	double ratio;
	for( i = 0; i < n; i++)
	{	if( dx[i] > 0. )
		{	ratio = (u[i] - x[i]) / dx[i];
			lambda = min(lambda, StepBound * ratio );
		}
		if( dx[i] < 0. )
		{	ratio = (l[i] - x[i]) / dx[i];
			lambda = min(lambda,  StepBound * ratio );
		}
		if( da[i] < 0. )
		{	ratio = - a[i] / da[i];
			lambda = min(lambda, StepBound * ratio );
		}
		if( db[i] < 0. )
		{	ratio = - b[i] / db[i];
			lambda = min(lambda, StepBound * ratio );
		}
	}
      	double f0 = Residual(n, Q, r, l, u, x, a, b, ftmp);

	// make sure get descent in residual
	bool ok  = false;
	double fs = 0.;
	while( ! ok )
	{	for(i = 0; i < n; i++)
		{	xOut[i] = x[i] + lambda * dx[i];
			aOut[i] = a[i] + lambda * da[i];
			bOut[i] = b[i] + lambda * db[i];

			assert( l[i] < xOut[i] );
			assert( u[i] > xOut[i] );
			assert( 0.   < aOut[i] );
			assert( 0.   < bOut[i] );
		}
		fs = Residual(n, Q, r, l, u, xOut, aOut, bOut, ftmp);

		bool small = fabs(fs - f0) <= 
			1e1 * DBL_EPSILON * (fabs(f0) + fabs(fs));
		assert( (! nearZero) || small );
		if( small )
		{	nearZero = true;
			for(i = 0; i < n; i++)
			{	xOut[i] = x[i];
				aOut[i] = a[i];
				bOut[i] = b[i];
			}
			ok = true;
		}
		else if( alpha == 1 )
		{	ok = fs < f0;
			if( ! ok )
			{	for(i = 0; i < n; i++)
				{	xOut[i] = x[i];
					aOut[i] = a[i];
					bOut[i] = b[i];
				}
				lambda = 0.;
				return (const char *) ("ok");
			}
		}
		else	ok =  fs < f0 * ( 1. - .5 * lambda*(1. - alpha) );
		if( ! ok )
		{	if( lambda > 1e-3 )
			{	lambda = lambda / 2.;
			}
			else if( alpha != 1 )
			{	return (const char *)
				("Next: norm of residual not descending");
			}
		}
	}

	double Qnorm = MaxAbs(n * n, Q);
	double rnorm = MaxAbs(n, r);
	for(i = 0; i < n; i++)
	{
		double x_small  = 1e+2 * DBL_EPSILON * (u[i] - l[i]);
		double a_small  = DBL_EPSILON * DBL_EPSILON * (u[i] - l[i]);
		double b_small  = DBL_EPSILON * DBL_EPSILON * (u[i] - l[i]);

		if( xOut[i] <= l[i] + x_small )
			xOut[i] = l[i] + x_small;

		if( xOut[i] >= u[i] - x_small )
			xOut[i] = u[i] - x_small;

		if( aOut[i] < a_small )
			aOut[i] = a_small;
		if( bOut[i] < b_small )
			bOut[i] = b_small;
	}

	return (const char *)("ok");
}
