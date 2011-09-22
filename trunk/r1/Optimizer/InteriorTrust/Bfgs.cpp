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

$begin Bfgs$$
$spell
	dsyev
	cstddef
	iostream
	cassert
	bool
	namespace
	std
	apxnorm
	sqrt
	Hs
	ys
	const
	Broyden
	Goldfarb
	Shanno
	Bfgs
	xk
	gk
	cp
	gp
	xp
	strcmp
	Hdxi
	fabs
$$

$section The Broyden-Fletcher-Goldfarb-Shanno Update$$

$table
$bold Syntax$$ $cnext
$syntax%const char * Bfgs(
	// Input Arguments
	size_t       %n%,
	double %epsilon%,
	double     *%xk%,  // length n
	double     *%gk%,  // ...
	double     *%xp%,  // ...
	double     *%gp%,  // ...
	// Input and Output Arguments
	double      *%H%   // length n * n
)%$$
$tend

$fend 20$$

$head Exceptions$$
The routine $code Bfgs$$ is $xref/glossary/Exception Safe/exception safe/$$.

$head Input Argument$$
Let the positive integer
$italic n$$,
the positive value	
$latex \varepsilon$$, 
$latex x_k \in \R^n$$, 
$latex g_k \in \R^n$$,
$latex x_p \in \R^n$$,
$latex g_p \in \R^n$$,
be the value of the corresponding input arguments.

$head H$$
The matrix $italic H$$ is both an input and an output argument.
We use $latex H_k \in \R^{n \times n}$$ 
to denote the input value of $italic H$$ and
$latex H_p \in \R^{n \times n}$$ to denote its output value.
We define the values 
$latex s \in \R^n$$,
$latex y \in \R^n$$
by
$latex \[
\begin{array}{rcl}
	s   & = & x_p - x_k \\
	y   & = & g_p - g_k 
\end{array}
\] $$
If the following conditions hold:
$latex \[
\begin{array}{rcr}
	| s |     & \geq &                              \varepsilon  \\
	s^T H_k s & \geq & | H_k |_\infty \; | s |^2 \; \varepsilon  \\
	y^T s     & \geq &              | y | \; | s | \; \varepsilon 
\end{array}
\] $$
the output value of $italic H$$ is given by
$latex \[
H_p  =  H_k 
     - \frac{ ( H_k s )( H_k s)^T }{s^T H_k s} 
     + \frac{ y y^T }{ y^T s }
\] $$
Otherwise the output value of $italic H$$ is given by
$latex H_p = H_k$$.

$subhead Condition Number$$
It is ensured that the condition number of the output value
of $latex H$$ is less
than or equal $latex \varepsilon^{-1}$$.
If necessary, a matrix $latex E$$ with 
$latex | E |_2 \leq \varepsilon | H |_2$$ is added
to the value of $latex Hp$$ defined above to ensure this condition.

$head Return Value$$
The return value of $code Bfgs$$ is
as follows:
$center
$table
	$cnext $pre  $$ $cnext $bold Condition$$
	$cnext $pre  $$ $cnext $bold Return Value$$
$rnext
if 
	$cnext $cnext $latex | s |  < \varepsilon$$  
	$cnext $cnext "|s| small" 
$rnext
else if 
	$cnext $cnext 
	$latex s^T H_k s < | H_k |_\infty \; | s |^2 \; \varepsilon $$
	$cnext $cnext "ss small" 
$rnext
else if 
	$cnext $cnext 
	$latex y^T s < | y | \; | s | \; \varepsilon $$
	$cnext $cnext "ys small" 
$rnext
else 
	$cnext $cnext 
	$cnext $cnext "ok" 
$tend
$$

$head Remark$$
If $latex H_k$$ is positive definite, 
$latex H_p$$ is also positive definite.

$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$codep */

# include "Bfgs.h"
# include "Memory.h"
# include "c2dsyev.h"

# include <cstddef>
# include <iostream>
# include <cassert>
# include <string>
# include <math.h>

bool BfgsTest(std::string &msg)
{	bool ok = true;

	using namespace std;

	size_t   i1;
	size_t   i2;
	size_t    i;
	size_t    j;
	double  sHs;

	size_t n   = 2;

	Memory<double> dMemory(2 * n * n + 5 * n); 
	double *A  = dMemory(n * n);
	double *H  = dMemory(n * n);
	double *xk = dMemory(n);
	double *gk = dMemory(n);
	double *xp = dMemory(n);
	double *gp = dMemory(n);
	double *Hs = dMemory(n);

	// A is a random n x m matrix
	for(j = 0; j < n * n; j++)
		A[j] = rand() / (double) RAND_MAX - .5;

	// xk, gk, xp, gp are random vectors
	double ys_sum = 0;
	for(i = 0; i < n; i++)
	{	xk[i] = rand() / (double) RAND_MAX - .5;
		gk[i] = rand() / (double) RAND_MAX - .5;
		xp[i] = rand() / (double) RAND_MAX - .5;
		gk[i] = rand() / (double) RAND_MAX - .5;

		if( 0 < i )
			ys_sum += (xp[i] - xk[i]) * (gp[i] - gk[i]);
	}

	// H = A * A^T
	double sum     = 0.;
	double apxnorm = 0.;
	for(i1 = 0; i1 < n; i1++)
	{	for(i2 = 0; i2 < n; i2++)
		{	H[i1 * n + i2] = 0.;
			for(j = 0; j < n; j++)
				H[i1 * n + i2] += A[i1 * n + j] * A[i2 * n + j];
			sum += H[i1 * n + i2] * H[i1 * n + i2];
		}
		apxnorm += sqrt(sum / double(n) );
	}
	apxnorm /= double(n);
	for(i = 0; i < n * n; i++)
		H[i] = H[i] / apxnorm;

	double epsilon  = 1e-10;
	
	// case where s = 0
	msg = Bfgs(n, epsilon, xk, gk, xk, gp, H);
	ok &= (msg == "|s| small");
      	
	// case where ys = 0
	gp[0] = gk[0] - ys_sum / (xp[0] - xk[0]);
	msg = Bfgs(n, epsilon, xk, gk, xp, gp, H);
	ok &= (msg == "ys small");
	gp[0] = rand() / (double) RAND_MAX - .5;

	// normal case
	msg = Bfgs(n, epsilon, xk, gk, xp, gp, H);
	ok &= (msg == "ok");

	// check update equation
	sHs = 0.;
	for(i = 0; i < n; i++)
	{	Hs[i] = 0;
		for(j = 0; j < n; j++)
			Hs[i] += H[i * n + j] * (xp[j] - xk[j]);

		sHs += (xp[i] - xk[i]) * Hs[i];
		ok &= fabs( Hs[i] - gp[i] + gk[i]  ) <= epsilon;
	}

	// construct a case where H * s is zero
	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
			H[i * n + j] -= Hs[i] * Hs[j] / sHs;
	}

	// case where sHs = 0 (up to numerical precision)
	msg = Bfgs(n, epsilon, xk, gk, xp, gp, H);
	ok &= (msg == "sHs small");

	msg = "";

	return ok;
}
/* $$
$end
-------------------------------------------------------------------------------
*/

# include "MaxAbs.h"
# include "CppADUtil.h"
# include <cstring>

# define BfgsTrace 1

const char * Bfgs(
	// Input Arguments
	size_t       n,
	double epsilon,
	double     *xk,  // length n
	double     *gk,  // ...
	double     *xp,  // ...
	double     *gp,  // ...
	// Input and Output Arguments
	double      *H   // length n * n
)
{	size_t i;
	size_t j;
	size_t k;

	assert( epsilon < 1. );

	Memory<double> dMemory(1 + 7 * n + n * n);
	double *s     = dMemory(n);
	double *y     = dMemory(n);
	double *Hs    = dMemory(n);
	double *W     = dMemory(n);
	double *WORK  = dMemory(3 * n + 1);
	double *A     = dMemory(n * n);

	// compute s, y and their Euclidean norms
	double snorm = 0.;
	double ynorm = 0.;
	for(i = 0; i < n; i++)
	{	s[i]   = xp[i] - xk[i];
		y[i]   = gp[i] - gk[i];

		snorm += s[i] * s[i];
		ynorm += y[i] * y[i];
	}
	snorm = sqrt( snorm );
	ynorm = sqrt( ynorm );

	// normalize s and y so that the vectors s and y in program below 
	// are s / |s| and y / |y| in documentation above.
	if( snorm > 0. )
		for(i = 0; i < n; i++)
			s[i] /= snorm;
	if( ynorm > 0. )
		for(i = 0; i < n; i++)
			y[i] /= ynorm;

	double Hinfinity = MaxAbs(n * n , H);
	double sOne      = 0.;
	double ys        = 0.;
	double sHs       = 0.;
	for(i = 0; i < n; i++)
	{	sOne += fabs( s[i] );
		ys   += y[i] * s[i];
		Hs[i]  = 0.;
		for(j = 0; j < n; j++)
			Hs[i] += H[i * n + j] * s[j];
		sHs  += s[i] * Hs[i];
	}

	if( snorm  < epsilon )
		return (const char *)("|s| small");

	if( sHs < epsilon * Hinfinity )
		return (const char *)("sHs small");

	if(  ys < epsilon )
		return (const char *)("ys small");	
	assert( ynorm != 0. );

	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
			H[i * n +j ] -= Hs[i] * Hs[j] / sHs;
	}

	double ratio = ynorm / snorm;
	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
			H[i * n +j ] +=  ratio * y[i] *  y[j] / ys;
	}

	// C link to Fortran symmetric eigenvector routine
	// uses ascii codes in place of character arguments
	int IJOB   = int( 'V' ); // compute eigenvalues and eigen vectors
	int IUPLO  = int( 'U' ); // the upper triangle of A is stored 
	int N      = n;          // order of the matrix A
	int LDA    = n;          // column major leading dimension of A 
	int LWORK  = 3 * n + 1;  // length of the work vector
	int INFO;                // return flag

	// compute eigen vectors in accending order
	for(i = 0; i < n; i++)
		for(j = 0; j < n; j++)
			A[i + j * n] = H[i * n + j];
	c2dsyev_(&IJOB, &IUPLO, &N, A, &LDA, W, WORK, &LWORK, &INFO);
	assert( INFO >= 0 );
	if( INFO > 0 ) 
		return (const char *) 
			("cannot ensure condition number condition");

	for(k = 0; k < n-1; k++)
	{	// check eignevector ( A[0+k*n], A[1+k*n], ... , A[n-1+k*n] )
		assert( W[k] <= W[n-1] );
		double diff =  epsilon * W[n-1] - W[k];
		if( diff > 0. )
		{	for(i = 0; i < n; i++)
			{	for(j = 0; j < n; j++)
					H[i * n + j] += diff * 
						A[i + k * n] * A[j + k * n];
			}
		}
	}

	return (const char *) ("ok");
}
