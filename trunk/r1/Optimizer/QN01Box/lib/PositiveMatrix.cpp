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

$begin PositiveMatrix$$
$spell
	Hin
	Hout
	dsyev
	bool
	std
	namespace
	Hx
	eps
	eigen
	cstddef
$$

$section Ensure that A Symmetric Matrix is Positive Definite$$

$table
$bold Syntax$$ $cnext
$syntax%void PositiveMatrix(
	// Input Arguments
	size_t       %n%,
	double     %eps%,
	// Input and Output Arguments
	double      *%H%   // length n * n
)%$$
$tend

$fend 20$$

$head Exceptions$$
The routine $code PositiveMatrix$$ uses
the QN01Box $xref/Error//error handler/$$ to report errors.

$head n$$
The argument $italic n$$ specifies 
the number row and column dimension of the matrix $italic H$$.

$head  eps$$
The argument $italic eps$$ is greater than zero and 
less than one.
It specifies
the minimum value for the ratio of the maximum eigen value
divided by the minimum eigen value for the output value of 
the matrix $italic H$$.

$head H$$
We use $italic Hin$$ and $italic Hout$$ for the input
and output value of $italic H$$.
The reciprocal condition number of $italic Hin$$ is the ratio of its
minimum eigen value divided by its maximum eigenvalue.
If this ratio is greater than or equal $italic eps$$,
$italic Hout$$ is equal to $italic Hin$$.
Otherwise, $italic Hout$$ is a matrix that has
its reciprocal condition number greater than or equal to 
$syntax%%eps% / 2%$$ 
and such that if $latex Hin * x = y$$ then 
$latex Hout * ( x + \Delta x) = y$$ where $latex \Delta x$$ 
is "small" (under the condition number constraint). 

$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$codep */

# include <QN01Box/PositiveMatrix.h>

# include <cstddef>
# include <string>
# include <math.h>
# include <QN01Box/Memory.h>
# include <QN01Box/c2dsyev.h>

bool PositiveMatrixTest(std::string &msg)
{	bool ok = true;
	using namespace QN01Box;

	using namespace std;

	size_t    i;
	size_t    j;
	size_t    k;

	size_t n   = 2;
	double eps = 1e-4;
	double H[] = {
		1., 0.,
		0., 0.  
	};
	PositiveMatrix(n, eps, H);
	double x[2];
	double sum_xHx   = 0.;
	double sum_xx    = 0.;
	double min_ratio = 100.;
	double max_ratio = -100.;
	for(k = 0; k < 100; k++)
	{	for(i = 0; i < n; i++)
			x[i] = rand() / (double) RAND_MAX - .5;
		for(i = 0; i < n; i++)
		{	sum_xx += x[i] * x[i];
			for(j = 0; j < n; j++)
				sum_xHx += x[i] * H[ i * n + j] * x[j];
		}
		if( min_ratio > sum_xHx / sum_xx )
			min_ratio = sum_xHx / sum_xx;
		if( max_ratio < sum_xHx / sum_xx )
			max_ratio = sum_xHx / sum_xx;
	}
	ok &= min_ratio >= eps * max_ratio;
	msg = "";

	return ok;
}
/* $$
$end
-------------------------------------------------------------------------------
*/

# include <QN01Box/MaxAbs.h>
# include <cstring>
# include <QN01Box/PositiveMatrix.h>


namespace QN01Box {
void PositiveMatrix(
	// Input Arguments
	size_t       n,
	double     eps,
	// Input and Output Arguments
	double      *H   // length n * n
)
{	size_t i;
	size_t j;
	size_t k;

	QN01BoxUsageError(eps < 1. , "PositiveMatrix", "1 <= eps" );
	QN01BoxUsageError(0. < eps , "PositiveMatrix", "eps <= 0" );

	Memory<double> dMemory(1 + 4 * n + n * n);
	double *W     = dMemory(n);
	double *WORK  = dMemory(3 * n + 1);
	double *A     = dMemory(n * n);

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
	QN01BoxUsageError( 
		INFO >= 0, "PositiveMatrix", "eigen-vector routine failed"
	);

	// check for case where we need to add a multiple of the identity
	if( W[0] < eps * W[n-1] )
	{	// multiple of identity
		double lambda = ( eps * W[n-1] - W[0] ) / ( 1. - eps );
		// add multiple of identity to H
		for(i = 0; i < n; i++)
			H[ i * n + i ] += lambda;
	}

# ifndef NDEBUG
	// compute eigen vectors in accending order
	for(i = 0; i < n; i++)
		for(j = 0; j < n; j++)
			A[i + j * n] = H[i * n + j];
	c2dsyev_(&IJOB, &IUPLO, &N, A, &LDA, W, WORK, &LWORK, &INFO);
	QN01BoxUsageError( 
		INFO >= 0, "PositiveMatrix", "eigen-vector routine failed"
	);
	QN01BoxUnknownError( W[n-1] > 0. , "PositiveMatrix" );
	for(k = 0; k < n; k++) QN01BoxUnknownError( 
		W[k] >= eps * W[n-1] / 2. , "PositiveMatrix"
	);
# endif

	return;
}
} // End QN01Box namespace
