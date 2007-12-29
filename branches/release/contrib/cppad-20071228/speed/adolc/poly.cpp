/* --------------------------------------------------------------------------
CppAD: C++ Algorithmic Differentiation: Copyright (C) 2003-07 Bradley M. Bell

CppAD is distributed under multiple licenses. This distribution is under
the terms of the 
                    Common Public License Version 1.0.

A copy of this license is included in the COPYING file of this distribution.
Please visit http://www.coin-or.org/CppAD/ for information on other licenses.
-------------------------------------------------------------------------- */
/*
$begin adolc_poly.cpp$$
$spell
	cppad
	hos
	Taylor
	std
	ddp
	cppad
	adouble
	std
	vector Vector
	typedef
	adolc
	Lu
	CppAD
	det
	hpp
	const
	bool
$$

$section Adolc Speed: Second Derivative of a Polynomial$$

$index adolc, speed polynomial$$
$index speed, adolc polynomial$$
$index polynomial, speed adolc$$

$head Operation Sequence$$
Note that the polynomial evaluation
$cref/operation sequence/glossary/Operation/Sequence/$$
does not depend on the argument to the polynomial.
Hence we use the same $cref/ADFun/$$ object for all the argument values.

$head compute_poly$$
$index compute_poly$$
Routine that computes the second derivative of a polynomial using Adolc:
$codep */
# include <vector>

# include <cppad/speed/uniform_01.hpp>
# include <cppad/poly.hpp>
# include <cppad/vector.hpp>

# include <adolc/adouble.h>
# include <adolc/interfaces.h>

void compute_poly(
	size_t                     size     , 
	size_t                     repeat   , 
	CppAD::vector<double>     &a        ,  // coefficients of polynomial
	CppAD::vector<double>     &z        ,  // polynomial argument value
	CppAD::vector<double>     &ddp      )  // second derivative w.r.t z  
{
	// -----------------------------------------------------
	// setup
	int tag  = 0;  // tape identifier
	int keep = 1;  // keep forward mode results in buffer
	int m    = 1;  // number of dependent variables
	int n    = 1;  // number of independent variables
	int d    = 2;  // order of the derivative
	double f;      // function value
	int i;         // temporary index

	// choose a vector of polynomial coefficients
	CppAD::uniform_01(size, a);

	// AD copy of the polynomial coefficients
	std::vector<adouble> A(size);
	for(i = 0; i < int(size); i++)
		A[i] = a[i];

	// domain and range space AD values
	adouble Z, P;

	// choose an argument value
	CppAD::uniform_01(1, z);

	// declare independent variables
	trace_on(tag, keep);
	Z <<= z[0]; 

	// AD computation of the function value 
	P = CppAD::Poly(0, A, Z);

	// create function object f : Z -> P
	P >>= f;
	trace_off();

	// allocate arguments to hos_forward
	double *x0 = new double  [n];
	double *y0 = new double  [m];
	double **x = new double* [n];
	double **y = new double* [m];
	for(i = 0; i < n; i++)
		x[i] = new double [d];
	for(i = 0; i < m; i++)
		y[i] = new double [d];

	// Taylor coefficient for argument
	x[0][0] = 1.;  // first order
	x[0][1] = 0.;  // second order
	
	// ------------------------------------------------------
	while(repeat--)
	{	// get the next argument value
		CppAD::uniform_01(1, z);
		x0[0] = z[0];

		// evaluate the polynomial at the new argument value
		hos_forward(tag, m, n, d, keep, x0, x, y0, y);

		// second derivative is twice second order Taylor coefficient
		ddp[0] = 2. * y[0][1];
	}
	// ------------------------------------------------------
	// tear down
	delete [] x0;
	delete [] y0;
	for(i = 0; i < n; i++)
		delete [] x[i];
	for(i = 0; i < m; i++)
		delete y[i];
	delete [] x;
	delete [] y;

	return;
}
/* $$
$end
*/
