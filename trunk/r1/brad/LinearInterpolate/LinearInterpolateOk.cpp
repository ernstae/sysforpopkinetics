/*
$begin LinearInterpolateOk$$
$latex \newcommand{\Bd}{ {\bf d} }$$
$spell
	Runge
	const
	iostream
	CppAD
	bool
	std
	cout
	endl
	xdata
	ydata
	ti
	tf
	zi
	zf
$$

$section Example using LinearInterpolate$$

$head Differential Equation$$
Suppose that we wish to solve the following differential equation:
$latex \[
\begin{array}{rcl}
	z(-2)  & = & 0 \\
	z'(t)  & = & 2 |t|
\end{array}
\] $$
for values of $latex t \in [-2, +2]$$
value greater than zero and less than 10.

$head Analytic Solution$$
For $latex t \leq 0$$, 
$latex \[
z(t)  =  
\left\{ \begin{array}{ll}
	2 - t^2/2   & {\rm if} \; t \leq 0 \\
	2 + t^2/2   & {\rm otherwise}
\end{array} \right.
\] $$
	
$codep */

# include <iostream>
# include "LinearInterpolate.h"
# include <CppAD/NearEqual.h>
# include <CppAD/Runge45.h>
# include <vector>

class Fun {
	const std::vector<double> xdata;
	const std::vector<double> ydata;
public:
	Fun(const std::vector<double> &xdata_, const std::vector<double> &ydata_)
	: xdata(xdata_), ydata(ydata_)
	{ }
	void Ode(double t, const std::vector<double> &z, std::vector<double> &f)
	{	f[0] = LinearInterpolate(t, xdata, ydata); }
};

double Z(double t)
{	double z;	
	if( t <= 0. )
		z = 2. - t * t / 2.; 
	else	z = 2. + t * t / 2.; 
	return z;
}

bool LinearInterpolateOk(void)
{	bool   ok = true;
	size_t  n = 3;
	using std::vector;

	// data that defines the absolute value function
	vector<double> xdata(n), ydata(n);
	xdata[0] = -2.; ydata[0] = 2.;
	xdata[1] =  0.; ydata[1] = 0.;
	xdata[2] =  2.; ydata[2] = 2.;

	// Construct Ode function with copy of data
	Fun F(xdata, ydata);

	// Try integration in one step: [-2,+2]
	// This will result in an error estimate > 1e-2
	vector<double> zi(1), zf(1), e(1);
	size_t M  = 1;
	double ti = xdata[0]; 
	double tf = xdata[2];
	zi[0]     = Z(ti);
	zf        = CppAD::Runge45(F, M, ti, tf, zi, e);
	ok       &= (e[0] > 1e-2);  
	ok       &= ! CppAD::NearEqual(Z(tf), zf[0], 0., 1e-2);

	// Break integration into two steps: First step [-2,0]
	// First step has an error estimate < 1e-10
	M     = 1;
	ti    = xdata[0]; 
	tf    = xdata[1];
	zi[0] = Z(ti);
	zf    = CppAD::Runge45(F, M, ti, tf, zi, e);
	ok   &= (e[0] <= 1e-10);  
	ok   &= CppAD::NearEqual(Z(tf), zf[0], 0., 1e-10);

	// Break integration into two steps: Second step [0,+2]
	// Second step has an error estimate < 1e-10
	ti    = xdata[1]; 
	tf    = xdata[2];
	zi[0] = Z(ti);
	zf    = CppAD::Runge45(F, M, ti, tf, zi, e);
	ok   &= (e[0] <= 1e-10);  
	ok   &= CppAD::NearEqual(Z(tf), zf[0], 0., 1e-10);

	return ok;
}

int main(void)
{	if( LinearInterpolateOk() )
		std::cout << "Ok:    LinearInterpolate" << std::endl;
	else	std::cout << "Error: LinearInterpolate" << std::endl;
	return 0;
}

/* $$
$end
*/
