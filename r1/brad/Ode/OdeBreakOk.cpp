/*
$begin OdeBreakOk$$
$spell
	namespace
	std
	endl
	exp
	Dirac
	Cpp
	Eval
	const
	bool
	eabs
	erel
	vector vector
	xout
	btime
	Eval eval
	otime
$$

$section Example Using OdeBreak$$

$head Differential Equation$$
Suppose that we wish to solve the following differential equation:
$latex \[
\begin{array}{rcl}
x_0 ( 0 ) & = & 0.9 \\
x_1 ( 0 ) & = & 0. \\
x_0^\prime (t) & = & \left\{ \begin{array}{ll}
	- x_0 (t) + .6 * \delta( t - .7) + .8 & {\rm if} \; t \leq .7 \\
	- x_0 (t) + .6 * \delta( t - .7)     & {\rm otherwise} 
\end{array} \right.
\\
x_1^\prime (t) & = & \left\{ \begin{array}{ll}
	x_0 (t) - x_1 (t) & {\rm if} \; t \leq .7 \\
	- x_1 (t)         & {\rm otherwise}
\end{array} \right.
\end{array}
\] $$
where $latex .6 * \delta ( t - .7) $$ is the Dirac delta function 
representation of a step increase of .6 in the value of
$latex x_0 (t)$$ at $latex t = .7$$

$head Analytic Solution$$

$head X_0 (t)$$
For $latex 0 < t \leq .7$$, the function $latex X_0 (t)$$ satisfies
the ordinary differential equation
$latex \[
\begin{array}{rcl}
X_0 ( 0 ) & = & 0.9 \\
X_0^\prime (t) & = & - X_0 (t) + .8 
\end{array}
\] $$
The solution of this initial value problem is
$latex \[
X_0 (t) = .1 * e^{-t} + .8 
\] $$ 
For $latex .7 < t \leq 1.$$, the function $latex X_0 (t)$$ satisfies
the ordinary differential equation
$latex \[
\begin{array}{rcl}
X_0 ( .7 ) & = & .1 * e^{-.7} + 1.4 \\
X_0^\prime (t) & = & - X_0 (t) 
\end{array}
\] $$
The solution of this initial value problem is
$latex \[
X_0 (t) = ( .1 * e^{-.7} + 1.4) * e^{.7 - t} 
\] $$ 


$head X_1 (t)$$
For $latex 0 < t \leq .7$$, the function $latex X_1 (t)$$ satisfies
the ordinary differential equation
$latex \[
\begin{array}{rcl}
X_1 ( 0 ) & = & 0.0 \\
X_1^\prime (t) 
& = & + X_0 (t) - X_1 (t) \\
& = & .1 * e^{-t} + .8 - X_1 (t) 
\end{array}
\] $$
The solution of this initial value problem is
$latex \[
X_1 (t) = .1 * t * e^{-t} + .8 * ( 1 - e^{-t} ) 
\] $$ 
For $latex .7 < t \leq 1.$$, the function $latex X_1 (t)$$ satisfies
the ordinary differential equation
$latex \[
\begin{array}{rcl}
X_1 ( .7 ) & = & .07 * e^{-.7} + .8 * ( 1 - e^{-.7} )\\
X_1^\prime (t) & = & - X_1 (t) 
\end{array}
\] $$
The solution of this initial value problem is
$latex \[
X_1 (t) = [ .07 * e^{-.7} + .8 * (1 - e^{-.7}) ]  * e^{.7 - t} 
\] $$ 


$codep */

# include <assert.h>
# include <CppAD/CppAD_vector.h>
# include <CppAD/NearEqual.h>

# include "OdeBreak.h"

using CppAD::vector;

namespace { // Begin empty namespace

class Eval {
private:
	size_t k;  // previous index passed to Break method
public:
	Eval(void)
	{	k = 3; } // initialize as invalid break point index value
	void Break(size_t k_, const vector<double> &x, vector<double> &g)
	{	assert( g.size() == 2 );
		assert( x.size() == 2 );
		k = k_;
		g[0] = g[1] = 0.;
		switch(k)
		{	case 0:
			g[0] = .9;
			break;

			case 1:
			break;

			case 2:
			g[0] = .6;
			break;

			default:
			assert(0);
		}
		return;
	}
	void Ode(double t, const vector<double> &x, vector<double> &f)
	{	assert( f.size() == 2 );
		assert( x.size() == 2 );
		switch(k)
		{	case 0:
			f[0] = - x[0] + .8;
			f[1] =   x[0] - x[1];
			break;

			case 1:
			// should never happen because btime[1] = btime[2]
			assert( 0 );

			case 2:
			f[0] = - x[0];
			f[1] = - x[1];
			break;

			default:
			assert(0);
		}
		return;
	}
};


} // End empty namespace

bool OdeBreakOk(void)
{	bool ok = true;
	size_t K = 3;  // number of break point times
	size_t J = 10; // number of output point times

	// evaluation method
	Eval eval;

	// break point times
	vector<double> btime(K);
	btime[0] = 0.;
	btime[1] = .7;
	btime[2] = .7;

	// output grid
	vector<double> otime(J);
	size_t j;
	for(j = 0; j < J; j++)
	otime[j] = double(j+1) / double(J);

	// absolute error 
	vector<double> eabs(2);
	eabs[0] = 1e-6;
	eabs[1] = 1e-6;

	// relative error
	double erel = 0.;

	// results vector
	vector<double> xout(2 * J);

	// numerical solution of differential equation
	OdeBreak(eval, btime, otime, eabs, erel, xout);

	// check the output values
	for(j = 0; j < J; j++)
	{	double t, x0, x1;
		t = otime[j];
		if( t <= .7 )
		{	x0 = .1 * exp(-t) + .8;
			x1 = .1 * t * exp(-t) + .8 * (1. - exp(-t));
		}
		else
		{	x0 = (.1 * exp(-.7) ) + 1.4;
			x0 *= exp(.7 - t);
			//
			x1 = .07 * exp(-.7) + .8 * (1 - exp(-.7));
			x1 *= exp(.7 - t);
		}
		ok &= CppAD::NearEqual(xout[0+j*2], x0, 1e-6, 1e-6);
		ok &= CppAD::NearEqual(xout[1+j*2], x1, 1e-6, 1e-6);
	}  
	return ok;
}

/* $$
$end
*/
