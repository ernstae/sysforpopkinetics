/*
$begin OneBreakOk$$
$spell
	otime
$$

$section Test OdeBreak With One Break Point$$

$head Differential Equation$$
Suppose that we wish to solve the following differential equation
for $latex x : \R \rightarrow \R^n$$ :
$latex \[
\begin{array}{rcll}
	x ( 0 )        & = & 0              \\
	x_0^\prime (t) & = & 2 * t           \\
	x_i^\prime (t) & = & i \cdot x_{i-1} (t) & (i > 0)
\end{array}
\] $$

$head Analytic Solution$$
$latex \[
	x_i (t)  =  t^{i+1}
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
	size_t n;  // dimension of this problem
	size_t k;  // previous index passed to Break method
public:
	Eval(size_t n_)
	{	n = n_;	 // dimension of this problem
		k = 3;   // invalid index value 
	} 
	void Break(size_t k_, const vector<double> &x, vector<double> &g)
	{	assert( g.size() == n );
		assert( x.size() == n );
		k = k_;
		switch(k)
		{	size_t i;

			case 0:
			for(i = 0; i < n; i++)
				g[i] = 0.;
			break;

			default:
			assert(0);
		}
		return;
	}
	void Ode(double t, const vector<double> &x, vector<double> &f)
	{	assert( f.size() == n );
		assert( x.size() == n );
		switch(k)
		{	size_t i;

			case 0:
			f[0] = 2. * t;
			for(i = 1; i < n; i++)
				f[i] = (2 + i) * x[i-1];
			break;

			default:
			assert(0);
		}
		return;
	}
};

} // End empty namespace

bool OneBreakOk(void)
{	bool ok = true;
	size_t K = 1; // number of break point times
	size_t J = 1; // number of output point times
	size_t n = 5; // number of components in x 

	// evaluation method
	Eval eval(n);

	// break point times
	vector<double> btime(1);
	btime[0] = 0.;

	// output grid
	vector<double> otime(J);
	size_t j;
	otime[0] = .5;

	// absolute error 
	vector<double> eabs(n);
	size_t i;
	for(i = 0; i < n; i++)
		eabs[i] = 0.;

	// relative error
	double erel = 1e-10;

	// results vector
	vector<double> xout(n);

	// numerical solution of differential equation
	OdeBreak(eval, btime, otime, eabs, erel, xout);

	// check the output values
	double tip2 = otime[0];
	for(i = 0; i < n; i++)
	{	tip2 *= otime[0]; 
		ok &= CppAD::NearEqual(xout[i], tip2, erel, eabs[i]);
	}

	return ok;
}
/* $$
$end
*/
