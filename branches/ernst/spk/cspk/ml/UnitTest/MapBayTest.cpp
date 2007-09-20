# include <valarray>
# include <cassert>
# include "../MapBay.h"

# include "LinearModel.h"
# include <CppAD/NearEqual.h>

bool MapBayTest(void)
{	using std::valarray;
	using std::slice;

	// initial flag
	bool ok = true;

	// number of measurements for each individual
	valarray<int> N(2);
	N[0] = 1;
	N[1] = 2;

	// time corresponding to each of the measurements
	valarray<double> T(3);
	T[0] = 1.;
	T[1] = 2.;
	T[2] = 3.;

	// data vector
	valarray<double> Y(3);
	Y[0] = 1.;
	Y[1] = 2.;
	Y[2] = 3.;

	// other model parameters
	double Rvar = .1;
	double Dvar = .2; 
	double Pi   = 4. * atan(1.);


	// construct the test model
	LinearModel Model(Dvar, Rvar, N, T);

	// fixed effect; i.e., slope
	double slope = .5;
	valarray<double> alpha(1);
	alpha[0] = slope;

	// number of random effects
	size_t n = 1;

	// Evaluate for first indiviudal
	size_t individual = 0;

	// data for this individual
	valarray<double> y = Y[ slice(0, 1, 1) ];

	// pseudo constructor for this case
	MapBaySet(&Model, y, alpha, individual, n);
	
	// random effect; i.e., intercept
	double intercept = .5;
	double bi[1];
	bi[0] = intercept;
	
	// evaluate the Map Bayesian objective
	void *null = 0;
	double MapBayValue = MapBay(bi, n, null);

	double residual   = Y[0] - slope * T[0] - intercept;
	double CheckValue = .5 * bi[0] * bi[0] / Dvar
	                  + .5 * log( 2 * Pi * Dvar) 
	                  + .5 * residual * residual / Rvar
	                  + .5 * log( 2 * Pi * Rvar); 
	
	ok &= CppAD::NearEqual( MapBayValue, CheckValue, 1e-10, 1e-10);

	return ok;
}
