# include "../GridIntegral.h"
# include <cmath>
# include <iostream>
# include <valarray>

namespace {
	double F(double *X, size_t m, void *p)
	{
		double sum = 0.;
		size_t i;
		for(i = 0; i < m; i++)
			sum += (i + 1) * X[i];
	
		return exp(-sum);
	}
}

bool GridIntegralTest(void)
{	bool ok = true;

	using std::valarray;

	size_t m        = 2;
	void  *p        = 0;
	valarray<int>    ngrid(50, m);
	valarray<double> Low(0., m);
	valarray<double> Up(1., m);
	
	double integralEstimate;
	double estimateStd;
	GridIntegral(F, m, p, ngrid, Low, Up, integralEstimate, estimateStd);

	double prod = 1.;
	double i;
	for(i = 1.; i <= m; i += 1.)
		prod *= (1. - exp(-i)) / i; 

	ok &= estimateStd <= 1e-2;
	ok &= fabs( integralEstimate - prod ) <= 2. * estimateStd;

	return ok;
}
