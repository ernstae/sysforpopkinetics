# include "GridIntegral.h"
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
	valarray<size_t> ngrid(50, m);
	valarray<double> Low(0., m);
	valarray<double> Up(1., m);
	
	double GridIntegralOut = GridIntegral(F, m, p, ngrid, Low, Up);

	double prod = 1.;
	double i;
	for(i = 1.; i <= m; i += 1.)
		prod *= (1. - exp(-i)) / i; 

	ok &= fabs( GridIntegralOut - prod ) <= 1e-3;

}
