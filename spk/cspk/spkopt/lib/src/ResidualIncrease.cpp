# include "Memory.h"
# include "QuadBox.h"
# include "Residual.h"
# include "MaxAbs.h"
# include "Next.h"

# include <cstddef>
# include <iostream>
# include <cassert>
# include <string>
# include <sstream>
# include <math.h>

static bool Ok(const char * msg)
{	return strcmp(msg, "ok") == 0;
}

bool ResidualIncrease(std::string &Msg)
{	bool ok = true;

	size_t kMax    = 20;
	size_t k       = 0;
	size_t level   = 0;
	size_t n       = 3;
	double epsilon = 6.8396011448489192e-12;
	double Q[] = {
	1530.5897683011265, 99679.654350699886, -2436.0973094540345, 
	99679.654350699886, 6491664.7713290714, -158651.50386502879, 
	-2436.0973094540345, -158651.50386502879, 3877.3323588440367
	};
	double r[] = {
	3.0665626982173677e-08, 3.6331283581603202e-06, 1.8728691930647301e-06
	};
	double l[] = {
	-0.0003762717290825, -0.0003762717290825, -0.0003762717290825
	};
	double u[] = {
	.0003762717290825, 0.0003762717290825, 0.0003762717290825
	};
	double x[] = {  0.,   0.,   0.};
	double a[] = {
	1.8177292143437356e-06, 1.8177292143437356e-06, 1.8177292143437356e-06
	};
	double b[] = {
	1.8177292143437356e-06, 1.8177292143437356e-06, 1.8177292143437356e-06
	};
	double eps = epsilon;
	Msg = QuadBox(
		std::cout, kMax, level, n, Q, r, l, u, k, eps, x, a, b);
	if( eps != epsilon )
		Msg = " :QuadBox could not obtain desired accuracy";
	if( Msg == "ok" )
	{	std::ostringstream buf;
		buf << " :QuadBox Iteration Count = " << k;
		Msg = buf.str();
	}
	else	ok = false;

	size_t i;
	for(i = 0; i < n; i++)
	{	double Qxi = 0;
		size_t j;
		for(j = 0; j < n; j++)
			Qxi += Q[i * n + j] * x[j];

		ok &= l[i] < x[i];
		ok &= x[i] < u[i];
		ok &= 0. < a[i]; 
		ok &= 0. < b[i]; 

		ok &= fabs( Qxi + r[i] - a[i] + b[i] ) <= epsilon;
		ok &= a[i] * (x[i] - l[i]) <= epsilon;
		ok &= b[i] * (u[i] - x[i]) <= epsilon;
	}

	return ok;
}
