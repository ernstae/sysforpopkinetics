# include <assert.h>
# include <math.h>
# include <stdio.h>

# include "f2c.h"
# include "adapt.h"

static double Alpha[] = {2., 10., 5., 3.};

double functn(int *ndim, double *z)
{	double r = exp( Alpha[0] * z[0] ) 
	         * cos( Alpha[1] * z[1] )
	         * sin( Alpha[2] * z[2] )
	         * Alpha[3] * z[3] * z[3];
	return r;
}
int main(void)
{	// integer arguments to adapt_
	integer ndim, minpts, maxpts, lenwrk, ifail;
	// double arguments to adapt_
	doublereal *a, *b, eps, relerr, *wrkstr, finest;
	// local integer variables
	int i, two_ndim, maxfac, rulcls;
	// local double variables
	double check;

	// input values to adapt_
	eps      = 1e-4; // requested relative accuracy
	ndim     = 4;    // numer of variables we are integrating w.r.t.
	two_ndim = 1;    // 2^ndim
	for(i = 0; i < ndim; i++)
		two_ndim *= 2;
	maxfac   = 100;
	minpts   = 100;  // minimum number of function evaluations to use
	rulcls   =  two_ndim + 2*ndim*ndim + 6*ndim + 1;
	// suggested value for maximum number of fucntion evaluations to use
	maxpts   = maxfac * rulcls;
	// lenght of the work space
	lenwrk   = (2 * ndim + 3) * (1 + maxfac) / 2;
	// lower limits for integration
	a        = (doublereal *) malloc( ndim * sizeof(double) );
	// upper limits for integration
	b        = (doublereal *) malloc( ndim * sizeof(double) );
	// work space with unspecifed results
	wrkstr = (doublereal *) malloc( lenwrk * sizeof(double) );
	assert( ndim >= 2 );
	for(i = 0; i < ndim; i++)
	{	a[i] = 0.;
		b[i] = 1.;
	}
	adapt_(
		&ndim,
		a,
		b,
		&minpts,
		&maxpts,
		functn,
		&eps,
		&relerr,
		&lenwrk,
		wrkstr,
		&finest,
		&ifail
	);
	check = ( ( exp(Alpha[0]*b[0]) - exp(Alpha[0]*a[0]) ) / Alpha[0] ) 
	      * ( ( sin(Alpha[1]*b[1]) - sin(Alpha[1]*a[1]) ) / Alpha[1] )
	      * ( ( cos(Alpha[2]*a[2]) - cos(Alpha[2]*b[2]) ) / Alpha[2] )
	      * ( Alpha[3] * (b[3]*b[3]*b[3] - a[3]*a[3]*a[3]) / 3. );

	printf("ifail  = %d\n", ifail );
	printf("maxpts = %d\n", maxpts);
	printf("minpts = %d\n", minpts);
	printf("relerr = %g\n", relerr);
	printf("check  = %g\n", check);
	printf("finest = %g\n", finest);
	printf("(check - finest) / check = %g\n", (check - finest) / check);

	free(a);
	free(b);
	free(wrkstr);

	return 0;
}
