# include <math.h>
# include <stdlib.h>
# include <stdio.h>
# include <markov_chain/metropolis_hastings.h>

// --- static data and functions used by metropolis_hastings_ok -----------
static double mu    = 1.;
static double sigma = 2.;
static double pi(double x)
{	double r = x - mu;
	double v = sigma * sigma;
	return exp( - .5 * r * r / v ); 
}
static double q(double y, double x)
{	double d = fabs(y - x);
	if( d <= .5 )
		return 1.;
	else	return 0.;
}
static double u(void)
{	
	double r = (double) rand();
	return r / RAND_MAX;
}
static double s(double x)
{	return u() + x - .5; }
// ----------------test of metropolis_hastings ---------------------------
int metropolis_hastings_ok(void)
{	size_t i;
	double sample_mean;
	double sample_variance;

	int ok = 1;

	// intentionally bad starting point for the chain
	double x      = 3.;
	double pi_x   = pi(x);

	double sum    = 0.;
	double sum_sq = 0.;
	size_t N      = 1000000;
	for(i = 0; i < N; i++)
	{	metropolis_hastings(&x, &pi_x, &pi, &q, &s, &u);
		sum    += x;
		sum_sq += x * x; 
	}
	sample_mean     = sum / (double) N;
	sample_variance = sum_sq / (double) N - sample_mean * sample_mean;

	ok &= fabs(sample_mean - mu) < .1;
	ok &= fabs(sample_variance - sigma * sigma) < .1;
 
	return ok;
}
// -----------------------------------------------------------------------
