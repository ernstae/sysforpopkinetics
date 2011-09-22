/*
$begin raftery_lewis$$
$spell
	len
	quantile
	Raftery
	const
	Gilks
	Spiegelhalter
$$

$section Raftery Lewis MCMC Convergence Criteria$$

$head Syntax$$
$syntax%raftery_lewis(%len%, %u%, %q%, %r%, %s%, %k%, %l%, %m%, %n%)%$$


$head len$$
The argument $italic len$$ has prototype
$syntax%
	size_t %len%
%$$
It specifies the length of the input Markov chain.

$head u$$
The argument $italic u$$ has prototype
$syntax%
	const double *%u%
%$$
and is the Markov chain of length $italic len$$; i.e.,
$syntax%
	%u%[0] , %...% , %u%[%len%-1]
%$$
is the input Markov chain.
We use $latex \ell$$ to denote $italic len$$
and define the sequence $latex w_i$$ for $latex i = 0 , \ldots , \ell-1$$
as a sorted version of $italic u$$.
We define the sample cumulative distribution $latex C( \gamma )$$ as follows:
$pre

$$
Let $latex L$$ be the maximum 
index $latex t$$ such that $latex w_t \leq \gamma$$.
Let $latex M$$ be the minimum 
index $latex t$$ such that $latex \gamma \leq w_M $$.
$latex \[
C( \gamma ) = \left\{ \begin{array}{ll}
	[ (w_M - \gamma) L + (\gamma - w_L) M ] / [ \ell ( w_M - w_L ) ]
	& {\rm if} \; w_L < w_M 
	\\
	(L + M) / (2 \ell) & {\rm if} \; w_L = w_M \\
\end{array} \right.

$head q$$
The argument $italic q$$ has prototype
$syntax%
	double %q%
%$$
We define $latex \gamma_q$$ by 
$latex C( \gamma ) < q$$ for all $latex \gamma < \gamma_q$$ and
$latex C( \gamma ) > q$$ for all $latex \gamma > \gamma_q$$.
With probability $italic s$$,
the quantile corresponding to the true cumulative and value $latex \gamma_q$$ 
is within distance $italic r$$ from $italic q$$.

$head r$$
The argument $italic r$$ has prototype
$syntax%
	double %r%
%$$
It specifies the precision for estimating the quantile value.


$head s$$
The argument $italic s$$ has prototype
$syntax%
	double %s%
%$$
It specifies the probability for estimating the quantile value 
to with in precision $italic r$$.

$head k$$
The argument $italic k$$ has prototype
$syntax%
	size_t *%k%
%$$
and points to a single element.
The input value of $syntax%*%k%$$ does not matter.
On output it is the spacing between elements of the 
chain that are used in the final estimation procedure. 
For example, 
$syntax%
	%u%[%m%] %,% 
	%u%[%m% + %k%] %,% 
	%u%[%m% + 2 * %k%] %, ... ,% 
	%u%[%m% + (n / k) * k%] %, ... ,% 
\] $$
should be used to do the estimation of the desired quantile.

$head l$$
The arugment $italic l$$ has prototype
$syntax%
	size_t *%l%
%$$
The input value of $syntax%*%l%$$ does not matter.
On output it is 
a rough guide for the minimum value of $italic len$$ that should
be used.

$head m$$
The argument $italic m$$ has prototype
$syntax%
	size_t *%m%
%$$
and points to a single element.
The input value of $syntax%*%m%$$ does not matter.
On output it is number of initial chain elements to discard.

$head n$$
The argument $italic n$$ has prototype
$syntax%
	size_t *%n%
%$$
and points to a single element.
The input value of $syntax%*%n%$$ does not matter.
On output it is minimum chain length 
(not counting the initial discarded section)
necessary to estimate the desired quantile to the desired precision.

$head Reference$$
Implementing MCMC, A.E. Raftery, S.M. Lewis,
$italic Markov Chain Monte Carlo in Practice$$,
W.R. Gilks,
S. Richardson,
D.J. Spiegelhalter,
Chapman & Hall,
1996.

$end
*/

# include <math.h>
# include <assert.h>
# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <gsl/gsl_rng.h>

static int is_markov_one(size_t len, size_t *z)
{	double G2, bic;
	size_t i, j, k;
	double x_ijk, x_ij, x_jk, x_j, tmp;

	// 2 by 2 by 2 array
	size_t tran[8];
	for(i = 0; i < 8; i++)
		tran[i] = 0;

	// count the number of second order transitions
	for(i = 2; i < len; i++)
		++tran[ z[i-2]*4 + z[i-1]*2 + z[i] ];


	// see page 271 of Bishop, Feinberg, Holland 
	G2 = 0;
	for(j = 0; j < 2; j++)
	{	x_j = tran[0*4 + j*2 + 0] + tran[0*4 + j*2 + 1]
		    + tran[1*4 + j*2 + 0] + tran[1*4 + j*2 + 1];
		for(i = 0; i < 2; i++)
		{	for(k = 0; k < 2; k++)
			{	x_ijk = tran[i*4 + j*2 + k];
				x_ij  = tran[i*4 + j*2 + (1-k)] + x_ijk;
				x_jk  = tran[(1-i)*4 + j*2 + k] + x_ijk;
				tmp   = x_ijk * x_j / ( x_ij * x_jk );
				G2   += 2. * x_ijk * log( tmp );
			}
		}
	}

	// bic corresponding to value of G2
	bic = G2 - log( (double) (len - 2) ) * 2.;

	return (bic < 0.);
}

static int cmp(const void *px, const void *py)
{	double x, y;
	int r;
	x = * (double *) px;
	y = * (double *) py;
	if( x < y )
		r = -1;
	else if ( x == y )
		r = 0;
	else	r = 1;
	return r;
}

void raftery_lewis(
	size_t len    , 
	double *u     , 
	double  q     , 
	double  r     , 
	double  s     , 
	size_t *k     , 
	size_t *l     , 
	size_t *m     ,
	size_t *n     )
{	size_t t, i, j, L, M;
	size_t m_star, n_star;
	size_t len_1, len_2, kount;
	double alpha, beta, gamma, epsilon, lambda, sigma;
	double max_ab, sum_ab, fraction, bic, tmp, P;
	int ok, zero_one;

	// a two 2 by 2 matrix;
	size_t tran[2 * 2];

	// allocate memory
	double *w  = (double *) malloc( sizeof(double) * len );
	size_t *zt = (size_t *) malloc( sizeof(size_t) * len );
	size_t *zk = (size_t *) malloc( sizeof(size_t) * len );

	// check input values
	assert( len > 0 );
	len_1 = len - 1;
	len_2 = len - 2;

	// check if original sequence is zeros and ones
	zero_one = 1;
	for(t = 0; t < len; t++)
		zero_one &= ((u[t] == 0.) | (u[t] == 1.));

	if( ! zero_one )
	{
		// copy from chain into work array
		memcpy(w, u, sizeof(double) * len);

		// sort the w array
		qsort(w, len, sizeof(double), cmp);

		// determine the value of gamma corresponding to q
		L = (size_t)( q * len_1 );
		M = L;
		while( L > 0 && w[L-1] == w[L] )
			--L;
		while( M < len_2 && w[M] == w[M+1] )
			++M;
		if( M < len_2 && (M < q * len_1) )
			++M;
		fraction = M - q * len_1;
		gamma    = fraction * w[L] + (1. - fraction) * w[M];

		// dichotomize the data with true for <= gamma
		for(t = 0; t < len; t++)
		{	if( u[t] <= gamma )
				zt[t] = 1;
			else	zt[t] = 0;
		}
	}
	else
	{	for(t = 0; t < len; t++)
			zt[t] = (size_t) u[t];
	}

	// loop to determine the proper value for k
	*k = 0;
	ok = 0;
	while( ! ok & (*k) * (*k) < len)
	{	// thin the chain in step size *k
		*k   += 1;
		kount = 0;
		for(t = 0; t < len; t += *k)
			zk[kount++] = zt[t];

		// test for order one markov chain
		ok = is_markov_one(kount, zk);
	}
	assert( ok );
# if 0

	// count the number of first order transitions
	for(j = 1; j < kount; j++)
		++tran[ zk[j-1]*2 + zk[j] ];

	// fit probability of transition from 0 -> 1
	alpha = tran[0*2 + 1] / (double) (tran[0*2 + 0] + tran[0*2 + 1]); 
	// fit probability of transition from 1 -> 0
	beta  = tran[1*2 + 0] / (double) (tran[1*2 + 0] + tran[1*2 + 1]); 
	// maximum of alpha and beta
	if( alpha > beta )
		max_ab = alpha;
	else	max_ab = beta; 
	// sum of alpha and beta
	sum_ab = alpha + beta;
	// compute lambda in equation for Q^l
	lambda = 1. - sum_ab;
	// half the requested precision
	epsilon = r / 2.;
	// compute m_star
	tmp = log( sum_ab * epsilon / max_ab );
	assert( tmp < 0. ); 
	tmp = tmp  / log( fabs(lambda) );
	assert( tmp > 0. ); 
	m_star = (size_t) (tmp + 1.);
	// burn in length
	*m = m_star * (*k);

	// set phi such that the probability that a standard normal is 
	// less than or equal phi is P.
	sigma = 1.;
	P     = (s + 1.) / 2.;
	phi = gsl_cdf_gaussian_Pinv(P, sigma);
	// compute n_star
	tmp = sum_ab * sum_ab * sum_ab * r * r;
	tmp = (2. - sum_ab) * alpha * beta * phi * phi / tmp;
	n_star = (size_t ) (tmp + 1.); 
	// suggest chain length
	*n = n_star * (*k);
	// minimum pilot chain length
	tmp =  phi * phi * q * (1. - q) / (r * r);
	*l  = (size_t) (tmp + 1.);
# endif
	
	// free memory
	free(zk);
	free(zt);
	free(w);
}

int main(void)
{	// pilot chain parameters
	size_t len     = 100;   // length
	double alpha   = .5;   // probability of 0 -> 1
	double beta    = .5;   // probability of 1 -> 0

	double q       = .5;
	double r       = .01;
	double s       = .01;
	size_t k, l, m, n;

	size_t t, i, j;
	double p, tmp, *u;
	const gsl_rng_type *T;

	// initialize random number generator
	gsl_rng_env_setup();
	T = gsl_rng_default;
	gsl_rng *rng = gsl_rng_alloc(T);

	// create a chain corresponding to alpha, beta
	u = malloc(len * sizeof(double) );
	u[0] = 1.;
	for(t = 1; t < len; t++)
	{	if( u[t-1] == 0. )
		{	// probability of changing value 0 -> 1
			p = alpha;
			// sample from a uniform (0, 1)
			tmp = gsl_rng_uniform(rng);
			if( tmp <= p )
				u[t] = 1.;
			else	u[t] = 0.; 	
		}
		else 
		{	// probability of changing value 1 -> 0
			p = beta;
			// sample from a uniform (0, 1)
			tmp = gsl_rng_uniform(rng);
			if( tmp <= p )
				u[t] = 0.;
			else	u[t] = 1.; 	
		}
	}

	raftery_lewis(len, u, q, r, s, &k, &l, &m, &n);

	// print results
	printf("k = %d\n", k);

	free(u);
	return 0;
}
