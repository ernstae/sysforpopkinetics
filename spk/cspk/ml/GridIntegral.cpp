/*
$begin GridIntegral$$
$spell
	const
	std
	valarray
	const
	Feval
$$

$section Numerical Integration by Evaluation on a Grid$$

$table
$bold Syntax$$
$cnext
$syntax%void GridIntegral(
	double (*%Feval%)(double *%X%, size_t %m%, void *p) ,
	size_t                        %m%   ,
	void                         *%p%   ,
	const std::valarray<int>     &%N%   ,
	const std::valarray<double>  &%L%   ,
	const std::valarray<double>  &%U%   ,
	double                       &%integralEstimate%,
	double                       &%estimateStd%     )%$$

$tend

$fend 25$$

$head integralEstimate$$
is an approximation for the integral
$latex \[
	\int_L^U f(x) \D x
\]$$

$head estimateStd$$
The input value of $italic estimateStd$$ does not matter.
Its output value
is an approximation for the standard deviation of $italic estimateIntegral$$.


$head Feval$$
The syntax
$syntax%
	Feval(%X%, %d%, %p%)
%$$
returns the value of the function $latex f$$ at the point
specified by $italic X$$ where $italic X$$ is a vector
of length $italic m$$. Both $italic m$$ and $italic p$$ are
the same value as in the call to $code GridIntegral$$.

$head d$$
The argument $italic m$$ to both $code GridIntegral$$ and
to $italic Feval$$ is the dimension of the space we are integrating
with respect to.

$head N$$
The vector $italic N$$ has length $italic m$$ and specifies
the number of grid points in each of the component directions.
The space between grid points in the $th i$$ component direction is
$latex \[
	( U_i - L_i ) / N_i
\] $$
(All the elements of $italic N$$ must be greater than zero.)

$head L$$
The vector $italic L$$ has length $italic m$$ and specifies
the lower limit for the integration.

$head U$$
The vector $italic U$$ has length $italic m$$ and specifies
the upper limit for the integration.

$end

*/

# include "GridIntegral.h"
# include <iostream>
# include <valarray>

namespace {

	// map the vector I to a single index
	size_t Index(
		size_t m, 
		const std::valarray<int> &N, 
		const size_t             *I)
	{	size_t index = 0;
		size_t i;
		i = m;
		while( i-- )
		{	index *= N[i];
			index += I[i];
		}
		return index;
	}

	// increment the vector I
	bool Increment(
		size_t m, 
		const std::valarray<int> &N, 
		size_t                   *I)
	{	size_t i = 0;
		I[i]++;
		while( i < m && I[i] == N[i] )
		{	I[i] = 0;
			i++;
			if( i < m )
				I[i]++;
			else	return false;
		}
		return true;
	}

}

void GridIntegral(
	double (*Feval)(double *X, size_t m, void *p)  ,
	size_t                                 m   ,
	void                                  *p   ,
	const std::valarray<int>              &N   ,
	const std::valarray<double>           &L   ,
	const std::valarray<double>           &U   ,
	double                                &integralEstimate,
	double                                &estimateStd   )
{
	double sumF = 0.;
	double *X  = new double[m];
	size_t *I  = new size_t[m];

	// initial grid point and volume of rectangle
	size_t Ntot   = 1;
	double volume = 1;
	size_t i;
	for(i = 0; i < m; i++)
	{	I[i] = 0;
		volume *= (U[i] - L[i]);
		Ntot   *= N[i];

		assert( N[i] > 0 );
	}
	double *F = new double[Ntot];


	bool    more = true;
	size_t count = 0;
	while( more )
	{	assert( count == Index(m, N, I) );

		// neXt grid point value
		for(i = 0; i < m; i++)
			X[i] = L[i] + (I[i] + .5) * (U[i] - L[i]) / N[i];

		// add function value at this grid point
		F[count] = Feval(X, m, p);
		sumF    += F[count];
		count++;

		// next grid point index
		more = Increment(m, N, I);
	}
	assert( count == Ntot );

	// compute the sum of the absolute second partial
	double sumAverage = 0.;
	size_t j;
	for(j = 0; j < m; j++)
	{	double sumAbs = 0.;
		size_t numAbs = 0;

		// in the j-th coordinate direction
		for(i = 0; i < m; i++)
			I[i] = 0;

		for(count = 0; count < Ntot; count++)
		{	bool ok = (0 < I[j] && I[j]+1 < N[j]);
			if( ok )
			{	double F0 = F[ Index(m, N, I) ];
				I[j] -= 1;
				double Fm = F[ Index(m, N, I) ];
				I[j] += 2;
				double Fp = F[ Index(m, N, I) ];
				I[j] -= 1;
				assert( count == Index(m, N, I) );
				//
				sumAbs += fabs( Fp - 2. * F0 + Fm );
				numAbs++;
			}
			Increment(m, N, I);
		}
		if( numAbs > 0 )
			sumAverage += sumAbs / numAbs;
	}

	delete [] F;
	delete [] X;
	delete [] I;

	integralEstimate = volume * sumF / double(count);
	estimateStd      = volume * sumAverage * sqrt( double(Ntot) ) / 24.;

	return;
}
