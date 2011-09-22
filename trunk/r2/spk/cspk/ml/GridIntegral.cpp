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
	Feval(%X%, %m%, %p%)
%$$
returns the value of the function $latex f$$ at the point
specified by $italic X$$ where $italic X$$ is a vector
of length $italic m$$. Both $italic m$$ and $italic p$$ are
the same value as in the call to $code GridIntegral$$.

$head m$$
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
(All the elements of $italic N$$ must be greater than or equal 2.)
Note that $code GridIntegral$$ may use up to 
$latex \[
	2 * N[0] * N[1] *  \cdots  * N[m-1]
\] $$
function evaluations to evaluate the integral and approximate 
the error.

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
# include <cassert>
# include <spk/SpkError.h>
# include <spk/SpkException.h>

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
	double  GridIntegral(
		double (*Feval)(double *X, size_t m, void *p)  ,
		size_t                                 m       ,
		void                                  *p       ,
		const std::valarray<int>              &N       ,
		const std::valarray<double>           &L       ,
		const std::valarray<double>           &U       )
	{
		double sumF = 0.;
		double *X  = new double[m];
		size_t *I  = new size_t[m];

		// set I to initial grid point index
		size_t Ntot   = 1; // total number of function evaluations
		double Volume = 1; // total volume of the region of integration
		size_t i;
		for(i = 0; i < m; i++)
		{	I[i] = 0;
			Volume *= (U[i] - L[i]);
			Ntot   *= N[i];
			assert( N[i] > 0 );
		}
		double F;

		bool    more = true;
		size_t count = 0;
		while( more )
		{	assert( count == Index(m, N, I) );

			// next grid point value
			for(i = 0; i < m; i++) X[i] = 
				L[i] + (I[i] + .5) * (U[i] - L[i]) / N[i];

			// add function value at this grid point
			F     = Feval(X, m, p);
			sumF += F;
			count++;

			// next grid point index
			more = Increment(m, N, I);
		}
		assert( count == Ntot );

		delete [] X;
		delete [] I;

		double integralEstimate = Volume * sumF / double(count);
		return integralEstimate;
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
	integralEstimate  = GridIntegral(Feval, m, p, N, L, U);

	std::valarray<int> N2 = N / 2;
	size_t i;
	for(i = 0; i < N.size(); i++)
	{	if( N2[i] < 1 )
		{	int max = SpkError::maxMessageLen();
			char Buffer[200];
			sprintf( Buffer, 
				"Grid: N[%d] = %d is to small\n", 
				i                               , 
				int(N[i])
			);
			if( max - 1 < strlen(Buffer) )
				Buffer[max-1] = '\0';
		 	throw SpkException (
				SpkError::SPK_USER_INPUT_ERR,
				Buffer                      ,
				__LINE__                    ,
				__FILE__
			);
		}
	}
		
	std::valarray<int> M = N;
	std::valarray<double> lower(L * 1.33333);
	std::valarray<double> upper(U * 1.33333);
	double compareEstimate = GridIntegral(Feval, m, p, M, lower, upper);
	estimateStd            = fabs(integralEstimate - compareEstimate);

	return;
}
