/*
$begin GridIntegral$$
$spell
	const
	std
	valarray
	const
$$

$section Numerical Integration by Evaluation on a Grid$$

$table
$bold Syntax$$
$cnext
$syntax%double GridIntegral(
	double (*%F%)(double *%X%, size_t %m%, void *p) ,
	size_t                        %m%   ,
	void                         *%p%   ,
	const std::valarray<size_t>  *%N%   ,
	const std::valarray<double>  &%L%   ,
	const std::valarray<double>  &%U%   )%$$

$tend

$fend 25$$

$head Return Value$$
is an approximation for the integral
$latex \[
	\int_L^U f(x) \D x
\]$$

$head F$$
The syntax
$syntax%
	F(%X%, %d%, %p%)
%$$
returns the value of the function $latex f$$ at the point
specified by $italic X$$ where $italic X$$ is a vector
of length $italic m$$. Both $italic m$$ and $italic p$$ are
the same value as in the call to $code GridIntegral$$.

$head d$$
The argument $italic m$$ to both $code GridIntegral$$ and
to $italic F$$ is the dimension of the space we are integrating
with respect to.

$head N$$
The vector $italic N$$ has length $italic m$$ and specifies
the number of grid points in each of the component directions.
The space between grid points in the $th i$$ component direction is
$latex \[
	( U_i - L_i ) / N_i
\] $$

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


double GridIntegral(
	double (*F)(double *X, size_t m, void *p)  ,
	size_t                                 m   ,
	void                                  *p   ,
	const std::valarray<size_t>           &N   ,
	const std::valarray<double>           &L   ,
	const std::valarray<double>           &U   )
{
	double sum = 0.;
	double *X  = new double[m];
	size_t *I  = new size_t[m];

	// initial grid point and volume of rectangle
	double volume = 1;
	size_t i;
	for(i = 0; i < m; i++)
	{	I[i] = 0;
		volume *= (U[i] - L[i]);
	}


	bool    more = true;
	size_t count = 0;
	while( more )
	{
		// neXt grid point value
		for(i = 0; i < m; i++)
			X[i] = L[i] + (I[i] + .5) * (U[i] - L[i]) / N[i];

		// add function value at this grid point
		sum += F(X, m, p);
		count++;

		// next grid point index
		i = 0;
		I[i]++;
		while( i < m && I[i] == N[i] )
		{	I[i] = 0;
			i++;
			more &= i < m;
			if( more )
				I[i]++;
		}
	}

	delete [] X;
	delete [] I;

	return volume * sum / double(count);
}
