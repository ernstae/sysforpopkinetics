/*
$begin data$$ 
$spell
	non_par
	const
	CppAD
$$

$section Data Set Simulation and Likelihood Evaluation$$

$head Syntax$$
$syntax%%y% = non_par::data_simulate(%m%, %beta%)%$$
$pre
$$
$syntax%%Psi% = non_par::data_likelihood(%beta%, %y%)%$$

$index simulate, population data$$
$index data, simulate population$$
$index population, simulate data$$

$head Purpose$$
Simulate a population analysis data set where the distribution
for the random effects is not specified.
Evaluate the individual likelihood corresponding to such a data set.


$head m$$
The argument $italic m$$ has prototype
$syntax%
	size_t %m%
%$$
It specifies the number of individuals in the simulated data set.

$head beta$$

$subhead simulate$$
The $code data_simulate$$ argument $italic beta$$ has prototype
$syntax%
	matrix<double> &%beta%
%$$
On input this matrix is size $latex 0 \times 0$$.
On output, it is an $latex m \times p$$ matrix
where $latex p$$ is the 
number of random effects per individual.
For $latex i = 0 , \ldots , m-1$$,
the $th i$$ row of $italic beta$$ is 
a simulated vector of random effects. 

$subhead likelihood$$
The $code data_likelihood$$ argument $italic beta$$ has prototype
$syntax%
	const matrix<%Type%> &%beta%
%$$
where $italic Type$$ is either $code double$$ or 
$code CppAD::AD<double>$$.
The argument $italic beta$$ is an $latex n \times p$$ matrix
where $latex n$$ is the number of values 
for the random effects and $latex p$$ is the 
number of random effects per individual.

$head y$$

$subhead simulate$$
The $code data_simulate$$ result $italic y$$ has prototype
$syntax%
	matrix<double> %y%
%$$
and is an $latex m \times q$$ matrix.
For $latex i = 0 , \ldots , m-1$$,
the vector $latex  y(i, :)$$ is simulated from the distribution
$latex \[
      {\bf p} [ y(i, :) \; | \; beta(i, :) ]
\] $$

$subhead likelihood$$
The $code data_likelihood$$ argument $italic y$$ has prototype
$syntax%
	const matrix<double> &%y%
%$$
and is an $latex m \times q$$ matrix.
For $latex i = 0 , \ldots , m-1$$,
the vector $latex  y(i, :)$$ was simulated from the distribution
$latex \[
      {\bf p} [ y(i, :) \; | \; beta\_y(i, :) ]
\] $$

$head Psi$$
The result $italic Psi$$ has prototype
$syntax%
	matrix<double> %Psi%
%$$
It is an $latex m \times n$$ matrix
with entry $latex (i, j)$$ equal to the likelihood 
$latex \[
	{\bf p} [ y(i, :) \; | \; beta(j, :) ]
\] $$

$end 
-----------------------------------------------------------------------
*/
# include <mat2cpp.hpp>
# include <boost/numeric/ublas/io.hpp>
# include <iostream>
# include <cmath>
# include <cassert>

namespace non_par { // BEGIN non_par namespace

mat2cpp::matrix<double> data_simulate(
	size_t m                      ,
	mat2cpp::matrix<double> &beta )
{	using namespace mat2cpp;
	size_t i, j;

	// check size of beta
	assert( (beta.size1() == 0) & (beta.size1() == 0) );
 
	// number of random effects per individual
	size_t p = 1;

	// number of measurement points per individual
	size_t q = 4;

	// standard deviation of the measurement noise
	double sigma = .2;

	// simulate the random effects uniform on [0, 2]
	beta  = rand(m, p);
	beta *= 2.;

	// modify to be uniform on [0,1] union (2,3]
	for(i = 0; i < m; i++)
	{	if( beta(i, 0) > 1. )
			beta(i, 0) += 1.;
	}

	// measurement noise
	matrix<double> noise = randn(m, q);
	noise *= sigma;

	// measurement values
	matrix<double> y(m, q);
	for(i = 0; i < m; i++)
	{	for(j = 0; j < q; j++)
		{	double time = 2. * j / double(q);
			double mean = exp( - beta(i, 0) * time );
			y(i, j)     = mean + noise(i, j);
		}
	}
	return y;
}

} // END non_par namespace 
