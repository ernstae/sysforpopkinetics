/*
$begin MontePopObj$$
$spell
	enum
	Eval
	Obj
	Spk
	std
	const
	valarray
	neg
$$

$index integral, population objective$$
$index population, objective integral$$
$index objective, population integral$$

$section Using Monte Carlo Integration to Estimate Population Objective$$

$table
$bold Syntax$$ $cnext
$syntax%void MontePopObj(
	enum MontePars::METHOD      %method%           ,
	SpkModel<double>            &%model%           ,
	const std::valarray<int>    &%N%               , 
	const std::valarray<double> &%y%               ,
	const std::valarray<double> &%alpha%           ,
	const std::valarray<double> &%L%               ,
	const std::valarray<double> &%U%               ,
	size_t                       %numberEval%      ,
	//
	double                      &%negLogLikeEstimate%,
	double                      &%estimateStd%
)%$$
$tend

$fend 25$$

$head Description$$
Monte-Carlo integration is used to approximate the negative log-likelihood
function corresponding to the entire population;
i.e.,
$latex \[
	- \sum_{i=0}^{M-1} \log \{ \int_L^U \exp [ - G_i (b) ] \D b \}
\] $$
where $italic M$$ is the number of individuals in the population study
and for $latex i = 0 , \ldots , M-1$$ 
the function $latex G_i (b)$$ is defined by 
$latex \[
\begin{array}{rcl}
G_i ( b ) & = &
\frac{1}{2} \log \det [ 2  \pi  D( \alpha ) ]
+
\frac{1}{2} b^T D( \alpha )^{-1} b
+
\frac{1}{2} \log \det [ 2  \pi  R_i ( b , \alpha ) ]
\\
& + &
\frac{1}{2} 
[ y_i - f_i ( b , \alpha )]^T 
	R_i ( b , \alpha )^{-1} 
		[ y_i - f_i ( b , \alpha )] 
\end{array}
\] $$
where $latex i$$ is the index corresponding to this individual,
$latex D ( \alpha )$$ is the covariance of the random effects,
$latex f_i ( b , \alpha )$$ is the mean, given the fixed and random effects,
of this individuals measurements, and
$latex R_i ( b , \alpha )$$ is the variance, given the fixed and random effects,
of this individuals measurements.

$head method$$
The argument $italic method$$ can either be 
$code MontePars::plain$$ or $code MontePars::miser$$.
It determines which GSL monte carlo integration method is used.

$head model$$
is a $xref/MonteSpkModel//Monte-Carlo model/$$ 
object that represents this population model.
This will be used to evaluate the model functions
$latex D ( \alpha )$$, $latex R_i ( b , \alpha )$$ and 
$latex f_i ( b , \alpha )$$.

$head N$$
contains the number of measurements for each
individual in the population.
The size of $italic N$$ is equal to the number
of individuals in the population; i.e., $italic M$$.


$head y$$
contains the data for all the individuals in the population.
It length must be equal to the sum of the elements in $italic N$$.

$head alpha$$
value of the fixed effects.

$head L$$
lower limits for the value of the random effects.

$head U$$
upper limits for the value of the random effects.

$head numberEval$$
number of evaluations of the 
$xref/MapBay//Map Bayesian objective/$$ 
that will be used for each individual.
Each evaluation will correspond to a different
value for the random effects; i.e., $latex b$$
in the functions $latex f_i (b , \alpha )$$ and 
$latex R_i (b, \alpha )$$.
The value of $italic i$$ will be the
same for $italic numberEval$$ evaluations. 
Then it will changed to correspond to the next value.
The value of $italic \alpha$$ will be the same
for all the evaluations.

$head negLogLikeEstimate$$
The input value of $italic negLogLikeEstimate$$ does not matter.
Its output value
is an approximation for the negative log-likelihood.
This is an approximately normal random variable with 
mean equal to the negative log of the true integral
and standard deviation equal to $italic estimateStd$$.

$head estimateStd$$
The input value of $italic estimateStd$$ does not matter.
Its output value
is an approximation for the standard deviation of 
$italic negLogLikeEstimate$$.

$end

*/
# include <valarray>
# include <cassert>
# include <spk/SpkModel.h>

# include "MapMonte.h"
# include "MontePopObj.h"

void MontePopObj(
	enum MontePars::METHOD       method          ,
	SpkModel<double>            &model           ,
	const std::valarray<int>    &N               , 
	const std::valarray<double> &y               ,
	const std::valarray<double> &alpha           ,
	const std::valarray<double> &L               ,
	const std::valarray<double> &U               ,
	size_t                       numberEval      ,
	//
	double                      &negLogLikeEstimate,
	double                      &estimateStd     ) 
{
	negLogLikeEstimate      = 0.;
	double estimateVariance = 0.;
	size_t i;
	for(i = 0; i < N.size(); i++)
	{	double estimateOne;
		double stdOne;
		MapMonte(method, model, N, y, alpha, L, U, i, 
			numberEval, estimateOne, stdOne);
		negLogLikeEstimate  -= log( estimateOne );
		estimateVariance    += stdOne * stdOne 
		                     / (estimateOne * estimateOne);
	}
	estimateStd = sqrt( estimateVariance );
}
