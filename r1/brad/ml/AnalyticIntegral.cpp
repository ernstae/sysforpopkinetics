# include <valarray>
# include <cassert>
# include <spk/SpkModel.h>

# include "MapBay.h"
# include "AnalyticIntegral.h"

#include <stdlib.h>
#include <gsl/gsl_sf_erf.h>


/*
$begin AnalyticIntegral$$
$spell
	Spk
	const
	valarray
	std
$$

$section Analytic Integration of Linear Model Map Bayesian Objective$$

$index analytic, test case$$
$index integral, test case$$
$index LinearModel, test case$$


$table
$bold Syntax$$ $cnext
$syntax%double AnalyticIntegral(
	SpkModel                    &%model%           ,
	const std::valarray<int>    &%N%               , 
	const std::valarray<double> &%y%               ,
	const std::valarray<double> &%alpha%           ,
	const std::valarray<double> &%L%               ,
	const std::valarray<double> &%U%               ,
	size_t                       %individual% 
)%$$
$tend

$fend 25$$


$head Linear Model$$
We use the $xref/MonteSpkModel/Notation/Monte-Carlo model notation/$$ 
with the individual index $italic i$$ fixed (and hence left out).
We also assume that there is only one random effect; i.e.,  $latex n = 1$$
and
$latex \[
\begin{array}{rcl}
	D  ( \alpha )            & = & \sigma_D^2 
	\\
	R ( \alpha , b )_{j,k}   & = & 
		\left\{
		\begin{array}{ll}
			\sigma_R^2  & {\rm if} \; j = k \\
			0           & {\rm otherwise}
		\end{array}
		\right.
	\\
	f ( \alpha , b )_j      & = & \alpha t_j + b
\end{array}
\] $$
where $latex t$$ is a vector containing the times of the measurements.

$head Analytic Integral$$
The first and second derivatives
of the Map Bayesian objective are given by
$latex \[
\begin{array}{rcl}
G^{(1)} ( b ) 
& = & \Sigma_D^{-1}  b - \Sigma_R^{-1} \sum_{j=1}^N ( y_j - \alpha t_j - b ) 
\\
& = & \left( \Sigma_D^{-1} + N \Sigma_R^{-1} \right) b
- \Sigma_R^{-1} \sum_{j=1}^N ( y_j - \alpha t_j ) 
\\
G^{(2)} ( b ) 
& = & \Sigma_D^{-1}  + N \Sigma_R^{-1} 
\end{array}
\] $$
We define $latex \hat{b}$$ as the value that minimizes $latex G(b)$$
and obtain the formula
$latex \[
\hat{b} = 
	\frac{ \Sigma_D } { N \Sigma_D + \Sigma_R }
		\sum_{j=1}^N ( y_j - \alpha t_j ) 
\] $$
$latex \[
\begin{array}{rcl}
	G(b) & = & G( \hat{b} )  + \frac{1}{2} G^{(2)} (b) ( b - \hat{b} )^2 
\end{array}
\] $$
We define the transformation $latex \Gamma ( b )$$ and obtain
$latex \[
\begin{array}{rcl}
\Gamma (b) & = &
\left( \Sigma_D^{-1} + N \Sigma_R^{-1} \right)^{1/2} ( b - \hat{b} )
\\
\Gamma^{-1} ( \gamma ) & = &
\left( \Sigma_D^{-1} + N \Sigma_R^{-1} \right)^{-1/2} \gamma + \hat{b}
\\
G[ \Gamma^{-1} ( \gamma ) ] & = & 
	G( \hat{b} ) + \frac{1}{2} \gamma^2
\\
\int_L^U \exp [ - G(b) ] \D b 
& = &
\exp[ - G( \hat{b} ) ]
\left( \Sigma_D^{-1} + N \Sigma_R^{-1} \right)^{-1/2} 
\int_{ \Gamma (L) }^{ \Gamma (U) } 
\exp( - \gamma^2 / 2 ) \D \gamma
\\
& = &
\exp[ - G( \hat{b} ) ]
\left( \Sigma_D^{-1} + N \Sigma_R^{-1} \right)^{-1/2} 
(  2 \pi  )^{1/2}
\{
Q[ \Gamma (L) ] - Q[ \Gamma (U) ]
\}
\end{array}
\] $$ 
where $latex Q(x)$$ is defined by
$latex \[
	Q(x) = ( 2 \pi )^{-1/2} \int_x^{\infty} \exp( - x^2 / 2 ) \D x
\] $$

$end

*/

double AnalyticIntegral(
	SpkModel                    &model           ,
	const std::valarray<int>    &N               , 
	const std::valarray<double> &y               ,
	const std::valarray<double> &alpha           ,
	const std::valarray<double> &L               ,
	const std::valarray<double> &U               ,
	size_t                       individual      )
{
	double Pi   = 4. * atan(1.);

	// set the fixed effects
	model.setPopPar(alpha);

	// set the individual
	model.selectIndividual(individual);

	// index in y where measurements for this individual starts
	size_t start = 0;
	size_t i;
	for(i = 0; i < individual; i++)
	{	assert( N[i] >= 0 );
		start += N[i];
	}

	// number of measurements for this individual
	assert( N[individual] >= 0 );
	size_t Ni = N[individual];

	// data for this individual
	std::valarray<double> yi = y[ std::slice( start, Ni, 1 ) ];

	// set the random effects = 0
	std::valarray<double> b(1);
	b[0] = 0.;
	model.setIndPar(b);

	std::valarray<double> Fi(Ni);
	model.dataMean(Fi);

	// model for the variance of random effects
	std::valarray<double> D(1);
	model.indParVariance(D);

	// model for variance of the measurements given random effects
	std::valarray<double> Ri( Ni * Ni );
	model.dataVariance(Ri);

	// determine bHat
	double residual = 0;
	size_t j;
	for(j = 0; j < Ni; j++)
	{	assert( Ri[j * Ni + j] == Ri[0] );
		residual += (yi[j] - Fi[j]);
	}
	std::valarray<double> bHat(1);
	bHat[0] = residual * D[0] / (Ni * D[0] + Ri[0]);

	// now evaluate the Map Bayesian objective at its optimal value
	model.setIndPar(bHat);
	model.dataMean(Fi);
	double sum = bHat[0] * bHat[0] / D[0] + log( 2. * Pi * D[0] );
	for(j = 0; j < Ni; j++)
	{	sum += (yi[j] - Fi[j]) * (yi[j] - Fi[j]) / Ri[0];
		sum += log( 2. * Pi * Ri[0] );
	}
	double GHat = .5 * sum;

	// square root of Hessian of the Map Bayesian objective
	double rootHessian = sqrt( 1. / D[0] + Ni / Ri[0]);

	// determine the values of beta that correspond to the limits
	double Gamma_L = (L[0] - bHat[0]) * rootHessian;
	double Gamma_U = (U[0] - bHat[0]) * rootHessian;

	// compute the upper tails corresponding to the standard normal
	double Q_L    = gsl_sf_erf_Q(Gamma_L);
	double Q_U    = gsl_sf_erf_Q(Gamma_U);

	// analytic value of the integral
	double factor   = exp(-GHat) * sqrt(2. * Pi) / rootHessian;
	return  factor * (Q_L - Q_U);
}
