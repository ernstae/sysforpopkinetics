# include <valarray>
# include <cassert>

# include "../MapBay.h"
# include "../MontePopObj.h"
# include "../AnalyticIntegral.h"

# include "LinearModel.h"
# include "NearEqual.h"

#include <stdlib.h>
#include <gsl/gsl_sf_erf.h>

bool MontePopObjTest(void)
{	bool ok = true;
	using std::valarray;

	// variance of measurements
	double SigmaR = .25;

	// variance of random effects
	double SigmaD = .5; 

	// the special constant
	double Pi   = 4. * atan(1.);

	// number of individuals
	size_t numberIndividuals  = 2;

	// number of measurements for each intividual
	valarray<int> N(numberIndividuals);
	size_t i;
	for(i = 0; i < numberIndividuals; i++)
		N[i] = i + 2;

	// number of fixed effects
	size_t numberFixedEffects = 1;

	// number of random effects
	size_t numberRandomEffects = 1;

	// lower and upper limits for integration
	valarray<double> L(numberRandomEffects);
	valarray<double> U(numberRandomEffects);
 	L[0] = -1.;
	U[0] =  1.;

	// time corresponding to each of the measurements
	int totalNumberMeasurements = N.sum();
	valarray<double> Time(totalNumberMeasurements);
	size_t j;
	size_t k = 0;
	for(i = 0; i < numberIndividuals; i++)
	{	for(j = 0; j < N[i]; j++)
			Time[k + j] = double(j);
		k += N[i];
	}

	// data vector
	double slope = .5;
	double intercept = 2.;
	valarray<double> y(totalNumberMeasurements);
	for(k = 0; k < totalNumberMeasurements; k++)
		y[k] = intercept + slope * Time[k]; 

	// construct the test model
	LinearModel Model(SigmaD, SigmaR, N, Time);

	// fixed effect; i.e., slope
	valarray<double> alpha(numberFixedEffects);
	alpha[0] = slope;

	// number of Map Bayesian evaluations for each individual
	size_t numberEval = 1000;

	// values returned by MontePopObj
	double integralEstimate;
	double estimateStd;
	MontePopObj(
		Model               , 
		N                   ,
		y                   ,
		alpha               ,
		L                   ,
		U                   ,
		numberEval          ,
		//
		integralEstimate    ,
		estimateStd
	);

	// value to check against
	double integral = 0;
	for(i = 0; i < numberIndividuals; i++)
	{
		integral -= log( AnalyticIntegral(
			Model               , 
			N                   ,
			y                   ,
			alpha               ,
			L                   ,
			U                   ,
			i
		) );
	}

	ok  &= NearEqual( integral, integralEstimate, 0., 3 * estimateStd);
	ok  &= ( (0. < estimateStd) & (estimateStd / integral < 1e-2) ); 

	return ok;
}
