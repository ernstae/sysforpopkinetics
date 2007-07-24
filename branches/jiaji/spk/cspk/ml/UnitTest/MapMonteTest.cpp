# include <valarray>
# include <cassert>

# include "../MapBay.h"
# include "../MapMonte.h"
# include "AnalyticIntegral.h"

# include "LinearModel.h"
# include <CppAD/NearEqual.h>

#include <stdlib.h>
#include <gsl/gsl_sf_erf.h>

bool MapMonteTest(void)
{	bool ok = true;
	using std::valarray;

	// variance of measurements
	double SigmaR = .25;

	// variance of random effects
	double SigmaD = .5; 

	// the special constant
	double Pi   = 4. * atan(1.);

	// number of individuals
	size_t numberIndividuals = 1;

	// number of measurements
	size_t numberMeasurements = 3;

	// number of fixed effects
	size_t numberFixedEffects = 1;

	// number of random effects
	size_t numberRandomEffects = 1;

	// lower and upper limits for integration
	valarray<double> L(numberRandomEffects);
	valarray<double> U(numberRandomEffects);
 	L[0] = -1.;
	U[0] =  1.;

	// number of measurements for each individual
	valarray<int> Ndata(numberIndividuals);
	Ndata[0] = numberMeasurements;

	// time corresponding to each of the measurements
	valarray<double> Time(numberMeasurements);
	Time[0] = 0.;
	Time[1] = 1.;
	Time[2] = 2.;

	// data vector
	valarray<double> Measurement(numberMeasurements);
	Measurement[0] = 0.;
	Measurement[1] = 1.;
	Measurement[2] = 2.;

	// construct the test model
	LinearModel Model(SigmaD, SigmaR, Ndata, Time);

	// fixed effect; i.e., slope
	double slope = .5;
	valarray<double> alpha(numberFixedEffects);
	alpha[0] = slope;

	// Evaluate for first indiviudal
	size_t individual = 0;

	// number of evaluations of the Map Bayesian objective
	size_t numberEval = 1000;

	// monte carlo method used
	enum MontePars::METHOD method[] = {
		MontePars::plain, 
		MontePars::miser
	};

	// value to check against
	double integral = AnalyticIntegral(
		Model               , 
		Ndata               ,
		Measurement         ,
		alpha               ,
		L                   ,
		U                   ,
		individual 
	);

	// values returned by MapMonte
	double integralEstimate;
	double estimateStd;
	size_t i;
	for(i = 0; i < 2; i++)
	{	MapMonte(
			method[i]           ,
			Model               , 
			Ndata               ,
			Measurement         ,
			alpha               ,
			L                   ,
			U                   ,
			individual          ,
			numberEval          ,
			//
			integralEstimate    ,
			estimateStd
		);

		ok  &= CppAD::NearEqual( 
			integral, integralEstimate, 0., 3 * estimateStd
		);
		ok  &= ( (0. < estimateStd) & (estimateStd < 1e-2) ); 
	}

	return ok;
}
