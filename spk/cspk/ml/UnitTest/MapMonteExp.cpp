# include <valarray>
# include <cassert>

# include "../MapBay.h"
# include "../MapMonte.h"

# include "ExpModel.h"
# include "NearEqual.h"

#include <stdlib.h>
#include <gsl/gsl_sf_erf.h>

namespace {
	double Data[]= {
		3.000, 9.300,
		4.000, 8.600,
		5.000, 6.700,
		6.000, 6.100,
		7.000, 5.800,
		8.000, 5.100,
		9.000,   4.400,
		10.000,  4.500,
		11.000,  4.000,
		14.000,  2.800,
		17.000,  2.900,
		20.000,  2.400,
		25.000,  1.880,
		30.000,  1.580,
		35.000,  1.200,
		40.000,  1.050,
		45.000,  0.950,
		50.000,  0.790,
		55.000,  0.720,
		60.000,  0.580,
		70.000,  0.520,
		80.000,  0.410,
		90.000,  0.330,
		100.000, 0.290,
		110.000, 0.260,
		120.000, 0.210,
		140.000, 0.160,
		160.000, 0.110,
		180.000, 0.090
	};
}

bool MapMonteExp(void)
{	bool ok = true;
	using CppAD::vector;

	// number of measurements
	std::valarray<int> N(1);
	N[0] = sizeof(Data) / (2 * sizeof(double));

	// value of the measurements
	std::valarray<double> Time( N[0] );
	std::valarray<double> y( N[0] );
	size_t j;
	for( j = 0; j < N[0]; j++)
	{	Time[j] = Data[ 2 * j ];
		y[j]    = Data[ 2 * j + 1 ];
	}


	// number of individual parameters (random effects)
	size_t nInd = 4;

	// number of population parameters (fixed effects)
	size_t nPop = nInd + nInd + 1;

	// value of the population parameters
	std::valarray<double> alpha(nPop);
	alpha[0] = 11.2659;            // Theta1
	alpha[1] = 4.44106;            // Theta2
	alpha[2] = 0.214275;           // Theta3
	alpha[3] = 0.0263766;          // Theta4
	alpha[4] = log( 0.0483123 );   // log( Omega11 )
	alpha[5] = log( 0.0471686 );   // log( Omega22 )
	alpha[6] = log( 0.0191292 );   // log( Omega33 )
	alpha[7] = log( 0.00537842 );  // log( Omega44 )
	alpha[8] = log( 0.0444396 );   // log( Sigma )

	// lower and upper limits for integration
	std::valarray<double> L(nInd);
	std::valarray<double> U(nInd);
	for(j = 0; j < nInd; j++)
 	{	U[j] = 2 * sqrt( exp( alpha[4+j] ) );
		L[j] = -U[j];
	}

	// construct the test model
	ExpModel model(N, Time);

	// Evaluate for first indiviudal
	size_t individual = 0;

	// number of evaluations of the Map Bayesian objective
	size_t numberEval = 1000;

	// values returned by MapMonte
	double integralEstimate;
	double estimateStd;
	MapMonte(
		model               , 
		N                   ,
		y                   ,
		alpha               ,
		L                   ,
		U                   ,
		individual          ,
		numberEval          ,
		//
		integralEstimate    ,
		estimateStd
	);

	return ok;
}
