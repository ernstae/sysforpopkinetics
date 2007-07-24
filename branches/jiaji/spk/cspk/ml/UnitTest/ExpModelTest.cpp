# include <valarray>
# include <cassert>

# include "../MapBay.h"
# include "../MapMonte.h"
# include "../GridIntegral.h"

# include "ExpModel.h"
# include "MontePars.h"
# include <CppAD/NearEqual.h>

#include <stdlib.h>
#include <gsl/gsl_sf_erf.h>

// zero (no miser test) one (yes miser test)
# define MISER 0

namespace {
	double Data[]= {
		3.000, 9.300,
//		4.000, 8.600,
		5.000, 6.700,
//		6.000, 6.100,
		7.000, 5.800,
//		8.000, 5.100,
		9.000,   4.400,
//		10.000,  4.500,
		11.000,  4.000,
//		14.000,  2.800,
		17.000,  2.900,
//		20.000,  2.400,
		25.000,  1.880,
//		30.000,  1.580,
		35.000,  1.200,
//		40.000,  1.050,
		45.000,  0.950,
//		50.000,  0.790,
		55.000,  0.720,
//		60.000,  0.580,
		70.000,  0.520,
//		80.000,  0.410,
		90.000,  0.330,
//		100.000, 0.290,
		110.000, 0.260,
//		120.000, 0.210,
		140.000, 0.160,
//		160.000, 0.110,
		180.000, 0.090
	};
}

// Map Bayesian objective
double ExpNegMapBay(double *x, size_t nB, void *parms)
{	return exp( - MapBay(x, nB, parms) );
}

bool ExpModelTest(void)
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

	// pseudo constructor of ExpNegMapBay for this individual
	MapBaySet(&model, y, alpha, individual, nInd);
	void *null = 0;
	double estimate_grid;
	double error_grid;
	std::valarray<int> number_grid(11, nInd);
	GridIntegral(
		ExpNegMapBay ,
		nInd         , 
		null         ,
		number_grid  ,
		L            ,
		U            ,
		estimate_grid,
		error_grid
	);
	error_grid = error_grid / estimate_grid;
	estimate_grid = log( estimate_grid ),
	ok &= error_grid < .1;
	//
	double estimate_plain;
	double error_plain;
	int    number_monte = 1;
	size_t i;
	for(i = 0; i < nInd; i++)
		number_monte *= number_grid[i]; 
	MapMonte(
		MontePars::plain ,
		model            ,
		N                ,
		y                ,
		alpha            ,
		L                ,
		U                ,
		individual       ,
		number_monte     ,
		estimate_plain   ,
		error_plain
	);
	error_plain = error_plain / estimate_plain;
	estimate_plain = log( estimate_plain ),

	ok &= error_plain < .5;
	ok &= fabs(estimate_grid-estimate_plain) < error_plain+error_grid;

	double estimate_vegas;
	double error_vegas;
	MapMonte(
		MontePars::vegas ,
		model            ,
		N                ,
		y                ,
		alpha            ,
		L                ,
		U                ,
		individual       ,
		number_monte     ,
		estimate_vegas   ,
		error_vegas
	);
	error_vegas = error_vegas / estimate_vegas;
	estimate_vegas = log( estimate_vegas ),

	ok &= error_vegas < .05;
	ok &= fabs(estimate_grid-estimate_vegas) < error_vegas+error_grid;

# if MISER
	std::cout << "estimate_grid = " << estimate_grid << std::endl;
	std::cout << "error_grid    = " << error_grid    << std::endl;
	std::cout << "estimate_plain = " << estimate_plain << std::endl;
	std::cout << "error_plain    = " << error_plain    << std::endl;
	std::cout << "estimate_vegas = " << estimate_vegas << std::endl;
	std::cout << "error_vegas    = " << error_vegas    << std::endl;
	double estimate_miser;
	double error_miser;
	MapMonte(
		MontePars::miser ,
		model            ,
		N                ,
		y                ,
		alpha            ,
		L                ,
		U                ,
		individual       ,
		number_monte     ,
		estimate_miser   ,
		error_miser
	);
	error_miser = error_miser / estimate_miser;
	estimate_miser = log( estimate_miser ),
	std::cout << "estimate_miser = " << estimate_miser << std::endl;
	std::cout << "error_miser    = " << error_miser    << std::endl;

	ok &= fabs(estimate_grid-estimate_miser) < error_miser+error_grid;
# endif

	return ok;
}
