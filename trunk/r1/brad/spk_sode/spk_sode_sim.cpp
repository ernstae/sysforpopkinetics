
# include <gsl/gsl_rng.h>
# include <gsl/gsl_randist.h>
# include "spk_sode_model.hpp"
# include <spk/fitPopulation.h>
# include <spk/Optimizer.h>
# include <spk/SpkException.h>
# include <cppad/lu_solve.hpp>

// Empty namespace 
namespace {
	using CppAD::vector;
	using std::valarray;

	void copy(valarray<double> &a, const vector<double> &v)
	{	assert(a.size() == v.size() );
		size_t i;
		for(i = 0; i < v.size(); i++)
			a[i] = v[i];
	}

	void copy(vector<double> &v, const valarray<double> &a)
	{	assert(v.size() == a.size() );
		size_t i;
		for(i = 0; i < v.size(); i++)
			v[i] = a[i];
	}
}

int main(void)
{	size_t i, j, k, k1, k2;

	// # individuals in population study
	size_t M   = 50;   

	// # measurements per subject
	size_t Ni  = 10;

	// time between first and second measurement
	double dt  = .5;

	// factor that maps simulation values to initial values
	double sim2initial = 1.5;

	// construct the spk_sode_model
	double         sigma = .001; // measurement noise std
	size_t         ell   = 7;     // # fixed effects
	size_t         m     = 3;     // # random effects
	vector<size_t> N(M);          // # measure for each subject
	vector<double> t(M * Ni);     // measurement times
	size_t Si = 0;
	for(i = 0; i < M; i++)
	{	N[i] = Ni;
		for(j = 0; j < Ni; j++)
			t[j + Si] = dt * j;
		Si += Ni;
	}
	// construct the population model
	spk_sode_model popModel(sigma, ell, m, N, t);

	// simulated fixed and random effects vectors
	vector<bool>   alpha_fixed(ell);
	vector<double> alpha_sim(ell);
	vector<double> eta_sim(m);

	// simulation: exp( mean negative log of initial volume )
	alpha_sim[0]   = 2.;
	alpha_fixed[0] = false;
	// simulation: exp( mean log of k_a )
	alpha_sim[1]   = 1.;
	alpha_fixed[1] = false;
	// simulation: exp( mean log of k_e )
	alpha_sim[2]   = .2;
	alpha_fixed[2] = false;
	// simulation: log( variance for log of initial volume )
	alpha_sim[3]   = log(alpha_sim[0] * 1e-2);     // 10% CV
	alpha_fixed[3] = false;
	// simulation: log( variance for log of k_a )
	alpha_sim[4]   = log(alpha_sim[1] * 1e-2);     // 10% CV
	alpha_fixed[4] = false;
	// simulation: log( variance for log of k_e )
	alpha_sim[5] = log(alpha_sim[2] * 1e-2);       // 10% CV
	alpha_fixed[5] = false;

	// simulate rate of change in - log(volume) per unit time
	// exp( alpha_sim[6] ) = drift
	double drift = 1e-1;
	alpha_sim[6] = log(drift);
	alpha_fixed[6] = false;

	// setup gsl random number generator
	gsl_rng_env_setup();
	gsl_rng *rng  = gsl_rng_alloc( gsl_rng_default );

	// simulate the data set 
	valarray<double> F(Ni);          // data mean for one individual
	vector<double> data( t.size() ); // entire data vector
	Si = 0;                          // offset for each individual
	valarray<double> alpha_array(ell);
	valarray<double> eta_array(m);
	copy(alpha_array, alpha_sim);
	popModel.setPopPar( alpha_array );
	for(i = 0; i < M; i++)
	{	// set individual in model
		popModel.selectIndividual(int(i));

		// random effects for this individual
		for(k = 0; k < m; k++)
		{	double sigma_k = exp( alpha_sim[m + k] / 2. );
			eta_sim[k] = gsl_ran_gaussian(rng, sigma_k);
		}

		for(j = 0; j < Ni; j++)
		{
			// for change in alpha_sim[0] * exp( eta_sim[0] )
			// equal to drift * dt
			eta_sim[0] =
			log( exp(eta_sim[0]) + drift * dt / alpha_sim[0] );
			// set the random effects
			copy(eta_array, eta_sim);
			popModel.setIndPar( eta_array );
			// evaluate the model
			popModel.dataMean(F);
			// only use the j-th data point
			data[Si + j] = F[j] + gsl_ran_gaussian(rng, sigma);
		}
		Si += Ni;
	}

	// fit the population model
	Objective objective = FIRST_ORDER; 
	valarray<int> nMeasurementsAll( N.size() );
	for(i = 0; i < N.size(); i++)
			nMeasurementsAll[i] = N[i];
	valarray<double> measurementsAll( t.size() );
	copy(measurementsAll, data);
	double epsilonIn  = 1e-4;
	int    nMaxIterIn = 40;
	int    levelIn    = 1;
	Optimizer popOptimizer(epsilonIn, nMaxIterIn, levelIn);
	levelIn = 0;
	Optimizer indOptimizer(epsilonIn, nMaxIterIn, levelIn);
	valarray<double> popParLow(ell), popParUp(ell), 
		popParIn(ell), popParStep(ell), popParOut(ell);
	for(k = 0; k < ell; k++)
	{	if( alpha_fixed[k] )
		{	popParLow[k] = alpha_sim[k];
			popParUp[k]  = alpha_sim[k];
			popParIn[k]  = alpha_sim[k];
		}
		else if( k < m )
		{	// alpha is exponential of mean of log of pk parameter
			popParLow[k] = alpha_sim[k] / 10.;
			popParUp[k]  = alpha_sim[k] * 10.;
			popParIn[k]  = alpha_sim[k] * sim2initial;
		}
		else 
		{	//  alpha is log of variance of pk parameter
			popParLow[k] = alpha_sim[k] - 5.;
			popParUp[k]  = alpha_sim[k] + 5.;
			popParIn[k]  = alpha_sim[k] + log(sim2initial);
		}
		popParStep[k] = 1e-2;
	}
	valarray<double> indParLow(m), indParUp(m), 
		indParAllIn(m * M), indParStep(m), indParAllOut(m * M);
	for(k = 0; k < m; k++)
	{	indParLow[k] = -2;
		indParUp[k]  = +2;
		indParStep[k] = 1e-3;
		for(i = 0; i < M; i++)
			indParAllIn[k + i * m] = 0.;
	}
	double popObjOut;
	valarray<double> popObj_popParOut(ell);
	valarray<double> popObj_popPar_popParOut(ell * ell);
	DirBasedParallelControls dirBasedParallelControls(false, NULL, NULL);
	try
	{	fitPopulation(
			popModel,
			objective,
			nMeasurementsAll,
			measurementsAll,
			popOptimizer,
			popParLow,
			popParUp,
			popParIn,
			popParStep,
			&popParOut,
			indOptimizer,
			indParLow,
			indParUp,
			indParAllIn,
			indParStep,
			&indParAllOut,
			&popObjOut,
			&popObj_popParOut,
			&popObj_popPar_popParOut,
			dirBasedParallelControls
		);
	}
	catch ( SpkException& e )
	{	std::cerr << "SpkException during fitPopulation" 
		          << std::endl << e;	
		return 1;
	}
	catch(...)
	{	std::cerr << "Unknown exception during fitPopulation"
		          << std::endl;	
		return 1;
	}
	// compute covariance matrix from information matrix
	vector<double> info(ell * ell), eye(ell * ell), cov(ell * ell);
	for(k1 = 0; k1 < ell; k1++)
	{	for(k2 = 0; k2 < ell; k2++)
		{	// info is symmetric part of second derivative apx
			info[k1 * ell + k2] = 
				popObj_popPar_popParOut[k1 + k2 * ell];
			info[k1 * ell + k2] += 
				popObj_popPar_popParOut[k1 + k2 * ell];
			info[k1 * ell + k2] /= 2.;
			if( alpha_fixed[k1] | alpha_fixed[k2] )
				info[k1 * ell + k2] = 0.;
			eye[k1 * ell + k2] = 0.;
		} 
		eye[k1 * ell + k1] = 1.;
		if( alpha_fixed[k1] )
			info[k1 * ell + k1] = 1.;
	}
	double logdet;
	int signdet = CppAD::LuSolve(ell, ell, info, eye, cov, logdet);
	printf("signdet = %d\n", signdet);
	// 
	printf("popObjOut = %12g\n", popObjOut);
	printf("%12s%12s%12s%12s%12s%12s\n", 
	"name", "simulation", "lower bnd", "upper bnd", "estimate", "std err"
	);
	const char *fmt = "%12s%12g%12g%12g%12g%12g\n";
	const char *name[] = {
		"E[vol^-1]",
		"E[k_a]",
		"E[k_e]",
		"V[log vol]",
		"V[log ka]",
		"V[log ke]",
		"drift"
	};
	double  std_err;
	for(k = 0; k < 3; k++)
	{	std_err = sqrt(cov[k * ell + k]);
		if( alpha_fixed[k] )
			std_err = 0.;
 		printf(fmt, name[k], alpha_sim[k], 
			popParLow[k], popParUp[k], popParOut[k], std_err
		);
	}
	for(k = 3; k < ell; k++)
	{	// standard error of alpha[k]
		std_err = sqrt(cov[k * ell + k]);
		// standard error of exp(alpha) 
		std_err *= exp(popParOut[k]);
		if( alpha_fixed[k] )
			std_err = 0.;
		printf(fmt, name[k], exp(alpha_sim[k]), 
		exp(popParLow[k]), exp(popParUp[k]), exp(popParOut[k]), std_err
		);
	}
	return 0;
}
