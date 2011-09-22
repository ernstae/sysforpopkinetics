
# include <gsl/gsl_rng.h>
# include <gsl/gsl_randist.h>
# include "spk_sode_model.hpp"
# include <spk/fitPopulation.h>
# include <spk/Optimizer.h>
# include <spk/SpkException.h>
# include <cppad/lu_solve.hpp>

// define Q_AT_INDEX_ZERO and R_AT_INDEX_ZERO
# include "smoother.hpp"

# define COEFFICIENT_OF_VARIATION .1
# define SIGMA_GAMMA              .01
# define SIGMA_DELTA              .01

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
	size_t M   = 10;   

	// # measurements per subject
	size_t Ni  = 10;

	// time between first and second measurement
	double dt  = .5;

	// factor that maps simulation values to initial values
	double sim2initial = 1.5;

	// number of fixed effects
	size_t ell = 7;

	// number of random effects
	size_t m = 2;

	// fract of drift over total time
	double a_drift = 1.;
	double b_drift = 0.;

	// simulated fixed and random effects vectors
	vector<bool>   alpha_fixed(ell);
	vector<double> alpha_sim(ell);
	vector<double> eta_sim(m);

	// mean a(t) at t = 0
	alpha_sim[0]   = 1.;
	alpha_fixed[0] = false;

	// mean b(t) at t = 0 
	alpha_sim[1]   = 1.;
	alpha_fixed[1] = false;

	// simulation: log( variance for log of a(0) )
	alpha_sim[2] = 2. * log( COEFFICIENT_OF_VARIATION );
	alpha_fixed[2] = false;

	// simulation: log( variance for log of b(0) )
	alpha_sim[3] = 2. * log( COEFFICIENT_OF_VARIATION );
	alpha_fixed[3] = false;

	// log of variance of measurement noise
	alpha_sim[4]   = 2. * log( COEFFICIENT_OF_VARIATION );
	alpha_fixed[4] = false;

	// log( variance for log of a(0) )
	alpha_sim[5]   = 2. * log( SIGMA_GAMMA );
	alpha_fixed[5] = true;

	// log( variance for log of b(0) )
	alpha_sim[6]   = 2. * log( SIGMA_DELTA );
	alpha_fixed[6] = true;

	// setup gsl random number generator
	gsl_rng_env_setup();
	gsl_rng *rng  = gsl_rng_alloc( gsl_rng_default );

	// simulate the data set 
	vector<size_t> N(M);           // number measurements per individual
	vector<double> t(Ni * M);      // time values
	vector<double> y(Ni * M);      // data values
	size_t Si = 0;                 // offset for each individual
	for(i = 0; i < M; i++)
	{	// random effects for this individual
		for(k = 0; k < m; k++)
		{	double sigma_k = exp( alpha_sim[m + k] / 2. );
			eta_sim[k] = gsl_ran_gaussian(rng, sigma_k);
		}

		// values of a_avg, b_avg and q(t), r(t) at t = 0
		double a_avg = alpha_sim[0] * exp( eta_sim[0] );
		double b_avg = alpha_sim[1] * exp( eta_sim[1] );
		double qj = Q_AT_INDEX_ZERO;
		double rj = R_AT_INDEX_ZERO;

		// compute the simulated data values for this subject
		N[i]  = Ni;
		for(j = 0; j < Ni; j++)
		{	double lambda = double(j) / double(Ni - 1) - .5;
			double aj = a_avg;
			double bj = b_avg;
			aj = a_avg * (lambda * a_drift + 1.);
			bj = b_avg * (lambda * b_drift + 1.);
			double sigma_epsilon = exp( alpha_sim[4] / 2. );
			t[Si + j] = j * dt;
			y[Si + j] = rj + gsl_ran_gaussian(rng, sigma_epsilon);
			// update for next time through loop
			rj = r_of_dt(dt, aj, bj, qj, rj);
			qj = qj * exp( - bj * dt);
		}
		Si += Ni;
	}

	// construct the model
	spk_sode_model popModel(N, t, y);

	// fit the population model
	Objective objective = FIRST_ORDER; 
	valarray<int> nMeasurementsAll( N.size() );
	for(i = 0; i < N.size(); i++)
		nMeasurementsAll[i] = N[i];
	valarray<double> measurementsAll( t.size() );
	copy(measurementsAll, y);
	double epsilonIn  = 1e-3;
	int    nMaxIterIn = 40;
	int    levelIn    = 1;
	Optimizer popOptimizer(epsilonIn, nMaxIterIn, levelIn);
	levelIn = 0;
	Optimizer indOptimizer(epsilonIn, nMaxIterIn, levelIn);
	valarray<double> popParLow(ell), popParUp(ell), 
		popParIn(ell), popParStep(ell), popParOut(ell);
	for(k = 0; k < ell; k++)
	{	if( k < m )
		{	// alpha is exponential of mean of log of pk parameter
			popParLow[k] = alpha_sim[k] / 10.;
			popParUp[k]  = alpha_sim[k] * 10.;
			popParIn[k]  = alpha_sim[k] * sim2initial;
		}
		else 
		{	//  alpha is log of a variance 
			popParLow[k] = alpha_sim[k] - 2 * log(100.);
			popParUp[k]  = alpha_sim[k] + 2 * log(100.);
			popParIn[k]  = alpha_sim[k] + log(sim2initial);
		}
		popParStep[k] = 1e-2 * (popParUp[k] - popParLow[k]);
		if( alpha_fixed[k] )
		{	popParLow[k] = alpha_sim[k];
			popParUp[k]  = alpha_sim[k];
			popParIn[k]  = alpha_sim[k];
		}
	}
	valarray<double> indParLow(m), indParUp(m), 
		indParAllIn(m * M), indParStep(m), indParAllOut(m * M);
	for(k = 0; k < m; k++)
	{	indParLow[k] = -2;
		indParUp[k]  = +2;
		indParStep[k] = 1e-2 * (indParUp[k] - indParLow[k]);
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
	//
	printf("signdet = %d\n", signdet);
	printf("popObjOut = %12g\n", popObjOut);
	printf("\n%12s%12s%12s%12s%12s%12s\n", 
	"name", "simulation", "lower bnd", "upper bnd", "estimate", "std err"
	);
	const char *fmt = "%12s%12g%12g%12g%12g%12g\n";
	const char *name[] = {
		"E[a]",
		"E[b]",
		"CV[a]",
		"CV[b]",
		"sig_eps",
		"sig_gamma",
		"sig_delta"
	};
	double  std_err;
	for(k = 0; k < m; k++)
	{	// standard error for alpha[k]
		std_err = sqrt( cov[k + k * ell] );
		if( alpha_fixed[k] )
			std_err = 0.;
 		printf(fmt, name[k], alpha_sim[k], 
			popParLow[k], popParUp[k], popParOut[k], std_err
		);
	}
	for(k = m; k < ell - 2; k++)
	{	// standard error of alpha[k]
		std_err = sqrt( cov[k + k * ell] );
		// standard error of exp(alpha / 2) 
		std_err *= exp(popParOut[k]/2.) / 2;
		if( alpha_fixed[k] )
			std_err = 0.;
		printf(fmt, name[k], exp(alpha_sim[k]/2.), 
		exp(popParLow[k]/2.), exp(popParUp[k]/2.), 
		exp(popParOut[k]/2.), std_err
		);
	}
	// open output file
	FILE *fp = fopen("spk_sode_sim.out", "w");
	fprintf(fp, "M = %d, Ni = %d\n%15s%15s%15s%15s\n", 
		M, Ni, "ti", "dri", "dai", "dbi"
	); 
	// compute the average weighted sum of squares for the gamma, delta
	printf("\n%15s%15s%15s%15s\n", 
		"individual", "epsilon_bar", "gamma_bar", "delta_bar"
	);
	vector<double> eta(m);
	vector<double> alpha(ell);
	for(k = 0; k < ell; k++)
		alpha[k] = popParOut[k];
	vector<double> ti(Ni), yi(Ni), ai(Ni), bi(Ni), qi(Ni), ri(Ni);
	Si = 0;
	double var_epsilon = exp( alpha[4] );
	double var_gamma   = exp( alpha[5] );
	double var_delta   = exp( alpha[6] );
	for(i = 0; i < M; i++)
	{	// for individal i
		eta[0] = indParAllOut[0 + i * m];
		eta[1] = indParAllOut[1 + i * m];
		//
		for(j = 0; j < Ni; j++)
		{	ti[j] = j * dt;
			yi[j] = y[j + Si];
		}
		abqr_of_initial(eta, alpha, ti, yi, ai, bi, qi, ri);
		double avgsq_epsilon = 0.;
		double avgsq_gamma   = 0.;
		double avgsq_delta   = 0.;
		double dri    = (yi[0] - ri[0]) / sqrt(var_epsilon);
		double dai    = 0.;
		double dbi    = 0.;
		avgsq_epsilon = dri * dri;
		fprintf(fp, "%15g%15g%15g%15g\n", ti[0], dri, dai, dbi);
		for(j = 1; j < Ni; j++)
		{	dri    = (yi[j] - ri[j]) / sqrt(var_epsilon);
			dai    = (ai[j] - ai[j-1]) / sqrt(var_gamma);
			dbi    = (bi[j] - bi[j-1]) / sqrt(var_delta);
			fprintf(fp, "%15g%15g%15g%15g\n", 
				ti[j], dri, dai, dbi
			);
			avgsq_epsilon += dri * dri;
			avgsq_gamma   += dai * dai;
			avgsq_delta   += dbi * dbi;
		}
		avgsq_epsilon /= Ni;
		avgsq_gamma   /= (Ni-1);
		avgsq_delta   /= (Ni-1);
		printf("%15d%15g%15g%15g\n", 
			i, 
			sqrt(avgsq_epsilon), 
			sqrt(avgsq_gamma), 
			sqrt(avgsq_delta)
		);
		Si += Ni;
	}
	fclose(fp);
	return 0;
}
