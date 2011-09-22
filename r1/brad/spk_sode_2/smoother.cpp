# include <gsl/gsl_rng.h>
# include <gsl/gsl_randist.h>
# include <cppad/cppad.hpp>
# include "smoother.hpp"

namespace {
	double r_of_t(double t, double a, double b)
	{	return a * (exp(-b * t) - exp(-a * t)) / (a - b);
	} 

}

int main(void)
{	using std::cout;
	using std::endl;
	using CppAD::vector;

	bool ok = true;

	// setup gsl random number generator
	gsl_rng_env_setup();
	gsl_rng *rng  = gsl_rng_alloc( gsl_rng_default );

	// ----------------------------------------------------------------
	// test r_of_dt
	size_t Ni = 10;
	double dt = 1.;
	size_t j;
	double a = 1.;
	double b = a - 1e-5;
	double rj = 0.;
	double qj = 1.;
	for(j = 0; j < Ni; j++)
	{	double rjp = r_of_dt(dt, a, b, qj, rj);
		double t   = (j+1) * dt;
		double check = r_of_t(t, a, b);
		ok    &= fabs(rjp - check) < 1e-6;
		qj     = exp(-a * dt ) * qj;
		rj     = rjp;
	}
	b = .1;
	rj = 0.;
	qj = 1.;
	for(j = 0; j < Ni; j++)
	{	double rjp = r_of_dt(dt, a, b, qj, rj);
		double t   = (j+1) * dt;
		double check = r_of_t(t, a, b);
		ok    &= fabs(rjp - check) < 1e-6;
		qj     = exp(-a * dt ) * qj;
		rj     = rjp;
	}
	// ----------------------------------------------------------------
	// test abqr_of_initial
	double sigma_epsilon = .01; // std for y
	double sigma_gamma   = 1.;  // std for a
	double sigma_delta   = .1;  // std for b
	double sigma_eta_0   = 1.;  // std for eta[0]
	double sigma_eta_1   = 1.;  // std for eta[1]
	vector<double> eta(2);
	eta[0] = 0.;
	eta[1] = 0.;
	vector<double> alpha(7);
	alpha[0] = a * .5;
	alpha[1] = b * 1.5;
	alpha[2] = 2. * log( sigma_eta_0 );
	alpha[3] = 2. * log( sigma_eta_1 );
	alpha[4] = 2. * log( sigma_epsilon );
	alpha[5] = 2. * log( sigma_gamma );
	alpha[6] = 2. * log( sigma_delta );
	vector<double> tvec(Ni);
	vector<double> yvec(Ni);
	for(j = 0; j < Ni; j++)
	{	tvec[j] = j * dt;
		yvec[j] = r_of_t(tvec[j], a, b) 
			+ gsl_ran_gaussian(rng, sigma_epsilon);
	}
	vector<double> avec(Ni);
	vector<double> bvec(Ni);
	vector<double> qvec(Ni);
	vector<double> rvec(Ni);
	abqr_of_initial(eta, alpha, tvec, yvec, avec, bvec, qvec, rvec);
	//
	// value of objective at solution
	double sumsq_solution = weighted_sumsq(alpha[4], alpha[5], alpha[6], 
		tvec, yvec, avec, bvec
	);
	double step = 1e-2;
	for(j = 1; j < Ni; j++)
	{	// make sure solution is optimal in a_j direction
		double aj = avec[j];
		avec[j] = aj + step;
		double sumsq = weighted_sumsq( alpha[4], alpha[5], alpha[6], 
			tvec, yvec, avec, bvec
		);
		ok &= sumsq >= sumsq_solution;
		avec[j] = aj - step;
		sumsq = weighted_sumsq( alpha[4], alpha[5], alpha[6], 
			tvec, yvec, avec, bvec
		);
		ok &= sumsq >= sumsq_solution;
		avec[j] = aj;
		// make sure solution is optimal in b_j direction
		double bj = bvec[j];
		bvec[j] = bj + step;
		sumsq = weighted_sumsq( alpha[4], alpha[5], alpha[6], 
			tvec, yvec, avec, bvec
		);
		ok &= sumsq >= sumsq_solution;
		bvec[j] = bj - step;
		sumsq = weighted_sumsq( alpha[4], alpha[5], alpha[6], 
			tvec, yvec, avec, bvec
		);
		ok &= sumsq >= sumsq_solution;
		bvec[j] = bj;
	}
	cout << "ok = " << ok << endl;
	return 0;
}
