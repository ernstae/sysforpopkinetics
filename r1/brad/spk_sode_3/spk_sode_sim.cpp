/*
$begin spk_sode_sim$$
$latex \newcommand{\B}[1]{{\bf #1}}$$

$section Example of SPK Fitting a Stochastic Differential Equation$$

$head Purpose$$
To test the idea of treating the process noise in a stochastic differential
equation as if it were random effects in a mixed effects model.

$head Deterministic ODE$$
We are given a vector of times $latex t \in \B{R}^{N(i)+1}$$.
For $latex j = 0 , \ldots , N_i$$
and $latex s \in [ t_j , t_{j+1} ]$$ we have the following deterministic ODE
$latex \[
\begin{array}{rcl}
	q ( t_j )     & = & q_j
	\\
	r ( t_j )     & = & r_j
	\\ 
	q^\prime ( s )          & = & - a_j q ( s )
	\\
	r^\prime ( s )          & = & + a_j q ( s ) - b_j r (s)
\end{array}
\] $$
Given $latex q_0$$ and $latex r_0$$
we define the rest of $latex q \in \B{R}^{N(i)+1}$$
and $latex r \in \B{R}^{N(i)+1}$$
by the following:
for $latex j = 0 , \ldots , N_i-1$$
$latex \[
\begin{array}{rcl}
	q_{j+1} = q ( t_{j+1} ) & = & q_j \exp [ - a_j ( t_{j+1} - t_j ) ]
	\\
	r_{j+1} = r ( t_{j+1} ) & = & \left[ r_j + a_j q_j 
	\frac{1 - \exp[ - ( a_j - b_j ) ( t_{j+1} - t_j ) ] }{ a_j - b_j }
		\right] \exp [ - b_j ( t_{j+1} - t_j ) ]
\end{array}
\] $$

$head Stochastic ODE$$
The stochastic part of our ODE is for the change in 
$latex a \in \B{R}^{N(i)}$$ ($latex \lambda = 1$$)
or the change in $latex b \in \B{R}^{N(i)}$$  ($latex \lambda = 0$$)
between time indices. 
To be specific
$latex \[
\begin{array}{rcl}
	a_0 & = & \mu_a + \eta_0
	\\
	b_0 & = & \mu_b + \eta_1
\end{array}
\] $$
and for $latex j = 1 , \ldots , N_i-1$$
$latex \[
\begin{array}{rcl}
	a_j & = & a_{j-1} + \lambda \eta_{j+1}
	\\
	b_j & = & b_{j-1} + (1 - \lambda) \eta_{j+1}
\end{array}
\] $$
where $latex \eta \in \B{R}^{N(i) + 1}$$,
$latex \eta_0 \sin \B{N} ( 0 , \sigma_a^2 )$$,
$latex \eta_1 \sin \B{N} ( 0 , \sigma_b^2 )$$,
and for $latex j = 2 , \ldots , N_i$$,
$latex \eta_j \sim \B{N} (0 , \sigma_\eta^2 )$$.
Note that if $latex \lambda = 0$$ corresponds to $latex a_j \equiv a_0$$ 
and $latex \lambda = 1$$ corresponds to $latex b_j \equiv b_0$$.
The measured data is given by $latex y \in \B{R}^{N(i)}$$
where $latex j = 0 , \ldots , N_i-1$$ 
$latex \[
	y_j = r_{j+1} + \varepsilon_j
\] $$
where 
$latex \varepsilon_j \sim \B{N} (0 , \sigma_\epsilon^2 )$$.

$head Fixed Effects$$
$table
$latex \mu_a$$  
	$cnext mean initial transfer rate $latex a_0$$
$rnext
$latex \mu_b$$  
	$cnext mean initial transfer rate $latex b_0$$
$rnext
$latex \log ( \sigma_a^2 )$$
	$cnext log of variance of $latex \eta_0$$ 
$rnext
$latex \log ( \sigma_b^2 )$$
	$cnext log of variance of $latex \eta_1$$ 
$rnext
$latex \log ( \sigma_\eta^2 )$$
	$cnext log of variance of $latex \eta_j$$ for $latex j = 2 , \ldots , N_i$$
$rnext
$latex \log ( \sigma_\varepsilon^2 )$$
	$cnext log of variance of measurement noise; i.e., 
		$latex y_j - r( t_{j+1} )$$
$tend

$head Simulation Values$$
$codep */
// Simulation parameters
# define SIMULATE_M              10    // number of individuals in study
# define SIMULATE_NI             10    // number measurements per individual
# define SIMULATE_TMAX           1.00  // time corresponding to last data point
# define SIMULATE_MU_A           2.00  // mean of a0
# define SIMULATE_MU_B           0.50  // mean of b0
# define SIMULATE_SIGMA_A        0.10  // std for a0
# define SIMULATE_SIGMA_B        0.10  // std for b0
# define SIMULATE_SIGMA_EPSILON  1e-3  // std for change in transfer rate
# define SIMULATE_Q0             1.00  // q(t) at t = 0
# define SIMULATE_R0             0.00  // r(t) at t = 0

// Other simulation parameters
# define SIMULATE_LAMBDA         1.00  // the \lambda parameter above (0 or 1)
# define SIMULATE_DELTA_A        0.00  // simulation value for a_{j+1} - a_j
# define SIMULATE_DELTA_B        0.00  // simulation value for b_{j+1} - b_j

// Factor that maps simulated values to initial values
# define SIMULATE_TO_INITIAL    0.50

/* $$
$end
-----------------------------------------------------------------------
*/
# include <gsl/gsl_rng.h>
# include <gsl/gsl_randist.h>
# include <spk/fitPopulation.h>
# include <spk/Optimizer.h>
# include <spk/SpkException.h>
# include <cppad/lu_solve.hpp>


// spk_sode_model.hpp uses SIMULATE_Q0 and SIMULATE_R0
# include "spk_sode_model.hpp"

# define CALCULATE_STANDARD_ERRORS 0

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
	size_t M   = SIMULATE_M;   

	// # measurements per subject
	size_t Ni  = SIMULATE_NI;

	// # number of measurements per subject plus one
	size_t Np  = Ni + 1;

	// change between time points
	double dt  = SIMULATE_TMAX / double(Ni);

	// number of fixed effects
	size_t ell = 6;

	// number of random effects per individual
	size_t m = Np;

	// simulated fixed and random effects vectors
	vector<bool>   alpha_fixed(ell);
	vector<double> alpha_sim(ell);
	// mean a(t) at t = 0
	alpha_sim[0]   = SIMULATE_MU_A;
	alpha_fixed[0] = true;

	// mean b(t) at t = 0 
	alpha_sim[1]   = SIMULATE_MU_B;
	alpha_fixed[1] = true;

	// log of variance of a0
	alpha_sim[2]   = 2. * log( SIMULATE_SIGMA_A );
	alpha_fixed[2] = true;

	// log of variance of b0
	alpha_sim[3]   = 2. * log( SIMULATE_SIGMA_B );
	alpha_fixed[3] = true;

	// log of variance of eta
	alpha_sim[4]   = 2. * log(1.);
	alpha_fixed[4] = false;

	// log of variance of epsilon
	alpha_sim[5]   = 2. * log( SIMULATE_SIGMA_EPSILON );
	alpha_fixed[5] = true;

	// setup gsl random number generator
	gsl_rng_env_setup();
	gsl_rng *rng  = gsl_rng_alloc( gsl_rng_default );

	// simulate the data set 
	vector<size_t> N(M);       // number measurements per individual
	vector<double> t(Np * M);  // time values
	vector<double> y(Ni * M);  // data values
	//
	for(i = 0; i < M; i++)
	{
		// initial values
		double aj = SIMULATE_MU_A +  gsl_ran_gaussian(rng, SIMULATE_SIGMA_A);
		double bj = SIMULATE_MU_B +  gsl_ran_gaussian(rng, SIMULATE_SIGMA_B);
		double qj = SIMULATE_Q0;
		double rj = SIMULATE_R0;

		// compute the simulated data values for this subject
		N[i]        = Ni;
		t[0 + i*Np] = 0.;
		for(j = 0; j < Ni; j++)
		{
			// advance values by one time index
			rj          = r_of_dt(dt, aj, bj, qj, rj);
			qj          = qj * exp( - aj * dt);
			double epsj = gsl_ran_gaussian(rng, SIMULATE_SIGMA_EPSILON);
			t[j+1 + i*Np] = (j + 1) * dt;
			y[j + i*Ni]   = rj + epsj;

			// advance for next time interval
			aj += SIMULATE_DELTA_A;
			bj += SIMULATE_DELTA_B;
		}
	}

	// construct the model
	spk_sode_model<double> popModel(N, t);
	spk_sode_model< CppAD::AD<double> > popModelAD(N, t);

	// fit the population model
	Objective objective = FIRST_ORDER;
	valarray<int> nMeasurementsAll( N.size() );
	for(i = 0; i < N.size(); i++)
		nMeasurementsAll[i] = N[i];
	valarray<double> measurementsAll( y.size() );
	copy(measurementsAll, y);
	double epsilonIn  = 1e-3;
	int    nMaxIterIn = 40;
	int    levelIn    = 1;
	Optimizer popOptimizer(epsilonIn, nMaxIterIn, levelIn);
	epsilonIn  = 1e-3;
	levelIn    = 0;
	nMaxIterIn = 100;
	Optimizer indOptimizer(epsilonIn, nMaxIterIn, levelIn);
	valarray<double> popParLow(ell), popParUp(ell), 
		popParIn(ell), popParStep(ell), popParOut(ell);

	popParIn[0] = SIMULATE_MU_A;
	popParIn[1] = SIMULATE_MU_B;
	for(k = 0; k < 2; k++)
	{	if( alpha_fixed[k] )
		{	popParLow[k] = popParIn[k];
			popParUp[k]  = popParIn[k];
		}
		else
		{	popParIn[k]  = SIMULATE_TO_INITIAL * popParIn[k];
			popParLow[k] = popParIn[k] / 10.;
			popParUp[k]  = popParIn[k] * 10.;
		}
	}
	popParIn[2] = 2. * log( SIMULATE_SIGMA_A );
	popParIn[3] = 2. * log( SIMULATE_SIGMA_B );
	popParIn[4] = 2. * log( 1. );
	popParIn[5] = 2. * log( SIMULATE_SIGMA_EPSILON );
	for(k = 2; k < ell; k++)
	{	if( alpha_fixed[k] )
		{	popParLow[k] = popParIn[k];
			popParUp[k]  = popParIn[k];
		}
		else
		{	popParIn[k] = SIMULATE_TO_INITIAL * popParIn[k];
			popParLow[k] = popParIn[k] - 2. * log ( 100. );
			popParUp[k]  = popParIn[k] + 2. * log ( 100. );
		}
	}
	valarray<double> indParLow(m), indParUp(m), 
		indParAllIn(m * M), indParStep(m), indParAllOut(m * M);

	indParUp[0]  = 3. * SIMULATE_SIGMA_A;
	indParLow[0] = - indParUp[0];
	indParUp[1]  = 3. * SIMULATE_SIGMA_B;
	indParLow[1] = - indParUp[1];
	for(k = 2; k < m; k++)
	{	indParUp[k]  = 3.;
		indParLow[k] = - indParUp[k];
	}
	for(k = 0; k < m; k++)
	{	indParStep[k] = 1e-2 * (indParUp[k] - indParLow[k]);
		for(i = 0; i < M; i++)
			indParAllIn[k + i * m] = 0.;
	}
	double popObjOut;
	valarray<double> popObj_popParOut(ell);
	valarray<double> popObj_popPar_popParOut(ell * ell);
	DirBasedParallelControls dirBasedParallelControls(false, NULL, NULL);
	try
	{	valarray<double> *Hessian = NULL;
# if CALCULATE_STANDARD_ERRORS
		Hessian = &popObj_popPar_popParOut;
# endif	
		fitPopulation(
			popModel,
			popModelAD,
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
			NULL,
			Hessian,
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
	bool no_compute;

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
			no_compute  = false;
			no_compute  |= alpha_fixed[k1];
			no_compute  |= alpha_fixed[k2];
			no_compute  |= (CALCULATE_STANDARD_ERRORS == 0);
			if( no_compute )
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
	printf("M  = %d\n", M);
	printf("Ni = %d\n", Ni);
	printf("dt = %f\n", dt);
	printf("\n%12s%12s%12s%12s%12s%12s\n", 
	"name", "simulation", "lower bnd", "upper bnd", "estimate", "std err"
	);
	const char *fmt = "%12s%12g%12g%12g%12g%12g\n";
	const char *name[] = {
		"mu_a",
		"mu_b",
		"sig_a",
		"sig_b",
		"sig_eta",
		"sig_epsilon"
	};
	double  std_err;
	for(k = 0; k < 2; k++)
	{	// standard error for alpha[k]
		std_err = sqrt( cov[k + k * ell] );
		if( alpha_fixed[k] )
			std_err = 0.;
 		printf(fmt, name[k], alpha_sim[k], 
			popParLow[k], popParUp[k], popParOut[k], std_err
		);
	}
	for(k = 2; k < ell; k++)
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
	vector<double> eta(Np);
	char filename[200];
	sprintf(filename, "plot_%g_%g_%g.m",
		SIMULATE_LAMBDA, SIMULATE_DELTA_A, SIMULATE_DELTA_B
	);
	i = 0;
	while(filename[i] != '\0' )
	{	if(filename[i] == '.' && filename[i+1] != 'm')
			filename[i] = 'v';
		i++;
	}
	FILE *fp = fopen(filename, "w");
	fprintf(fp, "data = [ ...\n");
	for(i = 0; i < M; i++)
	{	// for individal i
		for(k = 0; k < m; k++)
			eta[k] = indParAllOut[k + i * m];
		double tj = 0.;
		double aj = popParOut[0] + eta[0];
		double bj = popParOut[1] + eta[1];
		tj = 0.;
		for(j = 1; j <= Ni; j++)
		{	fprintf(fp, "%15g%15g%15g\n", tj, aj, bj);
			tj     = tj + dt;
			fprintf(fp, "%15g%15g%15g\n", tj, aj, bj);
			if( j < Ni )
			{	aj     += SIMULATE_LAMBDA*eta[j+1];
				bj     += (1.-SIMULATE_LAMBDA)*eta[j+1];
			}
		}
	}
	double xmin = 0.;
	double xmax = SIMULATE_TMAX;
	double amin = SIMULATE_MU_A - 1;
	double amax = SIMULATE_MU_A + 1;
	double bmin = SIMULATE_MU_B - 1;
	double bmax = SIMULATE_MU_B + 1;
	fprintf(fp, "];\n");
	fprintf(fp, "Ni = %d\n", Ni);
	fprintf(fp, "M  = %d\n", M);
	fprintf(fp, "mu_a = %f\n", SIMULATE_MU_A);
	fprintf(fp, "mu_b = %f\n", SIMULATE_MU_B);
	fprintf(fp, "sigma_a = %f\n", SIMULATE_SIGMA_A);
	fprintf(fp, "sigma_b = %f\n", SIMULATE_SIGMA_B);
	fprintf(fp, "delta_a = %f\n", SIMULATE_DELTA_A);
	fprintf(fp, "delta_b = %f\n", SIMULATE_DELTA_B);
	fprintf(fp, "lambda = %f\n", SIMULATE_LAMBDA);
	fprintf(fp, "a_limits = [ %f %f %f %f ];\n", xmin, xmax, amin, amax);
	fprintf(fp, "b_limits = [ %f %f %f %f ];\n", xmin, xmax, bmin, bmax);
	fprintf(fp, "text = \'lambda = %g, Delta a = %g, Delta b = %g, obj = %g\';\n",
		SIMULATE_LAMBDA, SIMULATE_DELTA_A, SIMULATE_DELTA_B, popObjOut
	);
	fprintf(fp, "filename = \'plot_%g_%g_%g.eps\';\n",
		SIMULATE_LAMBDA, SIMULATE_DELTA_A, SIMULATE_DELTA_B
	);
	char *matlab_source =
	"t  = reshape(data(:, 1), 2*Ni, M); % row is time, column is subject\n"
	"a  = reshape(data(:, 2), 2*Ni, M); \n"
	"b  = reshape(data(:, 3), 2*Ni, M);\n"
	;
	fprintf(fp, "%s", matlab_source);
	if( SIMULATE_LAMBDA == 1 )
	{	matlab_source = 
		"plot(t, a);\n"
		"ylabel('a(t)');\n"
		"axis(a_limits);\n"
		;
	}
	else
	{	matlab_source = 
		"plot(t, b);\n"
		"ylabel('b(t)');\n"
		"axis(b_limits);\n"
		;
	}
	fprintf(fp, "%s", matlab_source);
	matlab_source =
	"xlabel('time');\n"
	"title(text);\n"
	"print(filename);\n"
	;
	fprintf(fp, "%s", matlab_source);
	fclose(fp);
	return 0;
}
