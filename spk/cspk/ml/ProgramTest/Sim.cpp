/*
$begin LinearModelSim$$

$latex \newcommand{\N}{ {\bf N} }$$
$latex \newcommand{\E}{ {\bf E} }$$
$latex \newcommand{\V}{ {\bf V} }$$
$latex \newcommand{\P}{ {\bf p} }$$

$section Simple Population Linear Model Simulator$$

$head Statistical Model$$
All the random variables $latex \varepsilon_{i,j}$$ and $latex  \eta_i$$
are independent.
$latex \[
\begin{array}{rcl}
	y_{i,j} & = & ( \theta + \eta_i ) * t_j + \varepsilon_{i,j}
\\
	\varepsilon_{i,j} & \sim & \N ( 0 , \sigma^2 )
\\
	\eta_i            & \sim & \N ( 0 , \omega^2 )
\end{array}
\] $$

$head Marginal Likelihood$$
$latex \[
\begin{array}{rcl}
	\E[ y_{i,j} | \theta ] & = & \theta + \eta_i t_j 
\\
	\V[ y_{i,j} | \theta ] & = & t_j \omega^2 +  \sigma^2
\\
	\P[ y_{i,j} | \theta ] & = & 
	[ 2 \pi (t_j \omega^2 +  \sigma^2) ]^{-1/2}
	\exp [ - .5 * (y_{i,j} - \theta)^2 / (t_j \omega^2 + \sigma^2 ) ]
\end{array}
\] $$

$end
*/

# include <cmath>
# include <valarray>
# include <iostream>
# include <fstream>
# include <cstdlib>
# include <cmath>
# include <gsl/gsl_rng.h>
# include <gsl/gsl_randist.h>
# include <gsl/gsl_cdf.h>

double NegLogLikelihood(
	double theta, 
	double OmegaSq,
	double SigmaSq,
	std::valarray<double> &t, 
	std::valarray<double> &y) 
{
	size_t i;
	size_t j;
	double term;
	double pi      = 4. * atan(1.);
	size_t N       = t.size();
	size_t M       = y.size() / N;
	double sum     = 0.;
	for(i = 0; i < M; i++)
	{	for(j = 0; j < N; j++)
		{	term  = y[i * N + j] - theta * t[j];
			term  = term * term;
			term  = term / (t[j] * OmegaSq + SigmaSq);
			term += log(2. * pi * (t[j] * OmegaSq + SigmaSq) );
			term  = term / 2.;
			sum  += term;
		}
	}
	return sum;
}

int main(int argc, char *argv[])
{	using std::ofstream;
	using std::endl;
	using std::cerr;
	using std::atof;
	using std::log;
	int i, j;

	ofstream dataFile( "Sim.dat" );
	ofstream logFile( "Sim.log" );

	// command line arguments
	if( argc != 6 )
	{	cerr << "usage: Sim seed M theta sigma omega" << endl;
		cerr << "seed  = random number generator input seed" << endl;
		cerr << "M     = number of subjects in study" << endl;
		cerr << "theta = mean value for slope of line" << endl;
		cerr << "sigma = measurement noise standard deviation" << endl;
		cerr << "omega = random effects standard deviation" << endl;
		return 1;
	}
	int seed     = atoi(*(++argv));
	int       M  = atoi(*(++argv));
	double theta = atof(*(++argv));
	double sigma = atof(*(++argv));
	double omega = atof(*(++argv));

	logFile << "Command line arguments" << endl;
	logFile << "seed  = " << seed  << endl;
	logFile << "M     = " << M     << endl;
	logFile << "theta = " << theta << endl;
	logFile << "sigma = " << sigma << endl;
	logFile << "omega = " << omega << endl;

	// random number generator type
	const gsl_rng_type *T;
	gsl_rng_env_setup();
	T = gsl_rng_default;

	// random number generator
	gsl_rng *r;
	r = gsl_rng_alloc(T);
	gsl_rng_set(r, (unsigned long int) seed);


	double sum_y1    = 0.;
	double sum_sq_y0 = 0.;
	double sum_sq_y1 = 0.;

	std::valarray<double> t(2);
	std::valarray<double> y(2 * M);

	// time
	for(j = 0; j < 2; j++)
		t[j] = double(j);

	srand( (unsigned int) seed );
	for(i = 0; i < M; i++)
	{	// simulate eta
		double eta = gsl_ran_gaussian(r, omega);
		// measurement values
		for(j = 0; j < 2; j++)
		{	// simulate eps
			double eps = gsl_ran_gaussian(r, sigma);

			// measurement value
			y[i * 2 + j]   = (theta + eta) * t[j] + eps;

			// output data value
			dataFile << i+1          << "  ";
			dataFile << t[j]         << "  ";
			dataFile << y[i * 2 + j] << "  ";
			dataFile << endl;

			double ysq = y[i * 2 + j] * y[i * 2 + j];

			// accumulate sums
			if( j == 0 )
				sum_sq_y0 += ysq;
			else
			{	sum_y1    += y[i * 2 + j];
				sum_sq_y1 += ysq;
			}
		}
	}

	// some constants
	double omegaSq = omega * omega;
	double sigmaSq = sigma * sigma;
	double sumSq   = omegaSq + sigmaSq;

	// percent confidence interval
	double percent  = 95.;
	double fraction = ( 1. - percent / 100. ) / 2.;

	// true parameter values
	logFile << endl << "True parameter values" << endl;
	logFile << "theta:   " << theta   << endl;
	logFile << "sigma^2: " << sigmaSq << endl;
	logFile << "omega^2: " << omegaSq << endl;

	// maximum likelihood estimates 
	double thetaHat   = sum_y1 / M;
	double sigmaSqHat = sum_sq_y0 / M; 
	double omegaSqHat = sum_sq_y1 / M
	                  - thetaHat * thetaHat
	                  - sigmaSqHat;
	logFile << endl << "Estimates for this simulation" << endl;
	logFile << "theta:     " << thetaHat << endl;
	logFile << "sigma^2:   " << sigmaSqHat << endl;
	logFile << "omega^2:   " << omegaSqHat << endl;

	// corresponding value of negative log likelihood
	logFile << "Objective: " <<
	 NegLogLikelihood(thetaHat, omegaSqHat, sigmaSqHat, t, y) << endl;

	// standard deviation of estimate between simulations
	double thetaStd  = sqrt( sumSq / M);
	double sigmaStd   = sigmaSq * sqrt(2. / M);
	double omegaVar   = 2. * sumSq * sumSq * (M-1) / (M * M)
	                  + 2. * sigmaSq * sigmaSq / M;
	double omegaStd   = sqrt(omegaVar);
	logFile << endl << "Standard deviations between simulations" << endl;
	logFile << "theta:   " << thetaStd << endl;
	logFile << "sigma^2: " << sigmaStd << endl;
	logFile << "omega^2: " << omegaStd << endl;

	// confidence intervals corresponding to true values
	double thetaLow  = theta 
	                 + gsl_cdf_gaussian_Pinv(fraction, thetaStd);
	double thetaUp   = theta 
	                 + gsl_cdf_gaussian_Qinv(fraction, thetaStd);
	double sigmaLow   = sigmaSq
	                  + gsl_cdf_gaussian_Pinv(fraction, sigmaStd);
	double sigmaUp    = sigmaSq
	                  + gsl_cdf_gaussian_Qinv(fraction, sigmaStd);
	double omegaLow   = omegaSq
	                  + gsl_cdf_gaussian_Pinv(fraction, omegaStd);
	double omegaUp    = omegaSq
	                  + gsl_cdf_gaussian_Qinv(fraction, omegaStd);
	logFile << endl << percent 
	        << "% Confidence Interval (using true values)" << endl;
	logFile << "theta:   [" << thetaLow << ", " << thetaUp << "]" << endl;
	logFile << "sigma^2: [" << sigmaLow << ", " << sigmaUp << "]" << endl;
	logFile << "omega^2: [" << omegaLow << ", " << omegaUp << "]" << endl;

	// confidence intervals corresponding to estimates
	thetaLow  = thetaHat 
	          + gsl_cdf_gaussian_Pinv(fraction, thetaStd);
	thetaUp   = thetaHat 
	          + gsl_cdf_gaussian_Qinv(fraction, thetaStd);
	sigmaLow   = sigmaSqHat
	          + gsl_cdf_gaussian_Pinv(fraction, sigmaStd);
	sigmaUp    = sigmaSqHat
	          + gsl_cdf_gaussian_Qinv(fraction, sigmaStd);
	omegaLow   = omegaSqHat 
	          + gsl_cdf_gaussian_Pinv(fraction, omegaStd);
	omegaUp    = omegaSqHat 
	          + gsl_cdf_gaussian_Qinv(fraction, omegaStd);
	logFile << endl << percent 
	        << "% Confidence Interval (using estimates)" << endl;
	logFile << "theta:   [" << thetaLow << ", " << thetaUp << "]" << endl;
	logFile << "sigma^2: [" << sigmaLow << ", " << sigmaUp << "]" << endl;
	logFile << "omega^2: [" << omegaLow << ", " << omegaUp << "]" << endl;

	double omegaMean  = sumSq * (M-1) / M - sigmaSq;
	return  0;
}
