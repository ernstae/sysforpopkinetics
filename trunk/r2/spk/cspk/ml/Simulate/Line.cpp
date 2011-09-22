/*
$begin Line$$
$spell
$$
	

$latex \newcommand{\R}{ {\bf R} }$$
$latex \newcommand{\N}{ {\bf N} }$$
$latex \newcommand{\E}{ {\bf E} }$$
$latex \newcommand{\V}{ {\bf V} }$$
$latex \newcommand{\P}{ {\bf p} }$$

$section Line with Slope and Intercept as Random Effects$$

$head Statistical Model$$
We are given a vector of times $latex t \in \R^N$$
and the independent random variables
for $latex i = 0 , \ldots , M-1$$ :
$latex \[
\begin{array}{rcl}
\eta_0^i & \sim & \N ( 0 , \omega_0^2 )           \\
\eta_1^i & \sim & \N ( 0 , \omega_1^2 )           \\
\varepsilon^i & \sim & \N ( 0 , \sigma^2 I )
\end{array}
\] $$
where $latex \omega > 0$$, $latex \sigma > 0$$,  and
$latex I$$ is the $latex N \times N$$ identity matrix.
The measurements vectors $latex y^i \in \R^N$$ are given by
$latex \[
y_j^i  =  ( \theta_0 + \eta_0^i ) 
     +   ( \theta_1 + \eta_1^i ) t_j + \varepsilon_j^i
\] $$
for $latex j = 0 , \ldots , N-1$$ :

$head Marginal Likelihood$$
The expected values and variances where 
$latex \eta_0^i$$ and $latex \eta_1^i$$
are integrated out are:
$latex \[
\begin{array}{rcl}
\E[ y^i | \theta , \omega, \sigma ]_j 
& = & 
\theta_0 + \theta_1 t_j 
\\
\V[ y^i | \theta , \omega, \sigma ]_{j,k}
& = & 
\omega_0^2 + t_j t_k \omega_1^2 + \sigma^2 \delta( j - k )
\end{array}
\] $$
where $latex \delta ( j - k ) $$ is one if $latex j = k $$
and zero otherwise.
It follows that
$latex \[
\begin{array}{rcl}
- \log \P[ y^i | \theta , \omega, \sigma ] 
& = & 
\frac{1}{2} \log \det 
	\{ 2 \pi \V[ y^i | \theta , \omega, \sigma ] \}
+
\frac{1}{2}  
	( y^i - \theta_0 - \theta_1 t )^T
		\V[ y^i | \theta , \omega, \sigma ]^{-1}
			( y^i - \theta_0 - \theta_1 t )
\\
\log \P[ y | \theta , \omega, \sigma ]
& = &
\sum_{i=0}^{M-1}
	\log \P[ y^i | \theta , \omega, \sigma ]

\end{array}
\] $$

$end
*/

# include <cmath>
# include <CppAD/CppAD.h>
# include <QN01Box/PlusInfinity.h>
# include <QN01Box/Memory.h>
# include <QN01Box/QuasiNewton01Box.h>
# include <iostream>
# include <fstream>
# include <cstdlib>
# include <cmath>
# include <gsl/gsl_rng.h>
# include <gsl/gsl_randist.h>
# include <gsl/gsl_cdf.h>

// negative log likelihood 
template <class Float>
Float NegLogLikelihood(
	const CppAD::vector<Float>   x, 
	const CppAD::vector<double> &t, 
	const CppAD::vector<double> &y
) 
{	using CppAD::vector;

	size_t i, j, k;
	Float r;
	Float rsq;
	Float rvar;
	Float tmp;
	Float pi      = 4. * atan(1.);
	size_t N      = t.size();
	size_t M      = y.size() / N;

	// unpack x
	vector<Float> theta(2);
	vector<Float> omega2(2);
	Float sigma2;
	theta[0]  = x[0];
	theta[1]  = x[1];
	omega2[0] = x[2];
	omega2[1] = x[3];
	sigma2    = x[4];

	// compute the variance of y^i given theta
	CppAD::vector<Float> V(N * N);
	for(j = 0; j < N; j++)
	{	for(k = 0; k < N; k++)
		{	V[j * N + k] = omega2[0] + omega2[1] * t[j] * t[k];
		}
		V[j * N + j] += sigma2;
	}

	CppAD::vector<Float> ri(N);
	int                  signdet;
	CppAD::vector<Float> u(N);
	Float                logdet;
	Float                sum     = 0.;
	for(i = 0; i < M; i++)
	{	// residual for this individual	
		for(j = 0; j < N; j++)
			ri[j] = y[i * N + j] - theta[0] - theta[1] * t[j];

		// compute V^{-1} * ri and log(det(V))
		signdet = CppAD::LuSolve(N, 1, V, ri, u, logdet);
		assert(signdet > 0 );

		// add ri^T * V^{-1} * ri to sum 
		for(j = 0; j < N; j++)
			sum += ri[j] * u[j];

		// add log(det(2 * pi * V)) to sum
		sum += log(2 * pi) * double(N) + logdet;
	}
	return sum / 2.;
}

class Fun {
private:
	const CppAD::vector<double> t;
	const CppAD::vector<double> y;
	const CppAD::vector<double> xLow;
	const CppAD::vector<double> xUp;
	CppAD::vector<double>       xScaled;
public:
	Fun(
	const CppAD::vector<double> t_, 
	const CppAD::vector<double> y_,
	const CppAD::vector<double> xLow_,
	const CppAD::vector<double> xUp_
	)
	: t(t_), y(y_), xLow(xLow_), xUp(xUp_), xScaled(5)
	{	size_t j;
		for(j = 0; j < 5; j++)
			xScaled[j] = 2; // outside [0,1] limits
	}		
	const char *function(const double *xScaled_, double &f)
	{	size_t j;
		CppAD::vector<double> x(5);
		double lambda;
		for(j = 0; j < 5; j++)
		{	lambda = xScaled[j] = xScaled_[j];
			x[j] = xLow[j] * (1. - lambda) + xUp[j] * lambda;
		}
		f =  NegLogLikelihood(x, t, y);
		return "ok";
	}
	const char *gradient(double *g)
	{	using CppAD::vector;
		using CppAD::AD;

		vector< AD<double> > XScaled(5);
		size_t j;
		for(j = 0; j < 5; j++)
			XScaled[j] = xScaled[j];
		Independent(XScaled);

		vector< AD<double> > X(5);
		AD<double> Lambda;
		for(j = 0; j < 5; j++)
		{	Lambda = XScaled[j];
			X[j] = xLow[j] * (1. - Lambda) + xUp[j] * Lambda;
		}
		vector < AD<double> > f(1); 
		f[0] = NegLogLikelihood(X, t, y);
		CppAD::ADFun<double> F(XScaled, f);

		vector<double> G(5);
		G = F.Jacobian(xScaled);

		for(j = 0; j < 5; j++)
			g[j] = G[j];

		return "ok";
	}
};


int main(int argc, char *argv[])
{	using std::ofstream;
	using std::endl;
	using std::cerr;
	using std::atof;
	using std::log;
	using CppAD::vector;
	int i, j;

	ofstream dataFile( "Line.dat" );
	ofstream logFile( "Line.log" );

	// command line arguments
	char *default_args[] = {
		"Line",  // Line
		"123",   // seed
		"10",    // M
		"5",     // N
		"1",     // dt
		"1",     // theta[0]
		"1",     // theta[1]
		"1",     // omega2[0]
		"1",     // omega2[1]
		"1"      // sigma2
	};
	bool ok = argc == 10;
	if(argc == 2)
		ok =  strcmp(argv[1], "default") == 0;
	if( ! ok )
	{	const char *msg = 
		"Line default\n"
		"Line seed M N dt "
		"theta[0] theta[1] omega2[0] omega2[1] sigma2"; 
		cerr << "usage:" << endl;
		cerr << msg      << endl;
		cerr << "seed     = random number generator input seed" << endl;
		cerr << "M        = number of subjects in study" << endl;
		cerr << "N        = number of measurements per subject" << endl;
		cerr << "dt       = time between measurements" << endl;
		cerr << "theta[0] = mean for intercept of line" << endl;
		cerr << "theta[1] = mean for slope of line" << endl;
		cerr << "omega2[0]= variance for intercept of line" << endl;
		cerr << "omega2[1]= variance for slope of line" << endl;
		cerr << "sigma2   = variance for measurement noise" << endl;
		cerr << endl;
		cerr << "Line default =";
		for(j = 0; j < 10; j++)
			cerr << " " << default_args[j];
		cerr << endl;
		return 1;
	}
	if( argc == 2 )
		argv = default_args;
	
	int seed      = atoi(*(++argv));
	int    M      = atoi(*(++argv));
	int    N      = atoi(*(++argv));
	double dt     = atof(*(++argv));
	double xScaled[5];
	vector<double> x(5);
	vector<double> xLow(5);
	vector<double> xUp(5);
	for(j = 0; j < 5; j++)
	{	x[j]       = atof(*(++argv));
		xLow[j]    = 1e-2 * x[j];
		xUp[j]     = 1e+1 * x[j];
		xScaled[j] = (x[j] - xLow[j]) / (xUp[j] - xLow[j]);
	}
	vector<double> theta(2);
	vector<double> omega2(2);
	double sigma2;
	theta[0]   = x[0];
	theta[1]   = x[1];
	omega2[0]  = x[2];
	omega2[1]  = x[3];
	sigma2     = x[4];


	// random number generator type
	const gsl_rng_type *rtype;
	gsl_rng_env_setup();
	rtype = gsl_rng_default;

	// random number generator
	gsl_rng *r;
	r = gsl_rng_alloc(rtype);
	gsl_rng_set(r, (unsigned long int) seed);


	vector<double> t(N);
	vector<double> y(N * M);

	// time
	for(j = 0; j < N; j++)
		t[j] = double(j) * dt;

	srand( (unsigned int) seed );
	for(i = 0; i < M; i++)
	{	// simulate eta
		vector<double> eta(2);
		vector<double> omega(2);
		omega[0]  = std::sqrt(omega2[0]);
		omega[1]  = std::sqrt(omega2[1]);
		eta[0]       = gsl_ran_gaussian(r, omega[0]);
		eta[1]       = gsl_ran_gaussian(r, omega[1]);
		// measurement values
		for(j = 0; j < N; j++)
		{	// simulate eps
			double sigma = std::sqrt(sigma2);
			double eps   = gsl_ran_gaussian(r, sigma);

			// measurement value
			y[i * N + j]   = (theta[0] + eta[0]) 
			               + (theta[1] + eta[1]) * t[j] 
			               + eps;

			// output data value
			dataFile << i+1          << "  ";
			dataFile << t[j]         << "  ";
			dataFile << y[i * N + j] << "  ";
			dataFile << endl;

		}
	}

	// negative log likelihood as QuasiNewton01Box function object 
	Fun obj(t, y, xLow, xUp);


	// call QuasiNewton01Box
	std::ostream &os    = std::cout;
	int level           = 1;
	size_t ItrMax       = 100;
	size_t QuadMax      = 20 * ItrMax;
	size_t n            = 5;
	double delta        = 1e-6;
	bool sOkCur         = false;
	size_t ItrCur       = 0;
	size_t QuadCur      = 0;
	size_t BfgsCur      = 0;
	double rCur         = .5;
	double fCur;
	QN01Box::Memory<double> dMemory(3 * n + n * n);
	double *xCur = dMemory(n);
	double *sCur = dMemory(n);
	double *gCur = dMemory(n);
	double *HCur = dMemory(n * n);
	for(j = 0; j < n; j++)
	{	xCur[j] = xScaled[j];
		sCur[j] = 0.;
		for(i = 0; i < n; i++)
			HCur[ i * n + j] = 0.;
		HCur[ j * n + j ] = 1.;
	}
	obj.function(xCur, fCur);
	obj.gradient(gCur);

	logFile << "Initialization"                << endl;
	logFile << "seed          = "     << seed  << endl;
	logFile << "M             = "     << M     << endl;
	logFile << "N             = "     << N     << endl;
	logFile << "dt            = "     << dt    << endl;
	logFile << "fCur          = " << fCur      << endl;
	logFile << "thetaTrue[0]  = " << theta[0]  << endl;
	logFile << "thetaTrue[1]  = " << theta[1]  << endl;
	logFile << "omega2True[0] = " << omega2[0] << endl;
	logFile << "omega2True[1] = " << omega2[1] << endl;
	logFile << "sigma2True    = " << sigma2    << endl;

	const char *msg = QN01Box::QuasiNewton01Box(
		os,
		level,
		ItrMax,
		QuadMax,
		n,
		delta,
		obj,
		sOkCur,
		ItrCur,
		QuadCur,
		BfgsCur,
		rCur,
		fCur,
		xCur,
		sCur,
		gCur,
		HCur
	);
	
	logFile << "msg = " << msg << endl;
	double lambda;
	logFile << "Optimization Results:" << endl;
	logFile << "msg          = " << msg       << endl;
	logFile << "fCur         = " << fCur      << endl;
	for(j = 0; j < 5; j++)
	{	lambda = xCur[j];
		x[j] = xLow[j] * (1. - lambda) + xUp[j] * lambda;
		if( lambda < .01 )
		{	logFile << "Variable with value "   << x[j]; 
			logFile << " is at its lower limit" << endl; 
		}
		if( .99 < lambda )
		{	logFile << "Variable with value "   << x[j]; 
			logFile << " is at its upper limit" << endl; 
		}
	}
	theta[0]   = x[0];
	theta[1]   = x[1];
	omega2[0]  = x[2];
	omega2[1]  = x[3];
	sigma2     = x[4];

	logFile << "thetaHat[0]  = " << theta[0]  << endl;
	logFile << "thetaHat[1]  = " << theta[1]  << endl;
	logFile << "omega2Hat[0] = " << omega2[0] << endl;
	logFile << "omega2Hat[1] = " << omega2[1] << endl;
	logFile << "sigma2Hat    = " << sigma2    << endl;

	return  0;
}
