/*
$begin opt_measure_ok$$
$spell
	itr
	fabs
	Youngcheong
	endif
	strcmp
	xjk
	mu
	tmp
	gx
	gxx
$$
$latex \newcommand{\R}{{\bf R}}$$
$latex \newcommand{\N}{{\bf N}}$$
$latex \newcommand{\U}{{\bf U}}$$

$section Test Computation of Optimal Measure$$

$head Test Case Parameters$$
$table
$latex m$$       $cnext number of individuals in the study      $rnext
$latex p$$       $cnext number of random effects per individual $rnext
$latex q$$       $cnext number of measurements per individual   $rnext
$latex \sigma$$  $cnext standard deviation of measurement noise 
$tend

$head Individual Model$$
The random effects matrix $latex \bar{x} \in \R^{m \times p}$$ 
is simulated with
independent elements and $latex \U([-.5,.5])$$ distribution.
The measurement noise matrix $latex e \in \R^{m \times q}$$ 
is simulated with
independent elements and $latex \N (0, \sigma^2 )$$ distribution.
The measurement time vector $latex t \in \R^q$$.
and measurement value matrix $latex z \in \R^{m \times q}$$ are given by
$latex \[
\begin{array}{rcl}
t_k     & = & (k + 1) / q 
\\
z_{i,k} & = & e_{i,k} 
          + \bar{x}_{i,0} * t_k^0 + \cdots + \bar{x}_{i,p-1} * t_k^{p-1}
\end{array}
\] $$ 
The likelihood matrix 
$latex \Psi : \R^{n \times p} \rightarrow \R^{m \times n}$$ is 
given by
$latex \[
\Psi (x)_{i,j} = \prod_{k=0}^{q-1}  \frac{1}{\sqrt{2 \pi} \sigma}
\exp \left[ - \frac{1}{2} \left( 
	z_{i,k} - x_{j,0} * t_k^0 - \cdots - x_{j,p-1} * t_k^{p-1} 
\right)^2 / \sigma^2 \right]
\] $$
We define the non-parametric population objective function
$latex F : \R^{n \times p} \times \R_+^n \rightarrow \R$$ by
$latex \[
F(x, \lambda) = - \sum_{i=1}^m 
	\log \left[ \sum_{j=1}^n \Psi ( x )_{i,j} \lambda_j \right]
\] $$
This is a test that $cref/opt_measure/$$ optimizes this function
under the constraint that $latex x_{j,p} \in [ -.5 , +.5 ] $$
and that $latex \lambda_0 + \ldots + \lambda_{n-1} = 1$$.


$head Output File$$
The solution of the relaxed optimization problem, with
the discrete measure points constrained to be between zero and one,
is:
$code
$verbatim%cpp/opt_measure_ok.out%5%$$
$$


$head Source Code$$
The following is the source code for this example use of 
$xref/opt_measure/$$:
$spell	
	ytmp
	cassert
	hpp
	ublas
	io
	iostream
	fstream
	typedef
	cmath
	randn
	trans
	eps
	tpow
	cout
	atan
	matrix matrix
	res
	exp
	const
	cpp
	namespace
	bool
	Like like
	non_par
	mp
	mm
	obj
	xij
	std
	ofstream
	ios
	trunc
	endl
$$
$codep */

# include <mat2cpp.hpp>
# include <boost/numeric/ublas/io.hpp>
# include <cassert>
# include <cmath>
# include <iostream>
# include <fstream>
# include <CppAD/CppAD.h>

# include <non_par/relaxed.hpp>
# include <non_par/opt_measure.hpp>

namespace  {  // define class Like in empty namespace
class Like {
	typedef mat2cpp::matrix<double> Matrix;
private:
	// Test case parameters
	const size_t m;
	const size_t p;
	const size_t q; 
	const double sigma;

	// simulated values
	Matrix x_bar;   // random effects for each individual
	Matrix t;       // measurement times 
	Matrix z;       // measurement values
public:
	// constructor
	Like(size_t m_, size_t p_, size_t q_, double sigma_)
	: m(m_), p(p_), q(q_), sigma(sigma_), 
	x_bar(m, p), t(1, q), z(m, q)
	{	// std::cout << "start Like constructor" << std::endl;
		using namespace mat2cpp;
		size_t i, j, k;

		// simulate the random effects uniform on [-1, 1] 
		x_bar = rand(m, p) - scalar_matrix<double>(m, p, .5);

		// measurement noise
		Matrix noise = randn(m, q);
		noise       *= sigma;

		// measurement times
		for(k = 0; k < q; k++)
			t(0, k) = double(k + 1) / double(q);

		// for each individual in the study
		for(i = 0; i < m; i++)
		{	// for each measurement for this individual
			for(k = 0; k < q; k++)
			{	double tpow = 1.;
				double mean = 0.;
				// for each random effect value
				for(j = 0; j < p; j++)
				{	mean += x_bar(i, j) * tpow;
					tpow *= t(0, k);
				}
				z(i, k) = mean + noise(i, k);
			}
		}
		// std::cout << "end Like constructor" << std::endl;
	}
	// like(beta)
	template <class Type> mat2cpp::matrix<Type> operator()
		(const mat2cpp::matrix<Type> &beta) const
	{	// std::cout << "start like(beta)" << std::endl;
		using namespace mat2cpp;
		using std::log;
		size_t i, j, k;

		// number of parameters per individual
		assert( beta.size2() == p );
		assert( beta.size1() == 1 );

		// a universal constant
		double pi = 4. * std::atan(1.);

		// constant term in the negative log likelihood
		double term = q * (log(2. * pi) / 2. + log(sigma)); 

		// return matrix
		matrix<Type> psi(m, 1);

		// some values used in the inner loop
		Type sum, mean, res; 

		// for each individual in the study
		for(i = 0; i < m; i++)
		{	// initialize sum of negative log likelihoods
	  		sum = term;
			// for each measurement for this individual
			for(k = 0; k < q; k++)
			{	double tpow = 1.;
				mean = 0.;
				// for each random effect
				for(j = 0; j < p; j++)
				{	mean += beta(0, j) * tpow;
					tpow *= t(0, k);
				}
				res  = (z(i, k) - mean) / sigma;
				sum += res * res / 2.;
			}
			// value of likelihood
			psi(i, 0) = exp(- sum);
		}
		// std::cout << "end like(x)" << std::endl;
 		return psi;
	}
	mat2cpp::matrix<double> get_z(void) const
	{	return z; }
};

void print(std::ofstream &file, 
	const char *name, const mat2cpp::matrix<double> data )
{	size_t i, j;
        for(i = 0; i < data.size1(); i++)
        {       file << name << "(" << i << ",:) =";
                for(j = 0; j < data.size2(); j++)
                        file << " " << data(i, j);
                file << std::endl;
        }
	return;
}

mat2cpp::matrix<double> Psi_of(
	Like *like, size_t m, const mat2cpp::matrix<double> &X
)
{	size_t i, j, k;
	using mat2cpp::matrix;

	size_t n = X.size1();
	size_t p = X.size2();

	matrix<double> Psi(m, n);
	matrix<double> beta(1, p);
	matrix<double> psi(m, 1);
	for(j = 0; j < n; j++)
	{	for(k = 0; k < p; k++)
			beta(0, k) = X(j, k); 
		psi = (*like)(beta);
		for(i = 0; i < m; i++)
			Psi(i, j) = psi(i, 0);
	}
	return Psi;
}

} // END empty namespace

bool opt_measure_ok(void)
{	bool ok = true;
	using namespace mat2cpp;
	using CppAD::NearEqual;

	size_t j, k;

	size_t level   = 0;
	mat2cpp::matrix<size_t> max_itr(2, 1);
	max_itr(0, 0)  = 20;
	max_itr(1, 0)  = 20;
	mat2cpp::matrix<double> epsilon(5, 1);
	epsilon(0, 0)  = 1e-4;
	epsilon(1, 0)  = 1e-4;
	epsilon(2, 0)  = 1e-4;
	epsilon(3, 0)  = 1e-13;
	epsilon(4, 0)  = 1./4.;
	size_t m       = 4;
	size_t n       = 8;
	size_t p       = 2;
	size_t q       = 5;
	double sigma   = .2;

	// likelihood evaluation constructor
	Like like(m, p, q, sigma);

	matrix<double> xLow(1, p), xUp(1, p), X(n, p), lambda(n, 1);

	for(k = 0; k < p; k++)
	{	xLow(0, k) = -.5;
		xUp(0, k)  = +.5;
		X          = rand(n, p) - scalar_matrix<double>(n, p, .5);
	}

	matrix<double> info;
	const char *msg = non_par::opt_measure(level, max_itr, epsilon, 
		&like, m, xLow, xUp, X, lambda, info
	);
	ok &= strcmp(msg, "ok") == 0;

	// value of n returned by opt_measure
	assert( X.size2() == p );
	n = X.size1();

	ok &= std::abs(1. - sum(lambda)) <= epsilon(0, 0);

	// measurement matrix
	matrix<double> z = like.get_z();

	// relaxation factor
	double mu = 1e-10;

	// relaxed problem convergence criteria
	matrix<double> option(2, 1);
	option(0, 0) = epsilon(0, 0) * 1e-6;
	option(1, 0) = 20.;   // maximum number of iterations

	// values returned by relaxed
	matrix<double> lam(n, 1);
	matrix<double> w(m, 1);

	// optimal value of the relaxed objective 
	matrix<double> relaxed_info;
	matrix<double> Psi = Psi_of(&like, m, X);
	double obj = non_par::relaxed(Psi, mu, option, lam, w, relaxed_info);

	// check lambda
	for(j = 0; j < n; j++)
		ok &= (fabs(lambda(j, 0) - lam(j, 0)) <= 1e-4 );

	// for each vector of random effects
	double step = .01;
	for(j = 0; j < n; j++)
	{	// for each component of the random effects vector
		for(k = 0; k < p; k++)
		{	double xjk = X(j, k);
			if( xjk + step <= xUp(0, k) )
			{	X(j, k) = xjk + step;
				Psi = Psi_of(&like, m, X);
				double tmp = non_par::relaxed(
					Psi, mu, option, lam, w, relaxed_info
				);
				ok &= tmp >= obj;
			}
			if( xLow(0, k) <= xjk - step )
			{	X(j, k) = xjk - step;
				Psi = Psi_of(&like, m, X);
				double tmp = non_par::relaxed(
					Psi, mu, option, lam, w, relaxed_info
				);
				ok &= tmp >= obj;
			}
			X(j, k) = xjk;
		}
	}

	// write results to opt_measure_ok.out
	std::ofstream file;
	file.open("opt_measure_ok.out", std::ios::trunc);
	file << "Number of individuals:" << std::endl;
	file << "m      = " << m     << std::endl;
	file << "Number of discrete points in measure:" << std::endl;
	file << "n      = " << n     << std::endl;
	file << "Number of random effects per individual:" << std::endl;
	file << "p      = " << p     << std::endl;
	file << "Number of measurements per individual:" << std::endl;
	file << "q      = " << q     << std::endl;
	file << "Standard deviation of measurement noise:" << std::endl;
	file << "sigma  = " << sigma << std::endl;
	file << "Measurement values:" << std::endl;
	print(file, "z",    z);
	file << "Optimal location of discrete measure points:" << std::endl;
	print(file, "x",    X);
	file << "Optimal weights for discrete measure points:" << std::endl;
	print(file, "lambda",    lambda);
	file << "Optimization convergence:" << std::endl;
	print(file, "info", info);
	file.close();

	return ok;
}
/* $$
$end
*/
