/*
$begin bender_ok$$
$spell
	itr
	tmp
	gx
	gxx
$$
$latex \newcommand{\R}{{\bf R}}$$
$latex \newcommand{\N}{{\bf N}}$$
$latex \newcommand{\U}{{\bf U}}$$

$section Test Computation of Reduced Objective, Gradient, and Hessian$$

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
independent elements and $latex \U([0,1])$$ distribution.
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
          + \bar{x}_{i,0} * t_0^0 + \cdots + \bar{x}_{i,p-1} * t_{p-1}^{p-1}
\end{array}
\] $$ 
The likelihood matrix 
$latex \Psi : \R^{n \times p} \rightarrow \R^{m \times n}$$ is 
given by
$latex \[
\Psi (x)_{i,j} = \prod_{k=0}^{q-1}  \frac{1}{\sqrt{2 \pi} \sigma}
\exp \left[ - \frac{1}{2} \left( 
	z_{i,k} - x_{j,0} * t_0^0 - \cdots - x_{j,p-1} * t_{p-1}^{p-1} 
\right)^2 / \sigma^2 \right]
\] $$

$head Output File$$

$subhead Format$$
Example and test (see below) outputs a file called.
This file has the 
$xref/bender_ok/Test Case Parameters/test case parameter values/$$
followed by the following information:
$table
$code z$$ $cnext simulated data values $rnext
$code x$$ $cnext random effects matrix at we are evaluating objective $rnext
$code g$$ $cnext reduced objective function value $rnext
$code gx$$ $cnext gradient of reduced objective function value $rnext
$code gxx$$ $cnext Hessian of reduced objective function value 
$tend
For $latex i = 0 , \ldots , n-1$$ and $latex j = 0 , \ldots p-1$$
$latex \[
	gx( i * p + j, 0 ) = \partial_{x(i,j)} G(x)
\] $$
For 
$latex i = 0 , \ldots , n-1$$,
$latex j = 0 , \ldots p-1$$,
$latex k = 0 , \ldots , n-1$$, and
$latex l = 0 , \ldots , n-1$$
$latex \[
	gxx( i * p + j, k * p + l ) = \partial_{x(k,l)} \partial_{x(i,j)} G(x)
\] $$

$subhead Output$$
$code
$verbatim%cpp/bender_ok.out%5%$$
$$


$head Source Code$$
The following is the source code for this example use of $xref/Bender/$$:
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
	mu
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
# include <cppad/CppAD.h>
# include <non_par/bender.hpp>
# include <non_par/relaxed.hpp>

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

		// simulate the random effects uniform on [0, 1] 
		x_bar = rand(m, p);

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

		// for each measurement vector
		for(i = 0; i < m; i++)
		{	// initialize sum of negative log likelihoods
	  		sum = term;
			// for each measurement
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

template <class Like>
double objective(
	Like                          *like, 
	double                         mu  ,
	const mat2cpp::matrix<double> &x   , 
	mat2cpp::matrix<double>       &y   )
{	// std::cout << "start objective:" << std::endl;
	using namespace mat2cpp;
	size_t i, j, k;

	size_t n     = x.size1();
	size_t p     = x.size2();

	// determine m and evaluate Psi
	matrix<double> beta(1, p);
	for(k = 0; k < p; k++)
		beta(0, k) = x(0, k);
	matrix<double> psi = (*like)(beta);
	size_t m     = psi.size1();
	matrix<double> Psi(m, n);
	j = 0;
	while(j < n)
	{	for(i = 0; i < m; i++)
			Psi(i, j) = psi(i, 0);
		j++;
		if( j < n )
		{	for(k = 0; k < p; k++)
				beta(0, k) = x(j, k);
			psi = (*like)(beta);
		}
	}

	double eps         = 1e-13;
	size_t max_itr     = 20;
	matrix<double> option(2, 1);
	option(0, 0)       = eps;
	option(1, 0)       = double(max_itr);
	matrix<double> lam(n, 1);
	matrix<double> w(m, 1);
	matrix<double> info;

	// solve sub-problem
	double obj = non_par::relaxed(Psi, mu, option, lam, w, info);
	matrix<double> Psi_lam = prod(Psi, lam);
	matrix<double> PsiT_w  = prod(trans(Psi), w);

	// lam
	for(j = 0; j < n; j++)
		y(j, 0) = lam(j, 0);
	// w
	for(i = 0; i < m; i++)
		y(n+i, 0) = w(i, 0);
	// s = m - Psi^T * w
	for(j = 0; j < n; j++)
		y(n+m+j, 0) = m - PsiT_w(j, 0); 

	// std::cout << "end objective:" << std::endl;
	return obj;
}
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

} // END empty namespace

bool bender_ok(void)
{	bool ok = true;
	using namespace mat2cpp;
	using CppAD::NearEqual;

	size_t m     = 5;
	size_t n     = 4;
	size_t p     = 2;
	size_t q     = 3;
	double mu    = 1.;
	double sigma = .2;

	// likelihood evaluation constructor
	Like like(m, p, q, sigma);

	// chose value for random effects at random
	matrix<double> x = rand(n, p);

	// compute both objective and Y(x)
	matrix<double> y(n + m + n, 1);
	double obj = objective(&like, mu, x, y);

	matrix<double> g(1,       1);
	matrix<double> gx( n*p,   1);
	matrix<double> gxx(n*p, n*p);

	non_par::Bender(&like, mu, x, y, g, gx, gxx);

	// check value of objective function
	ok &= NearEqual(obj, g(0,0), 1e-12, 1e-12);

	// check first partials
	matrix<double> ytmp(n + m + n, 1);
	double step = 1e-5;
	double xij1, obj_p, obj_m, obj_x;
	size_t i1, j1, ell1;
	for(ell1 = 0; ell1 < n * p; ell1++)
	{	i1        = ell1 / p;
		j1        = ell1 - i1 * p;
		xij1      = x(i1, j1);
		x(i1, j1) = xij1 + step;
		obj_p     = objective(&like, mu, x, ytmp);
		x(i1, j1) = xij1 - step;
		obj_m     = objective(&like, mu, x, ytmp);
		obj_x     = (obj_p - obj_m) / (2 * step);
		ok       &= NearEqual(obj_x, gx(ell1, 0), 1e-6, 1e-6);
		x(i1, j1) = xij1;
	}

	// check second partials
	double xij2, obj_pp, obj_mm, obj_pm, obj_mp, obj_xx;
	size_t i2, j2, ell2;
	for(ell1 = 0; ell1 < n * p; ell1++)
        {       i1        = ell1 / p;
                j1        = ell1 - i1 * p;
		xij1      = x(i1, j1);
		for(ell2 = 0; ell2 < n; ell2++)
		{	i2        = ell2 / p;
			j2        = ell2 - i2 * p;
			xij2      = x(i2, j2);
			//
			x(i1, j1) = x(i1, j1) + step;
			x(i2, j2) = x(i2, j2) + step;
			obj_pp    = objective(&like, mu, x, ytmp);
			x(i2, j2) = x(i2, j2) - 2 * step;
			obj_pm    = objective(&like, mu, x, ytmp);
			x(i1, j1) = x(i1, j1) - 2 * step;
			obj_mm    = objective(&like, mu, x, ytmp);
			x(i2, j2) = x(i2, j2) + 2 * step;
			obj_mp    = objective(&like, mu, x, ytmp);
			//
			obj_xx    = (obj_pp - obj_pm - obj_mp + obj_mm);
			obj_xx   /= (4 * step * step);
			ok       &= NearEqual(
					obj_xx, gxx(ell1, ell2), 1e-3, 1e-3);
			x(i2, j2) = xij2;
		}
		x(i1, j1) = xij1;
	}

	// check evaluation of just the objective
	matrix<double> g_tmp(1,    1);
	matrix<double> empty(0,    0);
	non_par::Bender(&like, mu, x, y, g_tmp, empty, empty);
	ok &= NearEqual(g_tmp(0, 0), g(0, 0), 1e-12, 1e-12);

	// check evaluation of just objective and gradient
	matrix<double> gx_tmp(n*p, 1);
	non_par::Bender(&like, mu, x, y, g_tmp, gx_tmp, empty);
	for(ell1 = 0; ell1 < n * p; ell1++)
		ok &= NearEqual(gx_tmp(ell1, 0), gx(ell1, 0), 1e-12, 1e-12);

	// write results to bender_ok.out
	matrix<double> z = like.get_z();
	std::ofstream file;
	file.open("bender_ok.out", std::ios::trunc);
	file << "m      = " << m     << std::endl;
	file << "n      = " << n     << std::endl;
	file << "p      = " << p     << std::endl;
	file << "q      = " << q     << std::endl;
	file << "mu     = " << mu    << std::endl;
	file << "sigma  = " << sigma << std::endl;
	print(file, "z",    z);
	print(file, "x",    x);
	print(file, "g",    g);
	print(file, "gx",   gx);
	print(file, "gxx",  gxx);
	file.close();
	return ok;
}
/* $$
$end
*/
