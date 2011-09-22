/*
$begin npipp_ok.cpp$$
$spell
	bool
	cols
	cpp
	eps
	fabs
	hpp
	io
	itr
	namespace
	npipp
	qk
	roundoff
	std
	trans
	ublas
$$

$section Example and Test of npipp_of$$

$codep */
# include <mat2cpp.hpp>
# include <boost/numeric/ublas/io.hpp>
# include <boost/numeric/ublas/matrix_proxy.hpp>
# include <non_par/npipp_of.hpp>

namespace { // Begin empty namespace
bool npipp_one_ok()
{	// case where we can compute the analytic solution
	bool ok = true;
	using namespace mat2cpp;

	// case where solution is lam = [2/3, 1/3]. This can be determined 
	// by substituting lam(2) = 1 - lam(1) in Psi * lam and setting 
	// derivative of object w.r.t. lam equal to zero.
	size_t m       = 2;  // number of rows in Psi
	size_t n       = 3;  // number of columns in Psi (3rd column is zero)
	double eps     = 1e-12;
	matrix<double> Psi(m, n);
	Psi(0, 0) = 1.;  Psi(0, 1) = 0.; Psi(0, 2) = 0.;
	Psi(1, 0) = .25; Psi(1, 1) = 1.; Psi(1, 2) = 0.;
	matrix<double> option(1, 1);
	option(0, 0)   = eps;

	// values returned by npipp_of
	matrix<double> lam(n, 1), w(m, 1), info;

	// call to npipp_of
	npipp_of(Psi, option, lam, w, info);

	// number of iterations used
	size_t n_itr = info.size2() - 1;

	// compute y = m * 1_n - Psi' * w;
	matrix<double> y = 
		scalar_matrix<double> (n, 1, double(m)) - prod(trans(Psi), w);

	
	// check for feasible
	ok  &= min(lam) >= 0.;
	ok  &= min(w)   >= 0.;
	ok  &= min(y)   >= 0.;

	// check lambda
	ok  &= std::fabs(lam(0, 0) - 2./3.) < 1e-12;
	ok  &= std::fabs(lam(1, 0) - 1./3.) < 1e-12;

	// The duality gap is phi( Psi * lam ) + phi( w )
	// (It may be negative because of roundoff error.)
	double gap = sum( - log( prod(Psi, lam) ) ) + sum( - log(w) );
	ok  &= std::abs(gap) <= 1e-12;

	// check for descent of maximum residual measure (at each iteration)
	size_t k;
	for(k = 1; k < n_itr; k++)
	{	double q_last = info(0, k-1);
		double qk     = info(0, k);
		ok &= q_last > qk;
	}

	// check final convergence criteria
	ok &= info(0, n_itr) < eps;
	ok &= info(1, n_itr) < eps;

	return ok;
}
bool npipp_two_ok()
{	// case where n2 columns of Psi are nearly equal previous columns
	bool ok = true;
	using namespace mat2cpp;

	size_t m       = 20;       // number of rows in Psi
	size_t n       = 100;      // number of columns in Psi (must be even)
	size_t n2      = n / 2;
	double scale   = 1e-6;
	double eps     = 1e-12;

	matrix<double> A = rand(m, n2);
	matrix<double> E = rand(m, n2) * scale;

	// Psi = [ A , A + E ]
	matrix<double> Psi(m, n);
	slice rows;
	slice cols;
	rows = slice(0, 1,  m);
	cols = slice(0, 1, n2);
	matrix_slice_double (Psi, rows, cols) = A;
	cols = slice(n2, 1, n2);
	matrix_slice_double (Psi, rows, cols) = A + E;


	matrix<double> option(1, 1);
	option(0, 0)   = eps;

	// values returned by npipp_of
	matrix<double> lam(n, 1), w(m, 1), info;

	// call to npipp_of
	npipp_of(Psi, option, lam, w, info);

	// number of iterations used
	size_t n_itr = info.size2() - 1;

	// compute y = m * 1_n - Psi' * w;
	matrix<double> y = 
		scalar_matrix<double> (n, 1, double(m)) - prod(trans(Psi), w);

	
	// check for feasible
	ok  &= min(lam) >= 0.;
	ok  &= min(w)   >= 0.;
	ok  &= min(y)   >= 0.;

	// The duality gap is phi( Psi * lam ) + phi( w )
	// (It may be negative because of roundoff error.)
	double gap = sum( - log( prod(Psi, lam) ) ) + sum( - log(w) );
	ok  &= std::abs(gap) <= 1e-12;

	// check for descent of maximum residual measure (at each iteration)
	size_t k;
	for(k = 1; k < n_itr; k++)
	{	double q_last = info(0, k-1);
		double qk     = info(0, k);
		ok &= q_last > qk;
	}

	// check final convergence criteria
	ok &= info(0, n_itr) < eps;
	ok &= info(1, n_itr) < eps;

	return ok;
}
} // End empty namespace

bool npipp_ok()
{	bool ok = true;
	ok &= npipp_one_ok();
	ok &= npipp_two_ok();
	return ok;
}
/* $$
$end
*/

