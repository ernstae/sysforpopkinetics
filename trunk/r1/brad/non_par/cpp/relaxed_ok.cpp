/*
$begin relaxed_ok.cpp$$
$spell
	obj
	non_par
	bool
	cols
	cpp
	eps
	fabs
	hpp
	io
	itr
	namespace
	qk
	roundoff
	std
	trans
	ublas
$$

$section Example and Test of relaxed$$

$codep */
# include <mat2cpp.hpp>
# include <boost/numeric/ublas/io.hpp>
# include <boost/numeric/ublas/matrix_proxy.hpp>
# include <non_par/relaxed.hpp>

namespace { // Begin empty namespace
bool relaxed_one_ok()
{	// case where we can compute the analytic solution
	bool ok = true;
	using namespace mat2cpp;

	// case where solution is lam = [2/3, 1/3]. This can be determined 
	// by substituting lam(2) = 1 - lam(1) in Psi * lam and setting 
	// derivative of object w.r.t. lam equal to zero.
	size_t m       = 2;  // number of rows in Psi
	size_t n       = 3;  // number of columns in Psi (3rd column is zero)
	double eps     = 1e-12; // convergence criteria
	size_t max_itr = 20;    // maximum number of iterations
	double t       = 0.;    // relaxation parameter
	matrix<double> Psi(m, n);
	Psi(0, 0) = 1.;  Psi(0, 1) = 0.; Psi(0, 2) = 0.;
	Psi(1, 0) = .25; Psi(1, 1) = 1.; Psi(1, 2) = 0.;
	matrix<double> option(2, 1);
	option(0, 0)   = eps;
	option(1, 0)   = double(max_itr);

	// values returned by relaxed
	matrix<double> lam(n, 1), w(m, 1), info;

	// call to relaxed
	non_par::relaxed(Psi, t, option, lam, w, info);
	if( info.size1() == 0 )
	{	ok = false;
		return ok;
	}

	// number of iterations used
	size_t n_itr = info.size1() - 1;

	// compute s = m * 1_n - Psi' * w;
	matrix<double> s = 
		scalar_matrix<double> (n, 1, double(m)) - prod(trans(Psi), w);

	// maximum absolute element of Psi
	double Psi_max = max(Psi);

	
	// check for feasible
	ok  &= min(lam) >= 0.;
	ok  &= min(w)   >= 0.;
	ok  &= min(s)   >= 0.;

	// check lambda
	ok  &= std::fabs(lam(0, 0) - 2./3.) < 1e-12;
	ok  &= std::fabs(lam(1, 0) - 1./3.) < 1e-12;

	// The duality gap is phi( Psi * lam ) + phi( w )
	// (It may be negative because of roundoff error.)
	double gap = sum( - log( prod(Psi, lam) ) ) + sum( - log(w) );
	ok  &= std::abs(gap) <= 2 * m * eps * Psi_max;

	// check for descent of maximum residual measure (at each iteration)
	size_t k;
	for(k = 1; k < n_itr; k++)
	{	double q_last = info(k-1, 1);
		double qk     = info(k, 1);
		ok &= q_last > qk;
	}

	// check final convergence criteria
	matrix<double> Psi_lam = prod(Psi, lam);
	for(k = 0; k < n; k++)
		ok &= std::fabs( lam(k,0) * s(k,0) - t ) <= eps * Psi_max;
	for(k = 0; k < m; k++)
		ok &= std::fabs( w(k,0) * Psi_lam(k,0) - 1. ) <= eps * Psi_max;

	// ====================================================================
	// case where t is not zero
	t = 1e-1;
	double obj = non_par::relaxed(Psi, t, option, lam, w, info);

	// number of iterations used
	n_itr = info.size1() - 1;

	// compute s = m * 1_n - Psi' * w;
	s = scalar_matrix<double> (n, 1, double(m)) - prod(trans(Psi), w);

	// check for feasible
	ok  &= min(lam) >= 0.;
	ok  &= min(w)   >= 0.;
	ok  &= min(s)   >= 0.;

	// check for descent of maximum residual measure (at each iteration)
	for(k = 1; k < n_itr; k++)
	{	double q_last = info(k-1, 1);
		double qk     = info(k, 1);
		ok &= q_last > qk;
	}

	// check final convergence criteria
	Psi_lam = prod(Psi, lam);
	for(k = 0; k < n; k++)
		ok &= std::fabs( lam(k,0) * s(k,0) - t ) <= eps * Psi_max;
	for(k = 0; k < m; k++)
		ok &= std::fabs( w(k,0) * Psi_lam(k,0) - 1. ) <= eps * Psi_max;

	// check objective function value
	double check = -double(m);
	for(k = 0; k < n; k++)
	{	check += double(m) * lam(k, 0);
		check -= t * log( lam(k, 0) );
	}
	for(k = 0; k < m; k++)
		check -= log( Psi_lam(k, 0) );
	ok &= std::fabs( obj - check ) <= eps * Psi_max;

	return ok;
}
bool relaxed_two_ok()
{	// case where n2 columns of Psi are nearly equal previous columns
	bool ok = true;
	using namespace mat2cpp;

	size_t m       = 20;       // number of rows in Psi
	size_t n       = 100;      // number of columns in Psi (must be even)
	size_t n2      = n / 2;
	double scale   = 1e-6;
	double eps     = 1e-12;
	size_t max_itr = 20;
	double t       = 0.;

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


	matrix<double> option(2, 1);
	option(0, 0)   = eps;
	option(1, 0)   = double(max_itr);

	// values returned by relaxed
	matrix<double> lam(n, 1), w(m, 1), info;

	// call to relaxed
	non_par::relaxed(Psi, t, option, lam, w, info);
	if( info.size1() == 0 )
	{	ok = false;
		return ok;
	}

	// number of iterations used
	size_t n_itr = info.size1() - 1;

	// compute s = m * 1_n - Psi' * w;
	matrix<double> s = 
		scalar_matrix<double> (n, 1, double(m)) - prod(trans(Psi), w);

	// maximum absolute element of Psi
	double Psi_max = max(Psi);
	
	// check for feasible
	ok  &= min(lam) >= 0.;
	ok  &= min(w)   >= 0.;
	ok  &= min(s)   >= 0.;

	// The duality gap is phi( Psi * lam ) + phi( w )
	// (It may be negative because of roundoff error.)
	double gap = sum( - log( prod(Psi, lam) ) ) + sum( - log(w) );

	// need to multiply by two (very close to satisfied)
	ok  &= std::abs(gap) <= 2 * m * eps * Psi_max;

	// check for descent of maximum residual measure (at each iteration)
	size_t k;
	for(k = 1; k < n_itr; k++)
	{	double q_last = info(k-1, 1);
		double qk     = info(k, 1);
		ok &= q_last > qk;
	}

	// check final convergence criteria
	matrix<double> Psi_lam = prod(Psi, lam);
	for(k = 0; k < n; k++)
		ok &= std::fabs( lam(k,0) * s(k,0) - t ) <= eps * Psi_max;
	for(k = 0; k < m; k++)
		ok &= std::fabs( w(k,0) * Psi_lam(k,0) - 1. ) <= eps * Psi_max;

	// ====================================================================
	// case where t is not zero
	t = 1e-1;
	non_par::relaxed(Psi, t, option, lam, w, info);

	// number of iterations used
	n_itr = info.size1() - 1;

	// compute s = m * 1_n - Psi' * w;
	s = scalar_matrix<double> (n, 1, double(m)) - prod(trans(Psi), w);

	// check for feasible
	ok  &= min(lam) >= 0.;
	ok  &= min(w)   >= 0.;
	ok  &= min(s)   >= 0.;

	// check for descent of maximum residual measure (at each iteration)
	for(k = 1; k < n_itr; k++)
	{	double q_last = info(k-1, 1);
		double qk     = info(k, 1);
		ok &= q_last > qk;
	}

	// check final convergence criteria
	Psi_lam = prod(Psi, lam);
	for(k = 0; k < n; k++)
		ok &= std::fabs( lam(k,0) * s(k,0) - t ) <= eps * Psi_max;
	for(k = 0; k < m; k++)
		ok &= std::fabs( w(k,0) * Psi_lam(k,0) - 1. ) <= eps * Psi_max;

	return ok;
}
} // End empty namespace

bool relaxed_ok()
{	bool ok = true;
	ok &= relaxed_one_ok();
	ok &= relaxed_two_ok();
	return ok;
}
/* $$
$end
*/

