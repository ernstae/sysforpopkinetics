/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
/*************************************************************************
 *
 * File: spk_non_par_test.cpp
 *
 *
 * Unit test for the function spk_non_par.
 *
 * Author: Brad Bell
 *
 *************************************************************************/
/*
Spk Non-parametric Example and Test Case

sigma : standard deviation of the measurement noise
q     : number of measurements for each individual
M     : number of individuals in the population study
m     : number of fixed effects
n     : number of random effects in model
J     : number of discrete measure points
bSim  : simulated true value of the random effects for each subject
        for i = 0 , ... , M-1,
        bSim(i,:) is uniformly distributed on [-.5, .5].
eSim  : simulate value for measurement noise 
        for i = 0 , ... , M-1 and k = 0 , ... , q-1
        eSim(i, k) is N(0, sigma^2 ) distributed.
tSim  : time corresponding to each simulated measurement value
      : for k = 0 , ... , q-1, tSim(k) = (k + 1) / q
ySim  : simulate measurement values
        for i = 0 , ... , M-1 and k = 0 , ... , q-1
        ySim(i, k) = e(i, k) + bSim(i, 0) + bSim(i, 1) * tSim(k) 
                   + ... + bSim(i, q-1) * tSim(k)^(q-1)
*/
// *************************************************************************
# include <cmath>
# include <string>
# include <CppAD/CppAD.h>
# include <gsl/gsl_rng.h>
# include <gsl/gsl_randist.h>
# include <cppunit/TestSuite.h>
# include <cppunit/TestCaller.h>

# include "../../../spk/SpkModel.h"
# include "../../../spk/spk_non_par.h"
# include "../../../spk/WarningsManager.h"

# include "spk_non_par_test.h"
// *************************************************************************
namespace { // [Begin: unnamed namespace]

// -------------------------------------------------------------------------
template <class Scalar>
class polynomial_model : public SpkModel<Scalar>
{
	typedef std::valarray<double>   vector;
	typedef std::valarray<Scalar > Svector;
private:
	// constants
	const size_t q_;
	const size_t m_;
	const size_t M_;
	const size_t n_;
	const double sigma_;
	const bool   fit_variance_;   // This determines if the variance parameter is fit or not.
	vector       t_;
	// current state of the model
	size_t   i_;
	Svector alpha_;
	Svector b_;
public:
	polynomial_model(
		double sigma      , 
		size_t q          , 
		size_t m          , 
		size_t M          , 
		size_t n          , 
		bool fit_variance ) 
	: sigma_(sigma), q_(q), m_(m), M_(M), n_(n), t_(q)
	, alpha_(m), b_(n), fit_variance_(fit_variance)
	{	size_t k;
		for( k = 0; k < q_; k++)
			t_[k] = (k + 1.) / double(q);
	}; 
	~polynomial_model(void)
	{};
private:
	void doSetPopPar(const Svector &alpha)
	{	assert(alpha.size() == m_);
        	alpha_ = alpha;
	}
	void doSelectIndividual(int i)
	{	assert( ( 0 <= i ) & ( i < M_ ) ); 
		i_ = i;
	}
	void doSetIndPar(const  Svector &b)
	{	assert(b.size() == n_);
		b_ = b;
	}
	void doIndParVariance(Svector &D_out) const
	{	assert( D_out.size() == n_ * n_ );
		// not needed ?
		assert(0);
	}
	void doDataMean(Svector &f_out) const
	{	assert( f_out.size() == q_ );	
		size_t j, k, n_amplitude;
		if( fit_variance_ )
			n_amplitude = n_ - 1;
		else	n_amplitude = n_;
		for(k = 0; k < q_; k++)
		{	double   tpow = 1.;
			Scalar   sum  = 0.;
			for(j = 0; j < n_amplitude; j++)
			{	sum += b_[j] *  tpow;
				tpow *= t_[k];
			}
			f_out[ k ] = sum;
		}
	}
	void doDataVariance(Svector &R_out) const
	{	assert( R_out.size() == q_ * q_ );
		size_t k1, k2;
		for(k1 = 0; k1 < q_; k1++)
		{	for(k2 = 0; k2 < q_; k2++)
				R_out[ k1 * q_ + k2 ] = 0.;

			// If the variance parameter is being fit,
			// then use it to calculate the variance.
			if(fit_variance_)
				R_out[k1*q_ + k1] = exp( b_[n_-1] );
			else
				R_out[k1*q_ + k1] = sigma_ * sigma_;
		}
    	}
	// functions that should not be pure virtual but should rather have
	// a default definition that throws an exception
	bool doDataMean_indPar(std::valarray<double> &) const
	{	assert(0); }
	bool doDataVariance_indPar(std::valarray<double> &) const
	{	assert(0); }
};
// -------------------------------------------------------------------------
enum distribution { UNIFORM_01, GAUSSIAN_01 }; 
double simulate(distribution d)
{	static gsl_rng *rng   = 0;

	if( rng == 0 )
	{	const gsl_rng_type *T  = gsl_rng_default;
		rng                    = gsl_rng_alloc(T);
		unsigned long int seed = 1234;
		gsl_rng_set(rng, seed);
		assert( rng != 0 );
	}

	double return_value;
	switch(d)
	{	case UNIFORM_01:
		return_value = gsl_ran_flat(rng, 0., 1.);
		break;

		case GAUSSIAN_01:
		return_value = gsl_ran_gaussian(rng, 1.); 
		break;

		default:
		assert(0);
	}
	return return_value;
}
// -------------------------------------------------------------------------
void p_y_given_b(
	polynomial_model<double> &model   ,
	DoubleMatrix             &y       ,
	DoubleMatrix             &N       ,
	DoubleMatrix             &B       ,
	DoubleMatrix             &pout    )
{	typedef std::valarray<double> vector;

	size_t n  = B.nr();
	size_t J  = B.nc();
	size_t M  = N.nr();
	assert( pout.nr() == M );
	assert( pout.nc() == J );
	double pi = 4. * std::atan(1.);

	size_t i, j, k, ell;
	double residual, variance;

	double *p_ptr = pout.data();
	double *B_ptr = B.data();

	for(j = 0; j < J; j++)
	{	// move data from B for this discrete measurement point
		vector b(n);
		for(k = 0; k < n; k++)
			b[k] = B_ptr[k + j * n];

		double *y_ptr = y.data();
		for(i = 0; i < M; i++)
		{	model.selectIndividual(i);
			model.setIndPar(b);
			size_t Ni = size_t( *(N.data() + i) );
			vector f(Ni);
			model.dataMean(f);
			vector R(Ni * Ni);
			model.dataVariance(R);
			double sum = 0.;
			for(k = 0; k < Ni; k++)
			{	for(ell = 0; ell < Ni; ell++)
				if( k != ell )
					assert( R[ k * Ni + ell] == 0. );
				residual = (*y_ptr++) - f[k];
				variance = R[ k * Ni + k ];
				sum     += residual * residual / variance;
				sum     += log( 2 * pi * variance); 
			}
			p_ptr[ i + j * M ] = exp( - sum / 2. );
		}
	}
}
double objective(
	polynomial_model<double> &model   ,
	DoubleMatrix             &y       ,
	DoubleMatrix             &N       ,
	DoubleMatrix             &B       ,
	DoubleMatrix             &lam     )
{	typedef std::valarray<double> vector;
	size_t n  = B.nr();
	size_t J  = B.nc();
	size_t M  = N.nr();
	DoubleMatrix p(M, J);
	p_y_given_b(model, y, N, B, p);

	size_t i, j;
	double *lam_ptr = lam.data();
	double *p_ptr   = p.data();
	double objective = 0;
	for(i = 0; i < M; i++)
	{	double sum = 0;
		for(j = 0; j < J; j++)
			sum += lam_ptr[j] * p_ptr[ i + j * M ];
		objective += - log( sum );
	}
	return objective;
}
//--------------------------------------------------------------------
void one_fit(
	double sigma      , 
	size_t q          , 
	size_t m          , 
	size_t M          , 
	size_t n          , 
	size_t J          , 
	bool fit_variance )
{
	// ------------------- level ----------------------
	// mod(level, 10) is level  in spk_non_par
	// (level / 10) is level in QuasiNewton01Box
	size_t level = 00; 

	// ------------------- model ---------------------
	// sigma      = measure noise standard deviation
	// size_t q   = number of measurements per individual
	// size_t m   = number of fixed effects (not used)
	// size_t M   = number of individuals in the population
	// size_t n   = number of random effects per individual
	// size_t J   = number of discret measure points 

	// define ADdouble, vector, and ADvector
	typedef CppAD::AD<double>       ADdouble;
	typedef std::valarray<double>     vector;
	typedef std::valarray<ADdouble> ADvector;

	// some temporary indices
	size_t i, j, k;

	polynomial_model<double>       model(sigma, q, m, M, n, fit_variance);
	polynomial_model< ADdouble > admodel(sigma, q, m, M, n, fit_variance);

	// value for alpha (not used)
	ADvector adalpha(m);
	adalpha[0] = 0.;
	admodel.setPopPar(adalpha);
	vector     alpha(m);
	alpha[0]   = 0.;
	model.setPopPar(alpha);

	// ---------------------- N -----------------------
	DoubleMatrix N(M, 1);
	double *ptr = N.data();
	for(i = 0; i < M; i++)
		ptr[i] = q;

	// ---------------------- y ------------------------
	// random effects vector
	vector b(n);
	// data mean given random effects
	vector f(q);
	// simulate the data set
	DoubleMatrix y( M * q, 1);
	ptr = y.data();
	for(i = 0; i < M; i++)
	{	// simulate data for this individual
		model.selectIndividual(i);

		// simulate true random effects for this subject
		for(k = 0; k < n; k++)
			b[k] = simulate(GAUSSIAN_01);

		model.setIndPar(b);

		// data mean plus noise
		model.dataMean(f);
		for(k = 0; k < q; k++)
		{	// mean zero variance one Gaussian
			*ptr++ = f[k] + sigma * simulate(GAUSSIAN_01);
		}
	}

	// --------------------- max_itr ---------------------
	DoubleMatrix max_itr(2, 1);
	ptr = max_itr.data();
	ptr[0] = 100.;
	ptr[1] = 100.;

	// --------------------- epsilon ---------------------
	DoubleMatrix epsilon(5, 1);
	ptr = epsilon.data();
	if( fit_variance )
		ptr[0] = 1e-3;
	else	ptr[0] = 1e-4;
	ptr[1] = 1e-4;
	ptr[2] = 1e-4;
	ptr[3] = 1e-13;
	ptr[4] = 1./4.;

	// ----------------------- blow ----------------------
	DoubleMatrix blow(n, 1);
	ptr = blow.data();
	for(i = 0; i < n; i++)
		ptr[i] = -.5;

	// If the variance parameter is being fit,
	// set its lower limit differently
	if(fit_variance)
		ptr[n - 1] = log(1e-4);

	// ----------------------- bup -----------------------
	DoubleMatrix bup(n, 1);
	ptr = bup.data();
	for(i = 0; i < n; i++)
		ptr[i] = +.5;

	// If the variance parameter is being fit,
	// set its upper limit differently
	if(fit_variance)
		ptr[n - 1] = log(1e+4);

	// ----------------------- Bin -----------------------
	DoubleMatrix Bin(n, J);
	ptr = Bin.data();
	for(j = 0; j < J; j++)
	{	for(i = 0; i < n; i++)
		{	// uniform distribution on [-.5, +.5]
			// (include the variance parameter)
			ptr[j * n + i] = simulate(UNIFORM_01) - .5;
		}
	}

	// ----------------------- Bout ----------------------
	DoubleMatrix Bout;

	// ---------------------- lamout ---------------------
	DoubleMatrix lamout;

	// ----------------------- pout ----------------------
	DoubleMatrix pout;

	// ------------------------------------------------------------------
	try
	{	spk_non_par(
			level   ,
			admodel ,
			model   , 
			N       , 
			y       , 
			max_itr ,
			epsilon , 
			blow    , 
			bup     , 
			Bin     , 
			Bout    , 
			lamout  , 
			pout    
		);

	}
  	catch( const SpkException& e )
	{	CPPUNIT_ASSERT_MESSAGE( 
		"polynomial_fit_test: failed for a known reason.", 
		false 
		);
  	}
  	catch( ... )
	{	CPPUNIT_ASSERT_MESSAGE( 
		"polynomial_fit_test: failed for an unknown reason.", 
		false 
		);
  	}
	// value of J returned by spk_non_par
	J = Bout.nc(); 
	CPPUNIT_ASSERT_MESSAGE(
		"polynomial_fit_test: dimension error.",
		Bout.nr()   == n && 
		lamout.nr() == J && 
		lamout.nc() == 1 &&
		pout.nr()   == M &&
		pout.nc()   == J
	);

	DoubleMatrix check_pout(M, J);
	p_y_given_b(model, y, N, Bout, check_pout);
	bool ok = true;
	double *check = check_pout.data();
	ptr           = pout.data();
	for(i = 0; i < M; i++) 
	{	for(j = 0; j < J; j++)
		{	ok &= CppAD::NearEqual(
				ptr[i+j*M], check[i+j*M], 1e-10, 1e-10
			);
		}
	}
	CPPUNIT_ASSERT_MESSAGE(
		"polynomial_fit_test: error in pout.",
		ok
	);

	// check Bout
	ptr = Bout.data();
	double step = 1e-2;
	double obj  = objective(model, y, N, Bout, lamout);
	double eps  = *(epsilon.data() + 0);
	for(j = 0; j < J; j++)
	{	for(k = 0; k < n; k++)
		{	double bkj = ptr[k+j*n];
			ok        &= *(blow.data() + k) <= bkj;
			ok        &= bkj <= *(bup.data() + k);
			ptr[k+j*n] = bkj - step;
			if( ptr[k+j*n] >= *(blow.data() + k) )
			{	double obj_m = 
				objective(model, y, N, Bout, lamout);
				ok          &= obj_m >= obj - eps;
			}
			ptr[k+j*n] = bkj + step;
			if( ptr[k+j*n] <= *(bup.data() + k) )
			{	double obj_p = 
				objective(model, y, N, Bout, lamout);
				ok          &= obj_p >= obj - eps;
			}
			ptr[k+j*n] = bkj;
		}
	}
	CPPUNIT_ASSERT_MESSAGE(
		"polynomial_fit_test: Bout is not optimal.",
		ok
	);

	// Check lamout
	ptr = lamout.data();
	size_t jmax = 0;
	double sum  = 0.;
	for(j = 0; j < J; j++)
	{	sum += ptr[j];
		if( ptr[j] > ptr[jmax] )
			jmax = j;
	}
	ok &= CppAD::NearEqual(sum, 1., eps, eps);
	double lam_max = ptr[jmax];
	step           = lam_max * 1e-2;
	for(j = 0; j < J; j++) if( j != jmax )
	{	double lam_j = ptr[j];	
		ok       &= 0. <= lam_j;
		ok       &= lam_j <= 1;
		ptr[j]    = lam_j - step;
		ptr[jmax] = lam_max + step;
		if( 0. <= ptr[j] )
		{	double obj_m = objective(model, y, N, Bout, lamout);
			ok          &= obj_m >= obj - eps;
		}
		ptr[j] = lam_j + step;
		ptr[jmax] = lam_max - step;
		if( ptr[j] <= 1. )
		{	double obj_p = objective(model, y, N, Bout, lamout);
			ok          &= obj_p >= obj - eps;
		}
		ptr[j]    = lam_j;
		ptr[jmax] = lam_max;
	}
	CPPUNIT_ASSERT_MESSAGE(
		"polynomial_fit_test: lamout is not optimal.",
		ok
	);
	return;
}

} // [End: unnamed namespace]
// *************************************************************************


// -------------- CppUnit framework functions ------------------------------

void spk_non_par_test::setUp()
{	// initializations
}
void spk_non_par_test::tearDown()
{	// clean up
}
CppUnit::Test* spk_non_par_test::suite()
{	using CppUnit::TestSuite;
	using CppUnit::TestCaller;

	TestSuite *suiteOfTests = new TestSuite("spk_non_par_test");

	suiteOfTests->addTest(
		new TestCaller<spk_non_par_test>(
      			"polynomial_fit_test", 
			&spk_non_par_test::polynomial_fit_test
		)
	);
	suiteOfTests->addTest(
		new TestCaller<spk_non_par_test>(
      			"polynomial_and_variance_fit_test", 
			&spk_non_par_test::polynomial_and_variance_fit_test
		)
	);
	return suiteOfTests;
}

//--------------------------------------------------------------------
void spk_non_par_test::polynomial_fit_test(void)
{
	// can change these values or better yet 
	// run different combinations of these values
	double sigma = .5; // measure noise standard deviation
	size_t m     = 1;  // number of fixed effects (not used)
	size_t n     = 2;  // number of random effects per individual
	size_t q     = 4;  // number of measurements per individual
	size_t M     = 5;  // number of individuals in the population
	size_t J     = 10; // number of discret measure points 
	bool fit_variance = false; // variance is a fixed effect

	one_fit(sigma, q, m, M, n, J, fit_variance);

	// Uncomment these statements to see the warnings that are
	// generated for this test because its measure points are
	// constrained by their bounds.
	/*
	using namespace std;
	string warnings;
	WarningsManager::getAllWarnings( warnings );
	cout << "########################################" << endl;
	cout << warnings;
	cout << "########################################" << endl;
	*/

}

//--------------------------------------------------------------------
void spk_non_par_test::polynomial_and_variance_fit_test(void)
{
	// can change these values or better yet run different combinations 
	// of these values
	double sigma = .5; // measure noise standard deviation
	size_t m     = 1;  // number of fixed effects (not used)
	size_t n     = 3;  // number of random effects per individual
	size_t q     = 4;  // number of measurements per individual
	size_t M     = 5;  // number of individuals in the population
	size_t J     = 10; // number of discret measure points 
	bool fit_variance = true; // variance is a random effect

	one_fit(sigma, q, m, M, n, J, fit_variance);
}
