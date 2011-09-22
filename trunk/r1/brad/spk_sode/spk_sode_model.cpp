# include <cppad/cppad.hpp>
# include <spk/SpkModel.h>
# include "spk_sode_model.hpp"
# include <cassert>

// Suppose that V_{i,j} (x) is an (m, n) matrix valued function where
// x is a vector of length ell and V_x contains the derivative w.r.t x. 
// V_x [ j + i*m + k*m*n ] = partial V_{i,j} / partial x_k

// Empty namespace
namespace {
	using std::valarray;
	using CppAD::vector;
	using CppAD::AD;
	using CppAD::ADFun;
	using CppAD::abs;   // works for double aguments

	vector< AD<double> > ADvector(const vector<double> &x)
	{	vector< AD<double> > X(x.size());
		size_t j;
		for(j = 0; j < x.size(); j++)
			X[j] = x[j];
		return X;
	}
	vector<size_t> Sum(const vector<size_t> N)
	{	vector<size_t> S(N.size());
		size_t i = 0;
		S[0]     = 0;
		for(i = 1; i < N.size(); i++)
			S[i] = S[i-1] + N[i-1];
		return S;
	}
	
	template <class Scalar>
	void q2_of(
		vector<Scalar>       &q2        ,
		const vector<Scalar> &t         ,
		size_t                j_start   ,
		size_t                j_end     ,
		const vector<Scalar> &alpha     , 
		const vector<Scalar> &eta       ) 
	{	assert( j_start < j_end );
		assert( q2.size() == (j_end - j_start) );
		assert( j_end <= t.size() );
		assert( alpha.size() == 7 && eta.size() == 3 );
		Scalar k_a = alpha[1] * exp(eta[1]); 
		Scalar k_e = alpha[2] * exp(eta[2]); 
		bool near_equal_k = 
			abs(k_e - k_a) < 1e-6 * (abs(k_e) + abs(k_a));
		Scalar exp_a, exp_e, tj;
		size_t Ni = j_end - j_start;
		size_t j;
		for(j = 0; j < Ni; j++)
		{	tj = t[j + j_start];
			if( near_equal_k )
				q2[j] = k_a*tj*(1. - (k_e + k_a)*tj/2.);
			else
			{	exp_a = exp( - k_a * tj); 	
				exp_e = exp( - k_e * tj); 	
				q2[j] = k_a * (exp_a - exp_e) / (k_e - k_a);
			}
		}
		return;
	}
	
	template <class Scalar>
	void data_mean(
		vector<Scalar>       &F         ,
		const vector<Scalar> &t         ,
		size_t                j_start   ,
		size_t                j_end     ,
		const vector<Scalar> &alpha     , 
		const vector<Scalar> &eta       ) 
	{	q2_of(F, t, j_start, j_end, alpha, eta);
		size_t Ni = j_end - j_start;
		size_t j;
		Scalar factor = alpha[0] * exp(eta[0] );
		for(j = 0; j < Ni; j++)
			F[j] = factor * F[j];
	}
	
	template <class Scalar>
	void data_variance(
		vector<Scalar>       &R         ,
		Scalar                sigma     ,
		const vector<Scalar> &t         ,
		size_t                j_start   ,
		size_t                j_end     ,
		const vector<Scalar> &alpha     , 
		const vector<Scalar> &eta       ) 
	{	size_t Ni = (j_end - j_start);
		assert( R.size() == Ni * Ni );
		vector<Scalar> q2( Ni );
		q2_of(q2, t, j_start, j_end, alpha, eta);
		
		// assume time points are in increasing order
		size_t j, k;
		Scalar tmin_jk;
		Scalar factor = exp( alpha[6] );
		for(j = 0; j < Ni; j++)
		{	tmin_jk = t[j_start + j];
			for(k = j; k < Ni; k++) 
			{	R[j + k * Ni] = 
					factor * q2[j] * tmin_jk * q2[k]; 
				R[k + j * Ni] = R[j + k * Ni];
			}
			R[j + j *Ni] += sigma * sigma;
		}
	}
}

// constructor ----------------------------------------------------------
spk_sode_model::spk_sode_model( 
	double                sigma   ,
	size_t                ell     , 
	size_t                m       , 
	const vector<size_t> &N       , 
	const vector<double> &t       ) 
: sigma_(sigma), ell_(7), m_(3), N_(N), t_(t), t_ad_(ADvector(t)), S_(Sum(N)), 
alpha_(ell_), eta_(m_)
{	// inidialize individual index as an invalid value
		i_ = N_.size();
}
// set model state -------------------------------------------------------
void spk_sode_model::doSelectIndividual(int i)
{	assert( 0 <= i && i < N_.size() );
	i_ = i;
}
void spk_sode_model::doSetPopPar(const valarray<double> &alpha)
{	assert( alpha.size() == ell_ );
	size_t k;
	for(k = 0; k < ell_; k++)
		alpha_[k] = alpha[k];
	return;
}
void spk_sode_model::doSetIndPar(const valarray<double> &eta)
{	assert( eta.size() == m_ );
	size_t k;
	for(k = 0; k < m_; k++)
		eta_[k] = eta[k];
	return;
}
// function values ---------------------------------------------------------
void spk_sode_model::doIndParVariance(valarray<double> &Omega) const
{	Omega.resize(m_ * m_);

	assert( alpha_.size() == ell_);
	size_t k1, k2, k3;
	for(k1 = 0; k1 < m_; k1++)
	{	for(k2 = 0; k2 < m_; k2++)
		{	Omega[k1 * m_ + k2] = 0.;
		}
		k3 = m_ + k1;
		Omega[k1  + k1 * m_] = exp(alpha_[k3]);
	}
	return;
}
void spk_sode_model::doDataMean(valarray<double> &F) const
{	size_t Ni = N_[i_];
	F.resize(Ni);

	assert( i_ < N_.size() );
	vector<double> F_vec( Ni );
	size_t j_start = S_[i_];
	size_t j_end   = j_start + Ni;
	data_mean(F_vec, t_, j_start, j_end, alpha_, eta_);
	size_t j;
	for(j = 0; j < Ni; j++)
		F[j] = F_vec[j];
	return;
}
void spk_sode_model::doDataVariance(valarray<double> &R) const
{	size_t Ni = N_[i_];
	R.resize(Ni * Ni);

	vector<double> R_vec( Ni * Ni );
	size_t j_start = S_[i_];
	size_t j_end   = j_start + Ni;
	data_variance(R_vec, sigma_, t_, j_start, j_end, alpha_, eta_);
	size_t k;
	for(k = 0; k < Ni * Ni; k++)
		R[k] = R_vec[k];
	return;
}
// Derivative with respect to fixed effects --------------------------------
bool spk_sode_model::doIndParVariance_popPar
	(valarray<double> &Omega_alpha) const
{	size_t m_sq = m_ * m_;
	Omega_alpha.resize(m_sq * ell_);

	size_t k1, k2, k3;
	bool all_zero = true;
	for(k1 = 0; k1 < m_; k1++)
	{	for(k2 = 0; k2 < m_; k2++)
		{	for(k3 = 0; k3 < ell_; k3 ++)
				Omega_alpha[k2 + k1 * m_ + k3 * m_sq] = 0.;
		}
		k2 = k1;
		k3 = m_ + k1;
		Omega_alpha[k2 + k1 * m_ + k3 * m_sq] = exp(alpha_[k3]);
		all_zero &= (0. == Omega_alpha[k2 + k1 * m_ + k3 * m_sq]);
	}
	return ! all_zero;
}
bool spk_sode_model::doDataMean_popPar(valarray<double> &F_alpha) const
{	size_t Ni = N_[i_];
	F_alpha.resize( Ni * ell_ );

	assert( i_ < N_.size() );
	size_t j_start = S_[i_];
	size_t j_end   = j_start + Ni;
	vector< AD<double> > F_ad( Ni );
	vector< AD<double> > alpha_ad = ADvector(alpha_);
	vector< AD<double> > eta_ad   = ADvector(eta_);
	Independent(alpha_ad);
	data_mean(F_ad, t_ad_, j_start, j_end, alpha_ad, eta_ad);
	ADFun<double> ad_fun(alpha_ad, F_ad);
	vector<double> jacobian = ad_fun.Jacobian(alpha_);
	bool all_zero = true;
	size_t j, k;
	for(j = 0; j < Ni; j++)
	{	for(k = 0; k < ell_; k++)
		{	F_alpha[j + k * Ni] = jacobian[j * ell_ + k]; 
			all_zero &= (0. == F_alpha[j + k * Ni]);
		}
	}
	return ! all_zero;
}
bool spk_sode_model::doDataVariance_popPar(valarray<double> &R_alpha) const
{	size_t Ni = N_[i_];
	R_alpha.resize( Ni * Ni * ell_ );

	vector< AD<double> > R_ad( Ni * Ni );
	AD<double>  sigma_ad = sigma_;
	size_t j_start = S_[i_];
	size_t j_end   = j_start + Ni;
	vector< AD<double> > alpha_ad = ADvector(alpha_);
	vector< AD<double> > eta_ad   = ADvector(eta_);
	Independent(alpha_ad);
	data_variance(R_ad, sigma_ad, t_ad_, j_start, j_end, alpha_ad, eta_ad);
	ADFun<double> ad_fun(alpha_ad, R_ad);
	vector<double> jacobian = ad_fun.Jacobian(alpha_);
	bool all_zero = true;
	size_t jk, k;
	for(jk = 0; jk < Ni * Ni; jk++ )
	{	for(k = 0; k < ell_; k++)
		{	R_alpha[jk + k * Ni*Ni] = jacobian[jk * ell_ + k];
			all_zero &= (0. == R_alpha[jk + k * Ni*Ni]);
		}
	}
	return ! all_zero;
}
// Derivative with respect to random effects --------------------------------
bool spk_sode_model::doDataMean_indPar(valarray<double> &F_eta) const
{	size_t Ni = N_[i_];
	F_eta.resize( Ni * m_ );

	assert( i_ < N_.size() );
	size_t j_start = S_[i_];
	size_t j_end   = j_start + Ni;
	vector< AD<double> > F_ad( Ni );
	vector< AD<double> > alpha_ad = ADvector(alpha_);
	vector< AD<double> > eta_ad   = ADvector(eta_);
	Independent(eta_ad);
	data_mean(F_ad, t_ad_, j_start, j_end, alpha_ad, eta_ad);
	ADFun<double> ad_fun(eta_ad, F_ad);
	vector<double> jacobian = ad_fun.Jacobian(eta_);
	bool all_zero = true;
	size_t j, k;
	for(j = 0; j < Ni; j++)
	{	for(k = 0; k < m_; k++)
		{	F_eta[j + k * Ni] = jacobian[j * m_ + k]; 
			all_zero &= (0. == F_eta[j + k * Ni]);
		}
	}
	return ! all_zero;
}
bool spk_sode_model::doDataVariance_indPar(valarray<double> &R_eta) const
{	size_t Ni = N_[i_];
	R_eta.resize( Ni * Ni * m_ );

	vector< AD<double> > R_ad( Ni * Ni );
	AD<double>  sigma_ad = sigma_;
	size_t j_start = S_[i_];
	size_t j_end   = j_start + Ni;
	vector< AD<double> > alpha_ad = ADvector(alpha_);
	vector< AD<double> > eta_ad   = ADvector(eta_);
	Independent(eta_ad);
	data_variance(R_ad, sigma_ad, t_ad_, j_start, j_end, alpha_ad, eta_ad);
	ADFun<double> ad_fun(eta_ad, R_ad);
	vector<double> jacobian = ad_fun.Jacobian(eta_);
	bool all_zero = true;
	size_t jk, k;
	for(jk = 0; jk < Ni * Ni; jk++ )
	{	for(k = 0; k < m_; k++)
		{	R_eta[jk + k * Ni*Ni] = jacobian[jk * m_ + k];
			all_zero &= (0. == R_eta[jk + k * Ni*Ni]);
		}
	}
	return ! all_zero;
}
