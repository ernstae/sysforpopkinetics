# include <cppad/cppad.hpp>
# include <spk/SpkModel.h>
# include "spk_sode_model.hpp"
# include "smoother.hpp"
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
	void data_mean(
		vector<Scalar>       &F         ,
		const vector<Scalar> &t         ,
		const vector<Scalar> &y         ,
		size_t                j_start   ,
		size_t                j_end     ,
		const vector<Scalar> &alpha     , 
		const vector<Scalar> &eta       ) 
	{
		size_t Ni = j_end - j_start;
		vector<Scalar> ti(Ni), yi(Ni), ai(Ni), bi(Ni), qi(Ni), ri(Ni);
		size_t j;
		for(j = 0; j < Ni; j++)
		{	ti[j] = t[j + j_start];
			yi[j] = y[j + j_start];
		}
		abqr_of_initial(
			eta, alpha, ti, yi, ai, bi, qi, ri
		);
		for(j = 0; j < Ni; j++)
			F[j] = ri[j];
	}
}

// constructor ----------------------------------------------------------
spk_sode_model::spk_sode_model( 
	const vector<size_t> &N       , 
	const vector<double> &t       ,
	const vector<double> &y       ) 
: 
	ell_(7), 
	m_(2), 
	N_(N), 
	t_(t), 
	y_(y),
	S_(Sum(N)), 
	t_ad_(ADvector(t)), 
	y_ad_(ADvector(y)), 
	//
	// initialize non-constant values
	i_(N_.size()),
	alpha_(ell_), 
	eta_(m_)
{ }
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
{	assert( m_ == 2 );	
	Omega.resize(m_ * m_);
	assert( alpha_.size() == ell_);
	assert( m_ + m_ <= ell_ );
	size_t k1, k2;
	for(k1 = 0; k1 < m_; k1++)
	{	for(k2 = 0; k2 < m_; k2++)
			Omega[k1 + k2 * m_] = 0.;
		Omega[k1 + k1 * m_] = exp( alpha_[m_ + k1] );
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
	data_mean(F_vec, t_, y_, j_start, j_end, alpha_, eta_);
	size_t j;
	for(j = 0; j < Ni; j++)
		F[j] = F_vec[j];
	return;
}
void spk_sode_model::doDataVariance(valarray<double> &R) const
{	size_t Ni = N_[i_];
	R.resize(Ni * Ni);

	double var_epsilon = exp( alpha_[m_ + m_] );
	size_t j1, j2;
	for(j1 = 0; j1 < Ni; j1++)
	{	for(j2 = 0; j2 < Ni; j2++)
			R[j1 + j2 * Ni] = 0.;
		R[j1 + j1 * Ni] = var_epsilon;
	}
	return;
}
// Derivative with respect to fixed effects --------------------------------
bool spk_sode_model::doIndParVariance_popPar
	(valarray<double> &Omega_alpha) const
{	size_t m_sq = m_ * m_;
	Omega_alpha.resize(m_sq * ell_);

	size_t k1, k2, k;
	for(k1 = 0; k1 < m_; k1++)
	{	for(k2 = 0; k2 < m_; k2++)
		{	for(k = 0; k < ell_; k ++)
				Omega_alpha[k2 + k1 * m_ + k * m_sq] = 0.;
		}
		k2 = k1;
		k  = m_ + k1;
		Omega_alpha[k2 + k1 * m_ + k * m_sq] = exp(alpha_[k]);
	}
	bool non_zero = true;
	return non_zero;
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
	data_mean(F_ad, t_ad_, y_ad_, j_start, j_end, alpha_ad, eta_ad);
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

	size_t j1, j2, k;
	for(j1 = 0; j1 < Ni; j1++)
	{	for(j2 = 0; j2 < Ni; j2++)
		{	for(k = 0; k < ell_; k ++)
				R_alpha[j2 + j1 * Ni + k * Ni * Ni] = 0.;
		}
		j2 = j1;
		k  = 2 * m_;
		R_alpha[j2 + j1 * Ni + k * Ni * Ni] = exp(alpha_[k]);
	}
	bool non_zero = true;
	return non_zero;
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
	data_mean(F_ad, t_ad_, y_ad_, j_start, j_end, alpha_ad, eta_ad);
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

	size_t j1, j2, k;
	for(j1 = 0; j1 < Ni; j1++)
	{	for(j2 = 0; j2 < Ni; j2++)
		{	for(k = 0; k < m_; k ++)
				R_eta[j2 + j1 * Ni + k * Ni * Ni] = 0.;
		}
	}
	bool non_zero = false;
	return non_zero;
}
