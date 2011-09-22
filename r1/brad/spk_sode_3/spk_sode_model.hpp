// simulation model, requires the parameters 
// SIMULATE_LAMBDA, SIMULATE_Q0, and SIMULATE_R0
//
// Suppose that V_{i,j} (x) is an (m, n) matrix valued function where
// x is a vector of length ell and V_x contains the derivative w.r.t x. 
// V_x [ j + i*m + k*m*n ] = partial V_{i,j} / partial x_k



# include <cppad/cppad.hpp>
# include <spk/SpkModel.h>
# include <cppad/cppad.hpp>
# include <cassert>

using std::valarray;
using CppAD::vector;
using CppAD::AD;
using CppAD::ADFun;

inline double Value(double x)
{	return x; }

template <class Scalar>
class spk_sode_model : public SpkModel<Scalar>
{
private:
	// member data ----------------------------------------------------
	const size_t                ell_; // number of fixed effects
	const size_t                m_;   // # random effects per individual
	const vector<size_t>        N_;   // # measurement for each individual
	const vector<double>        y_;   // measurement values
	//
	vector<size_t>       S_;     // offset for each individual
	vector<Scalar>       t_;     // measurement times
	size_t               i_;     // current individual index
	vector<Scalar>       alpha_; // current fixed effects
	vector<Scalar>       eta_;   // current random effects
public:
	// constructor
	spk_sode_model( 
		const vector<size_t> &N       , 
		const vector<double> &t       );
	// virtual SpkModel member functions ------------------------------
	// set state functions
	void doSelectIndividual(int i);
	void doSetPopPar(const std::valarray<Scalar> &alpha);
	void doSetIndPar(const std::valarray<Scalar> &eta);
	// function values
	void doIndParVariance(std::valarray<Scalar> &Omega)              const;
	void doDataMean(std::valarray<Scalar> &F)                        const;
	void doDataVariance(std::valarray<Scalar> &R)                    const;
	// partials w.r.t alpha
	bool doIndParVariance_popPar(std::valarray<double> &Omega_alpha) const;
	bool doDataMean_popPar(std::valarray<double> &F_alpha)           const;
	bool doDataVariance_popPar(std::valarray<double> &R_alpha)       const;
	// partials w.r.t eta
	bool doDataMean_indPar(std::valarray<double> &F_eta)             const;
	bool doDataVariance_indPar(std::valarray<double> &R_eta)         const;
};

// Empty namespace
namespace {

	vector< AD<double> > ADvector(const vector<double> &x)
	{	vector< AD<double> > X(x.size());
		size_t j;
		for(j = 0; j < x.size(); j++)
			X[j] = x[j];
		return X;
	}
	vector< AD<double> > ADvector(const vector< AD<double> > &x)
	{	vector< AD<double> > X(x.size());
		X = x;
		return X;
	}
	// solve the deterministic ODE for one time step
	// Scalar can be either double, AD<double>, or AD< AD<double> >
	template <class Scalar>
	Scalar r_of_dt(Scalar dt, Scalar a, Scalar b, Scalar q0, Scalar r0)
	{	using CppAD::abs;
		bool ok = abs(a - b) > Scalar(1e-4) * (abs(a) + abs(b));
		Scalar term;
		if( ok )
			term = ( Scalar(1) - exp(-(a-b) * dt) ) / (a - b);
		else	term = dt - (a - b) * dt * dt / Scalar(2.);
		Scalar r = (r0 + a * q0 * term) * exp(-b * dt);
		return r;
	}


	// Given q[0] and r[0], solve stochastic difference equation for q, r
	// Scalar can be either double, AD<double>, or AD< AD<double> >
	template <class Scalar>
	void qr_of_tab(
		const vector<Scalar> &t         ,
		const vector<Scalar> &a         , 
		const vector<Scalar> &b         , 
		vector<Scalar>       &q         ,
		vector<Scalar>       &r         )
	{
		size_t Np = t.size();
		size_t Ni = Np - 1;
		assert( q.size() == Np );
		assert( r.size() == Np );
		assert( a.size() == Ni );
		assert( b.size() == Ni );
		size_t j;
		for(j = 0; j < Ni; j++)
		{	Scalar dt;
			dt      = t[j+1] - t[j];
			q[j+1]  = q[j] * exp( - a[j] * dt );
			r[j+1]  = r_of_dt(dt, a[j], b[j], q[j], r[j]);
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
	{
		size_t Np = j_end - j_start;
		size_t Ni = Np - 1;
		size_t M  = t.size() / Np;
		assert( F.size() == Ni );
		assert( t.size() == Np * M);
		vector<Scalar> ti(Np), ai(Ni), bi(Ni), qi(Np), ri(Np);
		size_t j;
		ti[0] = t[0 + j_start];
		ai[0] = alpha[0] + eta[0];
		bi[0] = alpha[1] + eta[1];
		for(j = 1; j < Ni; j++)
		{	ti[j] = t[j + j_start];
			ai[j] = ai[j-1] + SIMULATE_LAMBDA*eta[j+1];
			bi[j] = bi[j-1] + (1.-SIMULATE_LAMBDA)*eta[j+1];
		}
		ti[Ni] = t[Ni + j_start];
		qi[0]  = SIMULATE_Q0;
		ri[0]  = SIMULATE_R0;;
		qr_of_tab(ti, ai, bi, qi, ri);
		for(j = 0; j < Ni; j++)
			F[j] = ri[j+1];
	}
}

// constructor ----------------------------------------------------------
template <class Scalar>
spk_sode_model<Scalar>::spk_sode_model( 
	const vector<size_t> &N       , 
	const vector<double> &t       )
: 
	ell_(6), 
	m_(N[0] + 1), 
	N_(N), 
	S_(N.size()), 
	//
	// initialize non-constant values
	t_(t.size()), 
	i_(N_.size()),
	alpha_(ell_), 
	eta_(m_)
{	size_t i, j;
	for(j = 0; j < t.size(); j++)
		t_[j] = t[j];
	S_[0] = 0;
	for(i = 1; i < N.size(); i++)
	{	assert( N[i] == N[0] );
		S_[i] = S_[i-1] + N[i] + 1;
	}
}
// set model state -------------------------------------------------------
template <class Scalar>
void spk_sode_model<Scalar>::doSelectIndividual(int i)
{	assert( 0 <= i && i < N_.size() );
	i_ = i;
}
template <class Scalar>
void spk_sode_model<Scalar>::doSetPopPar(const valarray<Scalar> &alpha)
{	assert( alpha.size() == ell_ );
	size_t k;
	for(k = 0; k < ell_; k++)
		alpha_[k] = alpha[k];
	return;
}
template <class Scalar>
void spk_sode_model<Scalar>::doSetIndPar(const valarray<Scalar> &eta)
{	assert( eta.size() == m_ );
	size_t k;
	for(k = 0; k < m_; k++)
		eta_[k] = eta[k];
	return;
}
// function values ---------------------------------------------------------
template <class Scalar>
void spk_sode_model<Scalar>::doIndParVariance(valarray<Scalar> &Omega) const
{	size_t Ni = N_[0];
	assert( m_ == (Ni + 1) );
	Omega.resize(m_ * m_);
	assert( alpha_.size() == ell_);
	assert(  ell_ == 6 );
	size_t k1, k2;
	// initialize as zero
	for(k1 = 0; k1 < m_; k1++)
	{	for(k2 = 0; k2 < m_; k2++)
			Omega[k1 + k2 * m_] = 0.;
	}
	// set the diagonal
	Omega[0 + 0 * m_] = exp( alpha_[2] );
	Omega[1 + 1 * m_] = exp( alpha_[3] );
	for(k1 = 2; k1 < m_; k1++)
		Omega[k1 + k1 * m_] = exp( alpha_[4] );
	return;
}
template <class Scalar>
void spk_sode_model<Scalar>::doDataMean(valarray<Scalar> &F) const
{	size_t Ni = N_[i_];
	F.resize(Ni);
	assert( i_ < N_.size() );
	vector<Scalar> F_vec( Ni );
	size_t j_start = S_[i_];
	size_t j_end   = j_start + Ni + 1;
	data_mean(F_vec, t_, j_start, j_end, alpha_, eta_);
	size_t j;
	for(j = 0; j < Ni; j++)
		F[j] = F_vec[j];
	return;
}
template <class Scalar>
void spk_sode_model<Scalar>::doDataVariance(valarray<Scalar> &R) const
{	size_t Ni = N_[i_];
	R.resize(Ni * Ni);

	size_t j1, j2;
	for(j1 = 0; j1 < Ni; j1++)
	{	for(j2 = 0; j2 < Ni; j2++)
			R[j1 + j2 * Ni] = 0.;
		R[j1 + j1 * Ni] = exp( alpha_[ell_-1] );
	}
	return;
}
// Derivative with respect to fixed effects --------------------------------
template <class Scalar>
bool spk_sode_model<Scalar>::doIndParVariance_popPar
	(valarray<double> &Omega_alpha) const
{	size_t Ni = N_[i_];
	size_t m_sq = m_ * m_;
	Omega_alpha.resize(m_sq * ell_);

	size_t k1, k2, k;
	for(k1 = 0; k1 < m_; k1++)
	{	for(k2 = 0; k2 < m_; k2++)
		{	for(k = 0; k < ell_; k ++)
				Omega_alpha[k2 + k1 * m_ + k * m_sq] = 0.;
		}
	}
	Omega_alpha[0 + 0 * m_ + 0 * m_sq] = exp( Value( alpha_[2] ) ); 
	Omega_alpha[1 + 1 * m_ + 1 * m_sq] = exp( Value( alpha_[3] ) ); 
	for(k1 = 2; k1 < m_; k1++)
		Omega_alpha[k1 + k1 * m_ + k1 * m_sq] = exp( Value(alpha_[5]) );
	
	bool non_zero = true;
	return non_zero;
}
template <class Scalar>
bool spk_sode_model<Scalar>::doDataMean_popPar(valarray<double> &F_alpha) const
{	size_t Ni = N_[i_];
	F_alpha.resize( Ni * ell_ );

	assert( i_ < N_.size() );
	size_t j_start = S_[i_];
	size_t j_end   = j_start + Ni + 1;
	vector< AD<double> > F_ad( Ni );
	vector< AD<double> > alpha_ad = ADvector(alpha_);
	vector< AD<double> > eta_ad   = ADvector(eta_);
	vector< AD<double> > t_ad     = ADvector(t_);
	Independent(alpha_ad);
	data_mean(F_ad, t_ad, j_start, j_end, alpha_ad, eta_ad);
	ADFun<double> ad_fun(alpha_ad, F_ad);
	vector<double> alpha( alpha_.size() );
	size_t k;
	for(k = 0; k < alpha_.size(); k++)
		alpha[k] = Value( alpha_[k] );
	vector<double> jacobian = ad_fun.Jacobian(alpha);
	bool all_zero = true;
	size_t j;
	for(j = 0; j < Ni; j++)
	{	for(k = 0; k < ell_; k++)
		{	F_alpha[j + k * Ni] = jacobian[j * ell_ + k]; 
			all_zero &= (0. == F_alpha[j + k * Ni]);
		}
	}
	return ! all_zero;
}
template <class Scalar>
bool spk_sode_model<Scalar>::doDataVariance_popPar(valarray<double> &R_alpha) const
{	size_t Ni = N_[i_];
	R_alpha.resize( Ni * Ni * ell_ );

	size_t j1, j2, k;
	for(j1 = 0; j1 < Ni; j1++)
	{	for(j2 = 0; j2 < Ni; j2++)
		{	for(k = 0; k < ell_; k ++)
				R_alpha[j2 + j1 * Ni + k * Ni * Ni] = 0.;
		}
		R_alpha[j1 + j1 * Ni + (ell_-1) * Ni * Ni] = exp( Value(alpha_[ell_-1]) );
	}
	bool non_zero = true;
	return non_zero;
}
// Derivative with respect to random effects --------------------------------
template <class Scalar>
bool spk_sode_model<Scalar>::doDataMean_indPar(valarray<double> &F_eta) const
{	size_t Ni = N_[i_];
	F_eta.resize( Ni * m_ );

	assert( i_ < N_.size() );
	size_t j_start = S_[i_];
	size_t j_end   = j_start + Ni + 1;
	vector< AD<double> > F_ad( Ni );
	vector< AD<double> > alpha_ad = ADvector(alpha_);
	vector< AD<double> > eta_ad   = ADvector(eta_);
	vector< AD<double> > t_ad     = ADvector(t_);
	Independent(eta_ad);
	data_mean(F_ad, t_ad, j_start, j_end, alpha_ad, eta_ad);
	ADFun<double> ad_fun(eta_ad, F_ad);
	vector<double> eta( eta_.size() );
	size_t k;
	for(k = 0; k < eta_.size(); k++)
		eta[k] = Value( eta_[k] );
	vector<double> jacobian = ad_fun.Jacobian(eta);
	bool all_zero = true;
	size_t j;
	for(j = 0; j < Ni; j++)
	{	for(k = 0; k < m_; k++)
		{	F_eta[j + k * Ni] = jacobian[j * m_ + k]; 
			all_zero &= (0. == F_eta[j + k * Ni]);
		}
	}
	return ! all_zero;
}
template <class Scalar>
bool spk_sode_model<Scalar>::doDataVariance_indPar(valarray<double> &R_eta) const
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
