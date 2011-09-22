# include <cppad/cppad.hpp>
# include <spk/SpkModel.h>
# include <cppad/cppad.hpp>

class spk_sode_model : public SpkModel<double>
{
private:
	// member data ----------------------------------------------------
	const double                sigma_; // measuurement noise std
	const size_t                ell_; // number of fixed effects
	const size_t                m_;   // # random effects per individual
	const CppAD::vector<size_t> N_;   // # measurement for each individual
	const CppAD::vector<double> t_;   // measurement times
	const CppAD::vector< CppAD::AD<double> >  
	                            t_ad_;// measurement times
	const CppAD::vector<size_t> S_;   // offset for each individual
	//
	size_t                      i_;     // current individual index
	CppAD::vector<double>       alpha_; // current fixed effects
	CppAD::vector<double>       eta_;   // current random effects
public:
	// constructor
	spk_sode_model( 
		double                       sigma   ,
		size_t                       ell     , 
		size_t                       m       , 
		const CppAD::vector<size_t> &N       , 
		const CppAD::vector<double> &t       );
	// virtual SpkModel member functions ------------------------------
	// set state functions
	void doSelectIndividual(int i);
	void doSetPopPar(const std::valarray<double> &alpha);
	void doSetIndPar(const std::valarray<double> &eta);
	// function values
	void doIndParVariance(std::valarray<double> &Omega)              const;
	void doDataMean(std::valarray<double> &F)                        const;
	void doDataVariance(std::valarray<double> &R)                    const;
	// partials w.r.t alpha
	bool doIndParVariance_popPar(std::valarray<double> &Omega_alpha) const;
	bool doDataMean_popPar(std::valarray<double> &F_alpha)           const;
	bool doDataVariance_popPar(std::valarray<double> &R_alpha)       const;
	// partials w.r.t eta
	bool doDataMean_indPar(std::valarray<double> &F_eta)             const;
	bool doDataVariance_indPar(std::valarray<double> &R_eta)         const;
};
