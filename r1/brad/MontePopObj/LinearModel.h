# ifndef LinearModelIncluded
# define LinearModelIncluded

# include <valarray>
# include <cassert>
# include <spk/SpkModel.h>

/*
$begin LinearModel$$
$spell
	const
	std
	valarray
$$

$index LinearModel$$
$index constructor, linear model$$


$section A Linear Population Model Constructor$$

$table
$bold Syntax$$ $cnext
$syntax%LinearModel %model%(
	double                       %Sigma_D%,
	double                       %Sigma_R%,
	const std::valarray<int>    &%N%,
	const std::valarray<double>      &%t%
)%$$ 
$tend

$fend 25$$

$head Description$$
This syntax constructs a $xref/MonteSpkModel/$$ with the following structure:
$latex \[
\begin{array}{rcl}
	D  ( \alpha )            & = & \Sigma_D
	\\
	R^i ( \alpha , b^i )_{j,k}   & = & 
		\left\{
		\begin{array}{ll}
			\Sigma_R  & {\rm if} \; j = k \\
			0          & {\rm otherwise}
		\end{array}
		\right.
	\\
	f^i ( \alpha , b^i )_j      & = & \alpha_0 t_j + b^i_0
\end{array}
\] $$

$end
*/

class LinearModel : public SpkModel {
private:
	// constant part of private data
	const size_t   m;             // number of fixed effects
	const size_t   n;             // number of random effects
	const std::valarray<int>  N;  // measurements per individual
	const double   Sigma_D;       // variance of the random effect
	const double   Sigma_R;       // variance of the measurements

	// variable part of private data
	size_t                i;         // individual index          
	std::valarray<double> alpha;     // value of the fixed effect
	std::valarray<double> bi;        // value of the random effect


	// constant part of private data
	std::valarray<size_t>        offset; // for each individual
	const std::valarray<double>  t;      // for each measurement
public:
	LinearModel(
		double                        Sigma_D_,
		double                        Sigma_R_,
		const std::valarray<int>     &N_,
		const std::valarray<double>  &t_) : 
	m(1), 
	n(1), 
	N(N_), 
	Sigma_D(Sigma_D_), 
	Sigma_R(Sigma_R_), 
	alpha(m), 
	bi(m), 
	t(t_) 
	{
		size_t sum;

		// set the measurement offset vector
		offset.resize( N.size() );
		sum = 0;
		for(i = 0; i < N.size(); i++)
		{	assert( N[i] >= 0 );
			offset[i] = sum; 
			sum      += N[i];
		}
		assert( sum == t.size() );

		// invalid value for i used to make sure i set in certain cases
		i = N.size();
       	}
private:
	// Setting routines
	void doSetPopPar(const std::valarray<double> &alpha_)
	{	assert( alpha_.size() == m );
		alpha = alpha_;
	}
	void doSelectIndividual(int individual)
	{
		assert( individual >= 0 );
		assert( N.size() > individual );

		i = individual; 
	}
	void doSetIndPar(const std::valarray<double> &bi_)
	{	assert( bi_.size() == n );
		bi = bi_;
	}
	// Data mean routines
	void doDataMean(std::valarray<double> &fi) const
	{	assert( fi.size() == N[i] );
		assert( i < N.size() );

		size_t k;
		size_t j;
		j = offset[i];
		for(k = 0; k < N[i]; k++)
			fi[k] = alpha[0] * t[j+k] + bi[0];
	}
	bool doDataMean_indPar(std::valarray<double> &fi_ind) const
	{	assert( fi_ind.size() == n * N[i] );
		assert( i < N.size() );

		size_t k;
		size_t j;
		j = offset[i];
		for(k = 0; k < N[i]; k++)
			fi_ind[k] = 1.;

		return true;
	}
	bool doDataMean_popPar(std::valarray<double> &fi_alpha) const
	{	assert( fi_alpha.size() == m * N[i] );
		assert( i < N.size() );

		size_t k;
		size_t j;
		j = offset[i];
		bool flag = false;
		for(k = 0; k < N[i]; k++)
		{	fi_alpha[k] = t[j + k];
			flag       |= t[j + k] != 0.;
		} 

		return flag;
	}
	// Data variance routines
	void doDataVariance(std::valarray<double> &R) const
	{	assert( R.size() == N[i] * N[i] );
		assert( i < N.size() );

		size_t j;
		size_t k;
		size_t n = N[i];
		for(j = 0; j < n; j++)
		{	for(k = 0; k < n; k++)
				R[ j * n + k ] = 0.;
			R[ j * n + j] = Sigma_R;
		}
	}
	bool doDataVariance_indPar(std::valarray<double> &R_ind) const
	{	assert( R_ind.size() == n * N[i] * N[i] );
		assert( i < N.size() );

		size_t j;
		size_t k;
		size_t n = N[i];
		for(j = 0; j < n; j++)
		{	for(k = 0; k < n; k++)
				R_ind[ j * n + k ] = 0.;
		}
		return false;
	}
	bool doDataVariance_popPar(std::valarray<double> &R_pop) const
	{	assert( R_pop.size() == m * N[i] * N[i] );
		assert( i < N.size() );

		size_t j;
		size_t k;
		size_t n = N[i];
		for(j = 0; j < n; j++)
		{	for(k = 0; k < n; k++)
				R_pop[ j * n + k ] = 0.;
		}
		return false;
	}
	// Random effects variance routines
	void doIndParVariance(std::valarray<double> &D) const
	{	assert( D.size() == n * n );

		D[0] = Sigma_D;
	}
	
	bool doIndParVariance_popPar(std::valarray<double> &D_pop) const
	{	assert( D_pop.size() == m * n * n );

		D_pop[0] = 0.;
		return false;
	}
};


# endif
