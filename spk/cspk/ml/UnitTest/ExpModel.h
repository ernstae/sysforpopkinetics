# ifndef ExpModelIncluded
# define ExpModelIncluded

# include <valarray>
# include <cassert>
# include <spk/SpkModel.h>

/*
$begin ExpModel$$
$spell
	const
	std
	valarray
$$

$index ExpModel$$
$index constructor, exponential model$$


$section A Sum of Exponentials Population Model Constructor$$

$table
$bold Syntax$$ $cnext
$syntax%ExpModel %model%(
	double                        %SigmaD%,
	double                        %SigmaR%,
	const std::valarray<int>     &%N%,
	const std::valarray<double>  &%t%
)%$$ 
$tend

$fend 25$$

$head Description$$
This syntax constructs a $xref/MonteSpkModel/$$ with the following structure:
$latex \[
\begin{array}{rcl}
	D_{j,k}  ( \alpha )            & = & 
		\left\{
		\begin{array}{ll}
			exp( \alpha_{4+j} )  & {\rm if} \; j = k \\
			0             & {\rm otherwise}
		\end{array}
		\right.
	\\
	R^i ( \alpha , b^i )_{j,k}   & = & 
		\left\{
		\begin{array}{ll}
			exp( \alpha_8 )   & {\rm if} \; j = k \\
			0          & {\rm otherwise}
		\end{array}
		\right.
	\\
	f^i ( \alpha , b )_j  
	& = &
	\alpha_0 \exp( b_0 ) * \exp[ - \alpha_2 \exp( b_2 ) t^i_j ]
	\\
	& + &
	\alpha_1 \exp( b_1 ) * \exp[ - \alpha_3 \exp( b_3 ) t^i_j ]
\end{array}
\] $$

$end
*/

# include <valarray>
# include <CppAD/CppAD.h>

class ExpModel : public SpkModel<double> {
private:
	// constant part of private data
	const size_t                 nInd; // number of random effects
	const size_t                 nPop; // number of fixed effects
	CppAD::vector<size_t>           N; // measurements per individual

	// offset for each individual in the combined data array
	CppAD::vector<size_t>      offset;

	// time corresponding to each measurement in combined data array
	CppAD::vector<double>        Time;

	// CppAD differentiable function object and vectors
	CppAD::ADFun<double> *AD_F;
	mutable CppAD::vector<double>  domain;  // temporary domain vector
	mutable CppAD::vector<double>  range;  // temporary range vector

	// variable part of private data
	size_t              individual; // current individual index          
	size_t                      Ni; // value of N[individual]
	CppAD::vector<double>      Pop; // current value of the fixed effect
	CppAD::vector<double>      Ind; // current value of the random effects 
public:
	// constructor
	ExpModel(
		const std::valarray<int>    &N_, 
		const std::valarray<double> &Time_
	) : 
	nInd(4), 
	nPop(2*nInd+1), 
	Pop(nPop), 
	Ind(nInd),
	domain(nPop + nInd + 1),
	range(1)
	{	
		/*
		construct the CppAD differentiable function object
		A1 = THETA(1)*EXP(ETA(1))
		A2 = THETA(2)*EXP(ETA(2))
		L1 = THETA(3)*EXP(ETA(3))
		L2 = THETA(4)*EXP(ETA(4))
		F = A1*EXP (-L1*TIME) + A2*EXP(-L2*TIME)
		*/

		// declare independent variables
		using namespace CppAD;

		// set N 
		N.resize( N_.size() );
		size_t i;
		for(i = 0; i < N.size(); i++)
			N[i] = N_[i]; 

		// set Time 
		Time.resize( Time_.size() );
		size_t j;
		for(j = 0; j < Time.size(); j++)
			Time[j] = Time_[j]; 

		// independent variaiable vector = (alpha, b, t)
		CppAD::vector< AD<double> > pop_ind_time(nPop + nInd + 1);
		for(j = 0; j < nPop + nInd + 1; j++)
			pop_ind_time[j] = 1.;
		Independent(pop_ind_time); 
	
		// compute function
		AD<double> Theta1 = pop_ind_time[0];
		AD<double> Theta2 = pop_ind_time[1];
		AD<double> Theta3 = pop_ind_time[2];
		AD<double> Theta4 = pop_ind_time[3];
		AD<double> Eta1   = pop_ind_time[nPop];
		AD<double> Eta2   = pop_ind_time[nPop+1];
		AD<double> Eta3   = pop_ind_time[nPop+2];
		AD<double> Eta4   = pop_ind_time[nPop+3];
		AD<double> T      = pop_ind_time[nPop+nInd];
		AD<double> A1     = Theta1*exp(Eta1);
		AD<double> A2     = Theta2*exp(Eta2);
		AD<double> L1     = Theta3*exp(Eta3);
		AD<double> L2     = Theta4*exp(Eta4);
		CppAD::vector< AD<double> > F(1);
		F[0] = A1*exp (-L1*T) + A2*exp(-L2*T);

		// construct funciton object
		AD_F = new ADFun<double> (pop_ind_time, F);

		// set the measurement offset vector
		size_t sum;
		offset.resize( N.size() );
		sum = 0;
		for(i = 0; i < N.size(); i++)
		{	assert( N[i] >= 0 );
			offset[i] = sum; 
			sum      += N[i];
		}
		assert( sum == Time.size() );

		// invalid value for individual to illegal value
		// to make sure individual set in certain cases
		individual = N.size();
		Ni         = 0;
       	}
private:
	// Setting routines
	void doSetPopPar(const std::valarray<double> &Pop_)
	{	assert( Pop_.size() == nPop );
		size_t j;
		for(j = 0; j < nPop; j++)
			Pop[j] = Pop_[j];
	}
	void doSelectIndividual(int individual_)
	{
		assert( individual_ >= 0 );
		assert( N.size() > individual_ );

		individual = individual_;
		Ni         = N[individual];
	}
	void doSetIndPar(const std::valarray<double> &Ind_)
	{	assert( Ind_.size() == nInd );
		size_t j;
		for(j = 0; j < nInd; j++)
			Ind[j] = Ind_[j];
	}
	// Data mean routines
	void doDataMean(std::valarray<double> &fi) const
	{	assert( fi.size() == Ni );
		assert( individual < N.size() );

		size_t j, k;
		for(j = 0; j < nPop; j++)
			domain[j] = Pop[j];
		for(j = 0; j < nInd; j++)
			domain[nPop+j] = Ind[j];
	
		j = offset[individual];
		for(k = 0; k < Ni; k++)
		{	domain[nPop + nInd] = Time[j+k];
			range = AD_F->Forward(0, domain);
			fi[k] = range[0];
		}
	}
	bool doDataMean_indPar(std::valarray<double> &fi_ind) const
	{	assert( fi_ind.size() == Ni * nInd );
		assert( individual < N.size() );

		size_t j,k;
		j = offset[individual];
		for(k = 0; k < Ni; k++)
		{	for(j = 0; j < nPop; j++)
				domain[j] = Pop[j];
			for(j = 0; j < nInd; j++)
				domain[nPop + j] = Ind[j];
			domain[nPop + nInd] = Time[j+k];
			range = AD_F->Forward(0, domain);
			range[0] = 1.;
			domain = AD_F->Reverse(1, range);
			for(j = 0; j < nInd; j++)
				fi_ind[k * nInd + j] = domain[nPop + j];
		}

		return true;
	}
	bool doDataMean_popPar(std::valarray<double> &fi_pop) const
	{	assert( fi_pop.size() == Ni * nPop );
		assert( individual < N.size() );

		size_t k;
		size_t j;
		j = offset[individual];
		for(k = 0; k < Ni; k++)
		{	for(j = 0; j < nPop; j++)
				domain[j] = Pop[j];
			for(j = 0; j < nInd; j++)
				domain[nPop + j] = Ind[j];
			domain[nPop + nInd] = Time[j+k];
			range = AD_F->Forward(0, domain);
			range[0] = 1.;
			domain = AD_F->Reverse(1, range);
			for(j = 0; j < nPop; j++)
				fi_pop[k * nPop + j] = domain[j];
		}

		return true;
	}
	// Data variance routines
	void doDataVariance(std::valarray<double> &R) const
	{	assert( R.size() == Ni * Ni );
		assert( individual < N.size() );

		size_t j;
		size_t k;
		for(j = 0; j < Ni; j++)
		{	for(k = 0; k < Ni; k++)
				R[ j * Ni + k ] = 0.;
			R[ j * Ni + j] = exp( Pop[8] );
		}
	}
	bool doDataVariance_indPar(std::valarray<double> &R_ind) const
	{	assert( R_ind.size() == Ni * Ni * nInd );
		assert( individual < N.size() );

		size_t j, k, l;
		for(j = 0; j < Ni; j++)
		{	for(k = 0; k < Ni; k++)
				for(l = 0; l < nInd; l++)
					R_ind[ (j * Ni + k) * nInd + l ] = 0.;
		}
		return true;
	}
	bool doDataVariance_popPar(std::valarray<double> &R_pop) const
	{	assert( R_pop.size() == Ni * Ni * nPop );
		assert( individual < N.size() );

		size_t j, k, l;
		for(j = 0; j < Ni; j++)
		{	for(k = 0; k < Ni; k++)
				for(l = 0; l < nPop; l++)
					R_pop[ (j * Ni + k) * nPop + l ] = 0.;
			R_pop[ (j * Ni + j) * nPop + 8 ] = exp( Pop[8] );
		}
		return true;
	}
	// Random effects variance routines
	void doIndParVariance(std::valarray<double> &D) const
	{	assert( D.size() == nInd * nInd );

		size_t j, k;
		for(j = 0; j < nInd; j++)
		{	for(k = 0; k < nInd; k++)
				D[j * nInd + k] = 0.;
			D[ j * nInd + j] = exp( Pop[4 + j] );
		}
	}
	bool doIndParVariance_popPar(std::valarray<double> &D_pop) const
	{	assert( D_pop.size() == nInd * nInd * nPop );

		size_t j, k, l;
		for(j = 0; j < nInd; j++)
		{	for(k = 0; k < nInd; k++)
				for(l = 0; l < nPop; l++)
					D_pop[( j * nInd + k) * nPop + l] 
						= 0.;
			D_pop[ (j * nInd + j) * nPop + 4 + j] 
				= exp( Pop[4 + j] );
		}
		return true;
	}
};


# endif
