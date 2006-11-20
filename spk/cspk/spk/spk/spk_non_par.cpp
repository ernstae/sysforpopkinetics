/*
$begin spk_non_par$$
$latex \newcommand{\B}[1]{{\bf #1}}$$
$latex \newcommand{\R}{{\bf R}}$$
$latex \newcommand{\T}{{\rm T}}$$
$spell
	Karush Kuhn Tucker
	ublas
	mu
	std::cout
	itr
	pout
	CppAD
	Dirac
	lamout
	bup
	const
	nr
	spk
	nc
$$

$section Spk Nonparametric Population Analysis$$


$index spk_non_par$$
$index nonparametric, population analysis$$
$index population, nonparametric analysis$$

$head Syntax$$
$syntax%void spk_non_par(%level%, %model%, %N%, %y%, %epsilon%, % 
	blow%, %bup%, %Bin%, %Bout%, %lamout%, %pout%)%$$

$head Problem Definition$$
We define the discrete measure
$latex \Lambda$$ on $latex \R^n$$ 
and the non-parametric population objective function
$latex F : \R^{J \times n} \times \R_+^J \rightarrow \R$$ by
$latex \[
\begin{array}{rcl}
\Lambda ( b )     & = & \sum_{j=1} \lambda_j \delta ( b - B_j )
\\
F(B, \lambda) & = & - \sum_{i=1}^M \log [ \B{p} ( y^i | \Lambda ) ]
\end{array}
\] $$
where $latex \delta$$ denotes the Dirac delta function on $latex \R^n$$,
$latex \R_+$$ denotes the non-negative real numbers,
$latex B_j$$ denotes the $th j$$ row of the matrix $italic B$$,
$cref/M/non_par_model/Notation/M/$$ is the number of individuals
in the population, 
$cref/y^i/non_par_model/Notation/y^i/$$ is the 
measurement vector corresponding to individual $latex i$$,
the probability of $latex y^i$$ given $latex \Lambda$$ is given by
$latex \[
\begin{array}{rcl}
\B{p} ( y^i | \Lambda ) & = & 
\sum_{j=1} \lambda_j \B{p}( y^i | B_j )
\\
\B{p} ( y^i | b ) & = & 
\det \left[ 2 \pi R ( i, \alpha, b  ) \right]^{-1/2} 
\exp \left\{ 
		- \frac{1}{2} \left[ y^i - f(i, \alpha, b ) \right]^\T 
		R ( i, \alpha, b  )^{-1}
			\left[ y^i - f(i, \alpha, b ) \right]
\right\} 
\end{array}
\] $$
$cref/f(i,alpha,b)/non_par_model/Notation/f(i,alpha,b)/$$ 
is the mean of $latex y^i$$, and 
$cref/R(i,alpha,b)/non_par_model/Notation/R(i,alpha,b)/$$
is the variance of $latex y^i$$. 
The $code spk_non_par$$ routine determines a vector 
$latex \lambda \in \R^J$$
and matrix $latex B \in \R^{J \times n}$$ that solves the problem
$latex \[
\begin{array}{ll}
{\rm minimize}      
	& F(B, \lambda )
	\; , \;
	{\rm w.r.t.} \; \lambda \in \R_+^J \; , \; B \in \R^{J \times n}
\\
{\rm subject \; to} 
	& \sum_{j=1}^J \lambda_j = 1
	\; , \;
	blow \leq B_j \leq bup \; {\rm for} \; j = 1 , \ldots , J
\end{array}
\] $$

$head level$$
The argument $italic level$$ has prototype
$syntax%
	size_t %level%
%$$
It specifies the level of tracing to write to $code std::cout$$
during the execution of $code spk_non_par$$ as follows:

$subhead level == 0$$
No tracing is done

$subhead level >= 1$$
The current iteration number $italic itr$$,
the value of the relaxation parameter $code mu$$ and the
convergence information $code info$$ are printed at each iteration
(the relaxation parameter and all the convergence information 
should converge to zero).
The convergence information has three components 
in order as follows:
$list number$$
$latex \left| 1 - \sum_{j=1}^n \lambda_j \right|$$
$lnext
$latex \max_j 
| \gamma  - \partial_{\lambda(j)}  F(B, \lambda ) | 
$$
$lnext
$latex \max_{j,k} | F_B (B, \lambda)_{i,j} | $$.
$lend
where $latex \gamma \in \R$$ is given by
$latex \[
\gamma =
\left.
\sum_{j=1}^J \partial_{\lambda(j)} F( B , \lambda ) \lambda_j^2
\right/
\sum_{j=1}^J \lambda_j^2
\] $$


$subhead level >= 2$$
The weights corresponding to each discrete measure point is printed
(with the label $code lambda$$)
at each iteration.
In addition, the minimum distance from each discrete measure point
to another discrete measurement point is printed
with the label $code distance$$.


$subhead level >= 10$$
If $italic level$$ is greater than or equal ten,
$syntax%mod(%level%, 10)%$$ is used as a tracing level
for $code opt_measure$$ and 
$syntax%%level% / 10%$$ is used as a tracing level for the
corresponding call to 
$code QuasiNewton01Box$$.

$head model$$
The argument $italic model$$ has prototype
$syntax%
	SpkModel<%Scalar%> &%model%
%$$
where $italic Scalar$$ is the type $code CppAD::AD<double>$$.
It specifies the individual and population model
functions as per the $cref/non_par_model/$$ requirements.


$head N$$
The argument $italic N$$ has prototype
$syntax%
	const DoubleMatrix &%N%
%$$
The value $syntax%%N%.nr()%$$ must be equal to
$cref/M/non_par_model/Notation/M/$$
the number of individuals in the $italic model$$.
The value $syntax%%N%.nc()%$$ must be equal one.
For $latex i = 0 , \ldots , M-1$$,
$syntax%
	*(%N%.data() + %i%)
%$$
is equal to
$cref/N[i]/non_par_model/Notation/N[i]/$$
the number of measurements corresponding to individual $italic i$$
in the $italic model$$.


$head y$$
The argument $italic y$$ has prototype
$syntax%
	const DoubleMatrix &%y%
%$$
The value $syntax%%y%.nr()%$$ must be equal to
$latex N[0] + \cdots + N[M-1]$$.
The value $syntax%%y%.nc()%$$ must be equal one.
For $latex i = 0 , \ldots , M-1$$ and $latex j = 0 , \ldots , N[i]$$
$syntax%
	*(%y%.data() + %N%[0] + ... + %N%[%i%-1] + %j%)
%$$  
Is the $th j$$ data value corresponding to individual $italic i$$.

$head epsilon$$
The argument $italic epsilon$$ has prototype
$syntax%
	ublas::matrix<double> %epsilon%
%$$
and size $latex 4 \times 1$$.
It specifies the 
$table
$bold Description$$ $cnext $bold Name$$ $cnext $bold Suggest Value$$ $rnext
$cref/convergence criteria/spk_non_par/epsilon/Convergence Criteria/$$
$cnext $latex \varepsilon_0 = epsilon(0,0)$$ 
$cnext $pre  $$ $latex 10^{-4}$$
$rnext
$cref/joining criteria/spk_non_par/epsilon/Joining Criteria/$$
$cnext $latex  \varepsilon_1 = epsilon(1,0)$$ 
$cnext $pre  $$ $latex 10^{-4}$$
$rnext
$cref/sub-convergence criteria/spk_non_par/epsilon/Sub-Convergence Criteria/$$
$cnext $latex \varepsilon_2 = epsilon(2,0) $$ 
$cnext $pre  $$ $latex 10^{-13}$$
$rnext
$cref/relaxation factor/spk_non_par/epsilon/Relaxation Factor/$$
$cnext $latex \varepsilon_3 = epsilon(3,0) $$ 
$cnext $pre  $$ $latex 2^{-2}$$
$tend

$subhead Notation$$
We define the scaled projected gradient of $latex F$$
with respect to $latex B$$,
$latex F_B (B , \lambda ) \in \R^{J \times n} $$ by
$latex \[
F_B ( B , \lambda )_{j,k} = \left\{ \begin{array}{ll}
( bup_k - B_{j,k} ) \partial_{B(j,k)} F(B, \lambda)
	& {\rm if} \; \partial_{B(j,k)} F(B, \lambda) \geq 0
\\
( B_{j,k} - blow_k ) \partial_{B(j,k)} F(B, \lambda)
	& {\rm if} \; \partial_{B(j,k)} F(B, \lambda) \leq 0
\end{array} \right.
\] $$
We define $latex \gamma \in \R$$ by
$latex \[
\gamma =
\left.
\sum_{j=1}^J \partial_{\lambda(j)} F( B , \lambda ) \lambda_j^2
\right/
\sum_{j=1}^J \lambda_j^2
\] $$
We define the scaled infinity norm between $latex B_j$$ and $latex B_q$$ by
$latex \[
\| B_j - B_q \|_\infty =
\max_k \frac{ | B_{k,j} - B_{k,q} | } { bup_k - blow_k }
\] $$

$subhead Convergence Criteria$$
The output values for $latex B$$ and $latex \lambda$$ satisfy the
following approximate first order conditions for a minimum:
$list number$$
for $latex j = 1 , \ldots , J$$, $latex k = 1 , \ldots , p$$,
$latex blow_k \leq B_{j,k} \leq bup_k$$.
$lnext
$latex \varepsilon_0 \geq | 1 - \sum_{j=1}^J \lambda_j |$$ 
$lnext
for $latex j = 1 , \ldots , J$$, 
$latex \varepsilon_0 \geq  
| \gamma  - \partial_{\lambda(j)}  F(B, \lambda ) | \lambda_j
$$
$lnext
for $latex j = 1 , \ldots , J$$, $latex k = 1 , \ldots , p$$,
$latex 
\varepsilon_0 \geq | F_B (B, \lambda)_{j,k} | 
$$.
$lend

$subhead Joining Criteria$$
If there are two columns of $latex B$$ such that
$latex \[
	\varepsilon_1 \geq \| B_j - B_q \|_\infty 
\] $$
the two columns are joined, 
the weights are added,
the column dimension of $italic B$$ ($latex J$$) is reduced by one,
and the column dimension of $italic lambda$$ is reduced by one.

$subhead Sub-Convergence Criteria$$
The sub-problem, which determines the optimal weight vector 
$latex \lambda$$ is considered converged when the maximum residual
in any of the Karush Kuhn Tucker conditions is less than
$latex \varepsilon_2$$ times the maximum element in the
matrix that defines the sub-problem objective; i.e.
the likelihood of each individuals measurement vector
for each discrete measurement point.

$subhead Relaxation Factor$$
The amount the sub-problem relaxation factor is decreased each iteration.

$head blow$$
The argument $italic blow$$ has prototype
$syntax%
	const DoubleMatrix &%blow%
%$$
The value $syntax%%blow%.nr()%$$ must be equal to
the number of random effects
$cref/n/non_par_model/Notation/n/$$. 
The value $syntax%%blow%.nc()%$$ must be equal one.
It specifies a lower limit for the random effects.


$head bup$$
The argument $italic bup$$ has prototype
$syntax%
	const DoubleMatrix &%bup%
%$$
The value $syntax%%bup%.nr()%$$ must be equal to
the number of random effects
$cref/n/non_par_model/Notation/n/$$. 
The value $syntax%%bup%.nc()%$$ must be equal one.
It specifies an upper limit for the random effects.

$head Bin$$
The argument $italic Bin$$ has prototype
$syntax%
	const DoubleMatrix &%Bin%
%$$
Each column of $italic Bin$$ represents an initial guess for the location 
of the atomic measure points $latex \{ B_j \}$$ that solve the problem.
The value $syntax%%Bin%.nc()%$$ defines the initial value of $latex J$$; i.e.,
the number of atomic points in the non-parametric measure
for the random effects.
It must be greater than or equal
the number of individuals in the study
$cref/M/non_par_model/Notation/M/$$. 
The value $syntax%%bup%.nr()%$$ must be 
equal to $cref/n/non_par_model/Notation/n/$$ 
the number if random effects per atomic point in the measure.

$head Bout$$
The argument $italic Bout$$ has prototype
$syntax%
	DoubleMatrix &%Bout%
%$$
It must have the same sizes as $italic Bin$$.
The input value and dimension of $italic Bout$$ does not matter.
Upon return from $code spk_non_par$$,
$italic Bout$$ contains the location of the atomic measure points 
corresponding to the solution of the optimization problem.

$head lamout$$
The argument $italic lamout$$ has prototype
$syntax%
	DoubleMatrix &%lamout%
%$$
The input value and dimension of $italic lamout$$ does not matter.
Upon return from $code spk_non_par$$, $italic lamout$$ is a column
vector (with the same number of rows as $italic Bout$$) containing 
the weights corresponding to each of the atomic measure points
in the solution to the optimization problem.

$head pout$$
The argument $italic pout$$ has prototype
$syntax%
	DoubleMatrix &%pout%
%$$
The input value and dimension of $italic pout$$ does not matter.
Upon return from $code spk_non_par$$, 
the $latex (i, j)$$ element of $italic pout$$
is the probability density for 
$latex y^i$$ given the random effects value $latex b$$ 
is equal to the $th j$$ column of $italic Bout$$; i.e. 
$latex \[
	pout [ i * J + j ] = \B{p} ( y^i | b = B_j )
\] $$
where $italic B_j$$ is the $th j$$ column of $italic Bout$$
and $latex J$$ is the column dimenson of $italic Bout$$
(which may not be the same as the column dimension of $italic Bin$$).

$children%
	non_par_model.omh
%$$

$head Example$$
$code
$verbatim%spk_non_par_test.cpp%$$
$$


$end
-----------------------------------------------------------------------
*/
# include <CppAD/CppAD.h>
# include <non_par/opt_measure.hpp>
# include <mat2cpp.hpp>
# include <cmath>
# include <valarray>

# include "DoubleMatrix.h"
# include "spk_non_par.h"
# include "SpkModel.h"
# include "SpkException.h"
# include "SpkError.h"

namespace { // define the class Like in the empty namespace

class Like {
	typedef SpkModel< CppAD::AD<double> > ADModel;
	typedef SpkModel<double>                Model;
private:
	ADModel           *admodel_;
	Model               *model_;
	const double            *N_;
	const double            *y_;
	const size_t             M_;
	const size_t             n_;

public:
	Like(
		ADModel            &admodel       ,
		Model              &model         , 
		const DoubleMatrix &N             , 
		const DoubleMatrix &y             , 
		size_t              n             )
	: model_(&model), admodel_(&admodel),
	  N_(N.data()), y_(y.data()), M_(N.nr()), n_(n)
	{	}


	mat2cpp::matrix<double> operator() 
	(const mat2cpp::matrix<double> &beta) const
	{	// Future Optimization: make valarrays below private data
		// so do not need to reallocate for each call

		// check arguments
		assert(beta.size2() == n_ );
		assert(beta.size1() == 1 );
		// return matrix
		mat2cpp::matrix<double> psi(M_, 1);
		// random effects argument to model functions
		std::valarray<double> b(n_);
		// model for the mean of the measurement
		std::valarray<double> fi;
		// residual 
		std::valarray<double> ri;
		// wieghted residual 
		std::valarray<double> wi;
		// model for the variance of the measurement
		std::valarray<double> Ri;
		// log of determinant of Ri
		int signdet;
		double logdet;
		// number of measurement for current individual
		size_t Ni;
		// temporary indices
		size_t i, j, k;
		// constants
		double pi = 4. * std::atan(1.);
		// initialization
		size_t offset = 0;
		double sum;

		// for each individual in the study 
		for(i = 0; i < M_; i++)
		{	// set model for this individual
			model_ -> selectIndividual(i);
			// set random effect in model
			for(j = 0; j < n_; j++)
				b[j] = beta(0, j);
			model_ -> setIndPar(b);
			// evaluate mean
			Ni = size_t( *(N_ + i) );
			fi.resize(Ni);
			model_ -> dataMean(fi);
			// evaluate variance
			Ri.resize(Ni * Ni);
			model_ -> dataVariance(Ri);
			// compute the residual
			ri.resize(Ni);
			wi.resize(Ni);
			for(k = 0; k < Ni; k++)
				ri[k] = y_[offset + k] - fi[k];
			offset += Ni;
			// residual weighted by Ri^{-1} 
			signdet = CppAD::LuSolve(Ni, 1, Ri, ri, wi, logdet);
			assert( signdet == 1 );
			// sum of squrares term in objective
			sum = 0.;
			for(k = 0; k < Ni; k++)
				sum += ri[k] * wi[k];
			// log determinant term in objective
			sum += logdet + Ni * log(2. * pi);
			// convert to likelihood
			psi(i, 0) = exp( - sum / 2. );
		}
		return psi;
	} 

	mat2cpp::matrix< CppAD::AD<double> > operator() 
	(const mat2cpp::matrix< CppAD::AD<double> > &beta) const
	{	// Future Optimization: make valarrays below private data
		// so do not need to reallocate for each call

		// check arguments
		assert(beta.size2() == n_ );
		assert(beta.size1() == 1 );
		// return matrix
		mat2cpp::matrix< CppAD::AD<double> > psi(M_, 1);
		// random effects argument to model functions
		std::valarray< CppAD::AD<double> > b(n_);
		// model for the mean of the measurement
		std::valarray< CppAD::AD<double> > fi;
		// residual 
		std::valarray< CppAD::AD<double> > ri;
		// wieghted residual 
		std::valarray< CppAD::AD<double> > wi;
		// model for the variance of the measurement
		std::valarray< CppAD::AD<double> > Ri;
		// log of determinant of Ri
		int signdet;
		 CppAD::AD<double>  logdet;
		// number of measurement for current individual
		size_t Ni;
		// temporary indices
		size_t i, j, k;
		// constants
		 CppAD::AD<double>  pi = 4. * std::atan(1.);
		// initialization
		size_t offset = 0;
		CppAD::AD<double>  sum;

		// for each individual in the study 
		for(i = 0; i < M_; i++)
		{	// set model for this individual
			admodel_ -> selectIndividual(i);
			// set random effect in model
			for(j = 0; j < n_; j++)
				b[j] = beta(0, j);
			admodel_ -> setIndPar(b);
			// evaluate mean
			Ni = size_t( *(N_ + i) );
			fi.resize(Ni);
			admodel_ -> dataMean(fi);
			// evaluate variance
			Ri.resize(Ni * Ni);
			admodel_ -> dataVariance(Ri);
			// compute the residual
			ri.resize(Ni);
			wi.resize(Ni);
			for(k = 0; k < Ni; k++)
				ri[k] = y_[offset + k] - fi[k];
			offset += Ni;
			// residual weighted by Ri^{-1} 
			signdet = CppAD::LuSolve(Ni, 1, Ri, ri, wi, logdet);
			assert( signdet == 1 );
			// sum of squrares term in objective
			sum = 0.;
			for(k = 0; k < Ni; k++)
				sum += ri[k] * wi[k];
			// log determinant term in objective
			sum += logdet + Ni * log(2. * pi);
			// convert to likelihood
			psi(i, 0) = exp( - sum / 2. );
		}
		return psi;
	} 
};

} // end of empty namespace

extern void spk_non_par(
	size_t                             level       ,
	SpkModel< CppAD::AD<double> >     &admodel     ,
	SpkModel<double>                  &model       ,
	const DoubleMatrix                &N           ,
	const DoubleMatrix                &y           ,
	const DoubleMatrix                &epsilon     ,
	const DoubleMatrix                &blow        ,
	const DoubleMatrix                &bup         ,
	const DoubleMatrix                &Bin         ,
	DoubleMatrix                      &Bout        ,
	DoubleMatrix                      &lamout      ,
	DoubleMatrix                      &pout  )
{
	// temporary indices
	size_t i, j, k;

	// temporary double pointer
	double       *ptr;
	const double *ptr_c;

	// number of discrete measure points
	size_t J = Bin.nc();

	// number of random effects
	size_t n = blow.nr();
	
	// ------------ Arguments to non_par::opt_measure --------------------
	mat2cpp::matrix<double> eps(4, 1);
	eps(0, 0)    = *(epsilon.data() + 0);
	eps(1, 0)    = *(epsilon.data() + 1);
	eps(2, 0)    = *(epsilon.data() + 2);
	eps(3, 0)    = *(epsilon.data() + 3);

	// input likelihood function
	Like like(admodel, model, N, y, n);

	// input number of individuals in the population
	size_t M = N.nr();

	// input lower limit for the random effects
	mat2cpp::matrix<double> xLow(1, n);
	ptr_c = blow.data();
	for(k = 0; k < n; k++)
		xLow(0, k) = ptr_c[k];

	// input upper limit for the random effects
	mat2cpp::matrix<double> xUp(1, n);
	ptr_c = bup.data();
	for(k = 0; k < n; k++)
		xUp(0, k) = ptr_c[k];

	// input and return discrete measure points
	mat2cpp::matrix<double> X(J, n);
	ptr_c = Bin.data();
	for(j = 0; j < J; j++)
	{	for(k = 0; k < n; k++)
			X(j, k) = ptr_c[k + j * n];
	}

	// return weight corresponding to each measure oint
	mat2cpp::matrix<double> lambda(J, 1);

	// return convergence information
	mat2cpp::matrix<double> info; 

	// return status message
	const char *msg;

	// -----------------------------------------------------------------
	msg = non_par::opt_measure( level, eps, 
		&like, M, xLow, xUp, X, lambda, info
	);
	// -----------------------------------------------------------------

	if( strcmp(msg, "ok") != 0 )
	{	throw SpkException(
      			SpkError::SPK_NON_PAR_ERR,
			msg,
      			__LINE__,
      			__FILE__ 
		);
	}
	// determine number of discrete measure points
	assert( n == X.size2() );
	J = X.size1();

	// dimension the return matrices
	Bout.resize(n, J);
	lamout.resize(1, J);
	pout.resize(M, J);

	// retrun elements of Bout
	ptr = Bout.data();
	for(j = 0; j < J; j++)
	{	for(k = 0; k < n; k++)
			ptr[k + j * n] = X(j, k);
	}

	// return elements of lamout 
	ptr = lamout.data();
	for(j = 0; j < J; j++)
		ptr[j] = lambda(j, 0);

	// return elements of pout
	mat2cpp::matrix<double> beta(1, n);
	mat2cpp::matrix<double> psi(M, 1);
	ptr = pout.data();
	for(j = 0; j < J; j++)
	{	for(k = 0; k < n; k++)
			beta(0, k) = X(j, k);
		psi = like(beta);
		for(i = 0; i < M; i++)
			ptr[ i + j * M ] = psi(i, 0);
	}
	return;
}
