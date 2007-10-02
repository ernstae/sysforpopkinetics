# ifndef NON_PAR_OPT_MEASURE
# define NON_PAR_OPT_MEASURE
/* ----------------------------------------------------------------------------
$begin opt_measure$$ 
$spell
	eps
	Karush
	Kuhn
	std::cout
	mu
	itr
	non_par
	const
	cpp
	ublas
$$

$comment Latex macros local to this file$$
$latex \newcommand{\R}{{\bf R}}$$
$latex \newcommand{\T}{{\rm T}}$$

$section Non-Parametric Population Optimal Measure$$

$index measure, optimal$$
$index optimal, measure$$

$head Syntax$$
$syntax%%msg% = non_par::opt_measure(%level%, %max_itr%, %epsilon%, %
	like%, %m%, %xLow%, %xUp%, %X%, %lambda%, %info%)%$$

$head Purpose$$
We define the non-parametric population objective function
$latex F : \R^{n \times p} \times \R_+^n \rightarrow \R$$ by
$latex \[
F(X, \lambda) = - \sum_{i=1}^m 
	\log \left[ \sum_{j=1}^n \psi_i ( X_j ) \lambda_j \right]
\] $$
where $latex \R_+$$ denotes the non-negative real numbers,
$latex X_j$$ denotes the $th j$$ row of the matrix $italic X$$,
and $latex \psi_i ( \beta )$$ is the likelihood of the $th i$$ subjects
data given the $th i$$ subjects parameter vector is $latex \beta$$.
The $code opt_measure$$ routine determines a vector $latex \lambda$$
and matrix $latex X$$ that solves the problem
$latex \[
\begin{array}{ll}
{\rm minimize}      
	& F(X, \lambda )
	\; , \;
	{\rm w.r.t.} \; \lambda \in \R_+^n \; , \; X \in \R^{n \times p}
\\
{\rm subject \; to} 
	& \sum_{j=1}^n \lambda_j = 1
	\; , \;
	xLow \leq X_j \leq xUp \; {\rm for} \; j = 1 , \ldots , n
\end{array}
\] $$

$head level$$
The argument $italic level$$ has prototype
$syntax%
	size_t %level%
%$$
It specifies the level of tracing to write to $code std::cout$$
during the execution of $code opt_measure$$ as follows:

$subhead level == 0$$
No tracing is done

$subhead level >= 1$$
The current iteration number $italic itr$$,
the value of the relaxation parameter $italic mu$$ and the
convergence information $italic info$$ are printed at each iteration.

$subhead level >= 2$$
The weights corresponding to each discrete measure point is printed
(with the label $code lambda$$)
at each iteration. 
In addition, the minimum infinity from each discrete measure point
to another discrete measurement point is printed
with the label $code infinity_norm$$.

$subhead level >= 10$$
If $italic level$$ is greater than or equal ten,
$syntax%mod(%level%, 10)%$$ is used as a tracing level
for $code opt_measure$$ and 
$syntax%%level% / 10%$$ is used as a tracing level for the
corresponding call to 
$code QuasiNewton01Box$$.

$head max_itr$$
The argument $italic max_itr$$ has prototype
$syntax%
	const ublas::matrix<size_t> &%max_itr%
%$$ 
and size $latex 2 \times 1$$.
The value $syntax%%max_itr%(0,0)%$$ is the maximum number
of iterations allowed to satisfy the convergence
criteria for $code opt_measure$$.
The value $syntax%%max_itr%(1,0)%$$ is the maximum number
of iterations of allowed for each of the $cref/relaxed/$$ sub-problems.

$head epsilon$$
The argument $italic epsilon$$ has prototype
$syntax%
	const ublas::matrix<double> &%epsilon%
%$$
and size $latex 5 \times 1$$.
It specifies the 
$table
$bold Description$$ $cnext $bold Name$$ $cnext $bold Suggest Value$$ $rnext
$cref/convergence criteria/opt_measure/epsilon/Convergence Criteria/$$
$cnext $latex \varepsilon_0 = epsilon(0,0)$$ 
$cnext $pre  $$ $latex 10^{-4}$$
$rnext
$cref/zero weight criteria/opt_measure/epsilon/Zero Weight Criteria/$$
$cnext $latex  \varepsilon_1 = epsilon(1,0)$$ 
$cnext $pre  $$ $latex 10^{-4}$$
$rnext
$cref/joining criteria/opt_measure/epsilon/Joining Criteria/$$
$cnext $latex  \varepsilon_2 = epsilon(2,0)$$ 
$cnext $pre  $$ $latex 10^{-4}$$
$rnext
$cref/sub-convergence criteria/opt_measure/epsilon/Sub-Convergence Criteria/$$
$cnext $latex \varepsilon_3 = epsilon(3,0) $$ 
$cnext $pre  $$ $latex 10^{-13}$$
$rnext
$cref/relaxation factor/opt_measure/epsilon/Relaxation Factor/$$
$cnext $latex \varepsilon_4 = epsilon(4,0) $$ 
$cnext $pre  $$ $latex 2^{-2}$$
$tend

$subhead Convergence Criteria$$
We define the scaled projected gradient of $latex F$$
with respect to $latex X$$,
$latex F_X (X , \lambda ) \in \R^{n \times p} $$ by
$latex \[
F_X ( X , \lambda )_{j,k} = \left\{ \begin{array}{ll}
( xUp_k - X_{j,k} ) \partial_{X(j,k)} F(X, \lambda)
	& {\rm if} \; \partial_{X(j,k)} F(X, \lambda) \leq 0
\\
( X_{j,k} - xLow_k ) \partial_{X(j,k)} F(X, \lambda)
	& {\rm if} \; \partial_{X(j,k)} F(X, \lambda) \geq 0
\end{array} \right.
\] $$
The output values for $latex X$$ and $latex \lambda$$ satisfy the
following approximate first order conditions for a minimum:
$list number$$
for $latex j = 1 , \ldots , n$$, $latex k = 1 , \ldots , p$$,
$latex xLow_k \leq X_{j,k} \leq xUp_k$$.
$lnext
$latex \varepsilon_0 \geq | 1 - \sum_{j=1}^n \lambda_j |$$ 
$lnext
for $latex j = 1 , \ldots , n$$, 
$latex \varepsilon_0 \geq  
| \gamma - \partial_{\lambda(j)}  F(X, \lambda ) | \lambda_j 
$$
$lnext
for $latex j = 1 , \ldots , n$$, $latex k = 1 , \ldots , p$$,
$latex 
\varepsilon_0 \geq | F_X (X, \lambda)_{i,j} | 
$$.
$lend
where the value $latex \gamma \in \R $$ is given by
$latex \[
\gamma = \left.
	\sum_{j=1}^n \partial_{\lambda(j)}  F(X, \lambda )  \lambda_j^2
	\right/
	\sum_{j=1}^n \lambda_j^2
\] $$

$subhead Zero Weight Criteria$$
If at the end of an iteration
$latex \lambda_j \leq \varepsilon_1 \| \lambda \|_\infty$$,
the $th j$$ discrete measure point is removed.

$subhead Joining Criteria$$
If $latex X_j \in \R^{1 \times p}$$ and $latex X_q \in \R^{1 \times p}$$ 
are two discrete measurement points, we define the infinity 
norm distance between them as
$latex \[
\| X_j - X_q \|_\infty = \max_k \left\{ 
	\frac{| X_{j, k} - X_{q, k} |}{ xUp_k - xLow_k }
\right\}
\]$$
Note that this norm is bounded by one.
If at the end of an iteration,
$latex \[
	\| X_j - X_q \|_\infty \leq \varepsilon_2
\] $$,
the two discrete points are combined 
(and their weights are summed)
to form one discrete measure point.

$subhead Sub-Convergence Criteria$$
The sub-problem, which determines the optimal weight vector 
$latex \lambda$$ is considered converged when the maximum residual
in any of the Karush Kuhn Tucker conditions is less than
$latex \varepsilon_3$$; i.e.
the likelihood of each individuals measurement vector
for each discrete measurement point
(see $cref/eps in relaxed/relaxed/option/eps/$$ for a precise definition).

$subhead Relaxation Factor$$
The amount the sub-problem relaxation factor is decreased each iteration; i.e.,
the parameter $cref/mu/Bender/mu/$$ in $code Bender$$.

$head like$$
The $code opt_measure$$ argument $italic like$$ 
is a C++ function object that supports the syntax
$syntax%
	%psi% = (*%like%)(%beta%)
%$$

$subhead beta$$
The $italic like$$ argument $italic beta$$ has prototype
$syntax%
	const ublas::matrix<%Float%> &%beta%
%$$
and size $latex 1 \times p$$.
The type $italic Float$$ is either $code double$$
or it is $code CppAD::AD<double>$$.

$subhead psi$$
The $italic like$$ return value $italic psi$$ has prototype
$syntax%
	ublas::matrix<%Float%> %psi%
%$$
and has size $latex m \times 1$$.
The $th i$$ element of the return vector $italic psi$$ is 
the likelihood of the $th i$$ individual's data given
that its parameter values are equal to $latex beta$$.
The type $italic Float$$ is either $code double$$
or it is in the AD sequence of types about $code double$$.

$head m$$
The $code opt_measure$$ argument $italic m$$  head prototype
$syntax%
	size_t %m%
%$$
It specifies the number of individual in the study.
This must be the same as the number of row in the 
return value of $italic psi$$ above.

$head xLow$$
The $code opt_measure$$ argument
$italic xLow$$ has prototype
$syntax%
	const ublas::matrix<double> &%xLow%
%$$
and size $latex 1 \times p$$.
It specifies the lower limit for each of the rows of $italic X$$.

$head xUp$$
The $code opt_measure$$ argument
$italic xUp$$ has prototype
$syntax%
	const ublas::matrix<double> &%xUp%
%$$
and size $latex 1 \times p$$.
It specifies the upper limit for each of the rows of $italic X$$.

$head X$$
The $code opt_measure$$ argument $italic X$$ has prototype
$syntax%
	ublas::matrix<double> &%X%
%$$
and size $latex n \times p$$.
For $latex j = 1 , \ldots , n$$, the $th j$$ row of 
$italic X$$ specifies a value for the parameters.
The input value of $italic X$$ specifies where the optimization
starts and it's output value is where it ends.
The number of rows in the output value of $italic X$$
may be less than the number in the input value of $italic X$$,
but will be the same as the number of rows in $italic lambda$$.

$head lambda$$
The $code opt_measure$$ argument $italic lambda$$ has prototype
$syntax%
	ublas::matrix<double> &%lambda%
%$$
and size $latex n \times 1$$. 
The input value of the elements of $latex lambda$$ does not matter.
On output, it is where the optimization ends.
The number of rows in the output value of $italic lambda$$
may be less than the number in the input value of $italic lambda$$,
but will be the same as the number of rows in $italic X$$.

$head info$$
The $code opt_measure$$ argument
$italic info$$ has prototype
$syntax%
	ublas::matrix<double> &%info%
%$$
The size and element values of $italic info$$ on input does not matter.
On output, $italic info$$ has one row for each iteration 
and the information for iteration $latex \ell$$ is 
$latex \[
\begin{array}{rcl}
info(\ell, 0) & = & \left| 1 - \sum_{j=1}^n \lambda_j^\ell \right|
\\
info(\ell, 1) & = & \max_j \left\{ | 
	\gamma^\ell
	- \partial_{\lambda(j)}  F(X^\ell, \lambda^\ell ) 
| \right\}
\\
info(\ell, 2) & = & \max_{j,k} \{ | F_X ( X^\ell, \lambda^\ell )_{j,k} | \\
\\
info(\ell, 3) & = & F( X^\ell , \lambda^\ell ) 
\\
info(\ell, 4) & = &  \gamma^\ell
\\
info(\ell, 5) & = & n
\end{array}
\] $$
where the value $latex \gamma^\ell \in \R $$ is given by
$latex \[
\gamma^\ell = \left.
	\sum_{j=1}^n \partial_{\lambda(j)}  F(X^\ell , \lambda^\ell )  
	(\lambda_j^\ell)^2
	\right/
	\sum_{j=1}^n (\lambda_j^\ell)^2
\] $$

$head msg$$
If the return value of $italic msg$$ is equal to $code ok$$,
the routine $code opt_measure$$ succeeded.
Otherwise the other return values are undefined
and $italic msg$$ is an error message.


$children%
	cpp/opt_measure_ok.cpp
%$$
$head Example$$
The section $cref/opt_measure_ok/$$ contains an example and test
of $code opt_measure$$.

$end
----------------------------------------------------------------------------
*/
# include <mat2cpp.hpp>
# include <QN01Box/QuasiNewton01Box.h>
# include <QN01Box/zero_one_scale.h>
# include <non_par/relaxed.hpp>
# include <non_par/bender.hpp>


namespace non_par{ // BEGIN  non_par namespace

// Non-Parameteric population objective function objective
// F (x , lambda) = - sum_{i=1}^m \log[ sum_{j=1}^n psi_i ( X_j ) * lambda_j ]

template <class Like>
CppAD::AD<double> opt_measure_F(
	Like *like   ,  // likelihood evaluator
	size_t m     ,  // number of individuals
	size_t n     ,  // number of discrete points in measure
	size_t p     ,  // dimension of parameter space measure defined on
	// first n * p elements of X_lambda is the X matrix 
	// other n elements is the lambda weighting vector
	const CppAD::vector< CppAD::AD<double> > &X_lambda )
{	using CppAD::AD;

	// std::cout << "begin F::function:" << std::endl;
	typedef mat2cpp::matrix< AD<double> > Matrix;
	size_t i, j, k;

	Matrix beta(1, p);  // a parameter vector value
	Matrix psi(m, 1);   // likelihood for of one individual's data
	Matrix Psi(m, n);   // likelihood for all individuals 

	// Determine Psi
	for(j = 0; j < n; j++)
	{	for(k = 0; k < p; k++)
			beta(0, k) = X_lambda[ j * p + k ];
		psi = (*like)(beta);
		for(i = 0; i < m; i++)
			Psi(i, j) = psi(i, 0);
	}

	// compute the objective
	AD<double> F = 0.;
	for(i = 0; i < m; i++)
	{	AD<double> sum = 0.;
		for(j = 0; j < n; j++)
			sum += Psi(i, j) * X_lambda[n * p + j];
		F -= log(sum);
	}
	return F;
}

// Non-Parameteric population relaxed objective function objective
// F_mu (x , y) = - sum_{i=1}^m log[ sum_{j=1}^n psi_i ( X_j ) * lambda_j ]
//          - mu \sum_{j=1}^n log( lambda_j )
//          +  m * ( \sum_{j=1}^n \lambda_j - 1 )
// where y = ( lambda , w , s).
template <class Like>
class opt_measure_F_mu
{
	typedef mat2cpp::matrix<double> Dblmatrix;
private:
	Like           *like;
	// private variables used to communicate information
	size_t          max_itr_1;
	double          epsilon_3;
	double          mu;
	size_t          m;
	size_t          n;
	size_t          p;   
	Dblmatrix       X;
	Dblmatrix       y;
	// temporaries declared here to reduce memory allocation
	Dblmatrix       beta, psi, Psi, lam, w, option;
	Dblmatrix       info, Psi_lam, PsiT_w, f, g, H, empty;
public:
	opt_measure_F_mu(Like *like_, size_t max_itr_1_, double epsilon_3_, 
		double mu_, size_t m_, size_t n_, size_t p_) : 
	like(like_),            // likelihood evaluator
	max_itr_1(max_itr_1_),  // subproblem maximum number of iterations
	epsilon_3(epsilon_3_),  // subproblem convergence criteria
	mu(mu_),  // relaxation parameter
	m(m_),    // number of individuals
	n(n_),    // number of discrete points in measure
	p(p_),    // dimension of parameter space measure defined on
	X(n, p),             // discrete measure point
	y(n + m + n, 1),     // argmin F(x,y) w.r.t y = (lam , w , s)
	// 
	beta(1, p),          // a parameter vector value
	psi(m, 1),           // likelihood corresponding to each individual
	Psi(m, n),           // likelihood for each individual, measure point
	lam(n, 1),           // weight for each measurement point
	w(m, 1),             // dual variable in subproblem
	option(2, 1),        // convergence criteria for subproblem
	info(0, 0),          // subproblem convergence information 
	Psi_lam(m, 1),       // Psi * lam
        PsiT_w(n, 1),        // Psi^T * w
	//
	f(1, 1),             // value of relaxed objective
	g(n*p, 1),           // gradient of relaxed objective
	H(n*p, n*p),         // Hessian of relaxed objective
	empty(0, 0)          // the empty matrix
	{ }
	const char *function(const double *x_, double &f_)
	{	size_t i, j, k;
		// std::cout << "begin F_mu::function:" << std::endl;

		// copy the x argument to Matrix form
		for(j = 0; j < n; j++)
		{	for(k = 0; k < p; k++)
			{	X(j, k) = x_[j * p + k];
			}
		}

		// determine the Psi matrix for this argument
		for( j = 0; j < n; j++)
		{	for(k = 0; k < p; k++)
				beta(0, k) = X(j, k);
			psi = (*like)(beta);
			for(i = 0; i < m; i++)
				Psi(i, j) = psi(i, 0);
		}

		// arguments to non_par::relaxed
		option(0, 0) = epsilon_3;
		option(1, 0) = size_t( max_itr_1 );

		// compute argmin of F(x, y) w.r.t. y
		f_ = relaxed(Psi, mu, option, lam, w, info);
		size_t nr = info.size1();
		if( nr == 0 )
		{	const char *msg = 
			"opt_measure:: sub-problem error, info.size1() == 0";
			return msg;
		}
		if( info(nr-1,1) > epsilon_3 || info(nr-1,2) > epsilon_3 )
		{	const char *msg = 
			"opt_measure:: sub-problem did not converge.";
			return msg;
		}

		// Psi^T * w
		PsiT_w  = prod(trans(Psi), w);

		// lam
		for(j = 0; j < n; j++)
			y(j, 0) = lam(j, 0);
		// w
		for(i = 0; i < m; i++)
			y(n+i, 0) = w(i, 0);
		// s = m - Psi^T * w
		for(j = 0; j < n; j++)
			y(n+m+j, 0) = m - PsiT_w(j, 0); 

		// std::cout << "end F_mu::function:" << std::endl;
		return "ok";
	} 
	const char *gradient(double *g_)
	{	// should change Bender to pass pointer to function
		Bender(like, mu, X, y, f, g, empty);
		size_t i = n * p;
		while(i--)
			g_[i] = g(i, 0);
		return "ok";
	}
	const char *Hessian(double *H_)
	{	// should change Bender to pass pointer to function
		Bender(like, mu, X, y, f, g, H);
		size_t i = n * p;
		while(i--)
		{	size_t j = n * p;
			while(j--)
				H_[i * n * p + j] = H(i, j);
		}
		return "ok";
	}
	Dblmatrix get_lam(void)
	{	return lam; }
};

template <class Like>
void opt_measure_info(
	Like                          *like      , 
	size_t                         m         ,
	const mat2cpp::matrix<double> &xLow      , 
	const mat2cpp::matrix<double> &xUp       , 
	const mat2cpp::matrix<double> &X         , 
	const mat2cpp::matrix<double> &lambda    ,
	mat2cpp::matrix<double>       &info      )
{	using CppAD::AD;
	using CppAD::vector;
	size_t j, k;

	size_t n = X.size1();
	size_t p = X.size2();
	//
	assert( info.size2() == 6 );
	//
	assert( lambda.size1() == n );
	assert( lambda.size2() == 1 );
	//
	assert( xLow.size1() == 1 );
	assert( xLow.size2() == p );
	//
	assert( xUp.size1()  == 1 );
	assert( xUp.size2()  == p );
	//

	// tape computation of F(X, lambda)
	vector< AD<double> > AD_X_lambda(n * p + n);
	for(j = 0; j < n; j++)
	{	for(k = 0; k < p; k++)	
			AD_X_lambda[j * p + k] = X(j, k);
		AD_X_lambda[n * p + j] = lambda(j, 0);
	}
	CppAD::Independent(AD_X_lambda);
	vector< AD<double> > AD_F(1);
	AD_F[0] = opt_measure_F(like, m, n, p, AD_X_lambda);
	CppAD::ADFun<double> F(AD_X_lambda, AD_F);

	// compute deritative of F
	vector<double> w(1);
	w[0] = 1.;
	vector<double> F_X_lambda(n * p + n);
	F_X_lambda = F.Reverse(1, w);

	// extend info for this interation
	size_t nr = info.size1();
	bool preserve = true;
	info.resize(nr + 1, 6, preserve);

	// compute gamma
	double lambda_sq;
	double sum_lambda_sq  = 0.;
	double sum_partial    = 0;
	for(j = 0; j < n; j++)
	{	lambda_sq      = lambda(j, 0) * lambda(j, 0);
		sum_lambda_sq += lambda_sq;
		sum_partial   += lambda_sq * F_X_lambda[n * p + j];
	}
	double gamma = sum_partial / sum_lambda_sq;
	// | 1 - lambda(0) + ... + lambda(n-1) |;
	double sum = 0;
	for(j = 0; j < n; j++)
		sum += lambda(j, 0);
	info(nr, 0) = fabs(1 - sum);

	// first order condition w.r.t. lambda
	double v_max = 0.;
	for(j = 0; j < n; j++)
	{	double vj = std::abs(gamma - F_X_lambda[n * p + j]);
		vj       *= lambda(j, 0);
		v_max = std::max(vj, v_max);
	}
	info(nr, 1) = v_max;

	// max absolute scaled projected gradient w.r.t. X
	double g_max = 0.;
	for(j = 0; j < n; j++)
	{	for(k = 0; k < p; k++)
		{	double partial = F_X_lambda[ j * p + k];
			double gjk; 
			if( partial >= 0 )
				gjk = partial * (X(j,k) - xLow(0, k));
			else	gjk = partial * (xUp(0, k) - X(j,k));
			g_max = std::max(gjk, g_max);
		}
	}
	info(nr, 2) = g_max;

	// value of the objective function
	info(nr, 3) = Value( AD_F[0] );

	// value of gamma
	info(nr, 4)  = gamma;

	// value of n
	info(nr, 5) = double(n);
}

template <class Like>
const char *opt_measure_iterate(
	double                         mu          ,
	size_t                         qn01_level  ,
	size_t                         max_itr_1   ,
	double                         epsilon_3   ,
	Like                          *like        , 
	size_t                         m           ,
	const mat2cpp::matrix<double> &xLow        , 
	const mat2cpp::matrix<double> &xUp         , 
	mat2cpp::matrix<double>       &X           , 
	mat2cpp::matrix<double>       &lambda      )
{	
	// temporary indices
	size_t j, k;

	// inputs to opt_measure
	size_t        n       = X.size1(); 
	size_t        p       = X.size2();
	//
	assert( xLow.size1() == 1 );
	assert( xLow.size2() == p );
	//
	assert( xUp.size1()  == 1 );
	assert( xUp.size2()  == p );
	//
	assert( lambda.size1() == n );
	assert( lambda.size2() == 1 );


	// input arguments to QuasiNewton01Box that do not change
	std::ostream &os       = std::cout;
	size_t        ItrMax   = 80;
	size_t        QuadMax  = 40;
	size_t        N        = n * p;
	QN01Box::ConvergeNorm  norm    = QN01Box::StepMaxAbs;
	// xCur, a, b
	QN01Box::Memory<double> dMemory(6 * N + N * N);
	double *xCur    = dMemory(N);
	double *a       = dMemory(N);
	double *b       = dMemory(N);
	double *yCur    = dMemory(N);
	double *sCur    = dMemory(N);
	double *gCur    = dMemory(N);
	double *HCur    = dMemory(N * N);
	for(j = 0; j < n; j++)
	{	for(k = 0; k < p; k++)
		{	xCur[ j * p + k ] = X(j, k);
			a[ j * p + k ]    = xLow(0, k);
			b[ j * p + k ]    = xUp(0, k);
		}
	}

	const char *msg;

	// function object in x coordinates
	opt_measure_F_mu<Like> F_mu(like, max_itr_1, epsilon_3, mu, m, n, p);

	// input arguments to QuasiNewton01Box that depend on itr
	double        delta   = mu;
	size_t        ItrCur  = 0;
	size_t        QuadCur = 0;
	size_t        BfgsCur = 0;
	double        rCur    = 0.5;

	// function object in scaled coordinates
	QN01Box::zero_one_scale< opt_measure_F_mu<Like> > 
		obj(&F_mu, N, N, a, b);

	// map staring point xCur to zero one coordiantes 
	obj.to_zero_one(yCur, xCur); 

	// fCur
	double fCur;
	obj.function(yCur, fCur);
	// sCur
	bool sOkCur  = false;
	// gCur
	obj.gradient(gCur);
	// HCur
	obj.Hessian(HCur);

	msg = QN01Box::QuasiNewton01Box(
		// Inputs
		os         , // output stream for tracing
		qn01_level , // level of tracing (level=0 for no tracing)
		ItrMax     , // maximum total number of iterations
		QuadMax    , // maximum # iterations per Quadratic sub-problem
		N          , // number of componets in argument vector x
		norm       , // GradSumAbs, GradMaxAbs, StepSumAbs, StepMaxAbs
		delta      , // convergence criteria for norm 
		obj        , // non-parameteric relaxed objective function
     		// Inputs and Outputs
		sOkCur     , // should optimizer use the initial sCur
		ItrCur     , // current iteration counter 
		QuadCur    , // how many quadratic subproblems so far
		BfgsCur    , // how many BFGS updates have been applied so far
		rCur       , // trust region radiuse 1e-7 <= rCur <= 0.5
		fCur       , // value of the objective function at xCur
		yCur       , // current optimal estimate
		sCur       , // current step size
		gCur       , // value of the gradient at xCur
		HCur       // value of the current approximate Hessian
	);
	if( strcmp(msg, "ok") != 0 )
		return msg;

	// convert back to original coordinates
	obj.from_zero_one(yCur, xCur);

	// calculate fCur so that we set lambda to correspond to xCur
	msg    = F_mu.function(xCur, fCur);
	lambda = F_mu.get_lam();
	if( strcmp(msg, "ok") != 0 )
		return msg;

	// get X corresponding to xCur
	for(j = 0; j < n; j++)
		for(k = 0; k < p; k++)
			X(j, k) = xCur[ j * p + k ];

	msg = "ok";
	return msg;
}

// move row index n-1 to row index j and then resize matrices to have n-1 rows 
void opt_measure_remove(
	size_t                        i              ,
	mat2cpp::matrix<double>      &X              ,
	mat2cpp::matrix<double>      &lambda         )
{
	size_t n = X.size1();
	size_t p = X.size2();

	assert( i < n );
	assert( lambda.size1() == n );
	assert( lambda.size2() == 1 );

	// replace row i by row n-1
	size_t k;
	for(k = 0; k < p; k++)
		X(i, k) = X(n-1, k);
	lambda(i, 0) = lambda(n-1, 0);

	// remove row n-1 from matrix
	bool preserve = true;
	X.resize(n-1, p, preserve);
	lambda.resize(n-1, 1, preserve);
 
	return;
}

void opt_measure_join(
	double                         delta         , 
	mat2cpp::matrix<double>       &X             , 
	mat2cpp::matrix<double>       &lambda        , 
	mat2cpp::matrix<double>       &infinity_norm ,
	const mat2cpp::matrix<double> &xLow          , 
	const mat2cpp::matrix<double> &xUp
)
{	using mat2cpp::matrix;

	size_t n = X.size1();
	size_t p = X.size2();

	assert( lambda.size1() == n );
	assert( lambda.size2() == 1 );

	assert( infinity_norm.size1() == n );
	assert( infinity_norm.size2() == 1 );

	assert( xLow.size1() == 1 );
	assert( xLow.size2() == p );

	assert( xUp.size1()  == 1 );
	assert( xUp.size2()  == p );

	// join discrete measure points that are closer than delta
	size_t j1, j2, k;
	for(j1 = 0; j1 < n; j1++)
	{	// initialize at upper bound	
		infinity_norm(j1, 0) = 1.;
		for(j2 = j1+1; j2 < n; j2++)
		{	double diff;
			double dist = 0.;
			for(k = 0; k < p; k++)
			{	diff  = (X(j1, k) - X(j2, k));
				diff /= (xUp(0, k) - xLow(0, k));
				dist  = std::max(dist, fabs(diff));
			}
			if( dist < delta )
			{	lambda(j1, 0) += lambda(j2, 0);
				opt_measure_remove(j2, X, lambda);
				// new size of matrix
				n  = n - 1;
				// need to check new value in row j2
				j2 = j2 - 1;
			}
			else	if( dist < infinity_norm(j1, 0) )
					infinity_norm(j1, 0) = dist;
		}
	}
	return;
}

template <class Like>
const char *opt_measure(
	size_t                         level   ,
	const mat2cpp::matrix<size_t> &max_itr ,
	const mat2cpp::matrix<double> &epsilon ,
	Like                          *like    , 
	size_t                         m       ,
	const mat2cpp::matrix<double> &xLow    , 
	const mat2cpp::matrix<double> &xUp     , 
	mat2cpp::matrix<double>       &X       , 
	mat2cpp::matrix<double>       &lambda  ,
	mat2cpp::matrix<double>       &info    )
{	
	// temporary indices
	size_t j, k;

	// opt_measure_level
	size_t opt_measure_level = level % 10;

	// QN01Box tracing level
	size_t qn01_level     = level / 10;

	// max_itr values
	assert( max_itr.size1() == 2 );
	assert( max_itr.size2() == 1 );
	size_t max_itr_0 = max_itr(0, 0);
	size_t max_itr_1 = max_itr(1, 0);

	// epsilon values
	assert( epsilon.size1() == 5 );
	assert( epsilon.size2() == 1 );
	double epsilon_0 = epsilon(0, 0);
	double epsilon_1 = epsilon(1, 0);
	double epsilon_2 = epsilon(2, 0);
	double epsilon_3 = epsilon(3, 0);
	double epsilon_4 = epsilon(4, 0);

	// problem dimensions
	size_t        n       = X.size1(); 
# ifndef NDEBUG
	size_t        p       = X.size2();
# endif
	//
	assert( xLow.size1() == 1 );
	assert( xLow.size2() == p );
	//
	assert( xUp.size1()  == 1 );
	assert( xUp.size2()  == p );
	//
	assert( lambda.size1() == n );
	assert( lambda.size2() == 1 );

	// add info corresponding to initial point
	bool preserve = false;
	info.resize(0, 6, preserve);
	for(j = 0; j < n; j++)
		lambda(j, 0) = 1. / double(n);
	opt_measure_info(like, m, xLow, xUp, X, lambda, info);

	// initialize value of mu
	double mu = std::max(info(0, 0), info(0, 1));
	mu = epsilon_4 * std::max(info(0, 2), mu);

	// tracing at initail point
	if( opt_measure_level >= 1 )
	{	std::cout << "itr = 0, mu = " << mu << ", info = "
		          << info(0,0) << ", " 
		          << info(0,1) << ", "
		          << info(0,2) << ", " 
		          << info(0,3) << ", " 
		          << info(0,4) << ", "
		          << info(0,5) << std::endl;
	}

	// take the initial point as the optimal point if no
	// iterations were requested
	if( max_itr_0 == 0 )
		return "ok";

	size_t itr;
	const char *msg;
	for(itr = 1; itr <= max_itr_0; itr++)
	{	msg = opt_measure_iterate(
			mu             ,
			qn01_level     ,
			max_itr_1      ,
			epsilon_3      ,
			like           , 
			m              ,
			xLow           , 
			xUp            , 
			X              , 
			lambda      
		);
		if( strcmp(msg, "ok") != 0 )
		{	if( opt_measure_level >= 1 ) 
				std::cout << msg << std::endl;
			return msg;
		}
		// remove poitns that have weight less than epsilon_1
		// times the maximum weight
		double lambda_max = mat2cpp::max(lambda);
		j = n;
		while(j--)
		{	if( lambda(j, 0) < epsilon_1 * lambda_max )
			{	opt_measure_remove(j, X, lambda);
				n = n - 1;
				// at this point, either j is equal to n 
				// or lambda(j, 0) >= epsilon_1 * lambda_max
			}
		}

		// join points that are within epsilon_2 of each other
		// and compute the infinity norm sepration for other
		mat2cpp::matrix<double> infinity_norm(n, 1);
		opt_measure_join(
			epsilon_2     , 
			X             , 
			lambda        , 
			infinity_norm ,
			xLow          , 
			xUp
		);
		// update value of n
		assert( p = X.size2() );
		n = X.size1();
		
		// compute info corresponding to the current discrete measure
		opt_measure_info(like, m, xLow, xUp, X, lambda, info);

		// tracing
		if( opt_measure_level >= 1 )
		{	std::cout << "itr = " << itr << ", mu = " << mu 
			        << ", info = "
		          	<< info(itr,0) << ", " 
		          	<< info(itr,1) << ", "
		          	<< info(itr,2) << ", " 
		          	<< info(itr,3) << ", " 
			        << info(itr,4) << ", "
			        << info(itr,5) << std::endl;
		}
		if( opt_measure_level >= 2 )
		{	std::cout << "lambda = ";
			for(j = 0; j < n; j++)
				 std::cout << lambda(j,0) << ", ";
			std::cout << std::endl << "infinity_norm = ";
			for(j = 0; j < n; j++)
				 std::cout << infinity_norm(j,0) << ", ";
			std::cout << std::endl;
		}

		// check for convergence
		bool converge = true;
		for(k = 0; k < 3; k++)
			converge &= ( info(itr, k) <= epsilon_0  );
		if( converge )
			return "ok";

		// update mu
		mu = epsilon_4 * mu;
	}
	msg = "opt_measure: did not converge";
	return msg;
}

} // END non_par namespace

# endif
