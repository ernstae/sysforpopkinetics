/*
$begin relaxed$$
$spell
	obj
	non_par
	const
	eps
	Jacobian
	itr
	Karush Kuhn Tucker
	subproblem
	mu
	complementarity
	Kuhn
	Rockafellar
	Fenchel
$$

$comment Latex macros local to this file$$
$latex \newcommand{\R}{{\bf R}}$$
$latex \newcommand{\T}{{\rm T}}$$
$latex \newcommand{\D}{{\rm D}}$$

$section Non-Parametric Relaxed Interior Point Problem$$

$index relaxed$$
$index interior, point problem$$
$index problem, interior point$$
$index point, interior problem$$

$head Syntax$$
$syntax//obj/ = non_par::relaxed(/Psi/, /t/, /option/, /lam/, /w/, /info/)/$$

$head Primal Problem$$
We are given a matrix $latex \Psi \in \R_+^{m \times n}$$
(where $latex m \leq n$$) and a relaxation parameter $latex t$$.
This routine solves the following convex programming problem:
$latex \[
\begin{array}{lll}
 (P) & {\rm minimize} \; \phi_m ( \Psi \lambda )  + t \phi_n ( \lambda )
     + m ( 1_n^T \lambda - 1 )
     & {\rm w.r.t.} \;  \lambda \in \R_+^n
\end{array}
\] $$
where $latex \phi_m : \R_+^m \rightarrow \R$$ is defined by
$latex \[
\phi_m ( z ) = \left\{ \begin{array}{ll}
	- \log( z_1 ) - \log( z_2 ) -  \cdots - \log ( z_m )
	& {\rm if} \; z > 0_m
	\\
	+ \infty & {\rm otherwise}
\end{array} \right.
\] $$
and $latex 0_m \in \R^m$$ ($latex 1_n \in \R^n$$)
is the vector with all its elements equal to zero (one).
Note that at a minimizer of the primal problem
$latex \[
	\sum_{j=1}^n \lambda_j = 1 + t \frac{n}{m}
\] $$
(see $xref/relaxed/Theory/Karush Kuhn Tucker Conditions/1/$$ below).
Thus we can add this as an extra constraint with out changing the problem.


$head Dual Problem$$
The following dual problem is derived in the
$xref/relaxed/Theory/theory/$$ section below:
$latex \[
\begin{array}{lll}
 (Q) & {\rm maximize} - \phi( w )
     & {\rm w.r.t.} \;  w \in \R_+^m \; , \; s \in \R_+^n
\\
     & {\rm subject \, to}
     & \Psi^\T w  + s = m 1_n
\end{array}
\] $$

$head Psi$$
The argument $italic Psi$$ has prototype
$syntax%
	const matrix<double> &%Psi%
%$$
The values $italic m$$ and $italic n$$ below are the
number of rows and columns in this matrix $italic Psi$$.
It must hold that $latex m \leq n$$
and $latex \Psi 1_n > 0_m$$
(no row of $italic Psi$$ is entirely zero).

$head t$$
The argument $italic t$$ has prototype
$syntax%
	double %t%
%$$.
It must be greater than or equal to zero
and specifies the relaxation factor.

$head option$$
The argument $italic option$$ has prototype
$syntax%
	const matrix<double> &%option%
%$$

$subhead eps$$
The scalar $syntax%
	%eps% = %option%(0, 0)
%$$
must be greater than zero and specifies the
convergence criteria in terms of the maximum value for
$latex q$$ and $latex r$$ corresponding to the final iterate; i.e.,
$latex \[
	\max( q , r ) \leq eps 
\] $$
Note the if an error occurs, this condition may not be satisfied,
so one should use the last row of the matrix 
$cref/info/relaxed/info/$$ to make sure this condition 
is satisfied at the final iterate.

$subhead max_itr$$
The scalar $syntax%
	%max_itr% = size_t( %option%(1, 0) )
%$$
specifies the
maximum number of iterations that $code relaxed$$ will attempt
in order to satisfy the convergence criteria.


$head lam$$
The result $italic lam$$ has prototype
$syntax%
	matrix<double> &%lam%
%$$
and has $italic n$$ rows and one column
(on input and output).
The input value of the elements of $italic lam$$ does not matter.
The output value of $italic lam$$ is greater than zero and
is an approximate solution to the primal problem $latex (P)%$$.

$head w$$
The result $italic w$$ has prototype
$syntax%
	matrix<double> &%w%
%$$
and has $italic m$$ rows and one column
(on input and output).
The return vector $italic w$$ is greater than zero.
We define the output vector
$latex \[
	s = m 1_n - \Psi^\T w
\] $$
it follows that $latex s \in \R^n$$ and $latex s > 0$$.
In addition, the pair $latex (w, s)$$
is an approximate solution for the dual problem $latex (Q)%$$.

$head info$$
The result $italic info$$ has prototype
$syntax%
	matrix<double> &%info%
%$$
The size and element values of $italic info$$ on input does not matter.
On output, $italic info$$ has one row for each iteration 
(zero rows corresponds to an error condition) and
$latex \[
\begin{array}{rcl}
	info(k, 0) & = & \mu^k 
	\\
	q^k        & = & \max_{ i = 1, \cdots , n} 
		\left| \lambda_i^k s_i^k - t \right|
	\\
	info(k, 1) & = & q^k 
	\\
	r^k & = & \max_{i = 1 , \cdots , m}
		\left| [ \D ( w^k ) \Psi \lambda^k ]_i -  1 \right|
	\\
	info(k, 2) & = & r^k 
\end{array}
\] $$
where $latex w^k$$ and $latex \lambda^k$$ are the 
approximations for the corresponding output vectors at iteration $latex k$$ and
$latex s^k = m 1_n - \Psi^\T w^k$$.
The value $latex \mu^k$$ is defined in the
$xref/relaxed/Theory/Newton Step/Newton step/1/$$ discussion below.
The terms $latex q^k$$ and $latex r^k$$ are the maximum
absolute residuals in the
$xref/
	relaxed/
	Theory/
	Karush Kuhn Tucker Conditions/
	Karush Kuhn Tucker conditions/
	1
/$$.

$head obj$$
The return value $italic obj$$ is the value of the primal objective
function at the final solution; i.e.,
$latex \[
obj = \phi_m ( \Psi \lambda )  + t \phi_n ( \lambda ) + m ( 1_n^T \lambda - 1 )
\] $$


$children%
	cpp/relaxed_ok.cpp
%$$
$head Example$$
The routine $code relaxed_ok$$ (in the file $xref/relaxed_ok.cpp/$$)
is an example and test of $code relaxed$$.
It returns true for success and false for failure.

$head Theory$$

$subhead Extended Primal Problem$$
The $xref/relaxed/Primal Problem/primal problem/$$ is equivalent
to the problem
$latex \[
\begin{array}{llll}
 (P) & {\rm minimize}
     & \phi_m ( z )  + t \phi_n ( \lambda )
     + m ( 1_n^T \lambda - 1 )
     & {\rm w.r.t.} \;  z \in \R_+^m \; , \; \lambda \in \R_+^n
 \\
     & {\rm subject \; to}
     & z \leq \Psi \lambda
\end{array}
\] $$

$subhead Duality$$
The Lagrangian corresponding to the problem above
$latex L : \R_+^m \times \R_+^m \times \R_+^n \rightarrow \R$$
is defined by
$latex \[
L( w , z , \lambda ) = \left\{ \begin{array}{ll}
w^\T ( z - \Psi \lambda ) + \phi_m ( z )  + \phi_m ( z )
+ t \phi_n ( \lambda ) + m ( 1_n^T \lambda - 1 )
	& {\rm if} \; (w, z, \lambda) > 0_{m + m + n}
\\
+ \infty & {\rm if \, for \, some \,} i \;, \; z_i = 0
\\
+ \infty & {\rm if \, for \, some \,} j \;, \; \lambda_j = 0
\\
- \infty & {\rm if \; for \; some \;} i \;, w_i = 0
\end{array} \right.
\] $$
Note that the primal problem above is equivalent to minimizing
the following function of $latex (z , \lambda)$$:
$latex \[
P(z , \lambda ) = \sup \; L(w , z , \lambda )
	\; {\rm .w.r.t.} \; \; w \in \R_+^m
\] $$
The Fenchel-Rockafellar dual problem is to maximize
$latex \[
Q(w) = \inf \; L(w , z , \lambda )
	\; {\rm .w.r.t.} \; z \in \R_+^m \; , \; \lambda \in \R_+^n
\] $$
we regroup the terms in the Lagrangian as
$latex \[
L(w , z , \lambda ) = \left\{ \begin{array}{ll}
\phi_m (z) + z^\T w
+ t \phi_n ( \lambda ) + \lambda^\T ( m 1_n - \Psi^\T w ) - m
	& {\rm if} \; w > 0_m \; z > 0_m
\\
+ \infty & {\rm if \, for \, some \,} i \;, \; z_i = 0
\\
+ \infty & {\rm if \, for \, some \,} j \;, \; \lambda_j = 0
\\
- \infty & {\rm if \; for \; some \;} i \;, w_i = 0
\end{array} \right.
\] $$
For dual feasible points $latex w$$,
the minimizer of the Lagrangian with respect to $latex z_i$$ is
$latex w_i^{-1}$$ and the minimizer with respect to $latex \lambda_j$$ is
$latex t ( m - \Psi_j^T w )^{-1}$$ (where $latex \Psi_j$$ is the $latex j$$
column of $latex \Psi$$).
Using this, the dual problem is
$latex \[
\begin{array}{lll}
{\rm maximize}
 & - \phi_m ( w )
 + n \log ( t ) -  t \phi_n ( m 1_n - \Psi^T w )
 + t m
 & {\rm w.r.t.} \;  w \in \R_+^m
\\
{\rm subject \; to}
 & \Psi^\T w \leq m 1_n
\end{array}
\] $$
Using $latex s \in \R_+^n$$ as the slack variables in problem above
can be written as
$latex \[
\begin{array}{lll}
 (Q) & {\rm maximize} - \phi_m ( w )  - t \phi_n ( s ) + n \log ( t ) + t m
     & {\rm w.r.t.} \;  w \in \R_+^m \; , \; s \in \R_+^n
\\
     & {\rm subject \, to}
     & \Psi^\T w  + s = m 1_n
\end{array}
\] $$

$subhead Karush Kuhn Tucker Conditions$$
The following function is a Lagrangian for $latex (Q)$$:
$latex \[
	- \phi_m ( w )  - t \phi_n ( s ) + n \log ( t ) + t m
	- \lambda^T (  \Psi^\T w  + s -  m 1_n )	
\] $$
The corresponding KKT conditions are
$latex \[
\begin{array}{rcl}
	\Psi^\T w + s          & = & m 1_n \\
	\D (w) \Psi \lambda    & = & 1_m   \\
	\D ( \lambda ) s       & = & t 1_n
\end{array}
\] $$
where for a vector $latex v$$, $latex \D (v)$$ is the diagonal
matrix with $latex v$$ along the diagonal.
It follows from these conditions that
$latex \[
\begin{array}{rcl}
	\lambda^T \Psi^\T w + \lambda^T s & = & m \lambda^T 1_n \\
	1_m^T \D (w) \Psi \lambda         & = & 1_m^T 1_m       \\
	m                   + t n         & = & m \lambda^T 1_n \\
	\sum_{j=1}^n \lambda_j            & = & 1 + t \frac{n}{m}
\end{array}
\] $$


$subhead Relaxed Subproblem$$
During the calculations, we use the following change of variables:
$latex w = w / m$$, $latex s = s / m$$, and $latex \lambda = m \lambda$$.
Using this change of variables, and further relaxing the
complementarity condition using the parameter $latex \mu > 0$$,
the relaxed Kuhn-Tucker equations become:
$latex \[
F_\mu ( \lambda , w , s )
=
\left( \begin{array}{c}
	\Psi^\T w + s         \\
	\D (w) \Psi \lambda   \\
	\D ( \lambda ) s
\end{array} \right)
=
\left( \begin{array}{c}
	1_n   \\
	1_m   \\
	\mu 1_n
\end{array} \right)
\] $$
where the first equality is a definition of the function
$latex \[
	F_\mu : \R^n \times \R^m \times \R^n
	\rightarrow
	\R^n \times \R^m \times \R^n
\] $$
The Jacobian of $latex F_\mu$$ is
$latex \[
F_\mu^{(1)} ( \lambda , w , s )
=
\left( \begin{array}{ccc}
	0_{n,n}     & \Psi^\T            & \D( 1_n )      \\
	\D (w) \Psi & \D( \Psi \lambda ) & 0_{m,n}        \\
	\D (s)      & 0_{n,m}            & \D( \lambda )
\end{array} \right)
\] $$
where $latex 0_{n,m} \in \R^{n \times m}$$ is the matrix
with all its elements equal to zero.
Note that $latex \D( 1_n )$$ is the $latex n \times n$$
identity matrix.

$subhead Newton Step$$
At the $th k$$ iteration of the algorithm we are given
$latex \mu^k \in \R$$,
$latex \lambda^k \in \R^n$$, and
$latex w^k \in \R^m$$
where $latex ( \lambda^k , w^k , s^k )$$ is an approximate solution of the
$xref/relaxed/Theory/Relaxed Subproblem/relaxed subproblem/1/$$.
with $latex \mu = \mu^k$$.
The Newton Step at the $th k$$ iteration is the value of
$latex ( \Delta \lambda^k , \Delta w^k , \Delta s^k )$$  that solves
the linear equation
$latex \[
	F_k ( \lambda^k , w^k , s^k ) +
	F_k ^{(1)} ( \lambda^k , w^k , s^k )
	\left( \begin{array}{c}
		\Delta \lambda^k \\
		\Delta w^k       \\
		\Delta s^k
	\end{array} \right)
	=
	\left( \begin{array}{c}
		1_n   \\
		1_m   \\
		\mu^k 1_n
	\end{array} \right)
\] $$
(Note that we have used the short hand $latex F_k$$ for
$latex F$$ with the subscript $latex \mu^k$$.)
Moving $latex  F_k ( \lambda^k , w^k , s^k )$$ to the right hand side,
using the equations above for $latex F_k$$ and $latex F_k^{(1)}$$,
and restricting our algorithm to $latex s^k$$ that satisfy
$latex \[
	s^k = 1_n - \Psi^\T w^k
\] $$
we obtain the following linear equation for
$latex ( \Delta \lambda^k , \Delta w^k , \Delta s^k )$$
$latex \[
\left( \begin{array}{ccc}
      0_{n,n}     & \Psi^\T            & \D( 1_n )      \\
      \D (w) \Psi & \D( \Psi \lambda ) & 0_{m,n}        \\
      \D (s)      & 0_{n,m}            & \D( \lambda )
\end{array} \right)
\left( \begin{array}{c}
	\Delta \lambda \\
	\Delta w       \\
	\Delta s
\end{array} \right)
=
\left( \begin{array}{c}
	0                           \\
	1_m - \D (w) \Psi \lambda   \\
	\mu 1_n - \D ( \lambda ) s
\end{array} \right)
\] $$
where the superscript $latex k$$ has been dropped from the equation.
Given the value of $latex \Delta w$$,
we use the first row above to solve for $latex \Delta s$$.
Given the value of $latex \Delta w$$ and $latex \Delta s$$,
we use the third row above to solve for $latex \Delta \lambda$$.
Subtracting $latex \D( \lambda )$$ times the first row from the
third row, we obtain the following reduced set of equations:
$latex \[
\left( \begin{array}{cc}
      \D (w) \Psi & \D( \Psi \lambda )                \\
      \D (s)      & - \D( \lambda ) \Psi^\T
\end{array} \right)
\left( \begin{array}{c}
	\Delta \lambda \\
	\Delta w
\end{array} \right)
=
\left( \begin{array}{c}
	1_m - \D (w) \Psi \lambda   \\
	\mu 1_n - \D ( \lambda ) s
\end{array} \right)
\] $$
Given two vectors $latex u \in \R^n$$, $latex v \in \R^n$$,
where none of the components of $latex v$$ are zero,
we define the vector $latex u / v \in \R^n$$ by
$latex (u / v)_i = u_i / v_i$$.
We also define the vector $latex v^{-1} \in \R^n$$ by
$latex v_i^{-1} = 1 / v_i$$.
Subtracting $latex \Psi \D (s)^{-1}$$ times the second row
from $latex \D (w)^{-1}$$ times the first row,
we obtain the following reduced equation
$latex \[
\begin{array}{rcl}
[ \D (w)^{-1} \D( \Psi \lambda ) + \Psi \D( \lambda / s ) \Psi^T ]
\Delta w 
& = &
w^{-1} - \Psi \lambda
- \mu \Psi s^{-1}
+ \Psi \D ( \lambda / s ) s
\\
& = &
w^{-1}  - \mu \Psi s^{-1}
\end{array}
\] $$
$end
---------------------------------------------------------------------------

*/
# include <mat2cpp.hpp>
# include <boost/numeric/ublas/io.hpp>
# include <iostream>

namespace { // empty namespace
	void error(char *msg)
	{	std::cerr << msg << std::endl; }
}

namespace non_par { // BEGIN non_par namespace

double relaxed(
	const mat2cpp::matrix<double> &Psi     ,
	double                         t       ,
	const mat2cpp::matrix<double> &option  ,
	mat2cpp::matrix<double>       &lam     ,
	mat2cpp::matrix<double>       &w       ,
	mat2cpp::matrix<double>       &info    )
{
	/* ------- GENERAL DESCRIPTION OF SOLUTION METHOD ------------
	The path-following algorithm applies Newton's method to the
	perturbed system of nonlinear equations above; i.e.,
		Psi' * w + s      =  1_n            (1)
		w .* (Psi * lam)  =  1_m            (2)
		lam .* s          =  mu * 1_n       (3)
	where mu is decreased to the value t. The Newton step
	is damped so that the iterates remain strictly positive.
	In addition, the path-following parameter mu is reduced
	at each iteration in a manner that attempts to decrease
	its value at a rate that is approximately the same as the
	rate of decrease in the error in the nonlinear equation
		w .* (Psi * lam)  =  1_m
	Specifically, we try to reduce mu and
		mean( abs( w .* (Psi * lam) - 1_m ) )
	at approximately the same rate.
	------------------------------------------------------------- */
	// Setup and Notation
	// -----------------------------------------------------------
	using namespace mat2cpp;

	// temporary row and column index
	size_t i, j;

	// return value
	double obj;

	// Problem dimensions are determined by the argument Psi
	size_t m = Psi.size1();
	size_t n = Psi.size2();
	
	// column vectors of ones with length n and m
	scalar_matrix<double> one_n(n, 1, 1.);
	scalar_matrix<double> one_m(m, 1, 1.);

	// convergence criteria
	double eps     = option(0, 0);
	size_t max_itr = size_t( option(1, 0) );

	// scaled convergence criteria
	double Psi_max = max(Psi);

	// minimum allowable value for mu
	double mu_min = std::max(t, eps / 10.);

	// column vector with all its elements equal to t
	scalar_matrix<double> t_n(n, 1, t);
	
	// --------------------------------------------------
	// Check input
	// --------------------------------------------------
	info.resize(0, 3, false);
	if( min(Psi) < 0. )
	{	error("The matrix Psi has a negative element.");
		return 0.;
	}
	if( Psi_max == 0. )
	{	error("The matrix Psi is identically zero.");
		return 0.;
	}
	if( eps <= 0. )
	{	error("The value of eps = option(1, 0) <= 0.");
		return 0.;
	}
	if( max_itr >= 500 )
	{	error("max_itr >= 500");
		return 0.;
	}
	// --------------------------------------------------
	// Initialization
	// --------------------------------------------------
	// first approximate solution for the primal problem
	lam                     = one_n;
	
	// keep this matrix product
	// Psi_lam = Psi * lam
	matrix<double> Psi_lam  = prod(Psi, lam);
	
	// initialize w so that equation (2) is satisfied; i.e.,
	//            w .* (Psi * lam)  =  one_m            (2)
	// w = one_m ./ Psi_lam
	w                       = element_div(one_m, Psi_lam);
	
	// keep this matrix product
	// PsiT_w   = Psi' * w;
	matrix<double> PsiT_w   = prod( trans(Psi), w);

	// Multiply lam and divide w by factor.
	// Note that after this operation, equation (2) is still satisfied.
	double factor          = 2. * max( PsiT_w );
	lam                   *= factor;
	w                     /= factor;
	
	// update matrix products
	Psi_lam  *= factor;
	PsiT_w   /= factor;
	
	// Note that with the current value of w, max( PsiT_w ) = 1 / 2.
	// We initialze s so that equation (1) is satisfied; i.e.,
	//             Psi' * w + s  =  one_n               (1)
	// In fact, equation (1) will be satisfied through out the computation.
	matrix<double> s   = one_n - PsiT_w;

	// residual corresponding to equation (3) (with mu = t)
	//            lam .* s          =  mu * one_n       (3)
	matrix<double> q   =  abs( element_prod(lam, s) - t_n );

	// The initial value for residual in equation (2)
	//            w .* (Psi * lam)  =  one_m            (2)
	matrix<double> r   = abs(one_m - element_prod(w, Psi_lam) );
	
	// initial mu
	double res_old = ( sum(q) / n + sum(r) / m ) / 2.;
	double mu      = std::max(res_old, mu_min);

	// iteration counter and info for each iteration
	bool preserve = false;
	size_t n_itr   = 0;
	info.resize(n_itr + 1, 3, preserve);
	info(n_itr, 0) = mu;
	info(n_itr, 1) = max(q);
	info(n_itr, 2) = max(r);
	
	// -------------------------------------------------------------
	// Iterate until convergence
	// -------------------------------------------------------------
	while ( max(q) > eps || max(r) > eps )
	{	if ( n_itr >= max_itr )
		{	std::cout << "info = "    << info << std::endl;
			std::cout << "Psi_max = " << Psi_max << std::endl;
			std::cout << "eps = "     << eps << std::endl;
			error("relaxed: maximum number of interations");
			return 0.;
		}
		// mu * 1_n
		scalar_matrix<double> mu_n (n, 1, mu);	

		// mu_s = mu ./ s
		matrix<double> mu_s  = element_div(mu_n, s);

		// lam_s = lam ./ s
		matrix<double> lam_s  = element_div(lam , s);

		// Psi_lam_w = (Psi * lam) / w
		matrix<double> Psi_lam_w = element_div(Psi_lam, w);
	
		// use coef_dw to accumulate coef_dw * dw = rhs_dw
		// coef_dw = D(w)^{-1} * D(Psi*lam) + Psi * D(lam/s) * Psi^T
		matrix<double> coef_dw =
			prod( Psi, diag_prod( lam_s, trans(Psi) ) );
		for(i = 0; i < m; i++)
			coef_dw(i, i) += Psi_lam_w(i, 0);
	
		// use rhs_dw for the right hand side of coef_dw * dw = rhs_dw
		// rhs_dw = one_m ./ w - Psi * mu_s;
		matrix<double> rhs_dw =
			element_div(one_m,  w) - prod(Psi, mu_s);
		
		// compute the Newton step value for dw
		size_t rank;
		matrix<double> dw = matrix_div(coef_dw, rhs_dw, rank);
		
		// and check that this computation was successful.
		if( rank < m )
		{	error("Equation for dw is singular.");
			return 0.;
		}
	
		// check the residual in evaluation of dw
		// double dw_err = max(abs( prod(coef_dw, dw) - rhs_dw ) );
		// if( dw_err > eps )
		// {	error("error in linear inversion > eps");
		//	return 0.;
		// }
	
		// compute Newton step value for ds using
		// Psi' * dw + D(1_n) * ds = 0
		matrix<double> ds  = - prod(trans(Psi), dw);
	
		// compute the Newton step value for dlam using
		// D(s) * dlam + D(lam) * ds = mu * 1_n - D(lam) * s
		matrix<double> dlam  =  mu_s - lam - element_prod(lam_s, ds);
	
		// compute Newton step damping factor alp that keeps all of the
		// iterates strictly positive; i.e. w > 0, lam > 0, s > 0.
		double near_one = .99995;
		double alp_pri  = 1.;
		double term = max( - element_div(dlam, lam) );
		if( term > near_one )
			alp_pri = std::min(alp_pri , near_one / term );
		//
		double alp_dual = 1.;
		term = max( - element_div(ds,  s) );
		if( term > near_one )
			alp_dual = std::min(alp_dual , near_one / term );
		//
		term = max( - element_div(dw, w) );
		if( term > near_one )
			alp_dual = std::min(alp_dual , near_one / term );
		//
		double alp = std::min( alp_pri, alp_dual );
		//
		// compute the primal and dual iterates
		lam = lam + dlam * alp;
		w   = w   + dw   * alp;

		// compute the corresponding value of s. Up to roundoff,
		// s = s + ds gives same answer and is less work.
		PsiT_w  = prod(trans(Psi), w );
		s       = one_n - PsiT_w;

		// compute convergence criteria at new iterates
		q       =  abs( element_prod(lam, s) - t_n );
		Psi_lam =  prod(Psi,  lam);
		r       = abs(one_m - element_prod(w, Psi_lam) );
	
		// determine the new value for mu
		double res_avg = (sum(q) / n + sum(r) / m) / 2.;
		if( max(q) > eps )
		{	factor = (res_avg / res_old);
			factor = factor * factor * factor;
			factor = std::min(factor, .3);
			mu     = factor * mu;
			mu     = std::max(mu, mu_min);
		}
		res_old = res_avg;

		// add convergence criteria for this iteration to the list
		++n_itr;
		preserve = true;
		info.resize(n_itr + 1, 3, preserve);
		info(n_itr, 0) = mu;
		info(n_itr, 1) = max(q);
		info(n_itr, 2) = max(r);
	}

	// convert lam and w back to original scaling
	lam /= double(m);
	w   *= double(m);

	// compute objective value
	Psi_lam =  prod(Psi,  lam);  // could use old Psi_lam / m
	obj = -double(m);
	for(i = 0; i < m; i++)
		obj -= log( Psi_lam(i, 0) );
	for(j = 0; j < n; j++)
		obj -= t * log( lam(j, 0 ) );
	for(j = 0; j < n; j++)
		obj += m * lam(j, 0);

	return obj;
}

} // END non_par namespace
