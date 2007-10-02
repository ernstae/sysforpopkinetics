# ifndef NON_PAR_BENDER_INCLUDED
# define NON_PAR_BENDER_INCLUDED

/*
$begin Bender$$ 
$spell
	gx
	gxx
	const
	ublas
	non_par
	Cpp
	mu
	dy
	rm
	Karush
	Kuhn
	Tucker
$$

$comment Latex macros local to this file$$
$latex \newcommand{\R}{{\bf R}}$$
$latex \newcommand{\T}{{\rm T}}$$
$latex \newcommand{\D}{{\rm D}}$$

$section Non-Parametric Population Bender's Reduced Objective$$

$index Bender, objective$$
$index objective, Bender$$

$head Syntax$$
$syntax/non_par::Bender(/like/, /mu/, /x/, /y/, /g/, /gx/, /gxx/)/$$

$head Purpose$$
We are given a matrix valued function 
$latex \Psi : \R^{n \times p} \rightarrow \R_+^{m \times n}$$
(where $latex m \leq n$$) and a relaxation parameter $latex \mu$$
The function $latex \Psi$$ has special structure in that
the $th j$$ column of $latex \Psi (x)$$ only depends on the
$th j$$ row of $italic x$$.
We define our $latex \mu $$-relaxed objective function
$latex \[
F(x , y) = \phi_m [ \Psi (x) \lambda ] 
         + \mu \phi_n ( \lambda )  
         +  m ( 1_n^\T \lambda - 1 )
\] $$
where $latex y^\T = ( \lambda^\T , w^\T , s^\T  ) $$ with 
$latex \lambda \in \R_+^n$$, $latex w \in \R_+^m$$ and $latex s \in \R_+^n$$.
The function $latex \phi_m : \R_+^m \rightarrow \R$$ is defined by
$latex \[
\phi_m ( z ) = \left\{ \begin{array}{ll}
	- \log( z_1 ) - \log( z_2 ) -  \cdots - \log ( z_m )
	& {\rm if} \; z > 0_m
	\\
	+ \infty & {\rm otherwise}
\end{array} \right.
\] $$
The symbol $latex 0_m \in \R^m$$ ($latex 1_n \in \R^n$$)
denotes the vector with all its elements equal to zero (one).
Our goal is to solve the following problem
$latex \[
(P) \;\; {\rm minimize} \;\; F(x, y) \;\; 
	{\rm w.r.t.} \;\; y \in \R_+^{n + m + n} \;\;
	{\rm and} \;\; x \in \R^{n \times p}
\] $$
The Bender's reduced objective function $latex G(x)$$ is defined by
$latex \[
\begin{array}{rcl}
Y(x) 
& = & 
{\rm argmin} \; F(x, y) \;\; {\rm w.r.t.} \;\; y \in \R_+^{n + m + n}
\\
G(x) & = & F [x, Y(x) ]
\end{array}
\] $$
This routine computes
$latex G (x)$$, $latex G^{(1)} (x)$$ and $latex G^{(2)} (x)$$.


$head like$$
The $code Bender$$ argument $italic like$$ 
is a C++ function object that supports the syntax
$syntax%
	%psi% = (*%like%)(%beta%)
%$$

$subhead beta$$
The $italic like$$ argument $italic beta$$ has prototype
$syntax%
	const ublas::matrix< CppAD::AD<double> > &%beta%
%$$
and size $latex 1 \times p$$.

$subhead psi$$
The $italic like$$ return value $italic psi$$ has prototype
$syntax%
	ublas::matrix< CppAD::AD<double> > %psi%
%$$
and has size $latex m \times 1$$ and is the 
$th j$$ column of the matrix $latex \Psi (x)$$ when
the $th j$$ row of $italic x$$ is equal to $italic beta$$.

$head mu$$
The $code Bender$$ argument $italic mu$$ has prototype
$syntax%
	double mu
%$$
It specifies the relaxation parameter value.

$head x$$
The $code Bender$$ argument $italic x$$ has prototype
$syntax%
	const ublas::matrix<double> &%x%
%$$
and size $latex n \times p$$.
For $latex j = 1 , \ldots , n$$, the $th j$$ row of 
$italic x$$ specifies a value for the parameters.


$head y$$
The $code Bender$$ argument $italic y$$ has prototype
$syntax%
	const ublas::matrix<double> &%y%
%$$
its size $latex (n + m + n) \times 1$$ and it is equal to $latex Y(x)$$.


$head g$$
The $code Bender$$ argument $italic g$$ has prototype
$syntax%
	ublas::matrix<double> &%g%
%$$
and size $latex 1 \times 1$$.
The input values of the elements of
$italic g$$ do not matter.
Upon return from $code Bender$$,
$latex \[
	g[0] = G(x)
\]$$.

$head gx$$
The $code Bender$$ argument $italic gx$$ has prototype
$syntax%
	ublas::matrix<double> &%gx%
%$$
and size $latex (n * p) \times 1$$ or $latex 0 \times 0$$.
(If the size of $italic gx$$ is $latex 0 \times 0$$,
the size of $italic gxx$$ must also be $latex 0 \times 0$$.)
The input values of the elements of $italic gx$$ do not matter.
If its size is $latex 0 \times 0$$, the gradient of $latex G(x)$$
is not calculated.
Otherwise, upon return from $code Bender$$,
$latex \[
	gx(i * p + j, 0) = \partial_{x(i, j)} G (x)
\] $$
for $latex i = 0 , \ldots , n-1$$ and $latex j = 0 , \ldots , p-1$$.

$head gxx$$
The $code Bender$$ argument $italic gxx$$ has prototype
$syntax%
	ublas::matrix<double> &%gxx%
%$$
and size $latex (n * p) \times (n * p)$$  or $latex 0 \times 0$$.
The input values of the elements of $italic gxx$$ do not matter. 
If its size is $latex 0 \times 0$$, the Hessian of $latex G(x)$$
is not calculated.
Otherwise, upon return from $code Bender$$,
$latex \[
	gxx(i * p + j, k * p + \ell ) = 
		\partial_{x(i, j)} \partial_{x(k, \ell)} G (x)
\] $$.
for 
$latex i = 0 , \ldots , n-1$$,
$latex j = 0 , \ldots , p-1$$,
$latex k = 0 , \ldots , n-1$$, and
$latex \ell = 0 , \ldots , p-1$$.


$head Method$$
$subhead Karush Kuhn Tucker Conditions$$
The Karush Kuhn Tucker (KKT) conditions for a minimum with
respect to $latex y$$ are
$latex \[
H(x, y)
=
\left( \begin{array}{c}
	\Psi (x)^\T w + s       - m 1_n \\
	\D (w) \Psi (x) \lambda - 1_m   \\
	\D ( \lambda ) s        - \mu 1_n 
\end{array} \right)
=
0
\] $$
where the first equation is a definition for
$latex H : \R^{n \times p} \times \R^{n + m + n} \rightarrow  \R^{n + m + n}$$.
The partial derivatives $latex F_w$$ and $latex F_s$$ are both zero.
The partial derivative with respect to $latex \lambda$$ is given by
$latex \[
\begin{array}{rcl}
F_\lambda (x , y) 
& = & 
1_m^\T \D [ \Psi (x) \lambda ]^{-1} \Psi (x)
+ \mu 1_n^\T \D ( \lambda )^{-1}
- m 1_n
\end{array}
\] $$
Suppose $latex ( \lambda , w , s ) $$ satisfies
the KKT conditions corresponding to $latex x$$.
It follows that
$latex \[
\begin{array}{rcl}
F_\lambda (x , y) 
& = & 
1_m^\T \D [ \Psi (x) \lambda ]^{-1} \Psi (x)
+ s^\T
- m 1_n^\T
\\
& = & 
1_m^\T \D (w)  \Psi (x)
+ s^\T
- m 1_n^\T
\\
& = & 
w^\T  \Psi (x) + s^\T - m 1_n^\T 
\\
& = &
0
\end{array}
\] $$
In addition, function $latex F(x, y)$$ is convex with respect to $latex y$$.
Thus, 
if we implicitly define $latex Y(x)$$ by $latex H[ x , Y(x) ] = 0$$,
it follows that
$latex \[
G(x) = F[ x , Y(x) ]
\] $$ 

$subhead Sum Lambda$$
Suppose $latex ( \lambda , w , s ) $$ satisfies
the KKT conditions corresponding to $latex x$$.
It follows that
$latex \[
\begin{array}{rcl}
	m 1_n^\T         & = & w^\T  \Psi (x) + s^\T                 \\
	m 1_n^\T \lambda & = & w^\T  \Psi (x) \lambda + s^\T \lambda \\
	                 & = & w^\T  \D (w)^{-1} 1_m  + \mu n        \\
	                 & = & m  + \mu n                            \\
	\sum_{j=1}^n \lambda_j & = & 1 + \mu \frac{n}{m} 
\end{array}
\] $$
Thus this equation can be added as a constraint to the original problem
with out changing the solution.

$head Determining Delta y$$
The equation for $latex \Delta y$$ is
$latex \[
\begin{array}{rcl}
H_y (x, y) \Delta y & = & - h
\\
\left( \begin{array}{ccc}
	0_{n,n}          & \Psi (x)^\T              & \D ( 1_n )      \\
	\D (w) \Psi (x)  & \D [ \Psi (x) \lambda ]  & 0_{m,n}         \\
	\D (s)           & 0_{n,m}                  & \D ( \lambda)
\end{array} \right)
\left( \begin{array}{c} 
	\Delta \lambda \\ \Delta w \\ \Delta s 
\end{array} \right)
& = &
-
\left( \begin{array}{c} 
	a \\ b \\ c 
\end{array} \right)
\end{array}
\] $$
where $latex a$$, $latex b$$, and $latex c$$ are 
the corresponding components of $latex h = H(x, y)$$.
Given the value of $latex \Delta w$$,
we use the first row above to solve for $latex \Delta s$$.
Given the value of $latex \Delta w$$ and $latex \Delta s$$,
we use the third row above to solve for $latex \Delta \lambda$$.
Subtracting $latex \D( \lambda )$$ times the first row from the
third row, we obtain the following reduced set of equations:
$latex \[
\left( \begin{array}{cc}
      \D (w) \Psi (x) & \D [ \Psi (x) \lambda ) ]                \\
      \D (s)      & - \D( \lambda ) \Psi^\T (x)
\end{array} \right)
\left( \begin{array}{c}
	\Delta \lambda \\
	\Delta w
\end{array} \right)
=
\left( \begin{array}{c}
	- b  \\
	\D ( \lambda ) a - c
\end{array} \right)
\] $$
Given two vectors $latex u \in \R^n$$, $latex v \in \R^n$$,
where none of the components of $latex v$$ are zero,
we define the vector $latex u / v \in \R^n$$ by
$latex (u / v)_i = u_i / v_i$$.
Subtracting $latex \Psi (x) \D (s)^{-1}$$ times the second row
from $latex \D (w)^{-1}$$ times the first row,
we obtain the following reduced equation
$latex \[
[ \D (w)^{-1} \D [ \Psi (x) \lambda ] + \Psi (x) \D( \lambda / s ) \Psi^T (x) ]
\Delta w
=
\Psi (x) [ c / s  - \D( \lambda / s ) a ] - b / w
\] $$

$children%
	cpp/bender_ok.cpp
%$$
$head Example$$
The section $xref/bender_ok/$$ contains an example and test
of $code Bender$$.

$end
----------------------------------------------------------------------------
*/

# include <iostream>
# include <mat2cpp.hpp>
# include <CppAD/CppAD.h>

namespace non_par{ // BEGIN non_par namespace

template <class Like>
class Bender_fun {
	typedef mat2cpp::matrix< CppAD::AD<double> > ADmatrix;
	typedef CppAD::vector  < CppAD::AD<double> > ADvector;

	typedef mat2cpp::matrix<double>              Dblmatrix;
	typedef CppAD::vector  <double>              Dblvector;
private:
	const double     mu;
	const size_t      m;
	const size_t      n;
	const size_t      p;
	Like          *like;

	// Dbl state matrices
	Dblmatrix         Dbl_x;
	Dblmatrix         Dbl_y;
	Dblmatrix       Dbl_Psi;
	Dblmatrix        Dbl_lam;
	Dblmatrix   Dbl_Psi_lam;
	Dblmatrix         Dbl_w;
	Dblmatrix    Dbl_PsiT_w;
	Dblmatrix         Dbl_s;

	// AD state matrices
	ADmatrix         AD_Psi;
	ADmatrix         AD_lam;
	ADmatrix     AD_Psi_lam;
	ADmatrix           AD_w;
	ADmatrix      AD_PsiT_w;
	ADmatrix           AD_s;
public:
	// BenderQuad function object
	Bender_fun(Like *like_, double mu_, size_t m_, size_t n_, size_t p_)
	: mu(mu_), m(m_), n(n_), p(p_), like(like_)
	, Dbl_Psi(m, n), Dbl_lam(n, 1), Dbl_Psi_lam(m, 1)
        , Dbl_w(m, 1), Dbl_PsiT_w(n, 1), Dbl_s(n, 1)
	, AD_Psi(m, n), AD_lam(n, 1), AD_Psi_lam(m, 1)
        , AD_w(m, 1), AD_PsiT_w(n, 1), AD_s(n, 1)
	{ }
	// set AD state matrices
	void set_AD(const ADvector &x_, const ADvector &y)
	{	// std::cout << "bender.hpp: start set_AD" << std::endl;
		using namespace mat2cpp;
		size_t i, j, k;

		assert( x_.size() == n * p );
		assert( y.size() == n + m + n );

		// Psi
		ADmatrix beta(1, p);
		ADmatrix psi(m, 1);
		for(j = 0; j < n; j++)
		{	for(k = 0; k < p; k++)
				beta(0, k) = x_[ j * p + k ];
			psi = (*like) (beta);
			for(i = 0; i < m; i++)
				AD_Psi(i, j) = psi(i, 0);
		}

		// lam
		for(i = 0; i < n; i++)
			AD_lam(i, 0) = y[i];
		// Psi_lam
		AD_Psi_lam = prod(AD_Psi, AD_lam);
		// w
		for(i = 0; i < m; i++)
			AD_w(i, 0) = y[n+i];
		// PsiT_w
		AD_PsiT_w = prod( trans(AD_Psi), AD_w);
		// s
		for(i = 0; i < n; i++)
			AD_s(i, 0) = double(m) - AD_PsiT_w(i, 0);

		// std::cout << "bender.hpp: end set_AD" << std::endl;
	}
	// set Dbl state matrices
	void set_Dbl(const Dblvector &x_, const Dblvector &y)
	{	// std::cout << "bender.hpp: start set_Dbl" << std::endl;
		using namespace mat2cpp;
		size_t i, j, k;

		assert( x_.size() == n * p );
		assert( y.size() == n + m + n );

		// x
		Dbl_x.resize(n * p, 1);
		for(j = 0; j < n * p; j++)
			Dbl_x(j, 0) = x_[j];

		// y
		Dbl_y.resize(n + m + n, 1);
		for(j = 0; j < n + m + n; j++)
			Dbl_y(j, 0) = y[j];

		// Psi
		Dblmatrix beta(1, p);
		Dblmatrix psi(m, 1);
		for(j = 0; j < n; j++)
		{	for(k = 0; k < p; k++)
				beta(0, k) = x_[ j * p + k ];
			psi = (*like) (beta);
			for(i = 0; i < m; i++)
				Dbl_Psi(i, j) = psi(i, 0);
		}

		// lam
		for(i = 0; i < n; i++)
			Dbl_lam(i, 0) = y[i];
		// Psi_lam
		Dbl_Psi_lam = prod(Dbl_Psi, Dbl_lam);
		// w
		for(i = 0; i < m; i++)
			Dbl_w(i, 0) = y[n+i];
		// PsiT_w
		Dbl_PsiT_w = prod( trans(Dbl_Psi), Dbl_w);
		// s
		for(i = 0; i < n; i++)
			Dbl_s(i, 0) = double(m) - Dbl_PsiT_w(i, 0);

		// std::cout << "bender.hpp: end set_Dbl" << std::endl;
	}
	// Evaluate f = F(x, y)
	ADvector f(const ADvector &x, const ADvector &y)
	{	// std::cout << "bender.hpp: start f" << std::endl;
		ADvector f(1);
		size_t i;

		// set AD_Psi, AD_lam, AD_Psi_lam, AD_w, AD_PsiT_w, s
		set_AD(x, y);

		CppAD::AD<double> sum = - double(m);
		for(i = 0; i < n; i++)
		{	sum       -= mu * log( AD_lam(i, 0) );
			sum       += double(m) * AD_lam(i, 0);
		}
		for(i = 0; i < m; i++)
			sum -= log( AD_Psi_lam(i, 0) );

		f[0] = sum;

		// std::cout << "bender.hpp: end f" << std::endl;
		return f;
	} 
	// Evaluate h = H(x, y)
	ADvector h(const ADvector &x, const Dblvector &y)
	{	// std::cout << "bender.hpp: start h" << std::endl;
		ADvector h(n + m + n), Y(y.size());
		size_t i;

		// set AD_Psi, AD_lam, AD_Psi_lam, AD_w, AD_PsiT_w, AD_s
		i = y.size();
		while(i--)
			Y[i] = y[i];
		set_AD(x, Y);

		for(i = 0; i < n; i++)
			h[i] = AD_PsiT_w(i, 0) + AD_s(i, 0) - double(m);
		for(i = 0; i < m; i++)
			h[n+i] = AD_w(i, 0) * AD_Psi_lam(i, 0) - 1.;
		for(i = 0; i < n; i++)
			h[n+m+i] = AD_lam(i, 0) * AD_s(i, 0) - mu;

		// std::cout << "bender.hpp: end h" << std::endl;
		return h;
	} 
	ADvector dy(const Dblvector &x, const Dblvector &y, const ADvector &h)
	{	// std::cout << "bender.hpp: start dy" << std::endl;
		using namespace mat2cpp;
		size_t i, j;

		// check arguments are same as in call to fun.set_Dbl below
		for(i = 0; i < x.size(); i++)
			assert( Dbl_x(i, 0) == x[i] );
		for(i = 0; i < y.size(); i++)
			assert( Dbl_y(i, 0) == y[i] );

		// -----------------------------------------------------------
		// Calculations in double

		// Psi_lam_s = Psi * D( lam / s )
		Dblmatrix Dbl_Psi_lam_s(m, n);
		for(i = 0; i < m; i++)
		{	for(j = 0; j < n; j++)
			{	Dbl_Psi_lam_s(i, j) = 
				Dbl_Psi(i, j) * Dbl_lam(j, 0) / Dbl_s(j, 0);
			}
		}

		// coef_dw = Psi * D(lam/s) * Psi^T
		Dblmatrix Dbl_coef_dw = prod( Dbl_Psi_lam_s, trans(Dbl_Psi) );

		// coef_dw = D(w)^{-1} * D(Psi*lam) + Psi * D(lam/s) * Psi^T
		for(i = 0; i < m; i++)
			Dbl_coef_dw(i, i) += Dbl_Psi_lam(i, 0) / Dbl_w(i, 0);

		// LU factor linear equations 
		Dblvector Dbl_Lu(m * m);
		for(i = 0; i < m; i++)
		{	for(j = 0; j < m; j++)
				Dbl_Lu[ i * m + j ] = Dbl_coef_dw(i, j);
		} 
		CppAD::vector<size_t> ip(m);
		CppAD::vector<size_t> jp(m);
# ifndef NDEBUG
		int signdet = CppAD::LuFactor(ip, jp, Dbl_Lu);
		assert( signdet == 1 );
# else
		CppAD::LuFactor(ip, jp, Dbl_Lu);
# endif

		// -----------------------------------------------------------
		// Calculations in AD<double>

		// AD version of LU factor
		ADvector AD_Lu(m * m);
		i = m * m;
		while(i--)
			AD_Lu[i] = Dbl_Lu[i];

		// split h into (a, b, c)
		ADvector AD_a(n), AD_b(m), AD_c(n);
		for(i = 0; i < n; i++)
			AD_a[i] = h[i];
		for(i = 0; i < m; i++)
			AD_b[i] = h[n+i];
		for(i = 0; i < n; i++)
			AD_c[i] = h[n+m+i];

		// rhs_dw = Psi * [c / s - D(lam / s) a] - b / w
		ADmatrix AD_tmp(n, 1);
		for(i = 0; i < n; i++)
			AD_tmp(i, 0) = (AD_c[i] - Dbl_lam(i, 0) * AD_a[i]) 
			             / Dbl_s(i, 0);
		ADmatrix AD_rhs_dw(m, 1);
		for(i = 0; i < m; i++)
		{	AD_rhs_dw(i, 0) = 0.;
			for(j = 0; j < n; j++)
				AD_rhs_dw(i, 0) += Dbl_Psi(i, j) * AD_tmp(j, 0);
		}
		for(i = 0; i < m; i++)
			AD_rhs_dw(i, 0) -= AD_b[i] / Dbl_w(i, 0);

		// backsolve linear equations 
		ADvector AD_dw(m);
		for(i = 0; i < m; i++)
			AD_dw[i] = AD_rhs_dw(i, 0);
		CppAD::LuInvert(ip, jp, AD_Lu, AD_dw);

		// coefficient matrix is positive definate.
		assert( signdet == 1 );

		// compute ds = - a - Psi^T * dw
		ADvector AD_ds(n);
		for(i = 0; i < n; i++)
		{	AD_ds[i] = - AD_a[i];
			for(j = 0; j < m; j++)
				AD_ds[i] -= AD_Psi(j, i) * AD_dw[j];		
		}

		// compute dlam = - c / s - D(lam / s) * ds
		ADvector AD_dlam(n);
		for(i = 0; i < n; i++)
			AD_dlam[i] = (-  AD_c[i] - Dbl_lam(i, 0) * AD_ds[i] ) 
                                / Dbl_s(i, 0); 

		// combine (dlam, dw, ds) into dy
		ADvector AD_dy(n + m + n);
		for(i = 0; i < n; i++)
			AD_dy[i] = AD_dlam[i];
		for(i = 0; i < m; i++)
			AD_dy[n+i] = AD_dw[i];
		for(i = 0; i < n; i++)
			AD_dy[n+m+i] = AD_ds[i];

		// std::cout << "bender.hpp: end dy" << std::endl;
		return AD_dy;
	} 
};

template <class Like>
void Bender(
	Like                          *like, 
	double                         mu  ,
	const mat2cpp::matrix<double> &x   , 
	const mat2cpp::matrix<double> &y   , 
	mat2cpp::matrix<double>       &g   , 
	mat2cpp::matrix<double>       &gx  , 
	mat2cpp::matrix<double>       &gxx )
{	// std::cout << "bender.hpp: start Bender" << std::endl;
	using CppAD::AD;
	using mat2cpp::matrix;
	typedef CppAD::vector<double>       Vector;
	typedef CppAD::vector< AD<double> > ADvector;


	// check size of y
	assert( y.size1() > 2 * x.size1() && y.size2() == 1 );

	// problem size
	size_t n   = x.size1();
	size_t p   = x.size2();
	size_t m2n = y.size1();
	size_t m   = m2n - 2 * n;
	size_t np  = n * p;

	// construct link to CppAD::BenderQuad
	Bender_fun<Like> fun(like, mu, m, n, p);

	// copy input from matrices to vectors
	Vector x_(np);
	size_t i, j;
	for(i = 0; i < n; i++)
		for(j = 0; j < p; j++)
			x_[ i * p + j] = x(i, j);
	Vector y_(m2n);
	for(i = 0; i < m2n; i++)
			y_[i]  = y(i, 0);

	// set double values before calling bender_quad because
	// it calls Independent for AD<double>
	fun.set_Dbl(x_, y_);

	// check return function value size
	assert( g.size1()   == 1   && g.size2()  == 1 );

	// special case where not computing Hessian
	if( gxx.size1() == 0 && gxx.size2() == 0 )
	{	ADvector x_(np);
		size_t i, j;
		for(i = 0; i < n; i++)
			for(j = 0; j < p; j++)
				x_[ i * p + j] = x(i, j);

		ADvector y_(m2n);
		for(i = 0; i < m2n; i++)
				y_[i]  = y(i, 0);

		bool gradient = gx.size1() == np && gx.size2() == 1;
		if ( gradient)
			CppAD::Independent(x_);
		else	assert( gx.size1() == 0 && gx.size2() == 0 );

		// evaluate F(x, y)
		ADvector f_(1);
		f_ = fun.f(x_, y_);

		if( gradient )
		{	// construct AD fucntion object x -> F(x, y)
			CppAD::ADFun<double> F(x_, f_);

			// evaluate gradient of mapping above
			Vector w(1), dw(np);
			w[0] = 1.;
			dw   = F.Reverse(1, w);

			// copy gradient into return matrix
			size_t i;
			for(i = 0; i < np; i++)
				gx(i, 0) = dw[i];
		}

		// copy fucntion value into return matrix
		g(0, 0) = CppAD::Value(f_[0]);

		return;
	}

	// check gradient and Hessian return size
	assert( gx.size1() == np  && gx.size2() == 1 );
	assert( gxx.size1() == np && gxx.size2() == np );

	// dimension return vectors
	Vector g_(1), gx_(np), gxx_(np * np);
	
	CppAD::BenderQuad(x_, y_, fun, g_, gx_, gxx_);

	// copy output from vectors to matrices
	g(0, 0) = g_[0];
	for(i = 0; i < np; i++)
	{	gx(i, 0) = gx_[i];
		for(j = 0; j < np; j++)
			gxx(i, j) = gxx_[ i * np + j ];
	}
	// std::cout << "bender.hpp: end Bender" << std::endl;
	return;
}
} // END non_par namespace

# endif
