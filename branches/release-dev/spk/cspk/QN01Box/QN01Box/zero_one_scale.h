# ifndef ZERO_ONE_SCALE_INCLUDED
# define ZERO_ONE_SCALE_INCLUDED

/*
$begin zero_one_scale$$
$spell%
	const
	obj
	yy
%$$

$section Zero One Scale a Function's Argument Vector$$


$head Constructor$$
$syntax%zero_one_scale %obj%(%fun%, %m%, %n%, %a%, %b%)%$$

$head Purpose$$
Given the object $italic fun$$ that evaluations a function
defined on $latex [a, b]$$,
$code zero_one_scale$$ creates the object $italic obj$$
that evaluates a scaled version of the function
defined on $latex [0, 1]$$
(so that it can be used with $cref/QuasiNewton01Box/$$).

$head Notation$$
We are given $latex a \in \R^m$$,
$latex b \in \R^m$$ where $latex a \leq b$$, 
and $latex h : [a , b ] \rightarrow \R$$.
We define $latex n$$ as the number of indices such that
$latex a_i < b_i$$ and
$latex \[
\begin{array}{l}
I : \{ 0 , \ldots , n-1 \}  \rightarrow \{ 0 , \ldots , m - 1 \} 
\\
I(j) = \min \left\{ i : i > I(j-1) \; {\rm and} \; a_i < b_i  \right\}
\end{array}
\] $$
where the expression $latex I(j-1) $$ is interpreted as minus one 
when $latex j = 0$$.
We define the function
$latex \beta : [0, 1]^n \rightarrow \R^m$$ by
$latex \[
\beta_i (x) = \left\{ \begin{array}{cl}
a_i + x_j * ( b_i - a_i ) 
      & {\rm if} \; i = I(j) \; {\rm for \; some} \;  j 
\\
a_i   & {\rm otherwise; \; i.e.,} \; a_i = b_i
\end{array} \right.
\] $$
for $latex i = 0 , \ldots , m-1$$.
The function
$latex f : [0, 1] \rightarrow \R$$  is defined by
$latex f(x) = h ( \beta (x) )$$.

$head fun$$
The argument $italic fun$$ has prototype
$syntax%
	%Fun% *%fun%
%$$
It is input to the $code zero_one_scale$$ constructor
and must support the 
$cref/function/zero_one_scale/function/$$,
$cref/gradient/zero_one_scale/gradient/$$, and
$cref/Hessian/zero_one_scale/Hessian/$$
operations listed below
(for as long as the object $italic obj$$ exists).

$head m$$
The argument $italic m$$ has prototype
$syntax%
	size_t %m%
%$$
It specifies the dimension of the argument space for $latex h$$.

$head n$$
The argument $italic n$$ has prototype
$syntax%
	size_t %n%
%$$
It must be equal to the number of indices $latex i$$ such that
$latex a_i < b_i$$.

$head a$$
The argument $italic a$$ has prototype
$syntax%
	const double *%a%
%$$
It is a vector of length $italic m$$ that
specifies the lower limit for the domain of $latex h$$.

$head b$$
The argument $italic b$$ has prototype
$syntax%
	const double *%b%
%$$
It is a vector of length $italic m$$ that
specifies the upper limit for the domain of $latex h$$.

$head to_zero_one$$
The syntax
$syntax%
	%obj%.to_zero_one(%x%, %y_%)
%$$
maps a point $italic y_$$, in the box $latex [a, b]$$,
to the corresponding point $italic x$$,
in the box $latex [0, 1]^n$$; i.e., $latex \beta (x) = y\_$$.
The argument $italic x$$ has prototype
$syntax%
	double *%x%
%$$
It is a vector of length $italic n$$.
The argument $italic y_$$ has prototype
$syntax%
	const double *%y_%
%$$
It is a vector of length $italic m$$.

$head from_zero_one$$
The syntax
$syntax%
	%obj%.from_zero_one(%x_%, %y%)
%$$
maps a point $italic x_$$, in the box $latex [0, 1]^n$$,
to the corresponding point $italic y$$, in the box $latex [a, b]$$; i.e.,
$latex \beta (x\_) = y$$.
The argument $italic x_$$ has prototype
$syntax%
	const double *%x_%
%$$
It is a vector of length $italic n$$.
The argument $italic y$$ has prototype
$syntax%
	double *%y%
%$$
It is a vector of length $italic m$$.

$head function$$
The syntax
$syntax%
	%msg% = %fun%.function(%y_%, %h%)
%$$
set $italic h$$ to the corresponding value of the function $latex h$$.
The syntax
$syntax%
	%msg% = %obj%.function(%x_%, %f%)
%$$
set $italic f$$ to the corresponding value of the function $latex f$$.
The arguments $italic h$$ and $italic f$$ have prototypes
$syntax%
	double &%h%
	double &%f%
%$$
The arguments $italic y_$$ and $italic x_$$ have prototype
$syntax%
	const double *%y_%
	const double *%x_%
%$$
Their lengths are $italic m$$ and $italic n$$ respectively.

$head gradient$$
The syntax
$syntax%
	%msg% = %fun%.gradient(%h_y%)
%$$
set $italic h_y$$ to the derivative
of the function $latex h$$ with respect to its argument $latex y$$
(corresponding to the previous call to $syntax%%fun%.function%$$).
The syntax
$syntax%
	%msg% = %obj%.gradient(%f_x%)
%$$
set $italic f_x$$ to the derivative 
of the function $latex f$$ with respect to its argument $latex x$$
(corresponding to the previous call to $syntax%%obj%.function%$$).
The arguments $italic f_x$$ and $italic h_y$$ have prototype
$syntax%
	double *%h_y%
	double *%f_x%
%$$
Their lengths are $italic m$$ and $italic n$$ respectively.

$head Hessian$$
The syntax
$syntax%
	%msg% = %fun%.Hessian(%h_yy%)
%$$
set $italic h_yy$$ to the Hessian
of the function $latex h$$ with respect to its argument $latex y$$
(corresponding to the previous call to $syntax%%fun%.function%$$).
The syntax
$syntax%
	%msg% = %obj%.gradient(%f_xx%)
%$$
set $italic f_xx$$ to the Hessian 
of the function $latex f$$ with respect to its argument $latex x$$
(corresponding to the previous call to $syntax%%obj%.function%$$).
The arguments $italic h_yy$$ and $italic f_xx$$ have prototype
$syntax%
	double *%h_yy%
	double *%f_xx%
%$$
Their lengths are 
$latex m \times m$$ and $latex n \times n$$ respectively.
It does not matter if the matrices are stored in row major or column
major order because they should be symmetric.

$head msg$$
The $italic msg$$ results for 
$code function$$, $code gradient$$ and $code Hessian$$ above
has prototype
$syntax%
	const char *%msg%
%$$.
In each case,
the $italic msg$$ result for $italic obj$$ is equal
to the corresponding $italic msg$$ result for $italic fun$$.

$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$code
$verbatim%Test/zero_one_scale.cpp%0%// BEGIN PROGRAM%// END PROGRAM%$$
$$

$end
*/

# include <QN01Box/Memory.h>

namespace QN01Box { // Begin QN01Box namespace

template <class Fun>
class zero_one_scale {
private:
	// function pointer
	Fun* const     fun;
	// constants
	const size_t   m;
	const size_t   n;
	Memory<double> dmemory;
	double        *a;
	double        *d;
	// temporaries
	double        *y;
	double      *h_y;
	double     *h_yy;
	
public:
	zero_one_scale(
		Fun *fun_        , 
		size_t m_        , 
		size_t n_        ,
		const double *a_ , 
		const double *b_ ) 
	: fun(fun_) , m(m_) , n(n_) , dmemory(4 * m + m * m)
	{	size_t i, j;
		a    = dmemory(m);
		d    = dmemory(m);
		y    = dmemory(m);
		h_y  = dmemory(m);
		h_yy = dmemory(m * m);
		j = 0;
		for(i = 0; i < m; i++)
		{	a[i] = a_[i];	
			d[i] = b_[i] - a_[i];	
			QN01BoxUsageError( 
				d[i] >= 0.          ,
				"zero_one_scale"    ,
				"b[i] < a[i] for some index i"
			);
			j += size_t ( d[i] > 0. );
		}
		QN01BoxUsageError( 
			j == n           ,
			"zero_one_scale" ,
			"n not equal number of i such that a[i] < b[i]"
		);
	}
	void to_zero_one(double *x, const double *y_)
	{	size_t i, j;
		j = 0;
		for(i = 0; i < m; i++)
		{	if( d[i] > 0. )
			{	x[j] = (y_[i] - a[i]) / d[i];
				QN01BoxUsageError(
					0. <= x[j] && x[j] <= 1.    ,
					"zero_one_scale.to_zero_one",
					"y[i] < a[i] or b[i] < y[i]"
				);
				j++;
			}
		}
		QN01BoxUnknownError( j == n , "zero_one_scale.to_zero_one" );
		return;
	}
	void from_zero_one(const double *x_, double *y)
	{	size_t i, j;
		j = 0;
		for(i = 0; i < m; i++)
		{	if( d[i] > 0. )
			{	y[i] = a[i] + x_[j] * d[i];
				QN01BoxUsageError(
					0. <= x_[j] && x_[j] <= 1.  ,
					"zero_one_scale.to_zero_one",
					"x[j] < 0 or 1 < x[j]"
				);
				j++;
			}
			else	y[i] = a[i];
		}
		QN01BoxUnknownError( j == n , "zero_one_scale.from_zero_one" );
		return;
	}
	const char *function(const double *x_, double &f)
	{	const char *msg;
		from_zero_one(x_, y);
		msg = fun->function(y, f);
		return msg;
	}
	const char *gradient(double *f_x)
	{	const char *msg;
		size_t i, j;
		msg = fun->gradient(h_y);
		j = 0;
		for(i = 0; i < m; i++)
		{	if( d[i] > 0. )
			{	f_x[j] = h_y[i] * d[i];
				j++;
			}
		}
		QN01BoxUnknownError( j == n , "zero_one_scale.gradient" );
		return msg;
	}
	const char *Hessian(double *f_xx)
	{	const char *msg;
		size_t i1, j1, i2, j2;
		msg = fun->Hessian(h_yy);
		if( strcmp(msg, "not available") == 0 )
			return msg;
		j1 = 0;
		for(i1 = 0; i1 < m; i1++) 
		if( d[i1] > 0. )
		{	j2 = 0;
			for(i2 = 0; i2 < m; i2++) 
			if( d[i2] > 0. )
			{	f_xx[j1*n+j2] = h_yy[i1*m+i2] * d[i1] * d[i2];
				j2++;
			}
			QN01BoxUnknownError(
				j2 == n , "zero_one_scale.gradient" 
			);
			j1++;
		}
		QN01BoxUnknownError( j1 == n , "zero_one_scale.gradient" );
		return msg;
	}
};

} // End QN01Box namespace

# endif
