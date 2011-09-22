# ifndef OdeBreakIncluded
# define OdeBreakIncluded

/*
$begin OdeBreak$$
$latex \newcommand{\R}{{\bf R}}$$
$spell
	btime
	otime
	Runge
	Kutta
	Rosen
	Rosenbrock
	erel
	Cpp
	const
	eval
	eabs
	xout
$$

$section Multiple Break Point and Output Point Ode Integrator$$

$table
$bold Syntax$$
$syntax%OdeBreak(%eval%, %btime%, %otime%, %eabs%, %erel%, %xout%)%$$ 
$tend

$fend 20$$

$head Notation$$
$table
$bold Notation$$ $cnext $bold Description$$ $rnext
$latex J$$     $cnext number of output times, $latex J \geq 1$$  $rnext
$latex t_j$$   $cnext $th j$$ output time;                  $rnext
$latex K$$     $cnext number of break point times, $latex K \geq 1$$  $rnext
$latex b_k$$   $cnext for $latex k < K$$, 
               time of the $th k$$ break point              $rnext
$latex N$$     $cnext number of components in $latex X(t)$$ $rnext
$latex F^k$$   $cnext smooth part of differential equation  $rnext
$latex G^k$$   $cnext delta function multiplier in ODE
$tend

$head Define X(t)$$
We are given an array $latex b \in \R^K$$
such that for $latex k = 0 , \ldots , K-2$$, $latex b_k \leq b_{k+1}$$.
We define 
$latex X^{-1} (t) \equiv 0$$ for all $latex t \in ( - \infty , b_0 ]$$
and for $latex k = 0 , \ldots , K-1$$,
we define $latex X^k : [ b_k , b_{k+1} ] \rightarrow \R^n$$ by
$latex \[
\begin{array}{rcl}
X^k ( b_k ) & = & X^{k-1} ( b_k ) + G^k [ X^{k-1} ( b_k ) ] 
\\
\partial_t X^k (t)  & = & F^k [t, X^k (t)]
\end{array}
\] $$
Here and below we 
assume that $latex b_{K-1} < t_{J-1}$$ and we extend the vector $latex b$$ by
$latex \[
	b_K  =  t_{J-1} 
\] $$
We define the function 
$latex X : ( b_0 , b_K ] \rightarrow \R^N$$ by
$latex X(t) = X^k (t)$$ where $latex k$$ is chosen so that
$latex b_k < t  \leq b_{k+1}$$.
Note that only $latex k$$ such that $latex b_k < b_{k+1}$$
can satisfy this condition.
Also note that this definition makes $latex X(t)$$ left continuous; i.e.
$latex \[
	X(t) = {\rm lim} \; X(s) \; {\rm as} \; s \uparrow t
\] $$.

$head Vector$$
The type $italic Vector$$ must be a
$href%
	http://www.seanet.com/~bradbell/CppAD/simplevector.xml%
	SimpleVector
%$$
class with elements of type $italic Scalar$$.

$head Scalar$$
The type $italic Scalar$$ is either 
$code double$$ or $code CppAD::AD<double>$$.

$head eval$$
The class $italic Eval$$ and 
the object $italic eval$$ satisfy the prototype
$syntax%
	%Eval% &%eval%
%$$
The object $italic eval$$ must have a member function named
$code Break$$ that supports the syntax
$syntax%
	%eval%.Break(%k%, %x%, %g%)
%$$
which sets the vector $italic g$$ equal to
$latex G^k(x)$$.
The object $italic eval$$ must also have a member function
$code Ode$$ that supports the syntax
$syntax%
	%eval%.Ode(%t%, %x%, %f%)
%$$ 
This sets the vector $italic f$$ equal to
$latex F^k (t, x)$$ where
$italic k$$ corresponds to the previous call to
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.

$subhead k$$
The argument $italic k$$ to $syntax%%eval%.Break%$$
has prototype
$syntax%
	size_t %k%
%$$
It specifies that the function 
$latex X^k : [ b_k , b_{k+1} ] \rightarrow \R^n$$
corresponds to subsequent calls to
$syntax%%eval%.Ode%$$. 
The value of $italic k$$ 
will be between 0 and $latex K-1$$ inclusive.

$subhead t$$
The argument $italic t$$ to $syntax%%eval%.Ode%$$ 
has prototype
$syntax%
	const %Scalar% &%t%
%$$
and $latex b_k \leq t \leq b_{k+1}$$ 
where $latex k$$ corresponds to the previous call to 
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.

$subhead x$$
The argument $italic x$$ to $syntax%%eval%.Ode%$$ 
and $syntax%%eval%.Break%$$ 
is a vector with $latex N$$ elements and has prototype
$syntax%
	const %Vector% &%x%
%$$

$subhead f$$
The argument $italic f$$ to $syntax%%eval%.Ode%$$ 
is a vector with $latex N$$ elements and has prototype
$syntax%
	%Vector% &%f%
%$$
The input value of its elements does not matter.
The output value of its elements is equal to 
$latex F^k (t, x)$$
where $latex k$$ corresponds to the previous call to 
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.

$subhead g$$
The argument $italic g$$ to $syntax%%eval%.Break%$$ 
is a vector with $latex N$$ elements and has prototype
$syntax%
	%Vector% &%g%
%$$
The input value of its elements does not matter.
The output value of its elements is equal to 
$latex G^k(x)$$.

$head btime$$
The argument $italic btime$$ 
is a vector with $latex K$$ elements and has prototype
$syntax%
	const %Vector% &%btime%
%$$
It specifies the break point times for the differential equation; i.e.,
for $latex k < K, b_k = btime[k]$$.
In addition, 
the function $latex F^k (t, x)$$ is assumed to be infinitely differentiable 
with respect to $latex (t, x)$$.
It is assumed that 
$latex \[ 
	b_0 \leq b_1 \leq \ldots \leq b_{K-1}
\] $$

$head otime$$
The argument $italic otime$$ is a vector with $latex J$$ elements
and has prototype
$syntax%
	const %Vector% &%otime%
%$$
It specifies the times corresponding to the output values
for the solution of the differential equation; i.e. $latex X(t)$$.
It is assumed that 
$latex \[ 
	t_0 < t_1 < \ldots < t_{J-1}
\] $$
and $latex b_0 < t_0$$, $latex b_{K-1} < t_{J-1}$$.
where $latex t_j$$ is $syntax%%otime%[%j%]%$$.

$head eabs$$
The argument $italic eabs$$ is a vector of length $latex N$$
and has prototype
$syntax%
	const %Vector% &%eabs%
%$$
This specifies the desired absolute accuracy in the output values
(see $italic erel$$ below for more details).

$head erel$$
The argument $italic eabs$$ has prototype
$syntax%
	const %Scalar% &%erel%
%$$
and specifies the desired relative accuracy in the output values; i.e.,
for $latex i = 0 , \ldots , N-1$$ and $latex j = 0 , \ldots , J-1$$,
$latex \[
eabs[i] + erel * \left| X_i ( t_j ) \right|  
\geq 
\left| xout[ i + j * N ] - X_i ( t_j ) \right|
\] $$
where $latex t_j$$ is $syntax%%otime%[%j%]%$$.


$head xout$$
The argument $italic xout$$ is a vector of length $latex N * J$$
and has prototype
$syntax%
	%Vector% &%xout%
%$$
For $latex i = 0 , \ldots , N-1$$ and $latex j = 0 , \ldots , J-1$$,
the value $latex xout[ i + j * N ]$$ is an approximation
for $latex X_i ( otime[j] )$$ 
(see $italic erel$$ above for more details).

$children%
	OdeBreakOk.cpp
%$$
$head Example$$
The routine $xref/OdeBreakOk/$$ is an example and test of $code OdeBreak$$.
It returns true if the test passes and false otherwise.

$head Wish List$$
This is a preliminary version of this routine.
Below is a wish list for future enhancements
(feel free to suggest additions to this list):
$list number$$
Add a method option, name the current method $code Runge45$$
(a Runge-Kutta method),
and include the $code Rosen34$$ method 
(a Rosenbrock method) as an option.
$lnext
Add error detection for special cases such as
the error criteria could not be met or the 
evaluation of $latex F^k$$, or $latex G^k$$ failed.
$lend


$end
*/

# include <CppAD/OdeErrControl.h>
# include <CppAD/Runge45.h>
# include <cmath>

# define MaxNumberOdeStep 1000
# define MinNumberOdeStep   10

namespace {
	// local abs funciton for use of OdeBreak.h without all of CppAD
	// will cause a conflict if "using namespace CppAD" is also used. 
	double abs(double x)
	{	return	std::fabs(x); }
}

template <typename Scalar, typename Vector, typename Eval>
class Method {
private:
	Eval *F;
public:
	Method(Eval *eval)
	{	F = eval; }
	size_t order(void)
	{	return 4; }
	void step(
		const Scalar &ta, 
		const Scalar &tb, 
		const Vector &xa, 
		Vector &xb      ,
		Vector &eb      )
	{	size_t M = 1;
		xb = CppAD::Runge45(*F, M, ta, tb, xa, eb); 
	};
};

template <typename Scalar, typename Vector, typename Eval>
void OdeBreak(
	Eval &eval, 
	const Vector &btime, 
	const Vector &otime , 
	const Vector &eabs , 
	const Scalar &erel , 
	Vector &xout       )
{
	// Dimensions for this problem
	const size_t K = btime.size();
	const size_t J = otime.size();
	const size_t N = eabs.size();
	assert( K > 0 );  // number of break times must be greater than zero
	assert( J > 0 );  // number of output times must be greater than zero
	assert( N > 0 );  // state space dimension msut be greater than zero
	// result vector must have size equal to state space dimension
	// times the number of output times
	assert( N * J == xout.size() ); 

	// local vectors with same length as x 
	Vector x(N), g(N), e(N), enext(N), xnext(N);

	// break point times 
	size_t k;
	if( K > 1 )
	{	// values in btime array must be monatone non-decreasing
		for(k = 0; k < K-2; k++)
			assert( btime[k] <= btime[k+1] );
	}

	size_t j;
	
	// first break time must be less than first output time
	assert( btime[0] < otime[0] );
	if( J > 1 )
	{	// values in the otime array must be monatone increasing
		for(j = 0; j < J-2; j++)
			assert( otime[j] < otime[j+1] );
	}
	// last break time must be less than last output time
	assert( btime[K-1] < otime[J-1] );

	// integration method
	Method<Scalar, Vector, Eval> method(&eval);

	// total integration time
	Scalar total = otime[J-1] - btime[0];

	// step size parameters
	Scalar smax = total / MinNumberOdeStep;
	Scalar smin = smax;
	Scalar scur = smax;

	// initialize for loop
	k        = 0;        // break point index
	j        = 0;        // output point index
	Scalar t = btime[0];
	size_t i;
	for(i = 0; i < N; i++)
		x[i] = 0.;

	// repeat until all output points are calculated
	while( j < J )
	{	// internal consistency check in OdeBreak
		assert( t < otime[j] );
		if( k < K )
		{	// internal consistency check in OdeBreak
			assert( t <= btime[k] );	

			// check if this is a break point
			while( k < K && btime[k] == t )
			{	eval.Break(k, x, g);
				for(i = 0; i < N; i++)
					x[i] += g[i];
				k++;
			}
		}

		// determine next integration time limit
		Scalar tnext;
		if( k < K && btime[k] < otime[j] )
			tnext = btime[k];
		else	tnext = otime[j];

		// internal consistency check in OdeBreak
		assert( tnext > t );

		// absolute error bound for this step
		Scalar fraction = (tnext - t) / total;
		for(i = 0; i < N; i++)
			e[i] = fraction * eabs[i];

		// solve the ODE from t to tnext
		bool ok = false;
		while( ! ok )
		{	xnext = CppAD::OdeErrControl(method, 
				t, 
				tnext, 
				x, 
				smin, 
				smax, 
				scur, 
				e, 
				fraction * erel, 
				enext
			);

			// check if error criteria is satisfied at time tnext
			bool shrink = false;
			ok          = true;
			for(i = 0; i < N; i++)
			{	// assume that x(t) is monatone over interval
				Scalar reli = 
				(abs(x[i]) + abs(xnext[i])) * erel * fraction;
				ok     &= ( enext[i] <= reli + e[i]);
				shrink |= ( enext[i] > (reli + e[i]) / 10. );
			}
			shrink &= ( total / smin ) < MaxNumberOdeStep;
			if( shrink )
				smin = smin / 2;

			// internal consistency check in OdeBreak
			assert( ok | shrink );

			// avoid infinite loop
			if( ! ok )
				if( ! shrink )
					ok = true;
		}

		// advance t
		t = tnext;
		x = xnext;

		// check for output point
		if( t == otime[j] )
		{	for(i = 0; i < N; i++)
				xout[ i + j * N ] = x[i];
			j++; // index of next output time
		}
	}
}

# endif
