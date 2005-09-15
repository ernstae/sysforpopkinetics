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
	std
	ind
	dep
$$

$section Multiple Break Point and Output Point Ode Integrator$$

$table
$bold Syntax$$ $cnext
$syntax%OdeBreak(%method%, %eval%, %btime%, %otime%, %eabs%, %erel%, %xout%)%$$ 
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
$code double$$, $code CppAD::AD<double>$$ or
$code CppAD::AD< CppAD::AD<double> >$$.

$head method$$
The argument $italic method$$ has prototype
$syntax%
	const std::string &%method%
%$$ 
It's value must be equal to one of the following
$table
$bold method$$    $cnext $bold Description$$
$rnext
$code "Runge45"$$ $cnext 
	a 5-th order Runge-Kutta solver with 4-th order error estimation 
$rnext
$code "Rosen34"$$ $cnext 
	a 4-th order Rosenbrock solver with 3-rd order error estimation 
$tend
Note that if $italic method$$ is equal to $code "Runge45"$$,
the functions 
$syntax%%eval%.Ode_ind%$$ and $syntax%%eval%.Ode_dep%$$ 
will not be called.
In this case these functions only need have the correct prototypes; i.e.,
it is not necessary to compute the corresponding derivatives
$italic f_ind$$ and $italic f_dep$$.
(One could use an assert in the corresponding functions
to assure they are not called.)

$head eval$$
The class $italic Eval$$ and 
the object $italic eval$$ satisfy the prototype
$syntax%
	%Eval% &%eval%
%$$

$subhead eval.Break$$
The object $italic eval$$ must have a member function named
$code Break$$ that supports the syntax
$syntax%
	%eval%.Break(%k%, %x%, %g%)
%$$
which sets the vector $italic g$$ equal to
$latex G^k(x)$$.

$subhead g$$
The argument $italic g$$ to $syntax%%eval%.Break%$$ 
is a vector with $latex N$$ elements and has prototype
$syntax%
	%Vector% &%g%
%$$
The input value of its elements does not matter.
The output value of its elements is equal to 
$latex G^k(x)$$.

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

$subhead eval.Ode$$
The object $italic eval$$ must also have a member function
$code Ode$$ that supports the syntax
$syntax%
	%eval%.Ode(%t%, %x%, %f%)
%$$ 
This sets the vector $italic f$$ equal to
$latex F^k (t, x)$$ where
$italic k$$ corresponds to the previous call to
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.

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

$subhead eval.Ode_ind$$
The object $italic eval$$ must also have a member function
$code Ode_ind$$ that supports the syntax
$syntax%
	%eval%.Ode_ind(%t%, %x%, %f_ind%)
%$$ 
This sets the vector $italic f_ind$$ equal to
$latex \partial_t F^k (t, x)$$ where
$italic k$$ corresponds to the previous call to
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.

$subhead f_ind$$
The argument $italic f_ind$$ to $syntax%%eval%.Ode_ind%$$ 
is a vector with $latex N$$ elements and has prototype
$syntax%
	%Vector% &%f_ind%
%$$
The input value of its elements does not matter.
The output value of its elements is equal to 
$latex \partial_t F^k (t, x)$$
where $latex k$$ corresponds to the previous call to 
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.

$subhead eval.Ode_dep$$
The object $italic eval$$ must also have a member function
$code Ode_dep$$ that supports the syntax
$syntax%
	%eval%.Ode_dep(%t%, %x%, %f_dep%)
%$$ 
This sets the vector $italic f_dep$$ equal to
$latex \partial_x F^k (t, x)$$ where
$italic k$$ corresponds to the previous call to
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.

$subhead f_dep$$
The argument $italic f_dep$$ to $syntax%%eval%.Ode_dep%$$ 
is a vector with $latex N^2$$ elements and has prototype
$syntax%
	%Vector% &%f_dep%
%$$
The input value of its elements does not matter.
The output value of the element 
$latex \[
	f\_{dep} [i * N + j] = \partial_{x(j)} F_i^k (t, x)
\] $$
where $latex k$$ corresponds to the previous call to 
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.

$subhead t$$
The argument $italic t$$ to 
$syntax%%eval%.Ode%$$, 
$syntax%%eval%.Ode_ind%$$,  and
$syntax%%eval%.Ode_dep%$$, 
has prototype
$syntax%
	const %Scalar% &%t%
%$$
and $latex b_k \leq t \leq b_{k+1}$$ 
where $latex k$$ corresponds to the previous call to 
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.

$subhead x$$
The argument $italic x$$ to 
and $syntax%%eval%.Break%$$,
$syntax%%eval%.Ode%$$, 
$syntax%%eval%.Ode_ind%$$,  and
$syntax%%eval%.Ode_dep%$$, 
is a vector with $latex N$$ elements and has prototype
$syntax%
	const %Vector% &%x%
%$$

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
	test/unit/src/OdeBreakTest.cpp
%$$
$head Example$$
The routine $xref/OdeBreakOk/$$ is an example and test of $code OdeBreak$$.
It returns true if the test passes and false otherwise.

$head Wish List$$
Add an estimate of the accuracy of the solution to be used when
evaluating a integration solution from a CppAD ADFun<double> object.
This would enable one to know when it is necessary to re-tape for accuracy.

$end
*/

# include <CppAD/OdeErrControl.h>
# include <CppAD/Runge45.h>
# include <CppAD/Rosen34.h>
# include <cmath>
# include <spk/SpkException.h>

// Maximum number of Ode steps per time interval where the time intervals
// are defined by the union of the break point times and output times.
# define MaxNumberOdeStep 1000

// Minimum number of Ode steps between the initial and final time.
# define MinNumberOdeStep   10

namespace {
	// local abs funciton for use of OdeBreak.h without all of CppAD
	// will cause a conflict if "using namespace CppAD" is also used. 
	double abs(double x)
	{	return	std::fabs(x); }
}

template <typename Scalar, typename Vector, typename Eval>
class StepMethod {
private:
	Eval *F;
	enum { runge45, rosen34 } method;
public:
	StepMethod(Eval *eval, const std::string &method_)
	{	F = eval; 
		if( method_ == "Runge45" )
			method = runge45;
		else if( method_ == "Rosen34" )
			method = rosen34;
		else
		{	throw SpkException(
				SpkError::SPK_USER_INPUT_ERR,
				"Invalid Ode Integration method",
				__LINE__,
				__FILE__
			);
		}
	}
	size_t order(void)
	{	if( method == runge45 )
			return 4; 
		else	return 3;
	}
	void step(
		const Scalar &ta, 
		const Scalar &tb, 
		const Vector &xa, 
		Vector &xb      ,
		Vector &eb      )
	{	size_t M = 1;
		if( method == runge45 )
			xb = CppAD::Runge45(*F, M, ta, tb, xa, eb); 
		else	xb = CppAD::Rosen34(*F, M, ta, tb, xa, eb); 
	};
};

template <typename Scalar, typename Vector, typename Eval>
void OdeBreak(
	const std::string &method,
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

	// has a step size warning already been issued
	static bool OdeBreakWarning = false; 

	// message
	const char *message;


	if( K == 0 )  // number of break times must be greater than zero
	{	message = "Number of break times for the ODE solver is zero.";
		throw SpkException(
			SpkError::SPK_USER_INPUT_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}
	if( J == 0 )  // number of output times must be greater than zero
	{	message = "Number of output times for the ODE solver is zero.";
		throw SpkException(
			SpkError::SPK_USER_INPUT_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}
	if( N == 0 )  // state space dimension must be greater than zero
	{	message = "Number of ODE dependent variables is zero.";
		throw SpkException(
			SpkError::SPK_USER_INPUT_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}
	// result vector must have size equal to state space dimension
	// times the number of output times
	if( N * J != xout.size() )
	{	message = "ODE solution vector has the wrong size.";
		throw SpkException(
			SpkError::SPK_PROGRAMMER_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}

	// local vectors with same length as x 
	Vector x(N), g(N), e(N), enext(N), xnext(N);

	size_t k, j;
	// break point times 
	if( K > 1 )
	{	// values in btime vector must be monatone non-decreasing
		for(k = 0; k < K-2; k++)
		{
			if( btime[k] > btime[k+1] )
			{	message = "Ode break times must be"
					" in non-decreasing order.";
				throw SpkException(
					SpkError::SPK_PROGRAMMER_ERR,
					message ,
					__LINE__,
					__FILE__
				);
			}
		}
	}

	
	// first break time must be less than first output time
	if( btime[0] >= otime[0] )
	{	message =
		"Ode first break time must be less than first output time.";
		throw SpkException(
			SpkError::SPK_PROGRAMMER_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}
	if( J > 1 )
	{	// values in the otime vector must be monatone increasing
		for(j = 0; j < J-2; j++)
		{
			if( otime[j] >= otime[j+1] )
			{	message = "Ode output times must be"
					" in increasing order.";
				throw SpkException(
					SpkError::SPK_PROGRAMMER_ERR,
					message ,
					__LINE__,
					__FILE__
				);
			}
		}
	}
	// last break time must be less than last output time
	if( btime[K-1] >= otime[J-1] )
	{	message = "Last Ode break time must be"
				" less than last output time.";
		throw SpkException(
			SpkError::SPK_PROGRAMMER_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}

	// integration method
	StepMethod<Scalar, Vector, Eval> stepMethod(&eval, method);

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
	{	message = "An OdeBreak internal consistency check failed.";

		// internal consistency check in OdeBreak
		if( t >= otime[j] )
		{	throw SpkException(
				SpkError::SPK_PROGRAMMER_ERR,
		  		message ,
				__LINE__,
				__FILE__
			);
		}

		if( k < K )
		{	// internal consistency check in OdeBreak
			if( t > btime[k] )
			{	throw SpkException(
					SpkError::SPK_PROGRAMMER_ERR,
			  		message ,
					__LINE__,
					__FILE__
				);
			}

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
		if( tnext <= t )
		{	throw SpkException(
				SpkError::SPK_PROGRAMMER_ERR,
				message ,
				__LINE__,
				__FILE__
			);
		}

		// absolute error bound for this step
		Scalar fraction = (tnext - t) / total;
		for(i = 0; i < N; i++)
			e[i] = fraction * eabs[i];

		// solve the ODE from t to tnext
		bool ok = false;
		while( ! ok )
		{	xnext = CppAD::OdeErrControl(stepMethod, 
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
			}
			shrink  = ! ok;
			if( shrink )
			{	if( smin > tnext - t )
				{	// actual step during last call was 
					smin = tnext - t;
				}
				smin = smin / 2;
			}
			shrink &= ( (tnext - t) / smin) < MaxNumberOdeStep;

			// check for minimum step size
			if( !( ok | shrink ) )
			{	ok      = true;
				message =
				"Ode solver cannot obtain desired accuracy";
				if( ! OdeBreakWarning )
					std::cout << message << std::endl;	
				OdeBreakWarning = true;
			}
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
