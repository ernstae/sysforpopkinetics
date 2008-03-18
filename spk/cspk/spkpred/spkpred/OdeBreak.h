# ifndef OdeBreakIncluded
# define OdeBreakIncluded

/*
$begin OdeBreak$$
$latex \newcommand{\R}{{\bf R}}$$
$spell
	oleft
	Bool
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
$syntax%OdeBreak(%eval%, %xout%,
	%method%, %btime%, %otime%, %oleft%, %eabs%, %erel%)%$$ 
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

$head FloatVector$$
The type $italic FloatVector$$ must be a
$href%
	http://www.seanet.com/~bradbell/CppAD/simplevector.xml%
	SimpleVector
%$$
class with elements of type $italic Scalar$$.

$head BoolVector$$
The type $italic BoolVector$$ must be a
$href%
	http://www.seanet.com/~bradbell/CppAD/simplevector.xml%
	SimpleVector
%$$
class with elements of type $code bool$$.

$head Scalar$$
The type $italic Scalar$$ is either 
$code double$$, $code CppAD::AD<double>$$ or
$code CppAD::AD< CppAD::AD<double> >$$.

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

$syntax%
%g%
%$$
The argument $italic g$$ to $syntax%%eval%.Break%$$ 
is a vector with $latex N$$ elements and has prototype
$syntax%
	%FloatVector% &%g%
%$$
The input value of its elements does not matter.
The output value of its elements is equal to 
$latex G^k(x)$$.

$syntax%
%k%
%$$
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

$syntax%
%f%
%$$
The argument $italic f$$ to $syntax%%eval%.Ode%$$ 
is a vector with $latex N$$ elements and has prototype
$syntax%
	%FloatVector% &%f%
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

$syntax%
%f_ind%
%$$
The argument $italic f_ind$$ to $syntax%%eval%.Ode_ind%$$ 
is a vector with $latex N$$ elements and has prototype
$syntax%
	%FloatVector% &%f_ind%
%$$
The input value of its elements does not matter.
The output value of its elements is equal to 
$latex \partial_t F^k (t, x)$$
where $latex k$$ corresponds to the previous call to 
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.
If $syntax%%eval%.Ode_ind%$$ is called, $syntax%%eval%.Ode_dep%$$
will be called directly after it with the same values
for the arguments $italic t$$ and $italic x$$.

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

$syntax%
%f_dep%
%$$
The argument $italic f_dep$$ to $syntax%%eval%.Ode_dep%$$ 
is a vector with $latex N^2$$ elements and has prototype
$syntax%
	%FloatVector% &%f_dep%
%$$
The input value of its elements does not matter.
The output value of the element 
$latex \[
	f\_{dep} [i * N + j] = \partial_{x(j)} F_i^k (t, x)
\] $$
where $latex k$$ corresponds to the previous call to 
$syntax%%eval%.Break(%k%, %x%, %g%)%$$.
If $syntax%%eval%.Ode_dep%$$ is called, $syntax%%eval%.Ode_ind%$$
was called directly before it with the same values
for the arguments $italic t$$ and $italic x$$.

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
	const %FloatVector% &%x%
%$$

$head xout$$
The argument $italic xout$$ is a vector of length $latex N * J$$
and has prototype
$syntax%
	%FloatVector% &%xout%
%$$
For $latex i = 0 , \ldots , N-1$$ and $latex j = 0 , \ldots , J-1$$,
the value $latex xout[ i + j * N ]$$ is an approximation
for $latex X_i ( otime[j] )$$ 
(see $italic erel$$ above for more details).

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

$head btime$$
The argument $italic btime$$ 
is a vector with $latex K$$ elements and has prototype
$syntax%
	const %FloatVector% &%btime%
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
	const %FloatVector% &%otime%
%$$
It specifies the times corresponding to the output values
for the solution of the differential equation; i.e. $latex X(t)$$.
It is assumed that 
$latex \[ 
\begin{array}{c}
b_0 \leq t_0 
\\
b_{K-1} \leq t_{J-1}
\\
t_0 \leq t_1 \leq \ldots \leq t_{J-1}
\\
t_j = t_{j+1} \Rightarrow 
	L_j {\rm \; is \; true \; and \;} 
	L_{j+1} {\rm \; is \; false} 
\end{array}
\] $$
where $latex t_j$$ is $syntax%%otime%[%j%]%$$
and $latex L_j$$ is $syntax%%oleft%[%j%]%$$.

$head oleft$$
The argument $italic oleft$$ is a vector with $latex J$$ elements
and has prototype
$syntax%
	const %BoolVector% &%oleft%
%$$
It specifies if the output values correspond to the left (or right)
solution of the differential equation.
The left solution is defined by
$latex \[ 
	xout[ i + j * N ] \approx \lim_{t \uparrow otime[j]} X(t)
\] $$
The right solution is defined by
$latex \[ 
	xout[ i + j * N ] \approx \lim_{t \downarrow otime[j]} X(t)
\] $$

$head eabs$$
The argument $italic eabs$$ is a vector of length $latex N$$
and has prototype
$syntax%
	const %FloatVector% &%eabs%
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


$children%
	../test/unit/src/OdeBreakTest.cpp
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
# include <sstream>
# include <cmath>
# include <spk/isNotANumber.h>
# include <spk/SpkException.h>
# include <spk/WarningsManager.h>

// Maximum number of Ode steps per time interval where the time intervals
// are defined by the union of the break point times and output times.
# define MaxNumberOdeStep 1000

// Minimum number of Ode steps between the initial and final time.
# define MinNumberOdeStep   10

// incase a previous preprocessor macro has defined abs
# undef abs

namespace {
	// local abs funciton for use of OdeBreak.h without all of CppAD
	// will cause a conflict if "using namespace CppAD" is also used. 
	double abs(double x)
	{	return	std::fabs(x); }
}

template <class Eval, class Scalar, class FloatVector>
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
		const FloatVector &xa, 
		FloatVector &xb      ,
		FloatVector &eb      )
	{	size_t M = 1;
		if( method == runge45 )
			xb = CppAD::Runge45(*F, M, ta, tb, xa, eb); 
		else	xb = CppAD::Rosen34(*F, M, ta, tb, xa, eb); 
	};
};

template <class Eval, class Scalar, class FloatVector, class BoolVector>
void OdeBreak(
	Eval              &eval    , 
	FloatVector       &xout    ,
	const std::string &method  ,
	const FloatVector &btime   , 
	const FloatVector &otime   , 
	const BoolVector  &oleft   , 
	const FloatVector &eabs    , 
	const Scalar      &erel    )
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
	{	message = "OdeBreak: number of break times is zero.";
		throw SpkException(
			SpkError::SPK_USER_INPUT_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}
	if( J == 0 )  // number of output times must be greater than zero
	{	message = "OdeBreak: number of output times is zero.";
		throw SpkException(
			SpkError::SPK_USER_INPUT_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}
	if( N == 0 )  // state space dimension must be greater than zero
	{	message = "OdeBreak: number of dependent variables is zero.";
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
	{	message = "OdeBreak: solution vector has the wrong size.";
		throw SpkException(
			SpkError::SPK_PROGRAMMER_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}

	// local vectors with same length as x 
	FloatVector x(N), g(N), e(N), enext(N), xnext(N), maxnext(N), maxabs(N);

	size_t k, j;

	// break point times 
	k = K;
	while( k > 1 )
	{	k--;
		if( btime[k-1] > btime[k] )
		{	message =  "OdeBreak: "
			"break point times must be monotone non-decreasing";
			throw SpkException(
				SpkError::SPK_PROGRAMMER_ERR,
				message ,
				__LINE__,
				__FILE__
			);
		}
	}

	// first break time must be less than first output time
	if( otime[0] < btime[0] )
	{	message = "OdeBreak: "
		"first output time must be greater or equal first break time.";
		throw SpkException(
			SpkError::SPK_PROGRAMMER_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}

	// check order of output times
	j = J;
	while( j > 1 )
	{	j--;
		if( otime[j-1] > otime[j] )
		{	message = "OdeBreak: "
			"output times must be monotone non-decreasing.";
			throw SpkException(
				SpkError::SPK_PROGRAMMER_ERR,
				message ,
				__LINE__,
				__FILE__
			);
		}
		if( otime[j-1] == otime[j] && ((! oleft[j-1]) || oleft[j]) )
		{	message = "OdeBreak: "
			"equal output times do not obey rule for oleft.";
			throw SpkException(
				SpkError::SPK_PROGRAMMER_ERR,
				message ,
				__LINE__,
				__FILE__
			);
		}
	}

	// last break time must be less than or equal last output time
	if( btime[K-1] > otime[J-1] )
	{	message = "OdeBreak: "
		"last break time must be less or equal last output time.";
		throw SpkException(
			SpkError::SPK_PROGRAMMER_ERR,
			message ,
			__LINE__,
			__FILE__
		);
	}

	// integration method
	StepMethod<Eval, Scalar, FloatVector> stepMethod(&eval, method);

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
	{	x[i] =      Scalar(0);
		maxabs[i] = Scalar(0);
	}

	// repeat until all output points are calculated
	while( j < J )
	{	if( t > otime[j] )
		{	message = 
			"OdeBreak: an internal consistency check failed.";
			throw SpkException(
				SpkError::SPK_PROGRAMMER_ERR,
		  		message ,
				__LINE__,
				__FILE__
			);
		}
		if( k < K && t > btime[k] )
		{	message = 
			"OdeBreak: an internal consistency check failed.";
			throw SpkException(
				SpkError::SPK_PROGRAMMER_ERR,
		  		message ,
				__LINE__,
				__FILE__
			);
		}
		// check for output point (before break point)
		if( t == otime[j] && oleft[j] )
		{	for(i = 0; i < N; i++)
				xout[ i + j * N ] = x[i];
			j++; // index of next output time

			// check if we are done
			if( j == J )
				return;
		}
		// check if this is a break point
		while( k < K && btime[k] == t )
		{	eval.Break(k, x, g);
			for(i = 0; i < N; i++)
			{	x[i] += g[i];
				if( abs(x[i]) > maxabs[i] )
					maxabs[i] = abs(x[i]);
			}
			k++;
		}
		// check for output point (after break point)
		if( t == otime[j] && (! oleft[j]) )
		{	for(i = 0; i < N; i++)
				xout[ i + j * N ] = x[i];
			j++; // index of next output time

			// check if we are done
			if( j == J )
				return;
		}

		// determine next integration time limit
		Scalar tnext;
		if( k < K && j < J && btime[k] < otime[j] )
			tnext = btime[k];
		else	tnext = otime[j];

		if( tnext <= t )
		{	message = 
			"OdeBreak: an internal consistency check failed.";
			throw SpkException(
				SpkError::SPK_PROGRAMMER_ERR,
		  		message ,
				__LINE__,
				__FILE__
			);
		}

		// max sure smin is not to small for next interval
		if( smin < (tnext - t) / Scalar(MaxNumberOdeStep) )
			smin = (tnext - t) / Scalar(MaxNumberOdeStep);

		// absolute error bound for this interval (number of intervals
		// must be less than or equal btime.size() + otime.size() )
		Scalar fraction = total / Scalar(btime.size() + otime.size());
		for(i = 0; i < N; i++)
		{	if( erel * maxabs[i] > eabs[i] )
				e[i] = erel * maxabs[i];
			else	e[i] = eabs[i];
			e[i] *= fraction / Scalar(2);
		}

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
				fraction * erel / Scalar(2), 
				enext,
				maxnext
			);

			// check first element of xnext to see if it's Not a 
			// Number (NaN) since OdeErrControl will set xnext and
			// enext equal to NaN's if it cannot find a step size
			// that will allow the ODE's to be integrated successfully
			if( isNotANumber( xnext[0] ) )
			  {	std::ostringstream messageOSS;
				messageOSS << "The ordinary differential equations (ODE's) could not be solved for \ntime values from " 
					   << t << " to " << tnext << ".";
				throw SpkException(
					SpkError::SPK_ODE_SOLN_ERR,
					messageOSS.str().c_str(),
					__LINE__,
					__FILE__
				);
			}

			// check if error criteria is satisfied at time tnext
			bool shrink = false;
			ok          = true;
			for(i = 0; i < N; i++)
			{	// update maxabs
				Scalar maxabsi = maxabs[i];
				if( maxnext[i] > maxabsi )
					maxabsi = maxnext[i];

				// combine relative and absolute error
				Scalar bnd = maxabsi * erel + eabs[i];
				ok         &= static_cast<bool>(enext[i] <= bnd * fraction);
			}
			shrink  = ! ok;
			if( shrink )
			{	if( smin > tnext - t )
				{	// actual step during last call was 
					smin = tnext - t;
				}
				smin = smin / 2;
			}
			shrink &= static_cast<bool>(smin > (tnext - t)/Scalar(MaxNumberOdeStep));

			// check for minimum step size
			if( !( ok | shrink ) )
			{	ok      = true;
				message =
				"The ordinary differential equations (ODE's) were solved but not to the desired accuracy.";
				if( ! OdeBreakWarning )
				{	std::cout << message << std::endl;	
					WarningsManager::addWarning( message, __LINE__, __FILE__);
				}
				OdeBreakWarning = true;
			}

			// only use ok steps to update maxabs
			if( ok )
			{	for(i = 0; i < N; i++)
				{	// update maxabs
					if( maxnext[i] > maxabs[i] )
						maxabs[i] = maxnext[i];
				}
			}
		}

		// advance t
		t = tnext;
		x = xnext;
	}
	message = "OdeBreak: an internal consistency check failed.";
	throw SpkException(
		SpkError::SPK_PROGRAMMER_ERR,
		message ,
		__LINE__,
		__FILE__
	);
}

# endif
