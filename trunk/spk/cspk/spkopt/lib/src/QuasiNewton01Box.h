# ifndef QuasiNewton01BoxIncluded
# define QuasiNewton01BoxIncluded
/*
-----------------------------------------------------------------------
From:   Resource Facility for Population Kinetics
          Department of Bioengineering Box 352255
          University of Washington
          Seattle, WA 98195-2255

This file is part of the System for Population Kinetics (SPK), which
was developed with support from NIH grants RR-12609 and P41-
EB001975. Please cite these grants in any publication for which this
software is used and send a notification to the address given above.

SPK is Copyright (C) 1998-2003, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution.
-----------------------------------------------------------------------
Software:    Brad Bell (brad@apl.washington.edu)
Mathematics: Brad Bell & Jim Burke (burke@math.washington.edu)


$begin QuasiNewton01Box$$
$include Macro.tex$$
$spell
	Tmp
	Bradley
	bool
	optimizer
	subproblem
	subproblems
	det
	obj
	fp
	fm
	Bfgs
	df
	dq
	Complementarity
	const
	ostream
	Itr
	std
$$

$section Nonlinear Optimization with Box Constraints$$
$center
	(Bradley M. Bell & James V. Burke)
$$

$table
$bold Syntax$$ $cnext
$syntax%template <class %Fun%>
const char * QuasiNewton01Box(
	// Arguments that are only Inputs
	std::ostream    &%os%,
	int            %level%,
	size_t       %ItrMax%,
	size_t      %QuadMax%,
	size_t            %n%,
	double        %delta%,
	%Fun%          &%obj%,
	// Arguments that are both Inputs and Outputs
	bool        &%sOkCur%,
	size_t      &%ItrCur%,
	size_t     &%QuadCur%,
	size_t     &%BfgsCur%,
	double        &%rCur%,
	double        &%fCur%,
	double        *%xCur%, // length n 
	double        *%sCur%, // length n
	double        *%gCur%, // length n 
	const double  *%HCur%  // length n * n
)%$$
$tend

$fend 20$$

$head Original Problem$$
Determine a value of $italic x$$ such that the 
$xref/glossary/Infinity Norm/infinity norm/$$ of the 
$xref/glossary/p: Scaled Projected Gradient/scaled project gradient/$$
for the following problem is less than or equal 
$xref/QuasiNewton01Box/delta/delta/$$:
$latex \[
\begin{array}{lrl}
{\rm minimize} & f(x) & \wrt \; x \in \R^n \\
\st            & 0 \leq x \leq 1
\end{array}
\] $$

$head Convention$$

$subhead Arguments$$
All of the arguments to $code QuasiNewton01Box$$
are inputs; i.e., their values must
be set before $code QuasiNewton01Box$$ is called.
In addition, the arguments with names that
end in $italic Cur$$ are both inputs and outputs; i.e.
their values must be set before $code QuasiNewton01Box$$ is called
and they have the same definition (but possibly different values)
when $code QuasiNewton01Box$$ returns.

$subhead Exceptions$$
This routine $code QuasiNewton01Box$$ is
$xref/glossary/Exception Safe/exception safe/$$.
There is one stipulations to this,
calls to $italic obj$$ may not be exception safe because
$italic obj$$ is under the 
control of the calling routine and hence the callers responsibility.
Furthermore, if an exception does occur,
the arguments that are both inputs and outputs are valid
and can be used to continue the optimization process.

$head ItrMax$$
The argument $italic ItrMax$$ specifies the maximum allowable value
for $italic ItrCur$$. Thus $italic ItrMax$$
minus $italic ItrCur$$ is the maximum number of
$code QuasiNewton01Box$$ iterations that will be preformed,
and the maximum number of times that $code QuasiNewton01Box$$ will
change its state
(in response to this call).
There is at most one evaluation of the gradient of $latex f(x)$$
and at most one Bfgs update of the approximate Hessian for each
iteration.
There may be multiple evaluations of $latex f(x)$$
for each iteration.

$head QuadMax$$
The argument $italic QuadMax$$ specifies the maximum number 
of interior point iterations to use when solving the Quadratic subproblems
corresponding to all the iterations.
The value $italic QuadMax$$ minus $italic QuadCur$$ is the maximum
number of interior point iterations that will be used by this call
to $code QuasiNewton01Box$$.

$head n$$
The argument $italic n$$ specifies the number of components
in the argument vector $italic x$$.

$head delta$$
The argument $italic delta$$ specifies the convergence criteria.
If the return value of $code QuasiNewton01Box$$ is equal to "ok",
the infinity norm of the 
$xref/glossary/p: Scaled Projected Gradient/scaled projected gradient/$$,
for the $xref/QuasiNewton01Box/Original Problem/original problem/$$,
at the output value of $latex xCur$$
is less than or equal $italic delta$$.

$head Fun obj$$
The template argument $italic Fun$$ is a special class defined for this objective function.
Any data that is special to this functions definition can be stored as private data in the class
and initialized when $italic obj$$ object is constructed.
It must have the following member functions:

$subhead Objective Function$$
The syntax
$syntax%
	const char * obj.function(const double *%x%, double *%f%)
%$$
evaluates the objective.
If the return value of $italic obj$$ is equal to "ok",
this sets the scalar $italic f$$
equal to the objective function at $italic x$$
where $italic x$$ is a vector of length $italic n$$ and
$latex 0 \leq x \leq 1$$.
This may be equal to any finite value or $xref/PlusInfinity/$$
(it cannot be not a number).
$pre

$$
If the return value is plus infinity,
the function is not assumed to be smooth 
between its current best point ($italic xCur$$)
and the requested new point ($italic xNext$$ or $italic xTmp$$).
The optimizer will (in some sense) try to take a smaller step
in its search for a new $italic x$$ near $italic xCur$$ where the objective
is lower than its current best estimate of the minimizer.
The gradient of the objective will not be evaluated at 
any point where the objective is plus infinity.
$pre

$$
If the return value of 
$syntax%%obj%.function%$$ 
is not equal to "ok",
$code QuasiNewton01Box$$ will abort its operation and return with 
its return value
equal to the value returned by 
$syntax%%obj%.function%$$.

$subhead Gradient$$
The objective function is alway evaluated at the same $italic x$$ value directly before
evaluating the gradient of the objective function.
In addition, the corresponding value of the objective will be finite; i.e., 
not equal to plus infinity.
The syntax
$syntax%
	const char * obj.gradient(double *%g%)
%$$
evaluates the gradient using the value of $italic x$$ in the previous 
call to $syntax%%obj%.function%$$.
If the return value of 
$syntax%%obj%.gradient%$$ 
is not equal to "ok",
$code QuasiNewton01Box$$ will abort its operation and return with 
its return value
equal to the value returned by 
$syntax%%obj%.gradient%$$ .

$head sOkCur$$
The value $italic sOkCur$$ is discussed
together with $xref/QuasiNewton01Box/sCur/sCur/$$.

$head ItrCur$$
On input and output,
$italic ItrCur$$ contains the number of $code QuasiNewton01Box$$ iterations
that have been completed so far.
This is also the number of times that the state of
$code QuasiNewton01Box$$ has changed.
The current state is defined by the value of 
$italic rCur$$, $italic xCur$$, and $italic HCur$$.
$pre

$$
If the input value of $italic xCur$$ satisfies the convergence criteria,
the output value of $italic ItrCur$$ is equal to its input value
(the state of $code QuasiNewton01Box$$ does not change)
and the return value of $code QuasiNewton01Box$$ is equal to "ok".
$pre

$$
If the input value of $italic xCur$$ 
does not satisfies the convergence criteria,
and the input value of $italic ItrCur$$ equal to $italic ItrMax$$,
$code QuasiNewton01Box$$ will not change its state.
It will however attempt to set the output value of $italic sCur$$ 
to a solution of the
$xref/QuasiNewton01Box/sCur/Quadratic Subproblem/quadratic subproblem/1/$$.
$pre

$$
If the return value of $code QuasiNewton01Box$$ is not equal to "ok" and
the output value of $italic ItrCur$$ is equal to its input value,
$code QuasiNewton01Box$$ will never make any progress from 
its current state.
$pre

$$

$head QuadCur$$
On input and output,
$italic QuadCur$$ contains the total number of iterations
used by the Quadratic sub-problem solver so far
(counting all iterations of $code QuasiNewton01Box$$).

$head BfgsCur$$
On input and output,
$italic BfgsCur$$ contains the total number of Bfgs updates
that were applied to obtain the current approximate Hessian $italic HCur$$.
Note that the update is only applied on a subset of the 
$code QuasiNewton01Box$$ iterations
during which the current $italic x$$ value changes.

$head fCur$$
On input and output, $italic fCur$$ 
contains the value $latex f(xCur)$$.
This cannot (on input) and will not (on output)
be equal to $xref/PlusInfinity/$$.

$head rCur$$
On input and output, $italic rCur$$ must be between
$code MinRadius$$ (1e-7) and $code MaxRadius$$ (.5).
It specifies the current trust region radius
as an infinity norm bound on the step size in the
$xref/QuasiNewton01Box/sCur/Quadratic Subproblem/quadratic subproblem/1/$$.
$pre

$$
The value $italic rCur$$ is adjusted to be larger when the solution of 
the quadratic approximation does well in predicting descent of the 
original problem.
It is adjusted to be smaller when the solution of the quadratic approximation
does very poorly at predicting descent of the original problem.
If you restart $code QuasiNewton01Box$$ using values output from 
a previous call except that you change $italic rCur$$ to what you think is a 
better value, 
this will invalidate the previous value of $italic sCur$$ 
and you should therefore set $italic sOkCur$$ to false.

$head xCur$$
The argument $italic xCur$$ is a vector of length $italic n$$.
On input, it 
specifies the point at which to start the optimization procedure.
On output, it is the closest to optimal point obtained so far.

$head sCur$$
The argument $italic sCur$$ is a vector of length $italic n$$.
If $italic sOkCur$$ is false,
the elements of $italic sCur$$ are unspecified.
Otherwise, $italic sCur$$ solves the quadratic subproblem defined below

$subhead Quadratic Subproblem$$
Determine a value of $italic s$$ such that the 
$xref/glossary/Infinity Norm/infinity norm/$$ of the 
$xref/glossary/p: Scaled Projected Gradient/scaled project gradient/$$
for the following problem is less than or equal 
$xref/QuasiNewton01Box/rCur/rCur/$$ times
$xref/QuasiNewton01Box/delta/delta/$$:
$latex \[
\begin{array}{lcl}
{\rm minimize} 
	& gCur^T * s + (1/2) * s^T * HCur * s & \wrt \; s \in \R^n 
\\
\st            
	& 0 \leq (xCur + s) \leq 1 \; {\rm and} \; -rCur \leq s \leq rCur
\end{array}
\] $$
where 
$italic xCur$$, 
$italic gCur$$, and 
$italic HCur$$, are the corresponding output values.
Note that if the value zero solves the problem above,
the infinity norm of the scaled projected gradient for the 
$xref/QuasiNewton01Box/Original Problem/original problem/$$
is less than or equal $italic delta$$.

$subhead Discussion$$
$list number$$
If $italic sOkCur$$ is true on input,
and $italic ItrCur$$ is less than $italic ItrMax$$,
the next iteration of $code QuasiNewton01Box$$ will either keep
$italic xCur$$ the same or change it by adding $italic sCur$$ to it.

$lnext
If $italic sOkCur$$ is false on output,
one of the following conditions must hold:
$list alpha$$
The quadratic subproblem could not be solved.
$lnext
On output, $italic ItrCur$$ is less that $italic ItrMax$$
and an error message,
other than the maximum number of iterations error message,
has been returned by $code QuasiNewton01Box$$.
$lend

$lnext
The value $italic sCur$$ is an accurate estimate of the 
true solution minus $italic xCur$$ under the following assumptions:
$list alpha$$
The value $italic delta$$ 
is less than half the infinity norm of the value
of the scaled projected gradient corresponding to the input values.
$lnext
The value $italic sOkCur$$ is true.
$lnext
The infinity norm of $italic sCur$$ is less than $italic rCur$$.
$lnext
The value $italic xCur$$ is close to the true solution.
$lnext
The Hessian of the objective at the true solution of the 
$xref/QuasiNewton01Box/Original Problem/original problem/$$ 
is positive definite.
$lnext
The value $italic HCur$$ is close to the Hessian at $italic xCur$$.
$lend
$lend

$head gCur$$
The vector $italic gCur$$ must have length $latex n$$.
On input and output,
it contains the gradient of $latex f$$
at the point $latex xCur$$.
To be specific
$latex \[
	g[ i ] = \frac{ \partial }{ \partial x_i } f( xCur )
\] $$

$head HCur$$
The vector $italic HCur$$ must have length $latex n^2$$.
On input and output,
it must contain a positive definite approximate for the 
Hessian of $latex f$$
at the point $latex xCur$$.
(On input, if you have no better ideas about this approximation, 
it is suggested that you use the identity matrix.)
To be specific
$latex \[
	H[ i \, \times \, n + j ] \approx 
		\frac{ \partial^2 }{ \partial x_i \partial x_j }
		f( xCur )
\] $$

$head os$$
The argument $italic os$$ specifies which output stream the
tracing should be written to (if any tracing is done). 

$head level$$
The argument $italic level$$ specifies the amount of tracing to do 
during the optimization procedure.
If $latex level = 0$$, no tracing is done; i.e., $code QuasiNewton01Box$$
does not write any output.
Otherwise, tracing is done for each iteration during which the state changes
($italic rCur$$, $italic xCur$$, or $italic HCur$$ changes).

$subhead level > 0$$
If $italic level$$ is greater than zero,
the final values corresponding to the final iteration are printed.
Using $italic level$$ less than zero is useful when the optimization
will be continued and the final iteration values will be the same
as the first iteration of the continuation.

$subhead abs(level) >= 1$$
$table
$bold label$$ $cnext $bold Description$$ 
$rnext
$code k$$ $cnext
	The current iteration number
$rnext
$code r$$ $cnext
	The current value of the trust region radius; i.e., $latex r^k$$.
$rnext
$code f$$ $cnext
	The objective function value at the current iterate; i.e.,
	$latex f( x^k )$$.
$rnext
$code |p|$$ $cnext
	The infinity norm of the scaled projected gradient corresponding to 
	the current iterate; i.e., the infinity norm of $latex p( x^k) $$
	where $latex p$$ is the scaled projected gradient function.
$rnext
$code |s|$$ $cnext
	The infinity norm of $latex s^k$$ where $latex s^k$$ solves the problem
$latex \[
\begin{array}{lcl}
{\rm minimize} 
	& g( x^k )^T s + s^T H^k s / 2 & \wrt \; s \in \R^n 
\\
\st            
	& 0 \leq (x^k + s) \leq 1 \; {\rm and} \; -r^k \leq s \leq r^k
\end{array}
\] $$

$rnext
$code |H|$$ $cnext
	The infinity norm of Hessian of the quadratic approximation
	for the function $italic f$$; i.e., the infinity norm of $latex  H^k $$
$rnext
$code det(H)$$ $cnext
	The determinant of the Hessian of the quadratic approximation
	for the function $italic f$$; i.e., the determinant of $latex H^k $$.
$rnext
$code Bfgs$$  $cnext
	The current value of $italic BfgsCur$$.
$rnext
$code df$$ $cnext
	The following difference of objective function values 
	$latex f( x^k + s^k ) - f( x^k )$$ 
$rnext
$code dq$$ $cnext
	The current quadratic approximation for
	$latex f( x^k + s^k ) - f( x^k )$$; i.e.,
	$latex g( x^k ) s^k + (s^k)^T H^k s^k /2$$.
$rnext
$code eta$$ $cnext
	The current line search parameter $latex \eta^k$$. 
	The next iterate is given by the formula 
	$latex x^{k+1} = x^k + \eta^k s^k$$.
$rnext
$code |Herr|$$ $cnext
	The maximum absolute difference between the change in the gradient between 
	iterates and the prediction of change by $latex H^k$$; i.e.,
	the infinity norm of
	$latex g( x^{k+1} ) - g( x^k ) - H^k ( x^{k+1} - x^k )$$.
$rnext
$tend

$subhead abs(level) >= 2$$
The information is printed above the $syntax%%level% >= 1%$$ information.
$table
$bold label$$ $cnext $bold Description$$ 
$rnext
$code i$$    $cnext
	The component of the vectors being printed
$rnext
$code x$$     $cnext
	The current argument value $italic x^k$$; i.e., the value
	of $italic xCur$$ at the beginning of the iteration.
$rnext
$code s$$     $cnext
	The current step value $latex s^k$$.
$rnext
$code g$$    $cnext
	The current gradient $latex g( x^k )$$
$rnext
$code p$$     $cnext
	The current scaled projected gradient $latex p( x^k )$$.
$rnext
$tend

$subhead abs(level) >= 3$$
The information is printed above the $syntax%%level% >= 2%$$ information.
$table
$bold label$$ $cnext $bold Description$$ $rnext
$rnext
$code H$$     $cnext
	The approximation for the Hessian $latex H^k$$.
$tend

$subhead abs(level) >= 4$$
$table
$bold label$$ $cnext $bold Description$$ $rnext
$rnext
$code i$$     $cnext
	The component of $italic x$$ corresponding to the gradient check
$rnext
$code fp$$ $cnext
	The forward function value used in difference 
	approximation for gradient
$rnext
$code fm$$ $cnext
	The backward function value used in difference approximation for 
	gradient
$rnext
$code a$$ $cnext
	The finite difference approximation for the gradient
$rnext
$code g$$ $cnext
	The value of the gradient returned by $italic obj$$ for this component
$tend


$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$code
$verbatim%QuasiNewton01Box.cpp%0%// BEGIN PROGRAM%// END PROGRAM%$$
$$

$contents%
	License.omh%
	Memory.h%
	MaxAbs.cpp%
	Residual.cpp%
	Delta.cpp%
	Next.cpp%
	QuadBox.cpp%
	Bfgs.cpp%
	CppADUtil.h%
	PlusInfinity.h%
	glossary.omh
%$$

$end

$end
*/

# include <algorithm>

# include "QuadBox.h"
# include "MaxAbs.h"
# include "Bfgs.h"
# include "Memory.h"
# include "PlusInfinity.h"
# include <iostream> 

# include "CppADUtil.h"

namespace {
	void ScaleProjectGradient(
		size_t n, 
		const double *x, 
		const double *g, 
		double *p)
	{	while(n)
		{	--n;
			if( g[n] >= 0. )
				p[n] = (x[n] - 0.) * g[n];
			else 	p[n] = (1. - x[n]) * g[n];
		}
	}

	template <class Fun>
	const char *EvaluateFunction(std::ostream &os, const char *infinityMsg,
		Fun &obj, int level, const double *x, double  &f)
	{	const char *msg = 0;
		msg = obj.function(x, f);
		if( f == PlusInfinity(double(0)) )
			return infinityMsg;
		return msg;
	}

	template <class Fun>
	const char *EvaluateGradient(std::ostream &os,
		Fun &obj, int level, double *g)
	{	const char *msg = 0;
		msg = obj.gradient(g);
		return msg;
	}
	const char *DetermineStep(
		std::ostream &os,
		size_t        n,
		int           level,
		size_t        QuadMax,
		size_t        QuadLevel,
		double        delta,
		double        rCur,
		const double *xCur,
		const double *gCur,
		const double *gProj,
		const double *HCur,
		double       *aTmp,
		double       *bTmp,
		double       *sLow,
		double       *sUp,
		double       *sCur,
		size_t       &QuadCur)
	{	size_t i;
	
		// small relative to one maximum descent ratio for QuadBox 
		double epsilonQuadBox = 1e-10;
	
		// norm of the scaled projected gradient
		double gProjNorm = MaxAbs(n, gProj);
	
		// quadratic subproblem setup
		for(i = 0; i < n; i++)
		{	// initialize to same order as g	
			aTmp[i] = gProjNorm;
			bTmp[i] = gProjNorm;
	
			// do not overshoot lower limit
			if( xCur[i] - rCur < 0. )
				sLow[i] = -xCur[i];
			else	sLow[i] = -rCur;
	
			// do not overshoot upper limit
			if( xCur[i] + rCur > 1. )
				sUp[i] = 1. - xCur[i];
			else	sUp[i] = rCur;
	
			// initialize solution as center of box
			sCur[i] = .5 * (sLow[i] + sUp[i]);
		}
	
		// solve trust region sub-problem
		double deltaScaled = rCur * delta;
		if( deltaScaled < epsilonQuadBox * gProjNorm )
			deltaScaled =  epsilonQuadBox * gProjNorm;
		const char *msg = QuadBox(
			os, 
			QuadMax, 
			QuadLevel, 
			n, 
			HCur, 
			gCur, 
			sLow, 
			sUp, 
			QuadCur, 
			deltaScaled, 
			sCur, 
			aTmp, 
			bTmp 
		); 
		// check for case where cannot achieve accuracy
		if( deltaScaled > std::max( rCur * delta, 1e-3 * gProjNorm) ) 
		msg = "QuasiNewton01Box: cannot achieve requested accuracy";
		return msg;
	}
}

template <class Fun>
const char * QuasiNewton01Box(
	// Input Arguments
	std::ostream    &os,
	int           level,
	size_t       ItrMax,
	size_t      QuadMax,
	size_t            n,
	double        delta,
	Fun            &obj,
	// Input+Output Arguments
	bool        &sOkCur,
	size_t      &ItrCur,
	size_t     &QuadCur,
	size_t     &BfgsCur,
	double        &rCur,
	double        &fCur,
	double        *xCur,   // length n 
	double        *sCur,   // length n
	double        *gCur,   // length n 
	double        *HCur )  // length n * n
{
	// -------------------------------------------------------------

	// finite difference step size for finite difference gradient approx
	double StepSize     = 1e-5;

    	// maximum trust region radius
	const double MaxRadius    = .5;

      	// minimum trust region radius
	const double MinRadius    = 1e-7;

	// minimum fractional reduction to accept xNext on
	const double AcceptRatio = .01;   

    	// fractional reduction to shrink trust region on
	const double ShrinkRatio  = .2;

	// fractional reduction to expand trust region on
	const double ExpandRatio  = .95;

	// level of tracing during the quadratic subproblem
	const size_t QuadLevel = 0;

	// maximum increase in objective relative to linear approximation
	// over entire [0,1] box for which a Bfgs update will be applied
	const double maximumBfgs = 1e2;

	// small (relative to one) value for aborting Bfgs calculation
	// in addition, 1. / epsilonBfgs is a condition number bound for H
	const double epsilonBfgs = 1e-11;

	// -------------------------------------------------------------
	// function value corresponding to xCur and xNext
	double fNext;

	// descent in the approximation and actual objective
	double dq;
	double df;

	// message returned from obj
	const char * msg;


	// temporary indices
	size_t i;
	size_t j;

	// local memory
	size_t d_size = 8 * n;
	Memory<double> dMemory(d_size);
	double *xNext  = dMemory(n);
	double *aTmp   = dMemory(n);
	double *bTmp   = dMemory(n);
	double *gNext  = dMemory(n);
	double *gProj  = dMemory(n);
	double *sLow   = dMemory(n);
	double *sUp    = dMemory(n);
	double *xTmp   = dMemory(n);

	CppADvector<double> HTmp;
	CppADvector<double> Result;
	CppADvector<double> Rhs;
	if( abs(level) > 0 )
		HTmp.resize(n * n);

	// initial trust region radius
	rCur = std::min(rCur, MaxRadius);
	rCur = std::max(rCur, MinRadius);

	// check initial x value
	for(i = 0; i < n; i++)
		assert( (0. <= xCur[i]) & (xCur[i] <= 1.) );

	if( abs(level) >= 4 )
	{	// numerical check of the gradient evaluation
		for(i = 0; i < n; i++)
		{	double fp;
			double fm;
			double xi;
			double xp;
			double  a;

			xi       = xCur[i];
			xCur[i]  = xi + StepSize;
			xCur[i]  = xp = std::min(1., xCur[i] );
			msg = "objective is +infinity at input value of xCur";
			msg = EvaluateFunction(os, msg, obj, level, xCur, fp);
			if( strcmp(msg, "ok") != 0 )
				return msg;

			xCur[i]  = xi - StepSize;
			xCur[i]  = std::max(0., xCur[i] );
			msg = "objective is +infinity near input value of xCur";
			msg = EvaluateFunction(os, msg, obj, level, xCur, fm);
			if( strcmp(msg, "ok") != 0 )
				return msg;

			a   = (fp - fm) / (xp - xCur[i]); 

			os << "i = "  << i     << ", ";
			os << "fp = " << fp    << ", ";
			os << "fm = " << fm    << ", ";
			os << "a = "  << a     << ", ";
			os << "g = "  << gCur[i]  << std::endl;

			xCur[i] = xi;
		}
	}

	// initialize next coordinate direction used to stabalize Hessian 
	size_t index = ItrCur % n;

	// initialize error in previous Bfgs update
	double Herr = 0.;
	
	while( true )
	{	// check that input value of fCur is not + infinity
		assert( fCur != PlusInfinity(double(0)) );

		// the scaled projected gradient 
		ScaleProjectGradient(n, xCur, gCur, gProj);
		double gNormProj = MaxAbs(n, gProj);

		// solve for the next step
		if( ! sOkCur )
		{	// check if zero a good enough solution
			sOkCur = gNormProj < delta;
			if( sOkCur )
				for(i = 0; i < n; i++)
					sCur[i] = 0.;
		}
		if( ! sOkCur )
		{	msg = DetermineStep(
				os,
				n,
				level,
				QuadMax,
				QuadLevel,
				delta,
				rCur,
				xCur,
				gCur,
				gProj,
				HCur,
				aTmp,
				bTmp,
				sLow,
				sUp,
				sCur,
				QuadCur
			);
			sOkCur = strcmp(msg, "ok") == 0;
			if( ! sOkCur )
				return msg;
		}
		// do not print final trace if level <= 0
		bool done = MaxAbs(n, gProj) <= delta;
		if( done && level <= 0)
			return (const char *)("ok");
		done |= ItrCur >= ItrMax;
		if( done && level <= 0)
		{	msg = "ItrMax reached without convergence";
			return msg;
		}
		if( abs(level) >= 1 )
		{	size_t m       = 0;
			double logabsdet;
			for(i = 0; i < n*n; i++)
				HTmp[i] = HCur[i];
			CppAD::LuSolve(n, m, HTmp, Result, Rhs, logabsdet);

			os << "k = "    << ItrCur << std::endl; 
			os << "r = "    << rCur              << ", ";
			os << "f = "    << fCur              << ", ";
			os << "|s| = "  << MaxAbs(n, sCur)   << ", ";
			os << "|p| = "  << MaxAbs(n, gProj)  << ", ";
			os << " |H| = " << MaxAbs(n*n, HCur) << ", ";
			os << "det(H) = " << std::exp(logabsdet) << ", ";
			os << "Bfgs = " << BfgsCur;
			os << std::endl;
		}
		if( abs(level) >= 2 )
		{	for(i = 0; i < n; i++)
			{	os << "i = " << i        << ", ";
				os << "x = " << xCur[i]  << ", ";
				os << "s = " << sCur[i]  << ", ";
				os << "g = " << gCur[i]  << ", ";
				os << "p = " << gProj[i];
				os << std::endl;
			}
		}
		if( abs(level) >= 3 )
		{	for(i = 0; i < n; i++)
			{	os << "HCur(" << i << ",:) = ";
				for(j = 0; j < n; j++)
				{	os << HCur[i * n + j];
					if( j < n-1 )
						os << ", ";
					else	os << std::endl;
				}
			}
		}
		// done printing final trach for these cases
		if( MaxAbs(n, gProj) <= delta )
		{	assert( done );
			return (const char *)("ok");
		}
		if( ItrCur >= ItrMax )
		{	assert( done );
			msg = "ItrMax reached without convergence";
			return msg;
		}
		if( ItrCur >= ItrMax && MaxAbs(n, gProj) <= delta )
			return (const char *)("ok");
		if( ItrCur >= ItrMax )
		{	msg = "ItrMax reached without convergence";
			return msg;
		}

		double eta   = 1.;
		fNext = PlusInfinity(double(0));
		while( fNext == PlusInfinity(double(0)) && eta > MinRadius )
		{
			// corresponding value of x
			for(i = 0; i < n; i++)
			{	xNext[i] = xCur[i] + eta * sCur[i];

				// make sure do not go outside of box
				if( xNext[i] < 0. )
					xNext[i] = 0.;
				if( xNext[i] > 1. )
					xNext[i] = 1.;
			}

			// corresponding value of the objective function
			msg = "ok";
			msg = EvaluateFunction(os, msg, obj, level, xNext, fNext);
			if( strcmp(msg, "ok") != 0 )
				return msg;

			// check for value of + infinity
			if( fNext == PlusInfinity(double(0)) && eta > MinRadius )
				eta /= 2.;
		}
		msg = "objective is +infinity near current xCur";
		if( fNext == PlusInfinity(double(0)) )
			return msg;

		// descent in objective and in the quadratic approximation
		df = fNext - fCur;
		dq = 0.;
		for(i = 0; i < n; i++)
		{	dq += gCur[i] * sCur[i];
			for(j = 0; j < n; j++) dq += 
				sCur[i] * HCur[i * n + j] * sCur[j] / 2.;
		}

		// Line Search: extend this direction as long as good descent
		if( dq > 0. || df > eta * AcceptRatio * dq )
			eta = 0.;
		double sNorm  = MaxAbs(n, sCur);
		bool extend   = eta == 1.;
		while( extend )
		{	for(i = 0; i < n; i++)
			{	xTmp[i] = xCur[i] + 2. * eta * sCur[i];
				// make sure do not go outside of box
				if( xTmp[i] < 0. )
					xTmp[i] = 0.;
				if( xTmp[i] > 1. )
					xTmp[i] = 1.;
			}
			// corresponding value of objective 
			double fTmp;
			msg = "ok";
			msg = EvaluateFunction(os, msg, obj, level, xTmp, fTmp);
			if( strcmp(msg, "ok") != 0 )
				return msg;

			extend = (fNext - fTmp) > AcceptRatio*gNormProj*sNorm*eta;
			if( extend )
			{	assert( fTmp != PlusInfinity(double(0)) );
				eta    = 2. * eta;
				fNext  = fTmp;
				for(i = 0; i < n; i++)
					xNext[i] = xTmp[i];
			}
		}
		assert( fNext != PlusInfinity(double(0)) );

		// must revaluate objective to evaluate gradient at xNext 
		// (should use save and restore member functions of objective instead)
		msg = "objective is +infinity (but previously was different for same x)";
		msg = EvaluateFunction(os, msg, obj, level, xNext, fNext);
		if( strcmp(msg, "ok") != 0 )
			return msg;

		assert( fNext != PlusInfinity(double(0)) );
		msg = EvaluateGradient(os, obj, level, gNext);
		if( strcmp(msg, "ok") != 0 )
			return msg;

		// compute the gradient prediction error
		Herr = 0.;
		for(i = 0; i < n; i++)
		{	double sum = 0.;
			for(j = 0; j < n; j++)
				sum += HCur[i*n+j]*(xNext[j]-xCur[j]);
			sum = gNext[i] - gCur[i] - sum;
			Herr = std::max(fabs(sum), Herr);
		}

		// finish up level 1 tracing
		if( abs(level) >= 1 )
		{	os << "dq = "  << dq << ", ";
				os << "df = "  << df << ", ";
				os << "eta = "  << eta << ", ";
			os << "Herr = " << Herr;
			os << std::endl;
		}

		// flag indicating a change of state
		bool change = false;

		// update the approximation for the Hessian
		if( df < maximumBfgs * gNormProj )
		{	msg = Bfgs(n, epsilonBfgs, xCur, gCur, xNext, gNext, 
			           HCur);
			if( strcmp(msg, "ok") == 0 )
			{	change = true;
				BfgsCur++;
			}
		}
		if( dq < 0. && df < ExpandRatio * dq && rCur < MaxRadius )
		{	change = true;
			rCur = std::min( 2. * rCur, MaxRadius);
		}
		if( (dq >= 0. || df >= ShrinkRatio * dq) && rCur > MinRadius )
		{	change = true;
			rCur   = std::max( rCur / 5., MinRadius);
		}
		if( eta > 0. )
		{	change = true;

			// transfer the next point to current point
			fCur = fNext;
			for(i = 0; i < n; i++)
			{	xCur[i] = xNext[i];
				gCur[i] = gNext[i];
			}
		}

		// update the approximation for the Hessian
		for(i = 0; i < n; i++)
			xNext[i] = xCur[i];
		xNext[index] = xCur[index] + StepSize;
		if( xNext[index] > 1. )
			xNext[index] = xCur[index] - StepSize;

		msg = "objective is +infinity near xCur";
		msg = EvaluateFunction(os, msg, obj, level, xNext, fNext);
		if( strcmp(msg, "ok") != 0 )
			return msg;

		assert( fNext != PlusInfinity(double(0)) );
		msg = EvaluateGradient(os, obj, level, gNext);
		if( strcmp(msg, "ok") != 0 )
			return msg;

		msg = Bfgs(n, epsilonBfgs, xCur, gCur, xNext, gNext, HCur);
		if( strcmp(msg, "ok") == 0 )
		{	change = true;
			BfgsCur++;
		}
		index = (index + 1) % n;

		// check for no change in the state
		if( ! change )
		{	msg = "No progress during last iteration";
			return msg;
		}

		// increment change of state counter
		ItrCur++;

		// invalidate sCur because the sub-problem is going to change
		sOkCur = false;
	}
}

# endif
