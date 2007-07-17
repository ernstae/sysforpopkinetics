/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
/*************************************************************************
 *
 * File: firstOrderOpt.cpp
 *
 *
 * Optimizes the parametric population objective functions using first order
 * approximation.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: firstOrderOpt
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*

$begin FirstOrderOpt$$
$latex \newcommand{\B}[1]{{\bf #1}}$$
$latex \newcommand{\R}[1]{{\rm #1}}$$

$spell
        throwExcepIfMaxIter
        struct
        Model model
	cbc
	cerr
	const
	cout
	covariance
	Covariances
	cmath
	dmatInd
	drow
	dvec
	endl
	Fp
	ind
	Inv
	iomanip
	iostream
	Iter
	namespace
	Obj
	optimizer
	paramatric
	pd
	pdalp
	pmat
	pmatInd
	pdrow
	pvec
	Spk
	sqrt
	std
	stdout
	subvector
	var
	Varbl
	Vi
	bool
	Ri
	ind
	valarray
$$

$section Optimizing First Order Approximation For Population Objective$$

$center
$italic
$include shortCopyright.txt$$
$$
$$

$index firstOrderOpt$$
$index first, order approximation$$
$index approximation, first order$$
$index order, first approximation$$
$index population, fitting$$

$head Prototype$$
$syntax/void firstOrderOpt(
              SpkModel<double>&       /model/                      ,
              SpkModel< AD<double> >& /adModel/                    ,
              const DoubleMatrix&     /dvecN/                      ,
              const DoubleMatrix&     /dvecY/                      ,
              Optimizer&              /alpOptInfo/                 ,
              const DoubleMatrix&     /dvecAlpLow/                 ,
              const DoubleMatrix&     /dvecAlpUp/                  ,
              const DoubleMatrix&     /dvecAlpIn/                  ,
              DoubleMatrix*           /pvecAlpOut/                 ,
              const DoubleMatrix&     /dvecAlpStep/                ,
              Optimizer&              /bOptInfo/                   ,
              const DoubleMatrix&     /dvecBLow/                   ,
              const DoubleMatrix&     /dvecBUp/                    ,
              const DoubleMatrix&     /dmatBIn/                    ,
              DoubleMatrix*           /pmatBOut/                   ,
              const DoubleMatrix&     /dvecBStep/                  ,
              double*                 /pdLTildeOut/                ,
              DoubleMatrix*           /pdrowLTilde_alpOut/         ,
              DoubleMatrix*           /pmatLTilde_alp_alpOut/
)/$$


$head Purpose$$
Minimizes the likelihood corresponding to the first order approximation
for the parametric population model; to be specific, the
$cref/population notation/glossary/Population Notation/$$ 
functions $latex f_i ( \alpha , b )$$ and $latex R_i ( \alpha , b )$$ 
are approximated as follows:
$latex \[
\begin{array}{rcl}
\tilde{f}_i ( \alpha , b) & = & 
	f_i ( \alpha , 0) + \partial_b f_i  ( \alpha , 0) * b
\\
\tilde{R}_i ( \alpha , b ) & = & R_i ( \alpha , 0 )
\end{array}
\] $$
This simplified model is used to obtain the estimate 
$latex \hat{\alpha}$$ for the population parameters.
If estimates for the individual parameters are requested,
$latex \hat{b}_i$$ is
computed by maximizing, with respect to $latex b$$,
the likelihood corresponding to the original model functions;
i.e., $latex f_i ( \hat{\alpha} , b )$$ and 
$latex R_i ( \hat{\alpha} , b )$$.

$head Notation$$
$table
$latex b \in \B{R}^n$$ $cnext a value for the random effects for one subject
$rnext
$latex \alpha \in \B{R}^m$$ $cnext a for the fixed effects
$rnext
$latex M$$ $cnext number of subjects in the data set
$tend 

$head Exceptions$$
If an error is detected or failure occurs during the evaluation, 
an SpkException object is thrown.  
The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head SpkModel$$
The following $code SpkModel$$ member functions are used by 
$italic model$$ and $italic adModel$$:
$cref/SpkModel_selectIndividual/$$,
$cref/SpkModel_setIndPar/$$,
$cref/SpkModel_setPopPar/$$,
$cref/SpkModel_dataMean/$$,
$cref/SpkModel_dataVariance/$$,
$cref/SpkModel_indParVariance/$$.
The other member functions of these objects are not used.

$head Optimizer$$
These $cref/Optimizer/$$ objects 
$italic alpOptInfo$$ and $italic bOptInfo$$ contain the information 
that controls the fixed effects ($latex \alpha$$) 
and random effects ($latex b$$) optimization respectively.
Most of the optimizer information is accessible directly via public
get functions, e.g., the value epsilon is returned by the Optimizer 
class function $cref/getEpsilon/$$.
The following subsections specify how this function uses 
some of the elements of the Optimizer object that are accessed 
directly using get functions.
$pre

$$
In the discussion below, $italic optInfo$$ is 
$italic alpOptInfo$$ and $italic bOptInfo$$ for the fixed effects
and random effects optimization respectively.

$subhead epsilon$$
The input value
$syntax%
	double %epsilon% = %optInfo%.getEpsilon()
%$$
is used to specify the convergence criteria
for the optimizer.
It must be greater than zero.
A population parameter value $italic alpOut$$ 
is accepted as an estimate for $italic alpHat$$ 
(a the local minimizer of the first order objective)
if
$syntax%
        all( abs( %alpOut% - %alpHat% ) <= %epsilon% * ( %alpUp% - %alpLow% ) )
%$$
where $code abs$$ and $code <=$$ are defined element-by-element 
and $code all$$ is true if an only if every element in its argument is true.
Since $italic alpHat$$ is unknown, this function estimates the left hand
side of this inequality in a way that is a good approximation when 
the Hessian of the objective function is positive definite.

$subhead nMaxIter$$
The input value
$syntax%
	  int %nMaxIter% = %optInfo%.getNMaxIter();
%$$
must be greater than or equal to zero.
It specifies the maximum number of 
iterations to attempt before giving up on convergence.
If it is equal to zero, then the initial
value $italic alpIn$$ is used for the final value $italic alpOut$$, 
and any other requested output values are evaluated at that final value.

$subhead traceLevel$$
The input value
$syntax%
	int %level% = %optInfo%.getLevel();
%$$
specifies the amount of tracing.
Larger values of $italic level$$ result in more tracing.
Traced values are printed to standard output.
$pre

$$
Tracing is done using a scaled version of the
argument space function.  
For this scaled version the elements of
the parameter vector are constrained to the interval [0, 1]. 
$pre

$$
If $italic level$$ is greater than or equal to 4, then the tracing 
will include the gradient of the objective and a finite difference 
approximation for that gradient.
These two gradients can be compared as a check on the consistency 
of the objective function and its gradient.
$pre

$$
For more details on the tracing see the description of the level 
parameter for the optimizer $code QuasiNewton01Box$$.

$subhead isWarmStart$$
The input and output value
$syntax%
	 bool isWarmStart = %optInfo%.getIsWarmStart();
%$$
indicates whether it is possible to perform a warm start 
using the current optimizer state information.
If value is true, all of the $italic optInfo$$ fields mentioned in
$cref/setStateInfo/$$ must have been set.

$subhead nIterCompleted$$
The input and output value
$syntax%
	 int nIterCompleted = %optInfo%.getNIterCompleted();
%$$
is the number of iteration that have been 
completed by the optimizer.

$subhead isTooManyIter$$
The output value
$syntax%
	 bool isTooManyIter = %optInfo%.getIsTooManyIter();
%$$
indicates whether the too-many-iteration failure has occurred.
If $italic isTooManyIter$$ is true,
the $cref/epsilon/FirstOrderOpt/Optimizer/epsilon/$$
convergence criteria could not be satisfied 
with the allowable number of iteration
(so the best value so far is returned as $italic alpOut$$).

$subhead saveStateAtEndOfOpt$$
The input value
$syntax%
	 bool saveStateAtEndOfOpt = %optInfo%.getSaveStateAtEndOfOpt();
%$$
This $code const$$ flag indicates if the state information required 
for a warm start should be saved at the end of the optimization process
(just before $code firstOrderOpt$$ returns).
This information is can be retrieved using the 
$cref/getStateInfo/$$ function.
If an exception is thrown,
$code firstOrderOpt$$ does not return
and this warm state information is not saved.
$pre

$$
In the case where $italic optInfo$$ is $italic bOptInfo$$, 
$italic saveStateAtEndOfOpt$$ must be false.

$subhead optInfo.throwExcepIfMaxIter$$
The input value
$syntax%
	 bool throwExcepIfMaxIter = %optInfo%.getThrowExcepIfMaxIter();
%$$
indicates if the optimizer should throw an exception when
the maximum number of iterations is exhausted.
If this parameter is true, then when
the maximum number of iterations is exhausted, an exception will
be thrown and the output values for this function will not be set.
Otherwise, the calling program will
need to check 
$cref/isTooManyIter/FirstOrderOpt/Optimizer/isTooManyIter/$$
to see if the maximum number of iterations was exhausted.

$head Return$$
If this optimizer returns, it sets all of its output values.
If the output value 
$syntax%
	bool %isTooManyIter% = %optInfo%.isTooManyIter()
%$$ 
is true,
convergence was not achieved.

$head dvecN$$
This is a column vector containing $latex N$$ where 
$latex N_i$$ is the number of measurements corresponding to the
$th i$$ individual.
The length of $italic dvecN$$ specifies the number of 
individuals in the population study; i.e., $latex M$$.

$head dvecY$$
This is a column vector containing the data set for the entire population
(referred to as $latex y$$ below).
We define $latex s(0) = 0$$ and
for $latex i = 1 , \ldots , M$$ we define 
$latex \[
    s(i) = N_0 + N_1 + \cdots + N_i
\] $$
where $latex M$$ is the length of $italic dvecN$$.
The vector $italic dvecY$$ has length $latex s(M)$$ and
for $latex i = 0 , \ldots , M-1$$,
the data vector corresponding to the $th i$$ individual 
$latex y_i \in \B{R}^{N(i)}$$ is defined by
$syntax%
	%y%[%i%] = transpose( %
		dvecY% [%s%(%i%)] , %
			dvecY%[%s%(%i%)+1] , % . . . %, %
			dvecY% [%s%(%i%)+%N%[%i%]-1] )
%$$ 

$head dvecAlpLow$$
This is a column vector of length $latex m$$
that specifies the lower limit for 
$latex \alpha$$ during the optimization procedure.

$head dvecAlpUp$$
This is a column vector of length $latex m$$ that specifies the upper limit for 
$latex \alpha$$ during the optimization procedure.

$head dvecAlpIn$$
This is a column vector of length $latex m$$ 
that specifies the initial value for 
$latex \alpha$$ during the optimization procedure.

$head pvecAlpOut$$
If the pointer $italic pvecAlpOut$$ is $code NULL$$, 
$code firstOrderOpt$$ will not return the optimal fixed effects values.
Otherwise, the input value of $syntax%*%pvecAlpOut%$$ does not matter.
If $code firstOrderOpt$$ returns (does not throw an exception)
then the output value of 
$syntax%*%pvecAlpOut%$$ is a column vector 
of length $latex m$$ containing the 
estimate for the minimizer of the population objective function.

$head dvecAlpStep$$
This is a column vector of length $latex m$$
that specifies the step size used for approximating
the derivatives with respect to the fixed effects ($latex \alpha$$).
The value of this parameter does not matter if
$italic pmatLTilde_alp_alpOut$$ is $code NULL$$.

$head dvecBLow$$
This is a column vector of length $latex n$$ 
that specifies the lower limit for 
$latex b$$ during the optimization procedure.

$head dvecBUp$$
This is a column vector of length $latex n$$
that specifies the upper limit for 
$latex b$$ during the optimization procedure.

$head dmatBIn$$
This is a matrix with dimensions $latex n \times M$$ 
that specifies the initial values for 
$latex b$$ during the optimization procedure.
Each individual can have a different initial value for $latex b$$
and the $th i$$ column of $italic dvecBIn$$ specifies the initial
value for individual $latex i$$ for $latex i = 0 , \ldots , M-1$$.

$head pmatBOut$$
If the pointer $italic pmatBOut$$ is $code NULL$$, 
$code firstOrderOpt$$ will not return the optimal random effects values.
Otherwise, the input value of $syntax%*%pmatBOut%$$ does not matter.
Its output value is a matrix with dimensions $latex n \times M$$.
The $th i$$ column of $italic dvecBOut$$ contains the optimal
random effects value for individual 
$latex i$$ for $latex i = 0 , \ldots , M-1$$.
To be specific, the $th i$$ column 
is the minimizer of $cref/Lambda(alpha, b)/Lambda/$$ 
with respect to $latex b$$ where $latex \alpha$$ corresponds
to the optimal value for the population parameters
and $latex \Lambda$$ corresponds to the $th i$$ individual.

$head dvecBStep$$
This column vector has length $latex n$$ and
specifies the step size used for approximating
the derivatives with respect to the random effects.

$head pdLTildeOut$$
If $italic pdLTildeOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then $syntax%*%pdLTildeOut%$$ is set equal to $latex \tilde{L} ( \alpha )$$
where $latex \alpha$$ is the optimal value for the fixed effects
and $latex \tilde{L}$$ is the likelihood corresponding to the FO approximation
described under $cref/Purpose/FirstOrderOpt/Purpose/$$. 

$head pdrowLTilde_alpOut$$
If $italic pdrowLTilde_alpOut$$ is not $code NULL$$, 
then $syntax%*%pdrowLTilde_alpOut%$$ 
is a row vector of length $latex m$$.
If this function completed the optimization successfully, 
this row vector will contain 
$latex \[
	\partial_\alpha \tilde{L} ( \alpha )
\] $$
where $latex \alpha$$ is the optimal value for the fixed effects.

$head pdrowLTilde_alp_alpOut$$
If $italic pdrowLTilde_alp_alpOut$$ is not $code NULL$$, 
then $syntax%*%pdrowLTilde_alp_alpOut%$$ 
is an $latex m \times m$$ matrix.
If this function completed the optimization successfully, 
this matrix will contain 
$latex \[
	\partial_\alpha \partial_\alpha \tilde{L} ( \alpha )
\] $$
where $latex \alpha$$ is the optimal value for the fixed effects.
The approximation for this second derivative is formed using central
differences of $latex \partial_\alpha \tilde{L} ( \alpha )%$$ with
step sizes specified by $italic dvecAlpStep$$.

$syntax/

/dmatLambdaTilde_alpOut/
/$$

If $italic dmatLambdaTilde_alpOut$$ is not $code NULL$$, then the
$code DoubleMatrix$$ pointed to by $italic dmatLambdaTilde_alpOut$$
must be declared in the function that calls this function, and its
number of columns must be equal to the number of individuals and its
number of rows must be equal to the length of the population parameter
vector $math%pop%$$.
If $italic dmatLambdaTilde_alpOut$$ is not $code NULL$$, and if this
function completed the optimization successfully, then the $code
DoubleMatrix$$ pointed to by $italic dmatLambdaTilde_alpOut$$ will
contain the derivatives of this individuals' contributions to
the population objective function.
Each column of the matrix contains the transpose of the derivative
 for a single individual.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic dmatLambdaTilde_alpOut$$.

$children%
	firstOrderOptTest.cpp
%$$
$head Example$$
The file $cref/firstOrderOptTest.cpp/$$ is an example and test
for $code firstOrderOpt$$.

$end
*/

# include <valarray>
# include "quasiNewtonAnyBox.h"
# include "SpkModel.h"
# include "DoubleMatrix.h"
# include "Optimizer.h"
# include "cppad/cppad.hpp"
# include "mapOpt.h"


# define SPK_PROGRAMMER_ERROR(msg)                \
	throw SpkException(                       \
		SpkError::SPK_PROGRAMMER_ERR,     \
		msg,                              \
		__LINE__,                         \
		__FILE__                          \
	);

class FirstOrderObj : public QuasiNewtonAnyBoxObj
{
private:
	// parameters set by constructor
	SpkModel<double>*                 _model;
	SpkModel< CppAD::AD<double> >*    _adModel;
	const int                         _M;
	const int                         _m;
	const int                         _n;
	const double*                     _N;
	const double*                     _Y;
	const double*                     _bStep;
	// most recent value for alpha
	std::valarray<double>             _alpha_valarray;

	// private version of objective function used with double or AD<double>
	// to evaluate the objective for one individual
	template <class Scalar, class Model>
	void function(int ind, 
	const std::valarray<Scalar> &alpha, Scalar *obj, Model *model) const
	{	typedef std::valarray<Scalar> Vector;
		int Ni = int( _N[ind] );
		int i, j, k, ell;
		Vector b(_n);
		Vector f0(Ni);
		Vector fp(Ni);
		Vector fm(Ni);
		Vector f_b(Ni * _n);
		Vector R(Ni * Ni);
		Vector D(_n * _n);
		Vector r(Ni);
		Vector Rinv_r(Ni);
		for(j = 0; j < _n; j++)
			b[j] = Scalar(0);
		// evaluate the model at (alpha, 0)
		model->selectIndividual(ind);
		model->setIndPar(b);
		model->setPopPar(alpha);
		model->dataMean(f0);
		model->dataVariance(R);
		model->indParVariance(D);
		// compute the residual
		for(j = 0; j < Ni; j++)
			r[j] = _Y[j] - f0[j];
		// compute the finite difference approximation for f_b
		for(j = 0; j < _n; j++)
		{	b[j] = _bStep[j];
			model->setIndPar(b);
			model->dataMean(fp);
			b[j] = - _bStep[j];
			model->setIndPar(b);
			model->dataMean(fm);
			for(i = 0; i < Ni; i++)
				f_b[i + Ni * j] = .5 * (fp[i]-fm[i])/_bStep[j];
		}
		// R = R + f_b * D * f_b'
		for(i = 0; i < Ni; i++);
		for(j = i; j < Ni; j++)
		{	Scalar sum = 0;
			for(k = 0; k < _n; k++) 
			for(ell = 0; ell < _n; ell++)
			{	sum += f_b[i + Ni * k] * 
					D[ k + _n * ell] * f_b[j + Ni * ell];
			}
			R[i + j * Ni] += sum;
		}
		// compute R^{-1} * r
		Scalar logdet;
		int signdet = CppAD::LuSolve(Ni, 1, R, r, Rinv_r, logdet);
		if( signdet != 1 ) SPK_PROGRAMMER_ERROR(
			"firstOrder: R + f_b * D * f_b' not positive definite."
		);
		// 1/2 * log( det( 2 * pi * R ) ) + 1/2 * r' * R^{-1} * r
		double pi = 4. * std::atan(1.);
		logdet = logdet + Ni * log( 2 * pi );
		Scalar r_Rinv_r = 0;
		for(j = 0; j < Ni; j++)
			r_Rinv_r += r[j] * Rinv_r[j];
		*obj = .5 * (logdet + r_Rinv_r);
		return;
	}
public:
	// constructor
	FirstOrderObj(
		SpkModel<double>&              model      ,
		SpkModel< CppAD::AD<double> >& adModel    ,
		int                            m          ,
		int                            n          ,
		const DoubleMatrix&            dvecN      ,
		const DoubleMatrix&            dvecY      ,
		const DoubleMatrix&            dvecBStep  ,
		Optimizer&                     alpOptInfo )
	:
	_M(dvecN.nr()),
	_m(m),
	_n(n),
	_model(&model),
	_adModel(&adModel),
	_N(dvecN.data()),
	_Y(dvecY.data()),
	_bStep(dvecBStep.data()),
	_alpha_valarray(_m)
	{	// give the optimizer a pointer to this objective evaluator
		alpOptInfo.setObjFunc( this );
	}
	// objective function
	virtual void function(const DoubleMatrix &alpha, double *Ltilde)
	{	if( alpha.nr()!=_m || alpha.nc()!=1 ) SPK_PROGRAMMER_ERROR(
		"FirstOrderObj.function: alpha does not have proper dimensions"
		); 
		int i, j;
		double Ltilde_i;
		*Ltilde = 0;
		for(j = 0; j < _m; j++)
			_alpha_valarray[j] = *(alpha.data() + j);
		for(i = 0; i < _M; i++)
		{	function(i, _alpha_valarray, &Ltilde_i, _model);
			*Ltilde += Ltilde_i;
		}
		return;
	}
	// gradient of objective
	virtual void gradient (DoubleMatrix *Ltilde_alp) const
	{	typedef CppAD::AD<double> Scalar;
		typedef std::valarray<Scalar> adVector;
		typedef std::valarray<double> dVector;
		if( Ltilde_alp->nr()!=1 || Ltilde_alp->nc()!=_m ) 
		SPK_PROGRAMMER_ERROR(
		"FirstOrderObj.gradient: alpha does not have proper dimensions"
		); 
		int i, j;
		double *ptr_Ltilde_alp = Ltilde_alp->data();
		adVector alpha_valarray(_m);
		for(j = 0; j < _m; j++)
		{	alpha_valarray[j] = _alpha_valarray[j];
			ptr_Ltilde_alp[j] = 0.;
		}

		adVector Ltilde_i(1);
		dVector  Ltilde_alp_i(_m);
		dVector  weight(1);
		for(i = 0; i < _M; i++)
		{	CppAD::Independent(alpha_valarray);
			Scalar obj_i;
			function(i, alpha_valarray, &obj_i, _adModel);
			Ltilde_i[0] = obj_i;
			CppAD::ADFun<double> F(alpha_valarray, Ltilde_i);
			weight[0] = 1.;
			Ltilde_alp_i = F.Reverse(1, weight);
			for(j = 0; j < _m; j++)
				ptr_Ltilde_alp[j] += Ltilde_alp_i[j];
		}
		return;
	}
};

void firstOrderOpt(
              SpkModel<double>&              model                      ,
              SpkModel< CppAD::AD<double> >& adModel                    ,
              const DoubleMatrix&            dvecN                      ,
              const DoubleMatrix&            dvecY                      ,
              Optimizer&                     alpOptInfo                 ,
              const DoubleMatrix&            dvecAlpLow                 ,
              const DoubleMatrix&            dvecAlpUp                  ,
              const DoubleMatrix&            dvecAlpIn                  ,
              DoubleMatrix*                  pvecAlpOut                ,
              const DoubleMatrix&            dvecAlpStep                ,
              Optimizer&                     bOptInfo                   ,
              const DoubleMatrix&            dvecBLow                   ,
              const DoubleMatrix&            dvecBUp                    ,
              const DoubleMatrix&            dmatBIn                    ,
              DoubleMatrix*                  pmatBOut                  ,
              const DoubleMatrix&            dvecBStep                  ,
              double*                        pdLTildeOut                ,
              DoubleMatrix*                  pdrowLTilde_alpOut         ,
              DoubleMatrix*                  pmatLTilde_alp_alpOut     )
{	// Check for input errors
	if( bOptInfo.getSaveStateAtEndOfOpt() ) SPK_PROGRAMMER_ERROR(
		"fristOrderOpt: Invalid value for SaveStateAtEndOfOpt"
	);
	if( alpOptInfo.getThrowExcepIfMaxIter() ) SPK_PROGRAMMER_ERROR(
		"fristOrderOpt: Invalid value for ThrowExcepIfMaxIter"
	);

	// Special case where there is nothing to calculate
	bool nothing_to_compute = true;
	nothing_to_compute &= (pvecAlpOut              == 0 );
	nothing_to_compute &= (pmatBOut                == 0 );
	nothing_to_compute &= (pdLTildeOut              == 0 );
	nothing_to_compute &= (pdrowLTilde_alpOut       == 0 );
	nothing_to_compute &= (pmatLTilde_alp_alpOut   == 0 );
	if( nothing_to_compute )
		return;

	// get problem dimensions
	const int M = dvecN.nr();
	const int m = dvecAlpIn.nr();
	const int n = dmatBIn.nr();

	// A temporary column vector for holding the estimate for alp
	DoubleMatrix dvecAlpOutTemp(m, 1);

	// function objective
	FirstOrderObj objective( 
		model      ,
		adModel    ,
		m          ,
		n          ,
		dvecN      ,
		dvecY      ,
		dvecBStep  ,
		alpOptInfo 
	);

	// try to optimize the FO version of Ltilde
	const char *msg="firstOrderOpt: Invalid error message";
	try
	{	quasiNewtonAnyBox(
			objective            ,
			alpOptInfo           ,
			dvecAlpLow           ,
			dvecAlpUp            ,
			dvecAlpIn            ,
			&dvecAlpOutTemp      ,
			pdLTildeOut          ,
			pdrowLTilde_alpOut
		);
	}
	catch( SpkException& e)
	{	msg = "firstOrderOpt: fixed effects optimization "
		      "known exception.";
		throw e.push(
			SpkError::SPK_OPT_ERR,
			msg                  ,
			__LINE__             ,
			__FILE__
		);
	}
	catch ( ... )
	{	msg = "firstOrderOpt: fixed effects optimization "
		      "unknown exception.";
		throw SpkException(
			SpkError::SPK_UNKNOWN_OPT_ERR,
			msg                          ,
			__LINE__                     ,
			__FILE__
		);
	}

	// Using the FO approximation for the fixed effects alpha, 
	// determine each individuals random effects b_i
	// by optimizing the original model (not the FO approximation)
	if( pmatBOut )
	{
		int i, j, index_Y;
		DoubleMatrix dvecBIn(n, 1);	
		DoubleMatrix dvecBOut(n, 1);	

		// pointers to actual data in vectors and matrices
		const double *N       = dvecN.data();
		const double *Y       = dvecY.data();
		const double *alpIn   = dvecAlpIn.data();
		const double *bmatIn  = dmatBIn.data();
		double *bmatOut       = pmatBOut->data();
		double *bOut          = dvecBOut.data();
		double *bIn           = dvecBIn.data();

		// set the fixed effects value
		model.setPopPar( dvecAlpOutTemp.toValarray() );

		// include the Bayesian term in the individual objective
		bool withD = true;

		index_Y = 0;
		for(i = 0; i < M; i++);
		{	// data for this individual
			int Ni = static_cast<int>( N[i] ); 	
			DoubleMatrix dvecY_i(Ni, 1);
			double *Y_i = dvecY_i.data();
			for(j = 0; j < Ni; j++)
				Y_i[j] = Y[index_Y++];
			// initialize random effects to this individual
			for(j = 0; j < n; j++)
				bIn[j] = bmatIn[j + n];
			// optimization for this individual
			try
			{	mapOpt(
					model           ,
					dvecY_i         ,
					bOptInfo        ,
					dvecBLow        ,
					dvecBUp         ,
					dvecBIn         ,       
					&dvecBOut       ,
					dvecBStep       ,
					0               ,
					0               ,
					0               ,
					withD            
				);
			}
			catch( SpkException& e)
			{	msg = "firstOrderOpt: random effects "
				      "optimization known exception.";
				throw e.push(
					SpkError::SPK_OPT_ERR,
					msg                  ,
					__LINE__             ,
					__FILE__
				);
			}
			catch ( ... )
			{	msg = "firstOrderOpt: random effects "
				      "optimization unknown exception.";
				throw SpkException(
					SpkError::SPK_UNKNOWN_OPT_ERR,
					msg                          ,
					__LINE__                     ,
					__FILE__
				);
			}
			for(j = 0; j < n; j++)
				bmatOut[j + n] = bOut[j];
		}
	}
	return;
}
