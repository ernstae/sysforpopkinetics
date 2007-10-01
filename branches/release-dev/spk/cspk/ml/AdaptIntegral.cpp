/*
$begin AdaptIntegral$$
$spell
	const
	std
	valarray
	const
	Feval
	maxpts
	ndim
$$

$section A.C. Gen'z Adaptive Multidimensional Integration$$

$table
$bold Syntax$$
$cnext
$syntax%void AdaptIntegral(
	double (*%Feval%)(double *%X%, size_t %ndim%, void *p) ,
	size_t                        %ndim%            ,
	void                         *%p%               ,
        size_t                        %maxpts%          ,
	const std::valarray<double>  &%L%               ,
	const std::valarray<double>  &%U%               ,
	double                       &%integralEstimate%,
	double                       &%estimateStd%     )%$$

$tend

$fend 25$$

$head integralEstimate$$
is an approximation for the integral
$latex \[
	\int_L^U f(x) \D x
\]$$

$head estimateStd$$
The input value of $italic estimateStd$$ does not matter.
Its output value
is an approximation for the standard deviation of $italic estimateIntegral$$.


$head Feval$$
The syntax
$syntax%
	Feval(%X%, %ndim%, %p%)
%$$
returns the value of the function $latex f$$ at the point
specified by $italic X$$ where $italic X$$ is a vector
of length $italic ndim$$. Both $italic ndim$$ and $italic p$$ are
the same value as in the call to $code AdaptIntegral$$.

$head ndim$$
The argument $italic ndim$$ to both $code AdaptIntegral$$ and
to $italic Feval$$ is the dimension of the space we are integrating
with respect to.

$head maxpts$$
The value $italic maxpts$$ is
the maximum number of function evaluations to use.
This must be at least as big as
$latex \[
	2^ndim + 2 ndim^2 + 6 ndim + 1
\]$$
A suggested value is
$latex \[
	maxpts = 100 ( 2^ndim + 2 ndim^2 + 6 ndim + 1 )
\]$$

$head L$$
The vector $italic L$$ has length $italic ndim$$ and specifies
the lower limit for the integration.

$head U$$
The vector $italic U$$ has length $italic ndim$$ and specifies
the upper limit for the integration.

$end

*/

# include "AdaptIntegral.h"
# include <iostream>
# include <valarray>
# include <cassert>
# include <spk/SpkError.h>
# include <spk/SpkException.h>

# include "f2c.h"
# include "adapt.h"
# include <QN01Box/Memory.h>


void AdaptIntegral(
	doublereal (*Functn)(integer *ndim, doublereal *z)     ,
	size_t                            ndim             ,
        size_t                            maxpts           ,
	const std::valarray<double>      &L                ,
	const std::valarray<double>      &U                ,
	double                           &integralEstimate ,
	double                           &estimateStd      )
{
	size_t two_ndim = 1;    // 2^ndim
	size_t i;
	for(i = 0; i < ndim; i++)
		two_ndim *= 2;
	size_t rulcls = two_ndim + 2*ndim*ndim + 6*ndim + 1;
	size_t lenwrk = (2 * ndim + 3) * (1 + maxpts / rulcls) / 2;

	integer Ndim   = static_cast<integer>(ndim);
	integer Lenwrk = static_cast<integer>(lenwrk);
	integer Maxpts = static_cast<integer>(maxpts);
	integer Minpts = Maxpts; 
	integer Ifail;

	doublereal Eps = 1e-6;
	doublereal Finest;
	doublereal Relerr;

	size_t d_size = ndim + ndim + lenwrk;
	QN01Box::Memory<doublereal> dMemory(d_size);
	doublereal *A = dMemory(ndim);
	doublereal *B = dMemory(ndim);
	doublereal *Wrkstr = dMemory(lenwrk);

	for(i = 0; i < ndim; i++)
	{	A[i] = L[i];
		B[i] = U[i];
	}


        adapt_(
                &Ndim,
                A,
                B,
                &Minpts,
                &Maxpts,
                Functn,
                &Eps,
                &Relerr,
                &Lenwrk,
                Wrkstr,
                &Finest,
                &Ifail
        );
	if( static_cast<int>(Ifail) == 3 )
	{	// assume that minpts <= maxpts and rulcls <= maxpts
		throw SpkException(
			SpkError::SPK_USER_INPUT_ERR,
			"Adapt: number of random effects is < 2 or > 20",
			__LINE__,
			__FILE__
		);
	}
	integralEstimate = static_cast<double>(Finest);
	estimateStd      = static_cast<double>(Relerr);
	estimateStd     *= fabs(integralEstimate);
  
	return;
}
