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
 * File: lambda.cpp
 *
 *
 * Implementation of Lambda and its derivatives with respect to
 * alpha and b.
 *
 * Author: Sachiko Honda
 *
 * Reviewer: Mitch Watrous
 *
 * Follow-up: Sachiko Honda
 *
 *************************************************************************/


/*************************************************************************
 *
 * Function: lambda
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin lambda$$
$escape #$$
$spell
	Model model
    bool const alp iostream using namespace std NOREQ 
    dmat Da Fab Rab dvec nr nc pd cout endl placeholders
    distrib inv dinv Dinv pf pb Dinv int
    covariance covariances Spk
    ind
    palp
    valarray
$$ 

$section Individual Joint Negative Log-likelihood$$

$index lambda$$
$index lambda, individual joint #negative log-likelihood$$


$table
$bold Prototype:$$   $cend  
$syntax/double lambda( 
    SpkModel &/model/,
    const DoubleMatrix &/y/,
    const DoubleMatrix &/alp/,
    const DoubleMatrix &/b/,
    bool  includeD
  )/$$
$tend

See also: $xref/lambda_alp//lambda_alp/$$, $xref/lambda_b//lambda_b/$$.
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$comment [Review - Mitch - 12/11/01] Add " and math%b%" before 
the period at the end of this sentence and put dollar signs around 
"math%b%".$$
$comment [Respond - Sachiko - 12/17/01] Accepted.$$
Evaluates the negative log-likelihood of $math%y%$$ and $math%b%$$.
$pre

$$
To be specific,
$math%
                 1 %          %                 1               T  -1
Lambda(alp, b) = - #logdet[ 2 #pi R(alp, b) ] + - [y - f(alp, b)] R (alp, b) [y - f(alp, b)]
                 2 %          %                 2

                 1 %          %                 1  T  -1
               + - #logdet[ 2 #pi D(alp) ]    + - b  D (alp)  b
                 2 %          %                 2
%$$
(The equation above uses
$xref/glossary/Individual Notation/individual notation/$$.)

$head Reference$$
$italic
Approximating The Maximum Likelihood Estimate For Models With Random Parameter
$$

$head Return Value$$
$comment [Review - Mitch - 11/06/01] Delete "normally and"$$
$comment [Respond - Sachiko - 12/17/01] accepted$$

Upon a successful completion, the function returns the

$comment [Review - Mitch - 11/06/01] Change "objective function" to "math%Lambda%" and then put dollar signs around "math%Lambda%"$$ 
$comment [Respond - Sachiko - 12/17/01] accepted$$

$math%Lambda%$$ value as a double precision scalar.

$comment [Review - Mitch - 11/06/01] Change "a SpkException" to "an SpkException"$$
$comment [Respond - Sachiko - 12/17/01] accepted$$

If a failure occurs during the evaluation, an SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be 
$xref/glossary/Model Functions Depend on only b/a function of b/$$.
$syntax/
/y/
/$$
is a $math%m%$$ dimensional column vector containing the data vector.
$syntax/

/alp/
/$$
The $math%n%$$ dimensional column vector $italic alp$$
specifies a value for the fixed population parameter vector.
$syntax/

/b/
/$$
The $math%n%$$ dimensional column vector $italic b$$
specifies a value for the random population parameter vector.
$syntax/

/includeD/
/$$
$comment [Review - Mitch - 11/06/01] Change "D term" to "terms involving D"$$ 
$comment [Respond - Sachiko - 12/17/01] Accepted.$$

is a boolean flag indicating as to whether the terms involving D (the variance of individuals parameters)

$comment [Review - Mitch - 11/06/01] Replace whole line with "are to be included in math%Lambda%." 
and then put dollar signs around "math%Lambda%"$$ 
$comment [Respond - Sachiko - 12/17/01] Accepted.$$

are to be included in $math%Lambda%$$.

$comment [Review - Mitch - 11/06/01] Change "term" to "terms" and change "individual objective" to "calculation."$$ 
$comment [Respond - Sachiko - 12/17/01] Accepted.$$

If false is given, the D terms will be completely eliminated from the calculation.
$comment [Review - Mitch - 11/06/01] Delete whole line.$$
$comment [Respond - Sachiko - 12/17/01] Accepted.$$


$head Example$$
Suppose that
$math%
                /  b(1)     0  \
    R(alp, b) = |              |
                \  0      b(1) /

                / alp(1)  0 \
    D(alp)    = |           |
                \ 0  alp(1) /

                / alp(2) + b(2) \
    f(alp, b) = |               |
                \ alp(2) + b(2) /

                / 1 \
    y         = |   |
                \ 1 /
%$$
It follows that
$math%                         
Lambda(alp, b) = (1/2) #log{[2 #pi b(1)]^2}   + [1 - alp(2) - b(2)]^2 / b(1)
               + (1/2) #log{[2 #pi alp(1)]^2} + (1/2) [b(1)^2 + b(2)^2] / alp(1)
%$$
If all the components of $math%alp%$$ and $math%b%$$ are one,
$math%
Lambda(alp, b)     = #log(2 #pi) + #log(2 #pi) + 1  + 1
                   = 2 #log(2 #pi) + 2
 
%$$

$head Coding Example$$
If you compile, link, and run the following program:

$codep

    #include <iostream>
  
    #include "DoubleMatrix.h"
    #include "lambda.h"
    #include "SpkModel.h"

    static DoubleMatrix getY();
    static DoubleMatrix getAlp();
    static DoubleMatrix getB();

    static DoubleMatrix funD(    const DoubleMatrix &alp );
    static DoubleMatrix funD_alp(const DoubleMatrix &dmatD, const DoubleMatrix &alp );
    static DoubleMatrix funF(    const DoubleMatrix &alp,   const DoubleMatrix &b );
    static DoubleMatrix funF_alp(const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b );
    static DoubleMatrix funF_b(  const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b );
    static DoubleMatrix funR(    const DoubleMatrix &alp,   const DoubleMatrix &b );
    static DoubleMatrix funR_alp(const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b );
    static DoubleMatrix funR_b(  const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b );

    class UserModel : public SpkModel
    {
        DoubleMatrix _a, _b;
        int _i;

    public:
        UserModel(){};  
        ~UserModel(){};
    protected:
        void doSelectIndividual(int i)
        {
            _i = i;
        }
        void doSetPopPar(const valarray<double>& a)
        {
            _a = DoubleMatrix( a, 1 );
        }
        void doSetIndPar(const valarray<double>& b)
        {
            _b = DoubleMatrix( b. 1 );
        }
        void doIndParVariance( valarray<double> & ret ) const
        {
            ret = funD(_a).toValarray();
        }
        bool doIndParVariance_popPar( valarray<double> & ret ) const
        {
            doIndParVariance(ret);
            ret = funD_alp(ret, _a).toValarray();
            return !allZero(ret);
        }
        void doIndParVarianceInv( valarray<double> & ret ) const
        {
            doIndParVariance(ret);
            return getDinv(ret).toValarray();
        }
        bool doIndParVarianceInv_popPar( valarray<double> & ret ) const
        {
            doIndParVarianceInv(ret);
            ret = funDinv_alp(ret, _a).toValarray();
            return !allZero(ret);
        }
        bool doDataMean( valarray<double> & ret ) const
        {
            ret = funF(_a, _b).toValarray();
        }
        bool doDataMean_popPar( valarray<double> & ret ) const
        {
            doDataMean_popPar(ret);
            ret = funF_alp(ret, _a, _b).toValarray();
            return !allZero(ret);
        }
        bool doDataMean_indPar( valarray<double> & ret ) const
        {
            doDataMean_popPar(ret);
            ret = funF_b(ret, _a, _b).toValarray();
            return !allZero(ret);
        }
        void doDataVariance( valarray<double> & ret ) const
        {
            return funR(_a, _b);
        }
        bool doDataVariance_popPar( valarray<double> & ret ) const
        {
            doDataVariance(ret);
            ret = funR_alp(ret, _a, _b).toValarray();
            return !allZero(ret);
        }
        bool doDataVariance_indPar( valarray<double> & ret ) const
        {
            doDataVariance(ret);
            ret = funR_b(ret, _a, _b).toValarray();
            return !allZero(ret);
        }   
    };
    void main()
    {
        using namespace std;

        // initialize Model
        UserModel model;

        double       lambdaOut;

        // initialize y (individual's data), alpha (fixed parameter), and b (random parameter)
        DoubleMatrix y     = getY();
        DoubleMatrix alp   = getAlp();
        DoubleMatrix b     = getB();

        // evaluate the individual objective function
        lambdaOut = lambda( model, y, alp, b, true);
            cout << "Lambda - 2 log(2 pi) = " << lambdaOut - 2.0*log(2.0*PI) << endl;
    }

    //
    // y = [1]
    //     [1]
    //
    static DoubleMatrix getY()
    {
        DoubleMatrix y(2,1);
        y.fill(1.0);
        return y;
    }

    //
    // alpha = [1]
    //         [1]
    ///
    static DoubleMatrix getAlp()
    {
        DoubleMatrix alp(2,1);
        alp.fill(1.0);
        return alp;
    }

    //
    // b = [1]
    //     [1]
    //
    static DoubleMatrix getB()
    {
        DoubleMatrix b(2,1);
        b.fill(1.0);
        return b;
    }

    //
    // D = [alp(0)   0   ]
    //     [   0   alp(0)]
    //
    static DoubleMatrix funD( const DoubleMatrix &alp )
    {
        DoubleMatrix dmatD(2,2);
        double *pD = dmatD.data(),
               *palp = alp.data();

        pD[0] = palp[0];
        pD[1] = 0.0;
        pD[2] = 0.0;
        pD[3] = palp[0];
        return dmatD;
    }

    //
    // D_alp = [ 1  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 1  0 ]
    //
    static DoubleMatrix funD_alp( const DoubleMatrix &dmatD, const DoubleMatrix &alp )
    {
        DoubleMatrix dmatD_alp( dmatD.nr()*dmatD.nc(), alp.nr() );
        double *pD_alp = dmatD_alp.data();
        dmatD_alp.fill(0.0);

        pD_alp[0] = 1.0;
        pD_alp[3] = 1.0;
        return dmatD_alp;
    }

    //
    // f(alpha, b) = [ alp(1) + b(1) ]
    //               [ alp(1) + b(1) ]
    //
    static DoubleMatrix funF( const DoubleMatrix &alp, const DoubleMatrix &b )
    {
        DoubleMatrix dmatF(2,1);
        double *palp = alp.data(),
               *pb   = b.data();

        dmatF.fill( palp[1] + pb[1] );
        return dmatF;
    }

    //
    // f_alpha = [ 0  1 ]
    //           [ 0  1 ]
    //
    static DoubleMatrix funF_alp( const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b )
    {
        DoubleMatrix dmatF_alp(dvecF.nr(), alp.nr());
        double *pf_alp = dmatF_alp.data();

        pf_alp[0] = 0.0;
        pf_alp[1] = 0.0;
        pf_alp[2] = 1.0;
        pf_alp[3] = 1.0;
        return dmatF_alp;
    }

    //
    // f_b = [ 0  1 ]
    //       [ 0  1 ]
    //
    static DoubleMatrix funF_b( const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b )
    {
        DoubleMatrix dmatF_b(dvecF.nr(), b.nr());
        double *pf_b = dmatF_b.data();

        pf_b[0] = 0.0;
        pf_b[1] = 0.0;
        pf_b[2] = 1.0;
        pf_b[3] = 1.0;
        return dmatF_b;
    }

    //
    // R(alpha, b) = [ b(0)  0   ]
    //               [  0   b(0) ]
    //
    static DoubleMatrix funR(  const DoubleMatrix &alp, const DoubleMatrix &b )
    {
        DoubleMatrix dmatR(2,2);
        double *pR = dmatR.data(),
               *pb = b.data();

        pR[0] = pb[0];
        pR[1] = 0.0;
        pR[2] = 0.0;
        pR[3] = pb[0];
        return dmatR;
    }

    //
    // R_alp = [ 0  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //
    static DoubleMatrix funR_alp( const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b )
    {
        DoubleMatrix dmatR_alp(dmatR.nr()*dmatR.nc(), alp.nr());
        dmatR_alp.fill(0.0);
        return dmatR_alp;
    }

    //
    // R_b   = [ 1  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 1  0 ]
    //
    static DoubleMatrix funR_b( const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b )
    {
        DoubleMatrix dmatR_b(dmatR.nr()*dmatR.nc(), b.nr());
        double *pR_b = dmatR_b.data();
        dmatR_b.fill(0.0);
        pR_b[0] = 1.0;
        pR_b[3] = 1.0;
        return dmatR_b;
    }
$$
The following results will be displayed.
$codep
    Lambda - 2 log(2 pi) = 2

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <cstdlib>
#include <cassert>
#include <cmath>
#include "SpkModel.h"
#include "elsq_x.h"
#include "det.h"
#include "add.h"
#include "transpose.h"
#include "multiply.h"
#include "DoubleMatrix.h"
#include "pi.h"
#include "subtract.h"

#include "SpkValarray.h"

/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

static bool hasPosDet( const DoubleMatrix& dmatA );


/*------------------------------------------------------------------------
 * Local definitions
 *------------------------------------------------------------------------*/

static const double logTwoPi = log( 2.0 * PI );

double lambda( SpkModel &model, 
            const DoubleMatrix &dvecY, 
            const DoubleMatrix &dvecAlp, 
            const DoubleMatrix &dvecB,
            bool    withD
            )
{
    using namespace std;

    int nA = dvecAlp.nr();
    int nB = dvecB.nr();
    int nY = dvecY.nr();

    const valarray<double> alp = dvecAlp.toValarray();
    const valarray<double> b   = dvecB.toValarray();
    const valarray<double> y   = dvecY.toValarray();

    model.setPopPar(alp);
    model.setIndPar(b);

    // Calculate f(alp,b).
    valarray<double> fab(nY);
    model.dataMean(fab);
    assert(fab.size() == nY);

    // Calculate y - f(alp,b).
    valarray<double> residual = y - fab;

    //subtract( dvecY, dvecFab, dvecYMinusFab );

    // Calculate the first two terms in Lambda(alp, b),
    //
    //     1                              1                T  -1
    //     -  logdet[ 2  pi R(alp, b) ] + - [y - f(alp, b)]  R  (alp, b) [y - f(alp, b)]  .
    //     2                              2
    //
    double lambdaOut = 0.5 * ( nY * logTwoPi + 
      model.getDataCovariance().logdet() + 
      model.getDataCovariance().weightedSumOfSquares( residual ) );

    if( withD ){
      // Add in the last two terms in Lambda(alp, b), 
      //
      //     1                            1  T  -1
      //     - logdet[ 2 pi D(alp) ]    + - b  D  (alp)  b  .
      //     2                            2
      //
      lambdaOut += 0.5 * ( nB * logTwoPi + 
        model.getIndParCovariance().logdet() + 
        model.getIndParCovariance().weightedSumOfSquares( b ) );
    }

    return lambdaOut;
}

/*************************************************************************
 *
 * Function: lambda_alp (analytical derivative of lambda wrt alp)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin lambda_alp$$
$escape #$$
$spell
	Model model
    bool const alp iostream using namespace std NOREQ ind 
    dmat Da Fab Rab dvec nr nc pd cout endl placeholders palp
    distrib inv dinv Dinv pf pb Dinv int
    covariances
    covariance
    Spk
    pop
    valarray
$$ 

$comment [Review - Mitch - 12/11/01] Change the section title to
"Derivative of the Individual Joint Negative Log-likelihood with Respect to alp".$$
$comment [Respond - Sachiko - 12/17/01] Accepted.$$

$section Derivative of the Individual Joint Negative Log-likelihood with Respect to alp$$

$index lambda_alp$$
$index lambda, derivative of individual joint #negative log-likelihood$$

$table
$bold Prototype:$$   $cend  
$syntax/const DoubleMatrix lambda_alp( 
    SpkModel &/model/,
    const DoubleMatrix &/y/,
    const DoubleMatrix &/alp/,
    const DoubleMatrix &/b/,
    bool  includeD
    )/$$
$tend

See also: $xref/lambda//lambda/$$, $xref/lambda_b//lambda_b/$$
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Evaluates the partial true derivative of the negative log-likelihood of $math%y%$$ and $math%b%$$ 
with respect to the population parameter.  
$comment [Review - Mitch - 12/11/01] Move this sentence to a 
"Return Value" section.$$
$comment [Respond - Sachiko - 12/17/01] Accepted$$

$pre

$$
To be specific,
$math%
                 1 %          %                 1               T  -1
Lambda(alp, b) = - #logdet[ 2 #pi R(alp, b) ] + - [y - f(alp, b)] R (alp, b) [y - f(alp, b)]
                 2 %          %                 2

                 1 %          %                 1  T  -1
               + - #logdet[ 2 #pi D(alp) ]    + - b  D (alp)  b
                 2 %          %                 2
%$$
(The equation above uses
$xref/glossary/Individual Notation/individual notation/$$.)


$head Reference$$
$italic
Approximating The Maximum Likelihood Estimate For Models With Random Parameter
$$

$comment [Review - Mitch - 12/11/01] Add a "Return Value" section 
(see lambda() specification above) that discusses exceptions and
that describes the return value.$$
$comment [Respond - Sachiko - 12/17/01] Accepted$$

$head Return Value$$
The resulting value is returned as a $math%n%$$ dimensional row vector, 
where $math%n%$$ is the size of $italic alp$$.
If a failure occurs during the evaluation, an SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$comment [Review - Mitch - 12/11/01] Delete this sentence since it 
is no longer true.$$
$comment [Respond - Sachiko - 12/17/01] Accepted$$

$syntax/
/model/
/$$
This function expects $italic model$$ to be 
$xref/glossary/Model Functions Depend on only b/a function of b/$$

$syntax/
/y/
/$$
is a $math%m%$$ dimensional column vector containing the data vector.
$syntax/

/alp/
/$$
The $math%n%$$ dimensional column vector $italic alp$$
specifies a value for the fixed population parameter vector.
$syntax/

/b/
/$$
The $math%n%$$ dimensional column vector $italic b$$
specifies a value for the random population parameter vector.
$syntax/

/includeD/
/$$
$comment [Review - Mitch - 12/11/01] This should be the same as the 
includeD description in the lambda() specification above.$$ 
$comment [Respond - Sachiko - 12/17/01] Accepted.$$

is a boolean flag indicating as to whether the terms involving D (the variance of individuals parameters)
are to be included in $math%Lambda%$$.
If false is given, the D terms will be completely eliminated from the calculation.

$head Example$$
Suppose that
$math%
                /  b(1)     0  \
    R(alp, b) = |              |
                \  0      b(1) /

                / alp(1)  0 \
    D(alp)    = |           |
                \ 0  alp(1) /

                / alp(2) + b(2) \
    f(alp, b) = |               |
                \ alp(2) + b(2) /

                / 1 \
    y         = |   |
                \ 1 /
%$$
It follows that
$math%                         
Lambda(alp, b) = (1/2) #log{[2 #pi b(1)]^2}   + [1 - alp(2) - b(2)]^2 / b(1)
               + (1/2) #log{[2 #pi alp(1)]^2} + (1/2) [b(1)^2 + b(2)^2] / alp(1)
%$$
The transpose of $math%Lambda_alp(alp, b)%$$ is equal to
$math%
/ 1 / alp(1) - (1/2) [b(1)^2 + b(2)^2] / alp(1)^2  \
|                                                  |
\       - 2 [1 - alp(2) - b(2)] / b(1)             /
%$$
If all the components of $math%alp%$$ and $math%b%$$ are one,
$math%
Lambda(alp, b)     = #log(2 #pi) + #log(2 #pi) + 1  + 1
                   = 2 #log(2 #pi) + 2

Lambda_alp(alp, b) = [ 1 - 1 , 2 ]
                   = [ 0 , 2 ]

  
%$$

$head Coding Example$$
If you compile, link, and run the following program:

$codep

    #include <iostream>
    #include "DoubleMatrix.h"
    #include "lambda.h"
    #include "SpkModel.h"

    static DoubleMatrix getY();
    static DoubleMatrix getAlp();
    static DoubleMatrix getB();

    static DoubleMatrix funD(    const DoubleMatrix &alp );
    static DoubleMatrix funD_alp(const DoubleMatrix &dmatD, const DoubleMatrix &alp );
    static DoubleMatrix funF(    const DoubleMatrix &alp,   const DoubleMatrix &b );
    static DoubleMatrix funF_alp(const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b );
    static DoubleMatrix funF_b(  const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b );
    static DoubleMatrix funR(    const DoubleMatrix &alp,   const DoubleMatrix &b );
    static DoubleMatrix funR_alp(const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b );
    static DoubleMatrix funR_b(  const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b );

    class UserModel : public SpkModel
    {
        DoubleMatrix _a, _b;
        int _i;
    public:
        UserModel(){};  
        ~UserModel(){};
    protected:
        void doSelectIndividual(int i)
        {
            _i = i;
        }
        void doSetPopPar(const valarray<double>& a)
        {
            _a = DoubleMatrix( a, 1 );
        }
        void doSetIndPar(const valarray<double>& b)
        {
            _b = DoubleMatrix( b, 1 );
        }
        void doDataMean( valarray<double> & ret ) const
        {
            ret = funF(_a, _b).toValarray();
        }
        bool doDataMean_popPar( valarray<double> & ret ) const
        {
            doDataMean_popPar(ret);
            ret = funF_alp(ret, _a, _b).toValarray();
            return !allZero(ret);
        }
        bool doDataMean_indPar( valarray<double> & ret ) const
        {
            doDataMean_popPar(ret);
            ret = funF_b(ret, _a, _b).toValarray();
            return !allZero(ret);
        }
        void doDataVariance( valarray<double> & ret ) const
        {
            ret = funR(_a, _b).toValarray();
        }
        bool doDataVariance_popPar( valarray<double> & ret ) const
        {
            doDataVariance(ret);
            ret = funR_alp(ret, _a, _b).toValarray();
            return !allZero(ret);
        }
        bool doDataVariance_indPar( valarray<double> & ret ) const
        {
            doDataVariance(ret);
            ret = funR_b(ret, _a, _b).toValarray();
            return !allZero(ret);
        }   
        void doIndParVariance( valarray<double> & ret ) const
        {
            ret = funD(_a);
        }
        bool doIndParVariance_popPar( valarray<double> & ret ) const
        {
            doIndParVariance(ret);
            ret = funD_alp(ret, _a).toValarray();
            return !allZero(ret);
        }
        void doIndParVarianceInv( valarray<double> & ret ) const
        {
            doIndParVariance(ret);
            ret = getDinv(ret).toValarray();
        }
        bool doIndParVarianceInv_popPar( valarray<double> & ret ) const
        {
            doIndParVarianceInv(ret);
            ret = funDinv_alp(ret, _a).toValarray();
            return !allZero(ret);
        }
    };
    void main()
    {
        using namespace std;

        // initialize Model
        UserModel model;

        // to request all outputs, allocate memory to all placeholders
        double       lambdaOut;
        DoubleMatrix lambda_alpOut,
                     lambda_bOut;

        // initialize y (individual's data), alpha (fixed parameter), and b (random parameter)
        DoubleMatrix y     = getY(),
                     alp   = getAlp(),
                     b     = getB();

        // evaluate Lambda
        model.setPop(alp);
        lambdaOut = lambda( model, y, alp, b, true );
            cout << "Lambda - 2 log(2 pi) = " << lambdaOut - 2.0*log(2.0*PI) << endl;
        lambda_alpOut = lambda_alpOut(model, y, alp, b, true );
            cout << "Lambda_alp = " << endl;
            lambda_alpOut.print();
        }
        else{
            cout << "Something sent wrong during the evaluation." << endl;
        }
    }

    //
    // y = [1]
    //     [1]
    //
    static DoubleMatrix getY(){
        DoubleMatrix y(2,1);
        y.fill(1.0);
        return y;
    }

    //
    // alpha = [1]
    //         [1]
    ///
    static DoubleMatrix getAlp(){
        DoubleMatrix alp(2,1);
        alp.fill(1.0);
        return alp;
    }

    //
    // b = [1]
    //     [1]
    //
    static DoubleMatrix getB(){
        DoubleMatrix b(2,1);
        b.fill(1.0);
        return b;
    }

    //
    // D = [alp(0)   0   ]
    //     [   0   alp(0)]
    //
    static DoubleMatrix funD( const DoubleMatrix &alp ){
        DoubleMatrix dmatD(2,2);
        double *pD = dmatD.data(),
               *palp = alp.data();

        pD[0] = palp[0];
        pD[1] = 0.0;
        pD[2] = 0.0;
        pD[3] = palp[0];
        return dmatD;
    }

    //
    // D_alp = [ 1  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 1  0 ]
    //
    static DoubleMatrix funD_alp( const DoubleMatrix &dmatD, const DoubleMatrix &alp ){
        DoubleMatrix dmatD_alp( dmatD.nr()*dmatD.nc(), alp.nr() );
        double *pD_alp = dmatD_alp.data();
        dmatD_alp.fill(0.0);

        pD_alp[0] = 1.0;
        pD_alp[3] = 1.0;
        return dmatD_alp;
    }

    //
    // f(alpha, b) = [ alp(1) + b(1) ]
    //               [ alp(1) + b(1) ]
    //
    static DoubleMatrix funF( const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatF(2,1);
        double *palp = alp.data(),
               *pb   = b.data();

        dmatF.fill( palp[1] + pb[1] );
        return dmatF;
    }

    //
    // f_alpha = [ 0  1 ]
    //           [ 0  1 ]
    //
    static DoubleMatrix funF_alp( const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatF_alp(dvecF.nr(), alp.nr());
        double *pf_alp = dmatF_alp.data();

        pf_alp[0] = 0.0;
        pf_alp[1] = 0.0;
        pf_alp[2] = 1.0;
        pf_alp[3] = 1.0;
        return dmatF_alp;
    }

    //
    // f_b = [ 0  1 ]
    //       [ 0  1 ]
    //
    static DoubleMatrix funF_b( const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatF_b(dvecF.nr(), b.nr());
        double *pf_b = dmatF_b.data();

        pf_b[0] = 0.0;
        pf_b[1] = 0.0;
        pf_b[2] = 1.0;
        pf_b[3] = 1.0;
        return dmatF_b;
    }

    //
    // R(alpha, b) = [ b(0)  0   ]
    //               [  0   b(0) ]
    //
    static DoubleMatrix funR(  const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatR(2,2);
        double *pR = dmatR.data(),
               *pb = b.data();

        pR[0] = pb[0];
        pR[1] = 0.0;
        pR[2] = 0.0;
        pR[3] = pb[0];
        return dmatR;
    }

    //
    // R_alp = [ 0  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //
    static DoubleMatrix funR_alp( const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatR_alp(dmatR.nr()*dmatR.nc(), alp.nr());
        dmatR_alp.fill(0.0);
        return dmatR_alp;
    }

    //
    // R_b   = [ 1  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 1  0 ]
    //
    static DoubleMatrix funR_b( const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatR_b(dmatR.nr()*dmatR.nc(), b.nr());
        double *pR_b = dmatR_b.data();
        dmatR_b.fill(0.0);
        pR_b[0] = 1.0;
        pR_b[3] = 1.0;
        return dmatR_b;
    }
$$
The following results will be displayed.
$codep
    Lambda - 2 log(2 pi) = 2

    Lambda_alp =
    [0, 2]

$$
$end
*/
// [Review - Mitch - 12/11/01] Once the functions logdet_alp and 
// weightedSumOfSquares_alp have been completed for the Covariance
// classes, then the body of this function should be rewritten in 
// a similar fashion as the body of the lambda function above.

const DoubleMatrix lambda_alp(SpkModel &model, 
            const DoubleMatrix &dvecY, 
            const DoubleMatrix &dvecAlp, 
            const DoubleMatrix &dvecB,
            bool    withD
            )
{
    using namespace std;

    int nA = dvecAlp.nr();
    int nB = dvecB.nr();
    int nY = dvecY.nr();

    const valarray<double> alp = dvecAlp.toValarray();
    const valarray<double> b   = dvecB.toValarray();
    const valarray<double> y   = dvecY.toValarray();

    model.setPopPar(alp);
    model.setIndPar(b);

    // Calculate f_alp(alp,b) before f(alp,b) to allow caching.
    valarray<double> fab_alp;
    model.dataMean_popPar( fab_alp );
    DoubleMatrix dmatFab_alp( fab_alp, nA );
    assert( dmatFab_alp.nr() == nY );
    assert( dmatFab_alp.nc() == nA );

    // f(alp,b)
    valarray<double> fab( nY );
    model.dataMean( fab );
    DoubleMatrix dvecFab( fab, 1 );
    assert( dvecFab.nr() == nY );
    assert( dvecFab.nc() == 1 );

    // Calculate R_alp(alp,b) before R(alp,b) to allow caching.
    valarray<double> Rab_alp( nY * nY * nA );
    model.dataVariance_popPar( Rab_alp );
    DoubleMatrix dmatRab_alp( Rab_alp, nA );
    assert( dmatRab_alp.nr() == nY*nY );
    assert( dmatRab_alp.nc() == nA );

    // R(alp,b)
    valarray<double> Rab( nY * nY );
    model.dataVariance( Rab );
    DoubleMatrix dmatRab( Rab, nY );
    assert( dmatRab.nr() == nY );
    assert( dmatRab.nc() == nY );

    // invR(alp,b)
    assert( hasPosDet( dmatRab ) ); 
    valarray<double> RabInv( nY * nY );
    model.dataVarianceInv( RabInv );
    DoubleMatrix dmatRabInv( RabInv, nY );
    assert( dmatRabInv.nr() == nY );
    assert( dmatRabInv.nc() == nY );

    DoubleMatrix term1 = elsq_x( dvecY, dvecFab, dmatRab, dmatRabInv, dmatFab_alp, dmatRab_alp );

    if( withD )
    {
        // Calculate D_alp(alp) before D(alp) to allow caching.
        valarray<double> Da_alp( nB * nB * nA );
        model.indParVariance_popPar( Da_alp );
        DoubleMatrix dmatDa_alp( Da_alp, nA );
        assert( dmatDa_alp.nr() == nB*nB );
        assert( dmatDa_alp.nc() == nA );

        // D(alp)
        valarray<double> Da( nB * nB );
        model.indParVariance( Da );
        DoubleMatrix dmatDa( Da, nB );
        assert( dmatDa.nr() == nB );
        assert( dmatDa.nc() == nB );
        assert( hasPosDet(dmatDa) );
        
        valarray<double> DaInv( nB * nB );
        model.indParVarianceInv( DaInv );
        DoubleMatrix dmatDaInv( DaInv, nB );
        assert( dmatDaInv.nr() == nB );
        assert( dmatDaInv.nc() == nB );

        DoubleMatrix dvecZero( nB, 1 );
        dvecZero.fill(0.0);

        DoubleMatrix dmatZero_alp( nB, nA );
        dmatZero_alp.fill(0.0);
        
        DoubleMatrix term2 = elsq_x( dvecZero, dvecB, dmatDa, dmatDaInv, dmatZero_alp, dmatDa_alp );

        return add( term1, term2 );
    }

    /*
     * Case for dropping D term completely
     */
    else{

        return term1;
    }

}
/*************************************************************************
 *
 * Function: lambda_b (analytical derivative of lambda wrt b)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin lambda_b$$
$escape #$$
$spell
	Model model
    bool const alp iostream using namespace std NOREQ ind 
    dmat Da Fab Rab dvec nr nc pd cout endl placeholders palp
    distrib inv dinv Dinv pf pb Dinv int 
    covariances
    covariance
    Spk
    valarray
$$ 

$comment [Review - Mitch - 12/11/01] Change the section title to
"Derivative of the Individual Joint Negative Log-likelihood with Respect to b".$$
$comment [Respond - Sachiko - 12/17/01] Accepted.$$

$section Derivative of the Individual Joint Negative Log-likelihood with Respect to b$$

$index lambda_b$$
$cindex individual joint #negative log-likelihood$$

$table
$bold Prototype:$$   $cend  
$syntax/const DoubleMatrix lambda_b( 
    SpkModel &/model/,
    const DoubleMatrix &/y/,
    const DoubleMatrix &/alp/,
    const DoubleMatrix &/b/,
    bool  includeD
    )/$$
$tend

See also: $xref/lambda//lambda/$$, $xref/lambda_alp//lambda_alp/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Evaluates the partial true derivative of the negative log-likelihood of $math%y%$$ and $math%b%$$ 
with respect to the individual parameter.  
$comment [Review - Mitch - 12/11/01] Move this sentence to a 
"Return Value" section.$$
$comment [Respond - Sachiko - 12/17/01] Accepted.$$

$pre

$$
To be specific,
$math%
                 1 %          %                 1               T  -1
Lambda(alp, b) = - #logdet[ 2 #pi R(alp, b) ] + - [y - f(alp, b)] R (alp, b) [y - f(alp, b)]
                 2 %          %                 2

                 1 %          %                 1  T  -1
               + - #logdet[ 2 #pi D(alp) ]    + - b  D (alp)  b
                 2 %          %                 2
%$$
(The equation above uses
$xref/glossary/Individual Notation/individual notation/$$.)

$head Reference$$
$italic
Approximating The Maximum Likelihood Estimate For Models With Random Parameter
$$

$comment [Review - Mitch - 12/11/01] Add a "Return Value" section 
(see lambda() specification above) that discusses exceptions and
that describes the return value.$$
$comment [Respond - Sachiko - 12/17/01] Accepted.$$

$head Return Value$$
The resulting value is returned as a $math%n%$$ dimensional row vector, 
where $math%n%$$ is the size of $italic b$$.
If a failure occurs during the evaluation, an SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$comment [Review - Mitch - 12/11/01] Delete this sentence since it 
is no longer true.$$
$comment [Respond - Sachiko - 12/17/01] Accepted.$$

$syntax/
/model/
/$$
This function expects $italic model$$ to be 
$xref/glossary/Model Functions Depend on i - alp - b/a function of alp, b and i/$$.
$syntax/

/y/
/$$
is a $math%m%$$ dimensional column vector containing the data vector.
$syntax/

/alp/
/$$
The $math%n%$$ dimensional column vector $italic alp$$
specifies a value for the fixed population parameter vector.
$syntax/

/b/
/$$
The $math%n%$$ dimensional column vector $italic b$$
specifies a value for the random population parameter vector.
$syntax/

/includeD/
/$$
$comment [Review - Mitch - 12/11/01] This should be the same as the 
includeD description in the lambda() specification above.$$ 
$comment [Respond - Sachiko - 12/17/01] Accepted.$$

is a boolean flag indicating as to whether the terms involving D (the variance of individuals parameters)
are to be included in $math%Lambda%$$.
If false is given, the D terms will be completely eliminated from the calculation.

$head Example$$
Suppose that
$math%
                /  b(1)     0  \
    R(alp, b) = |              |
                \  0      b(1) /

                / alp(1)  0 \
    D(alp)    = |           |
                \ 0  alp(1) /

                / alp(2) + b(2) \
    f(alp, b) = |               |
                \ alp(2) + b(2) /

                / 1 \
    y         = |   |
                \ 1 /
%$$
It follows that
$math%                         
Lambda(alp, b) = (1/2) #log{[2 #pi b(1)]^2}   + [1 - alp(2) - b(2)]^2 / b(1)
               + (1/2) #log{[2 #pi alp(1)]^2} + (1/2) [b(1)^2 + b(2)^2] / alp(1)
%$$
The transpose of $math%Lambda_b(alp, b)%$$ is equal to
$math%
/ 1 / b(1) - [1 - alp(2) - b(2)]^2 / b(1)^2 + b(1) / alp(1) \
|                                                           |
\     - 2 [1 - alp(2) - b(2)] / b(1) + b(2) / alp(1)        /
%$$
If all the components of $math%alp%$$ and $math%b%$$ are one,
$math%
Lambda(alp, b)     = #log(2 #pi) + #log(2 #pi) + 1  + 1
                   = 2 #log(2 #pi) + 2

Lambda_alp(alp, b) = [ 1 - 1 , 2 ]
                   = [ 0 , 2 ]

Lambda_b(alp, b)   = [ 1 - 1 + 1 , 2 + 1]
                   = [ 1 , 3 ]

  
%$$

$head Coding Example$$
If you compile, link, and run the following program:

$codep

    #include <iostream>
    #include "DoubleMatrix.h"
    #include "lambda.h"
    #include "SpkModel.h"

    static DoubleMatrix getY();
    static DoubleMatrix getAlp();
    static DoubleMatrix getB();

    static DoubleMatrix funD(    const DoubleMatrix &alp );
    static DoubleMatrix funD_alp(const DoubleMatrix &dmatD, const DoubleMatrix &alp );
    static DoubleMatrix funF(    const DoubleMatrix &alp,   const DoubleMatrix &b );
    static DoubleMatrix funF_alp(const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b );
    static DoubleMatrix funF_b(  const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b );
    static DoubleMatrix funR(    const DoubleMatrix &alp,   const DoubleMatrix &b );
    static DoubleMatrix funR_alp(const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b );
    static DoubleMatrix funR_b(  const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b );

    class UserModel : public SpkModel
    {
        DoubleMatrix _a, _b;
        int _i;
    public:
        UserModel(){};  
        ~UserModel(){};
    protected:
        void doSelectIndividual(int i)
        {
            _i = i;
        }
        void doSetPopPar(const valarray<double>& a)
        {
            _a = DoubleMatrix( a, 1 );
        }
        void doSetIndPar(const valarray<double>& b)
        {
            _b = DoubleMatrix( b, 1 );
        }
        void doIndParVariance( valarray<double>& ret ) const
        {
            ret = funD(_a);
        }
        bool doIndParVariance_popPar( valarray<double>& ret ) const
        {
            doIndParVariance(ret);
            ret = funD_alp(ret, _a).toValarray();
            return !allZero(ret);
        }
        void doDataMean_popPar( valarray<double>& ret ) const
        {
            ret = funF(_a, _b).toValarray();
        }
        bool doDataMean_popPar( valarray<double>& ret ) const
        {
            doDataMean_popPar(ret);
            ret = funF_alp(ret, _a, _b).toValarray();
            return !allZero(ret);
        }
        bool doDataMean_indPar( valarray<double>& ret ) const
        {
            doDataMean_popPar(ret);
            ret = funF_b(ret, _a, _b).toValarray();
            return !allZero(ret);
        }
        void doDataVariance( valarray<double>& ret ) const
        {
            ret = funR(_a, _b).toValarray();
        }
        bool doDataVariance_popPar( valarray<double>& ret ) const
        {
            doDataVariance(ret);
            ret = funR_alp(ret, _a, _b).toValarray();
            return !allZero(ret);
        }
        bool doDataVariance_indPar( valarray<double>& ret ) const
        {
            doDataVariance(ret);
            ret = funR_b(ret, _a, _b).toValarray();
            return !allZero(ret);
        }   
        void doIndParVarianceInv( valarray<double>& ret ) const
        {
            doIndParVariance(ret);
            ret = getDinv(ret).toValarray();
            return !allZero(ret);
        }
        bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
        {
            doIndParVarianceInv(ret);
            ret = funDinv_alp(ret, _a).toValarray();
            return !allZero(ret);
        }
    };
    void main()
    {
        using namespace std;

        // initialize Model
        UserModel model;

        // to request all outputs, allocate memory to all placeholders
        double       lambdaOut;
        DoubleMatrix lambda_alpOut,
                     lambda_bOut;

        // initialize y (individual's data), alpha (fixed parameter), and b (random parameter)
        DoubleMatrix y     = getY(),
                     alp   = getAlp(),
                     b     = getB();

        // evaluate Lambda
        model.setPop(alp);
        lambdaOut = lambda( model, y, alp, b, true );
            cout << "Lambda - 2 log(2 pi) = " << lambdaOut - 2.0*log(2.0*PI) << endl;
        lambda_bOut = lambda_b( model, y, alp, b, true );
            cout << "Lambda_b = " << endl;
            lambda_bOut.print();
        }
        else{
            cout << "Something sent wrong during the evaluation." << endl;
        }
    }

    //
    // y = [1]
    //     [1]
    //
    static DoubleMatrix getY(){
        DoubleMatrix y(2,1);
        y.fill(1.0);
        return y;
    }

    //
    // alpha = [1]
    //         [1]
    ///
    static DoubleMatrix getAlp(){
        DoubleMatrix alp(2,1);
        alp.fill(1.0);
        return alp;
    }

    //
    // b = [1]
    //     [1]
    //
    static DoubleMatrix getB(){
        DoubleMatrix b(2,1);
        b.fill(1.0);
        return b;
    }

    //
    // D = [alp(0)   0   ]
    //     [   0   alp(0)]
    //
    static DoubleMatrix funD( const DoubleMatrix &alp ){
        DoubleMatrix dmatD(2,2);
        double *pD = dmatD.data(),
               *palp = alp.data();

        pD[0] = palp[0];
        pD[1] = 0.0;
        pD[2] = 0.0;
        pD[3] = palp[0];
        return dmatD;
    }

    //
    // D_alp = [ 1  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 1  0 ]
    //
    static DoubleMatrix funD_alp( const DoubleMatrix &dmatD, const DoubleMatrix &alp ){
        DoubleMatrix dmatD_alp( dmatD.nr()*dmatD.nc(), alp.nr() );
        double *pD_alp = dmatD_alp.data();
        dmatD_alp.fill(0.0);

        pD_alp[0] = 1.0;
        pD_alp[3] = 1.0;
        return dmatD_alp;
    }

    //
    // f(alpha, b) = [ alp(1) + b(1) ]
    //               [ alp(1) + b(1) ]
    //
    static DoubleMatrix funF( const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatF(2,1);
        double *palp = alp.data(),
               *pb   = b.data();

        dmatF.fill( palp[1] + pb[1] );
        return dmatF;
    }

    //
    // f_alpha = [ 0  1 ]
    //           [ 0  1 ]
    //
    static DoubleMatrix funF_alp( const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatF_alp(dvecF.nr(), alp.nr());
        double *pf_alp = dmatF_alp.data();

        pf_alp[0] = 0.0;
        pf_alp[1] = 0.0;
        pf_alp[2] = 1.0;
        pf_alp[3] = 1.0;
        return dmatF_alp;
    }

    //
    // f_b = [ 0  1 ]
    //       [ 0  1 ]
    //
    static DoubleMatrix funF_b( const DoubleMatrix &dvecF, const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatF_b(dvecF.nr(), b.nr());
        double *pf_b = dmatF_b.data();

        pf_b[0] = 0.0;
        pf_b[1] = 0.0;
        pf_b[2] = 1.0;
        pf_b[3] = 1.0;
        return dmatF_b;
    }

    //
    // R(alpha, b) = [ b(0)  0   ]
    //               [  0   b(0) ]
    //
    static DoubleMatrix funR(  const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatR(2,2);
        double *pR = dmatR.data(),
               *pb = b.data();

        pR[0] = pb[0];
        pR[1] = 0.0;
        pR[2] = 0.0;
        pR[3] = pb[0];
        return dmatR;
    }

    //
    // R_alp = [ 0  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //
    static DoubleMatrix funR_alp( const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatR_alp(dmatR.nr()*dmatR.nc(), alp.nr());
        dmatR_alp.fill(0.0);
        return dmatR_alp;
    }

    //
    // R_b   = [ 1  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 1  0 ]
    //
    static DoubleMatrix funR_b( const DoubleMatrix &dmatR, const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatR_b(dmatR.nr()*dmatR.nc(), b.nr());
        double *pR_b = dmatR_b.data();
        dmatR_b.fill(0.0);
        pR_b[0] = 1.0;
        pR_b[3] = 1.0;
        return dmatR_b;
    }
$$
The following results will be displayed.
$codep
    Lambda - 2 log(2 pi) = 2

    Lambda_b =
    [1, 3]

$$
$end
*/
// [Review - Mitch - 12/11/01] Once the functions logdet_alp and 
// weightedSumOfSquares_alp have been completed for the Covariance
// classes, then the body of this function should be rewritten in 
// a similar fashion as the body of the lambda function above.

const DoubleMatrix lambda_b( SpkModel &model, 
            const DoubleMatrix &dvecY, 
            const DoubleMatrix &dvecAlp, 
            const DoubleMatrix &dvecB,
            bool    withD
            )
{
    using namespace std;

    int nA = dvecAlp.nr();
    int nB = dvecB.nr();
    int nY = dvecY.nr();

    const valarray<double> alp = dvecAlp.toValarray();
    const valarray<double> b   = dvecB.toValarray();
    const valarray<double> y   = dvecY.toValarray();

    model.setPopPar(alp);
    model.setIndPar(b);

    // Calculate f_b(alp,b) before f(alp,b) to allow caching.
    valarray<double> fab_b;
    model.dataMean_indPar( fab_b );
    DoubleMatrix dmatFab_b( fab_b, nB );
    assert( dmatFab_b.nr() == nY );
    assert( dmatFab_b.nc() == nB );

    // f(alp,b)
    valarray<double> fab;
    model.dataMean( fab );
    DoubleMatrix dvecFab( fab, 1 );
    assert( dvecFab.nr() == nY );
    assert( dvecFab.nc() == 1  );

    // Calculate R_b(alp,b) before R(alp,b) to allow caching.
    valarray<double> Rab_b;
    model.dataVariance_indPar( Rab_b );
    DoubleMatrix dmatRab_b( Rab_b, nB );
    assert( dmatRab_b.nr() == nY*nY );
    assert( dmatRab_b.nc() == nB );

    // R(alp,b)
    valarray<double> Rab;
    model.dataVariance( Rab );
    DoubleMatrix dmatRab( Rab, nY );
    assert( dmatRab.nr() == nY );
    assert( dmatRab.nc() == nY );

    // invR(alp,b)
    assert( hasPosDet( dmatRab ) );
    valarray<double> RabInv;
    model.dataVarianceInv( RabInv );
    DoubleMatrix dmatRabInv( RabInv, nY );
    assert( dmatRabInv.nr() == nY );
    assert( dmatRabInv.nc() == nY );
    
    DoubleMatrix term1 = elsq_x( dvecY, dvecFab, dmatRab, dmatRabInv, dmatFab_b, dmatRab_b );

    if( withD ){
        // D(alp)
        valarray<double> Da;
        model.indParVariance( Da );
        DoubleMatrix dmatDa( Da, nB );
        assert( dmatDa.nr() == nB );
        assert( dmatDa.nc() == nB );

        assert( hasPosDet(dmatDa) );
        valarray<double> DaInv;
        model.indParVarianceInv( DaInv );
        DoubleMatrix dmatDaInv( DaInv, nB );
        assert( dmatDaInv.nr() == nB );
        assert( dmatDaInv.nc() == nB );

        DoubleMatrix term2 = multiply(transpose(dvecB), dmatDaInv);
        return add( term1, term2 );    
    }

    /*
     * Case for dropping D term completely
     */
    else{
        return term1;

        //return elsq_x( dvecY, dvecFab, dmatRab, dmatRabInv, dmatFab_b, dmatRab_b );
    }

}

/*************************************************************************
 *
 * Class: Lambda
 *
 *************************************************************************/
/*
$begin lambdaFuncOb$$
$spell
	Model model  
    dmat dtemp iostream std namespace pow approx ind int pop cout endl
    nc nr const bool pd centdiff ob typedef inv arg res
    dvec D_a pdb pda Dinv
    cfloat cmath spk fs aval bval inx covariances
    valarray
    
$$
$section Function Objects for lambda(alp,b) and Its True Derivatives$$

$index function objects, lambda$$
$index lambda, function object$$

$table
$bold Lambda:$$     $cend 
$syntax/template <class /Model/> class Lambda 
: public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>/$$ $rend
Constructor:        $cend
$syntax/Lambda</Model/>::Lambda(Model* /model/, const DoubleMatrix& /y/, bool /includeD/)/$$ $rend

$bold Lambda_alp:$$ $cend
$syntax/template <class /Model/> class Lambda_alp 
: public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>/$$ $rend
Constructor:        $cend
$syntax/Lambda</Model/>::Lambda_alp(Model* /model/, const DoubleMatrix& /y/, bool /includeD/)/$$ $rend

$bold Lambda_b:$$   $cend
$syntax/template <class /Model/> class Lambda_b 
: public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>/$$ $rend
Constructor:        $cend
$syntax/Lambda</Model/>::Lambda_b(Model* /model/, const DoubleMatrix& /y/, bool /includeD/)/$$
$tend

$fend 30$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code Lambda$$, $code Lambda_alp$$ and $code Lambda_b$$ classes wrap $xref/lambda//lambda()/$$, 
$xref/lambda_alp//lambda_alp()/$$ and $xref/lambda_b//lambda_b()/$$ functions, respectively, 
and allows user to call/evaluate the functions through $code operator()$$
in the form of a binary function.

$head Arguments$$
$syntax/
/Model/
/$$
$comment [Review - Mitch - 12/11/01] This should be the same as the 
model description in the lambda() specification above.$$
is the name of a user-provided model derived from SpkModel base class.
$syntax/

/model/
/$$
is a pointer to a population level user-provided model.
$syntax/

/y/
/$$
is a column vector containing measurements.
$syntax/

/includeD/
/$$
$comment [Review - Mitch - 12/11/01] This should be the same as the 
includeD description in the lambda() specification above.$$ 
should be true if you have implemented $code doIndParVariance()$$ in your user-provided model.

$head Public Members$$
$syntax/const DoubleMatrix operator(const DoubleMatrix /x1/, const DoubleMatrix /x2/) const
/$$
This member function evaluates the function associate with the object; $xref/lambda//lambda()/$$,
$xref/lambda_alp//lambda_alp/$$ or $xref/lambda_b//lambda_b()/$$
at the evaluation points specified by the $italic x1$$ and $italic x2$$.
It returns a n dimensional row vector, where n is the size of $italic x1$$ or $italic x2$$.

$head Example$$

The following piece of code demonstrates how to pass a function object to centdiff() algorithm as an example:

$codep
    #include <iostream>
    #include <cfloat>
    #include <cmath>

    #include "SpkValarray.h"
    #include "DoubleMatrix.h"
    #include "centdiff.h"
    #include "lambda.h"
    #include "inverse.h"
    #include "allZero.h"
    #include "SpkValarray.h"

    class LambdaCentdiffTestModel : public SpkModel
    {
        DoubleMatrix _a, _b;
        int _i;
    public:
        LambdaCentdiffTestModel(){};    
        ~LambdaCentdiffTestModel(){};
    protected:
        void doSelectIndividual(int inx)
        {
            _i = inx;
        }
        void doSetPopPar(const valarray<double>& a)
        {
            _a = DoubleMatrix( a, 1 );
        }
        void doSetIndPar(const valarray<double>& b)
        {
            _b = DoubleMatrix( b, 1 );
        }

        void doIndParVariance( valarray<double>& ret ) const
        {
            //
            // D = [a(0)   0   ]
            //     [   0   a(0)]
            //
            DoubleMatrix dmatD(2,2);
            double *pdD = dmatD.data();
            const double *pda = _a.data();

            pdD[0] = pda[0];
            pdD[1] = 0.0;
            pdD[2] = 0.0;
            pdD[3] = pda[0];
            ret = dmatD.toValarray();
        }
        bool doIndParVariance_popPar( valarray<double>& ret ) const
        {
            //
            // D(a)a = [ 1  0 ]
            //         [ 0  0 ]
            //         [ 0  0 ]
            //         [ 1  0 ]
            //
            DoubleMatrix dmatD = doIndParVariance(ret);
            DoubleMatrix dmatD_a( dmatD.nr()*dmatD.nc(), _a.nr() );
            double *pdD_a = dmatD_a.data();
            dmatD_a.fill(0.0);

            pdD_a[0] = 1.0;
            pdD_a[3] = 1.0;
            ret = dmatD_a.toValarray();
        }
        void doIndParVarianceInv( valarray<double>& ret ) const
        {
            doIndParVariance(ret);
            ret = inverse( ret );
        }
        bool doIndParVarianceInv_a( valarray<double>& ret ) const
        {
            DoubleMatrix dmatD;
            Dinv(dmatD);
            DoubleMatrix dmatD_a( dmatD.nr()*dmatD.nc(), _a.nr() );
            double *pdD_a = dmatD_a.data();
            dmatD_a.fill(0.0);

            pdD_a[0] = 1.0;
            pdD_a[3] = 1.0;
            ret = dmatD_a.toValarray();

            return !allZero(ret);
        }
        void doDataMean_popPar( valarray<double>& ret ) const
        {
            //
            // f(a, b) = [ a(1) + b(1) ]
            //           [ a(1) + b(1) ]
            //

            DoubleMatrix dmatF(2,1);
            const double *pda = _a.data();
            const double *pdb = _b.data();

            dmatF.fill( pda[1] + pdb[1] );
            ret = dmatF.toValarray();
        }
        bool doDataMean_popPar( valarray<double>& ret ) const
        {
            //
            // f(a, b)_a = [ 0  1 ]
            //             [ 0  1 ]
            //
            DoubleMatrix dvecF = doDataMean_popPar();
            DoubleMatrix dmatF_a(dvecF.nr(), _a.nr());
            double *pdF_a = dmatF_a.data();

            pdF_a[0] = 0.0;
            pdF_a[1] = 0.0;
            pdF_a[2] = 1.0;
            pdF_a[3] = 1.0;
            ret = dmatF_a.toValarray();

            return !allZero(ret);

        }
        bool doDataMean_indPar( valarray<double>& ret ) const
        {
            //
            // f(a, b)_b = [ 0  1 ]
            //             [ 0  1 ]
            //
            DoubleMatrix dvecF;
            doDataMean_popPar(dvecF);
            DoubleMatrix dmatF_b(dvecF.nr(), _b.nr());
            double *pdF_b = dmatF_b.data();

            pdF_b[0] = 0.0;
            pdF_b[1] = 0.0;
            pdF_b[2] = 1.0;
            pdF_b[3] = 1.0;
            ret = dmatF_b.toValarray();

            return !allZero(ret);

        }
        void doDataVariance( valarray<double>& ret ) const
        {
            //
            // R(a, b) = [ b(0)  0   ]
            //           [  0   b(0) ]
            //
            DoubleMatrix dmatR(2,2);
            double *pdR = dmatR.data();
	        const double *pdb = _b.data();

            pdR[0] = pdb[0];
            pdR[1] = 0.0;
            pdR[2] = 0.0;
            pdR[3] = pdb[0];
            ret = dmatR.toValarray();
        }
        bool doDataVariance_popPar( valarray<double>& ret ) const
        {
            //
            // R_a = [ 0  0 ]
            //       [ 0  0 ]
            //       [ 0  0 ]
            //       [ 0  0 ]
            //
            valarray<double> zeros(0.0, 4 * 2);
            ret = zeros;
            return !allZero(ret);
        }
        bool doDataVariance_indPar( valarray<double>& ret ) const
        {
            //
            // R_b   = [ 1  0 ]
            //         [ 0  0 ]
            //         [ 0  0 ]
            //         [ 1  0 ]
            //
            DoubleMatrix dmatR;
            doDataVariance(ret);
            DoubleMatrix dmatR_b(dmatR.nr()*dmatR.nc(), _b.nr());
            double *pdR_b = dmatR_b.data();
            dmatR_b.fill(0.0);
            pdR_b[0] = 1.0;
            pdR_b[3] = 1.0;
            ret = dmatR_b.toValarray();

            return !allZero(ret);
        } 
        void doDataVarianceInv( valarray<double>& ret ) const
        {
            doDataVariance(ret);
            ret = inverse(ret).toValarray();
        }
        bool doDataVarianceInv_indPar( valarray<double>& ret ) const
        {
            DoubleMatrix dmatR;
            doDataVarianceInv(dmatR);
            DoubleMatrix dmatR_b(dmatR.nr()*dmatR.nc(), _b.nr());
            double *pdR_b = dmatR_b.data();
            dmatR_b.fill(0.0);
            pdR_b[0] = 1.0;
            pdR_b[3] = 1.0;
            ret = dmatR_b.toValarray();
            return !allZero(ret);
        }
        bool doDataVarianceInv_popPar( valarray<double>& ret ) const
        {
            DoubleMatrix dmatR;
            doDataVarianceInv(dmatR);
            DoubleMatrix dmatR_b(dmatR.nr()*dmatR.nc(), _b.nr());
            double *pdR_b = dmatR_b.data();
            dmatR_b.fill(0.0);
            pdR_b[0] = 1.0;
            pdR_b[3] = 1.0;
            ret = dmatR_b.toValarray();
            return !allZero(ret);
        }

    };

    void lambdaCentdiffTest()
    {
        using namespace std;

        // Use n=2 for all alp, b and y, though they could have all different sizes
        int n = 2;

        // measurement vector
        DoubleMatrix y(n,1);
        y.fill(1);

        // population parameter
        DoubleMatrix pop(n,1);
        pop.fill(1);
        double *pdPop     = pop.data();

        // individual parameter
        DoubleMatrix ind(n,1);
        ind.fill(1);
        double *pdInd     = ind.data();

        // place holder for approximation for lambda(pop)_pop
        DoubleMatrix approxLambda_popOut;
        DoubleMatrix exactLambda_popOut;


        // step size vectors
        DoubleMatrix popStep(n,1);
        DoubleMatrix indStep(n,1);
        double *pdPopStep = popStep.data();
        double *pdIndStep = indStep.data();
        double dtemp;

        for( int i=0; i<n; i++){

            // set the step values for the population parameter
            dtemp = pow( DBL_EPSILON / (2.0/6.0), 1/3.0) + pdAlp[i];
            pdPopStep[i] = 10.0 * ( dtemp - pdPop[i] );

            // set zeros to the individual parameters so that
            // it is fixed.
            pdIndStep[i] = 0;
        }

        typedef Lambda<LambdaCentdiffTestModel> LAMBDA_PROTO;
        LambdaCentdiffTestModel model;
        
        lambda(model,y,pop,ind,0,&exactLambda_popOut,0,true);


        // Creating a Lambda function object to pass it to centdiff() algorithm
        LAMBDA_PROTO lambdaOb(&model, y, true);
        DoubleMatrix zeros(ind.nr(), ind.nc());       zeros.fill(0);
        approxLambda_popOut = centdiff_alp<LAMBDA_PROTO>(lambdaOb, pop, ind, popStep);
        cout << "approx = " << approxLambda_popOut << endl;
        cout << "exact  = " << exactLambda_popOut  << endl;
    }

$$
the program will display;
$codep

    approx = 1 by 2
    [ -5.0849075472330428e-009 2.0000000000000000e+000 ]

    exact  = 1 by 2
    [ 0.0000000000000000e+000 2.0000000000000000e+000 ]


$$
$end
*/
/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

/*************************************************************************
 *
 * Function: hasPosDet
 *
 *
 * Returns true if the determinant of the matrix A is positive.
 *
 *************************************************************************/

static bool hasPosDet( const DoubleMatrix& dmatA )
{
  // Compute b and c such that det(A) = b * 2^c.
  double b;
  long int c;
  det( dmatA , &b, &c );
  return ( b > 0.0 );
}
