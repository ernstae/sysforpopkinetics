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
 * File: mapTilde.cpp
 *
 *
 * Definition of mapTilde which solves the first order necessary
 * conditions for a solution to minimization of map Baysian objective
 * function (mapObj).
 *
 * Author: Sachiko Honda
 * 
 * Reviewer: Jiaji Du
 *
 * Follow-up: Sachiko Honda, 02/19/2002
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: mapTilde
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin mapTilde$$
$escape #$$

$spell
  Model model
    Rval fval Bayesian ok exp mitr eps diag Diff Obj lsq
    iostream bool int dmat pd std namespace const dvec fout
    Binitial endl drow cout maxitr Bupper Blower Bstep
    cmath nr nc ctime pre Spk
    doSetIndPar int pf pb covariances
    optimizer
    Ri
    resize
    valarray
  Optimizer optimizer
$$

$section Solving The First Order Necessary Conditions for Map Bayesian Difference Equation$$

$cindex map bayesian difference equation$$
$index mapTilde$$
$index individual, map bayesian difference equation$$

$table
$bold Prototype:$$ $cend
$syntax/void mapTilde(
    SpkModel           & /model/,
    const DoubleMatrix & /y/,
    Optimizer          & /optimizer/,
    const DoubleMatrix & /bIn/, 
    const DoubleMatrix & /bLow/, 
    const DoubleMatrix & /bUp/, 
    DoubleMatrix       & /bOut/, 
    const DoubleMatrix & /bStep/,
    DoubleMatrix       & /normOut/, 
    DoubleMatrix       * /mapObj_bOut/, 
    DoubleMatrix       * /mapObj_b_bOut/,
    bool                 /withD/
    )/$$ $rend
$tend

$table
$bold See also: $$ $cend
$tref Optimizer$$  $rend
$cend
$tref mapObj$$     $rend
$cend
$tref mapObjDiff$$ $rend
$cend
$tref mapOpt$$     $rend
$tend

$fend 35$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Solves the first order
necessary conditions for a solution to the following problem:
$math%
    #minimize MapObj(b) #subject #to bLow #le b #le bUp
%$$
where $math%
            1 %          %            1          T    -1
MapObj(b) = - #logdet[ 2 #pi R(b) ] + - [y - f(b)] R(b) [y - f(b)]
            2 %          %            2

            1 %          %            1  T  -1
          + - #logdet[ 2 #pi D ]    + - b  D  b
            2 %          %            2
%$$
and the derivative of $math%MapObj(b)%$$
is replaced by its central difference approximation.
This is the same as determining a point where the 
approximate $xref/glossary/Projected Gradient/projected gradient/$$
is zero. The approximate projected gradient replaces the gradient
by its central difference approximate in the definition of
the approximate gradient.
(The equation above uses
$xref/glossary/Individual Notation/individual notation/$$.)

$head Reference$$
$italic
Approximating The Maximum Likelihood Estimate For Models With Random Parameter
$$

$head Return Value$$
When a solution is found within the boundaries, given the maximum # of iterations, 
the function returns normally and
set the given output value place holders to the result values (ones that are requested).
If a failure occurs during the evaluation or no solution is found
within the boundaries, a SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/model/
/$$
is a pointer to a SpkModel object that is a function of $math%b%$$ if $italic withD$$ is $math%false%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$)
or a function of $math%alp%$$ and $math%b%$$ if $italic withD$$ is $math%true%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on alp and b/$$).
for details.

$syntax/

/y/
/$$
is a $math%m%$$ dimensional column vector containing the 
individual's data.

$syntax/

/optimizer/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer used in the individual 
level optimization.
$syntax/

/bIn/
/$$
is a $math%n%$$ dimensional column vector that
specifies the initial value for $italic b$$
during the optimization procedure and satisfies:
   
     $math%bLow <= bIn <= bUp%$$.

This value should be close to the solution to
the first order conditions because no line searching is 
done by $code MapTilde$$. 
The routine $mref/MapOpt/$$ can be used to obtain
a value for $italic bIn$$ that is close to the solution.
$syntax/

/bLow/
/$$
is a $math%n%$$ dimensional column vector that
specifies the lower limit for $math%b%$$ 

  $comment [Review - Jiaji - 12/3/01] Delete "math%b%" and its dollar signs in the following line$$

  $comment [Accepted - Sachiko] $$

during the optimization procedure.
$syntax/

/bUp/
/$$
is a $math%n%$$ dimensional column vector that
specifies the upper limit for $italic b$$ 
during the optimization procedure.
$syntax/

/bOut/
/$$
will contain a $math%n%$$ dimensional column vector
containing
approximate solution for the first order conditions.
$syntax/

/bStep/
/$$
is a $math%n%$$ dimensional column vector that
specifies the step size used for approximation of
the derivative of $math%MapObj_b(b)%$$ which
is used in the first order condition.
This is not used if 

  $comment [Review - Jiaji - 12/3/01] Please check the folowing line.  What is
  the meaning of that "MapObj_b_bOut is true"?$$

  $comment [Accepted - Sachiko] It is an artifact from the Omatrix prototype counterpart.  
  It should have been non-null.$$

$italic MapObj_b_bOut$$ is NULL.
$syntax/

/normOut/
/$$
will contain the norm of the
projected approximate gradient
at each iteration of the optimizer.  The dimension of this column vector is
the same as the number of iterations.
$syntax/

* /mapObj_bOut/
/$$
will point to the 
approximate gradient corresponding to $italic bOut$$ 
if the pointer is set to point to a $math%n%$$ dimensional row vector.
If NULL is given, no change is made to this argument.
$syntax/

* /mapObj_b_bOut/
/$$
will point to the derivative of the 
approximate gradient if a $math%n by n%$$ matrix is given.  
If NULL is given, no change is made to this argument.

$syntax/

/withD/
/$$
When $italic withD$$ is specified $math%false%$$,
the system assumes that the prior distribution is provided by the user.
In such a case, $italic withD$$ is a function of $math%alp%$$ and $math%b%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on alp and b/$$).
Otherwise, $italic model$$ is a function of only $math%b%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$)

$head Example$$
If you compile, link, and run the following program:
$codep
    #include <iostream>
    #include "DoubleMatrix.h"
    #include "SpkModel.h"
    #include "mapTilde.h"
    #include "transpose.h"
    #include "allZero.h"
    #include "Optimizer.h"

    //------------------------------------------------------------------------
    // Local function declarations
    //------------------------------------------------------------------------
    static void setExampleInSpec(
        DoubleMatrix &dvecY,            // y
        double       &dEps,             // eps
        int          &iMaxitr,          // mitr
        DoubleMatrix &dvecBinitial,     // bIn
        DoubleMatrix &dvecBupper,       // bUp
        DoubleMatrix &dvecBlower,       // bLow
        DoubleMatrix &dvecBstep,        // bStep
        DoubleMatrix &dvecBout,         // bOut
        DoubleMatrix &dvecNormOut,      // normOut
        DoubleMatrix *drowMapObj_bOut,      // mapObj_bOut
        DoubleMatrix *dmatMapObj_b_bOut     // mapObj_b_bOut
        );

    //------------------------------------------------------------------------
    // User-provided Model declaration & definition
    //------------------------------------------------------------------------
    class UserModel : public SpkModel
    {
        valarray<double> _b;
    public:
        UserModel(){}; 
        ~UserModel(){};
    protected:
        void doSetIndPar(const valarray<double>& b)
        {
            _b = b;
        }
        void doDataMean( valarray<double>& f ) const
        {
          //
          // f(b) = [ b(2) ]
          //        [ b(2) ]
          //
          f.resize( 2 );
          f[0] = _b[1];
          f[1] = _b[1];
        }
        bool doDataMean_indPar( valarray<double>& f_b ) const
        {
          //
          // f(b)_b = [ 0  1 ]
          //          [ 0  1 ]
          //
          f_b.resize( 2 * 2 );
          f_b[0] = 0.0;
          f_b[1] = 0.0;
          f_b[2] = 1.0;
          f_b[3] = 1.0;
          return true;
        }
        void doDataVariance( valarray<double>& R ) const
        {
          //
          // R(b) = [ exp^b(1)    0     ]
          //        [    0     exp^b(1) ]
          //
          R.resize( 2 * 2 );
          R[0] = exp(_b[0]);
          R[1] = 0.0;
          R[2] = 0.0;
          R[3] = exp(_b[0]);
        }
        bool doDataVariance_indPar( valarray<double>& R_b ) const
        {
          //
          // R(b)_b = [ exp^b(1)    0    ]
          //          [    0        0    ]
          //          [    0        0    ]
          //          [ exp^b(1)    0    ]
          //
          R_b.resize( 2 * 2 * 2 );
          R_b = 0.0;
          R_b[0] = exp(_b[0]);
          R_b[3] = exp(_b[0]);
          return true;
        }   
        void D( valarray<double>& D ) const
        {
          //  D     = [ 1.0  0.0 ]
          //          [ 0.0  0.5 ]
          //
          D.resize(2 * 2);
          D[0] = 1.0;
          D[1] = 0.0;
          D[2] = 0.0;
          D[3] = 1/2;
        }
    };

    //------------------------------------------------------------------------
    //  Main test body definition
    //------------------------------------------------------------------------
    void main()
    {
        using namespace std;

        int   n = 2;
        int   m = n;
        DoubleMatrix dvecY(m,1);
        double       dEps    = -1;
        int          iMaxitr = -1;
        DoubleMatrix dvecBinitial(n,1);
        DoubleMatrix dvecBupper(n,1);
        DoubleMatrix dvecBlower(n,1);
        DoubleMatrix dvecBstep(n,1);
        DoubleMatrix dvecBout(n,1);
        DoubleMatrix dvecNormOut;
        DoubleMatrix drowMapObj_bOut(1,n);     // mapObj_bOut
        DoubleMatrix dmatMapObj_b_bOut(n,n);     // mapObj_b_bOut
    
        UserModel model;
    
        setExampleInSpec(
            dvecY, dEps, iMaxitr, dvecBinitial, dvecBupper, dvecBlower, dvecBstep,
            dvecBout, dvecNormOut, &drowMapObj_bOut, &dmatMapObj_b_bOut
            );

        Optimizer optimizer( dEps, iMaxitr, 0 );
        if( mapTilde( model,
            dvecY, optimizer, dvecBinitial, dvecBlower, dvecBupper, 
            dvecBout, dvecBstep, dvecNormOut, &drowMapObj_bOut, &dmatMapObj_b_bOut, true ) ){

            cout << "b initial = " << endl;
            dvecBinitial.print();
            cout << endl;
            cout << "b out' = " << endl;
            (transpose(dvecBout)).print();
            cout << endl;
            cout << "norm out' = " << endl;
            (transpose(dvecNormOut)).print();
            cout << endl;
            cout << "mapObj_b out = " << endl;
            drowMapObj_bOut.print();
            cout << endl;
            cout << "mapObj_b_b out = " << endl;
            dmatMapObj_b_bOut.print();
        }
    }

    //------------------------------------------------------------------------
    //  Local function definitions
    //------------------------------------------------------------------------
    //
    // Function: setExampleInSpec
    //
    // 
    // Description
    // -----------
    //
    // Set parameters to values described in the O-matrix prototype's mapTilde.mat.
    //
    //  y     = [ 2.0 ]
    //          [ 2.0 ]
    //
    //  eps   = 1.0e-3
    //
    //  mitr  = 40
    //
    //  bIn   = [ 1.2 ]
    //          [ 0.8 ]
    //
    //  bUp   = [ 4.0 ]
    //          [ 4.0 ]
    //
    //  bLow  = [ -4.0 ]
    //          [ -4.0 ]
    //
    //  bStep = [ 0.05 ]
    //          [ 0.05 ]
    //
    //  initialize bOut
    //  initialize mapObj_bOut
    //  initialize mapObj_b_bOut
    //

    static void setExampleInSpec(
        DoubleMatrix &dvecY,            // y
        double       &dEps,             // eps
        int          &iMaxitr,          // mitr
        DoubleMatrix &dvecBinitial,     // bIn
        DoubleMatrix &dvecBupper,       // bUp
        DoubleMatrix &dvecBlower,       // bLow
        DoubleMatrix &dvecBstep,        // bStep
        DoubleMatrix &dvecBout,         // bOut
        DoubleMatrix &dvecNormOut,      // normOut
        DoubleMatrix &drowMapObj_bOut,  // mapObj_bOut
        DoubleMatrix &dmatMapObj_b_bOut // mapObj_b_bOut
        )
    {
        //
        //  y     = [ 2.0 ]
        //          [ 2.0 ]
        //
        dvecY.fill(2.0);
    
        //
        //  eps   = 1.0e-3
        //
        dEps = 1.0e-3;

        //
        //  mitr  = 40
        //
        iMaxitr = 40;

        //
        //  bIn   = [ 1.2 ]
        //          [ 0.8 ]
        //
        double *pBinitial = dvecBinitial.data();
        pBinitial[0] = 1.2;
        pBinitial[1] = 0.8;
    
        //
        //  bUp   = [ 4.0 ]
        //          [ 4.0 ]
        //
        dvecBupper.fill(4.0);
    
        //
        //  bLow  = [ -4.0 ]
        //          [ -4.0 ]
        //
        dvecBlower.fill(-4.0);

        //
        //  bStep = [ 0.05 ]
        //          [ 0.05 ]
        //
        dvecBstep.fill(0.05);
    
        //
        //  allocate bOut
        //
        dvecBout.fill(-1);

        //
        // allocate mapObj_bOut
        //
        drowMapObj_bOut.fill(-1);

        //
        // allocate mapObj_b_bOut
        //
        dmatMapObj_b_bOut.fill(-1);
    
    }


$$
then it will display the following when it is run:
$codep

    b initial =
    [1.2]
    [0.8]

    b out' =
    [0.000416752, 0.999792]

    norm out' =
    [1.97192, 1.00109, 0.044063, 0.000656466]

    mapObj_b out =
    [0.000637151, 0.000158068]

    mapObj_b_b out =
    [2.00034, 1.99978]
    [1.99895, 3.99805]

$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Revisit-Sachiko:
 *
 * Efficienty is not considered here.  To match this routine's appearance
 * to the original O-matrix prototype, I made a bunch of static routines
 * that act like wrappers; for instance op+ to perform add(a,b).
 *
 * This routine may run more or less iterations than the O-matrix's 
 * counterpart does.  It is, to my best knowledge, because A \ B returns
 * slightly different values due to floating point precision.
 * This can result in different true/false evaluations.  Final answers
 * are so far matching the results from O-matrix couterpart.
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <cfloat>
#include "mapTilde.h"
#include "mapObjDiff.h"
#include "backDiv.h"
#include "allTrue.h"
#include "inxToMax.h"
#include "norm.h"
#include "getMulCols.h"
#include "getMulRows.h"
#include "getSubblock.h"
#include "isLessThanOrEqualTo.h"
#include "isGreaterThanOrEqualTo.h"
#include "DBL_EPS_EQUAL_MULT.h"
#include "transpose.h"
#include "mulByScalar.h"
#include "divByScalar.h"
#include "subtract.h"
#include "add.h"
#include "DBL_EPS_EQUAL_MULT.h"
#include "inxToMax.h"

#include "matmax.h"
#include "matabs.h"
#include "isDmatEpsEqual.h"
#include "elementwiseAnd.h"
#include "elementwiseOr.h"
#include "SpkModel.h"
#include "DoubleMatrix.h"
#include "SpkException.h"
#include "placeRows.h"

namespace dbl_true_false{
    // Define constants
    const double dTRUE  = (double)true;
    const double dFALSE = (double)false;
}
/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/
static DoubleMatrix addScalarOnTail( const DoubleMatrix &dmatA, double b );

static DoubleMatrix operator!( const DoubleMatrix &A );
static DoubleMatrix operator==(const DoubleMatrix &A, const DoubleMatrix &B );
static DoubleMatrix operator!=(const DoubleMatrix &A, const DoubleMatrix &B );

/*************************************************************************
 *
 * Function: isActive
 *
 * 
 * Description
 * -----------
 *
 * Check if the cuurent b value is on the lower bound AND decreasing
 * or on the upper bound AND its value is increasing.
 *
 *      \  |      |  /
 *        \|      |/
 *         |\    /|
 *         |  \/  |
 *
 * When either case is true, it implies the b value is said to be active.
 *
 * Comment by Sachiko: Inlined on 02/19/2002 as suggested by Jiaji.
 * It had spent 0.1% of total running time and remained the same after
 * the modification, with ncomp test.
 *
 *************************************************************************/

static inline DoubleMatrix isActive( 
    const DoubleMatrix &dvecG,
    const DoubleMatrix &dvecLower,
    const DoubleMatrix &dvecUpper,
    const DoubleMatrix &dvecX
    )
{
    const int nX = dvecX.nr();
    assert( dvecG.nr()     == nX );
    assert( dvecLower.nr() == nX );
    assert( dvecUpper.nr() == nX );
    assert( dvecG.nc()     == 1 );
    assert( dvecLower.nc() == 1 );
    assert( dvecUpper.nc() == 1 );
    assert( dvecX.nc()     == 1 );

    DoubleMatrix dvecActive(nX,1);
    
    const double * x     = dvecX.data();
    const double * lower = dvecLower.data();
    const double * upper = dvecUpper.data();
    const double * g     = dvecG.data();
    double       * active= dvecActive.data();

    for( int i=0; i<nX; i++ )
    {
      active[i] = ( x[i] == lower[i] && g[i] >= 0.0 ) || ( x[i] == upper[i] && g[i] <= 0.0 );
    }

    return dvecActive;
}

/*========================================================================
 *
 *
 * Public Function Definition
 *
 *
 *========================================================================*/
#include <iomanip>
#include <cassert>
#include "SpkException.h"

static DoubleMatrix dvecCurB(__FILE__);
static DoubleMatrix active(__FILE__);
static DoubleMatrix notActive(__FILE__);
static DoubleMatrix dvecPosNewtonDir(__FILE__);
static DoubleMatrix isCurBonUpper(__FILE__);
static DoubleMatrix isCurBonLower(__FILE__);
static DoubleMatrix isFree(__FILE__);
static DoubleMatrix g(__FILE__);
static DoubleMatrix g_x(__FILE__);
static DoubleMatrix dvecCurBlower(__FILE__);
static DoubleMatrix dvecCurBupper(__FILE__);
static DoubleMatrix dvecTmpB(__FILE__);
static DoubleMatrix normSofar(__FILE__);
static DoubleMatrix dvecCurS(__FILE__);
static DoubleMatrix dvecCurL(__FILE__);
static DoubleMatrix dvecCurU(__FILE__);
static DoubleMatrix dvecCurBboth(__FILE__);
static DoubleMatrix drowCurMapObj_bOut(__FILE__);
static DoubleMatrix dmatCurMapObj_b_bOut(__FILE__);
static DoubleMatrix dvecPosNewtonDirTemp(__FILE__);
static DoubleMatrix dvecNegNewtonDir(__FILE__);
static DoubleMatrix tmp2(__FILE__);
static DoubleMatrix tmp3(__FILE__);

static DoubleMatrix dvecCurMapObj_bOut(__FILE__);
static DoubleMatrix drowNotActive(__FILE__);

void mapTilde(
    SpkModel &model,
    const DoubleMatrix &dvecY,
    Optimizer& optimizer,
    const DoubleMatrix &dvecBinitial, 
    const DoubleMatrix &dvecBlower, 
    const DoubleMatrix &dvecBupper, 
    DoubleMatrix &dvecBout, 
    const DoubleMatrix &dvecBstep,
    DoubleMatrix &dvecNormOut, 
    DoubleMatrix *drowMapObj_bOut, 
    DoubleMatrix *dmatMapObj_b_bOut,
    bool  withD
    )
{
    using namespace std;
    using namespace dbl_true_false;

    // Check assumptions.  If any assumption fails, the routine returns false immediately.
    int nB = dvecBinitial.nr();
    int nY = dvecY.nr();

    assert( dvecY.nc()        == 1 );
    assert( dvecBinitial.nc() == 1 );
    assert( dvecBlower.nc()   == 1 );
    assert( dvecBupper.nc()   == 1 );
    assert( dvecBstep.nc()    == 1 );

    assert( dvecBlower.nr()   == nB );
    assert( dvecBupper.nr()   == nB );
    assert( dvecBstep.nr()    == nB );

    assert( optimizer.getEpsilon() >  0.0 );
    assert( optimizer.getNMaxIter() >= 0 );
    assert( allTrue(dvecBstep >= 0.0) );
    assert( allTrue( bband((dvecBupper >= dvecBinitial), (dvecBinitial >= dvecBlower))));


    // Check output value place holders' sizes
    assert( dvecBout.nr()           == nB );
    assert( dvecBout.nc()           == 1  );
    assert( dvecNormOut.nr()        == nB );
    if(drowMapObj_bOut)
    {
        assert( drowMapObj_bOut->nr()   == 1 );
        assert( drowMapObj_bOut->nc()   == nB );
    }
    if(dmatMapObj_b_bOut)
    {
        assert( dmatMapObj_b_bOut->nr() == nB );
        assert( dmatMapObj_b_bOut->nc() == nB );
    }

    

    // Initialize boolean flags
    active.resize(nB,1);       active.fill( dFALSE );
    notActive.resize(nB,1); notActive.fill( dTRUE );

//    bool change   = false;
    bool converged = false;

    isCurBonUpper.resize(nB,1);
    isCurBonLower.resize(nB,1);
    isFree.resize(nB,1);

    normSofar.resize(1,1);
    dvecCurBboth.resize(nB,1);
    drowCurMapObj_bOut.resize(1,nB);
    dmatCurMapObj_b_bOut.resize(nB,nB);
    dvecPosNewtonDirTemp.resize( nB, 1 );
    dvecPosNewtonDir.resize( nB, 1 );
    dvecNegNewtonDir.resize( nB, 1 );
    
    double dNewtonDirectionCurStep;
    const double *pdBlower   = dvecBlower.data();
    const double *pdBupper   = dvecBupper.data();
    
    double *pdActive   = active.data();
    double *pdCurBboth = dvecCurBboth.data();
    double *pdNewtonDirection;
    int iCurMin, i, rowIndex;

    // Initialize # of iterations it goes through
    int iMaxitr = optimizer.getNMaxIter();
    double dEps = optimizer.getEpsilon();

    // Initialize a nB by 1 dvecCurB, where a temporary current b value is stored.
    dvecCurB.resize(nB,1);
    dvecCurB = dvecBinitial;
    double *pdCurB = dvecCurB.data();

    // Intialize a nB by 1 column vector dvecPosNewtonDir
    dvecPosNewtonDir.fill(1.0);

    bool change;
    for( int itr=0; itr<=iMaxitr; itr++ )
    {
        //
        // Evaluate the approximate gradient (mapObj_b) 
        // and corresponding Hessian (mapObj_b_b) with the current b value
        //
        mapObjDiff(model, dvecY, dvecBstep, dvecCurB, 
                    &drowCurMapObj_bOut, &dmatCurMapObj_b_bOut, withD, /* isFO = */ false);

        // 
        // Initialize the active constraints
        //
        // A derivative value is active if it is on the lower boundary and decreasing
        // or if it's on the upper boundary and increasing.
        // 
        // active[i] = ( x[i] == bLower[i] && g[i] >= 0.0 ) || ( x[i] == bUpper[i] && g[i] <= 0.0 )
        //
        // The left hand side vector, active, will contain true/false values in which
        // a TRUE indicates the corresponding g(x)[i] is active.
        //
        transpose(drowCurMapObj_bOut, dvecCurMapObj_bOut);    // the transpose of mapObj_b is a column vector.
        active = isActive(dvecCurMapObj_bOut, dvecBlower, dvecBupper, dvecCurB);

        // determine a proper active set
        change = true;
        if( iMaxitr == 0 )
        {
            //
            // If the user requested "0" iterations, don't bother looking for a solution.
            //
            dvecPosNewtonDir.fill(1.0);
            change = false;
        }

        // look for a solution
        while(change)
        {
            //
            // Get a vector containing true/false values for which a true indicates
            // NOT active.
            //
            notActive = !active;
            transpose(notActive, drowNotActive);

            //
            // Initialize a vector for which elements indicate the direction of Newton's search.
            //
            dvecPosNewtonDir.fill(0.0);

            //
            // If at least one of the elements of the mapObj's gradient, mapObj_b, is NOT active,
            //  
            if( !allTrue(active) )
            {
                // project onto the active set
                //
                // Extracts elements that are NOT active from the gradient
                // and place them into a vector, g.
                //
                transpose(  getMulCols( drowCurMapObj_bOut, drowNotActive ), g );
                //
                // Extracts elements that are NOT active from the hessian
                // and place them into a matrix, g_x.
                //
                g_x = getMulCols( getMulRows( dmatCurMapObj_b_bOut, notActive), drowNotActive );

                // Determine the Newton direction: dvecPosNewtonDirTemp = g_x \ g
                //
                // Find a vector that projects -(g_x) back to g.
                //
                try
                {
                    dvecPosNewtonDirTemp =  backDiv(mulByScalar(g_x, -1.0), g);
                }
                catch( SpkException& e )
                {
                    throw e.push(
                        SpkError::SPK_OPT_ERR,
                        "Determination of the Newton direction failed.", 
                        __LINE__,
                        __FILE__);
                }
                catch( const std::exception& e )
                {
                    throw SpkException(e,
                        "A standard exception was thrown during the determination of the Newton direction.", 
                        __LINE__,
                        __FILE__);
                }
                catch( ... )
                {
                    throw SpkException(SpkError::SPK_UNKNOWN_OPT_ERR,
                        "An exception of unknown type was thrown during the determination of the Newton direction.", 
                        __LINE__,
                        __FILE__);
                }

                //
                // Set newtonDirection[i], where 0 <= i < nB, to some non-zero value
                // if g[i] is NOT active.  Otherwise, keep it zero.
                //
                placeRows(dvecPosNewtonDirTemp, dvecPosNewtonDir, notActive);
            }       
            //
            // Look for more active directions: in opposite directions.
            // 
            dvecNegNewtonDir = mulByScalar(dvecPosNewtonDir, -1.0);

            //
            // Find the lower and upper boundaries relative to the current value of b.
            //
            // curBLower[i] = 0 if g[i] is active and the current b is on the (absolute) lower boundary.
            //             != 0 if g[i] is not active and the current b is not on the boundary.
            //
            // curBUpper[i] = 0 if g[i] is active and the current b is on the (absolute) upper boundary.
            //             != 0 if g[i] is not active and the current b is not on the boundary.
            // 
            mulByScalar( (dvecCurB == dvecBlower), dvecNegNewtonDir, dvecCurBlower );
            mulByScalar( (dvecCurB == dvecBupper), dvecPosNewtonDir, dvecCurBupper );

            //
            // If either of the relative upper boundary or the relative
            // lower boundary values are greater than 0, 
            // and if the corresponding g[i] is active, 
            // then something is wrong.
            //
            // If g[i] is active and (curBLower[i] != 0 || curBUp[i] != 0),
            // there's something wrong with maybe the model?
            // If g[i] was not active but (curBLower[i] != 0 || curBUp[i] != 0),
            // then, g[i] is said to be active.
            //
	    int indexMax = -1;
            double amountMax = 0.;
            int i;
            double * pdCurBlower = dvecCurBlower.data();
            double * pdCurBupper = dvecCurBupper.data();

            // Need this assignment for unknown reason
            pdActive = active.data(); 
            for(i = 0; i <nB; i++)
            {     if( (pdActive[i]!=dTRUE) && (pdCurBlower[i]>amountMax) )
                  {      indexMax = i;
                         amountMax = pdCurBlower[i];
                  }
                  if( (pdActive[i]!=dTRUE) && (pdCurBupper[i]>amountMax) )
                  {      indexMax = i;
                         amountMax = pdCurBupper[i];
                  }
            }
            change = indexMax >= 0;
            if( change )
            {
		pdActive[indexMax] = dTRUE;
            }
        }
        // determine the maximum allowable step size
        dNewtonDirectionCurStep = 1.0;
        iCurMin  = 0;
        pdNewtonDirection = dvecPosNewtonDir.data();
        for( i=0; i<dvecPosNewtonDir.nr(); i++ )
        {

            if( ( pdNewtonDirection[i] < 0.0-DBL_EPSILON )
                && ((pdBlower[i] - pdCurB[i]) > (dNewtonDirectionCurStep * pdNewtonDirection[i])) )
            {

                dNewtonDirectionCurStep = (pdBlower[i] - pdCurB[i]) / pdNewtonDirection[i];

            }
            if( ( pdNewtonDirection[i] > 0.0+DBL_EPSILON )
                && ((pdBupper[i] - pdCurB[i]) < (dNewtonDirectionCurStep * pdNewtonDirection[i])) )
            {

                dNewtonDirectionCurStep = (pdBupper[i] - pdCurB[i]) / pdNewtonDirection[i];
            }

        }
        if( iMaxitr == 0 )
        {
            dNewtonDirectionCurStep = 0.0;
        }

        // take the step and ensure that constraining components
        // are exactly equal to limiting values
        dvecPosNewtonDirTemp.resize( dvecPosNewtonDir.nr(), dvecPosNewtonDir.nc());
        mulByScalar(dvecPosNewtonDir, (1.0 + DBL_EPSILON * DBL_EPS_EQUAL_MULT ) * dNewtonDirectionCurStep, dvecPosNewtonDirTemp);
        add(dvecCurB, dvecPosNewtonDirTemp, dvecTmpB);
        
        mulByScalar(dvecPosNewtonDir, dNewtonDirectionCurStep, dvecPosNewtonDirTemp );
        add( dvecCurB, dvecPosNewtonDirTemp, dvecCurB );

        isCurBonUpper = dvecTmpB >= dvecBupper;
        isCurBonLower = dvecTmpB <= dvecBlower;
        isFree        = !bbor(isCurBonUpper, isCurBonLower);

        mulByScalar(isFree, dvecCurB, dvecPosNewtonDirTemp);
        mulByScalar(isCurBonUpper, dvecBupper, tmp2);
        mulByScalar(isCurBonLower, dvecBlower, tmp3);
        add(add( dvecPosNewtonDirTemp, tmp2), tmp3, dvecCurB);

        // check for convergednce
        active   = isActive( dvecCurMapObj_bOut, dvecBlower, dvecBupper, dvecCurB );
        if( allTrue(active) )
        {
            converged = true;
        }
        else
        {
            transpose(notActive, drowNotActive);
            transpose(  getMulCols( drowCurMapObj_bOut,drowNotActive), g );
            g_x      = getMulCols( getMulRows( dmatCurMapObj_b_bOut, notActive), drowNotActive );
            dvecCurS = getMulRows(dvecBstep,  notActive);
            dvecCurL = getMulRows(dvecBlower, notActive);
            dvecCurU = getMulRows(dvecBupper, notActive);
            dvecPosNewtonDirTemp = matabs(backDiv(g_x, g));

            /* Revisit-Sachiko:
               May replace "epsilon" back to "delta" which was used to be used here
               for convergednce check.
               converged = allTrue( matabs(g) <= divByScalar( dvecCurS, 1.0/dDelta) )
             */
            converged = allTrue( matabs(g) <= divByScalar( dvecCurS, 1.0/dEps) )
            || allTrue( dvecPosNewtonDirTemp <= mulByScalar(subtract(dvecCurU,dvecCurL), dEps) );
            if( itr == 0 )
            {
                normSofar.fill(norm(g));
            }
            else
            {
                normSofar = addScalarOnTail( normSofar, norm(g) );
            }
        }
        // Trace...
	//cout << "Maptilde Objective = " << normSofar << endl;
        if( converged || (iMaxitr == 0) )
        {
            dvecBout               = dvecCurB;
            dvecNormOut            = normSofar;
            if( drowMapObj_bOut   != 0 )
                *drowMapObj_bOut   = drowCurMapObj_bOut;
            if( dmatMapObj_b_bOut != 0 )
                *dmatMapObj_b_bOut = dmatCurMapObj_b_bOut;
            return;
        }

    }

    if( !converged )
    {
        throw SpkException(
                SpkError::SPK_TOO_MANY_ITER, 
                "Solution of the individual level first order necessary conditions failed \nbecause the maximum number of iterations was performed without convergence.",
                __LINE__, 
                __FILE__);
    }

}
//
// [Review - Jiaji - 12/3/01] Suggeston:  Make local functions inline.
//
//
// [Rejected - Sachiko] Unfortunately, VC++ requires inlined code
// defined in a header file.
//
/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

/*************************************************************************
 *
 * Function: addScalarOnTail
 *
 * 
 * Description
 * -----------
 *
 * Given a n dimensional vector (column/row), it returns a n+1 dimensional
 * vector (column/row) that contains the specified scalar value 
 * at the end of the original vector.
 * If the original vector were a single element vector, it will add
 * the scalar in the way the new vector forms a column vector.
 *
 *************************************************************************/
static DoubleMatrix addScalarOnTail( const DoubleMatrix &dmatA, double b )
{
    assert( (dmatA.nc() == 1) || (dmatA.nr() == 1) );

    int n;
    const double *pdA = dmatA.data();

    if( dmatA.nc() == 1 ){
        DoubleMatrix dmatC(dmatA.nr()+1, 1);
        n = dmatA.nr();
        double *pdC = dmatC.data();

        for( int i=0; i<n; i++ ){
            pdC[i] = pdA[i];
        }

        pdC[n] = b;
        return dmatC;
    }
    else{

        DoubleMatrix dmatC(1, dmatA.nc()+1);
        n = dmatA.nc();
        double *pdC = dmatC.data();

        for( int i=0; i<n; i++ ){
            pdC[i] = pdA[i];
        }

        pdC[n] = b;
        return dmatC;
    }

}

 
/*
 --------------------------------------------------------------------------
    Wrappers for convenience and code readibility
 --------------------------------------------------------------------------
 */

static DoubleMatrix operator!(const DoubleMatrix &A){
    using namespace dbl_true_false;

    DoubleMatrix B(A.nr(), A.nc());
    const double *pdA = A.data();
    double *pdB = B.data();

    for( int i=0; i<A.nr()*A.nc(); i++ ){
        if( pdA[i] == dFALSE )
            pdB[i] = dTRUE;
        else
            pdB[i] = dFALSE;
    }
    return B;
}    

// Updated 2-5-01 Alyssa
// fixed for const correctness

static DoubleMatrix operator==(const DoubleMatrix &A, const DoubleMatrix &B){
    assert( A.nr() == B.nr() );
    assert( A.nc() == B.nc() );
    using namespace dbl_true_false;

    int m = A.nr();
    int n = A.nc();
    const double *pdA = A.data();
    const double *pdB = B.data();

    DoubleMatrix C(m,n);
    double *pdC = C.data();

    for( int i=0; i<A.nr()*A.nc(); i++ ){
        if( pdA[i] == pdB[i] ){
            pdC[i] = dTRUE;
        }
        else
            pdC[i] = dFALSE;
    }
    return C;
}
static DoubleMatrix operator!=(const DoubleMatrix &A, const DoubleMatrix &B)
{
    return !(A == B);
}

