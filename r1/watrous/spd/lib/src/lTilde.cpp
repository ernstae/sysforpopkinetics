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
 * File: lTilde.cpp
 *
 *                   ~
 * Implementation of L(alp, y), eq. 10 in AMC 119 (2001) 57-75
 * 
 * Reference: Section 6. The modified Laplace objective,
 * AMC 119 (2001) 57-75,
 * "Approximating the marginal likelihood estimate for models with random parameters"
 * by Bradley M. Bell
 *
 * Author: Sachiko Honda
 *
 * David: Do you want to update the reference to refer to the published paper?
 * AMC 119 (2001) 57-75
 *
 * Sachiko: Yes.  Done.
 *************************************************************************/


/*************************************************************************
 *
 * Function: lTilde
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin lTilde$$
$section The Parametric Population Objective Function$$

$spell
	Model model 
    const int iostream cout endl namespace bool num eps mitr laplace ok
    resize
    fo
    dmat nr nc dvec drow funf rab bi sub pd fab da iomanip std
    setiosflags ios setprecision st nd multi processing
    ind inv distrib cerr
    int pf pb covariances
    covariance
    Spk
    Ri
    valarray
    enum
    enumulator
	optimizer
	Optimizer optimizer
$$

$index lTilde$$
$cindex parametric population objective function$$

$table
$bold Headers::$$     $cend
lTilde.h              $rend
                      $cend
Objective.h $rend
$bold Prototype:$$    $cend  
$syntax/void lTilde(
        bool                 /isMultiProcessed/,
        SpkModel             &/model/,
        enum Objective       /whichObjective/,
        const   DoubleMatrix &/y_forAll/,
        const   DoubleMatrix &/num_data_forEach/,
        Optimizer            &/optimizer/,
        const DoubleMatrix   &/alp/,
        const DoubleMatrix   &/bLow/,
        const DoubleMatrix   &/bUp/,
        const DoubleMatrix   &/bStep/,
        const DoubleMatrix   &/bIn_forAll/,
        DoubleMatrix       * /bOut/,
        double             * /LTildeOut/,
        DoubleMatrix       * /drowLTilde_alpOut/,
        DoubleMatrix       * /dmatLambdaTilde_alpOut/
        )/$$
$tend

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Evaluates the modified Laplace as defined in the reference
$italic
Approximating The Maximum Likelihood Estimate For Models With Random Parameter
$$.

$head Return Value$$
Upon a successful completion, the function returns normally and
set the given output value place holders to the result values (ones that are requested).
If a failure occurs during the evaluation, a $xref/SpkException//exception/$$ may be
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
The return value of $code lTilde$$ is true if it succeeds and false otherwise.
$math%false%$$ indicates no guarantees on any output values.

$syntax/

/isMultiProcessed/
/$$
A boolean flag indicating a mode SPK would run.  
True indicates the parallel processing mode.  False indicates the
single-machine, single-process mode.

$syntax/

/model/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.
$pre

$$
When $italic whichObjective$$ is specified $code NAIVE_FIRST_ORDER$$, $italic model$$ must
be an object of $xref/NaiveFoModel//NaiveFoModel/$$ which is a subclass of $xref/SpkModel//SpkModel/$$.

$syntax/

/whichObjective/
/$$
This value indicates the type of objective to be computed.
All the objectives defined in the $xref/Objective//Objective/$$ enumulator
$bold except for $code FIRST_ORDER$$ $$ which specifies the version of FO
that treats a population problem as a big individual problem are legal.
$syntax/

/y_forAll/
/$$
A column vector that stores all subjects' data.  The dimension must equal to
the sum of all numbers in $italic num_data_forEach$$ vector.
$syntax/

/num_data_forEach/
/$$
A column vector that stores the number of each subject's data values.  The dimension
, therefore, must equal to the number of subjects in the population.
$math%
                                     T
   y_1 = [y(1), y(2), ... , y(N(1)) ]
%$$
$syntax/

/optimizer/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer used in the individual 
level optimization.

$syntax/

/alp/
/$$
A column vector $italic alp$$
specifies a value for the fixed population parameter vector.
$syntax/

/bLow/
/$$
A column vector $italic bLow$$
specifies the lower limit for the parameter vector 
$math%b%$$ during the optimization procedure.
All the elements of this vector must be less than or equal zero.
The dimension of $italic bLow$$ must equal to the row dimension of
$italic bIn_forAll$$.

$syntax/

/bUp/
/$$
A column vector $italic bUp$$
specifies the upper limit for the parameter vector 
$math%b%$$ during the optimization procedure.
All the elements of this vector must be greater than or equal zero.
The dimension of $italic bUp$$ must equal to the row dimension of
$italic bIn_forAll$$.
$syntax/

/bStep/
/$$
A column vector $italic bStep$$
specifies the step size used for approximating
the derivatives with respect to the random population parameters.
The dimension of $italic bStep$$ must equal to the row dimension of
$italic bIn_forAll$$.
$syntax/

/bIn_forAll/
/$$
The Laplace objective requires determination
of the optimal random population parameters that correspond to the 
current value of $italic alp$$.
The double-precision matrix $italic bIn$$
specifies the initial value for the search for
the optimal random population parameter vectors.
The $th i$$ column of $italic bIn$$ corresponds to the $th i$$ individual.
Each column of $italic bIn_forAll$$ corresponds to each patient's initial estimate.
Therefore, the column dimension is equal to the number of patients in the population and
the row dimension is equal to the number of random parameters.
$syntax/

/bOut/
/$$
the output value $italic bOut$$
is a matrix with the same type and dimension as $italic bIn$$.
When user passes a matrix with right dimensions, 
the $th i$$ column of the matrix will contain
the optimal random population parameters that correspond to the 
$th i$$ individual and the current value of $italic alp$$.
$pre

$$
If $italic bOut$$ is specified null, $italic bOut$$ remains null.
$syntax/

/LTildeOut/
/$$
is
the output value $italic LTildeOut$$ is a double-precision scalar that contains
the value of the objective function.
$pre

$$
If $italic LTildeOut$$ is specified null, no computation is performed and
$italic LTildeOut$$ remains null.
$syntax/

/drowLTilde_alpOut/
/$$
the output value $italic drowLTilde_alpOut$$ is a row vector that is equal to
the derivative of the objective function w.r.t. $math%alp%$$.
The dimension must be equal to the number of $math%alp%$$ parameters.
If null is specified, null is returned.
$pre

$$
If $italic drowLTilde_alpOut$$ is specified null, no computation is performed and
$italic LTilde_alpOut$$ remains null.
$syntax/

/dmatLambdaTilde_alpOut/
/$$
If $italic dmatLambdaTilde_alpOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ object pointed to by $italic dmatLambdaTilde_alpOut$$ 
must be declared in the function that calls this function, and its number of columns
must be equal to the number of individuals and its number of rows must be equal to 
the length of the population parameter vector $math%alp%$$.  If $italic 
dmatLambdaTilde_alpOut$$ is not $code NULL$$ and this function completed successfully, then 
the $code DoubleMatrix$$ object pointed to by $italic dmatLambdaTilde_alpOut$$ 
will contain the derivatives of $bold individual$$ objectives with respect to $math%alp%$$ 
evaluated at $italic alp$$.  Each column of the matrix is the derivative for that 
individual with the index being equal to the row number.  Otherwise, this function will 
not attempt to change the contents of the $code DoubleMatrix$$ object pointed to by 
$italic dmatLambdaTilde_alpOut$$.  The default value of $italic dmatLambdaTilde_alpOut$$ is 
$code NULL$$.

$head Example$$
Suppose that there are two subjects and for each subject
$math%
	               / 1    0 \
	R_i (alp, b) = |        |
	               \ 0    2 /

	               / b \
	f_i(alp, b)  = |   |
	               \ b /

	               / 1 \
	y_i          = |   |
	               \ 1 /

	D(alp)       = alp
%$$
It follows that
$math%                         
Lambda_i (alp, b) = (1/2) #log{8 #pi^2}   + (3/4)(1 - b)^2
                  + (1/2) #log{2 #pi alp} + (1/2) b^2 / alp
%$$
It follows that the Hessian of $math%Lambda_i (alp, b)%$$
with respect to $math%b%$$ is
$math%
	3 / 2 + 1 / alp
%$$
In addition, the optimal value of $math%b%$$ solves the equation
$math%
	0   = -2 (3/4)(1 - b) +  b / alp
	0   = 1 - b - 2 b / (3 alp)
	1   = [1 + 2 / (3 alp)] b
	b   = 3 alp / (3 alp + 2)

%$$

Note that in this example,
the model function is linear,
and the data variance does not depend on the random effects.
Thus the three objective functions (laplace, expected hessian, fo) 
actually have the same value.
$pre

$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include <iomanip>
    #include "SpkValarray.h"
    #include "Optimizer.h"
    #include "DoubleMatrix.h"
    #include "SpkModel.h"
    #include "lTilde.h"
    #include "Objective.h"

    class UserModel : public SpkModel
    {
        DoubleMatrix _alp, _b;
        int _i;
    public:
        UserModel(){}  
        ~UserModel(){}
    private:
        void doSelectIndividual(int i)
        {
            _i = i;
        }
        void doSetPopPar(const valarray<double>& alp)
        {
            //
            // In this example, alp is a single element vector.
            //
            // alp = [ alp(1) ]
            //
            assert( alp.size() == 1 );
            _alp = alp;
        }
        void doSetIndPar(const valarray<double>& b)
        {
            //
            // In this example, b is a single element vector.
            //
            // b = [ b(1) ]
            //
            assert( b.size() == 1 );
            _b = b;
        }
        void doIndParVariance( valarray<double>& DOut ) const
        {
            //
            // D(alp) = alp
            //        = [ alp(1) ]
            //
            DOut = _alp;

        }
        bool doIndParVariance_popPar( valarray<double>& D_alpOut ) const
        {
            //
            // D(alp)_alp = [ 1 ];
            //
            D_alpOut.resize(1);
            D_alpOut = 1.0;
            return true;
        }
        void doDataMean( valarray<double>& fOut ) const
        {
            //
            // f(alp,b) = [ b(1) ]
            //            [ b(1) ]
            //
            fOut.resize( 2 );
            fOut = _b[0];
        }
        bool doDataMean_popPar( valarray<double>& f_alpOut ) const
        {
            //
            // f(alp,b)_alp = [ 0 ]
            //                [ 0 ]
            f_alpOut.resize( 2 );
            f_alpOut = 0.0;
            return false;
        }
        bool doDataMean_indPar( valarray<double>& f_bOut ) const
        {
            //
            // f(alp,b)_b = [ 1 ]
            //              [ 1 ]
            f_bOut.resize( 2 );
            f_bOut = 1.0;
            return true;
        }
        void doDataVariance( valarray<double>& ROut ) const
        {
            //
            // R(alp,b) = [ 1  0 ]
            //            [ 0  2 ]
            //
            ROut.resize( 4 );
            ROut[0] = 1.0;
            ROut[1] = 0.0;
            ROut[2] = 0.0;
            ROut[3] = 2.0;
        }
        bool doDataVariance_popPar( valarray<double>& R_alpOut ) const
        {
            //
            // R(alp,b)_alp = [ 0 ]
            //                [ 0 ]
            //                [ 0 ]
            //                [ 0 ]
            R_alpOut.resize( 4 );
            R_alpOut = 0.0;
            return false;
        }
        bool doDataVariance_indPar( valarray<double>& R_bOut ) const
        {
            //
            // R(alp,b)_b   = [ 0 ]
            //                [ 0 ]
            //                [ 0 ]
            //                [ 0 ]
            R_bOut.resize( 4 );
            R_bOut = 0.0;
            return false;
        }   
    };

    void main()
    {
        using namespace std;

        cout << setiosflags(ios::scientific) << setprecision(15);

        cout << "lTildeTest begins (no message will be generated unless encounters an error." << endl;

        const int numIndividuals = 2;
        UserModel model;
    
        // set individuals' data to:
        //   [ 1 ]
        //   [ 1 ]
        //   [ 1 ]
        //   [ 1 ]
        DoubleMatrix y(4,1);
        y.fill(1);    

        // set the vector containing the number of parameters for each individual's data.
        //   [ 2 ]
        //   [ 2 ]
        // NOTE: this reads the first two elements of y are for the 1st patient and
        //       the next two elements (3-4) of y are for the 2nd patient.
        DoubleMatrix N(2,1);
        N.fill(2);

        // set up optimizer
        double eps = 1e-5;
        int mitr = 40;
        int level = 0;
        Optimizer optimizer(eps, mitr, level);

        // set alp to:
        //  [ 2.1 ]
        //
        DoubleMatrix alp(1,1);
        alp.fill(2.1);

        // set bIn to:
        //  [ 0 ]
        //  [ 0 ]
        DoubleMatrix bIn(1,2);
        bIn.fill(0);

        // set bLow to:
        //  [ -2 ]
        //
        DoubleMatrix bLow(1,1);
        bLow.fill(-2);

        // set bUp to:
        //  [ +2 ]
        //
        DoubleMatrix bUp(1,1);
        bUp.fill(2);

        // set bStep to:
        //  [ 1e-2 ]
        //
        DoubleMatrix bStep(1,1);
        bStep.fill(1e-2);

        // allocate bOut
        DoubleMatrix bOut(1,1);

        // allocate lTildeOut
        double lTildeOut = 0.0;

        // allocate lTilde_alpOut
        DoubleMatrix lTilde_alpOut(1,1);

        // set a flag in order to request the parallel processing.
        bool isMultiProcessed = true;

        try{
            lTilde(isMultiProcessed, model, MODIFIED_LAPLACE, y, N, optimizer,
                   alp, bLow, bUp, bStep, bIn, &bOut, &lTildeOut, &lTilde_alpOut);
        }
        catch(...)
        {
            cerr << "lTilde failed" << endl;
            abort();
        }

        cout << "bOut = " << endl;
        bOut.print();
        cout << endl;
        cout << "lTildeOut = " << lTilde << endl;
        cout << endl;
        cout << "lTilde_alpOut = " << endl;
        lTilde_alpOut.print();
        cout << endl;

        return;
    }


$$
then it will display the following when it is run:
$codep
Node is running.

bOut =
[7.590361445783129e-001, 7.590361445783129e-001]

lTildeOut = 6.153455430753459e+000

lTilde_alpOut =
[2.308027289881899e-001]
$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <string>
#include <cmath>
#include <cfloat>
#include <cassert>
#include <exception>

#include "DoubleMatrix.h"
#include "SpkModel.h"
#include "allTrue.h"
#include "isGreaterThanOrEqualTo.h"
#include "isLessThanOrEqualTo.h"
#include "elementwiseAnd.h"
#include "getCol.h"
#include "replaceJth.h"
#include "add.h"
#include "IndDataPackage.h"
#include "expectedHessian.h"
#include "SpkException.h"
#include "Objective.h"
#include "lTilde.h"
#include "PARALLEL_FILE_CONSTS.h"
#include "transpose.h"
#include "mulByScalar.h"

/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

// David: some of these same operations are declared locally in other files
// (such as estimateB).  Presumably much of these will be implemented by
// MTL.

static DoubleMatrix getElements( const DoubleMatrix &original, int offset, int howMany );
static DoubleMatrix & operator+=(DoubleMatrix &A, const DoubleMatrix &B);
static const IndOutputDataPackage indAnalysis(SpkModel &model, const IndInputDataPackage& inpack);

// Counts the population level iterations
static int cntPopItrs = 0;

// Maintains the previous values for future need
static double prevLogdetLambda2diff  = 0.0;
static double prevLambda             = 0.0;
static DoubleMatrix prevLambdaTilde_alp(__FILE__);
static DoubleMatrix prevLTilde_alp     (__FILE__);

// Objects to minimize memory allocation activities
static DoubleMatrix dvecY_i            (__FILE__); 
static DoubleMatrix dvecBin_i          (__FILE__);
static DoubleMatrix dvecBhat_i         (__FILE__);
static DoubleMatrix drowLTilde_alp_i   (__FILE__);
static DoubleMatrix drowLTilde_alp     (__FILE__);
static DoubleMatrix dmatBoutTemp       (__FILE__);
static DoubleMatrix dmatLambdaTilde_alp(__FILE__);

/*------------------------------------------------------------------------
 * Function definition (SINGLE MODE)
 *------------------------------------------------------------------------*/

void lTilde(
        SpkModel     &model,
        enum Objective whichObjective,
        const         DoubleMatrix &dvecY_forAll,  // all individuals' data
        const         DoubleMatrix &dvecNumsOfDataforEachSubject,
        Optimizer&                 optimizer,
        const         DoubleMatrix &dvecAlp,
        const         DoubleMatrix &dvecBlow,
        const         DoubleMatrix &dvecBup,
        const         DoubleMatrix &dvecBstep,
        const         DoubleMatrix &dmatBin_forAll,
        DoubleMatrix *dmatBout_forAll,
        double       *dLTildeOut,
        DoubleMatrix *drowLTilde_alpOut,
		DoubleMatrix *dmatLambdaTilde_alpOut
        )
{
    using namespace std;

    //
    // The version of FO method that treats a population problem as a big individual problem
    // shall never reach here.  It goes from fitPopulation() -> firstOrderOpt() -> mapOpt().
    // 
    assert( whichObjective != FIRST_ORDER );

    // check the sizes of vectors
    const int  num_alp          = dvecAlp.nr();
    const int  num_y            = dvecY_forAll.nr();
    const int  num_subjects     = dvecNumsOfDataforEachSubject.nr();
    const int  num_b            = dmatBin_forAll.nr();
    const bool isLTildeOut      = (dLTildeOut!=0? true : false);
    const bool isLTilde_alpOut  = (((drowLTilde_alpOut!=0)||(dmatLambdaTilde_alpOut!=0))? true : false);
    const bool isBOut           = (dmatBout_forAll!=0? true : false);


    // If no evaluation is requested, return immediately.
    if( !isLTildeOut && !isLTilde_alpOut && !isBOut )
        return;

    assert( num_alp > 0 );
    assert( num_b   > 0 );
    assert( num_y   > 0 );
    assert( num_subjects > 0 );

    assert( dmatBin_forAll.nc() == num_subjects );
    assert( dvecBlow.nr()       == num_b );
    assert( dvecBstep.nr()      == num_b );
    assert( dvecBup.nr()        == num_b );
    assert( dvecAlp.nc()        == 1 );
    assert( dvecBlow.nc()       == 1 );
    assert( dvecBup.nc()        == 1 );
    assert( dvecBstep.nc()      == 1 );
    assert( dvecY_forAll.nc()   == 1 );

    int i = 0;  // index to a subject
    const double *pdNy = dvecNumsOfDataforEachSubject.data();

#ifndef NDEBUG
    
    if(dmatBout_forAll!=0)
        assert(dmatBout_forAll->nr() == num_b);
    if(drowLTilde_alpOut!=0)
        assert(drowLTilde_alpOut->nc() == num_alp);

    // check the size of y
    int sum = 0;
    for( i=0; i<num_subjects; i++ )
    {
        sum += static_cast<int>( pdNy[i] );
    }
    assert( sum == num_y );
#endif

    // check the values
    assert( allTrue( bband((dvecBup >= 0.0), (dvecBlow <= 0.0) ) ) );
    assert( allTrue( dvecBstep >= 0.0 ) );
    assert( optimizer.getEpsilon() > 0 );
    assert( optimizer.getNMaxIter() >= 0 );
    assert( optimizer.getLevel() >= 0 );

    // Population level variables
    double       dLogdetLambda2diff   = 0.0;
    double       dLambda              = 0.0;
    drowLTilde_alp.resize(1,num_alp); 
    drowLTilde_alp.fill(0);
    
    dmatLambdaTilde_alp.resize(num_alp, num_subjects);

    // Individual level variables
    int          inx_yi0 = 0;       // index to the 0th element of y_i's data
    int          num_y_i;           // number of the i-th individual's measurement data
    double       dLambda_i            = 0.0;
    double       dLogdetLambda2diff_i = 0.0;

    // Temporary place holder to fool for when dmatBout_forAll were specified null.
    dmatBoutTemp.resize( num_b, num_subjects );

    const PopConstVals popconsts(num_subjects, optimizer, whichObjective, 
                                 dvecBlow, dvecBup, dvecBstep);
    const PopVars      popvars(dvecAlp, true, true, isLTilde_alpOut);

    // Place holders for results
    IndOutputDataPackage outpack;
    IndResults results;
	
    //                     ~                                    ^
    // Sum det(Lambda(alp, bi(alp,yi), yi)_bi_bi) + Lambda(alp, bi(alp,yi), yi) over all subjects
    for( i=0; i<num_subjects; i++ ){
    
        model.selectIndividual(i);

        if( optimizer.getLevel() > 0 )
        {
            cout << "<PopID: " << cntPopItrs << ">" << endl;
            cout << "<IndID: " << i << ">" << endl;
        }

        // Get the number of data values for the ith subject.
        num_y_i = static_cast<int>( pdNy[i] );
        // Get the data for this subject.
        dvecY_i = getElements( dvecY_forAll, inx_yi0, num_y_i );

        // Get the initial b for the ith subject.
        dvecBin_i = getCol(dmatBin_forAll, i);

        // Pack input information in data structures
        const IndVars initvals( i, dvecBin_i, dvecY_i );
        const IndInputDataPackage inpack(cntPopItrs, popconsts, popvars, initvals);

        try{
            outpack = indAnalysis(model, inpack);
        }
        catch( const SpkException& e )
        {         
            //
            // [ Comment by Sachiko - 11/20/02 ]
            //
            // If an individual's hessian turns out to be non positive definite,
            // restore the previous values and let the line search try 
            // a different direction.  This can be known to happen with laplace.
            //
            if(    e[ e.size()-1 ].code() == SpkError::SPK_NOT_POS_DEF_ERR 
                && cntPopItrs > 0 )
            {
                //
                // Print out a warning message to the standard error if
                // user has specified a print level greater than 4.
                //
                cerr << "Warning! Backed up the search direction for " << i << "th individual!" << endl;
                const double FACTOR = 1.0;
                if( isLTildeOut  )
                {
                    if( prevLogdetLambda2diff >= 0.0 )
                    {
                        *dLTildeOut = prevLogdetLambda2diff + prevLambda;
                    }
                    else
                    {
                        *dLTildeOut = prevLambda;
                    }
                    prevLambda *= FACTOR;
                }
			    if( drowLTilde_alpOut )
			    {
                    *drowLTilde_alpOut = prevLTilde_alp;
			    }
			    if( dmatLambdaTilde_alpOut )
			    {
                    *dmatLambdaTilde_alpOut = prevLambdaTilde_alp;
			    }
                ++cntPopItrs;
                return;
            }
            throw;
        }
        catch( const std::exception& stde )
        {
	  const int max = SpkError::maxMessageLen();
	  char message[max];
	  sprintf( message, "%d-th individual's analysis failed.\n", i );

            throw SpkException(
                stde,
                message,
                __LINE__,
                __FILE__
                );
        }
        catch( ... )
        {
            const int max = SpkError::maxMessageLen();
	    char message[max];
            sprintf( message, "%d-th individual's analysis failed.\n", i );

            throw SpkException(
                SpkError::SPK_UNKNOWN_ERR,
                message,
                __LINE__,
                __FILE__
                );
        }
        
        results = outpack.indResults;
        if( results.getIndex() != i )
        {
            throw SpkException(
                SpkError::SPK_UNKNOWN_ERR,
                "Invalid index to an individual.",
                __LINE__,
                __FILE__
                );
        }

        dvecBhat_i = results.getHat();
            assert(dvecBhat_i.nr()==num_b);
        drowLTilde_alp_i = results.getTilde_pop();
            assert(drowLTilde_alp_i.nc()==num_alp);
        dLambda_i  = results.getLambda();
        dLogdetLambda2diff_i = results.getLogdetLambda2diff();
        
        //
        // Accumulating bi in a matrix
        //
        // This function will terminate if *i* is out of range.
        replaceJth(dmatBoutTemp, i, dvecBhat_i);

        //                         ^
        // Sum[Lambda(alp, bi(alp,yi), yi)] over all subjects
        //
        dLambda += (dLTildeOut==0? 0.0 : dLambda_i);

        //                                      ~
        // Sum[0.5 * logdet(Lambda(alp, bi(alp,yi), yi)_bi_bi / 2PI)] over all subjects
        //
        dLogdetLambda2diff += dLogdetLambda2diff_i;

		if( drowLTilde_alpOut )
		{
            drowLTilde_alp += drowLTilde_alp_i;
		}
		if( dmatLambdaTilde_alpOut )
		{
            replaceJth( dmatLambdaTilde_alp, i, transpose( drowLTilde_alp_i ) );
		}

        // Update the index to y for the next subjects
        inx_yi0 += num_y_i;
    }
    
    //         ~
    // Compute L
    //
    if( dLTildeOut != 0 )
    {
        *dLTildeOut = dLogdetLambda2diff 
               + dLambda;
        prevLogdetLambda2diff = dLogdetLambda2diff;
        prevLambda            = dLambda;
    }

    if( dmatBout_forAll ){
        *dmatBout_forAll = dmatBoutTemp;
        assert(dmatBout_forAll->nr() == num_b);
    }

    //         ~
    // Compute L_alp
    //
    if( drowLTilde_alpOut != 0 ){
        *drowLTilde_alpOut = drowLTilde_alp;
        assert(drowLTilde_alpOut->nc() == num_alp);
        prevLTilde_alp        = drowLTilde_alp;
    }

    //         ~
	// Compute Lambda_alp_i
    //
	if( dmatLambdaTilde_alpOut != 0 ){
        *dmatLambdaTilde_alpOut = dmatLambdaTilde_alp;
        assert(dmatLambdaTilde_alpOut->nr() == num_alp);
		assert(dmatLambdaTilde_alpOut->nc() == num_subjects);
        prevLambdaTilde_alp   = dmatLambdaTilde_alp;
    }

    ++cntPopItrs;
    return;
}

/*------------------------------------------------------------------------
 * Function definition (MULTI-PROCESS)
 *------------------------------------------------------------------------*/
#include <fstream>
#include <deque>
#include "StatusList.h"
#include "Channel.h"
#include "File.h"
#include "System.h"

 /* David: Although this might not be the best place to put this 
comment, another optimization to do is to take advantage of 
"super sparsity" when calculating central differences.  By 
super sparsity,it is meant that the derivatives of the users
mean and covariance matrices are sparse relative to the 
mean and covariance itself, because we expect individual
components to only depend on a few of the parameters.

Analytic  differentiation could take advantage of this.  This is 
because the analytic derivatives of functions like Lambda have been 
worked out using the product rule in terms of derivatives of the 
users mean and covariance matrices.  Hence, when the derivative of 
Lambda is computed, the super sparse matrices that the user will 
provide for derivatives are used in the computation.  On the other 
hand, central differenceing is directly applied to Lambda, instead 
of first expanding the central difference with the product rule 
which would allow us to plug-in central differences of users 
covariance matrices, which we expect to be super sparse.  So the 
thought is to work out formulas for the central differences of 
functions in terms of central differences of the user model. 

Unfortunately, SPK currently has no way to detect super sparsity 
for the central differences of the users covariance matrices.  
This would require some additional virtual functions in the model 
class, making it an optional optimization.  
*/

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// [Revisit - Parallel Mode SPK Capabilites are Not Supported - Mitch]
// The parallel mode capabilites of SPK are not supported for 
// SPD so that there is no current session ID. 
//
// This variable (timestamp) represents the current session ID.
//
//extern time_t SESSION_ID;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

void lTilde(
        bool          isMultiProcessed,
        const File    &sharedDiskSpace,
        SpkModel      &model,
        enum Objective  whichObjective,
        const         DoubleMatrix &dvecY_forAll,  // all individuals' data
        const         DoubleMatrix &dvecNumsOfDataforEachSubject,
        Optimizer&    optimizer,
        const         DoubleMatrix &dvecAlp,
        const         DoubleMatrix &dvecBlow,
        const         DoubleMatrix &dvecBup,
        const         DoubleMatrix &dvecBstep,
        const         DoubleMatrix &dmatBin_forAll,
        DoubleMatrix *dmatBout_forAll,
        double       *dLTildeOut,
        DoubleMatrix *drowLTilde_alpOut        )
{
    using namespace std;
    using namespace parallel_const;

    if( !isMultiProcessed ){
        lTilde( model, whichObjective, dvecY_forAll, dvecNumsOfDataforEachSubject,
        optimizer,dvecAlp,dvecBlow,dvecBup,dvecBstep,dmatBin_forAll,
        dmatBout_forAll,dLTildeOut,drowLTilde_alpOut );
        return;
    }

    //
    // The version of FO method that treats a population problem as a big individual problem
    // shall never reach here.  It goes from fitPopulation() -> firstOrderOpt() -> mapOpt().
    // 
    assert( whichObjective != FIRST_ORDER );

    // If no evaluation is requested, return immediately.
    // check the sizes of vectors
    const int  num_alp          = dvecAlp.nr();
    const int  num_y            = dvecY_forAll.nr();
    const int  num_subjects     = dvecNumsOfDataforEachSubject.nr();
    const int  num_b            = dmatBin_forAll.nr();
    const bool isLTildeOut      = (dLTildeOut!=0? true : false);
    const bool isLTilde_alpOut  = (drowLTilde_alpOut!=0? true : false);
    const bool isBOut           = (dmatBout_forAll!=0? true : false);

    // If no evaluation is requested, return immediately.
    if( !isLTildeOut && !isLTilde_alpOut && !isBOut )
        return;

    assert( num_alp > 0 );
    assert( num_b   > 0 );
    assert( num_y   > 0 );
    assert( num_subjects > 0 );

    assert( dmatBin_forAll.nc() == num_subjects );
    assert( dvecBlow.nr()       == num_b );
    assert( dvecBstep.nr()      == num_b );
    assert( dvecBup.nr()        == num_b );
    assert( dvecAlp.nc()        == 1 );
    assert( dvecBlow.nc()       == 1 );
    assert( dvecBup.nc()        == 1 );
    assert( dvecBstep.nc()      == 1 );
    assert( dvecY_forAll.nc()   == 1 );
   
    int i, j;

    // check the size of y
    const double *pdNy = dvecNumsOfDataforEachSubject.data();
    int sum = 0;
    for( i=0; i<num_subjects; i++ )
    {
        sum += static_cast<int>( pdNy[i] );
    }
    assert( sum == num_y );

    // check the values
    assert( allTrue( bband((dvecBup >= 0.0), (dvecBlow <= 0.0) ) ) );
    assert( allTrue( dvecBstep >= 0.0 ) );
    assert( optimizer.getEpsilon() > 0 );
    assert( optimizer.getNMaxIter() >= 0 );
    assert( optimizer.getLevel() >= 0 );


    // Population level variables
    double       dLogdetLambda2diff   = 0.0;
    double       dLambda              = 0.0;

    // Individual level variables
    int          inx_yi0 = 0;       // index to the 0th element of y_i's data
    int          num_y_i;           // number of the i-th individual's measurement data
    double       dLambda_i            = 0.0;
    double       dLogdetLambda2diff_i = 0.0;

    // Temporary place holder to fool for when dmatBout_forAll were specified null.
    dmatBoutTemp.resize( num_b, num_subjects );

    drowLTilde_alp.resize(1,num_alp); 
    drowLTilde_alp.fill(0);
    
    // Packing up information for distribution
    PopConstVals popconsts(num_subjects, optimizer, whichObjective, dvecBlow, dvecBup, dvecBstep);
    PopVars      popvars(dvecAlp, true, true, isLTilde_alpOut);
    
    // Place holders for results
    IndOutputDataPackage outpack;
    IndResults results;

    // various counters used for reissuing an individual's estimate
    IndInputDataPackage inpack_toBeReIssued;
    //int toBeReIssued;

    // open the communication channel
    MasterEndChannel channel(sharedDiskSpace);
    channel.open();

    PackageHandle handle;

    //
    // Post a file uniquely identifying this current session.
    //
    // Revisit - Sachiko; This whole operation should be performed by a published function
    // so that the file name can be encapsulated and nodes can access it without
    // knowing the exact name.
    //
    if( cntPopItrs == 0 )
    {
      char session[50];
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // [Revisit - Parallel Mode SPK Capabilites are Not Supported - Mitch]
      // The parallel mode capabilites of SPK are not supported for 
      // SPD so that there is no current session ID. 
      //sprintf( session, "%d", SESSION_ID );
      sprintf( session, "%d", 0 );
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      channel.write("session.id", session);
    }

    //
    // Post a unique file telling nodes the current (population level) iteration number.
    // Nodes checks this number against the number embedded in a data set for an individual
    // before posting the result.  
    //
    // Revisit - Sachiko: This whole operation should be performed by a published function
    // so that the file name can be encapsulated and nodes can access it without
    // knowing the exact name.
    //
    char str_number[ 100 ];
    sprintf( str_number, "%d", cntPopItrs );
    channel.write("pop.itr", str_number);

    StatusList checklist(num_subjects);
    inx_yi0 = 0;
    for( int id=0; id<num_subjects; id++ )
    {
    
        // Get the number of data values for the ith subject.
        num_y_i = static_cast<int>( pdNy[id] );
        // Get the data for this subject.
        dvecY_i = getElements( dvecY_forAll, inx_yi0, num_y_i );

        // Get the initial b for the ith subject.
        dvecBin_i = getCol(dmatBin_forAll, id);

        IndVars initvals( id, dvecBin_i, dvecY_i );

        IndInputDataPackage inpack(cntPopItrs, popconsts, popvars, initvals);
        try{ 

          handle = channel.post(inpack, false);

        }
        catch( const SpkException& e )
        {
            //
            // [ Comment by Sachiko - 11/20/02 ]
            //
            // If an individual's hessian turns out to be non positive definite,
            // restore the previous values and let the line search try 
            // a different direction.  This can be known to happen with laplace.
            //
            if(    e[ e.size()-1 ].code() == SpkError::SPK_NOT_POS_DEF_ERR 
                && cntPopItrs > 0 )
            {
                //
                // Print out a warning message to the standard error if
                // user has specified a print level greater than 4.
                //
                if( optimizer.getLevel() > 0 )
                    cerr << "Warning! Backed up the search direction for " << i << "th individual!" << endl;
                const double FACTOR = 1.0;
                if( isLTildeOut  )
                {
                    if( prevLogdetLambda2diff >= 0.0 )
                    {
                        *dLTildeOut = prevLogdetLambda2diff + prevLambda;
                    }
                    else
                    {
                        *dLTildeOut = prevLambda;
                    }
                    prevLambda *= FACTOR;
                }
			    if( drowLTilde_alpOut )
			    {
                    *drowLTilde_alpOut = prevLTilde_alp;
			    }
                ++cntPopItrs;
                return;
            }
            throw;
        }
        catch( const std::exception& stde )
        {
	  const int max = SpkError::maxMessageLen();
	  char message[max];
          sprintf( message, "%s\n \
                             A standard:exception was thrown during an attempt to post %d-th individual's data.\n",
		   stde.what(), id );
          throw SpkException( SpkError::SPK_PARALLEL_ERR, message, __LINE__, __FILE__ );
        }
        catch( ... )
        {
	  const int max = SpkError::maxMessageLen();
	  char message[max];
          sprintf( message, "An unknonw exception was thrown during an attempt to post %d-th individual's data.\n",
		   id );
          throw SpkException( SpkError::SPK_PARALLEL_ERR, message, __LINE__, __FILE__ );
        }
        checklist.issued(id, handle, inpack, false);

        inx_yi0 += num_y_i;
    }

    //
    // Wait until ALL packages get picked up by nodes.
    // 
    for( j=0; j<num_subjects;)
    {
      for( i=0; i<num_subjects; i++ )
      {
        if( !checklist.isCheckedOut(i) )
        {
          if( channel.hasReached( checklist.getHandle(i) ) )
          {
            //
            // Update the time stamp to the time actually picked up by a node
            //
            checklist.checkedOut(i);
            ++j;
          }
        }
      }
    }
#ifdef _DEBUG
    for( i=0; i<num_subjects; i++ )
      assert( checklist.isCheckedOut(i) );
#endif

    //                     ~                                    ^
    // Sum det(Lambda(alp, bi(alp,yi), yi)_bi_bi) + Lambda(alp, bi(alp,yi), yi) over all subjects
    //
    //
    // At this point, it is absolutely true that all packages have been picked up by nodes.
    // Unless the number of participating nodes is equal to or greater than the number of
    // individuals to be analysized, there must be at least one result package ready
    // to be picked up.
    //
    IndInputDataPackage inPack;
    inx_yi0 = 0;
    for( int cnt=0; cnt<num_subjects; ++cnt )
    {
        do{
            try{
                outpack = channel.get(false);
            }
            catch( SpkException& e )
            {
	      const int max = SpkError::maxMessageLen();
	      char mess[max];
              sprintf( mess, "An failure occured while an attempt to obtain an individual analysis result.\n" );
              throw e.push(SpkError::SPK_PARALLEL_ERR, mess, __LINE__, __FILE__ );
            }
            catch( const std::exception & stde )
            {
	      const int max = SpkError::maxMessageLen();
              char mess[max];
	      sprintf( mess, "%s\n \
                             An standard exception was thrown while an attempt to obtain an individual analysis result.\n",
		       stde.what() );
              throw SpkException(SpkError::SPK_PARALLEL_ERR, mess, __LINE__, __FILE__ );
            }
            catch( ... )
            {
	      const int max = SpkError::maxMessageLen();
	      char mess[max];
              sprintf( mess, "An unknown exception was thrown while an attempt to obtain an individual analysis result.\n" );
              throw SpkException( SpkError::SPK_PARALLEL_ERR, mess, __LINE__, __FILE__ );
            }
            
            if(outpack.empty())
            {
                int toBeReIssued = checklist.nextOldest();
                if( toBeReIssued >= 0 )
                {
                  inpack_toBeReIssued = checklist.getInPack(toBeReIssued);
                  try{
                    //
                    // Posts and waits until some node picks up this re-posted package.
                    // This ensures by the time this function returns, at least one node
                    // should have released a resulting package.
                    //
                    channel.post(inpack_toBeReIssued, true);
                  }
                  catch( SpkException& e )
                  {
		    const int max = SpkError::maxMessageLen();
		    char mess[max];
		    sprintf( mess, "Touble re-posting %d-th individual's data.\n", toBeReIssued );
                    throw e.push(SpkError::SPK_PARALLEL_ERR, mess, __LINE__, __FILE__);
                  }
#ifdef _DEBUG
                  cout << endl << "Master> Re-posted " << toBeReIssued << endl;
#endif
                }
            }
#ifdef _DEBUG
            else if(outpack.popItr != cntPopItrs )
            {
                  cerr << "Master> supposed to be " << cntPopItrs << " but was " << outpack.popItr << endl;
            }
#endif
            
        }while( outpack.empty() || checklist.isCompleted(outpack.indResults.getIndex()) 
            || outpack.popItr != cntPopItrs);


        results = outpack.indResults;
        int id = results.getIndex();
        inPack = checklist.getInPack(id);

        if( optimizer.getLevel() > 0 )
        {
            cout << "<PopID: " << cntPopItrs << ">" << endl;
            cout << "<IndID: " << id << ">" << endl;
        }

#ifdef _DEBUG
        cout << "Master> " << id << endl;
#endif

        dvecBhat_i           = results.getHat();
        dLambda_i            = results.getLambda();
        dLogdetLambda2diff_i = results.getLogdetLambda2diff();
        drowLTilde_alp_i     = results.getTilde_pop();

        //
        // Accumulating bi in a matrix
        //
        // This function terminates if id is out of range.
        replaceJth(dmatBoutTemp, id, dvecBhat_i);

        //                         ^
        // Sum[Lambda(alp, bi(alp,yi), yi)] over all subjects
        //
        dLambda += (dLTildeOut==0? 0.0 : dLambda_i);

        //                                      ~
        // Sum[0.5 * logdet(Lambda(alp, bi(alp,yi), yi)_bi_bi / 2PI)] over all subjects
        //
        dLogdetLambda2diff += dLogdetLambda2diff_i;

        if( isLTilde_alpOut )
        {
            drowLTilde_alp += drowLTilde_alp_i;
        }
        //
        // Mark the individual with "completed"
        //
        checklist.completed(id);
    }
    
    //         ~
    // Compute L
    //
    if( dLTildeOut != 0 )
    {
        *dLTildeOut = dLogdetLambda2diff 
               + dLambda;
        prevLogdetLambda2diff = dLogdetLambda2diff;
        prevLambda            = dLambda;
    }

    if( dmatBout_forAll ){
        *dmatBout_forAll = dmatBoutTemp;
        assert(dmatBout_forAll->nr() == num_b);
    }

    //         ~
    // Compute L_alp
    //
    if( drowLTilde_alpOut != 0 ){
        *drowLTilde_alpOut = drowLTilde_alp;
        assert(drowLTilde_alpOut->nc() == num_alp);
        prevLTilde_alp        = drowLTilde_alp;
    }
    ++cntPopItrs;
    return;
}
/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/
/*************************************************************************
 *
 * Function: getElements(const DoubleMatrix &original, int offset, int howMany)
 *
 * 
 * Description
 * -----------
 *
 * Return a vector containing data of original from offset to howMany,
 * as a seperate DM object.
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
 static DoubleMatrix getElements( const DoubleMatrix &original, int offset, int howMany )
 {
     DoubleMatrix dvecPart(howMany, 1);

     const double *start = original.data() + offset;
     double *pdPart     = dvecPart.data();

     std::copy(start, start+howMany, pdPart);
     return dvecPart;
 }

/*************************************************************************
 *
 * Function: operator+=(const DoubleMatrix &A, const DoubleMatrix &B)
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
static DoubleMatrix & operator+=(DoubleMatrix &A, const DoubleMatrix &B)
{
    A = add( A, B );
    return A;
    
}
/*************************************************************************
 *
 * Function: operator+(const DoubleMatrix &A, const DoubleMatrix &B)
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
/*
static DoubleMatrix operator+(const DoubleMatrix &A, const DoubleMatrix &B)
{
    return add( A, B );
}
*/
/*************************************************************************
 *
 * Local function: indAnalysis
 *
 * Performs the individual level computations and returns values required 
 * by IndOutputDataPackage. This will throw a std exception when fails.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Local function Specification
 *------------------------------------------------------------------------*/
#include "estimateB.h"
#include "lambda.h"
#include "lambda2diff.h"
#include "divByScalar.h"
#include "det.h"
#include "mulByScalar.h"
#include "rvec.h"
#include "inverse.h"
#include "transpose.h"
#include "pi.h"
#include "multiply.h"
#include "isSymmetric.h"

static const double PI2 = 2.0 * PI;

// David: below is another place where I wonder if doing the calculation
// of an objective and its derivative could be speeded up if the calculations
// were done in tandem.
// Notice that we first calculate the log of the determinant of a symmetric matrix in the objective,
// and then the derivative of this, which involves the inverse of this symmetric matrix.  Now in order to 
// calculate that determinant, I think the Nag routine will come very close to computing a full Cholesky
// factorization.  This same factorization should greatly speed up computing the inverse of this matrix.
// In fact I wonder if in the Nag routine for the inverse of a symmetric matrix, it just goes and computes
// the same Cholesky factorization that was nearly computed by the determinant function.
// Also, given the Cholesky factorization, there is no point in calling the log function, as we can just sum
// up the squares of the components on the diagonal.

// In the event that the user model class is not providing inverses of covariance matrices, this same idea
// should speed up computations involving objectives and objective derivatives for the \hat{b_i} and
// \tilde{b_i} loops.


// David: this function returns an object by value.  It would optimize the code a little bit
// if the function excepted a pointer to a IndOutputDataPackage
// object and filled out the members.
static DoubleMatrix dvecBlow(__FILE__);
static DoubleMatrix dvecBup(__FILE__);
static DoubleMatrix dvecBstep(__FILE__);
static DoubleMatrix dvecAlp(__FILE__);
static DoubleMatrix dvecBTilde_i(__FILE__);
static DoubleMatrix dmatBTilde_alp_i(__FILE__);
static DoubleMatrix dmatHessianTilde_i(__FILE__);
static DoubleMatrix dmatHessianTilde_alp_i(__FILE__);
static DoubleMatrix dmatHessianTilde_b_i(__FILE__);
static DoubleMatrix drowLambda_alp_i(__FILE__);

const IndOutputDataPackage indAnalysis(SpkModel &model, const IndInputDataPackage& inpack)
{
    using namespace std;

    const PopConstVals popConstVals = inpack.popConstVals;
    const PopVars popVars = inpack.popVars;
    const IndVars indVars = inpack.indVars;
    const int popItr      = inpack.popItr;

    const int whichObjective = static_cast<enum Objective>(popConstVals.getObjective());

    //
    // The version of FO method that treats a population problem as a big individual problem
    // shall never reach here.  It goes from fitPopulation() -> firstOrderOpt() -> mapOpt().
    // 
    assert( whichObjective != FIRST_ORDER );
    bool  isFo  = (whichObjective == NAIVE_FIRST_ORDER);
    
    dvecBlow = popConstVals.getLow();
    dvecBup  = popConstVals.getUp();
    dvecBstep= popConstVals.getStep();
    dvecAlp  = popVars.getPop();
    
    const bool  isHat = true;
    const bool  isLTildeOut     = popVars.isTilde();
    const bool  isLTilde_alpOut = popVars.isTilde_pop();
    
    dvecBin_i= indVars.getIn();
    dvecY_i  = indVars.getData();


    const int num_alp = dvecAlp.nr();
    const int num_b   = dvecBin_i.nr();
    const int who     = indVars.who();

    const valarray<double> alp   = dvecAlp.toValarray();
    const valarray<double> bStep = dvecBstep.toValarray();

    // Individual level variables
    dvecBhat_i.resize(num_b, 1);
    dvecBTilde_i.resize(num_b, 1);
    dmatBTilde_alp_i.resize(num_b, num_alp);
    drowLTilde_alp_i.resize(1, num_alp);

    double dLambda_i;
    double dLogdetLambda2diff_i;

    DoubleMatrix tmp1, tmp2;
    double p;
    long int q;

    //         ^
    // Compute bi (initial estimate of the ith subject's random population parameter).
    //
    estimateB(model, isFo, popConstVals.getOptimizer(), dvecY_i, dvecAlp, 
              dvecBin_i, dvecBlow, dvecBup, dvecBstep, 
              &dvecBhat_i, &dvecBTilde_i, (!isLTilde_alpOut? 0 : &dmatBTilde_alp_i));

    // Create the matrices to hold the output values from lambda2diff.
    dmatHessianTilde_i.resize( num_b, num_b );
    int nHessianTilde_xRows   = 0;
    int nHessianTilde_alpCols = 0;
    int nHessianTilde_bCols   = 0;
    if ( isLTilde_alpOut )
    {
        nHessianTilde_xRows   = num_b * num_b;  // Note:  x is either alp or b.
        nHessianTilde_alpCols = num_alp;
        nHessianTilde_bCols   = num_b;
    }
    dmatHessianTilde_alp_i.resize( nHessianTilde_xRows, nHessianTilde_alpCols );
    dmatHessianTilde_b_i.resize( nHessianTilde_xRows, nHessianTilde_bCols );

    //                             ~
    // Compute log det(Lambda(alp, bi(alp,yi), yi)_bi_bi)
    // --- 2nd order approximation of lambda with respect to the ith subject's random population parameter.
    //
    if( whichObjective == MODIFIED_LAPLACE )
    {
        lambda2diff(model, dvecY_i, dvecAlp, 
                   dvecBTilde_i, dvecBstep, 
                   &dmatHessianTilde_i, (!isLTilde_alpOut? 0 : &dmatHessianTilde_alp_i),
                   (!isLTilde_alpOut? 0 : &dmatHessianTilde_b_i), true); 
    }
    else if( whichObjective == EXPECTED_HESSIAN || whichObjective == NAIVE_FIRST_ORDER )
    {
        expectedHessian(model, dvecAlp, dvecBTilde_i, dvecBstep,
                        &dmatHessianTilde_i, (!isLTilde_alpOut? 0 : &dmatHessianTilde_alp_i),
                       (!isLTilde_alpOut? 0 : &dmatHessianTilde_b_i));
    }
    else
    {
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Invalid objective.", __LINE__, __FILE__);
    }

    //                                           ~
    // Compute p and q such that det(Lambda(alp, bi(alp,yi), yi)_bi_bi / 2PI) = p * 2^q.
    //
    // dmatHessianTilde_i may not be positive definite, especially when it was computed
    // in lambda2diff.  det() which performs cholesky decomposition to get the determinant
    // will throw an exception indicating the error.
    // This routine does not do anything with the error, but the caller, lTilde(),
    // will take care of that properly.
    //
    det( ( divByScalar(dmatHessianTilde_i, PI2) ), &p, &q );
    dLogdetLambda2diff_i = 0.5 * ( log( p ) + q * log( 2.0 ) );

    //                         ^
    // Compute Lambda(alp, bi(alp,yi), yi)
    drowLambda_alp_i.resize(1,num_alp);

	if( isLTildeOut )
		dLambda_i = lambda(model, dvecY_i, dvecAlp, dvecBhat_i, true);

    if( isLTilde_alpOut )
		drowLambda_alp_i = lambda_alp(model, dvecY_i, dvecAlp, dvecBhat_i, true);
    
    // Compute L_alp for this subject
    if( isLTilde_alpOut )
    {
        //
        // dmatHessianTilde_i could be singular.
        // It should have been detected earlier when the determinant of
        // the matrix is attempted to be computed.
        //
        tmp1 = mulByScalar(transpose( rvec(inverse(dmatHessianTilde_i)) ), 0.5);
        tmp2 = (add(dmatHessianTilde_alp_i, multiply(dmatHessianTilde_b_i, dmatBTilde_alp_i)));
        add(drowLambda_alp_i,multiply(tmp1, tmp2), drowLTilde_alp_i);
    }

    IndResults results(who, dvecBhat_i, dvecBTilde_i, drowLTilde_alp_i, dLambda_i, dLogdetLambda2diff_i);
    IndOutputDataPackage outpack(popItr, results);
    return outpack;
}

