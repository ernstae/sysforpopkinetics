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
 * File: lTildePvm.cpp
 *
 * Author: Jiaji Du
 *
  *************************************************************************/


/*************************************************************************
 *
 * Function: lTildePvm
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
  $begin lTildePvm$$
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
  PVM
  $$

  $index lTildePvm$$
  $cindex parametric population objective function using PVM$$

  $table
  $bold Headers::$$     $cend
  lTilde.h              $rend
  $cend
  Objective.h $rend
  $bold Prototype:$$    $cend  
  $syntax/void lTildePvm(
  int                  /nPvmTasks/,
  SpkModel<double>     &/model/,
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
  Evaluates the modified Laplace as defined in the reference using PVM
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

  /nPvmTasks/
  /$$
  The number of PVM tasks to spawn.

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

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <sstream>
#include <string>
#include <cmath>
#include <cfloat>
#include <cassert>
#include <exception>
#include <pvm3.h>
#include <fstream>
#include "spk/spkpvm.h"
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
#include "lTildePvm.h"
#include "transpose.h"
#include "mulByScalar.h"
#include "WarningsManager.h"
#include "intToOrdinalString.h"

enum RETURN_CODE { SUCCESS              = 0,
                   UNKNOWN_ERROR        = 1,
                   UNKNOWN_FAILURE      = 2,
                   PVM_FAILURE          = 3,
                   USER_ABORT           = 4,
                   FILE_ACCESS_ERROR    = 10,
                   OPTIMIZATION_ERROR   = 12,
                   STATISTICS_ERROR     = 13,
                   USER_INPUT_ERROR     = 14,
                   PROGRAMMER_ERROR     = 15,
                   SIMULATION_ERROR     = 16,
                   FILE_ACCESS_FAILURE  = 100,
                   RESERVED_DO_NOT_USE  = 101,
                   OPTIMIZATION_FAILURE = 102,
                   STATISTICS_FAILURE   = 103,
                   PROGRAMMER_FAILURE   = 105,
                   SIMULATION_FAILURE   = 106
                 };

/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/
static DoubleMatrix getElements( const DoubleMatrix &original, int offset, int howMany );
static DoubleMatrix & operator+=(DoubleMatrix &A, const DoubleMatrix &B);

// Counts the population level iterations
static int cntPopItrs = 0;

// Maintains the previous values for future need
static double prevLogdetLambda2diff  = 0.0;
static double prevLambda             = 0.0;
static DoubleMatrix prevLambdaTilde_alp(__FILE__);
static DoubleMatrix prevLTilde_alp     (__FILE__);
static std::vector<int> ind_tid;
static std::vector<int> host_tid;
static std::vector<int> ind_iid;
static std::vector<std::string> inputAll;
static bool first = true;
static int num_subjects;
static int parent_tid;
static int num_tasks;

// Objects to minimize memory allocation activities
static DoubleMatrix dvecY_i            (__FILE__); 
static DoubleMatrix dvecBin_i          (__FILE__);
static DoubleMatrix dvecBhat_i         (__FILE__);
static DoubleMatrix drowLTilde_alp_i   (__FILE__);
static DoubleMatrix drowLTilde_alp     (__FILE__);
static DoubleMatrix dmatBoutTemp       (__FILE__);
static DoubleMatrix dmatLambdaTilde_alp(__FILE__);

/*------------------------------------------------------------------------
 * Local Function definition
 *------------------------------------------------------------------------*/
void stop(char* message, int exit_value)  // for parallel
{
    std::cerr << message << std::endl;
    for(int i = 0; i < num_tasks; i++)
        pvm_kill(ind_tid[i]);
    pvm_initsend(PvmDataDefault);
    pvm_pkint(&exit_value, 1, 1);
    pvm_send(parent_tid, SpkPvmExitValue);
    pvm_exit();
    fclose( stdout );
    fclose( stderr );
    exit(exit_value);
}

/*------------------------------------------------------------------------
 * Local Function definition
 *------------------------------------------------------------------------*/
static void spawnInd(const char* input, int iid)
{
    int id = iid;
    if(iid >= num_tasks) id = iid % num_tasks;
    int tid, bytes, msgtag;   
    char cwd[100];
    getcwd(cwd, 100);
    char task[100];
    sprintf(task, "%s/indDriver", cwd);
    char* arg[3];
    arg[0] = const_cast<char*>(input);
    arg[1] = cwd;
    arg[2] = NULL;
    int rval = pvm_spawn(task, arg, 0, NULL, 1, &tid);
    if (rval != 1)
    {
        stop("pvm_spawn failed for individual", PVM_FAILURE);
        exit(PVM_FAILURE);
    }
    int host = pvm_tidtohost(tid);
    if(first && iid < num_tasks)
    {
        ind_tid.push_back(tid);
        ind_iid.push_back(iid);
        host_tid.push_back(host);
    }
    else
    {
        ind_tid[id]  = tid;
        ind_iid[id]  = iid;
        host_tid[id] = host;
    }

    // Establish notification of deletion of the host of the task we have just
    // spawned, if we haven't already asked to be notified for this host.
    // WARNING! Do not change the order of these notifications.
    bool host_notified = false;
    for(int k = 0; k < num_tasks; k++)
    {
        if(k != id && host_tid[k] == host_tid[id])
        {
            host_notified = true;
            break;
        }
    }

    if(!host_notified && pvm_notify(PvmHostDelete, PvmHostDelete, 1, &host) < 0)
    {
        stop("pvm_notify failed for PvmHostDelete", PVM_FAILURE);
        exit(PVM_FAILURE);
    }

    // Establish notification for task exit of the task we have just spawned
    if(pvm_notify(PvmTaskExit, PvmTaskExit, 1, &tid) < 0)
    {
        stop("pvm_notify failed for PvmTaskExit", PVM_FAILURE);
        exit(PVM_FAILURE);
    }
}

/*------------------------------------------------------------------------
 * Local Function definition
 *------------------------------------------------------------------------*/
static int respawn(int host)
{
    int n = 0;
    for(int i = 0; i < num_tasks; i++)
    {
        if(host_tid[i] == host)
        {
            host_tid[i] = 0;
            const char* input = inputAll[ind_iid[i]].c_str();
            spawnInd(input, ind_iid[i]);
            n++;
        }
    }
    return n;
}


/*------------------------------------------------------------------------
 * Local Function definition
 *------------------------------------------------------------------------*/
static int tid2id(int tid)
{
    for(int i = 0; i < num_tasks; i++)
        if(ind_tid[i] == tid)
            return i;
    return -1;
}

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
void lTildePvm(
            int                nPvmTasks,
	    SpkModel<double>   &model,
	    enum Objective     whichObjective,
	    const DoubleMatrix &dvecY_forAll,  // all individuals' data
	    const DoubleMatrix &dvecNumsOfDataforEachSubject,
	    Optimizer&         optimizer,
	    const DoubleMatrix &dvecAlp,
	    const DoubleMatrix &dvecBlow,
	    const DoubleMatrix &dvecBup,
	    const DoubleMatrix &dvecBstep,
	    const DoubleMatrix &dmatBin_forAll,
	    DoubleMatrix       *dmatBout_forAll,
	    double             *dLTildeOut,
	    DoubleMatrix       *drowLTilde_alpOut,
	    DoubleMatrix       *dmatLambdaTilde_alpOut
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
    num_subjects                = dvecNumsOfDataforEachSubject.nr();
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
    IndResults* resultsAll = new IndResults[num_subjects];

    //                     ~                                    ^
    // Sum det(Lambda(alp, bi(alp,yi), yi)_bi_bi) + Lambda(alp, bi(alp,yi), yi) over all subjects
    inx_yi0 = 0;

    // Get parent tid
    parent_tid = pvm_parent();

    // Get number of tasks
    num_tasks = num_subjects;
    if(num_tasks > nPvmTasks)
        num_tasks = nPvmTasks;

    for( i=0; i<num_subjects; i++ )
    {  
        model.selectIndividual(i);
        
        // Get the number of data values for the ith subject.
        num_y_i = static_cast<int>( pdNy[i] );

        // Get the data for this subject.
        dvecY_i = getElements( dvecY_forAll, inx_yi0, num_y_i );

        // Get the initial b for the ith subject.
        dvecBin_i = getCol(dmatBin_forAll, i);

        // Pack input information in data structures
        const IndVars initvals( i, dvecBin_i, dvecY_i );

        const IndInputDataPackage inPack(cntPopItrs, popconsts, popvars, initvals);

        // Convert IndOutputDataPackage to char array
        ostringstream oStringStream;
        oStringStream << inPack;
        string std_str = oStringStream.str();
        const char* input = std_str.c_str();

        // Add input to input list
        inputAll.push_back(std_str);

        if(first)
        {
            if(i < num_tasks)
            {
                // Spawn a PVM task
                spawnInd(input, i);
            }
            else
            {
                // Send input to indDriver
                char* str = const_cast<char*>(input);
                pvm_initsend(PvmDataDefault);
                pvm_pkstr(str);
                pvm_send(ind_tid[i % num_tasks], SpkPvmDataPackage);
                ind_iid[i % num_tasks] = i;
            }
        }
        else
        {
            // Send input to indDriver
            char* str = const_cast<char*>(input);
            pvm_initsend(PvmDataDefault);
            pvm_pkstr(str);
            pvm_send(ind_tid[i % num_tasks], SpkPvmDataPackage);
            ind_iid[i % num_tasks] = i;
        }
        inx_yi0 += num_y_i;
    
        if((i + 1) % num_tasks == 0 || i + 1 == num_subjects)
        {
            int ndone = 0;
            int bufid = 0;
            int nWarnings;

            if(i + 1 == num_subjects && num_subjects % num_tasks != 0)
                ndone = num_tasks - num_subjects % num_tasks;

            IndOutputDataPackage outPack;
            while((bufid = pvm_recv(-1, -1)) > 0)
            {
                int rval, bytes, msgtag, source, exit_tid;
                rval = pvm_bufinfo(bufid, &bytes, &msgtag, &source);
                if(msgtag == PvmHostDelete)
                {
                    pvm_upkint(&exit_tid, 1, 1);
                    respawn(exit_tid);
                }
                if(msgtag == PvmTaskExit)
                {
                    pvm_upkint(&exit_tid, 1, 1);
                    if(exit_tid == parent_tid) // my parent exit means user aborting job
                    {
                        stop("user aborting job", USER_ABORT);
                    }
                    if(tid2id(exit_tid) >= 0) // my child shouldn't exit before my exit 
                    {
                        stop("an individual task exited unexpectedly", UNKNOWN_FAILURE);
                    }
                }
                if(msgtag == SpkPvmResult)
                {
                    char* output = new char[bytes];

                    // Get results
                    pvm_upkstr(output);
                    string str(output);
                    istringstream iStringStream(str);
                    iStringStream >> outPack;
                    results = outPack.indResults;
                    resultsAll[results.getIndex()] = results;

                    // Get warnings
                    pvm_upkstr(output);
                    pvm_upkint(&nWarnings, 1, 1);
                    WarningsManager::addWarningList(output, nWarnings);

                    delete [] output;
                    ndone++;
                    if(ndone == num_tasks) break;
                }
                if(msgtag == SpkPvmErrorMessage)
                {
                    char* output = new char[bytes];

                    // Get SpkExceptions
                    pvm_upkstr(output);
                    string str(output);
                    istringstream iStringStream(str);
                    SpkException e;
                    iStringStream >> e;
            
                    // Get warnings
                    pvm_upkstr(output);
                    pvm_upkint(&nWarnings, 1, 1);
                    WarningsManager::addWarningList(output, nWarnings);

                    delete [] output;
                    throw e;
                }
            }
        }
    }

    for(i = 0; i < num_subjects; i++)
    {
        dvecBhat_i = resultsAll[i].getHat();
        assert(dvecBhat_i.nr()==num_b);
        drowLTilde_alp_i = resultsAll[i].getTilde_pop();
        assert(drowLTilde_alp_i.nc()==num_alp);
        dLambda_i  = resultsAll[i].getLambda();
        dLogdetLambda2diff_i = resultsAll[i].getLogdetLambda2diff();
        
        // Accumulating bi in a matrix
        // This function will terminate if *i* is out of range.
        replaceJth(dmatBoutTemp, i, dvecBhat_i);

        //                         ^
        // Sum[Lambda(alp, bi(alp,yi), yi)] over all subjects
        dLambda += (dLTildeOut==0? 0.0 : dLambda_i);

        //                                      ~
        // Sum[0.5 * logdet(Lambda(alp, bi(alp,yi), yi)_bi_bi / 2PI)] over all subjects
        dLogdetLambda2diff += dLogdetLambda2diff_i;

        if( drowLTilde_alpOut )
        {
	    drowLTilde_alp += drowLTilde_alp_i;
        }
        if( dmatLambdaTilde_alpOut )
        {
	    replaceJth( dmatLambdaTilde_alp, i, transpose( drowLTilde_alp_i ) );
        }
    }

    delete [] resultsAll;

    //         ~
    // Compute L
    if( dLTildeOut != 0 )
    {
        *dLTildeOut = dLogdetLambda2diff + dLambda;
        prevLogdetLambda2diff = dLogdetLambda2diff;
        prevLambda = dLambda;
    }

    if( dmatBout_forAll )
    {
        *dmatBout_forAll = dmatBoutTemp;
        assert(dmatBout_forAll->nr() == num_b);
    }

    //         ~
    // Compute L_alp
    if( drowLTilde_alpOut != 0 )
    {
        *drowLTilde_alpOut = drowLTilde_alp;
        assert(drowLTilde_alpOut->nc() == num_alp);
        prevLTilde_alp        = drowLTilde_alp;
    }

    //         ~
    // Compute Lambda_alp_i
    if( dmatLambdaTilde_alpOut != 0 )
    {
        *dmatLambdaTilde_alpOut = dmatLambdaTilde_alp;
        assert(dmatLambdaTilde_alpOut->nr() == num_alp);
        assert(dmatLambdaTilde_alpOut->nc() == num_subjects);
        prevLambdaTilde_alp   = dmatLambdaTilde_alp;
    }

    if(first)
        first = false;

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

