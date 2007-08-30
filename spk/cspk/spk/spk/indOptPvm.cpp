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
 * File: indOptPvm.cpp
 *
 *
 * IndOptPvm
 *
 * Author: Jiaji Du
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: indOptPvm()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin node$$
$spell 
   sub exe spk ie. const
   covariance
   inv
   covariances
   ind
   pathname
   bool
   Fo
$$

$section Individual Analysis in Parallel Mode$$

$index indOptPvm$$
$index parallel, individual, optimization, pvm$$

$table
$bold Prototype:$$   $cend  
$syntax/indOptPvm(const char* input, SpkModel<double>& model)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$bold indOptPvm$$ performs individual level optimization using the $xref/SpkModel//model/$$ 
and the $xref/IndInputDataPackage//individual input data package/$$ in character array form.  
If success, it sends the resulted $xref/IndOutputDataPackage//indvidual output data package 
to the PVM paraent in character array form, Otherwise it sends the error message to the PVM 
paraent also in character array form.

$head Arguments$$
$syntax/

/input
/$$
is a character string representaion of IndInputDataPackage.

$syntax/

/model/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <string>
#include <cmath>
#include <cfloat>
#include <sstream>
#include <exception>
#include <pvm3.h>

#include "spk/spkpvm.h"
#include "DoubleMatrix.h"
#include "indOptPvm.h"
#include "IndDataPackage.h"
#include "IndResults.h"
#include "lambda.h"
#include "estimateB.h"
#include "rvec.h"
#include "transpose.h"
#include "pi.h"
#include "lambda2diff.h"
#include "det.h"
#include "divByScalar.h"
#include "inverse.h"
#include "mulByScalar.h"
#include "add.h"
#include "Objective.h"
#include "NaiveFoModel.h"
#include "intToOrdinalString.h"
#include "WarningsManager.h"
using namespace std;

static int parent_tid;
static const IndOutputDataPackage indAnalysis(SpkModel<double> &model, const IndInputDataPackage& inpack);
static void process(SpkModel<double> &model, const char* input);
static void send(ostringstream stream, int message_tag);

int indOptPvm(const char* input, SpkModel<double>& model)
{
    parent_tid = pvm_parent();
    process(model, input);

    pvm_notify(PvmTaskExit, PvmTaskExit, 1, &parent_tid);
    int bufid = 0;
    int rval, bytes, msgtag, tid;
    while((bufid = pvm_recv(-1, -1)) > 0)
    {
        rval = pvm_bufinfo(bufid, &bytes, &msgtag, &tid);
        if(msgtag == PvmTaskExit)
        {
            pvm_exit();
            return 4;  // USER_ABORT
        }
        if(msgtag == SpkPvmDataPackage)
        {
            char* buf = new char[bytes];
            pvm_upkstr(buf);
            process(model, buf);
            delete [] buf;
        }
    }
    return 0;  // SUCCESSFUL
}

/*************************************************************************
 *
 * Local function: process
 *
 * Process individual optimization request.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Local function Specification
 *------------------------------------------------------------------------*/
void send(string message, int message_tag)
{
    const char* output = message.c_str();
    char* output_str = const_cast<char*>(output);
    pvm_pkstr(output_str);

    // Get warning messages if any
    string warningList;
    int warnings = WarningsManager::getWarningList(warningList);
    char* warning_str = const_cast<char*>(warningList.c_str());
    pvm_pkstr(warning_str);
    pvm_pkint(&warnings, 1, 1);
    WarningsManager::clearAllWarnings();

    // Send output and warning to fitDriver
    pvm_send(parent_tid, message_tag);
}

/*************************************************************************
 *
 * Local function: process
 *
 * Process individual optimization request.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Local function Specification
 *------------------------------------------------------------------------*/
void process(SpkModel<double> &model, const char* input)
{
    int    who;
    int    level;
    enum Objective whichObjective;
    int masterPopItr;

    IndInputDataPackage inPack;
    IndOutputDataPackage outPack;

    // Convert char array to IndInputDataPackage
    string str(input);
    istringstream iStringStream(str);
    iStringStream >> inPack;

    who            = inPack.indVars.who();
    level          = inPack.popConstVals.getOptimizer().getLevel();
    whichObjective = static_cast<enum Objective>( inPack.popConstVals.getObjective() );
    masterPopItr   = inPack.popItr;

    // Prepare pvm to send output
    pvm_setopt(PvmRoute, PvmDontRoute);
    pvm_initsend(PvmDataDefault);
    model.selectIndividual(who);
    try
    {
        if( whichObjective == NAIVE_FIRST_ORDER )
        {
            valarray<double> bStep = inPack.popConstVals.getStep().toValarray();
            NaiveFoModel foModel( &model, bStep );
            outPack = indAnalysis(foModel, inPack);
        }
        else
        {
            outPack = indAnalysis(model, inPack);
        }

        // Convert IndOutputDataPackage to char array
        ostringstream stream;
        stream << outPack;
        string message = stream.str();
        send(message, SpkPvmResult);
    }
    catch( SpkException& e )
    {
        const int max = SpkError::maxMessageLen();
        char buf[max];
        snprintf( buf, max, "The analysis failed for the %s individual due to SpkException.",
                 intToOrdinalString( who, ZERO_IS_FIRST_INT ).c_str() );
        e.push(SpkError::SPK_UNKNOWN_ERR, buf, __LINE__,__FILE__);
        ostringstream stream;
        stream << e;
        string message = stream.str();
        send(message, SpkPvmErrorMessage);
    }
    catch( const std::exception& stde )
    {
        const int max = SpkError::maxMessageLen();
        char buf[max];
        snprintf( buf, max, "The analysis failed for the %s individual due to std error.",
                 intToOrdinalString( who, ZERO_IS_FIRST_INT ).c_str() );
        SpkException e = SpkException(SpkError::SPK_STD_ERR, buf,    
                                      __LINE__, __FILE__ );
        ostringstream stream;
        stream << e;
        string message = stream.str();
        send(message, SpkPvmErrorMessage);
    }
    catch( ... )
    {
        const int max = SpkError::maxMessageLen();
        char buf[max];
        snprintf( buf, max, "The analysis failed for the %s individual due to unknown error.",
                 intToOrdinalString( who, ZERO_IS_FIRST_INT ).c_str() );
        SpkException e = SpkException(SpkError::SPK_UNKNOWN_ERR, buf,    
                                      __LINE__, __FILE__ );
        ostringstream stream;
        stream << e;
        string message = stream.str();
        send(message, SpkPvmErrorMessage);
    }
}

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
#include "multiply.h"
#include "expectedHessian.h"

static const double PI2 = 2.0 * PI;
/*------------------------------------------------------------------------
 * Local variables declarations
 *------------------------------------------------------------------------*/

static DoubleMatrix dvecAlp(__FILE__);
static DoubleMatrix dvecBlow(__FILE__);
static DoubleMatrix dvecBup(__FILE__);
static DoubleMatrix dvecBstep(__FILE__);
static DoubleMatrix dvecBin_i(__FILE__);
static DoubleMatrix dvecY_i(__FILE__);
static DoubleMatrix dvecBhat_i(__FILE__);
static DoubleMatrix dvecBTilde_i(__FILE__);
static DoubleMatrix dmatBTilde_alp_i(__FILE__);
static DoubleMatrix dmatHessianTilde_i(__FILE__);
static DoubleMatrix dmatHessianTilde_alp_i(__FILE__);
static DoubleMatrix dmatHessianTilde_b_i(__FILE__);
static DoubleMatrix drowLambda_alp_i(__FILE__);
static DoubleMatrix drowLambda_b_i(__FILE__);
static DoubleMatrix drowLTilde_alp_i(__FILE__);

const IndOutputDataPackage indAnalysis(SpkModel<double> &model, const IndInputDataPackage& inpack)
{
    using namespace std;

    const PopConstVals popConstVals = inpack.popConstVals;
    const PopVars popVars = inpack.popVars;
    const IndVars indVars = inpack.indVars;
    const int popItr      = inpack.popItr;
    const int whichObjective = static_cast<enum Objective>(popConstVals.getObjective());
    bool  isFo = (whichObjective == NAIVE_FIRST_ORDER);

    //
    // The version of FO method that treats a population problem as a big individual problem
    // shall never reach here.  It goes from fitPopulation() -> firstOrderOpt() -> mapOpt().
    // 
    assert( whichObjective != FIRST_ORDER );
    
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
    const int id   = indVars.who();

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
    // This hessian commputation is specific to Laplace method.  It will be replaced by
    // something else for FOCE (expected hessian).
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
    det( ( divByScalar(dmatHessianTilde_i, PI2) ), &p, &q );
    dLogdetLambda2diff_i = 0.5 * ( log( p ) + q * log( 2.0 ) );

    //                         ^
    // Compute Lambda(alp, bi(alp,yi), yi)
    drowLambda_alp_i.resize(1,num_alp);
    drowLambda_b_i.resize(1,num_b);

	if( isLTildeOut )
		dLambda_i = lambda(model, dvecY_i, dvecAlp, dvecBhat_i, true);

	if( isLTilde_alpOut )
		drowLambda_alp_i = lambda_alp(model, dvecY_i, dvecAlp, dvecBhat_i, true);

    // Compute L_alp for this subject
    if( isLTilde_alpOut ){
        tmp1 = mulByScalar(transpose( rvec(inverse(dmatHessianTilde_i)) ), 0.5);
        tmp2 = (add(dmatHessianTilde_alp_i, multiply(dmatHessianTilde_b_i, dmatBTilde_alp_i)));
        add(drowLambda_alp_i,multiply(tmp1, tmp2), drowLTilde_alp_i);
    }

    IndResults results(id, dvecBhat_i, dvecBTilde_i, drowLTilde_alp_i, dLambda_i, dLogdetLambda2diff_i);
    IndOutputDataPackage outpack(popItr, results);
    return outpack;
}
