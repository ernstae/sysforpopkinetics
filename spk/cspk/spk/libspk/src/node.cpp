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
 * File: node.cpp
 *
 *
 * Node
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: node()
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

$index node$$
$index parallel, node$$

$table
$bold Prototype:$$   $cend  
$syntax/node(const char* c_sharedDirectory, SpkModel& model)/$$
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
$bold Node$$ acquires an $xref/IndInputDataPackage//input data package/$$ though 
$xref/NodeEndChannel//Channel/$$ if a package is available and performs 
the individual analysis.  Before posting a set of results for a particular individual
packaged in a 
$xref/IndOutputDataPackage//output data package/$$ back to Master,
it checks the version of a problem to which the set of result belongs to against
the problem currently Master is working on.  
If matches, it posts the result set. Otherwise, the result set will be discarded or
will not be posted.

$pre

$$
It repeats this process until it detects $xref/broadCastEndOfSpk//the end of SPK signal/$$ sent
by $bold Master$$.  $bold Node$$ processes an individual's case without
regards to who she/he is (ie. ignoring the individual's index) or to how many
times the same individual's analysis has been requested in the past.
$pre

$$
When Node detects $xref/broadCastEndOfSpk//the end of SPK/$$, it terminates.

$head Arguments$$
$syntax/

/c_sharedDirectory
/$$
is a null-terminated char string specifying the full pathname to the shared directory
used by Master and Node for communication.

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
 *
 * Implementation Notes
 * --------------------
 *
 * Local functions duplicates their definitions in lTilde().
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <string>
#include <cmath>
#include <cfloat>
#include <fstream>
#include <exception>

#include "DoubleMatrix.h"
#include "node.h"
#include "SpkModel.h"
#include "Channel.h"
#include "IndDataPackage.h"
#include "IndResults.h"
#include "lambda.h"
#include "estimateB.h"
#include "isEndOfSpk.h"
#include "System.h"
#include "File.h"
#include "rvec.h"
#include "transpose.h"
#include "pi.h"
#include "lambda2diff.h"
#include "det.h"
#include "divByScalar.h"
#include "inverse.h"
#include "mulByScalar.h"
#include "add.h"
#include "PARALLEL_FILE_CONSTS.h"
#include "Objective.h"
#include "NaiveFoModel.h"

/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/
static DoubleMatrix getElements( const DoubleMatrix &original, int offset, int howMany );
static DoubleMatrix & operator+=(DoubleMatrix &A, const DoubleMatrix &B);
static const IndOutputDataPackage indAnalysis(SpkModel &model, const IndInputDataPackage& inpack);

//
// Retrieve a session ID posted by Master
//
static long getMasterSessionID( const char* sessionFileFullpath )
{
  using namespace std;

  long sessionID;  
  do
  {
    sessionID = -1;
    ifstream ifs(sessionFileFullpath);
    ifs >> sessionID;
    ifs.close();
  }while(sessionID < 0 );

  return sessionID;
}

//
// Retrieve a population level iteration number posted by Master
//
static int getMasterPopItr(const char* popItrFileFullpath)
{
  using namespace std;

  int masterPopItr;  
  do
  {
    masterPopItr = -1;
    ifstream ifs(popItrFileFullpath);
    ifs >> masterPopItr;
    ifs.close();
  }while(masterPopItr < 0 );

  return masterPopItr;
}


/*------------------------------------------------------------------------
 * Local variables declarations
 *------------------------------------------------------------------------*/
static DoubleMatrix dvecAlp(__FILE__);
static DoubleMatrix dvecBin(__FILE__);
static long curSessionID;

using namespace std;
void node(const char* c_sharedDirectory, SpkModel& model)
{
  File sharedDiskSpace(c_sharedDirectory, "");

  string popItrString = sharedDiskSpace.getPath();
  popItrString += "pop.itr";
  const char* popItrFileFullpath = popItrString.c_str();

  string sessionIDString = sharedDiskSpace.getPath();
  sessionIDString += "session.id";
  const char* sessionIDFileFullpath = sessionIDString.c_str();
  curSessionID = getMasterSessionID(sessionIDFileFullpath);

  string firstInputFile = sharedDiskSpace.getPath();
  firstInputFile += parallel_const::SPK_INDINPUT;
  firstInputFile += "*.";
  firstInputFile += parallel_const::SPK_MASTER_SUFFIX;

  //
  // Wait till Master starts putting data files
  //
  while( true )
  {
      if( !isEndOfSpk(sharedDiskSpace) && !(System::findfirst(firstInputFile.c_str())).empty() )
          break;
      System::sleep(5);
  }

  NodeEndChannel channel(sharedDiskSpace);
  int masterPopItr = 0;

  int    who;
  int    level;
  enum Objective whichObjective;

  IndInputDataPackage inPack;
  IndOutputDataPackage outPack;

  channel.open();
  cout << "Node is running." << endl;
  while( !isEndOfSpk(sharedDiskSpace) )
  {
      try{
          inPack = channel.get();
      }
      catch( const SpkError::ErrorCode & e )
      {
          //
          // This is a good exception.  It indicates
          // Master broadcasted the end of spk.
          // Let's get out of the loop and terminate normally.
          //
          if( e == SpkError::SPK_PARALLEL_END_SIGNAL )
            break;
          else
            throw;
      }
      catch(SpkException &e)
      {
        const int max = SpkError::maxMessageLen();
        char mess[max];

        if( System::exist(sharedDiskSpace) )
        {
          sprintf( mess, "Fatal error occured while accessing the shared directory.\n" );
        }
        else
        {
          sprintf( mess, "The shared directory is no longer accessible.\n" );
        }

        e.push(SpkError::SPK_UNKNOWN_ERR, mess, __LINE__, __FILE__);
        cerr << e << endl;
        abort();
      }
      catch(const std::exception& stde)
      {
        const int max = SpkError::maxMessageLen();
        char mess[max];

        if( System::exist(sharedDiskSpace) )
        {
          sprintf( mess, "Fatal IO error occured while accessing the shared directory.\n" );
        }
        else
        {
          sprintf( mess, "The shared directory is no longer accessible.\n" );
        }

        SpkException e(stde, mess, __LINE__, __FILE__);
        cerr << e << endl;
        abort();
      }
      catch(...)
      {
        const int max = SpkError::maxMessageLen();
        char mess[max];

        if( System::exist(sharedDiskSpace) )
        {
          sprintf( mess, "Fatal IO error occured while accessing the shared directory.\n" );
        }
        else
        {
          sprintf( mess, "The shared directory is no longer accessible.\n" );
        }

        SpkException e(SpkError::SPK_UNKNOWN_ERR, mess, __LINE__, __FILE__);
        cerr << e << endl;
        abort();
      }

      if( inPack.popItr != (masterPopItr = getMasterPopItr(popItrFileFullpath) ) )
      {
#ifdef _DEBUG
        cout << "Node> PopItr changed from " << inPack.popItr << " to " << masterPopItr << endl;
#endif
        continue;
      }

      who            = inPack.indVars.who();
      level          = inPack.popConstVals.getOptimizer().getLevel();
      whichObjective = static_cast<enum Objective>( inPack.popConstVals.getObjective() );
      masterPopItr   = inPack.popItr;

#ifdef _DEBUG
      cout << "Node> " << who << endl;
#endif
      if( level > 0 )
      {
          cout << "<PopID: " << masterPopItr << ">" << endl;
          cout << "<IndID: " << who << ">" << endl;
      }

      model.selectIndividual(who);
      try{
        //
        // [ Comment by Sachiko, 09/25/2002 ]
        //
        // This NaiveFoModel object construction is done at the
        // very top level routine (i.e. fitPopulation()) for the single process mode.
        // For parallel, it means imposing the users to choose and feed a NaiveFoModel
        // into node() if we think the role of node() routine
        // as equivalent of single's lTilde().
        // However, as of today, we would like to hide the existance of such a 
        // specialized model from the users.  Thus, construct an NaiveFoModel object
        // in this routine.
        // 
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
      }
      catch( SpkException& e )
      {
          const int max = SpkError::maxMessageLen();
          char buf[max];
          sprintf( buf, "\nThe %d-th individual's analysis failed.", who );

          channel.post(e.push(SpkError::SPK_UNKNOWN_ERR, buf, __LINE__,__FILE__));
          continue;
      }
      catch( const std::exception& stde )
      {
          const int max = SpkError::maxMessageLen();
          char buf[max];
          sprintf( buf, "\nThe %d-th individual's analysis failed.", who );

	  // PARALLEL -- FIX LATER
          //channel.post(SpkException(stde,
	  //  buf,
	  //  __LINE__,
	  //  __FILE__));
          continue;
      }
      catch( ... )
      {
          const int max = SpkError::maxMessageLen();
          char buf[max];
          sprintf( buf, "\nThe %d-th individual's analysis failed.", who );

	  //          channel.post(SpkException(
	  //  SpkError::SPK_UNKNOWN_ERR,
	  //  buf,
	  //  __LINE__,
	  //  __FILE__));
          continue;
      }

      //
      // Don't post if the computational result is for an obsolte problem.
      //
      long newSessionID = getMasterSessionID( sessionIDFileFullpath );
      if( newSessionID != curSessionID )
      {
#ifdef _DEBUG
        cout << "Node> A new session <" << newSessionID << ">" << endl;
#endif
        curSessionID = newSessionID;
      }
      else
      {
        channel.post(outPack);
      }

  }
  cout << endl;
  channel.close();

  cout << "Node is exiting normally..." << endl;
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
     const double *pdOriginal = original.data();
     double *pdPart     = dvecPart.data();
     for( int i=0; i<howMany; i++ )
         pdPart[i] = pdOriginal[offset+i];
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
#include "Objective.h"
#include "NaiveFoModel.h"

static const double PI2 = 2.0 * PI;
/*------------------------------------------------------------------------
 * Local variables declarations
 *------------------------------------------------------------------------*/

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

const IndOutputDataPackage indAnalysis(SpkModel &model, const IndInputDataPackage& inpack)
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
