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
 * File: SpdModel.cpp
 *
 *
 * This is the base class for population optimal design models.
 *
 * Most of its functions are copies of analagous functions from
 * SpkModel with minor modifications.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#include <cmath>

#include "SpkValarray.h"
#include "SpdModel.h"
#include "inverse.h"
#include "AkronBtimesC.h"
#include "mulByScalar.h"
#include "allZero.h"
#include "FullDataCovariance.h"
#include "FullIndParCovariance.h"

using SPK_VA::valarray;

static DoubleMatrix minusAkronBtimesC( 
  const DoubleMatrix &A, const DoubleMatrix &B, const DoubleMatrix &C );
static void minusAkronBtimesC( 
  const DoubleMatrix &A, const DoubleMatrix &B, const DoubleMatrix &C, DoubleMatrix& ret);


//*************************************************************************
//
//
//            SpdModel Constructors, Assignment Operators, and Destructors
//
//
//*************************************************************************


//*************************************************************************
//
//
//     SpdModel Virtual Members and Corresponding Public Interfaces
//
//
//*************************************************************************
/*************************************************************************
 *
 * Virtual function: private: doSetDesPar( const SPK_VA::valarray<double>& )
 * and corresponding public interface: setDesPar( const SPK_VA::valarray<double>& )
 *
 *************************************************************************/
/*
$begin SpdModel_setDesPar$$
$spell
	Model model 
    int
    void
    covariances
    -th
    Spd
    Ind
    const
    fi
    cerr
    endl
    Spk
    Ri
    valarray
$$

$section Set Design Parameter$$

$index SpdModel, doSetDesPar()$$
$index SpdModel, setDesPar()$$
$index User-provided Model, set design parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual void SpdModel::doSetDesPar( const SPK_VA::valarray<double>& /desPar/ )/$$ $rend
$bold Public Interface:$$   $rend  
$syntax/public: void SpdModel::setDesPar( const SPK_VA::valarray<double>& /desPar/ )/$$ $rend
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
Required for population optimal design.
$pre

$$
$syntax/doSetDesPar( SPK_VA::valarray<double> /desPar/ )/$$ sets $italic desPar$$ as the current
design parameter value and guarantees that subsequent attempts for evaluating the mathematical models
expressed by SpdModel virtual member functions are evaluated at the point.  
$pre

$$
$syntax/doSetDesPar()/$$ shall be only called via its corresponding
public interface, $syntax/setDesPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code setDesPar()$$ catches any exception thrown by the private interface,
$code doSetDesPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $code SpkError$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPD_MODEL_SET_DES_ERR$$.

$head Arguments$$
$syntax/
desPar
/$$
is a design parameter vector of size $math%nDesPar%$$, where $math%nDesPar > 0%$$.

$head Example$$
See $xref/SpdModel/Example/Example/$$

$end
*/
void SpdModel::doSetDesPar( const valarray<double>& desPar )
{
  throw SpkException(
     SpkError::SPD_MODEL_NOT_IMPLEMENTED_ERR, 
     "doSetDesPar() is not implemented",
     __LINE__, __FILE__);
}

void SpdModel::setDesPar( const valarray<double> &desPar )
{
    int nDesPar = desPar.size();
    assert( nDesPar > 0 );
    try{
      doSetDesPar(desPar);
    }
    catch( SpkException& e )
    {
      throw e.push(
          SpkError::SPD_MODEL_SET_DES_ERR, 
          "User-implemented doSetDesPar() threw an SpkException",
          __LINE__, 
          __FILE__ 
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "User-implemented doSetDesPar() threw std::exception", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPD_MODEL_SET_DES_ERR, 
            "User-implemented doSetDesPar() threw an std::exception",
            __LINE__, 
            __FILE__ 
            );
    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPD_MODEL_SET_DES_ERR, 
          "User-implemented doSetDesPar() threw an unknown exception",
          __LINE__, 
          __FILE__
        );
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Design State Variable Not Set for Data Covariance - Mitch]
    //
    // When the Covariance class hierarchy is finalized, then the 
    // following code block may become obsolete and ready for deletion.
    // Because the block is currently commented out, the data covariance 
    // object will not be notified when the design variable changes and
    // therefore it will not be able to cache its value for those times
    // where the design variable remains the same.
    /*
    DoubleMatrix desParDM(desPar, 1);

    try{
      pDataCovariance  ->setCovDesPar(desPar);
    }
    catch( SpkException& e )
    {
      throw e.push(
          SpkError::SPD_MODEL_SET_DES_ERR, 
          "Data Covariance's setCovDesPar() threw an SpkException",
          __LINE__, 
          __FILE__
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "Data Covariance's setCovDesPar() threw std::exception", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPD_MODEL_SET_DES_ERR, 
            "User-implemented setCovDesPar() threw an std::exception",
            __LINE__, 
            __FILE__ 
            );
    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPD_MODEL_SET_DES_ERR, 
          "Data Covariance's setCovDesPar() threw an unknown exception",
          __LINE__, 
          __FILE__
        );
    } 
    */
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
/*************************************************************************
 *
 * Virtual function: doDataMean_desPar()
 * and corresponding public interface: dataMean_desPar()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpdModel_dataMean_desPar$$
$spell
	Model model
   SPK_VA
   Ny 
   fi 
   th
   const
   Spk
   ind
   bool
   valarray
$$

$section Model for the Derivative of Mean of Individual's Data with respect to Design Parameter$$

$index SpdModel, doDataMean_desPar( SPK_VA::valarray<double>& )$$
$index SpdModel, dataMean_desPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the mean of data with respect to design parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpdModel::doDataMean_desPar( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpdModel::dataMean_desPar( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpdModel_dataMean//dataMean()/$$ $rend
$cend
$xref/SpdModel_dataMean_indPar//dataMean_indPar()/$$ $rend 
$cend
$xref/SpdModel_dataMean_popPar//dataMean_popPar()/$$ $rend 
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
Required for population optimal design.
$pre

$$
$code SpdModel::doDataMean_desPar()$$ calculates the derivative of the mean of a particular individual's
data with respect to 
$math%
               -          -
    chi_i  =  |  x_i       |  
              |  x_common  |
               -          -

%$$
for a currently selected individual (see $xref/SpdModel_selectIndividual//selectIndividual/$$) at
the currently set individual parameter (see $xref/SpdModel_setIndPar//setIndPar/$$), 
population parameter (see $xref/SpdModel_setPopPar//setPopPar/$$), 
and design parameter (see $xref/SpdModel_setDesPar//setDesPar/$$).  
$pre

$$
Note that the full design parameter vector $math%x%$$ contains the design vectors 
$math%x_i%$$ for all $math%M%$$ of the individuals in the population 
together with $math%x_common%$$, which is a vector of design 
parameters common to all of the individuals.
That is,
$math%
           -          -
          |  x_1       |
          |  x_2       |
          |   .        |
    x  =  |   .        |  ,
          |   .        |
          |  x_M       |
          |  x_common  |
           -          -

%$$
and $math%chi_i%$$ is therefore the combination of design parameters that each 
individuals' model functions will depend on.
$pre

$$
The evaluation result of $code doDataMean_desPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataMean_desPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataMean_desPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code dataMean_desPar()$$ catches any exception thrown by the private interface,
$code doDataMean_desPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $code SpkError$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPD_MODEL_DATA_MEAN_DES_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nY_i * nChi_i%$$, where $math%nY_i%$$ is the
number of measurements for a currently selected individual and $math%nChi_i%$$ is
the number of design parameters in $math%chi_i%$$.  Let $math%f%$$ denote the function expressed by 
$code doDataMean()$$. The resulting vector contains a sequence of partial derivatives
of $math%f(chi_i, popPar, indPar)%$$ with respect to each element of $math%chi_i%$$ (design parameter subvector):
$math%

  ret = { f(chi_i, popPar, indPar)_chi(1), f(chi_i, popPar, indPar)_chi(2), ... f(chi_i, popPar, indPar)_chi(nChi_i) }

%$$
where $math%chi(k)%$$ denotes the $math%k%$$-th element of $math%chi_i%$$.

$head Example$$
See $xref/SpdModel/Example/Example/$$

$end
*/
bool SpdModel::doDataMean_desPar( valarray<double>& ret ) const
{
   throw SpkException(
       SpkError::SPD_MODEL_NOT_IMPLEMENTED_ERR, 
       "doDataMean_desPar() is not implemented",
       __LINE__, 
       __FILE__
     );
}
bool SpdModel::dataMean_desPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataMean_desPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPD_MODEL_DATA_MEAN_DES_ERR, 
        "User-implemented doDataMean_desPar() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataMean_desPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPD_MODEL_DATA_MEAN_DES_ERR, 
        "User-implemented doDataMean_desPar() threw an std::exception",
        __LINE__, 
        __FILE__ 
        );  
  }   
  catch(...)
  {
    throw SpkException(
        SpkError::SPD_MODEL_DATA_MEAN_DES_ERR, 
        "User-implemented doDataMean_desPar() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: doDataMean_indPar_desPar()
 * and corresponding public interface: dataMean_indPar_desPar()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpdModel_dataMean_indPar_desPar$$
$spell
	Model model
   SPK_VA
   Ny 
   fi 
   th
   const
   Spk
   ind
   bool
   valarray
$$

$section Model for the Derivative of Mean of Individual's Data with respect to Individual and Design Parameters$$

$index SpdModel, doDataMean_indPar_desPar( SPK_VA::valarray<double>& )$$
$index SpdModel, dataMean_indPar_desPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the mean of data with respect to design parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpdModel::doDataMean_indPar_desPar( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpdModel::dataMean_indPar_desPar( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpdModel_dataMean//dataMean()/$$ $rend
$cend
$xref/SpdModel_dataMean_indPar//dataMean_indPar()/$$ $rend 
$cend
$xref/SpdModel_dataMean_popPar//dataMean_popPar()/$$ $rend 
$cend
$xref/SpdModel_dataMean_desPar//dataMean_desPar()/$$ $rend 
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
Required for population optimal design.
$pre

$$
$code SpdModel::doDataMean_indPar_desPar()$$ calculates the
derivative of the mean of a particular individual's
data with respect to the individual parameter 
and then with respect to 
$math%
               -          -
    chi_i  =  |  x_i       |  
              |  x_common  |
               -          -

%$$
for a currently selected individual (see $xref/SpdModel_selectIndividual//selectIndividual/$$) at
the currently set individual parameter (see $xref/SpdModel_setIndPar//setIndPar/$$), 
population parameter (see $xref/SpdModel_setPopPar//setPopPar/$$), 
and design parameter (see $xref/SpdModel_setDesPar//setDesPar/$$).  
$pre

$$
Note that the full design parameter vector $math%x%$$ contains the design vectors 
$math%x_i%$$ for all $math%M%$$ of the individuals in the population 
together with $math%x_common%$$, which is a vector of design 
parameters common to all of the individuals.
That is,
$math%
           -          -
          |  x_1       |
          |  x_2       |
          |   .        |
    x  =  |   .        |  ,
          |   .        |
          |  x_M       |
          |  x_common  |
           -          -

%$$
and $math%chi_i%$$ is therefore the combination of design parameters that each 
individuals' model functions will depend on.
$pre

$$
The evaluation result of $code doDataMean_indPar_desPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataMean_indPar_desPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataMean_indPar_desPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code dataMean_indPar_desPar()$$ catches any exception thrown by the private interface,
$code doDataMean_indPar_desPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $code SpkError$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPD_MODEL_DATA_MEAN_IND_DES_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nY_i * nIndPar * nChi_i%$$, 
where $math%nY_i%$$ is the number of measurements for the currently 
selected individual, 
$math%nIndPar%$$ is the size of the individual parameter vector, 
and $math%nChi_i%$$ is the number of design parameters in 
$math%chi_i%$$.  
Let $math%f_indPar%$$ denote the function expressed by 
$code doDataMean_indPar()$$. 
The resulting vector contains a sequence of partial derivatives
of $math%f_indPar(chi_i, popPar, indPar)%$$
with respect to each element of $math%chi_i%$$ (design 
parameter subvector):
$math%

  ret = { f_indPar(chi_i, popPar, indPar)_chi(1), f_indPar(chi_i, popPar, indPar)_chi(2), ... f_indPar(chi_i, popPar, indPar)_chi(nChi_i) }

%$$
where $math%chi(k)%$$ denotes the $math%k%$$-th element of $math%chi_i%$$.

$head Example$$
See $xref/SpdModel/Example/Example/$$

$end
*/
bool SpdModel::doDataMean_indPar_desPar( valarray<double>& ret ) const
{
   throw SpkException(
       SpkError::SPD_MODEL_NOT_IMPLEMENTED_ERR, 
       "doDataMean_indPar_desPar() is not implemented",
       __LINE__, 
       __FILE__
     );
}
bool SpdModel::dataMean_indPar_desPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataMean_indPar_desPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPD_MODEL_DATA_MEAN_IND_DES_ERR, 
        "User-implemented doDataMean_indPar_desPar() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataMean_indPar_desPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPD_MODEL_DATA_MEAN_IND_DES_ERR, 
        "User-implemented doDataMean_indPar_desPar() threw an std::exception",
        __LINE__, 
        __FILE__ 
        );  
  }   
  catch(...)
  {
    throw SpkException(
        SpkError::SPD_MODEL_DATA_MEAN_IND_DES_ERR, 
        "User-implemented doDataMean_indPar_desPar() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: doDataMean_indPar_popPar()
 * and corresponding public interface: dataMean_indPar_popPar()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpdModel_dataMean_indPar_popPar$$
$spell
	Model model
   SPK_VA
   Ny 
   fi 
   th
   const
   Spk
   ind
   bool
   valarray
$$

$section Model for the Derivative of Mean of Individual's Data with respect to Individual and Population Parameters$$

$index SpdModel, doDataMean_indPar_popPar( SPK_VA::valarray<double>& )$$
$index SpdModel, dataMean_indPar_popPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the mean of data with respect to design parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpdModel::doDataMean_indPar_popPar( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpdModel::dataMean_indPar_popPar( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpdModel_dataMean//dataMean()/$$ $rend
$cend
$xref/SpdModel_dataMean_indPar//dataMean_indPar()/$$ $rend 
$cend
$xref/SpdModel_dataMean_popPar//dataMean_popPar()/$$ $rend 
$cend
$xref/SpdModel_dataMean_desPar//dataMean_desPar()/$$ $rend 
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
Required for population optimal design.
$pre

$$
$code SpdModel::doDataMean_indPar_popPar()$$ calculates the
derivative of the mean of a particular individual's
data with respect to the individual parameter 
and then with respect to the population parameter
for a currently selected individual (see $xref/SpdModel_selectIndividual//selectIndividual/$$) at
the currently set individual parameter (see $xref/SpdModel_setIndPar//setIndPar/$$), 
population parameter (see $xref/SpdModel_setPopPar//setPopPar/$$), 
and design parameter (see $xref/SpdModel_setDesPar//setDesPar/$$).  
$pre

$$
The evaluation result of $code doDataMean_indPar_popPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataMean_indPar_popPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataMean_indPar_popPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code dataMean_indPar_popPar()$$ catches any exception thrown by the private interface,
$code doDataMean_indPar_popPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $code SpkError$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPD_MODEL_DATA_MEAN_IND_POP_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nY_i * nIndPar * nPopPar%$$, 
where $math%nY_i%$$ is the number of measurements for the currently 
selected individual, 
$math%nIndPar%$$ is the size of the individual parameter vector, 
and $math%nPopPar%$$ is the size of population parameter vector.
Let $math%f_indPar%$$ denote the function expressed by 
$code doDataMean_indPar()$$. 
The resulting vector contains a sequence of partial derivatives
of $math%f_indPar(chi_i, popPar, indPar)%$$
with respect to each element of $math%popPar%$$ (population
parameter vector):
$math%

  ret = { f_indPar(chi_i, popPar, indPar)_popPar(1), f_indPar(chi_i, popPar, indPar)_popPar(2), ... f_indPar(chi_i, popPar, indPar)_popPar(nPopPar) }

%$$
where $math%popPar(k)%$$ denotes the $math%k%$$-th element of $math%popPar%$$.

$head Example$$
See $xref/SpdModel/Example/Example/$$

$end
*/
bool SpdModel::doDataMean_indPar_popPar( valarray<double>& ret ) const
{
   throw SpkException(
       SpkError::SPD_MODEL_NOT_IMPLEMENTED_ERR, 
       "doDataMean_indPar_popPar() is not implemented",
       __LINE__, 
       __FILE__
     );
}
bool SpdModel::dataMean_indPar_popPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataMean_indPar_popPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPD_MODEL_DATA_MEAN_IND_POP_ERR, 
        "User-implemented doDataMean_indPar_popPar() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataMean_indPar_popPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPD_MODEL_DATA_MEAN_IND_POP_ERR, 
        "User-implemented doDataMean_indPar_popPar() threw an std::exception",
        __LINE__, 
        __FILE__ 
        );  
  }   
  catch(...)
  {
    throw SpkException(
        SpkError::SPD_MODEL_DATA_MEAN_IND_POP_ERR, 
        "User-implemented doDataMean_indPar_popPar() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: dataVariance_desPar()
 * and corresponding public interface: dataVariance_desPar()
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpdModel_dataVariance_desPar$$
$spell
	Model model
   SPK_VA
   Ny 
   fi 
   th
   const
   Spk
   ind
   bool
   Ri
   valarray
$$

$section Model for the Derivative of Variance of Individual's Data with respect to Design Parameter$$

$index SpdModel, doDataVariance_desPar( SPK_VA::valarray<double>& )$$
$index SpdModel, dataVariance_desPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the variance of data 
with respect to design parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpdModel::doDataVariance_desPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpdModel::dataVariance_desPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpdModel_dataVariance//dataVariance()/$$ $rend
$cend
$xref/SpdModel_dataVariance_indPar//dataVariance_indPar()/$$ $rend 
$cend
$xref/SpdModel_dataVariance_popPar//dataVariance_popPar()/$$ $rend 
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
Required for population optimal design.
$pre

$$
$code SpdModel::doDataVariance_desPar()$$ calculates the derivative of the variance of a particular individual's
data with respect to 
$math%
               -          -
    chi_i  =  |  x_i       |  
              |  x_common  |
               -          -

%$$
for a currently selected individual (see $xref/SpdModel_selectIndividual//selectIndividual/$$) at
the currently set individual parameter (see $xref/SpdModel_setIndPar//setIndPar/$$), 
population parameter (see $xref/SpdModel_setPopPar//setPopPar/$$), 
and design parameter (see $xref/SpdModel_setDesPar//setDesPar/$$).  
$pre

$$
Note that the full design parameter vector $math%x%$$ contains the design vectors 
$math%x_i%$$ for all $math%M%$$ of the individuals in the population 
together with $math%x_common%$$, which is a vector of design 
parameters common to all of the individuals.
That is,
$math%
           -          -
          |  x_1       |
          |  x_2       |
          |   .        |
    x  =  |   .        |  ,
          |   .        |
          |  x_M       |
          |  x_common  |
           -          -

%$$
and $math%chi_i%$$ is therefore the combination of design parameters that each 
individuals' model functions will depend on.
$pre

$$
The evaluation result of $code doDataVariance_desPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataVariance_desPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataVariance_desPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code doDataVariance_desPar()$$ catches any exception thrown by the private interface,
$code dataVariance_desPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $code SpkError$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPD_MODEL_DATA_VARIANCE_DES_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nY_i * nY_i * nChi_i%$$, where $math%nY_i%$$ is the
number of measurements for a currently selected individual and $math%nChi_i%$$ is
the number of design parameters in $math%chi_i%$$.  Let $math%R%$$ denote the function expressed by 
$code doDataVariance()$$. The resulting vector contains a sequence of partial derivatives
of $math%R(chi_i, popPar, indPar)%$$ with respect to each element of $math%chi_i%$$ (design parameter subvector):
$math%

  ret = { R(chi_i, popPar, indPar)_chi(1), R(chi_i, popPar, indPar)_chi(2), ... R(chi_i, popPar, indPar)_chi(nChi_i) }

%$$
where $math%chi(k)%$$ denotes the $math%k%$$-th element of $math%chi_i%$$.
$math%R(chi_i, popPar, indPar)_chi(k)%$$ holds the symmetric, positive definite property.

$head Example$$
See $xref/SpdModel/Example/Example/$$

$end
*/
bool SpdModel::doDataVariance_desPar( valarray<double>& ret ) const 
{
   throw SpkException(
       SpkError::SPD_MODEL_NOT_IMPLEMENTED_ERR, 
       "doDataVariance_desPar() is not implemented",
       __LINE__, __FILE__
     );
}
bool SpdModel::dataVariance_desPar( valarray<double> & ret ) const 
{ 
  try{
      return doDataVariance_desPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPD_MODEL_DATA_VARIANCE_DES_ERR, 
        "User-implemented doDataVariance_desPar() threw an SpkException",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataVariance_desPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPD_MODEL_DATA_VARIANCE_DES_ERR, 
        "User-implemented doDataVariance_desPar() threw an std::exception",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPD_MODEL_DATA_VARIANCE_DES_ERR, 
        "User-implemented doDataVariance_desPar() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: dataVarianceInv_desPar()
 * and corresponding public interface: dataVarianceInv_desPar()
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpdModel_dataVarianceInv_desPar$$
$spell
	Model model
   SPK_VA
   Ny 
   fi 
   th
   const
   Spk
   ind
   inv
   bool
   Ri
   valarray
$$

$section Model for the Derivative of Inverse of Variance of Individual's Data with respect to Design Parameter$$

$index SpdModel, doDataVarianceInv_desPar( SPK_VA::valarray<double>& )$$
$index SpdModel, dataVarianceInv_desPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the inverse of 
the variance of data with respect to design parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpdModel::doDataVarianceInv_desPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpdModel::dataVarianceInv_desPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpdModel_dataVarianceInv//dataVarianceInv()/$$ $rend
$cend
$xref/SpdModel_dataVarianceInv_indPar//dataVarianceInv_indPar()/$$ $rend 
$cend
$xref/SpdModel_dataVarianceInv_popPar//dataVarianceInv_popPar()/$$ $rend 
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
Required for population optimal design.  A default implementation
is provided.
$pre

$$
$code SpdModel::doDataVarianceInv_desPar()$$ calculates the derivative of the 
inverse of the variance of a particular individual's
data with respect to 
$math%
               -          -
    chi_i  =  |  x_i       |  
              |  x_common  |
               -          -

%$$
for a currently selected individual (see $xref/SpdModel_selectIndividual//selectIndividual/$$) at
the currently set individual parameter (see $xref/SpdModel_setIndPar//setIndPar/$$), 
population parameter (see $xref/SpdModel_setPopPar//setPopPar/$$), 
and design parameter (see $xref/SpdModel_setDesPar//setDesPar/$$).  
$pre

$$
Note that the full design parameter vector $math%x%$$ contains the design vectors 
$math%x_i%$$ for all $math%M%$$ of the individuals in the population 
together with $math%x_common%$$, which is a vector of design 
parameters common to all of the individuals.
That is,
$math%
           -          -
          |  x_1       |
          |  x_2       |
          |   .        |
    x  =  |   .        |  ,
          |   .        |
          |  x_M       |
          |  x_common  |
           -          -

%$$
and $math%chi_i%$$ is therefore the combination of design parameters that each 
individuals' model functions will depend on.
$pre

$$
The evaluation result of $code doDataVarianceInv_desPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataVarianceInv_desPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataVarianceInv_desPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code doDataVarianceInv_desPar()$$ catches any exception thrown by the private interface,
$code dataVarianceInv_desPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $code SpkError$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPD_MODEL_DATA_VARIANCE_INV_DES_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nY_i * nY_i * nChi_i%$$, where $math%nY_i%$$ is the
number of measurements for a currently selected individual and $math%nChi_i%$$ is
the number of design parameters in $math%chi_i%$$.  Let $math%RInv%$$ denote the function expressed by 
$code doDataVarianceInv()$$. The resulting vector contains a sequence of partial derivatives
of $math%RInv(chi_i, popPar, indPar)%$$ with respect to each element of $math%chi_i%$$ (design parameter subvector):
$math%

  ret = { RInv(chi_i, popPar, indPar)_chi(1), RInv(chi_i, popPar, indPar)_chi(2), ... RInv(chi_i, popPar, indPar)_chi(nChi_i) }

%$$
where $math%chi(k)%$$ denotes the $math%k%$$-th element of $math%chi_i%$$.
$math%RInv(chi_i, popPar, indPar)_chi(k)%$$ holds the symmetric, positive definite property.


$head Example$$
See $xref/SpdModel/Example/Example/$$

$end
*/
bool SpdModel::doDataVarianceInv_desPar( valarray<double>& ret ) const
{
    SPK_VA::valarray<double> RiInv;
    dataVarianceInv(RiInv);
    int nY_i = sqrt( RiInv.size() );
    assert( RiInv.size() == nY_i * nY_i );

    DoubleMatrix dmatRiInv( RiInv, nY_i );

    SPK_VA::valarray<double> Ri_chi;
    dataVariance_desPar(Ri_chi);
    int nChi_i = Ri_chi.size() / ( nY_i * nY_i );
    DoubleMatrix dmatRi_chi( Ri_chi, nChi_i );
    assert( Ri_chi.size() == nY_i * nY_i * nChi_i );

    ret = minusAkronBtimesC( dmatRiInv, dmatRiInv, dmatRi_chi ).toValarray();
    return !allZero(ret);
}


bool SpdModel::dataVarianceInv_desPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataVarianceInv_desPar(ret);
  }

  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPD_MODEL_INV_DATA_VARIANCE_DES_ERR, 
        "User-implemented doDataVarianceInv_desPar() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataVarianceInv_desPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPD_MODEL_INV_DATA_VARIANCE_DES_ERR, 
        "User-implemented doDataVarianceInv_desPar() threw an std::exception",
        __LINE__, 
        __FILE__ 
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPD_MODEL_INV_DATA_VARIANCE_DES_ERR, 
        "User-implemented doDataVarianceInv_desPar() threw an unknown exception",
        __LINE__, 
        __FILE__ 
      );
  }
}
/*************************************************************************
 *
 * Virtual function: doPopParPrior()
 * and corresponding public interface: popParPrior()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpdModel_popParPrior$$
$spell
   Model model
   Ny  
   th
   const
   Spd
   pop
   inv
   Covariance
   bool
   Ri
$$

$section Model for the Prior Distribution of Population Parameters$$

$index SpdModel, doPopParPrior( double& )$$
$index SpdModel, popParPrior( double& )$$
$index User-provided Model, prior distribution of the population parameters$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual void SpdModel::doPopParPrior( double& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: void SpdModel::popParPrior( double& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpdModel_popParPrior_popPar//popParPrior_popPar()/$$ $rend 
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
Required for population expected determinant optimal design.
$pre

$$
$code SpdModel::doPopParPrior()$$ calculates the prior distribution of the
population parameters at a currently set population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doPopParPrior()$$
shall be placed in $italic ret$$ and the function returns nothing.
$pre

$$
$syntax/doPopParPrior()/$$ shall be only called via its corresponding
public interface, $syntax/popParPrior()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code popParPrior()$$ catches any exception thrown by the private interface,
$code doPopParPrior()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $code SpkError$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPD_MODEL_POP_PRIOR_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a scalar.
Let $math%p%$$ denote the function expressed by 
$code doPopParPrior()$$. 
The resulting scalar contains the value for the prior distribution 
of the population parameters:
$math%

  ret = p(popPar) .

%$$

$head Example$$
See $xref/SpdModel/Example/Example/$$

$end
*/

void SpdModel::doPopParPrior( double& ret ) const
{
   throw SpkException(
       SpkError::SPD_MODEL_NOT_IMPLEMENTED_ERR,
       "doPopParPrior() is not implemented",
       __LINE__, __FILE__
     );
}


void SpdModel::popParPrior( double& ret ) const 
{ 
  try{
    doPopParPrior(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPD_MODEL_POP_PRIOR_ERR, 
        "User-implemented doPopParPrior() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doPopParPrior() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPD_MODEL_POP_PRIOR_ERR, 
        "User-implemented doPopParPrior() threw an std::exception",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPD_MODEL_POP_PRIOR_ERR, 
        "User-implemented doPopParPrior() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}

/*************************************************************************
 *
 * Virtual function: doPopParPrior_popPar()
 * and corresponding public interface: popParPrior_popPar()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpdModel_popParPrior_popPar$$
$spell
	Model model
   SPK_VA
   Ny  
   th
   const
   Spd
   pop
   inv
   bool
   Ri
   valarray
$$

$section Model for the Derivative of Prior Distribution of Population Parameters with respect to Population Parameter$$

$index SpdModel, doPopParPrior_popPar( SPK_VA::valarray<double>& )$$
$index SpdModel, popParPrior_popPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of prior distribution of the population parameters
with respect to population parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpdModel::doPopParPrior_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpdModel::popParPrior_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpdModel_popParPrior//popParPrior()/$$ $rend 
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
Required for population expected determinant optimal design.
$pre

$$
$code SpdModel::doPopParPrior_popPar()$$ calculates the derivative 
of the prior distribution of the 
population parameters with respect to the population parameter at
a currently set population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doPopParPrior_popPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doPopParPrior_popPar()/$$ shall be only called via its corresponding
public interface, $syntax/popParPrior_popPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code doPopParPrior_popPar()$$ catches any exception thrown by the private interface,
$code popParPrior_popPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $code SpkError$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPD_MODEL_POP_PRIOR_POP_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nPopPar%$$, where 
$math%nPopPar%$$ is the size of population parameters.  
Let $math%p%$$ denote the function expressed by 
$code doPopParPrior()$$. 
The resulting vector contains a sequence of partial
derivatives of $math%p(popPar)%$$ with respect to each 
element of $math%popPar%$$ (population parameter vector):
$math%

  ret = { p(popPar)_popPar(1), p(popPar)_popPar(2), ... p(popPar)_popPar(nPopPar) }

%$$
where $math%popPar(k)%$$ denotes the $math%k%$$-th element of $math%popPar%$$ vector.

$head Example$$
See $xref/SpdModel/Example/Example/$$

$end
*/
bool SpdModel::doPopParPrior_popPar( valarray<double>& ret ) const
{
   throw SpkException(
       SpkError::SPD_MODEL_NOT_IMPLEMENTED_ERR, 
       "doPopParPrior_popPar() is not implemented",
       __LINE__, __FILE__
     );
}


bool SpdModel::popParPrior_popPar( valarray<double>& ret ) const 
{ 
  try{
    return doPopParPrior_popPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPD_MODEL_POP_PRIOR_POP_ERR, 
        "User-implemented doPopParPrior_popPar() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doPopParPrior_popPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPD_MODEL_POP_PRIOR_POP_ERR, 
        "User-implemented doPopParPrior_popPar() threw an std::exception",
        __LINE__, 
        __FILE__ 
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPD_MODEL_POP_PRIOR_POP_ERR, 
        "User-implemented doPopParPrior_popPar() threw an unknown exception",
        __LINE__, 
        __FILE__ 
      );
  }
}
//*************************************************************************
//
//
//            Local functions
//
//
//*************************************************************************

static DoubleMatrix minusAkronBtimesC( const DoubleMatrix &A, const DoubleMatrix &B, const DoubleMatrix &C )
{
    return mulByScalar(AkronBtimesC( A, B, C ), -1.);
}

static void minusAkronBtimesC( const DoubleMatrix &A, const DoubleMatrix &B, const DoubleMatrix &C, DoubleMatrix & ret)
{
    ret = mulByScalar(AkronBtimesC( A, B, C ), -1.);
}



