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
 * File: SpkModel.cpp
 *
 * Definitions of SpkModel class members.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <cmath>

#include "SpkValarray.h"
#include "SpkModel.h"
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

static int _nIndPar = -1;
static int _nPopPar = -1;
static int _nY      = -1;

//*************************************************************************
//
//
//            SpkModel Constructors, Assignment Operators, and Destructors
//
//
//*************************************************************************
/*------------------------------------------------------------------------
 * Default constructor
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_default_constructor$$
$spell
	Model model 
    Spk
$$

$section Default Constructor$$

$index User-provided Model, Default Constructor$$

$table
$bold Prototype:$$   $cend  
$syntax/protected SpkModel::SpkModel()/$$
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The default constructor.  
$pre

$$
Since SpkModel is an abstract class, the direct use of this constructor
is only allowed for its subclasses.

$head Example$$
See $xref/SpkModel/Example/Example/$$
$end
*/

SpkModel::SpkModel()
: pDataCovariance(NULL), pIndParCovariance(NULL)
{
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Backward Compatable SpkModel Code - Mitch]
  // This code is temporary and should be deleted once all of 
  // the old SpkModel remnants are gone.
  //
  pDataCovariance   = new FullDataCovariance;
  pIndParCovariance = new FullIndParCovariance;

  pDataCovariance  ->setModel( this );
  pIndParCovariance->setModel( this );
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
/*------------------------------------------------------------------------
 * Copy constructor
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_copy_constructor$$
$spell
	Model model 
    const
    copyable
    Spk
$$

$section Copy Constructor$$

$index User-provided Model, Copy Constructor$$

$table
$bold Prototype:$$   $cend  
$syntax/protected SpkModel::SpkModel( const SpkModel &/right/ )/$$
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The copy constructor.  
$pre

$$
Since SpkModel is an abstract class, the direct use of this constructor
is only allowed for its subclasses.
$pre

$$
The copy constructor is provided to make the SpkModel class
more standard even though SPK itself does not currently require
its model classes to be copyable.

$head Arguments$$
$syntax/
right
/$$
The SpkModel to be copied.

$end
*/
SpkModel::SpkModel( const SpkModel &right )
: pDataCovariance(NULL), pIndParCovariance(NULL)
{
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Backward Compatable SpkModel Code - Mitch]
  // This code is temporary and should be deleted once all of 
  // the old SpkModel remnants are gone.
  //

  // These covariances should already have been constructed.
  assert( right.pDataCovariance );
  assert( right.pIndParCovariance );

  // These do not have copy constructors.
  pDataCovariance   = new FullDataCovariance();
  pIndParCovariance = new FullIndParCovariance();

  pDataCovariance  ->setModel( this );
  pIndParCovariance->setModel( this );
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

/*------------------------------------------------------------------------
 * Assigment operator
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_assignment_operator$$
$spell
	Model model 
    assignable
    const
    Spk
$$

$section Assignment Operator$$

$index User-provided Model, Assignment Operator$$

$table
$bold Prototype:$$   $cend  
$syntax/protected SpkModel::SpkModel& operator=( const SpkModel &/right/ )/$$
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The assignment operator.  
$pre

$$
Since SpkModel is an abstract class, the direct use of this operator
is only allowed for its subclasses.
$pre

$$
The assignment operator is provide to make the SpkModel class
more standard even though SPK itself does not currently require
its model classes to be assignable.

$head Arguments$$
$syntax/
right
/$$
The SpkModel to be copied.

$end
*/
SpkModel& SpkModel::operator=(const SpkModel &right)
{
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Backward Compatable SpkModel Code - Mitch]
  // This code is temporary and should be deleted once all of 
  // the old SpkModel remnants are gone.
  //

  // These covariances should already have been constructed.
  assert( pDataCovariance );
  assert( pIndParCovariance );

  if ( pDataCovariance )
  {
    delete pDataCovariance;
  }
  if ( pIndParCovariance )
  {
    delete pIndParCovariance;
  }

  // These covariances should already have been constructed.
  assert( right.pDataCovariance );
  assert( right.pIndParCovariance );

  // These do not have copy constructors.
  pDataCovariance   = new FullDataCovariance();
  pIndParCovariance = new FullIndParCovariance();

  pDataCovariance  ->setModel( this );
  pIndParCovariance->setModel( this );
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  return *this;
}

/*------------------------------------------------------------------------
 * Default destructor
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_default_destructor$$
$spell
	Model model 
    Spk
$$

$section Default Destructor$$

$index User-provided Model, Default Destructor$$

$table
$bold Prototype:$$   $cend  
$syntax/public virtual SpkModel::~SpkModel()/$$
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The default destructor.  

$end
*/
SpkModel::~SpkModel()
{
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Backward Compatable SpkModel Code - Mitch]
  // This code is temporary and should be deleted once all of 
  // the old SpkModel remnants are gone.
  //
  delete pDataCovariance;
  delete pIndParCovariance;
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

//*************************************************************************
//
//
//     SpkModel Virtual Members and Corresponding Public Interfaces
//
//
//*************************************************************************
/*************************************************************************
 *
 * Virtual function: private: doSelectIndividual(int)
 * and corresponding public: selectIndividual(int)
 *
 *************************************************************************/
/*
$begin SpkModel_selectIndividual$$
$spell
	Model model 
    int
    void
    covariances
    -th
    Spk
    Ind
    const
    fi
    cerr
    endl
    Spk
    Ri
    valarray
$$

$section Select Individual$$

$index SpkModel, doSelectIndividual()$$
$index SpkModel, selectIndividual()$$
$index User-provided Model, select an individual in a population$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual void SpkModel::doSelectIndividual( int /i/ )/$$ $rend
$bold Public Interface:$$ $rend
$syntax/public: void SpkModel::selectIndividual( int /i/ )/$$ $rend
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
Required for population analysis.
$pre

$$
$syntax/doSelectIndividual( int /i/ )/$$ selects $italic i$$-th individual in the
population and guarantees that subsequent attempts for evaluating the mathematical models
expressed by SpkModel virtual member functions yield to the corresponding individual's
values at the current parameters.  The valid value range for $italic i$$ is 0 to
N-1, where N is the number of individuals in the population.  $math%i=0%$$ shall
select the first individual.
$pre

$$
$syntax/doSelectIndividual()/$$ shall be only called via its corresponding
public interface, $syntax/selectIndividual()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code selectIndividual()$$ catches any exception thrown by the private interface,
$code doSelectIndividual()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_SET_INDEX_ERR$$.

$head Arguments$$
$syntax/
i
/$$
An integer indicating the individual to be selected.
The valid value range is from 0 to N-1, where N is the number of
individuals in the population.  
0 shall point to the first individual in the population.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/

void SpkModel::doSelectIndividual(int base0)
{
 throw SpkException(
     SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR,
     "doSelectIndividual(int) is not implemented",
     __LINE__, __FILE__);
}

void SpkModel::selectIndividual(int base0)
{
  try{
	doSelectIndividual(base0);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "User-implemented selectIndividual() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doSelectIndividual() threw an std::exception", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "User-implemented doSelectIndividual() threw an std::exception",
        __LINE__, 
        __FILE__
        );
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "User-implemented doSelectIndividual() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }

  try{
    pDataCovariance->selectCovIndividual(base0);   // Give doDataVariance(a, bi) the value.
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "Data Covariance's selectCovIndividual() threw an SpkException",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "Data Covariance's setIndex() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "User-implemented selectCovIndividual() threw an std::exception",
        __LINE__, 
        __FILE__ 
        );
  }  
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "Data Covariance's selectCovIndividual() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}


/*************************************************************************
 *
 * Virtual function: private: doSetPopPar( const SPK_VA::valarray<double>& )
 * and corresponding public interface: setPopPar( const SPK_VA::valarray<double>& )
 *
 *************************************************************************/
/*
$begin SpkModel_setPopPar$$
$spell
	Model model 
    int
    void
    covariances
    -th
    Spk
    Ind
    const
    fi
    cerr
    endl
    Spk
    Ri
    valarray
$$

$section Set Population Parameter$$

$index SpkModel, doSetPopPar()$$
$index SpkModel, setPopPar()$$
$index User-provided Model, set population parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual void SpkModel::doSetPopPar( const SPK_VA::valarray<double>& /popPar/ )/$$ $rend
$bold Public Interface:$$   $rend  
$syntax/public: void SpkModel::setPopPar( const SPK_VA::valarray<double>& /popPar/ )/$$ $rend
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
Required for population analysis.
$pre

$$
$syntax/doSetPopPar( SPK_VA::valarray<double> /popPar/ )/$$ sets $italic popPar$$ as the current
population parameter value and guarantees that subsequent attempts for evaluating the mathematical models
expressed by SpkModel virtual member functions are evaluated at the point.  
$pre

$$
$syntax/doSetPopPar()/$$ shall be only called via its corresponding
public interface, $syntax/setPopPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code setPopPar()$$ catches any exception thrown by the private interface,
$code doSetPopPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_SET_POP_ERR$$.

$head Arguments$$
$syntax/
popPar
/$$
is a population parameter vector of size $math%nPopPar%$$, where $math%nPopPar > 0%$$.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
void SpkModel::doSetPopPar( const valarray<double>& popPar )
{
  throw SpkException(
     SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
     "doSetPopPar() is not implemented",
     __LINE__, __FILE__);
}

void SpkModel::setPopPar( const valarray<double> &popPar )
{
    _nPopPar = popPar.size();
    assert( _nPopPar > 0 );
    try{
      doSetPopPar(popPar);
    }
    catch( SpkException& e )
    {
      throw e.push(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "User-implemented doSetPopPar() threw an SpkException",
          __LINE__, 
          __FILE__ 
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "User-implemented doSetPopPar() threw std::exception", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPK_MODEL_SET_POP_ERR, 
            "User-implemented doSetPopPar() threw an std::exception",
            __LINE__, 
            __FILE__ 
            );
    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "User-implemented doSetPopPar() threw an unknown exception",
          __LINE__, 
          __FILE__
        );
    }

    DoubleMatrix popParDM(popPar, 1);

    try{
      pDataCovariance  ->setCovPopPar(popPar);
    }
    catch( SpkException& e )
    {
      throw e.push(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "Data Covariance's setCovPopPar() threw an SpkException",
          __LINE__, 
          __FILE__
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "Data Covariance's setCovPopPar() threw std::exception", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPK_MODEL_SET_POP_ERR, 
            "User-implemented setCovPopPar() threw an std::exception",
            __LINE__, 
            __FILE__ 
            );
    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "Data Covariance's setCovPopPar() threw an unknown exception",
          __LINE__, 
          __FILE__
        );
    }        
    try{
      pIndParCovariance->setCovPopPar(popPar);  // Give D(a) the value.
    }
    catch( SpkException& e )
    {
      throw e.push(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "Individual Parameter Covariance's setCovPopPar() threw an SpkException",
          __LINE__, 
          __FILE__
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "Individual Parameter Covariance's setCovPopPar() threw std::exception.", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPK_MODEL_SET_POP_ERR, 
            "User-implemented setCovPopPar() threw an std::exception",
            __LINE__, 
            __FILE__ 
            );


    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "Individual Parameter Covariance's setCovPopPar() threw an unknown exception",
          __LINE__, 
          __FILE__ 
        );
    }
}
/*************************************************************************
 *
 * Virtual function: private: doSetIndPar( const SPK_VA::valarray<double>& )
 * and corresponding public interface: setIndPar( const SPK_VA::valarray<double>& )
 *
 *************************************************************************/
/*
$begin SpkModel_setIndPar$$
$spell
	Model model 
    int
    void
    covariances
    -th
    Spk
    Ind
    const
    fi
    cerr
    endl
    Ri
    valarray
    SPK_VA
$$

$section Set Individual Parameter$$

$index SpkModel, doSetIndPar()$$
$index SpkModel, setIndPar$$
$index User-provided Model, set individual parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual void SpkModel::doSetIndPar( const SPK_VA::valarray<double>&  /indPar/ ) const = 0/$$ $rend
$bold Public Interface:$$ $rend
$syntax/public: void SpkModel::setIndPar( const SPK_VA::valarray<double>&  /indPar/ ) const/$$ $rend
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
Required for both population analysis and individual analysis.
$pre

$$
$syntax/SpkModel::doSetIndPar( const SPK_VA::valarray<double>&  /indPar/ )/$$ sets $italic indPar$$ as the current
individual parameter value and guarantees that subsequent attempts for evaluating the mathematical models
expressed by SpkModel virtual member functions are evaluated at the point.  
$pre

$$
$syntax/doSetPopPar()/$$ shall be only called via its corresponding
public interface, $syntax/setIndPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code setIndPar()$$ catches any exception thrown by the private interface,
$code doSetIndPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_SET_IND_ERR$$.

$head Arguments$$
$syntax/
indPar
/$$
is an individual parameter vector of size $math%nIndPar%$$, where $math%nIndPar > 0%$$.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/

void SpkModel::setIndPar(const valarray<double> &indPar)
{
    _nIndPar = indPar.size();
    assert( _nIndPar > 0 );

    try{
      doSetIndPar(indPar);                          // Give model subclasses the value.
    }
    catch( SpkException& e )
    {
      throw e.push(
          SpkError::SPK_MODEL_SET_IND_ERR, 
          "User-implemented doSetIndPar() threw an SpkException",
          __LINE__, 
          __FILE__
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "User-implemented doSetIndPar() threw std::exception.", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPK_MODEL_SET_IND_ERR, 
            "User-implemented doSetIndPar() threw an std::exception",
            __LINE__, 
            __FILE__
            );
    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPK_MODEL_SET_IND_ERR, 
          "User-implemented doSetIndPar() threw an unknown exception",
          __LINE__, 
          __FILE__ 
        );
    }

    DoubleMatrix indParDM(indPar, 1);
    try{
      pDataCovariance->setCovIndPar(indPar);    // Give doDataVariance(a, bi) the value.
    }
    catch( SpkException& e )
    {
      throw e.push(
          SpkError::SPK_MODEL_SET_IND_ERR, 
          "Data Covariance's setCovIndPar() threw an SpkException",
          __LINE__, 
          __FILE__
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "Data Covariance's setCovIndPar() threw std::exception.", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPK_MODEL_SET_IND_ERR, 
            "User-implemented setCovIndPar() threw an std::exception",
            __LINE__, 
            __FILE__
            );
    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPK_MODEL_SET_IND_ERR, 
          "Data Covariance's setCovIndPar() threw an unknown exception",
          __LINE__, 
          __FILE__ 
        );
    }        
}
/*************************************************************************
 *
 * Virtual function: doDataMean( SPK_VA::valarray<double>& )
 * and corresponding public interface: dataMean( SPK_VA::valarray<double>& )
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_dataMean$$
$spell
	Model model
   SPK_VA
   Ny 
   fi 
   th
   const
   ind
   Spk
   bool
   Ri
   valarray
$$

$section Model for the Mean of Individual's Data$$

$index SpkModel, doDataMean( SPK_VA::valarray<double> & )$$
$index SpkModel, dataMean( SPK_VA::valarray<double> & )$$
$index User-provided Model, mean of data$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual void SpkModel::doDataMean( SPK_VA::valarray<double>& /ret/ ) const = 0/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: void SpkModel::dataMean( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_dataMean_indPar//dataMean_indPar()/$$ $rend
$cend
$xref/SpkModel_dataMean_popPar//dataMean_popPar()/$$ $rend 
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
Required for both population and individual analysis.
$pre

$$
$code SpkModel::doDataMean()$$ calculates the mean of a particular individual's
data for a currently selected individual (see $xref/SpkModel_selectIndividual//selectIndividual/$$) at
a currently set individual parameter (see $xref/SpkModel_setIndPar//setIndPar/$$) 
and a population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doDataMean()$$
shall be placed in $italic ret$$ and the function returns nothing.
$pre

$$
$syntax/doDataMean()/$$ shall be only called via its corresponding
public interface, $syntax/dataMean()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code dataMean()$$ catches any exception thrown by the private interface,
$code doDataMean()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_MEAN_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nYi%$$, where $math%nYi%$$ is the
number of currently selected individual's data.


$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/

void SpkModel::dataMean( valarray<double> & ret ) const 
{ 
  try{
    doDataMean(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_ERR, 
        "User-implemented doDataMean() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataMean() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_ERR, 
        "User-implemented doDataMean() threw an std::exception",
        __LINE__, 
        __FILE__
        );
  }    
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR, 
        "User-implemented doDataMean() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: doDataMean_indPar()
 * and corresponding public interface: dataMean_indPar()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_dataMean_indPar$$
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

$section Model for the Derivative of Mean of Individual's Data with respect to Individual Parameter$$

$index SpkModel, doDataMean_indPar( SPK_VA::valarray<double>& )$$
$index SpkModel, dataMean_indPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the mean of data with respect to individual parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpkModel::doDataMean_indPar( SPK_VA::valarray<double>& /ret/ ) const = 0/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpkModel::dataMean_indPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_dataMean//dataMean()/$$ $rend
$cend
$xref/SpkModel_dataMean_popPar//dataMean_popPar()/$$ $rend 
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
Required for both population and individual analysis.
$pre

$$
$code SpkModel::doDataMean_indPar()$$ calculates the derivative of the mean of a particular individual's
data with respect to the individual parameter,
for a currently selected individual (see $xref/SpkModel_selectIndividual//selectIndividual/$$) at
a currently set individual parameter (see $xref/SpkModel_setIndPar//setIndPar/$$) 
and population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doDataMean_indPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataMean_indPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataMean_indPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code dataMean_indPar()$$ catches any exception thrown by the private interface,
$code doDataMean_indPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_MEAN_IND_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nYi * nIndPar%$$, where $math%nYi%$$ is the
number of measurements for a currently selected individual and $math%nIndPar%$$ is
the size of individual parameter vector.  Let $math%f%$$ denotes the function expressed by 
$code doDataMean$$. The resulting vector contains a sequence of partial derivatives
of $math%f(popPar, indPar)%$$ with respect to each element of $math%indPar%$$ (individual parameter vector):
$math%

  ret = { f(popPar, indPar)_indPar(1), f(popPar, indPar)_indPar(2), ... f(popPar, indPar)_indPar(nIndPar) }

%$$
where $math%indPar(k)%$$ denotes $math%k%$$-th element of $math%indPar%$$ vector.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/

bool SpkModel::dataMean_indPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataMean_indPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_IND_ERR, 
        "User-implemented doDataMean_indPar() threw an SpkException",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataMean_indPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_IND_ERR, 
        "User-implemented doDataMean_indPar() threw an std::exception",
        __LINE__, 
        __FILE__
        );
  } 
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_IND_ERR, 
        "User-implemented doDataMean_indPar() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: doDataMean_popPar()
 * and corresponding public interface: dataMean_popPar()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_dataMean_popPar$$
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

$section Model for the Derivative of Mean of Individual's Data with respect to Population Parameter$$

$index SpkModel, doDataMean_popPar( SPK_VA::valarray<double>& )$$
$index SpkModel, dataMean_popPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the mean of data with respect to population parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpkModel::doDataMean_popPar( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpkModel::dataMean_popPar( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_dataMean//dataMean()/$$ $rend
$cend
$xref/SpkModel_dataMean_indPar//dataMean_indPar()/$$ $rend 
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
Required for population analysis.
$pre

$$
$code SpkModel::doDataMean_popPar()$$ calculates the derivative of the mean of a particular individual's
data with respect to the population parameter,
for a currently selected individual (see $xref/SpkModel_selectIndividual//selectIndividual/$$) at
a currently set individual parameter (see $xref/SpkModel_setIndPar//setIndPar/$$) 
and population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doDataMean_popPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataMean_popPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataMean_popPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code dataMean_popPar()$$ catches any exception thrown by the private interface,
$code doDataMean_popPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_MEAN_POP_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nYi * nPopPar%$$, where $math%nYi%$$ is the
number of measurements for a currently selected individual and $math%nPopPar%$$ is
the size of population parameter vector.  Let $math%f%$$ denotes the function expressed by 
$code doDataMean()$$. The resulting vector contains a sequence of partial derivatives
of $math%f(popPar, indPar)%$$ with respect to each element of $math%popPar%$$ (population parameter vector):
$math%

  ret = { f(popPar, indPar)_popPar(1), f(popPar, indPar)_popPar(2), ... f(popPar, indPar)_popPar(nPopPar) }

%$$
where $math%popPar(k)%$$ denotes $math%k%$$-th element of $math%popPar%$$ vector.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
bool SpkModel::doDataMean_popPar( valarray<double>& retVA ) const
{
   throw SpkException(
       SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
       "doDataMean_popPar() is not implemented",
       __LINE__, 
       __FILE__
     );
}
bool SpkModel::dataMean_popPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataMean_popPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_POP_ERR, 
        "User-implemented doDataMean_popPar() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataMean_popPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_POP_ERR, 
        "User-implemented doDataMean_popPar() threw an std::exception",
        __LINE__, 
        __FILE__ 
        );  
  }   
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_POP_ERR, 
        "User-implemented doDataMean_popPar() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: doDataVariance()
 * and corresponding public interface: dataVariance()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_dataVariance$$
$spell
	Model model
   SPK_VA
   Ny 
   th
   const
   Spk
   Covariance
   bool
   ind
   Ri
   valarray
$$
$section Model for the Variance of Individual's Data$$

$index SpkModel, doDataVariance( SPK_VA::valarray<double>& )$$
$index SpkModel, dataVariance( SPK_VA::valarray<double>& )$$
$index User-provided Model, variance of data$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual void SpkModel::doDataVariance( SPK_VA::valarray<double>& /ret/ ) const = 0/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: void SpkModel::dataVariance( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_dataVariance_indPar//dataVariance_indPar()/$$ $rend
$cend
$xref/SpkModel_dataVariance_popPar//dataVariance_popPar()/$$ $rend 
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
Required for both population and individual analysis.
$pre

$$
$code SpkModel::doDataVariance()$$ calculates the variance of a particular individual's
data for a currently selected individual (see $xref/SpkModel_selectIndividual//selectIndividual/$$) at
a currently set individual parameter (see $xref/SpkModel_setIndPar//setIndPar/$$) 
and population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doDataVariance()$$
shall be placed in $italic ret$$ and the function returns nothing.
$pre

$$
$syntax/doDataVariance()/$$ shall be only called via its corresponding
public interface, $syntax/dataVariance()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code dataVariance()$$ catches any exception thrown by the private interface,
$code doDataVariance()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_VARIANCE_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nYi * nYi%$$, where $math%nYi%$$ is the
number of currently selected individual's data.  The vector contains a column-ordered
matrix of dimensions, $math%nYi by nYi%$$, that is
symmetric and positive definite.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/

void SpkModel::dataVariance( valarray<double>& ret ) const 
{ 
  try{
    doDataVariance(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_ERR, 
        "User-implemented doDataVariance() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataVariance() threw std::exception", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_ERR, 
        "User-implemented doDataVariance() threw an std::exception",
        __LINE__, 
        __FILE__
        );  
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_VARIANCE_ERR, 
        "User-implemented doDataVariance() threw an unknown exception",
        __LINE__, 
        __FILE__ 
      );
  }
}
/*************************************************************************
 *
 * Virtual function: dataVariance_indPar()
 * and corresponding public interface: dataVariance_indPar()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_dataVariance_indPar$$
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

$section Model for the Derivative of Variance of Individual's Data with respect to Individual Parameter$$

$index SpkModel, doDataVariance_indPar( SPK_VA::valarray<double>& )$$
$index SpkModel, dataVariance_indPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the variance of data 
with respect to individual parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpkModel::doDataVariance_indPar( SPK_VA::valarray<double>& /ret/ ) const = 0/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpkModel::dataVariance_indPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_dataVariance//dataVariance()/$$ $rend
$cend
$xref/SpkModel_dataVariance_popPar//dataVariance_popPar()/$$ $rend 
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
Required for both population and individual analysis.
$pre

$$
$code SpkModel::doDataVariance_indPar()$$ calculates the derivative of the variance of a particular individual's
data with respect to the individual parameter,
for a currently selected individual (see $xref/SpkModel_selectIndividual//selectIndividual/$$) at
a currently set individual parameter (see $xref/SpkModel_setIndPar//setIndPar/$$) 
and population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doDataVariance_indPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataVariance_indPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataVariance_indPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code doDataVariance_indPar()$$ catches any exception thrown by the private interface,
$code dataVariance_indPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nYi * nYi * nIndPar%$$, where $math%nYi%$$ is the
number of measurements for a currently selected individual and $math%nIndPar%$$ is
the size of individual parameter vector.  Let $math%R%$$ denotes the function expressed by 
$code doDataVariance()$$. The resulting vector contains a sequence of partial derivatives
of $math%R(popPar, indPar)%$$ with respect to each element of $math%indPar%$$ (individual parameter vector):
$math%

  ret = { R(popPar, indPar)_indPar(1), R(popPar, indPar)_indPar(2), ... R(popPar, indPar)_indPar(nIndPar) }

%$$
where $math%indPar(k)%$$ denotes $math%k%$$-th element of $math%indPar%$$ vector.
$math%R(popPar, indPar)_indPar(k)%$$ holds the symmetric, positive definite property.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
bool SpkModel::dataVariance_indPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataVariance_indPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR, 
        "User-implemented doDataVariance_indPar() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataVariance_indPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR, 
        "User-implemented doDataVariance_indPar() threw an std::exception",
        __LINE__, 
        __FILE__
        );  
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR, 
        "User-implemented doDataVariance_indPar() threw an unknown exception",
        __LINE__, 
        __FILE__ 
      );
  }
}
/*************************************************************************
 *
 * Virtual function: dataVariance_popPar()
 * and corresponding public interface: dataVariance_popPar()
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_dataVariance_popPar$$
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

$section Model for the Derivative of Variance of Individual's Data with respect to Population Parameter$$

$index SpkModel, doDataVariance_popPar( SPK_VA::valarray<double>& )$$
$index SpkModel, dataVariance_popPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the variance of data 
with respect to population parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpkModel::doDataVariance_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpkModel::dataVariance_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_dataVariance//dataVariance()/$$ $rend
$cend
$xref/SpkModel_dataVariance_indPar//dataVariance_indPar()/$$ $rend 
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
Required for population analysis.
$pre

$$
$code SpkModel::doDataVariance_popPar()$$ calculates the derivative of the variance of a particular individual's
data with respect to the population parameter,
for a currently selected individual (see $xref/SpkModel_selectIndividual//selectIndividual/$$) at
a currently set individual parameter (see $xref/SpkModel_setIndPar//setIndPar/$$) 
and population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doDataVariance_popPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataVariance_popPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataVariance_popPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code doDataVariance_popPar()$$ catches any exception thrown by the private interface,
$code dataVariance_popPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_VARIANCE_POP_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nYi * nYi * nPopPar%$$, where $math%nYi%$$ is the
number of measurements for a currently selected individual and $math%nPopPar%$$ is
the size of population parameter vector.  Let $math%R%$$ denotes the function expressed by 
$code doDataVariance()$$. The resulting vector contains a sequence of partial derivatives
of $math%R(popPar, indPar)%$$ with respect to each element of $math%popPar%$$ (population parameter vector):
$math%

  ret = { R(popPar, indPar)_popPar(1), R(popPar, indPar)_popPar(2), ... R(popPar, indPar)_popPar(nPopPar) }

%$$
where $math%popPar(k)%$$ denotes $math%k%$$-th element of $math%popPar%$$ vector.
$math%R(popPar, indPar)_popPar(k)%$$ holds the symmetric, positive definite property.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
bool SpkModel::doDataVariance_popPar( valarray<double>& retVA ) const 
{
   throw SpkException(
       SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
       "doDataVariance_popPar() is not implemented",
       __LINE__, __FILE__
     );
}
bool SpkModel::dataVariance_popPar( valarray<double> & ret ) const 
{ 
  try{
      return doDataVariance_popPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_POP_ERR, 
        "User-implemented doDataVariance_popPar() threw an SpkException",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataVariance_popPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_POP_ERR, 
        "User-implemented doDataVariance_popPar() threw an std::exception",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_VARIANCE_POP_ERR, 
        "User-implemented doDataVariance_popPar() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: dataVarianceInv()
 * and corresponding public interface: dataVarianceInv()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_dataVarianceInv$$
$spell
	Model model
   SPK_VA
   Ny 
   th
   const
   Spk
   inv
   bool
   ind
   Ri
   valarray
$$

$section Model for the Inverse of Variance of Individual's Data$$

$index SpkModel, doDataVarianceInv( SPK_VA::valarray<double>& )$$
$index SpkModel, dataVarianceInv( SPK_VA::valarray<double>& )$$
$index User-provided Model, inverse of variance of data$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual void SpkModel::doDataVarianceInv( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: void SpkModel::dataVarianceInv( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_dataVarianceInv_indPar//dataVarianceInv_indPar()/$$ $rend
$cend
$xref/SpkModel_dataVarianceInv_popPar//dataVarianceInv_popPar()/$$ $rend 
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
Required for both population and individual analysis.  A default implementation
is provided.  The default implementation simply takes inverse of a matrix returned
by $xref/SpkModel_dataVariance//dataVariance()/$$.
$pre

$$
$code SpkModel::doDataVarianceInv()$$ calculates the inverse of the variance of a particular individual's
data for a currently selected individual (see $xref/SpkModel_selectIndividual//selectIndividual/$$) at
a currently set individual parameter (see $xref/SpkModel_setIndPar//setIndPar/$$) 
and population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doDataVarianceInv()$$
shall be placed in $italic ret$$ and the function returns nothing.
$pre

$$
$syntax/doDataVarianceInv()/$$ shall be only called via its corresponding
public interface, $syntax/dataVarianceInv()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code dataVarianceInv()$$ catches any exception thrown by the private interface,
$code doDataVarianceInv()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_VARIANCE_INV_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nYi * nYi%$$, where $math%nYi%$$ is the
number of currently selected individual's data.  The vector contains a column-ordered
matrix of dimensions, $math%nYi by nYi%$$, that is
symmetric and positive definite.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
void SpkModel::doDataVarianceInv( valarray<double> & ret ) const
{
  dataVariance(ret);
  _nY = static_cast<int>( sqrt( static_cast<double>( ret.size() ) ) );
  assert( ret.size() == _nY * _nY );
  ret = inverse(ret, _nY);
}
void SpkModel::dataVarianceInv( valarray<double>& ret ) const 
{ 
  try{
    doDataVarianceInv(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_ERR, 
        "User-implemented doDataVarianceInv() threw an SpkException",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataVarianceInv() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_ERR, 
        "User-implemented doDataVarianceInv() threw an std::exception",
        __LINE__, 
        __FILE__ 
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_ERR, 
        "User-implemented doDataVarianceInv() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: dataVarianceInv_indPar()
 * and corresponding public interface: dataVarianceInv_indPar()
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SPkModel_dataVarianceInv_indPar$$
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

$section Model for the Derivative of Inverse of Variance of Individual's Data with respect to Individual Parameter$$

$index SpkModel, doDataVarianceInv_indPar( SPK_VA::valarray<double>& )$$
$index SpkModel, dataVarianceInv_indPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the inverse of the variance of data with respect to individual parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpkModel::doDataVarianceInv_indPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpkModel::dataVarianceInv_indPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_dataVarianceInv//dataVarianceInv()/$$ $rend
$cend
$xref/SpkModel_dataVarianceInv_popPar//dataVarianceInv_popPar()/$$ $rend 
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
Required for both population and individual analysis.  A default implementation
is provided.
$pre

$$
$code SpkModel::doDataVarianceInv_indPar()$$ calculates the derivative of the inverse of the 
variance of a particular individual's
data with respect to the individual parameter,
for a currently selected individual (see $xref/SpkModel_selectIndividual//selectIndividual/$$) at
a currently set individual parameter (see $xref/SpkModel_setIndPar//setIndPar/$$) 
and population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doDataVarianceInv_indPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataVarianceInv_indPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataVarianceInv_indPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code doDataVarianceInv_indPar()$$ catches any exception thrown by the private interface,
$code dataVarianceInv_indPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nYi * nYi * nIndPar%$$, where $math%nYi%$$ is the
number of measurements for a currently selected individual and $math%nIndPar%$$ is
the size of individual parameter vector.  Let $math%RInv%$$ denotes the function expressed by 
$code doDataVarianceInv()$$. The resulting vector contains a sequence of partial derivatives
of $math%RInv(popPar, indPar)%$$ with respect to each element of $math%indPar%$$ (individual parameter vector):
$math%

  ret = { RInv(popPar, indPar)_indPar(1), RInv(popPar, indPar)_indPar(2), ... RInv(popPar, indPar)_indPar(nIndPar) }

%$$
where $math%indPar(k)%$$ denotes $math%k%$$-th element of $math%indPar%$$ vector.
$math%RInv(popPar, indPar)_indPar(k)%$$ holds the symmetric, positive definite property.


$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
bool SpkModel::doDataVarianceInv_indPar( valarray<double>& ret ) const
{
    valarray<double> RiInv;
    dataVarianceInv(RiInv);
    _nY = static_cast<int>( sqrt( static_cast<double>( RiInv.size() ) ) );
    assert( RiInv.size() == _nY * _nY );
    DoubleMatrix dmatRiInv( RiInv, _nY );

    valarray<double> Ri_b;
    dataVariance_indPar(Ri_b);
    DoubleMatrix dmatRi_b( Ri_b, _nIndPar );

    ret.resize( Ri_b.size() );

    ret = minusAkronBtimesC( dmatRiInv, dmatRiInv, dmatRi_b ).toValarray();
    return !allZero(ret);
}

bool SpkModel::dataVarianceInv_indPar( valarray<double> & ret ) const 
{ 
  try{
    return doDataVarianceInv_indPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_IND_ERR, 
        "User-implemented doDataVarianceInv_indPar() threw an SpkException",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataVarianceInv_indPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_IND_ERR, 
        "User-implemented doDataVarianceInv_indPar() threw an std::exception",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_IND_ERR, 
        "User-implemented doDataVarianceInv_indPar() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: dataVarianceInv_popPar()
 * and corresponding public interface: dataVarianceInv_popPar()
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_dataVarianceInv_popPar$$
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

$section Model for the Derivative of Inverse of Variance of Individual's Data with respect to Population Parameter$$

$index SpkModel, doDataVarianceInv_popPar( SPK_VA::valarray<double>& )$$
$index SpkModel, dataVarianceInv_popPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of the inverse of 
the variance of data with respect to population parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpkModel::doDataVarianceInv_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpkModel::dataVarianceInv_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_dataVarianceInv//dataVarianceInv()/$$ $rend
$cend
$xref/SpkModel_dataVarianceInv_indPar//dataVarianceInv_indPar()/$$ $rend 
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
Required for population analysis.  A default implementation
is provided.
$pre

$$
$code SpkModel::doDataVarianceInv_popPar()$$ calculates the derivative of the 
inverse of the variance of a particular individual's
data with respect to the population parameter,
for a currently selected individual (see $xref/SpkModel_selectIndividual//selectIndividual/$$) at
a currently set individual parameter (see $xref/SpkModel_setIndPar//setIndPar/$$) 
and population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doDataVarianceInv_popPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doDataVarianceInv_popPar()/$$ shall be only called via its corresponding
public interface, $syntax/dataVarianceInv_popPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code doDataVarianceInv_popPar()$$ catches any exception thrown by the private interface,
$code dataVarianceInv_popPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_VARIANCE_INV_POP_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nYi * nYi * nPopPar%$$, where $math%nYi%$$ is the
number of measurements for a currently selected individual and $math%nPopPar%$$ is
the size of population parameter vector.  Let $math%RInv%$$ denotes the function expressed by 
$code doDataVarianceInv()$$. The resulting vector contains a sequence of partial derivatives
of $math%RInv(popPar, indPar)%$$ with respect to each element of $math%popPar%$$ (population parameter vector):
$math%

  ret = { RInv(popPar, indPar)_popPar(1), RInv(popPar, indPar)_popPar(2), ... RInv(popPar, indPar)_popPar(nPopPar) }

%$$
where $math%popPar(k)%$$ denotes $math%k%$$-th element of $math%popPar%$$ vector.
$math%RInv(popPar, indPar)_popPar(k)%$$ holds the symmetric, positive definite property.


$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
bool SpkModel::doDataVarianceInv_popPar( valarray<double>& ret ) const
{
    valarray<double> RiInv;
    dataVarianceInv(RiInv);
    _nY = static_cast<int>( sqrt( static_cast<double>( RiInv.size() ) ) );
    assert( RiInv.size() == _nY * _nY );

    DoubleMatrix dmatRiInv( RiInv, _nY );

    valarray<double> Ri_a;
    dataVariance_popPar(Ri_a);
    DoubleMatrix dmatRi_a( Ri_a, _nPopPar );

    ret.resize( Ri_a.size() );

    ret = minusAkronBtimesC( dmatRiInv, dmatRiInv, dmatRi_a ).toValarray();
    return !allZero(ret);
}


bool SpkModel::dataVarianceInv_popPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataVarianceInv_popPar(ret);
  }

  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_POP_ERR, 
        "User-implemented doDataVarianceInv_popPar() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doDataVarianceInv_popPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_POP_ERR, 
        "User-implemented doDataVarianceInv_popPar() threw an std::exception",
        __LINE__, 
        __FILE__ 
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_POP_ERR, 
        "User-implemented doDataVarianceInv_popPar() threw an unknown exception",
        __LINE__, 
        __FILE__ 
      );
  }
}
/*************************************************************************
 *
 * Virtual function: doIndParVariance()
 * and corresponding public interface: indParVariance()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_indParVariance$$
$spell
	Model model
   SPK_VA
   Ny  
   th
   const
   Spk
   ind
   inv
   Covariance
   bool
   Ri
   valarray
$$

$section Model for the Variance of Individual Parameter$$

$index SpkModel, doIndParVariance( SPK_VA::valarray<double>& )$$
$index SpkModel, indParVariance( SPK_VA::valarray<double>& )$$
$index User-provided Model, variance of individual parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual void SpkModel::doIndParVariance( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: void SpkModel::indParVariance( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_indParVariance_popPar//indParVariance_popPar()/$$ $rend 
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
Required for population analysis.  Optional for individual analysis.
$pre

$$
$code SpkModel::doIndParVariance()$$ calculates the variance of
individual parameter at a currently set population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doIndParVariance()$$
shall be placed in $italic ret$$ and the function returns nothing.
$pre

$$
$syntax/doIndParVariance()/$$ shall be only called via its corresponding
public interface, $syntax/indParVariance()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code indParVariance()$$ catches any exception thrown by the private interface,
$code doIndParVariance()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_VARIANCE_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nIndPar * nIndPar%$$, where $math%nIndPar%$$ is the
number of individual parameter.  The vector contains a column-ordered
matrix of dimensions, $math%nIndPar by nIndPar%$$, that is
symmetric and positive definite.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/

void SpkModel::doIndParVariance( valarray<double>& retVA ) const
{
   throw SpkException(
       SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR,
       "doIndParVariance() is not implemented",
       __LINE__, __FILE__
     );
}


void SpkModel::indParVariance( valarray<double>& ret ) const 
{ 
  try{
    doIndParVariance(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_IND_VARIANCE_ERR, 
        "User-implemented doIndParVariance() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doIndParVariance() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_IND_VARIANCE_ERR, 
        "User-implemented doIndParVariance() threw an std::exception",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_IND_VARIANCE_ERR, 
        "User-implemented doIndParVariance() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}

/*************************************************************************
 *
 * Virtual function: doIndParVariance_popPar()
 * and corresponding public interface: indParVariance_popPar()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_indParVariance_popPar$$
$spell
	Model model
   SPK_VA
   Ny  
   th
   const
   Spk
   ind
   inv
   bool
   Ri
   valarray
$$

$section Model for the Derivative of Variance of Individual Parameter with respect to Population Parameter$$

$index SpkModel, doIndParVariance_popPar( SPK_VA::valarray<double>& )$$
$index SpkModel, indParVariance_popPar( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of variance of individual parameters
with respect to population parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpkModel::doIndParVariance_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpkModel::indParVariance_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_indParVariance//indParVariance()/$$ $rend 
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
Required for population analysis.  Optional for individual analysis.
$pre

$$
$code SpkModel::doIndParVariance_popPar()$$ calculates the derivative of the variance of 
individual parameter with respect to the population parameter at
a currently set population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doIndParVariance_popPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doIndParVariance_popPar()/$$ shall be only called via its corresponding
public interface, $syntax/indParVariance_popPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code doIndParVariance_popPar()$$ catches any exception thrown by the private interface,
$code indParVariance_popPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nIndPar * nIndPar * nPopPar%$$, where $math%nIndPar%$$ is
the size of individual parameter vector and $math%nPopPar%$$ is the size of
population parameter.  Let $math%D%$$ denotes the function expressed by 
$code doIndParVariance()$$. The resulting vector contains a sequence of partial derivatives
of $math%D(popPar)%$$ with respect to each element of $math%popPar%$$ (population parameter vector):
$math%

  ret = { D(popPar)_popPar(1), D(popPar)_popPar(2), ... D(popPar)_popPar(nPopPar) }

%$$
where $math%popPar(k)%$$ denotes $math%k%$$-th element of $math%popPar%$$ vector.
$math%D(popPar)_popPar(k)%$$ holds the symmetric, positive definite property.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
bool SpkModel::doIndParVariance_popPar( valarray<double>& retVA ) const
{
   throw SpkException(
       SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
       "doIndParVariance_popPar() is not implemented",
       __LINE__, __FILE__
     );
}


bool SpkModel::indParVariance_popPar( valarray<double>& ret ) const 
{ 
  try{
    return doIndParVariance_popPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_IND_VARIANCE_POP_ERR, 
        "User-implemented doIndParVariance_popPar() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doIndParVariance_popPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_IND_VARIANCE_POP_ERR, 
        "User-implemented doIndParVariance_popPar() threw an std::exception",
        __LINE__, 
        __FILE__ 
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_IND_VARIANCE_POP_ERR, 
        "User-implemented doIndParVariance_popPar() threw an unknown exception",
        __LINE__, 
        __FILE__ 
      );
  }
}
/*************************************************************************
 *
 * Virtual function: doIndParVarianceInv()
 * and corresponding public interface: indParVarianceInv()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_indParVarianceInv$$
$spell
	Model model
   SPK_VA
   Ny  
   th
   const
   Spk
   ind
   inv
   Dinv
   bool
   Ri
   valarray
$$

$section Model for the Inverse of Variance of Individual Parameter$$

$index SpkModel, doIndParVarianceInv( SPK_VA::valarray<double> & )$$
$index SpkModel, indParVarianceInv( SPK_VA::valarray<double> & )$$
$index User-provided Model, inverse of variance of individual parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual void SpkModel::doIndParVarianceInv( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: void SpkModel::indParVarianceInv( SPK_VA::valarray<double> & /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_indParVarianceInv_popPar//indParVarianceInv_popPar()/$$ $rend 
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
Required for the population analysis.  Optional for individual analysis.  
A default implementation is provided.
$pre

$$
$code SpkModel::doIndParVarianceInv()$$ calculates the inverse of the variance of
individual parameter at a currently set population parameter
(see $xref/SpkModel_setPopPar//setPopPar/$$).  
$pre

$$
The evaluation result of $code doIndParVarianceInv()$$
shall be placed in $italic ret$$ and the function returns nothing.
$pre

$$
$syntax/doIndParVarianceInv()/$$ shall be only called via its corresponding
public interface, $syntax/indParVarianceInv()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code indParVarianceInv()$$ catches any exception thrown by the private interface,
$code doIndParVarianceInv()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_VARIANCE_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nIndPar * nIndPar%$$, where $math%nIndPar%$$ is the
number of individual parameter.  The vector contains a column-ordered
matrix of dimensions, $math%nIndPar by nIndPar%$$, that is
symmetric and positive definite.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
void SpkModel::doIndParVarianceInv( valarray<double>& ret ) const
{
  indParVariance(ret);
  ret = inverse(ret, _nIndPar);
}

void SpkModel::indParVarianceInv( valarray<double>& ret ) const 
{ 
  try{
    doIndParVarianceInv(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_ERR, 
        "User-implemented doIndParVarianceInv() threw an SpkException",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doIndParVarianceInv() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_ERR, 
        "User-implemented doIndParVarianceInv() threw an std::exception",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_ERR, 
        "User-implemented doIndParVarianceInv() threw an unknown exception",
        __LINE__, 
        __FILE__
      );
  }
}
/*************************************************************************
 *
 * Virtual function: doIndParVarianceInv_popPar()
 * and corresponding public interface: indParVarianceInv_popPar()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_indParVarianceInv_popPar$$
$spell
	Model model
   SPK_VA
   Ny  
   th
   const
   Spk
   ind
   inv
   bool
   Ri
   valarray
$$

$section Model for the Derivative of Inverse of Variance of Individual Parameter with respect to Population Parameter$$

$index SpkModel, doIndParVarianceInv_popPar( SPK_VA::valarray<double>& )$$
$index SpkModel, indParVarianceInv_a( SPK_VA::valarray<double>& )$$
$index User-provided Model, derivative of inverse of variance of individual parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: virtual bool SpkModel::doIndParVarianceInv_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: bool SpkModel::indParVarianceInv_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
$tend

$table
$bold See also:$$ $cend
$xref/SpkModel_indParVarianceInv//indParVarianceInv()/$$ $rend 
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
Required for population analysis.
$pre

$$
The evaluation result of $code doIndParVariance_popPar()$$
shall be placed in $italic ret$$.  It returns $math%true%$$ if $italic ret$$ contains at least
one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$syntax/doIndParVarianceInv_popPar()/$$ shall be only called via its corresponding
public interface, $syntax/doIndParVarianceInv_popPar()/$$.  The public interface
shall be called only by the SPK library components unless when it is being unit-tested,
or SPK cannot guarantee any of the computational results.
$pre

$$
The public interface, $code doIndParVarianceInv_popPar()$$ catches any exception thrown by the private interface,
$code indParVarianceInv_popPar()$$, implemented by the user.
If the exception is an instance of $xref/SpkException//SpkException/$$, 
an additional $xref/SpkError//SpkError/$$ object will be appended to the list of errors
maintained within the object.  The new SpkError object will have error code set
to $code SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR$$.

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
$syntax/
ret
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nIndPar * nIndPar * nPopPar%$$, where $math%nIndPar%$$ is
the size of individual parameter vector and $math%nPopPar%$$ is the size of
population parameter.  Let $math%DInv%$$ denotes the function expressed by 
$code doIndParVarianceInv()$$. The resulting vector contains a sequence of partial derivatives
of $math%DInv(popPar)%$$ with respect to each element of $math%popPar%$$ (population parameter vector):
$math%

  ret = { DInv(popPar)_popPar(1), DInv(popPar)_popPar(2), ... DInv(popPar)_popPar(nPopPar) }

%$$
where $math%popPar(k)%$$ denotes $math%k%$$-th element of $math%popPar%$$ vector.
$math%DInv(popPar)_popPar(k)%$$ holds the symmetric, positive definite property.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
bool SpkModel::doIndParVarianceInv_popPar( valarray<double>& ret ) const
{
  valarray<double> Dinv;
  doIndParVarianceInv(Dinv);
  DoubleMatrix dmatDinv( Dinv, _nIndPar );

  valarray<double> D_a;
  doIndParVariance_popPar(D_a);
  DoubleMatrix dmatD_a( D_a, _nPopPar );

  ret.resize( D_a.size() );

  ret = minusAkronBtimesC( dmatDinv, dmatDinv, dmatD_a ).toValarray();
  return !allZero(ret);
}


bool SpkModel::indParVarianceInv_popPar( valarray<double>& ret ) const 
{ 
  try{
    return doIndParVarianceInv_popPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_POP_ERR, 
        "User-implemented doIndParVarianceInv_popPar() threw an SpkException",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "User-implemented doIndParVarianceInv_popPar() threw std::exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_POP_ERR, 
        "User-implemented doIndParVarianceInv_popPar() threw an std::exception",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_POP_ERR, 
        "User-implemented doIndParVarianceInv_popPar() threw an unknown exception",
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


/*************************************************************************
 *
 * Virtual function: const Covariance& getDataCovariance() const
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_dataCovariance$$
$spell
	Model model
  SPK_VA
  Covariance
  const
  cov
  Spk
  ind
  valarray
$$

$section Obtaining Reference to The Covariance of Measurement Data$$

$index SpkModel, getDataCovariance()$$
$index User-provided Model, data covariance$$

$table
$bold Public Interface:$$   $rend 
$syntax/public: const Covariance& SpkModel::getDataCovariance() const/$$  $rend
$tend

$table
$bold See also:$$ $cend
$xref/CovarianceClasses//Covariance classes/$$ $rend 
$tend

$fend 30$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Note$$
Deprecated!

$head Description$$
getDataCovariance returns a constant reference to an object representing the data covariance (ie. R() ).
$pre

$$
DataCovariance is a subclass of $xref/Covariance//Covariance/$$ class.
See $xref/CovarianceClasses//Covariance classes/$$ for more details.
$pre

$$

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
None.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
const Covariance& SpkModel::getDataCovariance()   const 
{ 
  return *pDataCovariance; 
}


/*************************************************************************
 *
 * Virtual function: const Covariance& getIndParCovariance() const
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_indParCovariance$$
$spell
	Model model
  SPK_VA
  Covariance
  ind
  const 
  cov
  Spk
  ind
  valarray
$$

$section Obtaining Reference to The Covariance of Individual Parameters$$

$index SpkModel, getIndParCovariance()$$
$index User-provided Model, individual parameter covariance$$

$table
$bold Public Interface:$$   $rend  
$syntax/public: const Covariance& SpkModel::getIndParCovariance() const/$$  $rend
$tend

$table
$bold See also:$$ $cend
$xref/CovarianceClasses//Covariance classes/$$ $rend 
$tend

$fend 30$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Note$$
Deprecated!

$head Description$$
getIndParCovariance() returns a constant reference to an object representing the individual parameters covariance (ie. doIndParVariance() ).
$pre

$$
IndParCovariance is a subclass of $xref/Covariance//Covariance/$$ class.
See $xref/CovarianceClasses//Covariance classes/$$ for more details.
$pre

$$

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Arguments$$
None.

$head Example$$
See $xref/SpkModel/Example/Example/$$

$end
*/
const Covariance& SpkModel::getIndParCovariance() const 
{ 
  return *pIndParCovariance; 
}

