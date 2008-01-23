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

// CppAD header files.
#include <CppAD/CppAD.h>

#include "SpkValarray.h"
#include "SpkModel.h"
#include "inverse.h"
#include "AkronBtimesC.h"
#include "mulByScalar.h"
#include "allZero.h"
#include "FullDataCovariance.h"
#include "FullIndParCovariance.h"
#include "doubleToScalarArray.h"
#include "scalarToDoubleArray.h"

using SPK_VA::valarray;

/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

static DoubleMatrix minusAkronBtimesC( 
  const DoubleMatrix &A, const DoubleMatrix &B, const DoubleMatrix &C );
static void minusAkronBtimesC( 
  const DoubleMatrix &A, const DoubleMatrix &B, const DoubleMatrix &C, DoubleMatrix& ret);

static int _nIndPar = -1;
static int _nPopPar = -1;
static int _nY      = -1;

} // [End: unnamed namespace]


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
    Model  model 
    Spk
$$

$section Default Constructor$$

$index User-provided Model, Default Constructor$$

$table
$bold Prototype:$$   $cend  
$syntax/protected: template<class Scalar> SpkModel<Scalar>::SpkModel()/$$
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

template<class Scalar>
SpkModel<Scalar>::SpkModel()
: pDataCovariance(NULL), pIndParCovariance(NULL)
{
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Backward Compatable SpkModel Code - Mitch]
  // This code is temporary and should be deleted once all of 
  // the old SpkModel remnants are gone.
  //
  pDataCovariance   = new FullDataCovariance;
  pIndParCovariance = new FullIndParCovariance;

  // This pointer to a double version of an SpkModel won't change the
  // type of the this pointer for this class if is intantianted with a
  // double Scalar and these covariance classes shouldn't be used when
  // Scalar is of type CppAD::AD<double>.
  SpkModel<double>* pDoubleSpkModel = dynamic_cast< SpkModel<double>* >( this );

  pDataCovariance  ->setModel( pDoubleSpkModel );
  pIndParCovariance->setModel( pDoubleSpkModel );
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
$syntax/protected: template<class Scalar> SpkModel<Scalar>::SpkModel( const SpkModel &/right/ )/$$
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
template<class Scalar>
SpkModel<Scalar>::SpkModel( const SpkModel &right )
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

  // This pointer to a double version of an SpkModel won't change the
  // type of the this pointer for this class if is intantianted with a
  // double Scalar and these covariance classes shouldn't be used when
  // Scalar is of type CppAD::AD<double>.
  SpkModel<double>* pDoubleSpkModel = dynamic_cast< SpkModel<double>* >( this );

  pDataCovariance  ->setModel( pDoubleSpkModel );
  pIndParCovariance->setModel( pDoubleSpkModel );
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
$syntax/protected: template<class Scalar> SpkModel<Scalar>::SpkModel& operator=( const SpkModel &/right/ )/$$
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
template<class Scalar>
SpkModel<Scalar>& SpkModel<Scalar>::operator=(const SpkModel &right)
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

  // This pointer to a double version of an SpkModel won't change the
  // type of the this pointer for this class if is intantianted with a
  // double Scalar and these covariance classes shouldn't be used when
  // Scalar is of type CppAD::AD<double>.
  SpkModel<double>* pDoubleSpkModel = dynamic_cast< SpkModel<double>* >( this );

  pDataCovariance  ->setModel( pDoubleSpkModel );
  pIndParCovariance->setModel( pDoubleSpkModel );
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
$syntax/public: template<class Scalar> virtual SpkModel<Scalar>::~SpkModel()/$$
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
template<class Scalar>
SpkModel<Scalar>::~SpkModel()
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
$syntax/private: template<class Scalar> virtual void SpkModel<Scalar>::doSelectIndividual( int /i/ )/$$ $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> void SpkModel<Scalar>::selectIndividual( int /i/ )/$$ $rend
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

template<class Scalar>
void SpkModel<Scalar>::doSelectIndividual(int base0)
{
 throw SpkException(
     SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR,
     "doSelectIndividual(int) is not implemented",
     __LINE__, __FILE__);
}

template<class Scalar>
void SpkModel<Scalar>::selectIndividual(int base0)
{
  try{
    doSelectIndividual(base0);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "The selection of the current individual failed.",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The selection of the current individual failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "The selection of the current individual failed because it threw a standard exception.",
        __LINE__, 
        __FILE__
        );
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "The selection of the current individual failed because it threw an unknown exception.",
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
        "The selection of the current individual for the individual data covariance failed.",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The selection of the current individual for the individual data covariance failed \nbecause it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "The selection of the current individual failed because it threw a standard exception.",
        __LINE__, 
        __FILE__ 
        );
  }  
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_SET_INDEX_ERR, 
        "The selection of the current individual for the individual data covariance failed because it threw an unknown exception.",
        __LINE__, 
        __FILE__
      );
  }
}


/*************************************************************************
 *
 * Virtual function: private: doSetPopPar( const SPK_VA::valarray<Scalar>& )
 * and corresponding public interface: setPopPar( const SPK_VA::valarray<Scalar>& )
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
$syntax/private: template<class Scalar> virtual void SpkModel<Scalar>::doSetPopPar( const SPK_VA::valarray<Scalar>& /popPar/ )/$$ $rend
$bold Public Interface:$$   $rend  
$syntax/public: template<class Scalar> void SpkModel<Scalar>::setPopPar( const SPK_VA::valarray<Scalar>& /popPar/ )/$$ $rend
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
$syntax/doSetPopPar( SPK_VA::valarray<Scalar> /popPar/ )/$$ sets $italic popPar$$ as the current
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
template<class Scalar>
void SpkModel<Scalar>::doSetPopPar( const valarray<Scalar>& popPar )
{
  throw SpkException(
     SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
     "doSetPopPar() is not implemented",
     __LINE__, __FILE__);
}

template<class Scalar>
void SpkModel<Scalar>::setPopPar( const valarray<Scalar> &popPar )
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
          "The setting of the current population parameter failed.",
          __LINE__, 
          __FILE__ 
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "The setting of the current population parameter failed because it threw a standard exception", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPK_MODEL_SET_POP_ERR, 
            "The setting of the current population parameter failed because it threw a standard exception.",
            __LINE__, 
            __FILE__ 
            );
    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "The setting of the current population parameter failed because it threw an unknown exception.",
          __LINE__, 
          __FILE__
        );
    }

    // Create a double version of the parameter.
    valarray<double> popParDouble( _nPopPar );
    scalarToDoubleArray( popPar, popParDouble );

    DoubleMatrix popParDM(popParDouble, 1);

    try{
      pDataCovariance  ->setCovPopPar(popParDouble);
    }
    catch( SpkException& e )
    {
      throw e.push(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "The setting of the current population parameter for the individual data covariance failed.",
          __LINE__, 
          __FILE__
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "The setting of the current population parameter for the individual data covariance failed \nbecause it threw a standard exception", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPK_MODEL_SET_POP_ERR, 
            "The setting of the current population parameter failed because it threw a standard exception.",
            __LINE__, 
            __FILE__ 
            );
    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "The setting of the current population parameter for the individual data covariance failed \nbecause it threw an unknown exception.",
          __LINE__, 
          __FILE__
        );
    }        
    try{
      pIndParCovariance->setCovPopPar(popParDouble);  // Give D(a) the value.
    }
    catch( SpkException& e )
    {
      throw e.push(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "The setting of the current population parameter for the individual parameter covariance parameter failed.",
          __LINE__, 
          __FILE__
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "The setting of the current population parameter for the individual parameter covariance parameter failed \nbecause it threw a standard exception.", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPK_MODEL_SET_POP_ERR, 
            "The setting of the current population parameter failed because it threw a standard exception.",
            __LINE__, 
            __FILE__ 
            );


    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPK_MODEL_SET_POP_ERR, 
          "The setting of the current population parameter for the individual parameter covariance parameter failed \nbecause it threw an unknown exception.",
          __LINE__, 
          __FILE__ 
        );
    }
}
/*************************************************************************
 *
 * Virtual function: private: doSetIndPar( const SPK_VA::valarray<Scalar>& )
 * and corresponding public interface: setIndPar( const SPK_VA::valarray<Scalar>& )
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
$syntax/private: template<class Scalar> virtual void SpkModel<Scalar>::doSetIndPar( const SPK_VA::valarray<Scalar>&  /indPar/ ) = 0/$$ $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> void SpkModel<Scalar>::setIndPar( const SPK_VA::valarray<Scalar>&  /indPar/ )/$$ $rend
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
$syntax/SpkModel<Scalar>::doSetIndPar( const SPK_VA::valarray<Scalar>&  /indPar/ )/$$ sets $italic indPar$$ as the current
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

template<class Scalar>
void SpkModel<Scalar>::setIndPar(const valarray<Scalar> &indPar)
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
          "The setting of the current individual parameter failed.",
          __LINE__, 
          __FILE__
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "The setting of the current individual parameter failed because it threw a standard exception.", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPK_MODEL_SET_IND_ERR, 
            "The setting of the current individual parameter failed because it threw a standard exception.",
            __LINE__, 
            __FILE__
            );
    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPK_MODEL_SET_IND_ERR, 
          "The setting of the current individual parameter failed because it threw an unknown exception.",
          __LINE__, 
          __FILE__ 
        );
    }

    // Create a double version of the parameter.
    valarray<double> indParDouble( _nIndPar );
    scalarToDoubleArray( indPar, indParDouble );

    DoubleMatrix indParDM(indParDouble, 1);
    try{
      pDataCovariance->setCovIndPar(indParDouble);    // Give doDataVariance(a, bi) the value.
    }
    catch( SpkException& e )
    {
      throw e.push(
          SpkError::SPK_MODEL_SET_IND_ERR, 
          "The setting of the current individual parameter for the individual data covariance failed.",
          __LINE__, 
          __FILE__
        );
    }
    catch(const std::exception& stde)
    {
      SpkException e(stde, "The setting of the current individual parameter for the individual data covariance failed \nbecause it threw a standard exception.", __LINE__, __FILE__);
      throw e.push(
            SpkError::SPK_MODEL_SET_IND_ERR, 
            "The setting of the current individual parameter failed because it threw a standard exception.",
            __LINE__, 
            __FILE__
            );
    }    
    catch(...)
    {
      throw SpkException(
          SpkError::SPK_MODEL_SET_IND_ERR, 
          "The setting of the current individual parameter for the individual data covariance failed \nbecause it threw an unknown exception.",
          __LINE__, 
          __FILE__ 
        );
    }        
}
/*************************************************************************
 *
 * Virtual function: doDataMean( SPK_VA::valarray<Scalar>& )
 * and corresponding public interface: dataMean( SPK_VA::valarray<Scalar>& )
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

$index SpkModel, doDataMean( SPK_VA::valarray<Scalar> & )$$
$index SpkModel, dataMean( SPK_VA::valarray<Scalar> & )$$
$index User-provided Model, mean of data$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: template<class Scalar> virtual void SpkModel<Scalar>::doDataMean( SPK_VA::valarray<Scalar>& /ret/ ) const = 0/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> void SpkModel<Scalar>::dataMean( SPK_VA::valarray<Scalar>& /ret/ ) const/$$  $rend
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
$code SpkModel<Scalar>::doDataMean()$$ calculates the mean of a particular individual's
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

template<class Scalar>
void SpkModel<Scalar>::dataMean( valarray<Scalar> & ret ) const 
{ 
  try{
    doDataMean(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_ERR, 
        "The evaluation of the mean of the individual's data failed.",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the mean of the individual's data failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_ERR, 
        "The evaluation of the mean of the individual's data failed because it threw a standard exception.",
        __LINE__, 
        __FILE__
        );
  }    
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR, 
        "The evaluation of the mean of the individual's data failed because it threw an unknown exception.",
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
$syntax/private: template<class Scalar> virtual bool SpkModel<Scalar>::doDataMean_indPar( SPK_VA::valarray<double>& /ret/ ) const = 0/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> bool SpkModel<Scalar>::dataMean_indPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
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
$code SpkModel<Scalar>::doDataMean_indPar()$$ calculates the derivative of the mean of a particular individual's
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

template<class Scalar>
bool SpkModel<Scalar>::dataMean_indPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataMean_indPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_IND_ERR, 
        "The evaluation of the derivative of the mean of the individual's data with respect to \nthe individual parameter failed.",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the derivative of the mean of the individual's data with respect to \nthe individual parameter failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_IND_ERR, 
        "The evaluation of the derivative of the mean of the individual's data with respect to \nthe individual parameter failed because it threw a standard exception.",
        __LINE__, 
        __FILE__
        );
  } 
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_IND_ERR, 
        "The evaluation of the derivative of the mean of the individual's data with respect to \nthe individual parameter failed because it threw an unknown exception.",
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
$syntax/private: template<class Scalar> virtual bool SpkModel<Scalar>::doDataMean_popPar( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> bool SpkModel<Scalar>::dataMean_popPar( SPK_VA::valarray<double> & /ret/ ) const/$$  $rend
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
$code SpkModel<Scalar>::doDataMean_popPar()$$ calculates the derivative of the mean of a particular individual's
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
template<class Scalar>
bool SpkModel<Scalar>::doDataMean_popPar( valarray<double>& retVA ) const
{
   throw SpkException(
       SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
       "doDataMean_popPar() is not implemented",
       __LINE__, 
       __FILE__
     );
}
template<class Scalar>
bool SpkModel<Scalar>::dataMean_popPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataMean_popPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_POP_ERR, 
        "The evaluation of the derivative of the mean of the individual's data with respect to \nthe population parameter failed.",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the derivative of the mean of the individual's data with respect to \nthe population parameter failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_POP_ERR, 
        "The evaluation of the derivative of the mean of the individual's data with respect to \nthe population parameter failed because it threw a standard exception.",
        __LINE__, 
        __FILE__ 
        );  
  }   
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_POP_ERR, 
        "The evaluation of the derivative of the mean of the individual's data with respect to \nthe population parameter failed because it threw an unknown exception.",
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

$index SpkModel, doDataVariance( SPK_VA::valarrayScalar& )$$
$index SpkModel, dataVariance( SPK_VA::valarrayScalar& )$$
$index User-provided Model, variance of data$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: template<class Scalar> virtual void SpkModel<Scalar>::doDataVariance( SPK_VA::valarrayScalar& /ret/ ) const = 0/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> void SpkModel<Scalar>::dataVariance( SPK_VA::valarrayScalar& /ret/ ) const/$$       $rend
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
$code SpkModel<Scalar>::doDataVariance()$$ calculates the variance of a particular individual's
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

template<class Scalar>
void SpkModel<Scalar>::dataVariance( valarray<Scalar> & ret ) const 
{ 
  try{
    doDataVariance(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_ERR, 
        "The evaluation of the individual's data covariance failed.",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the individual's data covariance failed because it threw a standard exception", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_ERR, 
        "The evaluation of the individual's data covariance failed because it threw a standard exception.",
        __LINE__, 
        __FILE__
        );  
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_VARIANCE_ERR, 
        "The evaluation of the individual's data covariance failed because it threw an unknown exception.",
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
$syntax/private: template<class Scalar> virtual bool SpkModel<Scalar>::doDataVariance_indPar( SPK_VA::valarray<double>& /ret/ ) const = 0/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> bool SpkModel<Scalar>::dataVariance_indPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
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
$code SpkModel<Scalar>::doDataVariance_indPar()$$ calculates the derivative of the variance of a particular individual's
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
template<class Scalar>
bool SpkModel<Scalar>::dataVariance_indPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataVariance_indPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR, 
        "The evaluation of the derivative of the individual's data covariance with respect to \nthe individual parameter failed.",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the derivative of the individual's data covariance with respect to \nthe individual parameter failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR, 
        "The evaluation of the derivative of the individual's data covariance with respect to \nthe individual parameter failed because it threw a standard exception.",
        __LINE__, 
        __FILE__
        );  
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR, 
        "The evaluation of the derivative of the individual's data covariance with respect to \nthe individual parameter failed because it threw an unknown exception.",
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
$syntax/private: template<class Scalar> virtual bool SpkModel<Scalar>::doDataVariance_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> bool SpkModel<Scalar>::dataVariance_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
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
$code SpkModel<Scalar>::doDataVariance_popPar()$$ calculates the derivative of the variance of a particular individual's
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
template<class Scalar>
bool SpkModel<Scalar>::doDataVariance_popPar( valarray<double>& retVA ) const 
{
   throw SpkException(
       SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
       "doDataVariance_popPar() is not implemented",
       __LINE__, __FILE__
     );
}
template<class Scalar>
bool SpkModel<Scalar>::dataVariance_popPar( valarray<double> & ret ) const 
{ 
  try{
      return doDataVariance_popPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the individual's data covariance with respect to \nthe population parameter failed.",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the derivative of the individual's data covariance with respect to \nthe population parameter failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_DATA_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the individual's data covariance with respect to \nthe population parameter failed because it threw a standard exception.",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_DATA_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the individual's data covariance with respect to \nthe population parameter failed because it threw an unknown exception.",
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

$index SpkModel, doDataVarianceInv( SPK_VA::valarray<Scalar>& )$$
$index SpkModel, dataVarianceInv( SPK_VA::valarray<Scalar>& )$$
$index User-provided Model, inverse of variance of data$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: template<class Scalar> virtual void SpkModel<Scalar>::doDataVarianceInv( SPK_VA::valarray<Scalar>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> void SpkModel<Scalar>::dataVarianceInv( SPK_VA::valarray<Scalar>& /ret/ ) const/$$       $rend
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
$code SpkModel<Scalar>::doDataVarianceInv()$$ calculates the inverse of the variance of a particular individual's
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
template<class Scalar>
void SpkModel<Scalar>::doDataVarianceInv( valarray<Scalar> & ret ) const
{
  valarray<Scalar> Ri;
  dataVariance( Ri );

  _nY = static_cast<int>( sqrt( static_cast<double>( Ri.size() ) ) );
  assert( Ri.size() == _nY * _nY );

  int j;

  // Create an identity matrix.
  valarray<Scalar> identity( _nY * _nY );
  identity = Scalar( 0 );
  for ( j = 0; j < _nY; j++ )
  {
    identity[j + j * _nY] = 1;
  }

  ret.resize( _nY * _nY );

  // Calculate the inverse by solving the equation
  //
  //     R  *  X  =  I  ,
  //
  // where I is the identity matrix and
  //
  //            -1
  //     X  =  R    .
  //
  int signdet;
  Scalar logdet;
  signdet = CppAD::LuSolve( _nY, _nY, Ri, identity, ret, logdet );
}
template<class Scalar>
void SpkModel<Scalar>::dataVarianceInv( valarray<Scalar>& ret ) const 
{ 
  try{
    doDataVarianceInv(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_ERR, 
        "The evaluation of the inverse of the individual's data covariance failed.",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the inverse of the individual's data covariance failed \nbecause it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_ERR, 
        "The evaluation of the inverse of the individual's data covariance failed \nbecause it threw a standard exception.",
        __LINE__, 
        __FILE__ 
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_ERR, 
        "The evaluation of the inverse of the individual's data covariance failed \nbecause it threw an unknown exception.",
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
$syntax/private: template<class Scalar> virtual bool SpkModel<Scalar>::doDataVarianceInv_indPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> bool SpkModel<Scalar>::dataVarianceInv_indPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
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
$code SpkModel<Scalar>::doDataVarianceInv_indPar()$$ calculates the derivative of the inverse of the 
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
template<class Scalar>
bool SpkModel<Scalar>::doDataVarianceInv_indPar( valarray<double>& ret ) const
{
    valarray<Scalar> RiScalar;
    dataVariance(RiScalar);
    _nY = static_cast<int>( sqrt( static_cast<double>( RiScalar.size() ) ) );
    assert( RiScalar.size() == _nY * _nY );
    
    valarray<double> RiInv( RiScalar.size() );
    scalarToDoubleArray( RiScalar, RiInv );
    RiInv = inverse(RiInv, _nY);
    assert( RiInv.size() == _nY * _nY );

    DoubleMatrix dmatRiInv( RiInv, _nY );

    valarray<double> Ri_b;
    dataVariance_indPar(Ri_b);
    DoubleMatrix dmatRi_b( Ri_b, _nIndPar );

    ret = minusAkronBtimesC( dmatRiInv, dmatRiInv, dmatRi_b ).toValarray();
    return !allZero(ret);
}

template<class Scalar>
bool SpkModel<Scalar>::dataVarianceInv_indPar( valarray<double> & ret ) const 
{ 
  try{
    return doDataVarianceInv_indPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_IND_ERR, 
        "The evaluation of the derivative of the inverse of the individual's data covariance with respect to \nthe individual parameter failed.",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the derivative of the inverse of the individual's data covariance with respect to \nthe individual parameter failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_IND_ERR, 
        "The evaluation of the derivative of the inverse of the individual's data covariance with respect to \nthe individual parameter failed because it threw a standard exception.",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_IND_ERR, 
        "The evaluation of the derivative of the inverse of the individual's data covariance with respect to \nthe individual parameter failed because it threw an unknown exception.",
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
$syntax/private: template<class Scalar> virtual bool SpkModel<Scalar>::doDataVarianceInv_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> bool SpkModel<Scalar>::dataVarianceInv_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
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
$code SpkModel<Scalar>::doDataVarianceInv_popPar()$$ calculates the derivative of the 
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
template<class Scalar>
bool SpkModel<Scalar>::doDataVarianceInv_popPar( valarray<double>& ret ) const
{
    valarray<Scalar> RiScalar;
    dataVariance(RiScalar);
    _nY = static_cast<int>( sqrt( static_cast<double>( RiScalar.size() ) ) );
    assert( RiScalar.size() == _nY * _nY );
    
    valarray<double> RiInv( RiScalar.size() );
    scalarToDoubleArray( RiScalar, RiInv );
    RiInv = inverse(RiInv, _nY);
    assert( RiInv.size() == _nY * _nY );

    DoubleMatrix dmatRiInv( RiInv, _nY );

    valarray<double> Ri_a;
    dataVariance_popPar(Ri_a);
    DoubleMatrix dmatRi_a( Ri_a, _nPopPar );

    ret = minusAkronBtimesC( dmatRiInv, dmatRiInv, dmatRi_a ).toValarray();
    return !allZero(ret);
}


template<class Scalar>
bool SpkModel<Scalar>::dataVarianceInv_popPar( valarray<double>& ret ) const 
{ 
  try{
    return doDataVarianceInv_popPar(ret);
  }

  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the inverse of the individual's data covariance with respect to \nthe population parameter failed.",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the derivative of the inverse of the individual's data covariance with respect to \nthe population parameter failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the inverse of the individual's data covariance with respect to \nthe population parameter failed because it threw a standard exception.",
        __LINE__, 
        __FILE__ 
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_INV_DATA_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the inverse of the individual's data covariance with respect to \nthe population parameter failed because it threw an unknown exception.",
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

$index SpkModel, doIndParVariance( SPK_VA::valarray<Scalar>& )$$
$index SpkModel, indParVariance( SPK_VA::valarray<Scalar>& )$$
$index User-provided Model, variance of individual parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: template<class Scalar> virtual void SpkModel<Scalar>::doIndParVariance( SPK_VA::valarray<Scalar>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> void SpkModel<Scalar>::indParVariance( SPK_VA::valarray<Scalar>& /ret/ ) const/$$       $rend
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
$code SpkModel<Scalar>::doIndParVariance()$$ calculates the variance of
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

template<class Scalar>
void SpkModel<Scalar>::doIndParVariance( valarray<Scalar>& retVA ) const
{
   throw SpkException(
       SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR,
       "doIndParVariance() is not implemented",
       __LINE__, __FILE__
     );
}


template<class Scalar>
void SpkModel<Scalar>::indParVariance( valarray<Scalar>& ret ) const 
{ 
  try{
    doIndParVariance(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_IND_VARIANCE_ERR, 
        "The evaluation of the individual parameter covariance failed.",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the individual parameter covariance failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_IND_VARIANCE_ERR, 
        "The evaluation of the individual parameter covariance failed because it threw a standard exception.",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_IND_VARIANCE_ERR, 
        "The evaluation of the individual parameter covariance failed because it threw an unknown exception.",
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
$syntax/private: template<class Scalar> virtual bool SpkModel<Scalar>::doIndParVariance_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> bool SpkModel<Scalar>::indParVariance_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
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
$code SpkModel<Scalar>::doIndParVariance_popPar()$$ calculates the derivative of the variance of 
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
template<class Scalar>
bool SpkModel<Scalar>::doIndParVariance_popPar( valarray<double>& retVA ) const
{
   throw SpkException(
       SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
       "doIndParVariance_popPar() is not implemented",
       __LINE__, __FILE__
     );
}


template<class Scalar>
bool SpkModel<Scalar>::indParVariance_popPar( valarray<double>& ret ) const 
{ 
  try{
    return doIndParVariance_popPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_IND_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the individual parameter covariance with respect to \nthe population parameter failed.",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the derivative of the individual parameter covariance with respect to \nthe population parameter failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_IND_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the individual parameter covariance with respect to \nthe population parameter failed because it threw a standard exception.",
        __LINE__, 
        __FILE__ 
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_IND_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the individual parameter covariance with respect to \nthe population parameter failed because it threw an unknown exception.",
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

$index SpkModel, doIndParVarianceInv( SPK_VA::valarray<Scalar> & )$$
$index SpkModel, indParVarianceInv( SPK_VA::valarray<Scalar> & )$$
$index User-provided Model, inverse of variance of individual parameter$$

$table
$bold Virtual Private Interface:$$   $rend  
$syntax/private: template<class Scalar> virtual void SpkModel<Scalar>::doIndParVarianceInv( SPK_VA::valarray<Scalar> & /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> void SpkModel<Scalar>::indParVarianceInv( SPK_VA::valarray<Scalar> & /ret/ ) const/$$       $rend
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
$code SpkModel<Scalar>::doIndParVarianceInv()$$ calculates the inverse of the variance of
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
template<class Scalar>
void SpkModel<Scalar>::doIndParVarianceInv( valarray<Scalar>& ret ) const
{
  valarray<Scalar> D;
  indParVariance( D );

  _nIndPar = static_cast<int>( sqrt( static_cast<double>( D.size() ) ) );
  assert( D.size() == _nIndPar * _nIndPar );

  int j;

  // Create an identity matrix.
  valarray<Scalar> identity( _nIndPar * _nIndPar );
  identity = Scalar( 0 );
  for ( j = 0; j < _nIndPar; j++ )
  {
    identity[j + j * _nIndPar] = 1;
  }

  ret.resize( _nIndPar * _nIndPar );

  // Calculate the inverse by solving the equation
  //
  //     D  *  X  =  I  ,
  //
  // where I is the identity matrix and
  //
  //            -1
  //     X  =  D    .
  //
  int signdet;
  Scalar logdet;
  signdet = CppAD::LuSolve( _nIndPar, _nIndPar, D, identity, ret, logdet );
}

template<class Scalar>
void SpkModel<Scalar>::indParVarianceInv( valarray<Scalar>& ret ) const 
{ 
  try{
    doIndParVarianceInv(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_ERR, 
        "The evaluation of the inverse of the individual parameter covariance failed.",
        __LINE__, 
        __FILE__
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the inverse of the individual parameter covariance failed \nbecause it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_ERR, 
        "The evaluation of the inverse of the individual parameter covariance failed \nbecause it threw a standard exception.",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_ERR, 
        "The evaluation of the inverse of the individual parameter covariance failed \nbecause it threw an unknown exception.",
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
$syntax/private: template<class Scalar> virtual bool SpkModel<Scalar>::doIndParVarianceInv_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$  $rend
$bold Public Interface:$$ $rend
$syntax/public: template<class Scalar> bool SpkModel<Scalar>::indParVarianceInv_popPar( SPK_VA::valarray<double>& /ret/ ) const/$$       $rend
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
template<class Scalar>
bool SpkModel<Scalar>::doIndParVarianceInv_popPar( valarray<double>& ret ) const
{
  valarray<Scalar> DScalar;
  indParVariance(DScalar);
  assert( DScalar.size() == _nIndPar * _nIndPar );
  
  valarray<double> DInv( DScalar.size() );
  scalarToDoubleArray( DScalar, DInv );
  DInv = inverse(DInv, _nIndPar);
  assert( DInv.size() == _nIndPar * _nIndPar );

  DoubleMatrix dmatDInv( DInv, _nIndPar );

  valarray<double> D_a;
  doIndParVariance_popPar(D_a);
  DoubleMatrix dmatD_a( D_a, _nPopPar );

  ret = minusAkronBtimesC( dmatDInv, dmatDInv, dmatD_a ).toValarray();
  return !allZero(ret);
}


template<class Scalar>
bool SpkModel<Scalar>::indParVarianceInv_popPar( valarray<double>& ret ) const 
{ 
  try{
    return doIndParVarianceInv_popPar(ret);
  }
  catch( SpkException& e )
  {
    throw e.push(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the inverse of the individual parameter covariance with respect to \nthe population parameter failed.",
        __LINE__, 
        __FILE__ 
      );
  }
  catch(const std::exception& stde)
  {
    SpkException e(stde, "The evaluation of the derivative of the inverse of the individual parameter covariance with respect to \nthe population parameter failed because it threw a standard exception.", __LINE__, __FILE__);
    throw e.push(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the inverse of the individual parameter covariance with respect to \nthe population parameter failed because it threw a standard exception.",
        __LINE__, 
        __FILE__
        );   
  }
  catch(...)
  {
    throw SpkException(
        SpkError::SPK_MODEL_INV_IND_VARIANCE_POP_ERR, 
        "The evaluation of the derivative of the inverse of the individual parameter covariance with respect to \nthe population parameter failed because it threw an unknown exception.",
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

namespace // [Begin: unnamed namespace]
{

static DoubleMatrix minusAkronBtimesC( const DoubleMatrix &A, const DoubleMatrix &B, const DoubleMatrix &C )
{
    return mulByScalar(AkronBtimesC( A, B, C ), -1.);
}

static void minusAkronBtimesC( const DoubleMatrix &A, const DoubleMatrix &B, const DoubleMatrix &C, DoubleMatrix & ret)
{
    ret = mulByScalar(AkronBtimesC( A, B, C ), -1.);
}

} // [End: unnamed namespace]


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
$syntax/public: template<class Scalar> const Covariance& SpkModel<Scalar>::getDataCovariance() const/$$  $rend
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

template<class Scalar>
const Covariance& SpkModel<Scalar>::getDataCovariance()   const 
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
$syntax/public: template<class Scalar> const Covariance& SpkModel<Scalar>::getIndParCovariance() const/$$  $rend
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

template<class Scalar>
const Covariance& SpkModel<Scalar>::getIndParCovariance() const 
{ 
  return *pIndParCovariance; 
}


/*************************************************************************
 *
 * Function: void SpkModel<Scalar>::invalidateIndParCovarianceCache()
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin SpkModel_invalidateIndParCovarianceCache$$
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

$section Invalidates the Cached Values for the Covariance of the Individual Parameters$$

$index SpkModel, invalidateIndParCovarianceCache()$$
$index User-provided Model, individual parameter covariance cache$$

$table
$bold Public Interface:$$   $rend  
$syntax/public: template<class Scalar> void SpkModel<Scalar>::invalidateIndParCovarianceCache()/$$  $rend
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
$head Description$$
Invalidates the cached values for the covariance of the individual parameters.

$end
*/

template<class Scalar>
void SpkModel<Scalar>::invalidateIndParCovarianceCache()
{ 
  pIndParCovariance->setCacheStatusInvalid(); 
}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

// Declare double versions of these functions.
template void SpkModel<double>::selectIndividual(int base0);
template void SpkModel<double>::setPopPar(const valarray<double> &popPar);
template void SpkModel<double>::setIndPar(const valarray<double> &indPar);

template void SpkModel<double>::dataMean( valarray<double>& ret ) const;
template bool SpkModel<double>::dataMean_indPar( valarray<double>& ret ) const;
template bool SpkModel<double>::dataMean_popPar( valarray<double>& ret ) const;

template void SpkModel<double>::dataVariance( valarray<double>& ret ) const;
template bool SpkModel<double>::dataVariance_indPar( valarray<double>& ret ) const;
template bool SpkModel<double>::dataVariance_popPar( valarray<double>& ret ) const;

template void SpkModel<double>::dataVarianceInv( valarray<double>& ret ) const;
template bool SpkModel<double>::dataVarianceInv_indPar( valarray<double>& ret ) const;
template bool SpkModel<double>::dataVarianceInv_popPar( valarray<double>& ret ) const;

template void SpkModel<double>::indParVariance( valarray<double>& ret ) const;
template bool SpkModel<double>::indParVariance_popPar( valarray<double>& ret ) const;

template void SpkModel<double>::indParVarianceInv( valarray<double>& ret ) const;
template bool SpkModel<double>::indParVarianceInv_popPar( valarray<double>& ret ) const;

template void SpkModel<double>::doSelectIndividual ( int base0 );
template void SpkModel<double>::doSetPopPar( const valarray<double>& inVA );

template bool SpkModel<double>::doDataMean_popPar( valarray<double>& ret ) const;

template bool SpkModel<double>::doDataVariance_popPar( valarray<double>& ret ) const;

template void SpkModel<double>::doDataVarianceInv( valarray<double>& ret ) const;
template bool SpkModel<double>::doDataVarianceInv_indPar( valarray<double>& ret ) const;
template bool SpkModel<double>::doDataVarianceInv_popPar( valarray<double>& ret ) const;

template void SpkModel<double>::doIndParVariance( valarray<double>& ret ) const;
template bool SpkModel<double>::doIndParVariance_popPar( valarray<double>& ret ) const;
template void SpkModel<double>::doIndParVarianceInv( valarray<double>& ret ) const;
template bool SpkModel<double>::doIndParVarianceInv_popPar( valarray<double>& ret ) const;

template SpkModel<double>::SpkModel();
template SpkModel<double>::SpkModel( const SpkModel &right );
template SpkModel<double>& SpkModel<double>::operator=(const SpkModel &right);

template SpkModel<double>::~SpkModel();

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// [Revisit - Backward Compatable SpkModel Code - Mitch]
// This code is temporary and should be deleted once all of 
// the old SpkModel remnants are gone.
//
template const Covariance& SpkModel<double>::getDataCovariance()   const;
template const Covariance& SpkModel<double>::getIndParCovariance() const;

template void SpkModel<double>::invalidateIndParCovarianceCache();
//
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


// Declare  CppAD::AD<double>  versions of these functions.
template void SpkModel< CppAD::AD<double> >::selectIndividual(int base0);
template void SpkModel< CppAD::AD<double> >::setPopPar(const valarray< CppAD::AD<double> > &popPar);
template void SpkModel< CppAD::AD<double> >::setIndPar(const valarray< CppAD::AD<double> > &indPar);

template void SpkModel< CppAD::AD<double> >::dataMean( valarray< CppAD::AD<double> >& ret ) const;
template bool SpkModel< CppAD::AD<double> >::dataMean_indPar( valarray<double>& ret ) const;
template bool SpkModel< CppAD::AD<double> >::dataMean_popPar( valarray<double>& ret ) const;

template void SpkModel< CppAD::AD<double> >::dataVariance( valarray< CppAD::AD<double> >& ret ) const;
template bool SpkModel< CppAD::AD<double> >::dataVariance_indPar( valarray<double>& ret ) const;
template bool SpkModel< CppAD::AD<double> >::dataVariance_popPar( valarray<double>& ret ) const;

template void SpkModel< CppAD::AD<double> >::dataVarianceInv( valarray< CppAD::AD<double> >& ret ) const;
template bool SpkModel< CppAD::AD<double> >::dataVarianceInv_indPar( valarray<double>& ret ) const;
template bool SpkModel< CppAD::AD<double> >::dataVarianceInv_popPar( valarray<double>& ret ) const;

template void SpkModel< CppAD::AD<double> >::indParVariance( valarray< CppAD::AD<double> >& ret ) const;
template bool SpkModel< CppAD::AD<double> >::indParVariance_popPar( valarray<double>& ret ) const;

template void SpkModel< CppAD::AD<double> >::indParVarianceInv( valarray< CppAD::AD<double> >& ret ) const;
template bool SpkModel< CppAD::AD<double> >::indParVarianceInv_popPar( valarray<double>& ret ) const;

template void SpkModel< CppAD::AD<double> >::doSelectIndividual ( int base0 );
template void SpkModel< CppAD::AD<double> >::doSetPopPar( const valarray< CppAD::AD<double> >& inVA );

template bool SpkModel< CppAD::AD<double> >::doDataMean_popPar( valarray<double>& ret ) const;

template bool SpkModel< CppAD::AD<double> >::doDataVariance_popPar( valarray<double>& ret ) const;

template void SpkModel< CppAD::AD<double> >::doDataVarianceInv( valarray< CppAD::AD<double> >& ret ) const;
template bool SpkModel< CppAD::AD<double> >::doDataVarianceInv_indPar( valarray<double>& ret ) const;
template bool SpkModel< CppAD::AD<double> >::doDataVarianceInv_popPar( valarray<double>& ret ) const;

template void SpkModel< CppAD::AD<double> >::doIndParVariance( valarray< CppAD::AD<double> >& ret ) const;
template bool SpkModel< CppAD::AD<double> >::doIndParVariance_popPar( valarray<double>& ret ) const;
template void SpkModel< CppAD::AD<double> >::doIndParVarianceInv( valarray< CppAD::AD<double> >& ret ) const;
template bool SpkModel< CppAD::AD<double> >::doIndParVarianceInv_popPar( valarray<double>& ret ) const;

template SpkModel< CppAD::AD<double> >::SpkModel();
template SpkModel< CppAD::AD<double> >::SpkModel( const SpkModel &right );
template SpkModel< CppAD::AD<double> >& SpkModel< CppAD::AD<double> >::operator=(const SpkModel &right);

template SpkModel< CppAD::AD<double> >::~SpkModel();

