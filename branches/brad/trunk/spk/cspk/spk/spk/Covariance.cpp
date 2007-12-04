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
 * File: Covariance.cpp
 *
 *
 * This is the abstract base class for all covariance matrix classes.
 * Objects of this class are used to calculate and cache various 
 * mathematical quantities involving covariance matrices.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
// Standard include files.
#include <cmath>
#include <cassert>

// SPK include files.
#include "Covariance.h"
#include "SpkValarray.h"

using SPK_VA::valarray;

/*------------------------------------------------------------------------
 * Local Declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  /*
  bool isSubsetDifferent( 
    const DoubleMatrix& dvecX, 
    const DoubleMatrix& dvecY, 
    const vector<int>& indexInY );

  bool isSubsetDifferent( 
    const valarray<double>& x, 
    const valarray<double>& y, 
    const vector<int>& indexInY );

  void copySubset( 
    DoubleMatrix& dvecX, 
    const DoubleMatrix& dvecY, 
    const vector<int>& indexInY );

  vector<int> seqIntVector( int nElem );

  bool isOkIndicesVector(  const vector<int>& indexInY, int nX, int nY );
  */

  bool isDifferent( const valarray<double> & x, const valarray<double>& Y );

} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: Covariance (Default Constructor)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_default_constructor$$
$spell 
  Covariance
$$

$section Default Constructor$$

$index Covariance, Default Constructor$$

$table
$bold Prototype:$$   $cend  
$syntax/protected Covariance::Covariance()/$$
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
Since $code Covariance$$ is an abstract class, the direct use of this constructor
is only allowed for its subclasses.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

Covariance::Covariance()
  : 
  timesCacheUpdated ( 0 ),
  index             ( -1 ),
  covPopPar         ( 0.0, 0 ),
  covIndPar         ( 0.0, 0 ),
  isCacheValid      ( false )
{
}


/*************************************************************************
 *
 * Function: Covariance (Copy Constructor)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_copy_constructor$$
$spell 
  const
  copyable
  Covariance
$$

$section Copy Constructor$$

$index Covariance, Copy Constructor$$

$table
$bold Prototype:$$   $cend  
$syntax/protected Covariance::Covariance( const Covariance &/right/ )/$$
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
Since $code Covariance$$ is an abstract class, the direct use of this constructor
is only allowed for its subclasses.
$pre

$$
The copy constructor is provided to make the $code Covariance$$ class
more standard even though SPK itself does not currently require
$code Covariance$$ objects to be copyable.

$head Arguments$$
$syntax/
right
/$$
The $code Covariance$$ to be copied.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

Covariance::Covariance( const Covariance &right )
  : 
  timesCacheUpdated ( right.timesCacheUpdated ),
  index             ( right.index ),
  covPopPar         ( right.covPopPar ),
  covIndPar         ( right.covIndPar ),
  isCacheValid      ( right.isCacheValid )
{
}


/*************************************************************************
 *
 * Function: Covariance (Assignment Operator)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_assignment_operator$$
$spell 
  assignable
  const
  Covariance
$$

$section Assignment Operator$$

$index Covariance, Assignment Operator$$

$table
$bold Prototype:$$   $cend  
$syntax/protected Covariance::Covariance& operator=( const Covariance &/right/ )/$$
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
Since $code Covariance$$ is an abstract class, the direct use of this operator
is only allowed for its subclasses.
$pre

$$
The assignment operator is provide to make the $code Covariance$$ class
more standard even though SPK itself does not currently require
$code Covariance$$ objects to be assignable.

$head Arguments$$
$syntax/
right
/$$
The $code Covariance$$ to be copied.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

Covariance& Covariance::operator=(const Covariance &right)
{
  timesCacheUpdated = right.timesCacheUpdated;
  index             = right.index;
  covPopPar         = right.covPopPar;
  covIndPar         = right.covIndPar;
  isCacheValid      = right.isCacheValid;

  return *this;
}


/*************************************************************************
 *
 * Function: destructor
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_default_destructor$$
$spell 
  Covariance
$$

$section Default Destructor$$

$index Covariance, Default Destructor$$

$table
$bold Prototype:$$   $cend  
$syntax/public virtual Covariance::~Covariance()/$$
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


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

Covariance::~Covariance()
{
}

/*************************************************************************
 *
 * Function: selectCovIndividual
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_selectCovIndividual$$
$spell
  covariance
  int
  cov
$$

$section Covariance::selectCovIndividual$$

$index setIndex$$
$cindex Set \the Current Individual$$

$table
$bold Prototype:$$   $cend  
$syntax/void selectCovIndividual( int /indexNew/ )
/$$
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

Sets the index for the current individual equal to 
$italic indexNew$$, where $math%indexNew > 0%$$.

$head Note$$

This function should never be called by the $code Covariance$$ 
member functions, neither directly nor indirectly. 
The reason for this is that the $code Covariance$$ member 
functions must not modify the state of the $code Covariance$$ 
objects.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void Covariance::selectCovIndividual( int indexNew ) 
{
  // If values have been cached, then compare the new and old 
  // values to see if the cached values are still valid. 
  if ( isCacheStatusValid() )
  {
    if ( index == indexNew )
    { 
      return;   // The cached values are ok.
    }
    else
    {
      setCacheStatusInvalid();
    }
  }

  index = indexNew;
}

/*************************************************************************
 *
 * Function: setCovPopPar
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_setCovPopPar$$
$spell
  const
  covariance
  dvec
  cov 
  valarray
$$

$section Covariance::setCovPopPar$$

$index setCovPopPar$$
$cindex Set \the Current Population Parameter$$

$table
$bold Prototype:$$   $cend  
$syntax/void setCovPopPar( const valarray<double>& /popParNew/ )
/$$
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

Sets the current value for the full population parameter 
vector equal to $italic popParNew$$.
$pre

$$
If this covariance matrix only depends on a subset of the 
elements in the full population parameter vector, then 
this function only sets the values for that subset.

$head Note$$

This function should never be called by the $code Covariance$$ 
member functions, neither directly nor indirectly. 
The reason for this is that the $code Covariance$$ member 
functions must not modify the state of the $code Covariance$$ 
objects.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/
void Covariance::setCovPopPar( const valarray<double>& popParNew ) 
{
  // If values have been cached, then check the elements of the 
  // parameter vector that affect this covariance matrix to see
  // if the cached values are still valid. 
  if ( isCacheStatusValid() )
  {
    if ( !isDifferent( covPopPar, popParNew ) )
    { 
      return;   // Cached values are ok.
    }
    else
    {
      setCacheStatusInvalid();
    }
  }
  covPopPar.resize( popParNew.size() );
  covPopPar = popParNew;
}

/*************************************************************************
 *
 * Function: setCovIndPar
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_setCovIndPar$$
$spell
  const
  covariance
  dvec
  ind
  cov
  valarray
$$

$section Covariance::setCovIndPar$$

$index setCovIndPar$$
$cindex Set \the Current Individual Parameter$$

$table
$bold Prototype:$$   $cend  
$syntax/void setCovIndPar( const valarray<double>& /indParNew/ )
/$$
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

Sets the current value for the full individual parameter 
vector equal to $italic indParNew$$.
$pre

$$
If this covariance matrix only depends on a subset of the 
elements in the full individual parameter vector, then 
this function only sets the values for that subset.

$head Note$$

This function should never be called by the $code Covariance$$ 
member functions, neither directly nor indirectly. 
The reason for this is that the $code Covariance$$ member 
functions must not modify the state of the $code Covariance$$ 
objects.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void Covariance::setCovIndPar( const valarray<double>& indParNew )
{
  // If values have been cached, then check the elements of the 
  // parameter vector that affect this covariance matrix to see
  // if the cached values are still valid. 
  if ( isCacheStatusValid() )
  {
    if ( !isDifferent( covIndPar, indParNew ) )
    { 
      return;   // Cached values are ok.
    }
    else
    {
      setCacheStatusInvalid();
    }
  }

  covIndPar.resize( indParNew.size() );
  covIndPar = indParNew;
}


/*************************************************************************
 *
 * Function: logdet
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_logdet$$
$spell
  const
  covariance
  logdet
  instantiated
$$

$section Covariance::logdet$$

$index logdet$$
$cindex Logarithm \of \the Determinant$$

$table
$bold Prototype:$$   $cend  
$syntax/virtual double logdet() const = 0
/$$
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

Returns the natural logarithm of the determinant of the current 
value of the covariance matrix.


$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/
double Covariance::logdet() const 
{ 
  return doLogdet(); 
}


/*************************************************************************
 *
 * Function: weightedSumOfSquares
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_weightedSumOfSquares$$
$spell
  const
  cov
  covariance
  dvec
  instantiated
$$

$section Covariance::weightedSumOfSquares$$

$index weightedSumOfSquares$$
$cindex Weighted Sum \of Squares$$

$table
$bold Prototype:$$   $cend  
$syntax/virtual double weightedSumOfSquares( const DoubleMatrix& \dvecZ\ ) const = 0;
/$$
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

Returns the sum of squares of the elements of the vector $math%Z%$$
weighted by the inverse of the current value of the covariance matrix.
To be specific, this function calculates
$math%

     T     -1
    Z   Cov    Z  ,

%$$
where Cov is the current value of the covariance matrix.


$head Arguments$$
$syntax/
/dvecZ/ 
/$$
The vector whose elements will be squared and then weighted in the sum.

$end
*/
double Covariance::weightedSumOfSquares( const valarray<double>& z ) const
{ 
  return doWeightedSumOfSquares(z); 
}

/*************************************************************************
 *
 * Function: cov
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_cov$$
$spell
  const
  cov
  covariance
  dvec
  instantiated
  inv
  ind
$$

$section Covariance::cov & (pure virtual) doCov$$
$index cov$$
$index doCov$$
$cindex Model \for Covariance$$

$table
$bold Prototype:$$   
$cend  
$syntax/public: void cov( DoubleMatrix& \ret\ ) const;/$$ $rend
$cend
$syntax/private: virtual void doCov( DoubleMatrix& \ret\ ) const = 0;/$$ $rend
$tend

See also: $xref/Covariance_cov_popPar//Covariance::cov_popPar()/$$,
$xref/Covariance_cov_indPar//Covariance::cov_indPar()/$$,
$xref/Covariance_inv//Covariance::inv()/$$,
$xref/Covariance_inv_popPar//Covariance::inv_popPar()/$$,
$xref/Covariance_inv_indPar//Covariance::inv_indPar()/$$,
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The public interface $syntax/void cov( DoubleMatrix& \ret\ ) const/$$
evaluates the user defined $syntax/virtual doCov( DoubleMatrix& \ret\ ) const/$$
at the current evaluation points.


$pre

$$



$head Arguments$$
$syntax/
/ret/ 
/$$
is a symmetric positive definite matrix resulted from evaluating $code doCov()$$.
$end
*/
/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/
void Covariance::cov( valarray<double>& ret ) const 
{ 
  doCov(ret);        
}

 /*************************************************************************
 *
 * Function: cov_popPar
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_cov_popPar$$
$spell
  const
  cov
  covariance
  wrt
  ind
  cols
  rows
  bool
  inv
  ind
$$

$section Covariance::cov_popPar & (pure virtual) doCov_popPar$$

$index cov_popPar$$
$index doCov_popPar$$
$cindex Evaluate Model \for Derivative of Covariance \wrt Population Parameter$$

$table
$bold Prototype:$$   
$cend  
$syntax/public: bool cov_popPar( DoubleMatrix& \ret\ ) const;/$$ $rend
$cend
$syntax/private: virtual bool doCov_popPar( DoubleMatrix& \ret\ ) const = 0;/$$ $rend
$tend

See also: $xref/Covariance_cov//Covariance::cov()/$$,
$xref/Covariance_cov_indPar//Covariance::cov_indPar()/$$,
$xref/Covariance_inv//Covariance::inv()/$$,
$xref/Covariance_inv_popPar//Covariance::inv_popPar()/$$,
$xref/Covariance_inv_indPar//Covariance::inv_indPar()/$$,

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The public interface $syntax/void cov_popPar( DoubleMatrix& \ret\ ) const/$$
evaluates the user defined $syntax/virtual doCov_popPar( DoubleMatrix& \ret\ ) const/$$
at the current evaluation points.

$pre

$$



$head Arguments$$
$syntax/
/ret/ 
/$$
is n by nPopPar matrix resulted from evaluating $code doCov_popPar()$$,
where n is the product of #rows and #cols of a matrix resulted from evaluating $code doCov()$$
and nPopPar is the size of the population parameter vector.
j-th column of matrix, $italic ret$$, contains the derivative of the covariance with respect to  
j-th element of the population parameter vector, in the column-major order.
Each column, or the derivative with respect to j-th, must be a symmetric positive definite matrix.

$end
*/
/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/
void Covariance::cov_popPar( valarray<double>& ret ) const 
{ 
  doCov_popPar(ret); 
}

/*************************************************************************
 *
 * Function: cov_indPar
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_cov_indPar$$
$spell
  const
  cov
  covariance
  wrt
  ind
  cols
  rows
  bool
  inv
  ind
$$

$section Covariance::cov_indPar & (pure virtual) doCov_indPar$$

$index cov_indPar$$
$index doCov_indPar$$
$cindex Evaluate Model \for Derivative of Covariance \wrt Individual Parameter$$

$table
$bold Prototype:$$
$cend
$syntax/public: bool cov_indPar( DoubleMatrix& \ret\ ) const;/$$ $rend
$cend
$syntax/private: virtual bool doCov_indPar( DoubleMatrix& \ret\ ) const = 0;/$$ $rend
$tend

See also: $xref/Covariance_cov//Covariance::cov()/$$,
$xref/Covariance_cov_popPar//Covariance::cov_popPar()/$$,
$xref/Covariance_inv//Covariance::inv()/$$,
$xref/Covariance_inv_popPar//Covariance::inv_popPar()/$$,
$xref/Covariance_inv_indPar//Covariance::inv_indPar()/$$,

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The public interface $syntax/void cov_indPar( DoubleMatrix& \ret\ ) const/$$
evaluates the user defined $syntax/virtual doCov_indPar( DoubleMatrix& \ret\ ) const/$$
at the current evaluation points.

$pre

$$



$head Arguments$$
$syntax/
/ret/ 
/$$
is n by nIndPar matrix resulted from evaluating $code doCov_indPar()$$,
where n is the product of #rows and #cols of a matrix resulted from evaluating $code doCov()$$
and nIndPar is the size of the individual parameter vector.
j-th column of matrix, $italic ret$$, contains the derivative of the covariance with respect to  
j-th element of the individual parameter vector, in the column-major order.
Each column, or the derivative with respect to j-th, must be a symmetric positive definite matrix.

$end
*/
/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/
void Covariance::cov_indPar( valarray<double>& ret ) const 
{ 
  doCov_indPar(ret);
}

/*************************************************************************
 *
 * Function: inv
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_inv$$
$spell
  const
  cov
  covariance
  wrt
  ind
  cols
  rows
  inv
  ind
$$

$section Covariance::inv & (pure virtual) doInv$$

$index inv$$
$index doInv$$
$cindex Inverse Model \for Covariance$$

$table
$bold Prototype:$$   
$cend  
$syntax/public: void inv( DoubleMatrix& \ret\ ) const;/$$ $rend
$cend  
$syntax/private: virtual void doInv( DoubleMatrix& \ret\ ) const = 0;/$$ $rend
$tend

See also: $xref/Covariance_cov//Covariance::cov()/$$,
$xref/Covariance_cov_popPar//Covariance::cov_popPar()/$$,
$xref/Covariance_cov_indPar//Covariance::cov_indPar()/$$,
$xref/Covariance_inv_popPar//Covariance::inv_popPar()/$$,
$xref/Covariance_inv_indPar//Covariance::inv_indPar()/$$,

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The public interface $syntax/void inv( DoubleMatrix& \ret\ ) const/$$
evaluates the user defined $syntax/virtual doInv( DoubleMatrix& \ret\ ) const/$$
at the current evaluation points.

$pre

$$



$head Arguments$$
$syntax/
/ret/ 
/$$
is a symmetric positive definite matrix resulted from evaluating $code doInv()$$
and the inverse of the matrix returned by $code doCov()$$.
$end
*/
/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

// No implementation for this abstract class.

/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/
void Covariance::inv( valarray<double>& ret ) const 
{ 
  doInv(ret);
}
/*************************************************************************
 *
 * Function: inv_popPar
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_inv_popPar$$
$spell
  const
  cov
  covariance
  wrt
  ind
  cols
  rows
  inv
  bool
  inv
$$

$section Covariance::inv_popPar & (pure virtual) doInv_popPar$$

$index inv_popPar$$
$index doInv_popPar$$
$cindex Derivative of Inverse Covariance \wrt Population Parameter$$

$table
$bold Prototype:$$   
$cend  
$syntax/public: bool covInv_popPar( DoubleMatrix& \ret\ ) const;/$$ $rend
$cend  
$syntax/private: virtual bool doCovInv_popPar( DoubleMatrix& \ret\ ) const = 0;/$$ $rend
$tend

See also: $xref/Covariance_cov//Covariance::cov()/$$,
$xref/Covariance_cov_popPar//Covariance::cov_popPar()/$$,
$xref/Covariance_cov_indPar//Covariance::cov_indPar()/$$,
$xref/Covariance_inv//Covariance::inv()/$$,
$xref/Covariance_inv_indPar//Covariance::inv_indPar()/$$,

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The public interface $syntax/void covInv_popPar( DoubleMatrix& \ret\ ) const/$$
evaluates the user defined $syntax/virtual doCovInv_popPar( DoubleMatrix& \ret\ ) const/$$
at the current evaluation points.

$pre

$$



$head Arguments$$
$syntax/
/ret/ 
/$$
is n by nPopPar matrix resulted from evaluating $code doCovInv_popPar()$$,
where n is the product of #rows and #cols of a matrix resulted from evaluating $code doCovInv()$$
and nPopPar is the size of the population parameter vector.
j-th column of matrix, $italic ret$$, contains the inverse of the derivative of the covariance with respect to  
j-th element of the population parameter vector, in the column-major order.
Each column, or the derivative with respect to j-th, must be a symmetric positive definite matrix.

$end
*/
/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void Covariance::inv_popPar( valarray<double>& ret ) const 
{ 
  doInv_popPar(ret); 
}
/*************************************************************************
 *
 * Function: covInv_indPar
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin Covariance_inv_indPar$$
$spell
  const
  cov
  covariance
  wrt
  ind
  cols
  rows
  inv
  bool
$$

$section Covariance::inv_indPar & (pure virtual) doInv_indPar$$

$index inv_indPar$$
$index doInv_indPar$$
$cindex Derivative of Inverse Covariance \wrt Population Parameter$$

$table
$bold Prototype:$$   
$cend  
$syntax/public: bool covInv_indPar( DoubleMatrix& \ret\ ) const;/$$ $rend
$cend  
$syntax/private: virtual bool doCovInv_indPar( DoubleMatrix& \ret\ ) const = 0;/$$ $rend
$tend

See also: $xref/Covariance_cov//Covariance::cov()/$$,
$xref/Covariance_cov_popPar//Covariance::cov_popPar()/$$,
$xref/Covariance_cov_indPar//Covariance::cov_indPar()/$$,
$xref/Covariance_inv//Covariance::inv()/$$,
$xref/Covariance_inv_popPar//Covariance::inv_popPar()/$$,

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The public interface $syntax/void covInv_indPar( DoubleMatrix& \ret\ ) const/$$
evaluates the user defined $syntax/virtual doCovInv_indPar( DoubleMatrix& \ret\ ) const/$$
at the current evaluation points.

$pre

$$



$head Arguments$$
$syntax/
/ret/ 
/$$
is n by nPopPar matrix resulted from evaluating $code doCovInv_indPar()$$,
where n is the product of #rows and #cols of a matrix resulted from evaluating $code doCovInv()$$
and nPopPar is the size of the individual parameter vector.
j-th column of matrix, $italic ret$$, contains the inverse of the derivative of the covariance with respect to  
j-th element of the individual parameter vector, in the column-major order.
Each column, or the derivative with respect to j-th, must be a symmetric positive definite matrix.

$end
*/
/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/
void Covariance::inv_indPar( valarray<double>& ret ) const 
{ 
  doInv_indPar(ret); 
}

/*========================================================================
 *
 *
 * Private function definitions
 *
 *
 *========================================================================*/
void Covariance::incrementUpdateCounter() const 
{ 
  timesCacheUpdated++; 
}
int Covariance::getTimesCacheUpdated() const 
{ 
  return timesCacheUpdated; 
}
void Covariance::setCacheStatusValid()   const 
{ 
  isCacheValid = true; 
}
void Covariance::setCacheStatusInvalid() const 
{ 
  isCacheValid = false; 
}

bool Covariance::isCacheStatusValid()    const 
{ 
  return isCacheValid; 
}

/*========================================================================
 *
 *
 * Local Definitions
 *
 *
 *========================================================================*/

namespace // [Begin: unnamed namespace]
{

/*************************************************************************
 *
 * Function: isDifferent
 *
 *
 * Compares the elements of a vector x to that of another vector y.
 * Return TRUE only if any element of x is NOT exactly equal to
 * the corresponding element of y.
 *
 *************************************************************************/

bool isDifferent( const valarray<double> & x, const valarray<double>& y )
{
  int n = x.size();
  assert( y.size() == n );

  for( int i=0; i<n; i++ )
  {
    if( x[i] != y[i] )
      return true;
  }
  return false;
}
/*************************************************************************
 *
 * Function: isSubsetDifferent
 *
 *
 * Returns true if any of the elements in the vector x, which contains
 * a subset of the elements in the vector y, is different from the 
 * element in y to which it corresponds.
 * 
 * The i-th element in the vector indexInY specifies the index for the
 * element in y that corresponds to the i-th element in x, i.e., 
 *
 *     x( i )  =  y( indexInY(i) )  .
 *
 * Thus, indexInY must have the same number of elements as x.
 *
 *************************************************************************/
/*
bool isSubsetDifferent( 
  const DoubleMatrix& dvecX, 
  const DoubleMatrix& dvecY, 
  const vector<int>& indexInY )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nX = dvecX.nr();


  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  // These must be column vectors.
  assert( dvecX.nc() == 1 );
  assert( dvecY.nc() == 1 );

  // There must be the same number of indices as elements in x.
  assert( indexInY.size() == nX );

  // There cannot be more elements in the subset than there
  // are elements in the full vector.
  assert( nX <= dvecY.nr() );


  //------------------------------------------------------------
  // Compare the elements in the subset to the full vector.
  //------------------------------------------------------------

  const double* x = dvecX.data();
  const double* y = dvecY.data();

  // Find the first element that's different and then return true.
  for ( int i = 0; i < nX; i++ )
  {
    assert( indexInY[i] >= 0 );
    assert( indexInY[i] < dvecY.nr() );

    if ( x[i] != y[indexInY[i]] )
    {
      return true;
    }
  }

  return false;
}
bool isSubsetDifferent( 
  const valarray<double>& x, 
  const valarray<double>& y, 
  const vector<int>& indexInY )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nX = x.size();


  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  // There must be the same number of indices as elements in x.
  assert( indexInY.size() == nX );

  // There cannot be more elements in the subset than there
  // are elements in the full vector.
  assert( nX <= y.size() );


  //------------------------------------------------------------
  // Compare the elements in the subset to the full vector.
  //------------------------------------------------------------

  // Find the first element that's different and then return true.
  for ( int i = 0; i < nX; i++ )
  {
    assert( indexInY[i] >= 0 );
    assert( indexInY[i] < y.size() );

    if ( x[i] != y[indexInY[i]] )
    {
      return true;
    }
  }

  return false;
}
*/

/*************************************************************************
 *
 * Function: copySubset
 *
 *
 * Copies a subset of the elements in the vector y to the vector x.
 * 
 * The i-th element in the vector indexInY specifies the index for the
 * element in y that corresponds to the i-th element in x, i.e., 
 *
 *     x( i )  =  y( indexInY(i) )  .
 *
 * Thus, indexInY must have the same number of elements as x.
 *
 *************************************************************************/
/*
void copySubset( 
  DoubleMatrix& dvecX, 
  const DoubleMatrix& dvecY, 
  const vector<int>& indexInY )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nX = dvecX.nr();


  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  // These must be column vectors.
  assert( dvecX.nc() == 1 );
  assert( dvecY.nc() == 1 );

  // There must be the same number of indices as elements in x.
  assert( indexInY.size() == nX );

  // There cannot be more elements in the subset than there
  // are elements in the full vector.
  assert( nX <= dvecY.nr() );


  //------------------------------------------------------------
  // Copy the subset of the elements from the full vector.
  //------------------------------------------------------------

  double* x = dvecX.data();
  const double* y = dvecY.data();

  for ( int i = 0; i < nX; i++ )
  {
    assert( indexInY[i] >= 0 );
    assert( indexInY[i] < dvecY.nr() );

    x[i] = y[indexInY[i]];
  }
}
*/
/*************************************************************************
 *
 * Function: seqIntVector
 *
 *
 * Returns a vector that contains the integers 0, 1, ... , (nElem-1).
 *
 *************************************************************************/
/*
vector<int> seqIntVector( int nElem )
{
  vector<int> v( nElem );
  for ( int i = 0; i < nElem; i++ )
  {
    v[i] = i;
  }
  return v;
}
*/

/*************************************************************************
 *
 * Function: isOkIndicesVector
 *
 *
 * Returns true if the vector of indices indexInY is valid, where 
 * indexInY is assumed to contain the indices of nX elements in a 
 * vector that is itself nY elements long.
 *
 *************************************************************************/
/*
bool isOkIndicesVector(  const vector<int>& indexInY, int nX, int nY )
{
  //------------------------------------------------------------
  // Check the dimensions.
  //------------------------------------------------------------

  bool ok = true;

  ok = ( nX >= 0 ) && ok;
  ok = ( nY >= 0 ) && ok;

  ok = ( nX <= nY ) && ok;
  
  // There must be the same number of indices as elements in x.
  ok = ( indexInY.size() == nX ) && ok;

  // There cannot be more elements in the subset than there
  // are elements in the full vector.
  ok = ( nX <= nY ) && ok;


  //------------------------------------------------------------
  // Check the indices.
  //------------------------------------------------------------

  for ( int i = 0; i < nX; i++ )
  {
    ok = ( indexInY[i] >= 0 ) && ok;
    ok = ( indexInY[i] < nY ) && ok;
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  return ok;
}

*/
} // [End: unnamed namespace]
