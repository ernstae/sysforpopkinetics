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
 * File: FullCovariance.cpp
 *
 *
 * This is an abstract subclass of Covariance from which subclasses for 
 * full covariance matrices may be derived.
 *
 * Objects of this class are used to calculate and cache various 
 * mathematical quantities involving full covariance matrices.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

//
// These pragma are to disable "too long literals" warnings generated
// as a result of using STL component, std::vector.
//
#pragma warning( disable : 4786 )  
#include <vector>

#include <cmath>
#include <cassert>


// SPK include files.
#include "DoubleMatrix.h"
#include "FullCovariance.h"
#include "transpose.h"
#include "isDmatEpsEqual.h"
#include "isSymmetric.h"

// NAG include files.
#include "nag.h"
#include "nagf03.h"
#include "nagf06.h"


using namespace std;


/*************************************************************************
 *
 * Function: updateCache
 *
 *
 * Checks the current state of the cache and recomputes the cached 
 * quantities if necessary.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Specifications related to this function.
 *------------------------------------------------------------------------*/

/*
$begin FullCovariance_RedefiningMathFunctions$$
$spell
  cholesky
  const
  covariance
  dmat
  logdet
  implementer
$$

$section Redefining the Cache Dependent Functions$$

$index updateCache$$
$cindex Redefining \the Cache \Dependent \Functions$$

The mathematical functions involving the logdet, weighted sum of squares, 
and their derivatives may be redefined in subclasses of this class
in order to take advantage of more efficient algorithms or methods known 
to the implementer of the subclasses.
It is important to note that these functions depend on the cached values 
that this class maintains.
$pre

$$
If any of these functions is redefined, then its new implementation 
must contain a call to the function $code FullCovariance::updateCache()$$, 
and the call must occur before the redefined function makes use of the 
cached values of the Cholesky factor for the covariance matrix 
$code choleskyCached$$ or the cached value of the log determinant 
value $code logdetCached$$.  
$pre

$$
For example, if the function being redefined is 
$code weightedSumOfSquares()$$, and if the subclass is called 
$code UserFullCovariance$$, then the redefined version of 
$code weightedSumOfSquares()$$ must look something like this:
$codep

    const double UserFullCovariance::weightedSumOfSquares()
    { 
      FullCovariance::updateCache();

      // Calculate the weighted sum of squares using the cached 
      // Cholesky factor and a more efficient algorithm.
      double efficientWeightedSumOfSquares;

      // ...

      return efficientWeightedSumOfSquares;
    }

$$

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void FullCovariance::updateCache() const
{
  if ( !isCacheStatusValid() )
  {
    // Cache the Cholesky factor.
    calcCholesky( choleskyCached );

    // Cache the log of the determinant.
    logdetCached = calcLogdetFromCholesky( choleskyCached );

    setCacheStatusValid();

    incrementUpdateCounter();
  }
}


/*************************************************************************
 *
 * Function: FullCovariance (Default Constructor)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin FullCovariance_default_constructor$$
$spell 
  Covariance
$$

$section Default Constructor$$

$index FullCovariance, Default Constructor$$

$table
$bold Prototype:$$   $cend  
$syntax/protected FullCovariance::FullCovariance()/$$
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
Since $code FullCovariance$$ is an abstract class, the direct use of this constructor
is only allowed for its subclasses.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

FullCovariance::FullCovariance()
  : 
  choleskyCached  ( 0.0, 0 ),
  logdetCached    ( 0.0 )
{
}


/*************************************************************************
 *
 * Function: FullCovariance (Copy Constructor)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin FullCovariance_copy_constructor$$
$spell 
  const
  copyable
  FullCovariance
$$

$section Copy Constructor$$

$index FullCovariance, Copy Constructor$$

$table
$bold Prototype:$$   $cend  
$syntax/protected FullCovariance::FullCovariance( const FullCovariance &/right/ )/$$
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
Since $code FullCovariance$$ is an abstract class, the direct use of this constructor
is only allowed for its subclasses.
$pre

$$
The copy constructor is provided to make the $code FullCovariance$$ class
more standard even though SPK itself does not currently require
$code FullCovariance$$ objects to be copyable.

$head Arguments$$
$syntax/
right
/$$
The $code FullCovariance$$ to be copied.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

FullCovariance::FullCovariance( const FullCovariance &right )
  : 
  choleskyCached  ( right.choleskyCached ),
  logdetCached    ( right.logdetCached )
{
}


/*************************************************************************
 *
 * Function: FullCovariance (Assignment Operator)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin FullCovariance_assignment_operator$$
$spell 
  assignable
  const
  Covariance
$$

$section Assignment Operator$$

$index FullCovariance, Assignment Operator$$

$table
$bold Prototype:$$   $cend  
$syntax/protected FullCovariance::FullCovariance& operator=( const FullCovariance &/right/ )/$$
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
Since $code FullCovariance$$ is an abstract class, the direct use of this operator
is only allowed for its subclasses.
$pre

$$
The assignment operator is provide to make the $code FullCovariance$$ class
more standard even though SPK itself does not currently require
$code FullCovariance$$ objects to be assignable.

$head Arguments$$
$syntax/
right
/$$
The $code FullCovariance$$ to be copied.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

FullCovariance& FullCovariance::operator=(const FullCovariance &right)
{
  choleskyCached = right.choleskyCached;
  logdetCached   = right.logdetCached;

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
$begin FullCovariance_default_destructor$$
$spell 
  Covariance
$$

$section Default Destructor$$

$index FullCovariance, Default Destructor$$

$table
$bold Prototype:$$   $cend  
$syntax/public virtual FullCovariance::~FullCovariance()/$$
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

FullCovariance::~FullCovariance()
{
}


/*************************************************************************
 *
 * Function: private: virtual void doCov( DoubleMatrix & ret )  = 0
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin FullCovariance_doCov$$
$spell
  calc
  Cov
  covariance
  const
  dmat
  instantiated
$$

$section FullCovariance::doCov$$

$index cov$$
$cindex \Calculate \the Covariance \Matrix$$

$table
$bold Prototype:$$   $cend  
$syntax/private: void doCov( DoubleMatrix& /dmatCov/ ) const = 0/$$ $rend

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

Returns the current value of the covariance matrix.

$head Note$$

(1.) This function is declared pure virtual so that it must be defined in 
some subclass of this abstract class before objects can be instantiated.
$pre

$$
(2.) This function not make use of any cached values, and thus it's 
implementation does not need to call the cache update function. 

$head Arguments$$
$syntax/
/dmatCov/ 
/$$
On output this matrix will contain the elements of the covariance matrix, 
which must be square, symmetric, and positive-definite.  
Its dimensions may be changed on output from what they were on input.

$end
*/


/*************************************************************************
 *
 * Function: calcCholesky
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin FullCovariance_calcCholesky$$
$spell
  calc
  cholesky
  chol
  Cov
  covariance
  const
  dmat
  valarray
  Spk
$$

$section FullCovariance::calcCholesky$$

$index calcCholesky$$
$cindex \Calculate \the \Covariance \Matrix Cholesky Factor$$

$table
$bold Prototype:$$   $cend  
$syntax/virtual void calcCholesky( valarray<double>& /chol/ ) const
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

Sets the matrix $math%Chol%$$ equal to the lower triangular Cholesky 
factor of the current value of the covariance matrix $math%Cov%$$
such that
$math%

                    T
  Cov  =  Chol  Chol   .

%$$

$head Note$$

(1.) This function is declared virtual and can be redefined in subclasses
of this class.
$pre

$$
(2.) This function should not make use of any cached values.  
Thus, if this function is redefined, it's new implementation 
does not need to call the cache update function. 

$head Arguments$$
$syntax/
/dmatChol/ 
/$$
On output this matrix will contain the lower triangular Cholesky 
factor of the current value for the covariance matrix $math%Cov%$$.  
Its dimensions may be different from what they were on input.
When it fails to compute a Cholesky factorization, it throws
an $xref/SpkException//SpkException/$$ exception.

$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Note: this does no caching, i.e., it does not set choleskyCached, 
 * since we don't want to require the user to set it
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Local Declarations
 *------------------------------------------------------------------------*/
static DoubleMatrix dvecP(__FILE__);
/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void FullCovariance::calcCholesky( valarray<double>& VAChol )  const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Initially set the Cholesky factor Chol equal to the current value 
  // for the covariance matrix Cov.
  doCov( VAChol );
  DoubleMatrix dmatChol( VAChol, static_cast<int>(sqrt( static_cast<double>(VAChol.size()))) );

  int nChol = dmatChol.nr();
  assert( VAChol.size() == nChol * nChol );

  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  assert( dmatChol.nc() == nChol );    // Cov must be square.
  assert( nChol > 0 );                 // Cov must have at least one row.
  assert( isSymmetric( dmatChol ) );   // Cov must be symmetric.


  //------------------------------------------------------------
  // Define the parameters for the function nag_real_cholesky (f03aec), 
  // which computes a Cholesky factorization of a real symmetric
  // positive-definite matrix, and evaluates its determinant.
  //------------------------------------------------------------
  // Parameter: n.
  // Input: n, the order of the matrix Cov. 
  // Output: unspecified.
  // Constraint: n >= 1. 
  Integer n = nChol;
  assert( n >= 1 );

  // Parameter: tdchol.
  // Input: the last dimension of the array chol as declared in the
  // function from which nag_real_cholesky is called.
  // Output: unspecified.
  // Constraint: tdchol >= n.
  Integer tdchol = n;

  // Parameter: chol.
  // Input: the upper triangle of the n by n positive-definite
  // symmetric matrix Cov.  The elements of the array below the
  // diagonal need not be set.
  // Output: the sub-diagonal elements of the lower triangular
  // matrix Chol. The upper triangle of Cov is unchanged.
  // Note:  The data elements of DoubleMatrix matrices are stored 
  // in column-major order while NAG routines expect arrays to be
  // stored in row-major order.
  double* chol = dmatChol.data();

  // Parameter: p
  // Input: unspecified.
  // Output: the reciprocals of the diagonal elements of Chol.
  dvecP.resize( nChol, 1);
  double* p = dvecP.data();

  // Parameter: detf.
  // Parameter: dete.
  // Input: unspecified.
  // Output: the determinant of Cov is given by detf * 2.0^dete.
  // It is given in this form to avoid overflow or underflow.
  double detf;
  Integer dete;


  //------------------------------------------------------------
  // Perform the Cholesky factorization.
  //------------------------------------------------------------

  // Revisit - Exceptions - Mitch: if an error occurs in this
  // NAG routine, the program will be stopped using exit or abort. 
  // Brad: changed to an assert for tracking in debugger 12/12/00
  // Sachiko: Changed to exception throwing 10/15/2002
  static NagError fail;
  INIT_FAIL(fail);
  nag_real_cholesky( n, chol, tdchol, p, &detf, &dete, &fail);
  if( fail.code != NE_NOERROR )
  {
    switch( fail.code )
    {
    case NE_NOT_POS_DEF:
      throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, 
        "The matrix is not positive-definite,  possibly due to round-ing errors.",
      __LINE__, __FILE__ );
      break;
    default:
      throw SpkException( SpkError::SPK_UNKNOWN_ERR, 
        "Failed to compute a Cholesky factorization.",
      __LINE__, __FILE__ );
      break;
    }
  }


  //------------------------------------------------------------
  // Transform the Cholesky factor for output.
  //------------------------------------------------------------

  // ************************************************************
  // * Note:  The data elements of DoubleMatrix matrices are    *
  // * stored in column-major order while NAG routines expect   *
  // * arrays to be stored in row-major order.                  *
  // ************************************************************

  // Set the final values for the elements of the DoubleMatrix that 
  // contains the lower triangular Cholesky factor Chol.
  for ( int i = 0; i < nChol; i++ )
  {
    // Set the diagonal elements.
    chol[i + i * nChol] = 1.0 / p[i];
    
    for ( int j = 0; j < i; j++ )
    {
      // Copy the super-diagonal elements to the sub-diagonal.
      chol[i + j * nChol] = chol[j + i * nChol];
      
      // Zero the super-diagonal elements.
      chol[j + i * nChol] = 0.0;
    }
  }
  VAChol = dmatChol.toValarray();
}


/*************************************************************************
 *
 * Function: calcLogdetFromCholesky
 *
 *
 * Returns the log of the determinant of the matrix for which the 
 * lower triangular Cholesky factor is Chol,
 *
 *                      T
 *     logdet( Chol Chol  )  .
 *
 *************************************************************************/

double FullCovariance::calcLogdetFromCholesky( const valarray<double>& chol ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nChol = static_cast<int>(sqrt(static_cast<double>(chol.size())));


  //------------------------------------------------------------
  // Validate the current state (debug version only).
  //------------------------------------------------------------

  assert( chol.size() == nChol * nChol );


  //------------------------------------------------------------
  // Compute the logdet.
  //------------------------------------------------------------

  // Sum the logs of the diagonals.
  double logsum = 0.0;
  for ( int i = 0; i < nChol; i++ )
  {
    logsum += log( choleskyCached[i + i * nChol] );
  }

  // Return
  //                      T                       2         ---
  //     logdet( Chol Chol  )  =  log[ det( Chol )  ]  =  2 >    log( chol   )  .
  //                                                        ---           ii
  return 2.0 * logsum;
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
$begin FullCovariance_logdet$$
$spell
  const
  covariance
  logdet
$$

$section FullCovariance::logdet$$

$index logdet$$
$cindex Logarithm \of \the Determinant$$

$table
$bold Prototype:$$   $cend  
$syntax/virtual const double logdet() const
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

$head Note$$

(1.) This function is declared virtual and can be redefined in subclasses
of this class.
$pre

$$
(2.) If this function is redefined, it's new implementation 
must update the cached values before it does any computations.  
See $mref/FullCovariance_RedefiningMathFunctions/$$ for more.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

double FullCovariance::doLogdet() const
{ 
  updateCache();

  return logdetCached;
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
$begin FullCovariance_weightedSumOfSquares$$
$spell
  calc
  Cov
  covariance
  const
  dvec
  valarray
$$

$section FullCovariance::weightedSumOfSquares$$

$index weightedSumOfSquares$$
$cindex \Calculate \the Weighted Sum \Of Squares$$

$table
$bold Prototype:$$   $cend  
$syntax/virtual const double weightedSumOfSquares( const valarray<double>& /z/ ) const
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

$head Note$$

(1.) This function is declared virtual and can be redefined in subclasses
of this class.
$pre

$$
(2.) If this function is redefined, it's new implementation 
must update the cached values before it does any computations.  
See $mref/FullCovariance_RedefiningMathFunctions/$$ for more.

$head Arguments$$
$syntax/
/z/ 
/$$
The vector whose elements will be squared and then weighted in the sum.

$end
*/


/*------------------------------------------------------------------------
 * Local Declarations
 *------------------------------------------------------------------------*/

// Declare this static to reduce the calls to the DoubleMatrix constructor.
static DoubleMatrix dvecX( __FILE__ );


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

double FullCovariance::doWeightedSumOfSquares( const valarray<double>& z ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  updateCache();

  DoubleMatrix dmatCholeskyCached( choleskyCached, static_cast<int>(sqrt(static_cast<double>(choleskyCached.size()))));
  int nChol = dmatCholeskyCached.nr();


  //------------------------------------------------------------
  // Validate the current state (debug version only).
  //------------------------------------------------------------

  assert( dmatCholeskyCached.nc() == nChol );


  //------------------------------------------------------------
  // Define the parameters for the NAG function dtrsv (f06pjc) 
  // that performs one of the matrix-vector operations,
  //
  //             -1                    -T
  //    x  <--  A   x    or    x <--  A   x  ,
  //
  //  where A is an n by n real triangular matrix, and x is an 
  //  n element real  vector.
  //------------------------------------------------------------
  
  // Parameter: uplo  -  MatrixTriangle
  // Input: specifies whether A is upper or lower triangular as follows:
  // if uplo = UpperTriangle, A is upper triangular;
  // if uplo = LowerTriangle, A is lower triangular.
  // Constraint: uplo = UpperTriangle or LowerTriangle.
  // Note:  Because the data elements of DoubleMatrix are stored 
  // in column-major order and NAG routines expect arrays to be
  // stored in row-major order, the lower triangle Cholesky factor
  // in the cached DoubleMatrix, dmatCholeskyCached, is upper triangular
  // as far as the NAG routine is concerned.
  MatrixTriangle uplo = UpperTriangle;

  // Parameter: trans  -  MatrixTranspose
  // Input: specifies the operation to be performed as follows:
  // if trans = NoTranspose, x  <--  A^(-1) x ;
  // if trans = Transpose or ConjugateTraspose, x  <--  A^(-T) x.
  // Constraint: trans = NoTranspose, Transpose or ConjugateTranspose.
  // Note:  Since we want the product of the inverse of the lower
  // triangular Cholesky factor (rather than the upper triangular
  // factor) we want the NAG routine to take the transpose of A
  // in addition to computing its inverse.
  MatrixTranspose trans = Transpose;

  // Parameter: diag  -  MatrixUnitTriangular
  // Input: specifies whether A has non-unit or unit diagonal elements, 
  // as follows:
  // if diag = NotUnitTriangular, the diagonal elements are stored explicitly;
  // if diag = UnitTriangular, the diagonal elements are assumed to be 1, and 
  //           are not referenced.
  // Constraint: diag = NotUnitTriangular or UnitTriangular.
  MatrixUnitTriangular diag = NotUnitTriangular;

  // Parameter: n  -  Integer
  // Input: n, the order of the matrix A.
  // Constraint: n >= 0.
  Integer n = nChol;
  assert( n >= 0 );

  // Parameter: a[n][tda]  -  double
  // Input: the n by n triangular matrix A. If uplo = UpperTriangle, 
  // A is upper triangular and the elements of the array below the 
  // diagonal are not referenced; if uplo = LowerTriangle, A is lower 
  // triangular and the elements of the array above the diagonal are 
  // not referenced. If diag = UnitTriangular, the diagonal elements 
  // of A are not referenced, but are assumed to be 1.
  // Note:  See the note for the uplo parameter above.
  double* a = dmatCholeskyCached.data();

  // Parameter: tda  -  Integer
  // Input: the last dimension of the array a as declared in the 
  // function from which dtrsv is called.
  // Constraint: tda >= max(1,n).
  Integer tda = n;

  // Parameter: x[]  -  double []
  // Input: the vector x of length n.
  // Output: the updated vector x.
  DoubleMatrix dvecZ( z, 1 );
  dvecX = dvecZ;
  double* x = dvecX.data();

  // Parameter: incx  -  Integer
  // Input: the increment in the subscripts of x between successive 
  // elements of x.
  // Constraint: incx != 0.
  Integer incx = 1;


  //------------------------------------------------------------
  // Compute the product of the Cholesky inverse and the input vector.
  //------------------------------------------------------------

  // Since on input to dtrsv,
  //
  //     x  =  z  ,
  // 
  // and
  //
  //     A  =  Chol  ,  
  //
  // then on output, 
  //
  //               -1
  //     x  =  Chol    z  .
  //
  dtrsv(uplo, trans, diag, n, a, tda, x, incx);

  choleskyCached = dmatCholeskyCached.toValarray();

  //------------------------------------------------------------
  // Compute the weighted sum of squares.
  //------------------------------------------------------------

  // Compute the weighted sum of squares,
  //
  //      T     -1          T             T  -1
  //     z   Cov    z  =   z   ( Chol Chol  )    z
  //
  //                      -            -  T
  //                     |      -1      |         -1
  //                 =   |  Chol    z   |     Chol     z.
  //                      -            -
  //
  //                            2
  //                 =  || x  ||   .
  //
  double value = 0.0;
  for ( int i = 0; i < nChol; i++ )
  {
    value += x[i] * x[i];
  }

  return value;
}

