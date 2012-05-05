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

extern "C"{
#include <clapack.h>
#include <cblas.h>
}

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
#include "symmetrize.h"
void FullCovariance::calcCholesky( valarray<double>& VAChol )  const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Initially set the Cholesky factor Chol equal to the current value 
  // for the covariance matrix Cov.
  doCov( VAChol );
  int n = static_cast<int>( sqrt( static_cast<double>( VAChol.size() ) ) );
  int lda = n;
  clapack_dpotrf( CblasColMajor, CblasLower, n, &VAChol[0], lda );
  symmetrize( VAChol, n, VAChol );
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

#include "symmetrize.h"
/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

double FullCovariance::doWeightedSumOfSquares( const valarray<double>& z ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  updateCache();
  
  //------------------------------------------------------------
  // Define the parameters for the BLAS function dtrsv 
  // that performs one of the matrix-vector operations,
  //
  //             -1                    -T
  //    x  <--  A   x    or    x <--  A   x  ,
  //
  //  where A is an n by n real triangular matrix, and x is an 
  //  n element real  vector.
  //------------------------------------------------------------

  // Parameter: order - enum CBLAS_ORDER
  // Input: specifies the order in which the matrix A is stored in the array, a.
  // if order = CblasColMajor, the array, a, is assumed to store the values of A
  //            in the column major order.
  // if order = CblasRowMajor, the array, a, is assumed to store the values of A
  //            in the row major order.
  // NOTE: In SPK, all matrices are stored in the column major order.
  enum CBLAS_ORDER order = CblasColMajor;

  // Parameter: uplo  -  enum CBLAS_UPLO
  // Input: specifies whether A is upper or lower triangular as follows:
  // if uplo = CblasUpper, A is upper triangular;
  // if uplo = CblasLower, A is lower triangular.
  enum CBLAS_UPLO uplo = CblasLower;

  // Parameter: trans  -  enum CBLAS_TRANSPOSE
  // Input: specifies the operation to be performed as follows:
  // if trans = CblasNoTrans, x  <--  A^(-1) x ;
  // if trans = Transpose or ConjugateTraspose, x  <--  A^(-T) x.
  // Constraint: trans = CblasNoTrans, CblasTrans or CblasConjTrans.
  enum CBLAS_TRANSPOSE trans = CblasNoTrans;

  // Parameter: diag  -  MatrixUnitTriangular
  // Input: specifies whether A has non-unit or unit diagonal elements, 
  // as follows:
  // if diag = CblasNonUnit, the diagonal elements are stored explicitly;
  // if diag = CblasUnit, the diagonal elements are assumed to be 1, and 
  //           are not referenced.
  // Constraint: diag = CblasNonUnit or CblasUnit.
  enum CBLAS_DIAG diag = CblasNonUnit;

  // Parameter: n  -  
  // Input: n, the order of the matrix A.
  // Constraint: n >= 0.
  int n = static_cast<int>( sqrt( static_cast<double>( choleskyCached.size() ) ) );
  assert( n * n == choleskyCached.size() );

  // Parameter: lda  -  int
  // Input: the last dimension of the array a as declared in the 
  // function from which dtrsv is called.
  // Constraint: lda >= max(1,n).
  int lda = n;
  assert( z.size() == n );

  // Parameter: a[n][lda]  -  double
  // Input: the n by n triangular matrix A. If uplo = CblasUpper, 
  // A is upper triangular and the elements of the array below the 
  // diagonal are not referenced; if uplo = CblasLower, A is lower 
  // triangular and the elements of the array above the diagonal are 
  // not referenced. If diag = CblasUnit, the diagonal elements 
  // of A are not referenced, but are assumed to be 1.
  // Note:  See the note for the uplo parameter above.
  double* a = &(choleskyCached[0]);

  // Parameter: x[]  -  double []
  // Input: the vector x of length n.
  // Output: the updated vector x.
  valarray<double> x( z );

  // Parameter: incx  -  int
  // Input: the increment in the subscripts of x between successive 
  // elements of x.
  // Constraint: incx != 0.
  int incx = 1;
  
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

  cblas_dtrsv( order, uplo, trans, diag, n, a, lda, &x[0], incx );

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
  for ( int i = 0; i < n; i++ )
  {
    value += x[i] * x[i];
  }
  return value;

}

