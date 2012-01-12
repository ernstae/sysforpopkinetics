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
 * File: det.cpp
 *
 *
 * Computes the determinant of a positive definite symmetric matrix.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: det
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin det$$
$spell det const dmat iostream iomanip int pd pl cout setiosflags ios 
setprecision endl cstdlib pow cmath namespace std
  Spk
$$

$section Determinant of a Positive Definite Symmetric Matrix$$

$index det$$
$cindex Determinant \of \a matrix$$

$table
$bold Prototype$$   $cend
$syntax/void det( const DoubleMatrix& /dmatA/, double* /pdB/, long int* /plC/ )/$$   $rend 
$tend

See also: $xref/inverse//inverse()/$$

$fend 25$$
$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Evaluates the determinant of the positive definite symmetric matrix $math%A%$$.
$pre

$$
To be specific, this function computes the determinant of 
positive definite symmetric $math%A%$$ as
$math%
                    c
    det( A ) = b * 2  ,

%$$
where $math%b%$$ is a real number and $math%c%$$ is an integer.

$head Arguments$$
$syntax/
/dmatA/
/$$
The $code DoubleMatrix$$ $italic dmatA$$ contains the 
positive definite symmetric matrix $math%A%$$.  
If the matrix does not have these properties, 
it throws an $xref/SpkException//SpkException/$$.

$syntax/

/pdB/
/$$
On output, the $code double$$ value pointed to by $italic pdB$$ will 
be equal to $math%b%$$.
Note that the $code double$$ value pointed to by $italic pdB$$ must 
be declared in the function that calls this function.

$syntax/

/plC/
/$$
On output, the $code long int$$ value pointed to by $italic plC$$ will 
be equal to $math%c%$$.
Note that the $code long int$$ value pointed to by $italic plC$$ must 
be declared in the function that calls this function.


$head Example$$
If you compile and link the C++ program,
$codep

#include "det.h"
#include "DoubleMatrix.h"
#include <iostream>
#include <cstdlib>
#include <iomanip>
#include <cmath>

int main()
{
  using namespace std;

  int nARow = 2;
  int nACol= nARow;
  DoubleMatrix dmatA( nARow, nACol );
  double* pdAData = dmatA.data();
  pdAData[0 + 0 * nARow] = 1000000.0;
  pdAData[0 + 1 * nARow] =      -1.0;
  pdAData[1 + 0 * nARow] =      -1.0;
  pdAData[1 + 1 * nARow] =       2.0;

  double b;
  long int c;
  det( dmatA , &b, &c );
  
  cout << setiosflags(ios::scientific) << setprecision(15);
  cout << "A = " << endl;
  dmatA.print();
  cout << endl;
  cout << "det( A )     = " << b * pow( 2.0, c ) << endl;

  return EXIT_SUCCESS;
}

$$
then it should output the following when it is run:
$codep

A =
[1.000000000000000e+006, -1.000000000000000e-003]
[-1.000000000000000e+000, 2.000000000000000e+000]

det( A )     = 1.999999000000000e+006
$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
extern "C"{
#include <cblas.h>
#include <clapack.h>
};

#include <iostream>
#include <cassert>
#include <cmath>
#include <stdio.h>

#include "det.h"
#include "transpose.h"
#include "isDmatEpsEqual.h"
#include "DoubleMatrix.h"
#include "intToOrdinalString.h"


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void det( const DoubleMatrix &A, double* pdB, long int* plC )
{
  using namespace std;
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int m = A.nr();
  int n = A.nc();

  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  assert( m == n );   // The matrix A must be square.
  assert( m > 0 );    // A must have at least one row.

  #ifndef NDEBUG //===============[Begin: debug only code]========

  // Create a scale matrix.
  DoubleMatrix scale(A);
  double * pScale = scale.data();
  for (int i = 0; i < n; i++)
  { 
      for(int j = 0; j < n; j++)
      {
            pScale[i + j * n] = 10.0 * (fabs( pScale[i + j * n] +  pScale[j + i * n] )) ;
      }
  }
  
  // Verify that A is symmetric up to epsilon by checking the
  // equality of it and its transpose.
  assert( isDmatEpsEqual( A, A, scale ) );
  
  #endif // ======================[End:   debug only code]========

  assert( pdB != 0 );
  assert( plC != 0 );


  //------------------------------------------------------------
  // Define the parameters for the function clapack_dpotrf(), 
  // which computes a Cholesky factorization of a real symmetric
  // positive-definite matrix, and evaluates its determinant.
  // Note: this function (det) only makes use of the 
  // determinant value and not the Cholesky factorization.
  //------------------------------------------------------------
  
  // Parameter: n.
  // Input: n, the order of the matrix A. 
  // Output: unspecified.
  // Constraint: n >= 1. 
  assert( n >= 1 );

  // Parameter: lda.
  // Input: the last dimension of the array a as declared in the
  // function from which dgetrm() is called.
  // Output: unspecified.
  // Constraint: lda >= n.
  int lda = n;

  // Parameter: a.
  // Input: the upper triangle of the n by n positive-definite
  // symmetric matrix A.  The elements of the array below the
  // diagonal need not be set.
  // Output: the L (lower) triangle without diagonal elements which are unity
  //         and the U (upper) triangle
  double a[n*n];
  copy( A.data(), A.data()+m*n, a );

  // Parameter: detf.
  // Parameter: dete.
  // Input: unspecified.
  // Output: the determinant of A is given by detf * 2.0^dete.
  // It is given in this form to avoid overflow or underflow.
  double detf;
  int    dete;

  //------------------------------------------------------------
  // Perform the Cholesky factorization.
  // (using ATLAS implementation of DPOTRF())
  //------------------------------------------------------------
  //
  // DPOTRF() computes the Cholesky factorization of a real 
  // symmetric/Hermitian positive definite matrix A such that
  // The factorization has the form A = U^H * U, if uplo
  // is CblasUpper, or A = L*L^H, if uplo is CblasLower,
  // where U is an upper triangular matrix and L is lower triangular.
  // This factorization is sometimes referred to as
  // "taking the square root of" the matrix.  Thus, one
  // should immediately realize that the resulting L
  // contains the square root of the actual values.
  //
  enum CBLAS_UPLO  uplo  = CblasLower;
  enum CBLAS_ORDER order = CblasColMajor;

  int info = clapack_dpotrf( order, uplo, n, a, lda );
  if( info > 0 )
    {
      char mess[SpkError::maxMessageLen()];
      snprintf( mess, SpkError::maxMessageLen(), 
                "The leading minor of order %d is not positive definite, and the factorization \ncould not be completed!",
	       info );
      throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, 
			  mess,
			  __LINE__, __FILE__ );
    }
  else if( info < 0 )
    {
      char mess[SpkError::maxMessageLen()];
      snprintf( mess, SpkError::maxMessageLen(),
                "Programming error!  The %s argument to clapack_dgetrf() is illegal.", 
               intToOrdinalString( -info, ONE_IS_FIRST_INT ).c_str() );
      throw SpkException( SpkError::SPK_UNKNOWN_ERR, 
			  mess,
			  __LINE__, __FILE__ );
    }

  /*
  // trace #of permutations
  int nInterchanges = 0;
  for( int i=0, tmp; i<n-1; i++ )
    {
      if( ipiv[i] != i )
	{
	  tmp = ipiv[i];
	  ipiv[ i ] = ipiv[ tmp ];
	  ipiv[ tmp ] = i;
	  nInterchanges++;
	}
    }
  // #permulations = even --- sign = +1
  //               = odd  --- sign = -1
  //double sign = pow( -1.0, nInterchanges );
  */

  // The determinant of symmetric positive definite matrix
  // is ALWAYS POSITIVE!!!
  double sign = +1.0;

  //                    n
  //                  -----
  //                   | |   
  //  sign * det(A) =  | | L(i,j)^2, i=j
  //
  //                 i=0, j=0
  //
  double det = sign;
  for( int j=0; j<n; j++ )
    {  
      for( int i=0; i<n; i++ )
	{
	  if( i==j )
	    {
	      // To avoid over- or under-flow,
	      // devide (make smaller) the devider
	      // by two.
	      // Note: The devider does not have to
	      // be 2.  It is 2 only because the NAG implementation
	      // of GDETRF() returned the determiant-related info
	      // in this way.
	      det *= a[j*n+i] * a[j*n+i] / 4.0;
	    }
	}
    }
  
  detf = det;
  dete = 2 * n;
  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------
 
  // Set the pieces of the determinant.
  *pdB = detf;
  *plC = dete;

  return;
}
/*************************************************************************
 *
 *
 * Computes the determinant of a matrix using LU factorization
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*************************************************************************
 *
 * Function: det (valarray version)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin detVA$$
$spell det const dmat iostream iomanip int pd pl cout setiosflags ios 
setprecision endl cstdlib pow cmath namespace std
valarray
Spk
$$

$section Determinant of a Matrix (valarray version)$$

$index det$$
$cindex Determinant \of \a matrix$$

$table
$bold Prototype$$   $cend
$syntax/void det( const SPK_VA::valarray<double>& /A/, int /n/, double* /pdB/, long int* /plC/ )/$$   $rend 
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
Evaluates the determinant of the square matrix $math%A%$$.
$pre

$$
To be specific, this function computes the determinant of $math%A%$$ as
$math%
                    c
    det( A ) = b * 2  ,

%$$
where $math%b%$$ is a real number and $math%c%$$ is an integer.

$head Arguments$$
$syntax/
/A/
/$$
The $code valarray$$ $italic A$$ contains the square matrix $math%A%$$ in the column major order.  
If the matrix is not LU-decomposable, it throws an $xref/SpkException//SpkException/$$.
$syntax/

/n/
/$$
The integer $italic n%$$ specifies the number of columns in $math%A%$$.
$syntax/

/pdB/
/$$
On output, the $code double$$ value pointed to by $italic pdB$$ will 
be equal to $math%b%$$.
Note that the $code double$$ value pointed to by $italic pdB$$ must 
be declared in the function that calls this function.

$syntax/

/plC/
/$$
On output, the $code long int$$ value pointed to by $italic plC$$ will 
be equal to $math%c%$$.
Note that the $code long int$$ value pointed to by $italic plC$$ must 
be declared in the function that calls this function.


$head Example$$
If you compile and link the C++ program,
$codep

#include "det.h"
#include "SpkValarray.h"
#include <iostream>
#include <cstdlib>
#include <iomanip>
#include <cmath>

using SPK_VA::valarray;

int main()
{
  using namespace std;

  int nARow = 2;
  int nACol= nARow;
  valarray<double> A( nARow * nACol );

  A[0 + 0 * nARow] = 1000000.0;
  A[0 + 1 * nARow] =      -1.0;
  A[1 + 0 * nARow] =      -1.0;
  A[1 + 1 * nARow] =       2.0;

  double b;
  long int c;
  det( A, nACol, &b, &c );
  
  cout << setiosflags(ios::scientific) << setprecision(15);
  cout << "A = " << endl;
  printInMatrix( A, nACol );
  cout << endl;
  cout << "det( A )     = " << b * pow( 2.0, c ) << endl;

  return EXIT_SUCCESS;
}

$$
then it should output the following when it is run:
$codep

A =
[1.000000000000000e+006, -1.000000000000000e-003]
[-1.000000000000000e+000, 2.000000000000000e+000]

det( A )     = 1.999999000000000e+006
$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include "SpkValarray.h"

using SPK_VA::valarray;

void det( const valarray<double> &A, int n, double* pdB, long int* plC )
{
  using namespace std;
 
  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  assert( n > 0 );               // A must have at least one row.
  assert( A.size() == n * n );   // The matrix A must be square.
  int m = n;

  #ifndef NDEBUG //===============[Begin: debug only code]========

  // Create a scale matrix.
  valarray<double> scale( A );
  for (int i = 0; i < n; i++)
  { 
      for(int j = 0; j < n; j++)
      {
            scale[i + j * n] = 10.0 * (fabs( scale[i + j * n] +  A[j + i * n] )) ;
      }
  }
  
  // Verify that A is symmetric up to epsilon by checking the
  // equality of it and its transpose.
  assert( isDmatEpsEqual( DoubleMatrix( A, n ), DoubleMatrix( transpose( A, n ), n ), DoubleMatrix( scale, n ) ) );
  
  #endif // ======================[End:   debug only code]========

  assert( pdB != 0 );
  assert( plC != 0 );


  //------------------------------------------------------------
  // Define the parameters for the function clapack_dgetrf(), 
  // which computes a Cholesky factorization of a real symmetric
  // positive-definite matrix, and evaluates its determinant.
  // Note: this function (det) only makes use of the 
  // determinant value and not the LU factorization.
  //------------------------------------------------------------
  
  // Parameter: n.
  // Input: n, the order of the matrix A. 
  // Output: unspecified.
  // Constraint: n >= 1. 
  assert( n >= 1 );

  // Parameter: lda.
  // Input: the last dimension of the array a as declared in the
  // function from which dgetrf is called.
  // Output: unspecified.
  // Constraint: lda >= n.
  int lda = n;

  // Parameter: a.
  // Input: the upper triangle of the n by n positive-definite
  // symmetric matrix A.  The elements of the array below the
  // diagonal need not be set.
  // Output: the L (lower) triangle without diagonal elements which are unity
  //         and the U (upper) triangle
  double a[n*n];
  copy( &(A[0]), &(A[m*n]), a );

  // Parameter: detf.
  // Parameter: dete.
  // Input: unspecified.
  // Output: the determinant of A is given by detf * 2.0^dete.
  // It is given in this form to avoid overflow or underflow.
  double detf;
  int    dete;

  //------------------------------------------------------------
  // Perform the Cholesky factorization 
  // (using ATLAS implementation of DPOTRF())
  //------------------------------------------------------------
  //
  // DPOTRF() computes the Cholesky factorization of a real 
  // symmetric/Hermitian positive definite matrix A such that
  // The factorization has the form A = U^H * U, if uplo
  // is CblasUpper, or A = L*L^H, if uplo is CblasLower,
  // where U is an upper triangular matrix and L is lower triangular.
  // This factorization is sometimes referred to as
  // "taking the square root of" the matrix.  Thus, one
  // should immediately realize that the resulting L
  // contains the square root of the actual values.

  enum CBLAS_UPLO  uplo  = CblasLower;
  enum CBLAS_ORDER order = CblasColMajor;

  int ipiv[n];

  int info = clapack_dpotrf( order, uplo, n, a, lda );
  if( info )
  {
    if( info > 0 )
      {
	char mess[SpkError::maxMessageLen()];
        snprintf( mess, SpkError::maxMessageLen(),
                  "The leading minor of order %d is not positive definite, and the factorization \ncould not be completed!",
	         info );
	throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, 
			    mess,
			    __LINE__, __FILE__ );
      }
    else if( info < 0 )
      {
	char mess[SpkError::maxMessageLen()];
	snprintf( mess, SpkError::maxMessageLen(), 
                 "Programming error!  The %s argument to clapack_dgetrf() is illegal.", 
                 intToOrdinalString( -info, ONE_IS_FIRST_INT ).c_str() );
	throw SpkException( SpkError::SPK_UNKNOWN_ERR, 
			    mess,
			    __LINE__, __FILE__ );
      }
  }
  /*
  // trace #of permutations
  int nInterchanges = 0;
  for( int i=0, tmp; i<n-1; i++ )
    {
      if( ipiv[i] != i )
	{
	  tmp = ipiv[i];
	  ipiv[ i ] = ipiv[ tmp ];
	  ipiv[ tmp ] = i;
	  nInterchanges++;
	}
    }
  // #permulations = even --- sign = +1
  //               = odd  --- sign = -1
  //double sign = pow( -1.0, nInterchanges );
  */

  // The determinant of symmetric positive definite matrix
  // is ALWAYS POSITIVE!!!
  double sign = +1.0;

  //                    n
  //                  -----
  //                   | |   
  //  sign * det(A) =  | | L(i,j)^2, i=j
  //
  //                 i=0, j=0
  //
  double det = sign;
  for( int j=0; j<n; j++ )
    {  
      for( int i=0; i<n; i++ )
	{
	  if( i==j )
	    {
	      // To avoid over- or under-flow,
	      // devide (make smaller) the devider
	      // by 2^2.
	      // Note: The devider does not have to
	      // be 2.  It is 2 only because the NAG implementation
	      // of GDETRF() returned the determiant-related info
	      // in this way.
	      det *= a[j*n+i] * a[j*n+i] / 4.0;
	    }
	}
    }
  
  detf = det;
  dete = 2 * n;
  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------
 
  // Set the pieces of the determinant.
  *pdB = detf;
  *plC = dete;

  return;
}
