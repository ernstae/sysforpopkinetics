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
 * Computes the determinant of a symmetric, positive-definite,
 * double precision matrix.
 *
 * Author: Mitch Watrous
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

$section Determinant of a Matrix$$

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
Evaluates the determinant of the square, symmetric, positive-definite 
matrix $math%A%$$.
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
/dmatA/
/$$
The $code DoubleMatrix$$ $italic dmatA$$ contains the square, symmetric, 
positive-definite matrix $math%A%$$.  If the matrix did not
have these properties, it throws an $xref/SpkException//SpkException/$$.

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

#include <iostream>
#include <cassert>
#include <cmath>

#include "det.h"
#include "transpose.h"
#include "isDmatEpsEqual.h"
#include "nag.h"
#include "nag_types.h"
#include "nag_stdlib.h"
#include "nagf03.h"
#include "DoubleMatrix.h"


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void det( const DoubleMatrix &dmatA, double* pdB, long int* plC )
{
  using namespace std;
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nARow = dmatA.nr();
  int nACol = dmatA.nc();


  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  assert( nARow == nACol );   // The matrix A must be square.
  assert( nARow > 0 );        // A must have at least one row.

  #ifndef NDEBUG //===============[Begin: debug only code]========

  // Create a scale matrix.
  DoubleMatrix dmatScale( dmatA );
  double* pdScaleData = dmatScale.data();
  for (int i = 0; i < nARow; i++)
  { 
      for(int j = 0; j < nACol; j++)
      {
            pdScaleData[i + j * nARow] = 10.0 * (fabs( pdScaleData[i + j * nARow] +  pdScaleData[j + i * nACol] )) ;
      }
  }
  
  // Verify that A is symmetric up to epsilon by checking the
  // equality of it and its transpose.
  assert( isDmatEpsEqual( dmatA, transpose( dmatA ), dmatScale ) );
  
  #endif // ======================[End:   debug only code]========

  assert( pdB != 0 );
  assert( plC != 0 );


  //------------------------------------------------------------
  // Define the parameters for the function nag_real_cholesky (f03aec), 
  // which computes a Cholesky factorization of a real symmetric
  // positive-definite matrix, and evaluates its determinant.
  // Note: this function (det) only makes use of the 
  // determinant value and not the Cholesky factorization.
  //------------------------------------------------------------
  
  // Parameter: n.
  // Input: n, the order of the matrix A. 
  // Output: unspecified.
  // Constraint: n >= 1. 
  Integer n = nARow;
  assert( n >= 1 );

  // Parameter: tda.
  // Input: the last dimension of the array a as declared in the
  // function from which nag_real_cholesky is called.
  // Output: unspecified.
  // Constraint: tda >= n.
  Integer tda = n;

  // Parameter: a.
  // Input: the upper triangle of the n by n positive-definite
  // symmetric matrix A.  The elements of the array below the
  // diagonal need not be set.
  // Output: the sub-diagonal elements of the lower triangular
  // matrix L. The upper triangle of A is unchanged.
  // Note: (1.) Because the data elements of DoubleMatrix are stored 
  // in column-major order and NAG routines expect arrays to be
  // stored in row-major order, the call to nag_real_cholesky 
  // will actually compute det( A^T ), which is equal to det( A ).
  // (2.) A temporary copy of A is created here because dmatA is
  // declared const, but nag_real_cholesky overwrites the lower 
  // triangle of the matrix pointed to by a.
  DoubleMatrix dmatATemp = dmatA;
  double* a = dmatATemp.data();

  // Parameter: p
  // Input: unspecified.
  // Output: the reciprocals of the diagonal elements of L.
  DoubleMatrix dmatLDiag = DoubleMatrix( nARow, 1 );
  double* p = dmatLDiag.data();

  // Parameter: detf.
  // Parameter: dete.
  // Input: unspecified.
  // Output: the determinant of A is given by detf * 2.0^dete.
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
  nag_real_cholesky( n, a, tda, p, &detf, &dete, &fail);
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
 * Computes the determinant of a symmetric, positive-definite,
 * double precision matrix.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: det (valarray version)
 * a derivation of Mitch's DoubleMatrix version of det()
 *
 * Author: Sachiko Honda (based upon Mitch's version)
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
Evaluates the determinant of the square, symmetric, positive-definite 
matrix $math%A%$$.
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
The $code valarray$$ $italic A$$ contains the square, symmetric, 
positive-definite matrix $math%A%$$ in the column major order.  If the matrix did not
have these properties, it throws an $xref/SpkException//SpkException/$$.
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
  DoubleMatrix( A, nACol ).print();
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
  // Preliminaries.
  //------------------------------------------------------------

  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  assert( n > 0 );               // A must have at least one row.
  assert( A.size() == n * n );   // The matrix A must be square.

  #ifndef NDEBUG //===============[Begin: debug only code]========

  // Create a scale matrix.
  valarray<double> scale = A;
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
  // Define the parameters for the function nag_real_cholesky (f03aec), 
  // which computes a Cholesky factorization of a real symmetric
  // positive-definite matrix, and evaluates its determinant.
  // Note: this function (det) only makes use of the 
  // determinant value and not the Cholesky factorization.
  //------------------------------------------------------------
  
  // Parameter: nag_n.
  // Input: nag_n, the order of the matrix A. 
  // Output: unspecified.
  // Constraint: nag_n >= 1. 
  Integer nag_n = n;
  assert( nag_n >= 1 );

  // Parameter: tda.
  // Input: the last dimension of the array a as declared in the
  // function from which nag_real_cholesky is called.
  // Output: unspecified.
  // Constraint: tda >= nag_n.
  Integer tda = nag_n;

  // Parameter: a.
  // Input: the upper triangle of the nag_n by nag_n positive-definite
  // symmetric matrix A.  The elements of the array below the
  // diagonal need not be set.
  // Output: the sub-diagonal elements of the lower triangular
  // matrix L. The upper triangle of A is unchanged.
  // Note: (1.) Because the data elements of DoubleMatrix are stored 
  // in column-major order and NAG routines expect arrays to be
  // stored in row-major order, the call to nag_real_cholesky 
  // will actually compute det( A^T ), which is equal to det( A ).
  // (2.) A temporary copy of A is created here because dmatA is
  // declared const, but nag_real_cholesky overwrites the lower 
  // triangle of the matrix pointed to by a.
  valarray<double> a = A;

  // Parameter: p
  // Input: unspecified.
  // Output: the reciprocals of the diagonal elements of L.
  valarray<double> p( n );

  // Parameter: detf.
  // Parameter: dete.
  // Input: unspecified.
  // Output: the determinant of A is given by detf * 2.0^dete.
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
  nag_real_cholesky( nag_n, &a[0], tda, &p[0], &detf, &dete, &fail);
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
  // Finish up.
  //------------------------------------------------------------
 
  // Set the pieces of the determinant.
  *pdB = detf;
  *plC = dete;

  return;
}
