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
 * File: inverse.cpp
 *
 *
 * Computes the inverse of a square, symmetric, positive-definite,
 * double precision matrix.
 *
 * Revisit-Mitch: We may want to consider moving this function to 
 * DoubleMatrix class as a member function.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 *     DoubleMatrix version of inverse() 
 *
 *************************************************************************/

/*
$begin inverse$$
$spell dmat const iostream iomanip namespace std int pdbl cout 
        setiosflags ios setprecision endl spk
$$

$section Inverse of a matrix stored as a DoubleMatrix object$$

$index inverseDoubleMatrix$$
$index matrix, inverse --- DoubleMatrix version$$

$table
$bold Prototype$$   $cend  
$syntax/const DoubleMatrix inverse(const DoubleMatrix& /dmatA/)/$$   $rend 
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
Returns the matrix inverse of the square, symmetric, positive-definite 
$code DoubleMatrix$$ $italic dmatA$$. The return value is of type 
$code DoubleMatrix$$, and it has the same dimensions as $italic dmatA$$.

$head Arguments$$
$syntax/
/dmatA/
/$$
The $code DoubleMatrix$$ $italic dmatA$$ contains the square, symmetric, 
positive-definite matrix $math%A%$$.  If the matrix did not
have these properties, it throws an $xref/SpkException//SpkException/$$.

$head Example$$
If you compile and link the C++ program,
$codep

#include <iostream>
#include <iomanip>
#include "DoubleMatrix.h"
#include "inverse.h"

void main()
{
  using namespace std;

  int nARow = 2;
  int nACol= nARow;
  DoubleMatrix dmatA(nARow, nACol);
  double* pdblAData = dmatA.data();
  pdblAData[0 + 0 * nARow] = 1000000.0;
  pdblAData[0 + 1 * nARow] =      -1.0;
  pdblAData[1 + 0 * nARow] =      -1.0;
  pdblAData[1 + 1 * nARow] =       2.0;
  
  cout << setiosflags(ios::scientific) << setprecision(15);

  cout << "A = " << endl;
  dmatA.print();
  cout << endl;

  cout << "inverse(A) = " << endl;
  inverse(dmatA).print();
  cout << endl;
}

$$
then it should output the following when it is run:
$codep

A =
[1.000000000000000e+006, -1.000000000000000e+000]
[-1.000000000000000e+000, 2.000000000000000e+000]

inverse(A) =
[1.000000500000250e-006, 5.000002500001250e-007]
[5.000002500001249e-007, 5.000002500001249e-001]

$$
$end
*/

/*************************************************************************
 *
 *     Function Implementation 
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 *     Standard Header Files
 *------------------------------------------------------------------------*/

#include <cassert>
#include <cmath>


/*------------------------------------------------------------------------
 *     RFPK Header Files
 *------------------------------------------------------------------------*/

#include "inverse.h"
#include "transpose.h"
#include "isDmatEpsEqual.h"
#include "DoubleMatrix.h"

using SPK_VA::valarray;

/*------------------------------------------------------------------------
 *     Local Function Declarations
 *------------------------------------------------------------------------*/

static void INagInvRealSymmPosDefMatrix(const DoubleMatrix& dmatA, 
                                         DoubleMatrix* pdmatAInv);

static DoubleMatrix dmatAInv(__FILE__);
/*------------------------------------------------------------------------
 *     DoubleMatrix version of inverse()
 *------------------------------------------------------------------------*/

const DoubleMatrix inverse(const DoubleMatrix& dmatA) 
{
  int nARow = dmatA.nr();
  int nACol = dmatA.nc();
  assert(nARow == nACol);   // A must be square.

#ifndef NDEBUG //===============[Begin: debug only code]========

  // Create a scale matrix.
  DoubleMatrix dmatScale(dmatA);
  double* pdblScaleData = dmatScale.data();
  for (int i = 0; i < nARow * nACol; i++) {
    pdblScaleData[i] = 2.0 * fabs(pdblScaleData[i]);
  }
  
  // Verify that A is symmetric up to epsilon by checking the
  // equality of it and its transpose.
  if( ! isDmatEpsEqual(dmatA, transpose(dmatA), dmatScale) )
  {
    std::cerr << dmatA << " is not symmetric." << std::endl;
    abort();
  }
  
#endif // ======================[End:   debug only code]========

  dmatAInv.resize(nARow, nACol);

  // Call the interface to the NAG routine that solves a set
  // of real linear equations with multiple right-hand sides
  // in order to determine the inverse.
  INagInvRealSymmPosDefMatrix(dmatA, &dmatAInv);

  return dmatAInv;
}
/*************************************************************************
 *
 *     valarray version of inverse() 
 *
 *************************************************************************/

/*
$begin inverseVA$$
$spell dmat const iostream iomanip namespace std int pdbl cout spk
        setiosflags ios setprecision endl
        valarray
        cols
        spk
$$

$section Inverse of a matrix stored in a vector$$

$index inverseValarray$$
$index matrix, inverse --- valarray version$$

$table
$bold Prototype$$   $cend  
$syntax/const valarray<double> inverse(const valarray<double>& /a/, int nCols)/$$   $rend 
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
Returns the matrix inverse of the square, symmetric and positive-definite 
matrix, $italic a$$, stored in a valarray object in the column order. 

$head Arguments$$
$syntax/
/dmatA/
/$$
The $code valarray$$ $italic a$$ contains the square, symmetric, 
positive-definite matrix $math%A%$$ in the column major order.
If the matrix did not
have these properties, it throws an $xref/SpkException//SpkException/$$.

$head Example$$
If you compile and link the C++ program,
$codep

#include <iostream>
#include <iomanip>
#include "SpkValarray.h"
#include "inverse.h"

void main()
{
  using namespace std;

  int nRows = 2;
  int nCols= nARow;
  valarray<double> a (nRows * nCols);

  a[0 + 0 * nARow] = 1000000.0;
  a[0 + 1 * nARow] =      -1.0;
  a[1 + 0 * nARow] =      -1.0;
  a[1 + 1 * nARow] =       2.0;
  
  cout << setiosflags(ios::scientific) << setprecision(15);

  cout << "a = " << endl;
  DoubleMatrix( a, nRows ).print();
  cout << endl;

  cout << "inverse(a, nCols) = " << endl;
  DoubleMatrix( inverse(a, nCols), nCols ).print();
  cout << endl;
}

$$
then it should output the following when it is run:
$codep

A =
[1.000000000000000e+006, -1.000000000000000e+000]
[-1.000000000000000e+000, 2.000000000000000e+000]

inverse(A) =
[1.000000500000250e-006, 5.000002500001250e-007]
[5.000002500001249e-007, 5.000002500001249e-001]

$$
$end
*/

/*------------------------------------------------------------------------
 *     valarray version of inverse()
 *------------------------------------------------------------------------*/
const valarray<double> inverse( const valarray<double> &a, int nCols )
{
	DoubleMatrix dmatA( a, nCols );
	return inverse( dmatA ).toValarray();
}


/*************************************************************************
 *
 *     Local functions
 *
 *************************************************************************/

/*************************************************************************
 * Function:  INagInvRealSymmPosDefMatrix.
 *
 *
 * Description
 * -----------
 *
 * Interface to the functions distributed by NAG (Numerical Algorithm 
 * Group) that are called by this function in order to invert the real, 
 * symmetric, positive-definite matrix A.
 *
 * The NAG functions called by this interface function are:
 *
 *    nag_real_cholesky (f03aec),
 *    nag_real_cholesky_solve_mult_rhs (f04agc).
 *
 * The function nag_real_cholesky (f03aec) is used here to compute the 
 * Cholesky factorization of the real, symmetric, positive-definite 
 * matrix A.
 *
 * The function nag_real_cholesky_solve_mult_rhs (f04agc) is used here
 * to calculate the approximate solution of a set of real symmetric 
 * positive-definite linear equations with multiple right-hand sides, 
 * 
 *     A X  =  B,
 *   
 * where A has been factorized by nag_real_cholesky (f03aec).  
 * 
 * When this interface function calls nag_real_cholesky_solve_mult_rhs 
 * (f04agc), the value for B is set equal to an identity matrix with the
 * same dimensions as A, Thus, the value computed for X is the inverse 
 * of A.
 *
 * Arguments
 * ---------
 *
 * dmatA        is the DoubleMatrix to be inverted.  It is also referred 
 *              to as A in these specifications.
 *
 * pdmatAInv    is a pointer to the inverse of A.
 *
 *************************************************************************/


/*------------------------------------------------------------------------
 *     NAG Header Files
 *------------------------------------------------------------------------*/

#include "nag.h"
#include "nag_types.h"
#include "nag_stdlib.h"
#include "nagf03.h"
#include "nagf04.h"

#include "identity.h"

static DoubleMatrix dmatATrans(__FILE__);
static DoubleMatrix dmatAInvTrans(__FILE__);
static DoubleMatrix dmatLDiag(__FILE__);
static DoubleMatrix dmatB(__FILE__);
/*------------------------------------------------------------------------
 *     Function Definition
 *------------------------------------------------------------------------*/

static void INagInvRealSymmPosDefMatrix(const DoubleMatrix& dmatA, 
                                         DoubleMatrix* pdmatAInv)
{
  //***********************************************************
  // Preliminaries.
  //***********************************************************

  int nARow = dmatA.nr();
  int nACol = dmatA.nc();
  assert(nARow == nACol);   // A must be square.
  assert(nARow > 0);        // A must have more than one row.

  // Because the data elements of DoubleMatrix are stored in
  // column-major order and NAG routines expect arrays to be
  // stored in row-major order, instantiate a new DoubleMatrix
  // that is the transpose of A.
  transpose(dmatA, dmatATrans);
  double* pdblATransData = dmatATrans.data();

  // Instantiate a DoubleMatrix that will hold the transpose of 
  // the inverse of A.
  dmatAInvTrans.resize(nARow, nACol);
  double* pdblAInvTransData = dmatAInvTrans.data();

  // This column vector will hold the reciprocals of the 
  // diagonal elements of L, the Cholesky factor of A. 
  dmatLDiag.resize(nARow, 1);
  double* pdblLDiag = dmatLDiag.data();

  // Set B equal to an identity matrix with the same dimensions 
  // as A.
  dmatB = identity(nARow);
  double* pdblBData = dmatB.data();
  /*
  DoubleMatrix dmatB(nARow, nACol);
  double* pdblBData = dmatB.data();

  int i, j;
  for (j = 0; j < nACol; j++) {
    for (i = 0; i < nARow; i++) {
      pdblBData[i + j * nARow] = 0.0;
    }
    pdblBData[j + j * nARow] = 1.0;
  }
  */

  //***********************************************************
  // Define the parameters for nag_real_cholesky.
  //
  // Note that the input and output specifications below apply 
  // to the parameters only as they are used with nag_real_cholesky, 
  // i.e. they are not the specifications for the parameters  
  // to this function, INagInvRealSymmPosDefMatrix.
  //***********************************************************

  // Parameter: n.
  // Input: n, the order of the matrix A. 
  // Output: unspecified.
  // Constraint: n >= 1. 
  Integer n = nARow;
  assert(n >= 1);

  // Parameter: a[n][tda].
  // Input: the upper triangle of the n by n positive-definite
  // symmetric matrix A.  The elements of the array below the
  // diagonal need not be set.
  // Output: the sub-diagonal elements of the lower triangular
  // matrix L. The upper triangle of A is unchanged.
  double* a = pdblATransData;

  // Parameter: tda.
  // Input: the last dimension of the array a as declared in the
  // function from which nag_real_cholesky is called.
  // Output: unspecified.
  // Constraint: tda >= n.
  Integer tda = n;

  // Parameter: p[n].
  // Input: unspecified.
  // Output: the reciprocals of the diagonal elements of L.
  double* p = pdblLDiag;

  // Parameter: detf.
  // Parameter: dete.
  // Input: unspecified.
  // Output: the determinant of A is given by detf * 2.0^dete.
  // It is given in this form to avoid overflow or underflow.
  double detf;
  Integer dete;

  //***********************************************************
  // Perform the Cholesky factorization.
  //***********************************************************
 
  // Revisit - Exceptions - Mitch: if an error occurs in this
  // NAG routine, the program will be stopped using exit or abort.
  // Brad: changed to an assert for tracking in debugger 12/12/00
  // Sachiko: Changed to exception throwing 10/15/2002
  static NagError fail;
  INIT_FAIL(fail);
  nag_real_cholesky(n, a, tda, p, &detf, &dete, &fail);
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


  //***********************************************************
  // Define the parameters for nag_real_cholesky_solve_mult_rhs.
  //
  // Note that the input and output specifications below apply 
  // to the parameters only as they are used with 
  // nag_real_cholesky_solve_mult_rhs, i.e. they are not the 
  // specifications for the parameters to this function, 
  // INagInvRealSymmPosDefMatrix.
  //***********************************************************

  // Parameter: n.
  // Input: n, the order of the matrix A. 
  // Output: unspecified.
  // Constraint: n >= 1. 
  assert(n == nARow);
  assert(n >= 1);

  // Parameter: nrhs.
  // Input:r, the number of right-hand sides.
  // Output: unspecified.
  // Constraint: nrhs >= 1.
  Integer nrhs = n;
  assert (nrhs >= 1);

  // Parameter: a[n][tda].
  // Input:the upper triangle of the n by n positive-definite
  // symmetric matrix A, and the sub-diagonal elements of its
  // Cholesky factor L, as returned by nag_real_cholesky
  // (f03aec).
  // Output: unspecified.
  assert(a == pdblATransData);

  // Parameter: tda.
  // Input:the last dimension of the array a as declared in the
  // function  from which nag_real_cholesky_solve_mult_rhs is
  // called.
  // Output: unspecified.
  // Constraint: tda >= n.
  assert(tda == n);

  // Parameter: p[n].
  // Input:the reciprocals of the diagonal elements of L, as
  // returned by nag_real_cholesky (f03aec).
  // Output: unspecified.
  assert(p == pdblLDiag);

  // Parameter: b[n][tdb].
  // Input:the n by r right-hand side matrix B.  
  // Output: unspecified.
  double* b = pdblBData;

  // Parameter: tdb.
  // Input:the last dimension of the array b as declared in the
  // function from which nag_real_cholesky_solve_mult_rhs is
  // called.
  // Output: unspecified.
  // Constraint: tdb >= nrhs.
  Integer tdb = nrhs;

  // Parameter: x[n][tdx].
  // Input: unspecified.
  // Output: the n by r solution matrix X.  
  double* x = pdblAInvTransData;

  // Parameter: tdx.
  // Input:the last dimension of the array x as declared in the
  // function from which nag_real_cholesky_solve_mult_rhs is
  // called.
  // Output: unspecified.
  // Constraint:  tdx >= nrhs.
  Integer tdx = nrhs;

  //***********************************************************
  // Solve the system of equations to determine the inverse.
  //***********************************************************
 
  // Revisit - Exceptions - Mitch: if an error occurs in this
  // NAG routine, the program will be stopped using exit or abort. 
  nag_real_cholesky_solve_mult_rhs(n, nrhs, a, tda, p, b, tdb, x, 
    tdx, NAGERR_DEFAULT);
    
  //***********************************************************
  // Finish up.
  //***********************************************************

  // Transpose the inverse so that it is in column-major order.
   transpose(dmatAInvTrans, *pdmatAInv);
  
  return;
}
