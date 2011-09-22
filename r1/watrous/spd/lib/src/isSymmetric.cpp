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
 * From:   Resource Facility for Population Kinetics
 *         Department of Bioengineering Box 352255
 *         University of Washington
 *         Seattle, WA 98195-2255
 *
 * Copyright (C) 2002, University of Washington,
 * Resource Facility for Population Kinetics. All Rights Reserved.
 *
 * This software was developed with support from NIH grant RR-12609.
 * Please cite this grant in any publication for which this software
 * is used and send a notification to the address given above.
 *
 * Check for updates and notices at:
 * http://www.rfpk.washington.edu
 *
 *************************************************************************/

/*************************************************************************
 *
 * File: isSymmetric.cpp
 *
 *
 * isSymmetric() tests whether a matrix is symmetic up to epsilon.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: isSymmetric
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin isSymmetric$$
$spell 
  valarray
  spk
$$

$section Verify Symmetricity of a Matrix Up To Epsilon$$

$index isSymmetric$$
$cindex matrix, symmetric$$

$table
$bold Prototype:$$   $cend  
$syntax/bool isSymmetric( const DoubleMatrix& /A/ )/$$ $rend
$xyntax/bool isSymmetric( const SPK_VA::valarray<double>& /A/, int /n/ ) $rend

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
Test whether $italic A$$ is symmetric up to an epsilon quality
It returns true if it is determined symmetric; otherwise false.

$head Arguments$$

$syntax/
/A/
/$$
is a $math%n by n%$$ square matrix to be tested.

$syntax/

/n/
specifies the size of $italic A$$ when it is an object of $code valarray$$.
$head Example$$
If you compile, link, and run the following program:
$codep

  #include <iostream>
  #include "DoubleMatrix.h"
  #include "isSymmetric.h"

  int main()
  {
    using namespace std;

    const int n = 3;

    //
    // Create a 3 by 3 matrix initialized to:
    // 
    //  A = [ 1.0  0.0  0.6 ]
    //      [ 0.0  2.0  0.0 ]
    //      [ 0.6  0.0  3.0 ]
    //
    DoubleMatrix A( n * n );
    A[0] = 1.0;
    A[1] = 0.0;
    A[2] = 0.6;
    A[3] = 0.0;
    A[4] = 2.0;
    A[5] = 0.0;
    A[6] = 0.6;
    A[7] = 0.0;
    A[8] = 3.0;

    cout << "A = " << DoubleMatrix( A, n ) << endl;
    cout << "is symmetric? " << (isSymmetric( A, n ) ? "yes" : "no") << endl;
  }

$$
then it will display the following when it is run:
$codep

    A = 3 by 3
    [ 1.0  0.0  0.6 ]
    [ 0.0  2.0  0.0 ]
    [ 0.6  0.0  3.0 ]

    is symmetric? yes

$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * With the current implementation, A=[ 0.0 ] and B=[ DBL_EPSILON - DBL_MIN ]
 * are determined not equal.  Given the definition of DBL_EPSILON,
 * "DBL_EPSILON is the smallest number, x, such that x + c is not equal to c,
 * it 0.0 and (DBL_EPSILON - some number) should be considered the same.
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <cmath>
#include <algorithm>

#include "DoubleMatrix.h"
#include "isDmatEpsEqual.h"
#include "transpose.h"
#include "matabs.h"
#include "mulByScalar.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
bool isSymmetric( const DoubleMatrix& dmatA )
{
  int nA = dmatA.nr();
  assert( dmatA.nc() == nA );   // A must be square.

  // Create a scale matrix.
  // The scale values are in synch with the symmetry test block
  // found in inverse() function.
  DoubleMatrix dmatScale( matabs(mulByScalar(dmatA, 2.0)) );

  // Verify that A is symmetric up to epsilon by checking the
  // equality of it and its transpose.
  return ( isDmatEpsEqual( dmatA, transpose( dmatA ), dmatScale ) );
} 

#include <valarray>
#include "isDblEpsEqual.h"

using SPK_VA::valarray;

bool isSymmetric( const valarray<double>& A, int nColsA )
{
  assert( nColsA * nColsA == A.size() );
  valarray<double> At = transpose(A,nColsA);

  valarray<double> scale( A.apply( fabs ) * 2.0 );
  // Verify that A is symmetric up to epsilon by checking the
  // equality of it and its transpose.
  for( int i=0;i< A.size(); i++ )
  {
    if( !isDblEpsEqual( A[i], At[i], scale[i] ) )
      return false;
  }
  return true;
}
