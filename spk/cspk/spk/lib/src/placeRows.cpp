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
 * File: placeRows.cpp
 *
 *
 * Place the rows in matrix A to specified rows in matrix B.
 * A column vector containing true (1) and false (0) specifies
 * which rows of B to be replaced.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: placeRows
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin placeRows$$
$spell 
$$

$section Place The Rows of A in Specified Rows of B$

$index placeRows$$
$index matrix, place the rows of A in B$$

$table
$bold Prototype:$$   $cend  
$syntax/const DoubleMatrix placeRows(const DoubleMatrix& A, DoubleMatrix& B, const DoubleMatrix& mask)/$$
$tend

See also: $xref/getMulRows//getMulRows/$$, $xref/getMulCols//getMulCols/$$, $xref/placeCols//placeCols/$$
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Place the rows of $italic A$$ in the specified rows of $italic B$$.  $italic mask$$
specifies which rows of $italic B$$ is to be replaced.  The return value of this function
is $italic B$$.

$head Arguments$$

$syntax/

/A/
/$$
is a m by n matrix containing data that is to replace the specified rows of
$italic B$$, where m and n are greater than or equal to zero.

$syntax/

/B/
/$$
is a m+k by n matrix whose specified rows are to be replaced,
where k is greater than or equal to zero.

$syntax/

/mask/
/$$
is a m+k dimensional column vector that specifies which rows of $italic B$$ are to be replaced.
$math%i%$$-th element of $italic mask$$ is true (greater than 0) if 
the row is to be replaced and false (equal to 0) otherwise, such that
the total number of trues is equal to m.


$head Example$$
If you compile, link, and run the following program:
$codep

   #include <iostream>

   #include "DoubleMatrix.h"
   #include "placeRows.h"


   void main()
   {
      using namespace std;

      int m = 2;
      int n = 3;
      int k = 1;

      //
      // Set A to be:
      // 
      //   [ 1 1 1 ]
      //   [ 1 1 1 ]
      //
      DoubleMatrix A(m,n);
      A.fill(1.0);

      //
      // Set B to be:
      //
      //   [ 0 0 0 ]
      //   [ 0 0 0 ]
      //   [ 0 0 0 ]
      //
      DoubleMatrix B(m,n);
      B.fill(0.0);

      DoubleMatrix mask(m,1);
      double* pMask = data();

      //
      // Set the mask so that first and third rows of A are replaced by
      // the corresponding rows of B.
      //
      //     [ 1 ]
      //     [ 0 ]
      //     [ 1 ]
      //
      pMask[0] = 1;
      pMask[1] = 0;
      pMask[2] = 1;

      cout << "B before:" << endl;
      B.print();

      replace(A, B, mask);

      cout << "B after: " << endl;
      B.print();

   }
$$
then it will display the following when it is run:
$codep

    B before:
    [ 0  0  0 ]
    [ 0  0  0 ]
    [ 0  0  0 ]

    B after:
    [ 1  1  1 ]
    [ 0  0  0 ]
    [ 1  1  1 ]
$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * 
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include "DoubleMatrix.h"
#include "placeRows.h"
#include "countTrues.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
const DoubleMatrix placeRows(
               const DoubleMatrix& dmatData,
               DoubleMatrix &dmatTarget,
               const DoubleMatrix &dvecMask
               )
{

  if( dmatTarget.nr() == 0 || dmatTarget.nc() == 0 )
      return dmatTarget;
  if( countTrues(dvecMask) == 0 )
      return dmatTarget;

  assert( dmatTarget.nr() == dvecMask.nr() );
  assert( dmatTarget.nc() == dmatData.nc() );
  assert( dvecMask.nc()   == 1 );
  assert( countTrues(dvecMask) == dmatData.nr() );

  int nrData = dmatData.nr();
  int ncData = dmatData.nc();
  int rows = dmatTarget.nr();
  int cols = dmatTarget.nc();
  double *pdTarget = dmatTarget.data();
  const double *pdMask   = dvecMask.data();
  const double *pdData   = dmatData.data();

  int i, j;
  int cnt = 0;

  for( i=0; i<dvecMask.nr(); i++ ){
      if( pdMask[i] ){
          for( j=0; j<cols; j++ ){
             pdTarget[i+j*rows] = pdData[cnt+j*nrData];
          }
          ++cnt;
      }
  }
  return dmatTarget;
}


