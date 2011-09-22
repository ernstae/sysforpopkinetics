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
/*
 * matabs.cpp
 *
 * For each i-j-th element x in matrix A, x is |y|
 * such that y is the i-j-th element of matrix B.
 *
 * Author: Sachiko Honda
 */
/*
-----------------------------------------------------------
   Function specification
-----------------------------------------------------------
$begin matabs$$

$spell dmat cout endl int nc nr const iostream matabs namespace
abs std pd$$
$section Get absolute values as another matrix$$

$index matrix, absolute value$$
$index abs$$
$index matabs$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix matabs(const DoubleMatrix &/A/)/$$
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
Given a $math%m%$$ by $math%n%$$ matrix $italic A$$, 
$syntax/matabs(/A/)/$$ returns another  
$math%m%$$ by $math%n%$$ matrix $italic B$$, 
such that $math%(b(i,j) in B) = (|a(i,j)| in A)%$$, 
where $math%m > i \ge 0%$$ and $math%n > j \ge 0%$$.

$head Example$$
If you compile, link and run the following program, 
$codep
#include <iostream>
#include "matabs.h"

void main(){

  using namespace std;

  DoubleMatrix dmatA(3,2), dmatB(3,2);
  double       *pdA = dmatA.data();

  // Setting A to a matrix:
  //    [  1  -4 ]
  //    [ -2   5 ]
  //    [  3  -6 ]
  for( int i=0; i<dmatA.nr()*dmatA.nc(); i++ ){
      pdA[i] = (i+1)*(i%2 == 0.0? 1.0: -1.0);
  }

  dmatB = matabs(dmatA);

  cout << "Original matrix A" << endl;
  dmatA.print();

  cout << "Absolute version of A" << endl;
  dmatB.print();

$$
a following matrix is displayed.

$codep
Original matrix A
  [  1  -4 ]
  [ -2   5 ]
  [  3  -6 ]
Absolute version of A
  [ 1  4 ]
  [ 2  5 ]
  [ 3  6 ]
  
$$
$end
-----------------------------------------------------------
   Function implementation
-----------------------------------------------------------
*/
#include <iostream>
#include <cmath>
#include "matabs.h"
#include "DoubleMatrix.h"

// Returns absolute values
DoubleMatrix matabs( const DoubleMatrix &dmatA ){
    using namespace std;

	int iRows = dmatA.nr(), 
		iCols = dmatA.nc();

	DoubleMatrix dmatB(iRows, iCols);
	const double *pdA = dmatA.data();
	double *pdB = dmatB.data();

	for( int i=iRows*iCols-1; i>=0; i-- ){
        pdB[i] = fabs(pdA[i]);
	}
	return dmatB;
}
