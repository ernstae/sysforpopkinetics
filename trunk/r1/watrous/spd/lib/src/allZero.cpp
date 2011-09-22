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
 * File: allZero.cpp
 *
 *
 * Function that returns true if every single element of a matrix/vector
 * is zero.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: allZero
 *
 *************************************************************************/

/*
$begin allZero$$
$spell 
  inx 
  cout 
  pd 
  max 
  endl 
  dmat 
  int 
  const 
  iostream 
  namespace 
  std 
  bool
  valarray
$$

$section Testing Whether a Matrix is Filled With Zero$$

$index allZero$$
$index matrix, all zero?$$
$index logical operation, all zero?$$

$table
$bold Prototype:$$   $cend  
$syntax/bool allZero(const DoubleMatrix &/A/)/$$ $rend
$syntax/bool allZero(const valarray<double> &/A/)/$$ $rend
$tend

See also: $xref/allTrue//allTrue/$$

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns true if (0.0 - DBL_EPSILON) < A[i,j] < (0.0 + DBL_EPSILON) for every element of A, 
or false otherwise.

$head Arguments$$

$syntax/

&/A/
/$$
is a $math%m by n%$$ arbitrary matrix.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "allTrue.h"

    void main()
    {
        using namespace std;

        //
        // Test the DoubleMatrix version.
        //

        DoubleMatrix dmatA(3,3);
        double *pdA = dmatA.data();

        // set the matrix to:
        // 
        // [0, 3, 2]
        // [1, 0, 3]
        // [2, 1, 0]
        //
        for( int i=0; i<3*3; i++ )
            pdA[i] = i%4;

        cout << "Matrix version:" << endl;
        cout << (allZero(dmatA)? "All zero." : "Some element is non zero.") << endl;


        //
        // Test the valarray version.
        //

        // Create a vector of size 3 filled with all 0.0.
        valarray<double> x( 3, 0.0 );

        cout << "valarray version:" << endl;
        cout << (allZero(x)? "All zero." : "Some element is non zero.") << endl;
    }

$$
then it will display the following when it is run:
$codep

  Matrix version:
  Some element is non zero.

  valarray version:
  All zero.

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include "SpkValarray.h"
#include "DoubleMatrix.h"
#include "allZero.h"
#include <cfloat>

using SPK_VA::valarray;

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

bool allZero(const DoubleMatrix &dmatA)
{

    const int m = dmatA.nr();
    const int n = dmatA.nc();
    const double *pdA = dmatA.data();

    for( int i=0; i<m*n; i++ )
    {
        if( (-DBL_EPSILON >= pdA[i]) || (pdA[i] >= DBL_EPSILON) )
        {
            return false;
        }
    }
    return true;
}

bool allZero(const valarray<double> &x)
{
    const int n = x.size();

    for( int i=0; i<n; i++ )
    {
        if( (-DBL_EPSILON >= x[i]) || (x[i] >= DBL_EPSILON) )
        {
            return false;
        }
    }
    return true;
}

