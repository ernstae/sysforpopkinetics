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
 * File: identity.cpp
 *
 *
 * Overloaded functions that return n by n identity matrix
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: const DoubleMatrix identity( int n )
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin identityNum$$
$spell const int ident
$$

$section Get a n by n identity matrix$$

$index identity,$$
$index matrix, identify$$

$table
$bold Prototype:$$   $cend  
$syntax/const DoubleMatrix identity( int /n/)/$$
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
Returns a $math%n by n%$$ identity matrix, where $math%n > 0%$$.  
For $math%n <=0%$$, the program terminates.

$head Arguments$$

$syntax/

/n/
/$$
The dimension of the identity matrix.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include "DoubleMatrix.h"
    #include "identity.h"

    void main(){
        // get a 3 by 3 identity matrix
        DoubleMatrix ident3 = identity(3);

        // display the matrix
        ident3.print();
    }
$$
then it will display the following when it is run:
$codep

[ 1  0  0 ]
[ 0  1  0 ]
[ 0  0  1 ]

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <cassert>
#include "identity.h"
#include "DoubleMatrix.h"

const DoubleMatrix identity( int n )
{
    assert( n > 0 );
    DoubleMatrix dmatIdentN(n,n);
    double *pdIdentN = dmatIdentN.data();
    int i,j;
    for( j=n-1; j>=0; j-- ){
        for( i=n-1; i>=0; i-- ){
            pdIdentN[i+j*n] = (i==j? 1.0: 0.0);
        }
    }
    return dmatIdentN;
    
}
/*************************************************************************
 *
 * Function: void identity( int n, valarray<double>& I )
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin identityVA$$
$spell 
  const 
  int 
  ident
  valarray
  Spk
  cout
  endl
$$

$section Identity Matrix (valarray version)$$

$index identity,$$
$index matrix, identify (valarray version)$$

$table
$bold Prototype:$$   $cend  
$syntax/void identity( int /n/, SPK_VA::valarray<double>& /I/ )/$$
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
It creates an $math%n%$$ sided identity matrix and returns it
as a column-major ordered vector through $italic I$$.

$head Arguments$$

$syntax/
/n/
/$$
is the size of the identity matrix.

$syntax/

/I/
/$$
will contain $math%n%$$ sized identity matrix as a column-major ordered vector.
It must have $math%n * n%$$ elements space allocated. 


$head Example$$
If you compile, link, and run the following program:
$codep

    #include "SpkValarray.h"
    #include "identity.h"

    void main()
    {
      using SPK_VA::valarray;

      int n = 3;

      // This will hold an n identity matrix as a column major ordered vector.
      valarray<double> I( n * n );

      // Modify I so that it will essentially contains a matrix:
      // [ 1 0 0 ]
      // [ 0 1 0 ]
      // [ 0 0 1 ]
      // in the column major order.
      //
      identity( n, I );
      
      cout << "[ ";
      for( int i=0; i<n * n; i++ )
      {
        cout << I[i] << ", ";
      }
      cout << " ]" << endl;

    }
$$
then it will display the following when it is run:
$codep

[ 1, 0, 0, 0, 1, 0, 0, 0, 1, ]

$$
$end
*/

#include "SpkValarray.h"
using SPK_VA::valarray;
using SPK_VA::slice;

void identity( int n, SPK_VA::valarray<double>& I )
{
  assert( I.size() == n * n );

  I = 0.0;
  I[ slice(0, n, n+1) ] = 1.0;
}
