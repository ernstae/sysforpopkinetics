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
 * getCol.cpp
 * 
 * getCol(DoubleMatrix &A, int col) returns the 
 * specified column in the matrix A as a separate DoubleMatrix.
 * col# begins from 1.
 */
/*
-----------------------------------------------------------
   Function specification
-----------------------------------------------------------
$begin getCol$$

$spell 
    const dmat cout endl int pd th namespace iostream jth const mat
    st std nr nc dvec nd druglab spk getcol cpp
    vec
$$ 

$section Get a single column as a column vector$$

$index matrix, single column copy$$
$index getCol$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix getCol(const DoubleMatrix &/A/, const int /j/)/$$ $rend
$syntax/void getCol(const DoubleMatrix& dmatA, const int j, DoubleMatrix& vecJ)/$$

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
Returns the specified column in a matrix as a separate object.
Given a $math%m by n%$$ matrix, $italic j%$$ must be
greater than or equal to $math%0%$$ and less than $math%n%$$.
If $italic j$$ is out of range, the program terminates.

$syntax/

&/A/
/$$
is the original $math%m by n%$$ matrix.
$syntax/

/j/
/$$
specifies the column that should be returned as a 
separate DoubleMatrix object.  $italic j$$ must be
greater than or equal to $math%0%$$ and
less than $math%n%$$.  
$syntax/

/vecJ/
/$$
is a reference to a DoubleMatrix object which has m by 1 dimensions.  
The jth column of $italic A$$ is placed in this vector object.

$head Example$$
If you compile, link and run the following program, 
$codep
    #include <iostream>
    #include "getCol.h"

    void main(){

      using namespace std;

      DoubleMatrix dmatA(3,2),
                   dvecB;
      double       *pdA    = dmatA.data();

      // Setting A to a matrix:
      //    [ 1  4 ]
      //    [ 2  5 ]
      //    [ 3  6 ]
      for( int i=0; i<dmatA.nr()*dmatA.nc(); i++ ){
          pdA[i] = i+1;
      }

      // This is a good example
      dvecB = getCol(dmatA, 1);

      cout << "Original matrix A" << endl;
      dmatA.print();

      cout << "1st column of B is: " << endl;
      dvecB.print();

    }

$$
the following results are displayed.

$codep
    Original matrix A
    [1, 4]
    [2, 5]
    [3, 6]

    1st column of B is:
    [4]
    [5]
    [6]
$$
$end
-----------------------------------------------------------
   Function implementation
-----------------------------------------------------------
*/


#include <iostream>
#include "getCol.h"
#include "DoubleMatrix.h"
DoubleMatrix getCol( const DoubleMatrix &dmatA, const int col )
{
    int nRows = dmatA.nr();

    DoubleMatrix dvecJth(nRows,1);
    getCol(dmatA,col,dvecJth);
    return dvecJth;
}
void getCol(const DoubleMatrix& dmatA, const int col, DoubleMatrix& dvecJth)
{
    using namespace std;

	int m = dmatA.nr(),
		n = dmatA.nc();

	if( col < 0 || col >= n ){
		cerr << "getCol: the index must be within " << 0 << " <= " << n << endl;
		abort();
	}

	assert(dvecJth.nr() == m);
    assert(dvecJth.nc() == 1);

    double *pdJth = dvecJth.data();
	const double *pdA   = dmatA.data();

    std::copy(pdA+(col*m),pdA+(col*m+m),pdJth);
	//return dvecJth;
}
