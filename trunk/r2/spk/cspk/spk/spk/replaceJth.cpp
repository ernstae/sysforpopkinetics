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
 * File: replaceJth.cpp
 *
 *
 * Replace the jth column of the original matrix given as the first
 * argument with the column vector given as the 3rd argument.
 *
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: replaceJth
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin replaceJth$$
$spell 
iostream namespace std cout endl cerr const th jth bool int
$$

$section Replace the jth column with a column vector$$ 

$index replaceJth$$
$index matrix, replace a column$$

$bold Prototype:$$
$syntax/
void replaceJth(DoubleMatrix &/A/, const int /j/, const DoubleMatrix &b)
/$$


$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Replace data in the j-th column of the original $math%m by n%$$ matrix with
a $math%m%$$ dimensional column vector.  This operation directly modifies the original
matrix's data space.  When dimensions do not match, the system terminates.
$head Arguments$$

$syntax/

/A/
/$$
is a $math%m by n%$$ matrix whose jth column is to be modified.

$syntax/

/j/
/$$
is the column index, greater than or equal to 0 and less than $math%n%$$.

$syntax/

/b/
/$$
is a $math%m%$$ dimensional column vector whose data replaces the jth
column of $italic A$$.  

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "replaceJth.h"

    void replaceJthTest(){
        using namespace std;

        // set A to a 3 by 3 matrix filled with 0
        DoubleMatrix A(3,3);
        A.fill(0);

        // set b to a 3 dimensional column vector
        DoubleMatrix b(3,1);
        b.fill(1);

        cout << "original A" << endl;
        A.print();
        cout << endl;

        // Replace the contents of the 1st column with the elements of b.
        replaceJth( A, 1, b );
        A.print();
        
    }

$$
then it will display the following when it is run:
$codep

  original A
  [ 0  0  0 ]
  [ 0  0  0 ]
  [ 0  0  0 ]

  new A
  [ 0  1  0 ]
  [ 0  1  0 ]
  [ 0  1  0 ]


$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <iostream>
#include "replaceJth.h"
#include "DoubleMatrix.h"
/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void replaceJth( DoubleMatrix &dmatOrg, const int jth, const DoubleMatrix &colvec )
{
    int m = dmatOrg.nr();
    int n = dmatOrg.nc();

    assert( colvec.nr() == m );
    assert( colvec.nc() == 1 );
    
    assert( jth >= 0 && jth < n );

    double *pdOrg  = dmatOrg.data();
    const double *pdColvec = colvec.data();

    for( int i=0; i<m; i++ )
    {
    
        pdOrg[jth*m + i] = pdColvec[i];
    }
}
