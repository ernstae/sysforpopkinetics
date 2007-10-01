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
// OMHelp at end

/******************************************************************************
*
*	Function:	calcMean
*
*	Description:	Matrix returned is equal to mean of the entries in the Matrix V.
*
*       Author:		Viet Nguyen
*       Updated by:     Sachiko Honda
*
*	Parameters:	const valarray<double> &V, int Q
*	Return Value:	vector 
*
******************************************************************************/

#include "calcMean.h"

using SPK_VA::valarray;
using SPK_VA::slice;

valarray<double> calcMean(const valarray<double> &V, int Q)
{
  assert (Q > 0 && Q != 0);
  
  int i = 0, j = 0;    // iterators
  int M = V.size()/Q;  // number of subjects
  
  assert (M > 0);
  assert (M != 0);

  double sum = 0.0;	
  
  valarray<double> mean(Q);								
  
  for( int i=0; i<Q; i++ )
    mean[i] = (V[ slice(i, M, Q) ]).sum() / M;
  return mean;
}

/*
$begin calcMean$$

$section Calculating the Mean Values of a Set of Data $$

$spell
calculat
iostream
namespace
std
nr
nc
cout
endl
ith
jth
valarray
const
spk
$$

$index calculat testing model mean matrix $$

$table
$bold Prototype:$$ $cend 
$syntax/valarray<double> calcMean( const valarray<double> &V, int Q ) /$$
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

Generates the mean value of each row in a given matrix $italic V$$ and stores 
this value in a  vector.

$pre

$$

The formula for the mean of a single row of a matrix is:

$pre

      M
  1  ---
  -  \   V[i,j]   						
  M  /__			
     j=1

  where M is the number of columns in V and V[i,j] is the ith row 
  and jth column of V.

$$

$pre
$$

$head Return Value$$

The i-th element of the resulting vector corresponds to the
mean of i-th row of matrix $italic V$$.

$head Arguments$$

$syntax/
/V/
/$$
$italic V$$ stores a Q by M matrix in column major order,
where Q and M > 0.

$syntax/
/Q/
/$$
$italic Q$$ is the number of rows in the matrix $italic V$$.


$head Example$$

If you compile, link, and run the following program,
$codep
	
    #include <iostream>
    #include <spk/SpkValarray.h>
    #include <spk/calcMean.h>

	void main()
	{
		using namespace std;

		valarray V(3 * 2), Mean(3);

		// Setting V to a matrix:
		//    [ 1  4 ]
		//    [ 2  5 ]
		//    [ 3  6 ]

		for (int i = 0; i < V.size(); i++ )
		{
			pV[i] = i+1;
		}

		Mean = calcMean(V, 3);

		cout << "Mean of V: " << Mean << endl;
    }

$$

The output: 
$math%
Mean of V: { 2.5, 3.5, 4.5 }
%$$
will be printed.

$end

*/
