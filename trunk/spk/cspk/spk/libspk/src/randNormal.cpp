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

/********************************************************************************
*
*	Function:     randNormal
*
*	Description:  Matrix returned is equal to the matrix product: (L)(indx), 
*		      where L is the Cholesky factor of V and indx is a vector of 
*		      random numbers normally distributed with mean zero and 
*		      variance 1.
*
*       Author: Viet Nguyen
*       Updated by: Sachiko Honda
*
*	Parameters:	const valarray<double> &V, int n
*	Return Value:	vector 
*
********************************************************************************
*
*	Divided into 2 steps:
*
*	Step #1:  Create indx - a vector of random numbers normally distributed
*	Step #2:  Calculate the Cholesky factor of V
*
********************************************************************************/
#include <nag.h>
#include <nagf03.h>  // for Cholesky factoring
#include <nagg05.h>  // random number generator
#include "SpkValarray.h"
#include "multiply.h"
#include "randNormal.h"

using SPK_VA::valarray;

valarray<double> randNormal( const valarray<double> & V, int n )
{
  //--------------------------------------------------------------------------
  // Preliminary steps
  //--------------------------------------------------------------------------
  
  int i = 0, j = 0;// iterators
  
  assert (n > 0);
  assert (V.size() == n * n);

  //--------------------------------------------------------------------------
  // Step #1:  Create indx - a vector of random numbers normally distributed
  //--------------------------------------------------------------------------
  
  double indx[n];							
  
  // Fill indx with random numbers from a normal distribution of mean 0 and stdev 1
  for (i = 0; i < n; i++)
    {
      indx[i] = nag_random_normal(0, 1);
    }
  
  //--------------------------------------------------------------------------
  // Step #2:  Calculate the Cholesky factor of V
  //--------------------------------------------------------------------------
  
  // Need parameters:
  // 
  // int n           = order of matrix V
  // double Vt[]     = the transposed matrix that will be factored
  // int tda         = the last dimension of V
  // double p[]      = an array that stores the reciprocals of the diagonals of the cholesky
  // double detf     = helps in calculating determinant (not used)
  // int dete        = helps in calculating determinant (not used)
  // NagError *fail  = reporting failures
  
  // Parameter #2: non-const version of V which will receive the Cholesky factor.
  double Vt[ n * n ];
  for( int i=0; i<n*n; i++ )
    {
      Vt[i] = V[i];
    }

  // Parameter #3:
  int tda = n;
  assert (tda >= 1);
  
  // Parameter #4:
  double p[ n ];  // Make p an nx1 matrix
  
  // Parameters #5-7
  double  detf;	 // Dummy 
  Integer dete;	 // Dummy
  
  static NagError fail;  // This portion is lifted from inverse.cpp
  INIT_FAIL(fail);
  
  nag_real_cholesky(n, Vt, tda, p, &detf, &dete, &fail);
  
  assert(fail.code == NE_NOERROR);
  
  valarray<double> V_copy( n * n );
  // nag_real_cholesky returns row major format, so transpose it
  for( j = 0; j < n; j++ )
    {
      for( i = 0; i < n; i++ )
	{
	  V_copy[ j + i * n ] = Vt[ i + j * n ];
	}
    }
    
  for (i = 1; i < n; i++)  // format V_copy to be lower triangular
    {
      for (j = 0; j < i; j++)
	{
	  V_copy [i*n + j] = 0.0;  // all upper triangular values become zero
	}
    }
  
  for (int i = 0; i < n; i++)  // copy the diagonal values into V_copy
    {
      V_copy[i + i*n] = 1/p[i];
    }
  
  return (multiply(V_copy, n, valarray<double>( indx, n ), 1));  // returns an n x 1 matrix
}

/*
$begin randNormal$$

$section Creates a vector of random values from a multivariate normal distribution$$

$spell
Cholesky
cbc
const
iostream
namespace
std
cout
endl
$$

$index Cholesky testing random number normal distribution multivariate$$

$table
$bold Prototype:$$ $cend 
$syntax/ valarray<double> randNormal( const valarray<double> &/V/, int n, Integer /seed/)/$$
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

Computes a vector of random values from a multivariate normal distribution
with mean zero and variance $italic V$$.  A seed value for the random number
generation must be set prior to calling this function.  

$head Return Value$$

Returns an $italic n$$ by $italic 1$$ matrix, where $italic n$$ is the 
number of rows of $italic V$$.  

$head Implementation$$

Given a symmetric, positive-definite matrix $italic V$$, computes the 
Cholesky factor of $italic V$$ and an $italic n$$ by $italic 1$$ matrix 
of random numbers from a normal distribution of mean zero and variance 1, 
where $italic n$$ is the number of rows of $italic V$$.  Computes the product 
of the Cholesky factor and the matrix of random numbers and returns that 
product.

$head Arguments$$

$syntax/
/V/
/$$
The $code valarray<double> $italic V$$ contains a symmetric and positive-definite
matrix in the column major order.

$syntax/
/n/
/$$
$italic n$$ is the order of $italic V$$.

$syntax/
/seed/
/$$
The $code Integer$$ (defined in nag.h) $italic seed$$ will be used as a seed to
generate random numbers.

$head Example$$

If you compile, link, and run the following program,
$codep
	
	#include <iostream>
	#include <spk/SpkValarray.h>
	#include <nag.h>
	#include "randNormal.h"

	void main()
	{
		
		using namespace std;

		Integer seed = 1;

		// Required for NAG routines
		g05cbc(seed);					

		valarray<double> V( 2 * 2 ), randNorm( 2 );

		// Setting V to a symmetric, positive definite matrix:
		//    [ 2  1 ]
		//    [ 1  3 ]

		V[0] = 2;
		V[1] = 1;
		V[2] = 1;
		V[3] = 3;

		randNorm = randNormal(V, 2, seed);

		// Cholesky factor is equal to:
		//    [ 1.41421   0       ]
		//    [	0.707107  1.58114 ]

		// Random vector with seed value = 1:
		//    [ -0.25993  ]
		//    [ -0.728662 ]

		cout << "Product of Cholesky and random vector: " << endl;
	        cout << randNorm << endl;
    }

$$
 
$math%
Product of Cheolesky and random vector:  { -0.367596, -1.335914 }

%$$
will be printed.

$end

*/
