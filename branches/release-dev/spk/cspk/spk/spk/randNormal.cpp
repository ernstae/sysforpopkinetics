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
*		      the specified variance.
*
*       Author: Viet Nguyen
*       Updated by: Sachiko Honda
*
********************************************************************************/

#include <cstdlib>

#include "SpkValarray.h"
#include "multiply.h"
#include "randNormal.h"
#include "isSymmetric.h"
#include "intToOrdinalString.h"

extern "C"{
  #include <atlas/clapack.h>
  #include <atlas/cblas.h>
}

using SPK_VA::valarray;

//============================================================================
// Version of randNormal() that generate random numbers drawn from
// normal distribution with mean zero.
//============================================================================
const valarray<double> randNormal( int n )
{
  //--------------------------------------------------------------------------
  // Preliminary steps
  //--------------------------------------------------------------------------
  const double mean = 0.0;  
  assert (n > 0);

  //--------------------------------------------------------------------------
  // Step #1: Generate random numbers drawn from normal distribution
  //          with mean zero.
  //--------------------------------------------------------------------------
  
  valarray<double> r(n);							
  
  // From Linux manual page for rand()
  // NOTES
  //     The versions of rand() and srand() in the Linux C Library use the  same
  //     random  number  generator as random() and srandom(), so the lower-order
  //     bits should be as random as the higher-order bits.  However,  on  older
  //     rand()  implementations, the lower-order bits are much less random than
  //     the higher-order bits.
  //                                                                             
  //     In Numerical Recipes in C: The Art of Scientific Computing (William  H.
  //     Press, Brian P. Flannery, Saul A. Teukolsky, William T. Vetterling; New
  //     York: Cambridge University Press, 1992 (2nd ed., p. 277)), the  follow-
  //     ing comments are made:
  //            "If  you want to generate a random integer between 1 and 10, you
  //            should always do it by using high-order bits, as in
  //                                                                             
  //                   j=1+(int) (10.0*rand()/(RAND_MAX+1.0));
  //                                                                             
  //            and never by anything resembling
  //                                                                             
  //                   j=1+(rand() % 10);
  //                                                                             
  //            (which uses lower-order bits)."
                                                                               
  for (int i = 0; i < n; i++)
    {
      double rsq, v1, v2;
      // Draw random numbers from normal distribution of mean 0 and stdev 1
      do{
	// Pick two uniform numbers in the square extending from -1 to +1 in each direction.
	// rand()/(RAND_MAX+1.0) normalizes the genrated number.
	// NOTE: Need two to compute the equation of circle.
	v1 = 2.0 * (mean + rand()/(RAND_MAX+1.0)) - 1.0;
	v2 = 2.0 * (mean + rand()/(RAND_MAX+1.0)) - 1.0;
	
	// See if v1 and v2 are in the unit circle
	rsq = v1 * v1 + v2 * v2;
      }while( rsq >= 1.0 || rsq == 0.0 ); // if they are not in the unit circule, try again.

      // Make the Box-Muller transformation to get a normal deviate.
      r[i]   = v1 * sqrt( -2.0*log(rsq)/rsq );
    }
  return r;
}

//============================================================================
// The version of randNormal() that ensures the random numbers vary
// within the specified variance.
//============================================================================
const valarray<double> randNormal( const valarray<double> & V, int n )
{
  //--------------------------------------------------------------------------
  // Preliminary steps
  //--------------------------------------------------------------------------
  assert (V.size() == n * n);
  assert (isSymmetric( V, n ) );

  //--------------------------------------------------------------------------
  // Step #1: Generate random numbers drawn from normal distribution
  //          with mean zero.
  //--------------------------------------------------------------------------
  valarray<double> r(n);
  r = randNormal(n);

  //--------------------------------------------------------------------------
  // Step #2: Compute the Cholesky factor
  //--------------------------------------------------------------------------
  enum CBLAS_ORDER order = CblasColMajor;
  enum CBLAS_UPLO  uplo  = CblasLower;

  // double *a
  //
  // (on entry) The array pointed by *a contains the values of matrix A(n,n) 
  // to be decomposed in the column major order if "order" = CblasColMajor,
  // or in the row major order if "order" = CblasRowMajor.
  //
  // (on exit)  The array pointed by *a contains the L such that A = L*L^T
  // if "uplo" is set to CblasLower, or the U if "uplo" = CblasUpper.
  valarray<double> A( V );
  double *a = &(A[0]);

  // int lda
  //
  // The leading dimension of A.  It is the #of rows if order = CblasColMajor
  // or #of columns if order = CblasRowsMajor.
  int lda  = n;

  int info = clapack_dpotrf( order, uplo, n, a, lda );
  if( info > 0 )
    {
      char mess[SpkError::maxMessageLen()];
      snprintf( mess, SpkError::maxMessageLen(), 
	       "The leading minor of order %d is not positive definite, and the factorization could not be completed!",
	        info );
      throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, 
			  mess,
			  __LINE__, __FILE__ );
    }
  else if( info < 0 )
    {
      char mess[SpkError::maxMessageLen()];
      snprintf( mess, SpkError::maxMessageLen(),
                "Programming error!  The %s argument to clapack_dgetrf() is illegal.", 
                intToOrdinalString( -info, ONE_IS_FIRST_INT ).c_str() );
      throw SpkException( SpkError::SPK_UNKNOWN_ERR, 
			  mess,
			  __LINE__, __FILE__ );
    }

  // Zero out the upper triangle.
  for( int j=0; j<n; j++ )
    {
      for( int i=0; i<j; i++ )
	{
	  A[i+j*n] = 0.0;
	}
    }
  //--------------------------------------------------------------------------
  // Step #3: Weight the random numbers with mean zero with the variance.
  //--------------------------------------------------------------------------
  return multiply( A, n, r, 1 );
}

/*
$begin randNormal$$

$section Creates a vector of random values from a multivariate normal distribution$$

$spell
cstdlib
srand
spk
valarray
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
$syntax/ valarray<double> randNormal( const valarray<double> &/V/, int n, int /seed/)/$$
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
The $code valarray<double>$$ $italic V$$ contains a symmetric and positive-definite
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
        #include <cstdlib> // for srand()
	#include <spk/SpkValarray.h>
	#include "randNormal.h"

	void main()
	{
		
		using SPK_VA::valarray;
                using cout;
                using endl;

		// Generated a seed
		int seed = 1;

		// Start the random number generator with the seed.
		srand(1);					

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
