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
*	Function:       residuals
*
*	Description:	vector returned is equal to the residuals of a certain set 
*			of data y.
*
*       Author:		Viet Nguyen
*       Updated by:     Sachiko Honda
*
*
*	Parameters:	SpkModel &model
*			const valarray<double> &alp
*			const valarray<double> &y
*			const valarray<double> &bAll
*
*	Return Value:	vector
*
******************************************************************************/
#include "residuals.h"
#include "SpkValarray.h"
#include "SpkModel.h"

using SPK_VA::valarray;

valarray<double> residuals(SpkModel &model,
			   int   nSubjects,
			   const valarray<double> &alp,
			   const valarray<double> &y,
			   const valarray<int>    &N,
			   const valarray<double> &bAll)
{
  int i, j, k, l, m, n;	            // iterators
  int Q = bAll.size() / nSubjects;  // the number of random effects
  int M = nSubjects;                // the number of subjects
  int y_length = y.size();
  
  valarray<double> errors = y;      // same dimension as y
  valarray<double> bi(Q), fi;
  
  model.setPopPar( alp );
  
  for (i = 0, k = 0, l = 0; i < M; i++)  // individuals start at 1, go to M
    {
      model.selectIndividual(i);         // selectIndividual sets i as it is
      fi.resize( N[i] );

      for (j = 0; j < Q; j++, k++)       // each column of bi = ith column of bAll
	{
	  bi[j] = bAll[k];				
	}
      
      model.setIndPar(bi);
      
      model.dataMean(fi);
            
      for (m = 0; m < N[i]; m++, l++)       // fill errors with fi values
	errors[l] = fi[m];
      
    }
  
  for (i = 0; i < y_length; i++)
    errors[i] = y[i] - errors[i];
  
  return errors;
}

/*
$begin residuals$$

$section Generates residuals $$

$spell
	Model model
const
std
namespace
dmat
dvec
pd
ppkaoptexample
Ind
pdmat
var
drow
doSetPopPar
doSetIndPar
fi
ith
Covariances
iostream
nonnegative
bool
Ri
Spk
$$

$index residual testing data model measurement $$

$table
$bold Prototype:$$ $cend 
$syntax/valarray<double> residuals(SpkModel &/model/,
                                   int   m,
                                   const valarray<double> &/alp/,
				   const valarray<double> &/y/,
				   const valarray<int>    &/N/,
				   const valarray<double> &/bAll/))/$$
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

Generates the set of residuals for a set of measurements $italic y$$ 
based on $italic model$$ -- a user model which is derived from SpkModel -- given
a vector of fixed effects $italic alp$$ and a set of random effects $italic bAll$$.

$pre

$$
$italic residuals$$ returns a vector of errors e, where

$pre
	
	e[i] = y[i] - f(alp, bAll[i])

and y[i] is the measurement data for the ith individual. 

$$



$head Return Value$$

Returns a $italic n$$ by $italic 1$$ matrix, where $italic n$$ is the number of
rows in the matrix $italic y$$.  

$head Arguments$$

$syntax/
/model/
/$$
A user implementation of the $code SpkModel$$ class that is dependent
on all $italic i$$, $italic alp$$ and $italic b$$, where $italic i$$
is the index to an individual in the population.

$syntax/

/m/
/$$
The $italic m specifies the number of subjects/individuals
in the population.

$syntax/

/alp/
/$$
The $code valarray<double>$$ $italic alp$$ contains the 
$xref/glossary/Population Notation/fixed effects/$$ for 
each subject.  All of the fixed effects must be nonnegative.

$syntax/

/y/
/$$
The $code valarray<double>$$ $italic y$$ contains the measurement data
for each subject in the model.  y: R^(the total number of measurements
in the population)

$syntax/

/N/
/$$
The $code valarray<int>$$ $italic N$$ contains the number of
measurements for each individual.  N: I^m.
$syntax/

/bAll/
/$$
The $code valarray<double>$$ $italic bAll$$ contains the random effects
for each subject in the model.  $italic bAll[ j + i * n ]$$ corresponds
to i-th individual's j-th data, where n is the size of $italic b$$ vector.
bAll: R^(n*m)

$head Example$$

If you compile, link, and run the following program,
$codep

#include <iostream>
#include "identity.h"
#include "residuals.h"
#include "printInMatrix.h"

using std::string;

class PopModel : public SpkModel
{
    valarray<double> _a, _b;
    int _i;

public:
    PopModel( int nAlp, int nB )
    {
       _a.resize( nAlp );
       _b.resize( nB );
    }
    ~PopModel(){}
protected:
    void doSelectIndividual(int i)
    {
        _i = i;
    }
    void doSetPopPar(const valarray<double>& alp)
    {
        _a = alp;
    }
    void doSetIndPar(const valarray<double>& b)
    {
        _b = b;
    }
    void doDataMean( valaaray<double>& fiOut ) const 
    {
        //--------------------------------------------------------------
	// Calculates
	//
	//                 /                 \ 
	//     f(alp, b) = |  alp(1) + b(1)  |  .
	//                 \                 / 
	//
	//--------------------------------------------------------------
	fiOut = _a[0] + _b[0];
    }
    bool doDataMean_popPar( valarray<double>& fi_alpOut ) const 
    {
        //--------------------------------------------------------------
	// Calculates
	//
	//                     /           \ 
	//     f_alp(alp, b) = |  1     0  |  .
	//                     \           / 
	//
	//--------------------------------------------------------------
        fi_alpOut[0] = 1.0;
	fi_alpOut[1] = 0.0;
	return true;
    }
    bool doDataMean_indPar( valarray<double>& fi_bOut ) const
    {
        //--------------------------------------------------------------
	//
	// Calculates
	//
	//                   /     \ 
	//     f_b(alp, b) = |  1  |  .
	//                   \     / 
	//
	//--------------------------------------------------------------
	fi_bOut = 1.0;
	return true;
    }
    void doDataVariance( valarray<double>& RiOut ) const
    {
        //--------------------------------------------------------------
	// 
	//  Calculates
	//
	//                 /     \ 
	//     R(alp, b) = |  1  |  .
	//                 \     / 
	//
	//--------------------------------------------------------------
	RiOut = 1.0;
    }
    bool doDataVariance_popPar( valarray<double>& Ri_alpOut ) const
    {
        //--------------------------------------------------------------
	//
	// Calculates
	//
	//                     /           \ 
	//     R_alp(alp, b) = |  0     0  |  .
	//                     \           / 
	//
	//--------------------------------------------------------------
	Ri_alpOut = 0.0;
	return false;
    }
    bool doDataVariance_indPar( valarray<double>& Ri_bOut ) const
    {
        //--------------------------------------------------------------
	//
	// Calculates
	//
	//                   /     \ 
	//     R_b(alp, b) = |  0  |  .
	//                   \     / 
	//
	//--------------------------------------------------------------
	Ri_bOut = 0.0;
	return false;
    }
    void doIndParVariance( valarray<double>& DOut ) const
    {
        //--------------------------------------------------------------
	//
	// Calculates
	//
	//              /          \ 
	//     D(alp) = |  alp(2)  |  .
	//              \          / 
	//
	//--------------------------------------------------------------
	DOut = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& D_alpOut ) const
    {
        //--------------------------------------------------------------
	//
	// Calculates
	//
	//                  /           \ 
	//     D_alp(alp) = |  0     1  |  .
	//                  \           / 
	//
	//--------------------------------------------------------------
	D_alpOut[0] = 0.0;
	D_alpOut[1] = 1.0;
	return true;
    }
};

//--------------------------------------------------------------
//
// Function: main
//
//--------------------------------------------------------------

void main()
{
	//------------------------------------------------------------
	// Preliminaries.
	//------------------------------------------------------------

	using namespace std;
	int nAlp = 2;
	int nB   = 1;

	//------------------------------------------------------------
	// Quantities related to the user-provided model.
	//------------------------------------------------------------

	PopModel model( nAlp, nB );

	//------------------------------------------------------------
	// Quantities that define the problem.
	//------------------------------------------------------------

	 // Mean and variance of the true transfer rate, betaTrue.
	double meanBetaTrue = 1.0;
	double varBetaTrue  = 5.0;

	// Number of individuals.
	int nInd = 10;


	//------------------------------------------------------------
	// Quantities related to the data vector, y.
	//------------------------------------------------------------

	// Measurement values, y.
	valarray<double> y( nInd );

	// Number of measurements for each individual. 
	valarray<int>    N( 1, nInd );

	valarray<double> bTrue(nB * nInd);

	//------------------------------------------------------------
	// Quantities related to the fixed population parameter, alp.
	//------------------------------------------------------------

	valarray<double> alpTrue( nAlp );

	// Set the values associated with alp(1).
	alpTrue[ 0 ] = meanBetaTrue;

	// Set the values associated with alp(2).
	alpTrue[ 1 ] = varBetaTrue;

	//------------------------------------------------------------
	// Setting values for y and bAll.
	//------------------------------------------------------------

	// Setting y to the matrix:
	//
	//		[ 0 ]
	//		[ 1 ]
	//		[ 2 ]
	//		[ 3 ]
	//		[ 4 ]
	//		[ 5 ]
	//		[ 6 ]
	//		[ 7 ]
	//		[ 8 ]
	//		[ 9 ]
	//

	for (int i = 0; i < nInd; i++)
	{
		y[i] = i;
	}

	// Setting bAll to the matrix:
	//
	//		[ 0 0 0 0 0 0 0 0 0 0 ]
	//

	for (i = 0; i < nInd; i++)
	{
		bTrue[i] = 0;
	}


	//------------------------------------------------------------
	//
	// Residuals, or e, should equal: 
	//
	//		[ -1 ]
	//		[ 0 ]
	//		[ 1 ]
	//		[ 2 ]
	//		[ 3 ]
	//		[ 4 ]
	//		[ 5 ]
	//		[ 6 ]
	//		[ 7 ]
	//		[ 8 ]
	//	
	// since in this example:
	//
	//	e[i] = y[i] - f(alp, b[i])
	//		 = y[i] - alp[0] - b[0]
	//		 = y[i] - 1 - 0
	//		 = y[i] - 1
	//
	//
	//------------------------------------------------------------

	valarray<double> residualOut = residuals(model, nInd, alpTrue, y, N, bTrue);

	//------------------------------------------------------------
	// Print the results.
	//------------------------------------------------------------

	printInMatrix( residualOut, 1 );

}

$$

the vector 
$math%
	[ -1 ]
	[ 0 ]
	[ 1 ]
	[ 2 ]
	[ 3 ]
	[ 4 ]
	[ 5 ]
	[ 6 ]
	[ 7 ]
	[ 8 ]
%$$
will be printed.

$end

*/
