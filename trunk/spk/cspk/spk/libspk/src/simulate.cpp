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

/*************************************************************************
*
*   Function:		simulate
*
*   Description:	Simulates measurements and random effects for a model 
*			given the fixed effects for each subject.				
*
*   Author:		Viet Nguyen
*   Updated by:         Sachiko Honda
*
*   Parameters:		SPK_Model &model
*			const valarray<double> &alp
*			const valarray<int>    &N
*			const valarray<double> &bLow
*			const valarray<double> &bUp
*			valarray<double> &yOut
*			valarray<double> &bAllOut
*			Integer seed
*
*	Return Value:	bool
*
**************************************************************************
*
*	Divided into 4 steps:
*	
*	Step #1:  Fill bAllOut
*	Step #2:  Create simY - an unconcatenated version of yOut
*	Step #3:  Fill simY
*	Step #4:  Concatenate simY in yOut
*
*************************************************************************/
#pragma warning( disable : 4786 )
#include <vector>
#include <ctime>
#include <nag.h>
#include <nagg05.h>
#include "SpkValarray.h"
#include "SpkModel.h"
#include "simulate.h"
#include "randNormal.h"

using namespace std;
using SPK_VA::valarray;
using SPK_VA::slice;

bool simulate( SpkModel &model,
	       const valarray<double> &alp,
	       const valarray<int>    &N,
	       const valarray<double> &bLow,
	       const valarray<double> &bUp,
	       valarray<double>       &yOut,
	       valarray<double>       &bAllOut,
	       int seed )
{		
  // *** Random number seed for NAG routines - Default value is random ***
  g05cbc( static_cast<Integer>( seed ) );
  
  // *** Constants/Iterators ***
  int i, j, k;	      // Iterators
  int nIndividuals = N.size();     // Get the number of subjects
  int nB           = bLow.size();  // Get the number of random effects
  int yOut_length  = N.sum();
   
  // *** Initial Conditions ***
  assert( bUp.size() == nB );
  assert( nIndividuals > 0 );
  assert( nB >= 0 );

  // *** Fixing sizes of output matrices ***						
  bAllOut.resize(nB * nIndividuals);  // Set bAllOut to be the correct size

  //  *** Data accessors ***
	
  //--------------------------------------------------------------------------
  // Step #1:  *** Fill bAllOut ***
  //--------------------------------------------------------------------------
  model.setPopPar(alp);

  valarray<double> D( nB * nB );
  model.indParVariance(D);// D matrix from model

  valarray<double> D_norm( nB );

  for (i = 0, k = 0; i < nIndividuals; i++)  // do this nIndividuals times
    {
      D_norm = randNormal(D, nB);                // call randomizer for each time
           
      for (j = 0; j < nB; j++, k++)          // copy entries from latest call to randomizer
	{
	  assert(bUp[j] > bLow[j]);
	  
	  if (D_norm[j] >= bUp[j])           // value of randNormal[j] is too high
	    bAllOut[k] = bUp[j];
	  else if (D_norm[j] <= bLow[j])     // value of randNormal[j] is too low
	    bAllOut[k] = bLow[j];
	  else
	    bAllOut[k] = D_norm[j];										
	}
    }
  
  //--------------------------------------------------------------------------
  // Step #2:  *** Create simY - an unconcatenated version of yOut ***
  //--------------------------------------------------------------------------
  
  vector< valarray<double> > simY( nIndividuals );  // simY is a vector of nIndividuals matrices
  
  //--------------------------------------------------------------------------
  // Step #3:  *** Fill simY ***
  //--------------------------------------------------------------------------
  
  valarray<double> bi( 0.0, nB );            // same dimension as bLow and bUp
  valarray<double> Ri, fi, ei;               // Create matrices needed to fill simY
  
  for (i = 0, k = 0; i < nIndividuals; i++)  // individuals start at 1, go to nIndividuals
    {					     // k indexes the entire bAllOut matrix
      model.selectIndividual(i);	     // selectIndividual sets i as it is
      fi.resize( N[i] );
      Ri.resize( N[i] * N[i] );
      ei.resize( N[i] );
      /*
      for (j = 0; j < nB; j++,	k++)	     // each column of bi = ith column of bAllOut
	{
	  bi[j] = bAllOut[k];				
	}
      
      model.setIndPar(bi);
      */
      model.setIndPar( bAllOut[ slice( k, nB, 1 ) ] );
      model.dataMean(fi);
      model.dataVariance(Ri);
      ei = randNormal( Ri, N[i] );
      
      simY[i].resize( N[i] );
      simY[i] = fi + ei;	            // simY[i] = yi = fi + ei
      
    }
  
  //--------------------------------------------------------------------------
  // Step #4:  *** Concatenate simY in yOut  *** 
  //--------------------------------------------------------------------------
  
  for (i = 0, k = 0; i < nIndividuals; i++)    // loop through nIndividuals matrices
    {
      for (j = 0; (double)j < N[i]; j++, k++)  // loop through # of measurements
	{
	  yOut[k] = simY[i][j];                // copies the values one by one
	}
    }
  
  return true;  // How will this handle an error? //
}

/*
$begin simulate$$

$section Simulation of Measurements and Random Effects for a given Model$$

$spell
	Model model
    valarray
	bool
	Spk
	Modelwith
	Covariances
	Covariance
	covariance
	sqrt
	ith
	const
	cout
	endl
	Cov
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
	Covariances
	iostream
	nd
	redimensioned
    Ri
$$
$index simulate testing model $$

$table
$bold Prototype:$$ $cend 
$syntax/bool simulate(
		SPK_Model &/model/, 
		const DoubleMatrix &/alp/, 
		const DoubleMatrix &/N/,		
		const DoubleMatrix &/bLow/,
		const DoubleMatrix &/bUp/,
		DoubleMatrix &/yOut/,
		DoubleMatrix &/bAllOut/
		Integer /seed/)/$$
$tend

$fend 35$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

Generates a set of measurements and a set of random effects for the user 
based on $italic model$$ -- a user model which is derived from SPK_Model -- 
given a vector of fixed effects $italic alp$$ and a matrix of measurements 
taken for each subject, $italic N$$. 

$pre

$$

The simulated measurements and random effects are controlled by the parameter
$italic alp$$.  Once the fixed effects for the model are set, $italic simulate$$ 
then generates random effects from the distribution

$pre

	b[i] ~ N(0, D(alp) )

$$

where D(alp) is the model's D function, evaluating on the fixed effects in $italic alp$$.


$pre

$$
The model's f($italic alp$$, b[i]) and R($italic alp$$, b[i]) functions are then
called for each individual $italic i$$ to simulate the measurement means and the
noise in the measurements, respectively.

$pre

$$

The set of data created are placed in the matrix $italic yOut$$.  The random
effects for all subjects are placed in the matrix $italic bAllOut$$.  The input values
of the matrices $italic yOut$$ and $italic bAllOut$$ do not matter, as they
will be changed and any previous information stored in them will be overwritten.
In addition, the variation of the random effects is bounded above by $italic bUp$$ and
bounded below by $italic bLow$$.

$head Return Value$$

Returns a boolean value indicating success $italic (i.e. a set
of data was successfully created)$$ or failure $italic (i.e. there was an 
error in the data generation)$$.

$head Assumptions$$

The same number of random effects act on each subject.

$head Notation$$

If A is a $code DoubleMatrix$$ we use A[i] to denote the ith element of A.  Unless noted
otherwise, the indexing begins at zero.

$head Arguments$$

$syntax/
/model/
/$$
A user implementation of the $code SPK_Model$$ class.

$syntax/

/alp/
/$$
The $code DoubleMatrix$$ $italic alp$$ contains the 
$xref/glossary/Population Notation/fixed effects/$$ for the simulation.  The measurements
and the random effects will be simulated from these fixed effects as described above. 

$syntax/

/N/
/$$
The $code DoubleMatrix$$ $italic N$$ contains the number of measurements
taken for each subject, i.e., N[i] is the number of measurements taken for
the ith subject and indexing begins at 1 instead of zero.  
The number of measurements must be positive.  $italic N$$ 
is assumed to be an $italic M$$ by $italic 1$$ matrix, where $italic M$$ is 
the number of subjects.

$syntax/

/bLow/
/$$
The $code DoubleMatrix$$ $italic bLow$$ contains the lower bounds on the
variation of the random effects for all subjects, i.e., bLow[i] is 
the lower bound on the variation of the ith random effects.  Generally, $italic bLow$$
is negative, as random effects have mean zero.  $italic bLow$$ is 
assumed to be a $italic Q$$ by $italic 1$$ matrix, where $italic Q$$ is the 
number of random effects.

$syntax/

/bUp/
/$$
The $code DoubleMatrix$$ $italic bUp$$ contains the upper bounds on the
variation of the random effects for all subjects, i.e., bUp[i] is 
the upper bound on the variation of the ith random effects.  Generally, $italic bUp$$
is positive, as random effects have mean zero.  $italic bUp$$ is 
assumed to be a $italic Q$$ by $italic 1$$ matrix, where $italic Q$$ is the 
number of random effects.

$syntax/

/yOut/
/$$
Simulated data for each subject is placed in the $code DoubleMatrix$$ 
$italic yOut$$.  $italic yOut$$ is a column vector; the first $italic N[0]$$
elements correspond to the data for the 1st individual, the next $italic N[1]$$
elements correspond to the data for the 2nd individual, etc.

$syntax/

/bAllOut/
/$$
The random effects for all of the subjects is placed in the$code DoubleMatrix$$ 
$italic bAllOut$$.  The input size of $italic bAllOut$$ can be anything; it will
be redimensioned as a $italic Q$$ by $italic M$$ matrix.  Any data that was present in 
$italic bAllOut$$ will be overwritten.  The ith column of $italic bAllOut$$ corresponds 
to the random effects for the ith individual.  If any of the random effects 
are above the upper bound or below the lower bound, they will be set at 
the upper bound or the lower bound, respectively.  

$syntax/

/seed/
/$$
The default value of $italic seed$$ is random.  The user can pass an 
$code Integer$$ if a different starting seed value for the random number 
generators is desired.


$head Example$$

If you compile, link, and run the following program,
$codep

// In the following example, simulate() takes in parameters and calculates
// yOut and bAllOut.
	
#include <iostream>
#include "identity.h"

#include "simulate.h"
#include "randNormal.h"
#include "sampleCovariance.h"
#include "allZero.h"

using std::string;

namespace ppkaoptexample
{
	static DoubleMatrix funF    ( const DoubleMatrix &dvecAlp, 
								  const DoubleMatrix &dvecB );
	static DoubleMatrix funF_alp( const DoubleMatrix &dvecF,   
								  const DoubleMatrix &dvecAlp, 
								  const DoubleMatrix &dvecB );
	static DoubleMatrix funF_b  ( const DoubleMatrix &dvecF, 
								  const DoubleMatrix &dvecAlp,   
								  const DoubleMatrix &dvecB );
	static DoubleMatrix funR    ( const DoubleMatrix &dvecAlp, 
								  const DoubleMatrix &dvecB );
	static DoubleMatrix funR_alp( const DoubleMatrix &dmatR,   
							      const DoubleMatrix &dvecAlp, 
								  const DoubleMatrix &dvecB );
	static DoubleMatrix funR_b  ( const DoubleMatrix &dmatR, 
								  const DoubleMatrix &dvecAlp,   
								  const DoubleMatrix &dvecB );
	static DoubleMatrix funD    ( const DoubleMatrix &dvecAlp );
	static DoubleMatrix funD_alp( const DoubleMatrix &dmatD,   
								  const DoubleMatrix &dvecAlp );
}

class PopModel : public SpkModel
{
    DoubleMatrix _a, _b;
    int _i;
    const int nAlp, nB, nY;

public:
    PopModel( int nAlpIn, int nBIn, int nYIn )
    : nAlp(nAlpIn), nB(nBIn), nY(nYIn)
    {}
    ~PopModel(){}
protected:
    void doSelectIndividual(int i)
    {
        _i = i;
    }
    void doSetPopPar(const valarray<double>& alp)
    {
        _a = DoubleMatrix( alp, 1 );
    }
    void doSetIndPar(const valarray<double>& b)
    {
        _b = DoubleMatrix( b, 1 );
    }
    void doDataMean( valarray<double> & ret ) const 
    {
        using namespace ppkaoptexample;
        ret = funF(_a, _b).toValarray();
    }
    bool doDataMean_popPar( valarray<double> & ret ) const 
    {
        using namespace ppkaoptexample;
        
        doDataMean(ret);
        ret = funF_alp( DoubleMatrix( ret, 1 ), _a, _b).toValarray();
        return !allZero(ret);
    }
    bool doDataMean_indPar( valarray<double> & ret ) const
    {
        using namespace ppkaoptexample;
        doDataMean(ret);
        ret = funF_b( DoubleMatrix( ret, 1 ), _a, _b).toValarray();
        return !allZero(ret);
    }
    void doDataVariance( valarray<double> & ret ) const
    {
        using namespace ppkaoptexample;
        ret = funR(_a, _b).toValarray();
    }
    bool doDataVariance_popPar( valarray<double> & ret ) const
    {
        using namespace ppkaoptexample;
        doDataVariance(ret);
        ret = funR_alp( DoubleMatrix( ret, nY ), _a, _b).toValarray();
        return !allZero(ret);
    }
    bool doDataVariance_indPar( valarray<double> & ret ) const
    {
        using namespace ppkaoptexample;
        doDataVariance(ret);
        ret = funR_b( DoubleMatrix( ret, nB ), _a, _b).toValarray();
        return !allZero(ret);
    }
    void doIndParVariance( valarray<double> & ret ) const
    {
        using namespace ppkaoptexample;
        ret = funD(_a).toValarray();
    }
    bool doIndParVariance_popPar( valarray<double> & ret ) const
    {
        using namespace ppkaoptexample;
        doIndParVariance(ret);
        ret = funD_alp( DoubleMatrix( ret, nB ), _a).toValarray();
        return !allZero(ret);
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

	// Number of measurements.
	int nY = nInd;

	// Measurement values, y.
	DoubleMatrix dvecY( nY, 1 );
	double* pdYData = dvecY.data();

	// Number of measurements for each individual. 
	DoubleMatrix dvecN( nInd, 1 );
	dvecN.fill( (double) 1 );

	// Mean, variance, and standard deviation of eTrue and bTrue.
	double meanETrue = 0.0;
	double varETrue  = 1.0;
	double sdETrue   = sqrt( varETrue );
	double meanBTrue = 0.0;
	double varBTrue  = varBetaTrue;
	double sdBTrue   = sqrt( varBTrue );

	DoubleMatrix dmatBTrue(1, nInd);

	//------------------------------------------------------------
	// Quantities related to the fixed population parameter, alp.
	//------------------------------------------------------------

	int nAlp = 2;

	DoubleMatrix dvecAlpTrue( nAlp, 1 );

	double* pdAlpTrueData = dvecAlpTrue.data();

	// Set the values associated with alp(1).
	pdAlpTrueData[ 0 ] = meanBetaTrue;

	// Set the values associated with alp(2).
	pdAlpTrueData[ 1 ] = varBetaTrue;

	//------------------------------------------------------------
	// Quantities related to the random population parameters, b.
	//------------------------------------------------------------

	int nB = 1;

	DoubleMatrix dvecBLow ( nB, 1 );
	DoubleMatrix dvecBUp  ( nB, 1 );

	dvecBLow .fill( -1.5e+1 );
	dvecBUp  .fill( +1.0e+1 );

	//------------------------------------------------------------
	// Quantities related to the user-provided model.
	//------------------------------------------------------------

	PopModel model( nAlp, nB, nY/nInd );

	//------------------------------------------------------------
	// Simulate measurements for each individual.
	//------------------------------------------------------------
	simulate(model, dvecAlpTrue, dvecN, dvecBLow, dvecBUp, dvecY, dmatBTrue, 1);

	//------------------------------------------------------------
	// Print the results.
	//------------------------------------------------------------

	cout << "yOut:" << endl;
	dvecY.print();
	cout << endl;

	cout << "bAllOut:" << endl;
	dmatBTrue.print();
	cout << endl;

	//
	// The mean of the random effects in this example converges to 0.
	//
	cout << "mean of bAllOut:" << endl;
	Matrix mean = calcMean(dmatBTrue);
	mean.print();
	cout << endl;

	//
	// The covariance of the random effects in this example converges to D(alp).
	//
	cout << "covariance of bAllOut:" << endl;
	Matrix Cov = sampleCovariance(dmatBTrue);
	Cov.print();
	cout << endl;

}

  

//--------------------------------------------------------------
//
// Function: funF
//
//
// Calculates
//
//                 /                 \ 
//     f(alp, b) = |  alp(1) + b(1)  |  .
//                 \                 / 
//
//--------------------------------------------------------------

static DoubleMatrix ppkaoptexample::funF( const DoubleMatrix &dvecAlp, 
                                          const DoubleMatrix &dvecB )
{
	DoubleMatrix dvecF( 1, 1 );

	const double* pdAlpData = dvecAlp.data();
	const double* pdBData   = dvecB  .data();

	dvecF.fill( pdAlpData[ 0 ] + pdBData[ 0 ] );

	return dvecF;
}


//--------------------------------------------------------------
//
// Function: funF_alp
//
//
// Calculates
//
//                     /           \ 
//     f_alp(alp, b) = |  1     0  |  .
//                     \           / 
//
//--------------------------------------------------------------

static DoubleMatrix ppkaoptexample::funF_alp( const DoubleMatrix &dvecF, 
                                              const DoubleMatrix &dvecAlp, 
                                              const DoubleMatrix &dvecB )
{
	DoubleMatrix drowF_alp( 1, 2 );

	double* pdF_alpData = drowF_alp.data();

	pdF_alpData[ 0 ] = 1.0;
	pdF_alpData[ 1 ] = 0.0;

	return drowF_alp;
}


//--------------------------------------------------------------
//
// Function: funF_b
//
//
// Calculates
//
//                   /     \ 
//     f_b(alp, b) = |  1  |  .
//                   \     / 
//
//--------------------------------------------------------------

static DoubleMatrix ppkaoptexample::funF_b( const DoubleMatrix &dvecF, 
                                            const DoubleMatrix &dvecAlp, 
                                            const DoubleMatrix &dvecB )
{
	return identity( 1 );
}


//--------------------------------------------------------------
//
// Function: funR
//
//
// Calculates
//
//                 /     \ 
//     R(alp, b) = |  1  |  .
//                 \     / 
//
//--------------------------------------------------------------

static DoubleMatrix ppkaoptexample::funR( const DoubleMatrix &dvecAlp, 
                                          const DoubleMatrix &dvecB )
{
	return identity( 1 );
}


//--------------------------------------------------------------
//
// Function: funR_alp
//
//
// Calculates
//
//                     /           \ 
//     R_alp(alp, b) = |  0     0  |  .
//                     \           / 
//
//--------------------------------------------------------------

static DoubleMatrix ppkaoptexample::funR_alp( const DoubleMatrix &dmatR,   
                                              const DoubleMatrix &dvecAlp, 
                                              const DoubleMatrix &dvecB )
{
	DoubleMatrix dmatR_alp( 1, 2 );

	dmatR_alp.fill(0.0);

	return dmatR_alp;
}


//--------------------------------------------------------------
//
// Function: funR_b
//
//
// Calculates
//
//                   /     \ 
//     R_b(alp, b) = |  0  |  .
//                   \     / 
//
//--------------------------------------------------------------

static DoubleMatrix ppkaoptexample::funR_b( const DoubleMatrix &dmatR, 
                                            const DoubleMatrix &dvecAlp,   
                                            const DoubleMatrix &dvecB )
{
	DoubleMatrix dmatR_b( 1, 1 );

	dmatR_b.fill(0.0);

	return dmatR_b;
}


//--------------------------------------------------------------
//
// Function: funD
//
//
// Calculates
//
//              /          \ 
//     D(alp) = |  alp(2)  |  .
//              \          / 
//
//--------------------------------------------------------------

static DoubleMatrix ppkaoptexample::funD( const DoubleMatrix &dvecAlp )
{
	DoubleMatrix dmatD( 1, 1 );

	const double* pdAlpData = dvecAlp.data();

	dmatD.fill( pdAlpData[ 1 ] );

	return dmatD;
}


//--------------------------------------------------------------
//
// Function: funD_alp
//
//
// Calculates
//
//                  /           \ 
//     D_alp(alp) = |  0     1  |  .
//                  \           / 
//
//--------------------------------------------------------------

static DoubleMatrix ppkaoptexample::funD_alp( const DoubleMatrix &dmatD,
                                              const DoubleMatrix &dvecAlp )
{
	DoubleMatrix dmatD_alp( 1, 2 );

	double* pdD_alpData = dmatD_alp.data();

	pdD_alpData[ 0 ] = 0.0;
	pdD_alpData[ 1 ] = 1.0;

	return dmatD_alp;
}

$$

the matrices
$math%
yOut:
[ 1.99088 ]
[ -2.83752 ]
[ 3.47354 ]
[ -1.60409 ]
[ -1.90355 ]
[ 0.750448 ]
[ 4.56466 ]
[ 1.50014 ]
[ 0.698962 ]
[ 4.66475 ]

bAllOut:
[ -0.58122 -1.62934 2.00363 -1.63388 -2.42738 -0.869417 2.60903 1.37082 -1.87685 2.80124 ]

mean of bAllOut:
[ -0.0233364 ]

covariance of bAllOut:
[ 4.04166 ]

%$$
will be printed.

$end

*/
