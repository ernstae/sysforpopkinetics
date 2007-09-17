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
 *   Updated by:        Sachiko Honda
 *
 *   Parameters:	SPK_Model &model
 *			const valarray<double> &alp
 *			const valarray<int>    &N
 *			const valarray<double> &bLow
 *			const valarray<double> &bUp
 *			valarray<double> &yOut
 *			valarray<double> &bAllOut
 *			Integer seed
 *
 *	Return Value:	void
 *
 *************************************************************************/
/*
  $begin simulatePop$$

  $section Simulation of Measurements and Random Effects for a given Population Model$$

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
  $index simulate population$$

  $table
  $bold Prototype:$$ $cend 
  $syntax/void simulate( SPK_Model              &/model/, 
               const valarray<double> &/alp/, 
               const valarray<int>    &/N/,		
               const valarray<double> &/bLow/,
               const valarray<double> &/bUp/,
               valarray<double>       &/yOut/,
               valarray<double>       &/bAllOut/
               int                    /seed/ )/$$ $rend
  $cend
  $syntax/void simulate( SPK_Model             &/model/, 
               const valarray<double> &/alp/, 
               const valarray<int>    &/N/,		
               const valarray<double> &/bLow/,
               const valarray<double> &/bUp/,
               valarray<double>       &/yOut/,
               valarray<double>       &/bAllOut/ )/$$ $rend
  $tend

  $fend 45$$

  $center
  $italic
  $include shortCopyright.txt$$
  $$
  $$
  $pre
  $$
  $head Description$$

  Generates a set of measurements and a set of random effects for the user 
  based on $italic model$$ -- a user model which is derived from SpkModel -- 
  given a vector of fixed effects $italic alp$$ and a set of measurements 
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

  $head Assumptions$$

  The same number of random effects act on each subject.

  $head Notation$$

  If A is a $code DoubleMatrix$$ we use A[i] to denote the ith element of A.  Unless noted
  otherwise, the indexing begins at zero.

  $head Arguments$$

  $syntax/
  /model/
  /$$
  A user implementation of the $code SpkModel$$ class that is dependent on
  all $italic alp$$, $italic b$$ and $italic i$$.

  $syntax/

  /alp/
  /$$
  The $code valarray<double>$$ $italic alp$$ contains the 
  $xref/glossary/Population Notation/fixed effects/$$ for the simulation.  The measurements
  and the random effects will be simulated from these fixed effects as described above. 

  $syntax/

  /N/
  /$$
  The $code valarray<int>$$ $italic N$$ contains the number of measurements
  taken for each subject, i.e., N[i] is the number of measurements taken for
  the ith subject and indexing begins at 1 instead of zero.  
  The number of measurements must be positive.  $italic N$$ 
  is assumed to be an $italic M$$ by $italic 1$$ matrix, where $italic M$$ is 
  the number of subjects.

  $syntax/

  /bLow/
  /$$
  The $code valarray<double>$$ $italic bLow$$ contains the lower bounds on the
  variation of the random effects for all subjects, i.e., bLow[i] is 
  the lower bound on the variation of the ith random effects.  Generally, $italic bLow$$
  is negative, as random effects have mean zero.  $italic bLow$$ is 
  assumed to be a $italic Q$$ by $italic 1$$ matrix, where $italic Q$$ is the 
  number of random effects.

  $syntax/

  /bUp/
  /$$
  The $code valarray<double>$$ $italic bUp$$ contains the upper bounds on the
  variation of the random effects for all subjects, i.e., bUp[i] is 
  the upper bound on the variation of the ith random effects.  Generally, $italic bUp$$
  is positive, as random effects have mean zero.  $italic bUp$$ is 
  assumed to be a $italic Q$$ by $italic 1$$ matrix, where $italic Q$$ is the 
  number of random effects.

  $syntax/

  /yOut/
  /$$
  Simulated data for each subject is placed in the $code valarray<double>$$ 
  $italic yOut$$.  The input size of $italic yOut$$ must be sized to the total number of measurements 
  prior to the call.  $italic yOut$$ is a column vector; the first $italic N[0]$$
  elements correspond to the data for the 1st individual, the next $italic N[1]$$
  elements correspond to the data for the 2nd individual, etc.

  $syntax/

  /bAllOut/
  /$$
  The random effects for all of the subjects is placed in the$code valarray<double>$$ 
  $italic bAllOut$$.  The input size of $italic bAllOut$$ must be sized to
  the resulting size, which is $italic Q$$ by $italic M$$. Any data that was present in 
  $italic bAllOut$$ will be overwritten.  The ith column of $italic bAllOut$$ corresponds 
  to the random effects for the ith individual.  If any of the random effects 
  are above the upper bound or below the lower bound, they will be set at 
  the upper bound or the lower bound, respectively.  

  $syntax/

  /seed/
  /$$
  (optional) The user can pass an  $code integer$$
  if a different starting seed value for the random number 
  generators is desired.


  $head Example$$

  If you compile, link, and run the following program,
  $codep

  // In the following example, simulate() takes in parameters and calculates
  // yOut and bAllOut.
	
  #include <iostream>
  #include <spk/SpkValarray.h>
  #include <spk/identity.h>

  #include <spk/simulate.h>
  #include <spk/randNormal.h>
  #include <spk/allZero.h>

  using std::string;

  class PopModel : public SpkModel
  {
  valarray<double> _a, _b;
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
  _a = alp;
  }
  void doSetIndPar(const valarray<double>& b)
  {
  _b = b;
  }
  void doDataMean( valarray<double> & fiOut ) const 
  {
  //--------------------------------------------------------------
  //
  // Calculates
  //
  //                 /                 \ 
  //     f(alp, b) = |  alp(1) + b(1)  |  .
  //                 \                 / 
  //
  //--------------------------------------------------------------
  fiOut = _a[0] + _b[0];
  }
  bool doDataMean_popPar( valarray<double> & fi_alpOut ) const 
  {
  //--------------------------------------------------------------
  //
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
  bool doDataMean_indPar( valarray<double> & fi_bOut ) const
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
  void doDataVariance( valarray<double> & RiOut ) const
  {
  //--------------------------------------------------------------
  //
  // Calculates
  //
  //                 /     \ 
  //     R(alp, b) = |  1  |  .
  //                 \     / 
  //
  //--------------------------------------------------------------
  RiOut = 1.0;
  return true;
  }
  bool doDataVariance_popPar( valarray<double> & Ri_alpOut ) const
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
  bool doDataVariance_indPar( valarray<double> & Ri_bOut ) const
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
  void doIndParVariance( valarray<double> & DOut ) const
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
  return (_a[1]!=0? true : false );
  }
  bool doIndParVariance_popPar( valarray<double> & D_alpOut ) const
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
  int nY = nInd * 1;
  
  // Size of alp vector
  int nAlp = 2;

  // size of b vector
  int nB = 1;
  
  // Measurement values, y.
  valarray<double> y( nY );
  
  // Number of measurements for each individual. 
  valarray<int> N( 1, nInd );
  
  // Mean, variance, and standard deviation of eTrue and bTrue.
  double meanETrue = 0.0;
  double varETrue  = 1.0;
  double sdETrue   = sqrt( varETrue );
  double meanBTrue = 0.0;
  double varBTrue  = varBetaTrue;
  double sdBTrue   = sqrt( varBTrue );
  
  valarray<double> bTrue(nInd);
  
  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------
  
  valarray<double> alpTrue( nAlp );
  
  // Set the values associated with alp(1).
  alpTrue[ 0 ] = meanBetaTrue;
  
  // Set the values associated with alp(2).
  alpTrue[ 1 ] = varBetaTrue;
  
  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------
  
  valarray<double> bLow ( -1.5e+1, nB );
  valarray<double> bUp  ( +1.5e+1, nB );
    
  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------
  
  PopModel model( nAlp, nB, nY/nInd );
  
  //------------------------------------------------------------
  // Simulate measurements for each individual.
  //------------------------------------------------------------
  simulate(model, alpTrue, N, bLow, bUp, y, bTrue, 1);
  
  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------
  
  cout << "yOut:" << y << endl;
  cout << endl;
  
  cout << "bAllOut:" << bTrue << endl;
  cout << endl;
  
  //
  // The mean of the random effects in this example converges to 0.
  //
  valarray<double> mean = calcMean(bTrue, nB);
  cout << "mean of bAllOut:" << mean << endl;
  
  //
  // The covariance of the random effects in this example converges to D(alp).
  //
  Matrix Cov = sampleCovariance(bTrue, nInd);
  cout << "covariance of bAllOut:" << Cov <<  endl;
  cout << endl;

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
#pragma warning( disable : 4786 )
#include <vector>
#include <ctime>
#include "SpkValarray.h"
#include "SpkModel.h"
#include "simulate.h"
#include "randNormal.h"
#include "intToOrdinalString.h"

using namespace std;
using SPK_VA::valarray;
using SPK_VA::slice;

void simulate( SpkModel &model,
	       const valarray<double> &alp,
	       const valarray<int>    &N,
	       const valarray<double> &bLow,
	       const valarray<double> &bUp,
	       valarray<double>       &yOut,
	       valarray<double>       &bAllOut,
	       int seed )
{		
  // *** Random number seeding - Default value is random ***
  srand( seed );
  simulate( model,
	    alp,
	    N,
	    bLow,
	    bUp,
	    yOut,
	    bAllOut );
}
void simulate( SpkModel &model,
	       const valarray<double> &alp,
	       const valarray<int>    &N,
	       const valarray<double> &bLow,
	       const valarray<double> &bUp,
	       valarray<double>       &yOut,
	       valarray<double>       &bAllOut )
{
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
	
  //--------------------------------------------------------------------------
  // Step #1:  *** Simulate the random effects, b, and fill bAllOut ***
  //--------------------------------------------------------------------------
  model.setPopPar(alp);

  valarray<double> D( nB * nB );
  try{
    model.indParVariance(D);// D matrix from model
  }
  catch( SpkException& e )
    {
      e.push( SpkError::SPK_UNKNOWN_ERR, "Evaluation of D(alp) failed during data simulation.\n",
	      __LINE__, __FILE__ );
      throw e;
    }
  catch( ... )
    {
      throw SpkError( SpkError::SPK_UNKNOWN_ERR, "Evaluation of D(alp) failed during data simulation.\n",
		      __LINE__, __FILE__ );
    }

  valarray<double> D_norm( nB );

  for (i = 0, k = 0; i < nIndividuals; i++)  // do this nIndividuals times
    {
     
      try{
	D_norm = randNormal(D, nB);                // call randomizer for each time
      }
      catch( SpkException& e )
	{
	  char buf[ SpkError::maxMessageLen() ];
	  snprintf( buf, SpkError::maxMessageLen(),
                    "Failed to simulate the %s individual's random effects.\n", 
		    intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
	  e.push( SpkError::SPK_UNKNOWN_ERR, buf, __LINE__, __FILE__ );
	  throw e;
	}
      catch( ... )
	{
	  char buf[ SpkError::maxMessageLen() ];
	  snprintf( buf, SpkError::maxMessageLen(),
                    "Failed to simulate the %s individual's random effects.\n", 
		    intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
	  throw SpkError( SpkError::SPK_UNKNOWN_ERR, buf, __LINE__, __FILE__ );
	}
           
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
  // Step #3:  *** Simulate y and fill yOut ***
  //--------------------------------------------------------------------------
  
  valarray<double> bi( 0.0, nB );            // same dimension as bLow and bUp
  valarray<double> Ri, fi, ei;               // Create matrices needed to fill simY
  valarray<double> simYi;

  for (i = 0, j = 0, k = 0; i < nIndividuals; j+=N[i++], k+=nB)  // individuals start at 1, go to nIndividuals
    {					     // k indexes the entire bAllOut matrix
      try{
	model.selectIndividual(i);	     // selectIndividual sets i as it is
	fi.resize( N[i] );
	Ri.resize( N[i] * N[i] );
	ei.resize( N[i] );
	//simY[i].resize( N[i] );
        simYi.resize( N[i] );

	// Simulate the data set for i-th individual, given b.
        // Call the version of simulate() which does not reset the seed value
        // so that the data sets between individuals are kept independent.
	simulate( model, N[i], bAllOut[ slice( k, nB, 1 ) ], simYi );

        // Place the data for i-th individual in the output array.
        yOut[ slice( j, N[i], 1 ) ] = simYi; 
     }
      catch( SpkException& e )
	{
	  char buf[ SpkError::maxMessageLen() ];
	  snprintf( buf, SpkError::maxMessageLen(),
                    "Failed to simulate measurements for the %s individual.\n",
		    intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
	  e.push( SpkError::SPK_UNKNOWN_ERR, buf, __LINE__, __FILE__ );
	  throw e;
	}
      catch( ... )
	{
	  char buf[ SpkError::maxMessageLen() ];
	  snprintf( buf, SpkError::maxMessageLen(),
                    "Failed to simulate measurements for the %s individual.\n",
		    intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
	  throw SpkError( SpkError::SPK_UNKNOWN_ERR, buf, __LINE__, __FILE__ );
	} 
    }
  
  return;
}
/*************************************************************************
 *
 *   Function:		simulate
 *
 *   Description:	Simulates measurements for a model 
 *			given the random effects for the subject.				
 *
 *   Author:		Viet Nguyen
 *   Updated by:        Sachiko Honda
 *
 *   Parameters:	SPK_Model &model
 *                      int n
 *			const valarray<double> &b
 *			valarray<double> &yOut
 *			Integer seed
 *
 *	Return Value:	void
 *
 **************************************************************************/
/*
  $begin simulateInd$$

  $section Simulation of Measurements for a given Individual Model$$

  $spell
  Model model
  valarray
  bool
  Spk
  Modelwith
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
  $index simulate population$$

  $table
  $bold Prototype:$$ $cend 
  $syntax/void simulate( SPK_Model              &/model/,
               int                     /n/,
               const valarray<double> &/bIn/,
               valarray<double>       &/yOut/,
               int                     /seed/ )/$$ $rend
  $cend
  $syntax/void simulate( SPK_Model              &/model/,
               int                     /n/,
               const valarray<double> &/bIn/,
               valarray<double>       &/yOut/ )/$$ $rend
  $tend

  $fend 30$$

  $center
  $italic
  $include shortCopyright.txt$$
  $$
  $$
  $pre
  $$
  $head Description$$

  Generates a set of measurements for the individual $italic model$$.

  $pre

  $$

  The simulated measurements are controlled by the parameter
  $italic b$$ and drown from the distribution:
  $pre

  y[i] = f(b) + e
  e ~ N(0, R(b))

  $$
  where R(b) is the model's R function, evaluating on the random effects in $italic b$$.

  The set of data created are placed in the matrix $italic yOut$$.  The input values
  of the matrix $italic yOut$$ does not matter, as it
  will be changed and any previous information stored in it will be overwritten.

  $head Assumptions$$

  $head Notation$$

  If A is a $code DoubleMatrix$$ we use A[i] to denote the ith element of A.  Unless noted
  otherwise, the indexing begins at zero.

  $head Arguments$$

  $syntax/
  /model/
  /$$
  A user implementation of the $code SpkModel$$ class that is dependent on
  $italic b$$.

  $syntax/

  /n/
  /$$
  The number of measurements to be simulated.

  $syntax/

  /bIn/
  /$$
  The $code valarray<double>$$ $italic bLow$$ contains the random effects.

  $syntax/

  /yOut/
  /$$
  Simulated data is placed in the $code valarray<double>$$ 
  $italic yOut$$.  The input size of $italic yOut$$ must be the same as $math%n%$$.

  $syntax/

  /seed/
  /$$
  (optional) The user can pass an  $code integer$$
  if a different starting seed value for the random number 
  generators is desired.


  $head Example$$

  If you compile, link, and run the following program,
  $codep

  // In the following example, simulate() takes in parameters and calculates
  // yOut and bAllOut.
	
  #include <iostream>
  #include <spk/SpkValarray.h>
  #include <spk/identity.h>

  #include <spk/simulate.h>
  #include <spk/randNormal.h>
  #include <spk/allZero.h>

  using std::string;
  using SPK_VA::valarray;
  using namespace std;

  class SimulateExampleIndModel : public SpkModel
  {
    valarray<double>  _b;
    const int nB, nY;

    public:
    SimulateExampleIndModel( int nBIn, int nYIn )
    : nB(nBIn), nY(nYIn), _b(nBIn)
    {}
    ~SimulateExampleIndModel(){}
    protected:
    void doSetIndPar(const valarray<double>& b)
    {
      _b = b;
    }
    void doDataMean( valarray<double> & fOut ) const 
    {
      //--------------------------------------------------------------
      //
      // Calculates
      //
      //            /       \ 
      //     f(b) = |  b(1)  |  .
      //            \       / 
      //
      //--------------------------------------------------------------
      fOut = _b[0];
    }

    bool doDataMean_indPar( valarray<double> & f_bOut ) const
    {
      //--------------------------------------------------------------
      //
      // Calculates
      // 
      //              /     \ 
      //     f_b(b) = |  1  |  .
      //              \     / 
      //
      //--------------------------------------------------------------
      f_bOut = 1.0;
      return true;
    }
    void doDataVariance( valarray<double> & ROut ) const
    {
      //--------------------------------------------------------------
      //
      // Calculates
      //
      //            /                                \ 
      //     R(b) = |  1  0  0  0  0  0  0  0  0  0  |
      //            |  0  1  0  0  0  0  0  0  0  0  |
      //            |  0  0  1  0  0  0  0  0  0  0  |
      //            |  0  0  0  1  0  0  0  0  0  0  |
      //            |  0  0  0  0  1  0  0  0  0  0  |
      //            |  0  0  0  0  0  1  0  0  0  0  |
      //            |  0  0  0  0  0  0  1  0  0  0  |
      //            |  0  0  0  0  0  0  0  1  0  0  |
      //            |  0  0  0  0  0  0  0  0  1  0  |
      //            |  0  0  0  0  0  0  0  0  0  1  |
      //            \                                / 
      //
      //--------------------------------------------------------------
      ROut = 0.0;
      ROut[ slice( 0, nY, nY+1 ) ] = 1.0;
    }
    bool doDataVariance_indPar( valarray<double> & R_bOut ) const
    {
      //--------------------------------------------------------------
      //
      // Calculates
      //
      //              /     \ 
      //     R_b(b) = |  0  |  .
      //              \     / 
      //
      //--------------------------------------------------------------
      R_bOut = 0.0;
      return false;
    }
  };

  //--------------------------------------------------------------
  //
  // Function: main
  //
  //--------------------------------------------------------------

  int main()
  {
    //------------------------------------------------------------
    // Quantities related to the data vector, y.
    //------------------------------------------------------------
  
    // Number of measurements.
    int nY = 10;
  
    // size of b vector
    int nB = 1;
  
    // Measurement values, y.
    valarray<double> y( nY );

    // Seed
    int seed = 3;
    
    //------------------------------------------------------------
    // Quantities related to the random population parameters, b.
    //------------------------------------------------------------
  
    valarray<double> b ( 0.0, nB );
    
    //------------------------------------------------------------
    // Quantities related to the user-provided model.
    //------------------------------------------------------------
  
    SimulateExampleIndModel model( nB, nY );
  
    //------------------------------------------------------------
    // Simulate measurements for each individual.
    //------------------------------------------------------------
  
    simulate(model, nY, b, y, 1);

    //------------------------------------------------------------
    // Print the results.
    //------------------------------------------------------------
  
    cout << "yOut:" << y << endl;
    cout << endl;
  }

  $$

  the matrices
  $math%
  yOut:{ 1.11227, 0.608056, -0.712082, -1.71895, -0.400054, -2.27172, 0.866331, -1.03258, -0.358203, -1.11381 }

  %$$
  will be printed.

  $end

*/
void simulate( SpkModel               &indModel,
	       int                     n,
	       const valarray<double> &b,
	       valarray<double>       &yOut,
	       int seed )
{		
  // *** Random number seeding - Default value is random ***
  srand( seed );

  // *** generate a data set based upon the seed value.
  simulate( indModel, n, b, yOut );
  return;
}

// This version of simulate() assumes that a seed has been already set.
void simulate( SpkModel               &indModel,
               int                     n,
               const valarray<double> &b,
               valarray<double>       &yOut )
{
  // *** Constants/Iterators ***
  int nB = b.size();    // Get the number of random effects
   
  // *** Initial Conditions ***
  assert( nB >= 0 );
  
  //--------------------------------------------------------------------------
  // Step #2:  *** Size yOut  ***
  //--------------------------------------------------------------------------
  
  yOut.resize( n );
  
  //--------------------------------------------------------------------------
  // Step #3:  *** Fill yOut ***
  //--------------------------------------------------------------------------
  
  valarray<double> R( n*n ), f(n), e(n);  // Create matrices needed to fill yOut
  
  try{
    indModel.setIndPar( b );
    indModel.dataMean(f);
    indModel.dataVariance(R);  
    e = randNormal( R, n );
  
    //---------------------------------------------------
    // REVISIT - Sachiko - 07/16/2004
    // Bound the values of e?
    //---------------------------------------------------
    yOut = f + e;	            // simY = y = f + e
  }
  catch( SpkException& e )
    {
      char buf[ SpkError::maxMessageLen() ];
      snprintf( buf, SpkError::maxMessageLen(), "Failed to simulate measurements.\n" );
      e.push( SpkError::SPK_UNKNOWN_ERR, buf, __LINE__, __FILE__ );
      throw e;
    }
  catch( ... )
    {
      char buf[ SpkError::maxMessageLen() ];
      snprintf( buf, SpkError::maxMessageLen(), "Failed to simulate measurements.\n" );
      throw SpkError( SpkError::SPK_UNKNOWN_ERR, buf, __LINE__, __FILE__ );
    } 
 
  return;
}
