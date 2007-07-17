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
/**********************************************************************************
 * File: main.cpp
 *
 *
 * DiffEqnModelTest parallel-enabled test driver.
 *
 * Authors: Sachiko Honda, Mitch Watrous
 *
 **********************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
//
// Include the header declaring fitPopulation() and other necessary
// components to do the population analysis.
//
#include <spk/fitPopulation.h>

//
// Include the header declaring SpkException class and associated components.
//
#include <spk/SpkException.h>

//
// Include the header declaring components necessary to run
// this application in parallel mode.
//
#include <spk/ParallelControls.h>

//
// Include header declaring the DiffEqnModel class.
//
#include "DiffEqnModel.h"

//
// Include the hearder declaring a function that performs cholesky factorization.
//
#include <spk/cholesky.h>

//
// Headers required to compile this main.cpp which contains
// program statements validating inputs and results.
//
#include <iostream>
#include <ctime>
#include <cmath>
#include <string>
#include <fstream>
#include <spk/pi.h>
#include "readNonmemTheophylline.h"
#include <spk/printInMatrix.h>
#include <spk/System.h>

using SPK_VA::valarray;

/*------------------------------------------------------------------------
 * Local Function Declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  bool compareSpkToNonmem(  Objective objective, 
                            double objSpk,
                            const valarray<double>& thetaSpk,
                            const valarray<double>& omegaSpk,
                            int nIndPar,
                            const valarray<double>& sigmaSpk,
                            double tolObj,
                            double tolTheta,
                            double tolOmega,
                            double tolSigma );

  double spkToNonmemObj( double lTilde, int nYTotal );

  void usage();

  void copyright();

  bool equalToNSigDigitsTest( double x, 
                              double y, 
                              int digits,
                              std::string xName,
                              std::string yName );

  bool equalToNSigDigitsTest( const valarray<double> &x, 
                              const valarray<double> &y, 
                              int nCols,
                              int digits,
                              std::string xName,
                              std::string yName );

  bool isEqualToNSigDigits( double x, double y, int digits );

  bool isEqualToNSigDigits( const valarray<double> &x, 
                            const valarray<double> &y, 
                            int digits );
} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Local namespace declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  namespace nonmemLaplace
  {
    double nonmemObj();
    const valarray<double> nonmemTheta();
    const valarray<double> nonmemOmega();
    const valarray<double> nonmemSigma();
  }

  namespace nonmemFoce
  {
    double nonmemObj();
    const valarray<double> nonmemTheta();
    const valarray<double> nonmemOmega();
    const valarray<double> nonmemSigma();
  }

  namespace nonmemFo
  {
    double nonmemObj();
    const valarray<double> nonmemTheta();
    const valarray<double> nonmemOmega();
    const valarray<double> nonmemSigma();
  }
} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: main
 *
 *************************************************************************/

int main(int argc, const char* argv[])
{
    using namespace std;

    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    // Display a note.
    copyright();
    cout << endl;
    cout << "Type \"DiffEqnModelTest ?\" for usage details" << endl;
    cout << endl;


    //------------------------------------------------------------
    // Set default values for the command line arguments.
    //------------------------------------------------------------

    // 
    // This test case requires to read a file containing measurement data.  
    // By default it looks for "Theophylline40Simulated.dat" in the current directory.
    //
    char datafile[1024];
    strcpy(datafile, "Simulated_Data_NonZeroTime.dat");

    //
    // The default step size for ODE solver is 0.1.
    // 
    double odestep        = 0.01;
    char   odestepStr[5];
    strcpy(odestepStr, "0.01");

    //
    // A string requesting an objective.
    // The default is modified laplace: MODIFIED_LAPLACE is defined in the namespace, population_analysis.
    //
    enum Objective objective = MODIFIED_LAPLACE;

    //
    // A flag indicating whether user has requested "parallel" or "single" execution.
    // The default is "single" (false).
    //
    bool isMultiple       = false;

    //
    // A flag indicating whether this execution is as the master or node when "parallel" is requested.
    // The default is "master".
    //
    bool isMaster         = true;     // This is default

    //
    // A flag indicating whether user has provided a shared directory for parallel communication.
    // The initial value is false.
    //
    bool isDirSet         = false;

    // 
    // A string specifying a shared directory for parallel communication.
    //
    char c_sharedDirectory[512+1];

    //
    // A string representing a whole sequence of command line arguments that invoke this executable
    // as a node.
    //
    char coNodeCommand[1024+1];


    //------------------------------------------------------------
    // Parse the command line arguments.
    //------------------------------------------------------------

    for( int i=1; i<argc; i++ )
    {
      // 
      // ? --- display the usage and exit.
      //
      if( strcasecmp(argv[i], "?") == 0 )
      {
          usage();
          return 0;
      }

      //
      // -data --- followed by the path and the name of a file containg measurement data.
      // The default is "./Theophylline40Simulated.dat".
      // 
      if( strcasecmp(argv[i], "-data") == 0 )
      {
          i++;
          strcpy(datafile, argv[i]);
      }

      //
      // -ode --- followed by the ODE step size.
      // The default is "0.1".
      //
      if( strcasecmp(argv[i], "-ode") == 0 )
      {
          i++;
          odestep = atof(argv[i]);
          strcpy(odestepStr, argv[i]);
      }

      //
      // -objective --- followed by an objective.
      // The default is "modified_laplace".
      //
      if( strcasecmp(argv[i], "-objective") == 0 )
      {
          i++;
          if( strcasecmp(argv[i], "modified_laplace") == 0 )
              objective = MODIFIED_LAPLACE;
          else if( strcasecmp(argv[i], "expected_hessian") == 0 )
              objective = EXPECTED_HESSIAN;
          else if( strcasecmp(argv[i], "first_order") == 0 )
              objective = FIRST_ORDER;
          else if( strcasecmp(argv[i], "naive_first_order") == 0 )
              objective = NAIVE_FIRST_ORDER;
          else
          {
              cerr << "Invalid objective: " << argv[i] << endl;
              usage();
              return -1;
          }
      }

      //
      // -who --- followed by either "master" or "node".
      // The default is "master".
      // 
      if( strcasecmp( argv[i], "-who" ) == 0 )
      {
         if( argc > i )
         {
            ++i;
            if( strcasecmp( argv[i], "master") == 0 )
            {
               isMaster = true;
            }
            else if( strcasecmp( argv[i], "node") == 0 )
            {
               isMaster = false;
            }
            else
            {
               usage();
               return -1;
            }
            isMultiple = true;
         }
         else
         {
            usage();
            return -1;
         }
      }

      //
      // -dir --- followed by a shared directory path for parallel communication.
      // 
      if( strcasecmp( argv[i], "-dir") == 0 )
      {
         if( argc > i )
         {
            ++i;
            strcpy(c_sharedDirectory, argv[i]);
            File junk(c_sharedDirectory, "junk.tmp");
            ofstream ofs(junk.getFullname().c_str());
            if( ofs.is_open() )
            {
               isDirSet = true;
               ofs.close();
               try{
                 System::del(junk);
               }
               catch(...)
               {
                   cerr << c_sharedDirectory << " is not writable by you." << endl;
                   usage(); 
                   return -1;
               }
            }
            else
            {
               cerr << c_sharedDirectory << " is not writable by you." << endl;
               usage();
               return -1;
            }
            isMultiple = true;
         }
         else
         {
             usage();
             return -1;
         }
      }
    }
    cout << endl;


    //------------------------------------------------------------
    // Validate the inputs.
    //------------------------------------------------------------

    //
    // Display an error message if the user has specified
    // multiple process mode and FIRST_ORDER.
    // The version of FO that is specified by FIRST_ORDER (Objective)
    // does not run in the multiple process mode.
    //
    if( isMultiple && (objective == FIRST_ORDER ) )
    {
      cerr << "!!!!! ERROR !!!!! " << endl;
      cerr << "The version of FO you specified (\"first_order\") does ";
      cerr << "not run in the multiple process mode." << endl;
      cerr << "Did you mean \"naive_first_order\"?" << endl;
      cerr << endl << endl;
      usage();
      return -1;
    }

    //
    // Check if user provided all required arguments.
    // If user requested "parallel" execution, then a shared directory path
    // must be also provided.
    //
    if(isMultiple && !isDirSet)
    {
      cerr << "!!!!! ERROR !!!!! " << endl;
      cerr << "A shared directory for communication has to be specified." << endl;
      usage();
      cerr << endl << endl;
      usage();
      return -1;
    }


    //------------------------------------------------------------
    // Define the problem to be solved.
    //------------------------------------------------------------

    // Set the number of population parameters (fixed effects).
    int nPopPar = 10;

    // Set the number of individual parameters (random effects).
    int nIndPar = 3;

    // Determine the number of individuals and data values in the file.
    int nInd = 0;
    int nTotalData = 0;
    valarray<int>    N(0);
    valarray<double> gamma(0);
    valarray<double> w(0);
    valarray<double> t(0);
    valarray<double> y(0);

    // Read the data from the file
    readNonmemTheophylline(datafile, nInd, nTotalData, N, gamma, w, t, y);

    // Construct a model object based upon the initialied values.
    DiffEqnModel model( nPopPar, nIndPar, odestep, N, gamma, w, t );


    //------------------------------------------------------------
    // Do extra when user requests "parallel" execution.
    //------------------------------------------------------------

  if( isMultiple  )
    {
      //
      // If this is executed as the master, construct a string
      // representing a whole command line arguments that invoke a node.
      //
      if( isMaster )
      {
          coNodeCommand[0] = '\0';
          for( int i=0; i<argc; i++ )
          {
              if( strcmp(argv[i], "master") == 0 )
              {
                  strcat(coNodeCommand, "node");
              }
              else
                  strcat(coNodeCommand, argv[i]);
              strcat(coNodeCommand, " ");

          }
      }

      //
      // If this is executed as a node, call node() function and return.
      //
      else
      {
          node(c_sharedDirectory, model);
          return 0;
      }
    }


    //------------------------------------------------------------
    // Create a DirBasedParallelControls object.
    //------------------------------------------------------------

      DirBasedParallelControls parallelControls( isMultiple,
                                               c_sharedDirectory,
                                               coNodeCommand );


    //------------------------------------------------------------
    // Quantities related to the population parameters.
    //------------------------------------------------------------

    valarray<double> aLow(nPopPar);
    valarray<double> aIn(nPopPar);
    valarray<double> aUp(nPopPar);
    valarray<double> aStep(nPopPar);

    // Population parameter (fixed effect) associated with the transfer
    // rate from the absorption to the sample compartment.
    aLow[0] = 0.1;
    aIn[0]  = 3.0;
    aUp[0]  = 5.0;

    // Population parameter (fixed effect) associated with the transfer 
    // rate from the sample compartment to the outside world.
    aLow[1] = 0.008;
    aIn[1]  = 0.08;
    aUp[1]  = 0.5;

    // Population parameter (fixed effect) associated with the clearance.
    aLow[2] = 0.004;
    aIn[2]  = 0.04;
    aUp[2]  = 0.9;

    // Population parameter (fixed effect) associated with the covariance
    // of the individuals' data.
    aIn[3] = log(0.4); 
    aLow[3] = aIn[3] - log(5.0);
    aUp[3]  = aIn[3] + log(5.0);

    //
    //         /                          \
    // Omega = |  1.0     0.005    0.3    |
    //         |  0.005   0.0002   0.006  |
    //         |  0.3     0.006    0.4    |
    //         \                          /
    //
    valarray<double> Omega( nIndPar * nIndPar );
    Omega[0] = 1.0;
    Omega[1] = 0.005;
    Omega[2] = 0.3;
    Omega[3] = 0.005;
    Omega[4] = 0.0002;
    Omega[5] = 0.006;
    Omega[6] = 0.3;
    Omega[7] = 0.006;
    Omega[8] = 0.4;
    
    // Set the initial values for the Cholesky factor, C, of the individual
    // parameters (random effects) covariance such that D = C * C^T
    valarray<double> C = cholesky( Omega, nIndPar );

    aIn[4] = C[0];
    aIn[5] = C[1];
    aIn[6] = C[4];
    aIn[7] = C[2];
    aIn[8] = C[5];
    aIn[9] = C[8];

    // Set the bounds for the Cholesky factor of the individual parameters
    // (random effects) covariance.
    int k;
    int kDiag = 4;
    int nOffDiag = 0;
    for ( int j = 0; j < 3; j++ )
    {
        // Don't allow the sign of the Cholesky factor's diagonal 
        // elements to change.  This prevents the diagonal elements 
        // of the covariance from being zero.
        aLow[kDiag] = 1.0e-3 * aIn[kDiag];
        aUp[kDiag]  = 10.0 * aIn[kDiag];

        // Allow the Cholesky factor's off-diagonal elements to have
        // any sign and to be equal to zero.
        for ( k = kDiag - nOffDiag; k < kDiag; k++ )
        {
          aLow[k] = -5.0 * fabs(aIn[k]);
          aUp[k]  = +5.0 * fabs(aIn[k]);
        }

        kDiag += ++nOffDiag + 1;
    }

    // Set the step sizes for all of the population parameters (fixed effects). 
    for(k = 0; k < nPopPar; k++)
    {
        aStep[k] = (aUp[k] - aLow[k]) / 1000.;
    }


    //------------------------------------------------------------
    // Quantities related to the individual parameters.
    //------------------------------------------------------------

    valarray<double> bLow(nIndPar);
    valarray<double> bUp(nIndPar);
    valarray<double> bStep(nIndPar);

    // There is one initial value vector for each individual.
    valarray<double> bIn(nIndPar * nInd);

    // Use the same step sizes for the population parameters (fixed effects)
    // and for the individual parameters (random effects).
    for(k = 0; k < nIndPar; k++)
    {
        bStep[k] = aStep[k];
    }

    bLow = -10.0;
    bIn  =  0.0;
    bUp  = +10.0; 


    //------------------------------------------------------------
    // Final preparations before calling fitPopulation.
    //------------------------------------------------------------

    assert( isMaster );

    //
    // Optimizer controls for individual level optimization
    //
    // The first argument specifies the tolerance
    // The second argument is for max #iterations
    // The third argument is for print out level
    //
    Optimizer indOptimizer( 1e-3, 50, 0 );

    //
    // Optimizer controls for population level optimization
    //
    // The first argument specifies the tolerance
    // The second argument is for max #iterations
    // The third argument is for print out level
    //
    Optimizer popOptimizer( 1e-3, 100, 1 );

    // Output values that should be calculated.
    valarray<double> aOut(nPopPar), bOut(nIndPar * nInd), LTilde_aOut(nPopPar);
    valarray<double>* LTilde_a_aOut = NULL;
    double LTildeOut;

    //
    // Variables needed to keep dates and other extra information.
    // These are for debugging/timing.
    //
    time_t began;
    time_t finished;
    double duration_sec = 0.0;
    std::string configuration;

#ifdef NDEBUG
    configuration = "Release";
#else
    configuration = "Debug";
#endif

    cout << "---------------------------------------------------------------" << endl;
    cout << "   " << " < ";

      switch(objective)
      {
      case MODIFIED_LAPLACE:
        cout << "Modified Laplace";
        break;
      case EXPECTED_HESSIAN:
        cout << "Expected Hessian";
        break;
      case FIRST_ORDER:
        cout << "First Order";
        break;
      case NAIVE_FIRST_ORDER:
        cout << "Naive First Order";
        break;
      default:
        cerr << "Unknown Objective";
        break;
      }

    cout << " > " << configuration << " version" << " [ODE Step Size = " << odestepStr << "]" << endl;
    if(parallelControls.getIsParallel())
    {
        cout << "           * Master is running in the multiple process mode *" << endl;
    }
    else
    {
        cout << "                   * Single process mode *" << endl;
    }
    cout << "---------------------------------------------------------------" << endl;

    
    //------------------------------------------------------------
    // Optimize the parametric population objective function.
    //------------------------------------------------------------

    began = clock();

    try{
        fitPopulation(
                       model, 
                       objective, 
                       model._N, 
                       y, 
                       popOptimizer,
                       aLow, 
                       aUp, 
                       aIn,
                       aStep,
                       &aOut, 
                       indOptimizer,
                       bLow, 
                       bUp, 
                       bIn,
                       bStep,
                       &bOut, 
                       &LTildeOut, 
                       &LTilde_aOut, 
                       LTilde_a_aOut,
                       parallelControls
                     );
    }
    catch(const SpkException& e)
    {
        cerr << "fitPopulation threw SpkException... terminating..." << endl;
        cerr << e;
        return -1;
    }
    catch(...)
    {
        cerr << "fitPopulation failed... terminating..." << endl;
        return -1;
    }

    finished = clock();
    duration_sec = (double)(finished - began)/CLOCKS_PER_SEC;

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Slowdown Caused by Post fitPopulation Code- Mitch]
    // Depending on what the code after the call to fitPopulation looks
    // like, this test takes different times to execute.
    //
    // If the code is left alone, this test takes about 17.9 seconds.
    //
    // If the code from here to the next Revisit note is enclosed in 
    // an if ( true ) block, and if the call to compareSpkToNonmem() 
    // is commented out, then this test takes about 14.5 seconds.
    //
    // If the code from here to the next Revisit note is commented out
    // completely, and if the call to compareSpkToNonmem() is commented 
    // out, then this test takes 13.7 seconds. 
    //
    // Note that these times are all for the first order method with
    // ode step size 0.01 on Mitch's machine.
    /*
    if ( true )
    {
    */
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    //------------------------------------------------------------
    // Convert the SPK values to their NONMEM equivalents.
    //------------------------------------------------------------

    // SPK's objective differs from NONMEM's by a constant.
    int nYTotal = y.size();
    double equivNonmemObj = spkToNonmemObj( LTildeOut, nYTotal );

    // The first nIndPar elemements of this model's population parameter
    // (fixed effects) vector are equivalent to NONMEM's theta vector.
    valarray<double> thetaSpk( nIndPar );
    for(k = 0; k < nIndPar; k++)
    {
        thetaSpk[k] = aOut[k];
    }

    // SPK's individual parameters (random effects) covariance
    // is equivalent to NONMEM'S OMEGA matrix.
    valarray<double> D = DiffEqnModel::Dval(nIndPar, aOut);

    // The common variance factor for this model's individual data
    // covariance is equivalent to a 1 by 1 NONMEM SIGMA matrix.
    valarray<double> sigmaSpk( 1 );
    sigmaSpk[0] = exp( aOut[3] );


    //------------------------------------------------------------
    // Display the results.
    //------------------------------------------------------------

    cout << "----------------------------------------------------------" << endl;

    cout << "SPK objective (LTilde) = " << LTildeOut << endl;
    cout << endl;
    cout << "Equivalent NONMEM Objective (2 LTilde - " << nYTotal  
      << " log(2 pi)) = " << equivNonmemObj << endl;
    cout << endl;
    cout << "SPK population parameter (a) =" << endl;
    printInMatrix( aOut, 1 );
    cout << endl;
    cout << "SPK individual parameter covariance (D) =" << endl;
    printInMatrix( D, 3 );
    cout << endl;
    cout << "SPK individual data covariance (R) scale factor = " 
      << exp( aOut[3] ) << endl;
    cout << endl;
    
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Slowdown Caused by Post fitPopulation Code- Mitch]
    // Depending on what the code after the call to fitPopulation looks
    // like, this test takes different times to execute.
    /*
    }
    */
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    //------------------------------------------------------------
    // Validate the results.
    //------------------------------------------------------------

    // Set the tolerances for the various comparisons.
    // NONMEM guaranteers 3 siginificant digits.
    // So, set the tolerances to the maximum of 1% difference.
    int tolObj   = 3;
    int tolTheta = 3;
    int tolOmega = 3;
    int tolSigma = 3;

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Slowdown Caused by Post fitPopulation Code- Mitch]
    // Depending on what the code after the call to fitPopulation looks
    // like, this test takes different times to execute.
    //
    compareSpkToNonmem(
      objective,
      equivNonmemObj,
      thetaSpk,
      D,
      nIndPar,
      sigmaSpk,
      tolObj,
      tolTheta,
      tolOmega,
      tolSigma );
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    //------------------------------------------------------------
    // Finish up.
    //------------------------------------------------------------

    cout << "----------------------------------------------------------" << endl;
    printf("             It took %.3f seconds\n", duration_sec);
    cout << "----------------------------------------------------------" << endl;

    if(parallelControls.getIsParallel())
        cout << "Master is exiting..." << endl;

    cout << endl;

    return 0;
}


/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

namespace // [Begin: unnamed namespace]
{

/*****************************************************************
 *
 * Function: compareSpkToNonmem
 *
 * Returns true if the values from SPK agree with the values
 * calculated using NONMEM.
 *
 *****************************************************************/

bool compareSpkToNonmem(  Objective objective, 
                          double objSpk,
                          const valarray<double>& thetaSpk,
                          const valarray<double>& omegaSpk,
                          int nIndPar,
                          const valarray<double>& sigmaSpk,
                          double tolObj,
                          double 
tolTheta,
                          double tolOmega,
                          double tolSigma )
{
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    using namespace std;


    //------------------------------------------------------------
    // Get the NONMEM values.
    //------------------------------------------------------------

    double           objNonmem;
    valarray<double> thetaNonmem( thetaSpk.size() );
    valarray<double> omegaNonmem( omegaSpk.size() );
    valarray<double> sigmaNonmem( sigmaSpk.size() );

    if ( objective == MODIFIED_LAPLACE )
    {
      using namespace nonmemLaplace;
      objNonmem   = nonmemObj();
      thetaNonmem = nonmemTheta();
      omegaNonmem = nonmemOmega();
      sigmaNonmem = nonmemSigma();
    }
    else if ( objective == EXPECTED_HESSIAN )
    {
      using namespace nonmemFoce;
      objNonmem   = nonmemObj();
      thetaNonmem = nonmemTheta();
      omegaNonmem = nonmemOmega();
      sigmaNonmem = nonmemSigma();
    }
    else if ( objective == FIRST_ORDER || objective == NAIVE_FIRST_ORDER )
    {
      using namespace nonmemFo;
      objNonmem   = nonmemObj();
      thetaNonmem = nonmemTheta();
      omegaNonmem = nonmemOmega();
      sigmaNonmem = nonmemSigma();
    }
    else
    {
      cerr << "Invalid objective: " << objective << endl;
    }


    //------------------------------------------------------------
    // Compare the values.
    //------------------------------------------------------------

    bool agree = true;
  
    agree = equalToNSigDigitsTest( 
      objSpk,
      objNonmem,
      static_cast<int>(tolObj),
      "SPK's NONMEM equivalent objective",
      "NONMEM objective" ) && agree;

    agree = equalToNSigDigitsTest( 
      thetaSpk,
      thetaNonmem,
      1,
      static_cast<int>(tolTheta),
      "SPK's population parameters (fixed effects) that correspond to NONMEM's THETA",
      "NONMEM fixed effects vector (THETA)" ) && agree;

    agree = equalToNSigDigitsTest( 
      omegaSpk,
      omegaNonmem,
      nIndPar,
      static_cast<int>(tolOmega),
      "SPK's individual parameters (random effects) covariance (D)",
      "NONMEM covariance for eta random effects (OMEGA)" ) && agree;

    agree = equalToNSigDigitsTest( 
      sigmaSpk,
      sigmaNonmem,
      1,
      static_cast<int>(tolSigma),
      "SPK's individual data covariance SIGMA equivalent",
      "NONMEM covariance for epsilon random effects (SIGMA)" ) && agree;


    //------------------------------------------------------------
    // Finish up.
    //------------------------------------------------------------

    if ( agree )
    {
      cout << "Test Succeeded:  All values are equal (within tolerance).";
    }
    else
    {
      cout << "Test Failed:  Not all values are equal (within tolerance).";
    }
    cout << endl;

    return agree;
}


/*****************************************************************
 *
 * Function: spkToNonmemObj
 *
 * Calculates the NONMEM objective function value that is 
 * equivalent to the SPK objective function value lTilde, where
 * the total number of data values is nYTotal.
 *
 *****************************************************************/

double spkToNonmemObj( double lTilde, int nYTotal )
{
    return 2.0 * lTilde - nYTotal * log(2.0 * PI);
}


/*************************************************************************
 *
 * Function: usage
 *
 *************************************************************************/

void usage()
{
   using namespace std;
   cout << endl;
   cout << "Usage: DiffEqnModelTest [-data DATA_FILE] [-ode ODE_STEP] [ (-who (master | node)) (-dir SharedDir) ] [ -objective ( first_order | naive_first_order | expected_hessian | modified_laplace ) ]" << endl;
   cout << endl;
   cout << "\t-data --- (optional) followed by a path to a data file." << endl;
   cout << "\t          The program looks for Theophylline40Simulated.dat in the current directory by default." << endl;
   cout << "\t-ode  --- (optional) followed by a step size." << endl;
   cout << "\t-objective --- (optional) followed by one of the followings:" << endl;
   cout << "\t               \"expected_hessian\", " << endl;
   cout << "\t               \"modified_laplace\", " << endl;
   cout << "\t               \"first_order\" or " << endl;
   cout << "\t               \"naive_first_order\"." << endl;
   cout << "\t          The default value is modified_laplace." << endl;
   cout << endl;
   cout << "By default the program executes in the ordinary single process mode." << endl;
   cout << "To enable parallel computation, specify the followings:" << endl;
   cout << "\t-who    --- (required) followed by a keyword either \"master\" or \"node\"." << endl;
   cout << "\t            The program executes as Master by default." << endl;
   cout << "\t-dir    --- (required) followed by a path to the shared directory" << endl;
   cout << "\t            e.x. \\\\pasta\\public\\shared\\" << endl;
   cout << endl;
}


/*************************************************************************
 *
 * Function: copyright
 *
 *************************************************************************/

void copyright()
{
  using namespace std;

  cout << " *************************************************************************" << endl;
  cout << " *                                                                       *" << endl;
  cout << " *  From:   Resource Facility for Population Kinetics                    *" << endl;
  cout << " *          Department of Bioengineering Box 352255                      *" << endl;
  cout << " *          University of Washington                                     *" << endl;
  cout << " *          Seattle, WA 98195-2255                                       *" << endl;
  cout << " *                                                                       *" << endl;
  cout << " *  Copyright (C) 2002, University of Washington,                        *" << endl;
  cout << " *  Resource Facility for Population Kinetics. All Rights Reserved.      *" << endl;
  cout << " *                                                                       *" << endl;
  cout << " *  This software was developed with support from NIH grant RR-12609.    *" << endl;
  cout << " *  Please cite this grant in any publication for which this software    *" << endl;
  cout << " *  is used and send a notification to the address given above.          *" << endl;
  cout << " *                                                                       *" << endl;
  cout << " *  Check for updates and notices at:                                    *" << endl;
  cout << " *  http://www.rfpk.washington.edu                                       *" << endl;
  cout << " *                                                                       *" << endl;
  cout << " *************************************************************************" << endl;
  cout << endl;
}


/*************************************************************************
 *
 * Function: relTolEqualTest  (double version)
 *
 *************************************************************************/

bool equalToNSigDigitsTest( double x, 
                            double y, 
                            int    digits,
                            std::string xName,
                            std::string yName )
{
  using namespace std;

  if ( isEqualToNSigDigits( x, y, digits ) )
  {
      return true;
  }
  else
  {
    cout << "[Values are not equal up to relative tolerance]" << endl;
    cout << endl;
    cout << xName << " = " << x << endl;
    cout << endl;
    cout << yName << " = " << y << endl;
    cout << endl;

    return false;
  }
}
  

/*************************************************************************
 *
 * Function: relTolEqualTest  (valarray<double> version)
 *
 *************************************************************************/

bool equalToNSigDigitsTest( const valarray<double> &x, 
                            const valarray<double> &y, 
                            int nCols,
                            int digits,
                            std::string xName,
                            std::string yName )
{
  using namespace std;

  if ( isEqualToNSigDigits( x, y, digits ) )
  {
      return true;
  }
  else
  {
    cout << "[Values are not equal up to relative tolerance]" << endl;
    cout << endl;
    cout << xName << " = " << endl;
    printInMatrix( x, nCols );
    cout << endl;
    cout << yName << " = " << endl;
    printInMatrix( y, nCols );
    cout << endl;

    return false;
  }
}
  

/*************************************************************************
 *
 * Function: isEqualToNSigDigits
 *
 * Determine two double-precision numbers are equal up to X significant
 * digits.
 *
 *************************************************************************/
bool isEqualToNSigDigits( double x, double y, int n )
{
  double scale = fabs( x ) > fabs( y ) ? fabs( x ) : fabs( y );
  double tol   = 1.0 / n;

  if ( fabs( x - y ) < tol * scale )
  {
    return true;
  }
  else 
  {
    return false;
  }
}

bool isEqualToNSigDigits(  const valarray<double> &x, 
                           const valarray<double> &y, 
                           int  n )
{
  assert( x.size() == y.size() );
  
  for( int i = 0; i < x.size(); i++ )
  {
    if( !isEqualToNSigDigits( x[i], y[i], n ) )
    {
      return false;
    }
  }
  return true;
}

/*************************************************************************
 *
 * Function: nonmemLaplace::nonmemObj
 *
 *************************************************************************/

double nonmemLaplace::nonmemObj()
{
  // This value was computed using NONMEM.
  return 328.;
}


/*************************************************************************
 *
 * Function: nonmemLaplace::nonmemTheta
 *
 *************************************************************************/

const valarray<double> nonmemLaplace::nonmemTheta()
{
  int nRows = 3;
  int nCols = 1;

  valarray<double> value( nRows*nCols );

  // These values were computed using NONMEM.
  value[ 0 ] = 3.17e+00;
  value[ 1 ] = 7.89e-02;
  value[ 2 ] = 4.03e-02;

  return value;
}


/*************************************************************************
 *
 * Function: nonmemLaplace::nonmemOmega
 *
 *************************************************************************/

const valarray<double> nonmemLaplace::nonmemOmega()
{
  int nRows = 3;
  int nCols = 3;

  valarray<double> value( nRows*nCols );

  // These values were computed using NONMEM.
  value[ 0 ] = 2.44e+00;
  value[ 1 ] = 6.40e-03;
  value[ 2 ] = 2.45e-01;
  value[ 3 ] = 6.40e-03;
  value[ 4 ] = 1.75e-04;
  value[ 5 ] = 4.59e-03;
  value[ 6 ] = 2.45e-01;
  value[ 7 ] = 4.59e-03;
  value[ 8 ] = 3.87e-01;

  return value;
}


/*************************************************************************
 *
 * Function: nonmemLaplace::nonmemSigma
 *
 *************************************************************************/

const valarray<double> nonmemLaplace::nonmemSigma()
{
  int nRows = 1;
  int nCols = 1;

  valarray<double> value( nRows*nCols );

  // These values were computed using NONMEM.
  value[ 0 ] = 4.03e-01;

  return value;
}


/*************************************************************************
 *
 * Function: nonmemFoce::nonmemObj
 *
 *************************************************************************/

double nonmemFoce::nonmemObj()
{
  // This value was computed using NONMEM.
  return 329.;
}


/*************************************************************************
 *
 * Function: nonmemFoce::nonmemTheta
 *
 *************************************************************************/

const valarray<double> nonmemFoce::nonmemTheta()
{
  int nRows = 3;
  int nCols = 1;

  valarray<double> value( nRows*nCols );

  // These values were computed using NONMEM.
  value[ 0 ] = 3.12e+00;
  value[ 1 ] = 7.83e-02;
  value[ 2 ] = 4.02e-02;

  return value;
}


/*************************************************************************
 *
 * Function: nonmemFoce::nonmemOmega
 *
 *************************************************************************/

const valarray<double> nonmemFoce::nonmemOmega()
{
  int nRows = 3;
  int nCols = 3;

  valarray<double> value( nRows*nCols );

  // These values were computed using NONMEM.
  value[ 0 ] = 2.40e+00;
  value[ 1 ] = 6.66e-03;
  value[ 2 ] = 2.40e-01;
  value[ 3 ] = 6.66e-03;
  value[ 4 ] = 1.76e-04;
  value[ 5 ] = 4.59e-03;
  value[ 6 ] = 2.40e-01;
  value[ 7 ] = 4.59e-03;
  value[ 8 ] = 3.86e-01;


  return value;
}


/*************************************************************************
 *
 * Function: nonmemFoce::nonmemSigma
 *
 *************************************************************************/

const valarray<double> nonmemFoce::nonmemSigma()
{
  int nRows = 1;
  int nCols = 1;

  valarray<double> value( nRows*nCols );

  // These values were computed using NONMEM.
  value[ 0 ] = 4.03e-01;

  return value;
}


/*************************************************************************
 *
 * Function: nonmemFo::nonmemObj
 *
 *************************************************************************/

double nonmemFo::nonmemObj()
{
  // This value was computed using NONMEM.
  return 396.;
}


/*************************************************************************
 *
 * Function: nonmemFo::nonmemTheta
 *
 *************************************************************************/

const valarray<double> nonmemFo::nonmemTheta()
{
  int nRows = 3;
  int nCols = 1;

  valarray<double> value( nRows*nCols );

  // These values were computed using NONMEM.
  value[ 0 ] = 2.64e+00;
  value[ 1 ] = 7.64e-02;
  value[ 2 ] = 3.94e-02;

  return value;
}


/*************************************************************************
 *
 * Function: nonmemFo::nonmemOmega
 *
 *************************************************************************/

const valarray<double> nonmemFo::nonmemOmega()
{
  int nRows = 3;
  int nCols = 3;

  valarray<double> value( nRows*nCols );

  // These values were computed using NONMEM.
  value[ 0 ] = 1.90e+00;
  value[ 1 ] = 1.29e-02;
  value[ 2 ] = 3.17e-01;
  value[ 3 ] = 1.29e-02;
  value[ 4 ] = 2.76e-04;
  value[ 5 ] = 8.51e-03;
  value[ 6 ] = 3.17e-01;
  value[ 7 ] = 8.51e-03;
  value[ 8 ] = 5.72e-01;

  return value;
}


/*************************************************************************
 *
 * Function: nonmemFo::nonmemSigma
 *
 *************************************************************************/

const valarray<double> nonmemFo::nonmemSigma()
{
  int nRows = 1;
  int nCols = 1;

  valarray<double> value( nRows*nCols );

  // These values were computed using NONMEM.
  value[ 0 ] = 4.55e-01; //4.50e-01;

  return value;
}


} // [End: unnamed namespace]
