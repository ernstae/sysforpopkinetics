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
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


         DiffEqnModel based user test interface specification


         Sachiko Honda, 11/22/02


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*/
/*
============================================================================

     The list of command line arguments to a DiffEqnModel driver
   
============================================================================

This section specifies a list of command line arguments expected by
a DiffEqnModel based user test driver.

----------------------------------------------
     Notation Used
----------------------------------------------
()     : a list of choices
|      : logical OR
{}     : optional
[ - ]  : range
""     : case insensitive string
-xxx   : argument specifier followed by value(s)
----------------------------------------------

All of the following arguments are required.  
The order of appearances may be arbitrary:


-data NONMEMDATA

    NONMEMDATA      =: a path name for a NONMEM data file
    
    Note: When a relative path is used the directory in which 
          the C++ executable exists is considered as the base.

-out OUTPUT_FILENAME

    OUTPUT_FILENAME =: a path name for an output file.

    Note: When a relative path is used the directory in which 
          the C++ executable exists is considered as the base.

-method METHOD

    METHOD        =: ( "laplace" | "foce" | "fo" | "naive_fo" )

-pk MODEL_TYPE

    MODEL_TYPE    =: ( "additive" | "proportional" | "exponential" )

-error ERROR_TYPE

    ERROR_TYPE    =: ( "additive" | "model_based" | "data_based" )

-sigma SIGMA

    SIGMA         =: [ -2.2250738585072014e-308 - +1.7976931348623158e+308 ]
    
-popmitr POP_MAX_ITERS

    POP_MAX_ITERS =: ( [ 0 - 2147483647 ] | "inf" )

    Note: When "inf" is specified, the maximum (signed) integer,
          2147483647 will be applied.

-indmitr IND_MAX_ITERS

    IND_MAX_ITERS =: ( [ 0 - 2147483647 ] | "inf" )

    Note: When "inf" is specified, the maximum (signed) integer,
          2147483647 will be applied.

-omega ( "block" VAL1 VAL2 VAL3 VAL4 VAL5 VAL6 | "diagonal" VAL1 VAL2 VAL3 )
       
       VALi       =: [ -2.2250738585072014e-308 - +1.7976931348623158e+308 ]

       where i = 1, 2, 3 for "block", = 1, 2, ... 6 for "diagonal".
    
       If "block", the initial estimate for omega is:
               /                      \
               |  VAL1                |
               |  VAL2   VAL3         |
               |  VAL4   VAL5   VAL6  |
               \                      /
       If "diagonal", the initial estimate for omega is:
               /                      \
               |  VAL1    0      0    |
               |   0     VAL2    0    |
               |   0      0     VAL3  |
               \                      /
       

-theta LOW1 IN1 UP1 LOW2 IN2 UP2 LOW3 IN3 UP3

    LOWi =: [ -2.2250738585072014e-308 - +1.7976931348623158e+308 ]
    INi  =: [ LOWi                     - UPi                      ]
    UPi  =: [ -2.2250738585072014e-308 - +1.7976931348623158e+308 ]

    where i=1, 2, 3.

-form FORM
    
  FORM =: ( "direct" | "exponential" )

  This specifies the type of parameterization of Sigma

-eta LOW1 UP1 LOW2 UP2 LOW3 UP3

    LOWi =: the lower boundary for eta(i)
    Upi  =: the upper boundary for eta(i)

    Note: The initial value for eta(i) is always 0.0 (hard-coded).

-formulation ( "1" | "2" | "3" )
    
    1:  R^-1 * S * R^-1
    2:  R^-1
    3:  S^-1

  where R is a matrix containing the second derivative of the population objective (lTilde_alp_alp)
  and S is Sum{ [(LTilde_i )_alp] [(LTilde_i)_alp] }.
============================================================================

     The output from a DiffEqnModel driver
   
============================================================================
This section specifies the output from a DiffEqnModel based user test driver.

----------------------------------------------
     Notation Used
----------------------------------------------
THETA1  : theta(1)
THETA2  : theta(2)
THETA3  : theat(3)
SIGMA   : sigma(1,1)
OMEGA11 : omega(1,1)
OMEGA21 : omega(2,1)
OMEGA22 : omega(2,2)
OMEGA31 : omega(3,1)
OMEGA32 : omega(3,2)
OMEGA33 : omega(3,3)
ETA1    : eta(1)
ETA2    : eta(2)
ETA3    : eta(3)
----------------------------------------------

Upon a successful completion, the executable will return 0
and saves the results into the output file provided by the caller
as a command line argument.

The output file will contain the following text upon a
successful completion:

THETA1\n
THETA2\n
THETA3\n 
SIGMA\n
OMEGA12\n
OMEGA21\n
OMEGA22\n
OMEGA31\n
OMEGA32\n
OMEGA33\n
OBJVAL\n
ETA1\n
ETA2\n
ETA3\n

If execution fails, -1 will be returned and the specified output
file will being with a string "error" teminated by a new line caracter,
\n, followed by error messages captured during the execution if
there were any.
*/

#pragma warning( disable : 4786 )

#include <iostream>
#include <string>
#include <math.h>
#include <ctime>
#include <fstream>
#include <cstring>
#include <map>

#include "../../../../spk/SpkValarray.h"
#include "../../../../spk/cholesky.h"
#include "../../../../spk/Objective.h"
#include "../../../../spk/multiply.h"
#include "../../../../spk/transpose.h"
#include "../../../../spk/printInMatrix.h"
#include "../../../../spk/symmetrize.h"
#include "../../../../spk/Optimizer.h"
#include "../../../../spk/fitPopulation.h"
#include "../../../../spk/pi.h"
#include "../../../../spk/popStatistics.h"

#include "readNonmemTheophylline.h"
#include "DiffEqnModel.h"
#include "convertStatisticsToNonmem.h"
#include "nonmemEqvContainers.h"

using SPK_VA::valarray;
using namespace nonmemEqvContainers;

const int SUCCESS = 0;
const int FAILURE = -1;

static double spkToNonmemObj( double lTilde, int nYTotal )
{
    return 2.0 * lTilde - nYTotal * log(2.0 * PI);
}

static int usage( const std::string& message = "error" )
{
    using namespace std;
    cerr << message << endl;

    return static_cast<int>( FAILURE );
}

int main( int argc, const char *const argv[] )
{
    using namespace std;

    FpErrorChecker fp;
    fp.clear();

    //
    // Argument keywords
    //
    const string KEY_NONMEMDATA      = "-data";
    const string KEY_OUTPUT_FILENAME = "-out";
    const string KEY_METHOD          = "-method";
    const string KEY_PK              = "-pk";
    const string KEY_ERROR           = "-error";
    const string KEY_SIGMA           = "-sigma";
    const string KEY_POPMAXITER      = "-popmitr";
    const string KEY_INDMAXITER      = "-indmitr";
    const string KEY_OMEGA           = "-omega";
    const string KEY_THETA           = "-theta";
    const string KEY_SIGMAFORM       = "-form";
    const string KEY_ETA             = "-eta";
    const string KEY_FORMULATION     = "-formulation";

    const string VAL_ADDITIVE        = "additive";
    const string VAL_PROPORTIONAL    = "proportional";
    const string VAL_EXPONENTIAL     = "exponential";
    const string VAL_MODEL_BASED     = "model_based";
    const string VAL_DATA_BASED      = "data_based";
    const string VAL_BLOCK           = "block";
    const string VAL_DIAGONAL        = "diagonal";
    const string VAL_LAPLACE         = "laplace";
    const string VAL_FOCE            = "foce";
    const string VAL_FO              = "fo";
    const string VAL_NAIVE_FO        = "naive_fo";
    const string VAL_DIRECT          = "direct";

    //
    // A table to keep track of which argument has been given by the user.
    //
    std::map<string, bool> isGiven;
    isGiven.insert( map<string, bool>::value_type( KEY_NONMEMDATA,      false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_OUTPUT_FILENAME, false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_METHOD,          false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_PK,              false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_ERROR,           false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_SIGMA,           false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_POPMAXITER,      false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_INDMAXITER,      false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_OMEGA,           false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_THETA,           false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_SIGMAFORM,       false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_ETA,             false ) ); 
    isGiven.insert( map<string, bool>::value_type( KEY_FORMULATION,     false ) ); 
  

    //
    // Placeholders for argument values
    //
    string                         datafile;
    string                         outfile;
    Objective                      objective;
    double                         sigmaVal;
    enum DiffEqnModel::PK_TYPE     pk;
    enum DiffEqnModel::ERROR_TYPE  error;
    enum DiffEqnModel::OMEGA_TYPE  omegastruc;
    valarray<double>               omegaInFull(9);
    int                            popMaxIters;
    int                            indMaxIters;
    valarray<double>               thetaLow(3);
    valarray<double>               thetaIn(3);
    valarray<double>               thetaUp(3);
    valarray<double>               etaLow (3);
    valarray<double>               etaIn  (3);
    valarray<double>               etaUp  (3);
    bool                           exponential;
    enum PopCovForm                stat_form;

    //
    // Containers for xml output
    //
    Status status( false, false, false, false );
    Message message;

    //
    // Parse the argument list in an arbitrary order
    //
    int i, j, cnt;
    map<string, bool>::iterator itr;
    for( i=1; i<argc; i++ )
    {
        if( strcasecmp( argv[i], KEY_NONMEMDATA.c_str() ) == 0 )
        {
            ++i;
            if( i < argc )
                datafile = argv[i];
            else return usage( KEY_NONMEMDATA );

            itr = isGiven.find( KEY_NONMEMDATA );
            (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_OUTPUT_FILENAME.c_str() ) == 0 )
        {
            ++i;
            if( i < argc )
                outfile = argv[i];
            else return usage( KEY_OUTPUT_FILENAME );

            itr = isGiven.find( KEY_OUTPUT_FILENAME );
            (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_METHOD.c_str() ) == 0 )
        {
            ++i;
            if( i < argc )
            {
                if( strcasecmp( argv[i], VAL_LAPLACE.c_str() ) == 0 )
                    objective = MODIFIED_LAPLACE;
                else if( strcasecmp( argv[i], VAL_FOCE.c_str() ) == 0 )
                    objective = EXPECTED_HESSIAN;
                else if( strcasecmp( argv[i], VAL_FO.c_str() ) == 0 )
                    objective = FIRST_ORDER;
                else if( strcasecmp( argv[i], VAL_NAIVE_FO.c_str() ) == 0 )
                    objective = NAIVE_FIRST_ORDER;
                else
                {
                    cerr << "Invalid method! (" << VAL_LAPLACE << " | " << VAL_FOCE << " | " << VAL_FO << " | " << VAL_NAIVE_FO << ")" << endl;
                    return usage( argv[i] );
                }
            }
            else return usage( KEY_METHOD );

            itr = isGiven.find( KEY_METHOD );
            (*itr).second = true;

        }
        else if( strcasecmp( argv[i], KEY_PK.c_str() ) == 0 )
        {
            ++i;
            if( i < argc )
            {
                if( strcasecmp( argv[i], VAL_ADDITIVE.c_str() ) == 0 )
                    pk = DiffEqnModel::PK_ADDITIVE;
                else if( strcasecmp( argv[i], VAL_PROPORTIONAL.c_str() ) == 0 )
                    pk = DiffEqnModel::PK_PROPORTIONAL;
                else if( strcasecmp( argv[i], VAL_EXPONENTIAL.c_str() ) == 0 )
                    pk = DiffEqnModel::PK_EXPONENTIAL;
                else
                {
                    cerr << "Invalid PK type! (" << VAL_ADDITIVE << " | " << VAL_PROPORTIONAL << " | " << VAL_EXPONENTIAL << ")" << endl;
                    return usage( argv[i] );
                }
            }
            else return usage( KEY_PK );

            itr = isGiven.find( KEY_PK );
            (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_ERROR.c_str() ) == 0 )
        {
            ++i;
            if( i < argc )
            {
                if( strcasecmp( argv[i], VAL_ADDITIVE.c_str() ) == 0 )
                    error = DiffEqnModel::ER_ADDITIVE;
                else if( strcasecmp( argv[i], VAL_MODEL_BASED.c_str() ) == 0 )
                    error = DiffEqnModel::ER_MODEL_BASED;
                else if( strcasecmp( argv[i], VAL_DATA_BASED.c_str() ) == 0 )
                    error = DiffEqnModel::ER_DATA_BASED;
                else
                {
                    cerr << "Invalid error type! (" << VAL_ADDITIVE << " | " << VAL_MODEL_BASED << " | " << VAL_DATA_BASED << ")" << endl;
                    return usage( argv[i] );
                }
            }
            else return usage( KEY_ERROR );

            itr = isGiven.find( KEY_ERROR );
            (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_SIGMA.c_str() ) == 0 )
        {
            ++i;
            if( i < argc )
            {
                sigmaVal = atof( argv[i] );
            }
            else return usage( KEY_SIGMA );           

            itr = isGiven.find( KEY_SIGMA );
            (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_POPMAXITER.c_str() ) == 0 )
        {
            ++i;
            if( i < argc )
            {
                popMaxIters = atoi( argv[i] );
                if( popMaxIters < 0 )
                {
                    return usage( "# of maximum iterations (population level) must be a positive number." );
                }
            }
            else return usage( KEY_POPMAXITER );           

            itr = isGiven.find( KEY_POPMAXITER );
            (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_INDMAXITER.c_str() ) == 0 )
        {
            ++i;
            if( i < argc )
            {
                indMaxIters = atoi( argv[i] );
                if( indMaxIters < 0 )
                {
                    return usage( "# of maximum iterations (individual level) must be a positive number." );
                }
            }
            else return usage( KEY_INDMAXITER );           

            itr = isGiven.find( KEY_INDMAXITER );
            (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_OMEGA.c_str() ) == 0 )
        {
            ++i;
            if( i < argc )
            {
                omegaInFull = 0.0;
                if( strcasecmp( argv[i], VAL_BLOCK.c_str() ) == 0 )
                {
                    omegastruc = DiffEqnModel::BLOCK;
                    //
                    // The values given by the user fills the lower triangle of the 
                    // matrix, omega:
                    //
                    //         /                          \
                    // omega = |  val(1)    0       0     |
                    //         |  val(2)  val(3)    0     |
                    //         |  val(4)  val(5)  val(6)  |
                    //         \                          /
                    //
                    if( i+6 < argc )
                    {
                        omegaInFull[0] = atof( argv[i+1] );
                        omegaInFull[1] = atof( argv[i+2] );
                        omegaInFull[2] = atof( argv[i+4] );
                        omegaInFull[4] = atof( argv[i+3] );
                        omegaInFull[5] = atof( argv[i+5] );
                        omegaInFull[8] = atof( argv[i+6] );
                        i += 6;
                    }
                    else return usage( "-omaga must be followed by 6 extra arguments for block." );
                }
                else if( strcasecmp( argv[i], VAL_DIAGONAL.c_str() ) == 0 )
                {
                    omegastruc = DiffEqnModel::DIAGONAL;
                    if( i+3 < argc )
                    {
                        omegaInFull[0] = atof( argv[i+1] );
                        omegaInFull[4] = atof( argv[i+2] );
                        omegaInFull[8] = atof( argv[i+3] );
                        i += 3;
                    }
                    else return usage( "-omaga must be followed by 3 extra arguments for diagonal." );
                }
                else return usage( "-omega must be followed by values" );
            }
            else return usage( KEY_OMEGA );           

            itr = isGiven.find( KEY_OMEGA );
            (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_THETA.c_str() ) == 0 )
        {
            if( i+9 < argc )
            {
                for( int j=0; j<3; j++ )
                {
                    thetaLow[j] = atof( argv[i+1] );
                    thetaIn [j] = atof( argv[i+2] );
                    thetaUp [j] = atof( argv[i+3] );
                    i += 3;
                }
            }
            else return usage( KEY_THETA );           

            itr = isGiven.find( KEY_THETA );
            (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_SIGMAFORM.c_str() ) == 0 )
        {
          ++i;
          if( strcasecmp( argv[i], VAL_DIRECT.c_str() ) == 0 )
            exponential = false;
          else if( strcasecmp( argv[i], VAL_EXPONENTIAL.c_str() ) == 0 )
            exponential = true;
          else
          {
              cerr << "Invalid sigma form! (" << VAL_DIRECT << " | " << VAL_EXPONENTIAL << ")" << endl;
              return usage( argv[i] );
          }

          itr = isGiven.find( KEY_SIGMAFORM );
          (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_ETA.c_str() ) == 0 )
        {
          if( i+6 < argc )
          {
              for( int j=0; j<3; j++ )
              {
                  etaLow[j] = atof( argv[i+1] );   // lower boundary value
                  etaIn[j]  = 0.0;               // initial value (fixed 0.0)
                  etaUp[j]  = atof( argv[i+2] );   // upper boundary value
                  i += 2;
              }
          }
          else return usage( KEY_ETA );

          itr = isGiven.find( KEY_ETA );
          (*itr).second = true;
        }
        else if( strcasecmp( argv[i], KEY_FORMULATION.c_str() ) == 0 )
        {
          ++i;
          stat_form = static_cast<enum PopCovForm>( atoi( argv[i] ) );
 
          itr = isGiven.find( KEY_FORMULATION );
          (*itr).second = true;
       }
    }

    //
    // Check if the user provided all required arguments.
    //
    for( itr = isGiven.begin(); itr != isGiven.end(); itr++ )
    {
        if( !(*itr).second )
        {
          cerr << "Argument < " << (*itr).first << " > is missing." << endl;
          return usage( (*itr).first );
        }
    }

    //
    // Display all values acquired from the command line
    //
    cout << "Input data file:              " << datafile << endl;
    cout << "Output file:                  " << outfile << endl;
    cout << "Objective:                    ";
        switch( objective )
        {
        case FIRST_ORDER:
            cout << "FO" << endl;
            break;
        case NAIVE_FIRST_ORDER:
            cout << "Naive FO" << endl;
            break;
        case EXPECTED_HESSIAN:
            cout << "FOCE" << endl;
            break;
        case MODIFIED_LAPLACE:
            cout << "Laplace" << endl;
            break;
        default:
            cerr << "Invalid value" << endl;
            break;
        }
    cout << "Max #iterations (population): " << popMaxIters << endl;
    cout << "Max #iterations (individual): " << indMaxIters << endl;
    cout << "Sigma form:                   " << ( exponential? VAL_EXPONENTIAL : VAL_DIRECT ) << endl;
    cout << "Sigma:                        " << sigmaVal << endl;
    cout << "PK model type:                ";
        switch( pk )
        {
        case DiffEqnModel::PK_ADDITIVE:
            cout << "Additive" << endl;
            break;
        case DiffEqnModel::PK_PROPORTIONAL:
            cout << "Proportional" << endl;
            break;
        case DiffEqnModel::PK_EXPONENTIAL:
            cout << "Exponential" << endl;
            break;
        default:
            cerr << "Invalid value" << endl;
            break;
        }
    cout << "Error model type:             ";
        switch( error )
        {
        case DiffEqnModel::ER_ADDITIVE:
            cout << "Additive" << endl;
            break;
        case DiffEqnModel::ER_MODEL_BASED:
            cout << "Model based " << endl;
            break;
        case DiffEqnModel::ER_DATA_BASED:
            cout << "Data based" << endl;
      break;
        default:
            cerr << "Invalid value" << endl;
            break;
        }
    cout << "Omega (";
        switch( omegastruc )
        {
        case DiffEqnModel::BLOCK:
            cout << "block):                " << transpose(omegaInFull, 3) << endl;
            break;
        case DiffEqnModel::DIAGONAL:  
            cout << "diagonal):             " << transpose(omegaInFull, 3) << endl;
            break;
        default:
            cerr << "invalid";
            break;
        }

    cout << "Theta(1):                     " << "(" << thetaLow[0] << ", " << thetaIn[0] << ", " << thetaUp[0] << ")" << endl;
    cout << "Theta(2):                     " << "(" << thetaLow[1] << ", " << thetaIn[1] << ", " << thetaUp[1] << ")" << endl;
    cout << "Theta(3):                     " << "(" << thetaLow[2] << ", " << thetaIn[2] << ", " << thetaUp[2] << ")" << endl;
    
    cout << "Eta(1):                       " << "(" << etaLow[0] << ", " << etaIn[0] << ", " << etaUp[0] << ")" << endl;
    cout << "Eta(2):                       " << "(" << etaLow[1] << ", " << etaIn[1] << ", " << etaUp[1] << ")" << endl;
    cout << "Eta(3):                       " << "(" << etaLow[2] << ", " << etaIn[2] << ", " << etaUp[2] << ")" << endl;

    cout << "Statistics formulation        " << stat_form << endl;

    //--------------------------------------------------------------------
    //
    // Hard-wired values
    //
    //--------------------------------------------------------------------
    const int    nPopPar       = omegastruc == DiffEqnModel::BLOCK? 10:7;
    const int    nIndPar       = 3;
    const double odeStep       = 0.01;
    const int    popPrintLevel = 1;
    const int    indPrintLevel = 0;
    const double popEpsilon    = 1e-3;
    const double indEpsilon    = 1e-3;

    // Continue displaying hardwired values
    cout << "# of population parameters:   " << nPopPar << endl;
    cout << "# of individual parameters:   " << nIndPar << endl;
    cout << "Population opt. print level:  " << popPrintLevel << endl;
    cout << "Individial opt. print level:  " << indPrintLevel << endl;
    cout << "Population opt. epsilon:      " << popEpsilon << endl;
    cout << "Individual opt. epsilon:      " << indEpsilon << endl;

    //--------------------------------------------------------------------
    //
    // Read the NONMEM datafile to get measurement data and others
    //
    //--------------------------------------------------------------------
    int              nInds;
    int              nDataTotal;
    valarray<int>    nDataPerInd(0);
    valarray<double> gamma(0);
    valarray<double> weight(0);
    valarray<double> time(0);
    valarray<double> data(0);

    //
    // Read the data from the file
    //
    readNonmemTheophylline(datafile.c_str(), nInds, nDataTotal, nDataPerInd, gamma, weight, time, data);

    //
    // Show what I got
    //
    cout << "# individuals:                " << nInds << endl;
    cout << "# total measurements:         " << nDataTotal << endl;
    cout << "# measurements / individual:  " << nDataPerInd << endl;
    //cout << "gamma                      :  " << endl << gamma << endl;
    //cout << "weight                     :  " << endl << weight << endl;
    //cout << "time                       :  " << endl << time << endl;
    //cout << "data                       :  " << endl << data << endl;

    //--------------------------------------------------------------------
    //
    // Construct DiffEqnModel model object
    //
    //--------------------------------------------------------------------
    DiffEqnModel model( nPopPar, nIndPar, odeStep, nDataPerInd, gamma, weight, time, 
                        data, pk, error, omegastruc, exponential );

    //--------------------------------------------------------------------
    //
    // Intialize popParIn(alpIn), popParLow(alpLow), popParUp(alpUp)
    //
    // alp(1) :           mean of mass transfer rate 
    //                    from absorption to sample compartment
    // alp(2) :           mean of mass transfer rate from sample to outside
    // alp(3) :           mean coefficient of the clearance to weight relationship
    // exp(alp(4)) :      variance of measurement noise
    // alp(5) - alp(10) : elements of Cholesky factor of the random
    //                    effects covariance
    //
    //--------------------------------------------------------------------

    valarray<double> popParIn    ( nPopPar );
    valarray<double> popParLow   ( nPopPar );
    valarray<double> popParUp    ( nPopPar );
    valarray<double> popParStep  ( nPopPar );

    //
    // To compute the cholesky factor of the block diagonal matrix, Omega,
    // Omega has to be fully filled.  Use symmetrize() to
    // fill the upper triangle.
    //
    symmetrize( omegaInFull, nIndPar, omegaInFull );

    //
    // Compute the cholesky factor for Omega to obtain
    // values for alpIn(5) - alp(10)
    //
    valarray<double> C = cholesky( omegaInFull, nIndPar );

    cout << "Cholesky factor of omega:     " << endl;
    printInMatrix( C, nIndPar );
    cout << endl;

    popParIn[0]  = thetaIn[0];
    popParIn[1]  = thetaIn[1];
    popParIn[2]  = thetaIn[2];

    popParLow[0] = thetaLow[0];
    popParLow[1] = thetaLow[1];
    popParLow[2] = thetaLow[2];

    popParUp[0]  = thetaUp[0];
    popParUp[1]  = thetaUp[1];
    popParUp[2]  = thetaUp[2];

    if( exponential )
    {
      popParIn [3] = log( sigmaVal );
      popParLow[3] = popParIn[3] - log( 5.0 );
      popParUp [3] = popParIn[3] + log( 5.0 );
    }
    else
    {
      popParIn [3] = sigmaVal;
      popParLow[3] = popParIn[3] / 5.0;
      popParUp [3] = popParIn[3] * 5.0;
    }

    if( omegastruc == DiffEqnModel::BLOCK )
    {
        popParIn[4]  = C[0];  // C(1,1)
        popParIn[5]  = C[1];  // C(2,1) 
        popParIn[6]  = C[4];  // C(2,2)
        popParIn[7]  = C[2];  // C(3,1)
        popParIn[8]  = C[5];  // C(3,2)
        popParIn[9]  = C[8];  // C(3,3)

        popParLow[4] = popParIn[4]         * +0.001;
        popParLow[5] = fabs( popParIn[5] ) * -5.0;
        popParLow[6] = popParIn[6]         * +0.001;
        popParLow[7] = fabs( popParIn[7] ) * -5.0;
        popParLow[8] = fabs( popParIn[8] ) * -5.0;
        popParLow[9] = popParIn[9]         * +0.001;

        popParUp[4]  = popParIn[4]         * +10.0;
        popParUp[5]  = fabs( popParIn[5] ) * +5.0;
        popParUp[6]  = popParIn[6]         * +10.0;
        popParUp[7]  = fabs( popParIn[7] ) * +5.0;
        popParUp[8]  = fabs( popParIn[8] ) * +5.0;
        popParUp[9]  = popParIn[9]         * +10.0;
    }
    else
    {
        popParIn[4]  = omegaInFull[0];
        popParIn[5]  = omegaInFull[4];
        popParIn[6]  = omegaInFull[8];

        popParLow[4] = popParIn[4] * 1.0e-3;
        popParLow[5] = popParIn[5] * 1.0e-3;
        popParLow[6] = popParIn[6] * 1.0e-3;

        popParUp[4]  = popParIn[4] * 10.0;
        popParUp[5]  = popParIn[5] * 10.0;
        popParUp[6]  = popParIn[6] * 10.0;
    }

    //
    // Set the step sizes for all of the population parameters (fixed effects). 
    //
    for(i = 0; i < nPopPar; i++)
    {
      popParStep[i] = (popParUp[i] - popParLow[i]) / 1000.0;
    }

    //--------------------------------------------------------------------
    //
    // Intialize indParIn(bIn), indParLow(bLow), indParUp(bUp)
    //
    // b(1) : random effects for mass transfer rate 
    //        from absorption to sample compartment
    // b(2) : random effects for mass transfer rate
    //        from sample to outside
    // b(3) : random effects for weight normalized clearance
    //        (this value could be much larger in magunitude
    //         relative to the other random effects because
    //         it is weighted by weight)
    //--------------------------------------------------------------------
    valarray<double> indParIn    ( 0.0, nIndPar * nInds );  // all zeros
    valarray<double> indParLow   ( nIndPar );
    valarray<double> indParUp    ( nIndPar );
    valarray<double> indParStep  ( nIndPar );

    indParLow[0] = etaLow[0];
    indParLow[1] = etaLow[1];
    indParLow[2] = etaLow[2];

    indParIn [0] = etaIn[0];
    indParIn [1] = etaIn[1];
    indParIn [2] = etaIn[2];

    indParUp [0] = etaUp[0];
    indParUp [1] = etaUp[1];
    indParUp [2] = etaUp[2];

    //
    // Use the same step sizes for the population parameters (fixed effects)
    // and for the individual parameters (random effects).
    //
    for(i = 0; i < nIndPar; i++)
    {
        indParStep[i] = popParStep[i];
    }

    //--------------------------------------------------------------------
    //
    // Set optimizer controls
    //
    //--------------------------------------------------------------------
    //
    // Optimizer controls for individual level optimization
    //
    Optimizer indOptimizer( indEpsilon, indMaxIters, indPrintLevel );

    //
    // Optimizer controls for population level optimization
    //
    Optimizer popOptimizer( popEpsilon, popMaxIters, popPrintLevel );

    //--------------------------------------------------------------------
    //
    // Final preparation before calling fitPopulation
    //
    //--------------------------------------------------------------------
    //
    // Output values that should be calculated.
    //
    valarray<double> popParOut    ( nPopPar );
    valarray<double> indParOut    ( nIndPar * nInds );
	valarray<double> LTilde_aOut  ( nPopPar );
    valarray<double> LTilde_a_aOut( nPopPar*nPopPar );
    double LTildeOut;

    cout << "--------------------------------------------------------------------" << endl;
    cout << "   " << "< ";

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

    cout << " > ";
#ifdef NDEBUG
    cout << "Release";
#else
    cout << "Debug";
#endif
    cout << " version" << endl;
    cout << "--------------------------------------------------------------------" << endl;

    
    //--------------------------------------------------------------------
    //
    // Optimize the parametric population objective function.
    //
    //--------------------------------------------------------------------
    //
    // Variables needed to keep dates and other extra information.
    // These are for debugging/timing.
    //
    time_t began;
    time_t finished;
    double duration_sec = 0.0;
    began = clock();
    try{
        status.isParamEstimateAttempted = true;
        fitPopulation(
                       model, 
                       objective, 
                       model._N, 
                       data, 
                       popOptimizer,
                       popParLow, 
                       popParUp, 
                       popParIn,
                       popParStep,
                       &popParOut, 
                       indOptimizer,
                       indParLow, 
                       indParUp, 
                       indParIn,
                       indParStep,
                       &indParOut, 
                       &LTildeOut, 
                       &LTilde_aOut, 
                       &LTilde_a_aOut
                       );
      status.isParamEstimateSucceeded = true;

    }
    catch(const SpkException& e)
    {
        message += e;
        cerr << e << endl;
    }
    catch( const std::exception& ste)
    {
      SpkException e( ste, "fitPopulation() failed.", __LINE__, __FILE__ );
      message += e;
      cerr << e << endl;
    }
    catch( ... )
    {
      SpkException e( SpkError::SPK_UNKNOWN_OPT_ERR, "fitPopulation() failed.", __LINE__, __FILE__ );
      message += e;
      cerr << e << endl;
    }

    //------------------------------------------------------------
    // Compute statistics of population parameter estimates.
    //------------------------------------------------------------
    valarray<double> popParCovOut   ( nPopPar * nPopPar );
    valarray<double> popParSeOut    ( nPopPar );
    valarray<double> popParCorOut   ( nPopPar * nPopPar );
    valarray<double> popParCovNonmem( nPopPar * nPopPar );
    valarray<double> popParSeNonmem ( nPopPar );
    valarray<double> popParCorNonmem( nPopPar * nPopPar );
    valarray<bool>   popParMask     ( true, nPopPar );
    if( status.isParamEstimateSucceeded )
    {
      try
      {
        status.isStdErrorAttempted = true;
        popStatistics(
              model,
              objective,
              model._N,
              data,
              popParOut,
              popParMask,
              LTilde_a_aOut,
              indParOut,
              indParLow,
              indParUp,        
              indParStep,
              stat_form,
              &popParCovOut,
              &popParSeOut,
              &popParCorOut,
              NULL,
              NULL
              );

		// Calculate accuracy of the optimization | Hessian^-1 * Gradient |.
        double accuracy = 0.0;
		valarray<double> p = multiply( popParCovOut, nPopPar, LTilde_aOut, 1 );
		for( int k = 0; k < nPopPar; k++ )
		{
		  accuracy += p[ k ] * p[ k ];
		}
		accuracy = sqrt( accuracy );
        cout << "Fitting accuracy indicator = " << accuracy << endl;

		// Convert Spk statistics to Nonmem statistics
        convertStatisticsToNonmem(
                             omegastruc == DiffEqnModel::DIAGONAL, 
                             exponential,
                             popParOut,
                             popParCovOut,
							 popParSeOut,
							 popParCorOut,
                             popParCovNonmem,
                             popParSeNonmem,
                             popParCorNonmem
                             );

        status.isStdErrorSucceeded = true;
      }
      catch(const SpkException& e)
      {
          message += e;
          cerr << e << endl;
      }
      catch( const std::exception& ste)
      {
        SpkException e( ste, "fitPopulation() failed.", __LINE__, __FILE__ );
        message += e;
        cerr << e << endl;
      }
      catch( ... )
      {
        SpkException e( SpkError::SPK_UNKNOWN_OPT_ERR, "fitPopulation() failed.", __LINE__, __FILE__ );
        message += e;
        cerr << e << endl;
      }
    }

    finished = clock();
    duration_sec = (double)(finished - began)/CLOCKS_PER_SEC;

    //------------------------------------------------------------
    // Convert the SPK values to their NONMEM equivalents.
    //------------------------------------------------------------

    // SPK's objective differs from NONMEM's by a constant.
    int nYTotal = data.size();
    double equivNonmemObj = spkToNonmemObj( LTildeOut, nYTotal );

    // The first nIndPar elemements of this model's population parameter
    // (fixed effects) vector are equivalent to NONMEM's theta vector.
    valarray<double> thetaSpk( nIndPar );
    for(int k = 0; k < nIndPar; k++)
    {
        thetaSpk[k] = popParOut[k];
    }

    // SPK's individual parameters (random effects) covariance
    // is equivalent to NONMEM'S OMEGA matrix.
    valarray<double> D = DiffEqnModel::Dval(nIndPar, popParOut, omegastruc);

    // The common variance factor for this model's individual data
    // covariance is equivalent to a 1 by 1 NONMEM SIGMA matrix.
    valarray<double> sigmaSpk( 1 );
    if( exponential )
        sigmaSpk[0] = exp( popParOut[3] );
    else
        sigmaSpk[0] = popParOut[3];

    //------------------------------------------------------------
    // Display the results.
    //------------------------------------------------------------
    cout << "--------------------------------------------------------------------" << endl;
    cout << "   " << duration_sec << " seconds." << endl;
    cout << "--------------------------------------------------------------------" << endl;

    cout << "Objective  = " << equivNonmemObj << endl;
    cout << "Theta(1)   = " << popParOut[0] << endl;
    cout << "Theta(2)   = " << popParOut[1] << endl;
    cout << "Theta(3)   = " << popParOut[2] << endl;
    cout << "Omega(1,1) = " << D[0] << endl;
    if( omegastruc == DiffEqnModel::BLOCK ) cout << "Omega(2,1) = " << D[1] << endl;
    cout << "Omega(2,2) = " << D[4] << endl;
    if( omegastruc == DiffEqnModel::BLOCK ) cout << "Omega(3,1) = " << D[6] << endl;
    if( omegastruc == DiffEqnModel::BLOCK ) cout << "Omega(3,2) = " << D[7] << endl;
    cout << "Omega(3,3) = " << D[8] << endl;
    cout << "Sigma(1,1) = " << sigmaSpk[0] << endl;

    //------------------------------------------------------------
    // Write the results.to the output file
    //------------------------------------------------------------
    ofstream ofs( outfile.c_str() );
    ofs << "Objective  = " << equivNonmemObj << endl;
    ofs << "Theta(1)   = " << popParOut[0] << endl;
    ofs << "Theta(2)   = " << popParOut[1] << endl;
    ofs << "Theta(3)   = " << popParOut[2] << endl;
    ofs << "Omega(1,1) = " << D[0] << endl;
    if( omegastruc == DiffEqnModel::BLOCK ) ofs << "Omega(2,1) = " << D[1] << endl;
      ofs << "Omega(2,2) = " << D[4] << endl;
    if( omegastruc == DiffEqnModel::BLOCK ) ofs << "Omega(3,1) = " << D[6] << endl;
    if( omegastruc == DiffEqnModel::BLOCK ) ofs << "Omega(3,2) = " << D[7] << endl;
    ofs << "Omega(3,3) = " << D[8] << endl;
    ofs << "Sigma(1,1) = " << sigmaSpk[0] << endl;
    ofs << "-----------------------------------------------------------------" << endl;
    ofs << "Spk Covariance Matrix = " << endl;
    printInMatrix( popParCovNonmem, nPopPar, ofs );
    ofs << "-----------------------------------------------------------------" << endl;
    ofs << "Spk standard error vector = " << endl;
    printInMatrix( popParSeNonmem, nPopPar, ofs );
    ofs << "-----------------------------------------------------------------" << endl;
    ofs << "Spk Correlation matrix = " << endl;
    printInMatrix( popParCorNonmem, nPopPar, ofs );
    ofs << "-----------------------------------------------------------------" << endl;
    ofs.close();
    cout << endl;
    cout << "... Estimation: " << (status.isParamEstimateSucceeded? "successful" : "failed") << endl;
    cout << "... Standard error computation: " << (status.isStdErrorSucceeded? "successful" : "failed") << endl;
    cout << "... produced " << outfile << " in the current directory !!!" << endl;

    //------------------------------------------------------------
    // Write the results.to an XML output file
    //------------------------------------------------------------
    Source source( "1.0 beta 1", "DiffEqnModelTest" );
    
    PopEpsilon epsilon( popEpsilon );

    NonmemObjective nonmemObjective( 1.0e-2, objective, equivNonmemObj );
    
    const int nTheta = 3;
    Theta theta( nTheta, 1.0e-2 );
    theta.parameterization = DIRECT;
    theta.in     = thetaIn;
    theta.up     = thetaUp;
    theta.low    = thetaLow;
    for( i=0; i<nTheta; i++ )
    {
      theta.out[i]      = popParOut[i];
      theta.stdError[i] = popParSeNonmem[i];
    }
    
    const int nOmega = 3;
    Omega *omega;
    if( omegastruc == DiffEqnModel::BLOCK )
    {
      omega = new Omega( nOmega, Omega::FULL, 1.0e-2 );
      omega->parameterization = INDIRECT;
      omega->in       = omegaInFull;
      omega->out      = D;
      // Switch elements to match Nonmem's covariance order
      double temp = popParSeNonmem[ 5 ];
      popParSeNonmem[ 5 ] = popParSeNonmem[ 6 ];
      popParSeNonmem[ 6 ] = temp;
      for( j=0, cnt=3; j<nOmega; j++ )
      {
        for( i=0; i<nOmega; i++ )
        {
          if( i <= j )
          {
            omega->stdError[ i + j*nOmega ] = omega->stdError[ i*nOmega + j ] = popParSeNonmem[ cnt ];
            cnt++;
          }
          else
            omega->stdError[ i + j*nOmega ] = 0;
        }
      }
    }
    else
    {
      omega = new Omega( nOmega, Omega::DIAGONAL, 1.0e-2 );
      omega->parameterization = DIRECT;
      for( j=0; j<nOmega; j++ )
      {
        omega->low[ j ] = popParLow[ j + 4 ];
        omega->up[ j ]  = popParUp[ j + 4 ];
      }
      omega->in       = omegaInFull;
      omega->out      = D;
      for( j=0; j<nOmega; j++ )
      {
        omega->stdError[ j ] = popParSeNonmem[ j + 3 ];
      }
    }

    Sigma sigma( 1, Sigma::DIAGONAL, 1.0e-2 );
    sigma.parameterization = INDIRECT;
    sigma.in       = exp( popParIn[3] );
    sigma.low      = exp( popParLow[3] );
    sigma.up       = exp( popParUp[3] );
    sigma.out      = exp( popParOut[3] );
    sigma.stdError = popParSeNonmem[ nPopPar - 1 ];

    CovarianceOfEstimate covPopPar( nPopPar );
    covPopPar.parameterization[ slice( 0, 3, 1 ) ]         = true;
    covPopPar.parameterization[ slice( 3, nPopPar-3, 1 ) ] = false;
    covPopPar.out = popParCovNonmem;

    CorrelationOfEstimate corPopPar( nPopPar );
    corPopPar.parameterization[ slice( 0, 3, 1 ) ]         = true;
    corPopPar.parameterization[ slice( 3, nPopPar-3, 1 ) ] = false;
    corPopPar.out = popParCorNonmem;

    Eta eta( nIndPar, nInds, 1.0e-2 );
    eta.parameterization = DIRECT;
    eta.in  = etaIn;
    //eta.low = etaLow;
    //eta.up  = etaUp;
    eta.out = transpose( indParOut, nInds );

    Prediction pred( model._N, 1.0e-2 );
    valarray<double> fiOut;
    valarray<double> fOut( -1.0, model._N.sum() );
    try{
      model.setPopPar( popParOut );
      for( i=0, cnt=0; i<nInds; i++ )
      {
        model.selectIndividual( i );
        model.setIndPar( indParOut[ slice( nIndPar*i, nIndPar, 1 ) ] );
        model.dataMean( fiOut );
        fOut[ slice( cnt, model._N[i], 1 ) ] = fiOut;
        cnt += model._N[i];
      }
    }
    catch( const SpkException& e )
    {
      message += e;
    }
    pred.out = fOut;

    string xmlout = outfile + ".xml";
    ofstream oxml( xmlout.c_str() );
    oxml << "<?xml version = \"1.0\"?>" << endl;
    oxml << "<spkTestResults>" << endl;
    oxml << source.xml()                                                              << endl;
    oxml << status.xml()                                                              << endl;
    oxml << message.xml()                                                             << endl;
    oxml << epsilon.xml()                                                             << endl;
    oxml << nonmemObjective.xml(status.isParamEstimateAttempted)                      << endl;
    oxml << theta.xml( status.isParamEstimateSucceeded, status.isStdErrorSucceeded )  << endl;
    oxml << omega->xml( status.isParamEstimateSucceeded, status.isStdErrorSucceeded ) << endl;
    oxml << sigma.xml( status.isParamEstimateSucceeded, status.isStdErrorSucceeded )  << endl;
    oxml << covPopPar.xml( status.isStdErrorSucceeded )   << endl;
    oxml << corPopPar.xml( status.isStdErrorSucceeded )   << endl;
    oxml << eta.xml( status.isParamEstimateSucceeded ) << endl;
    oxml << pred.xml( status.isParamEstimateSucceeded ) << endl;
    oxml << "</spkTestResults>" << endl;
    oxml.close();
    cout << endl;
    cout << "... produced " << outfile + ".xml" << " in the current directory !!!" << endl;


    delete omega;
    fp.check( __LINE__, __FILE__ );
    return SUCCESS;
}
