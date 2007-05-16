/*************************************************************************
 *//**
 * @file checkParamIdent.cpp
 * 
 * 
 * Implements checkParamIdent() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// Identifiability header files.
#include "calcGroebnerBasis.h"
#include "checkParamIdent.h"
#include "IdentException.h"

// Standard library header files.
#include <cmath>
#include <sstream>
#include <string>
#include <vector>

// CMP big number library header files.
#include <gmp.h>

// GiNaC computer algebra library header files.
#include <ginac/ginac.h>


/*------------------------------------------------------------------------
 * Local functions
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  /***********************************************************************
   *
   * Function: defaultNumericPrintFuncToPrintDoublesAsIntegers
   *
   * This function prints GiNaC real, non-integer numeric values as
   * integers.
   *
   * It is intended to be used with GiNaC's default output format
   * (print_dflt).
   *
  /***********************************************************************/

  void defaultNumericPrintFuncToPrintDoublesAsIntegers(
    const GiNaC::numeric&     x,
    const GiNaC::print_dflt&  printContext,
    unsigned                  parentOperatorLevel )
  { 
    using namespace GiNaC;
    
    // Print integers and complex numbers in the normal way.
    if ( x.is_integer() || !x.is_real() )
    {
      x.print( printContext.s );
    }
    else
    {
      // Print real values enclosed in paranthesis.
      printContext.s << "(";
    
      // Get a double versions of x and use standard math
      // functions to get its exponent and mantissa.
      double xDouble = x.to_double();
    
      // Print the value.
      if ( xDouble == 0.0 )
      {
        // Handle the case where the value is equal to zero.
        printContext.s << "0";
      }
      else
      {
        // Handle the case where the value is negative.
        if ( xDouble < 0.0 )
        {
          printContext.s << "-";
          xDouble = -1.0 * xDouble;
        }
    
        // Calculate the exponent and mantissa such that
        //
        //                        exponent
        //     x  =  mantissa * 10          .
        //
        int    exponent = static_cast<int>( floor( log10( xDouble ) ) );
        double mantissa = xDouble / std::pow( 10.0, exponent );
    
        // Calculate a new exponent and mantissa such that
        //
        //                        exponent
        //     x  =  mantissa * 10
        //
        //            -                      -
        //           |               nDigits  |     (exponent - nDigits)
        //        =  |  mantissa * 10         | * 10                      .
        //            -                      -
        //
        //                           newExponent
        //        =  newMantissa * 10             .
        //
        int nDigits = 4;
        int newExponent = exponent - nDigits;

        // Note that the first value for the new mantissa has been
        // multiplied by an extra factor of ten so that it can be
        // rounded properly before the extra factor of ten is removed.
        int newMantissa = static_cast<int>( mantissa * std::pow( 10.0, nDigits + 1 ) );
        newMantissa = ( newMantissa + 5 ) / 10;
    
        printContext.s << newMantissa;

        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // [Revisit - Only 5 Digits of Mantissa Printed for Doubles - Mitch]
        // For now, only print the the first five digits of the mantissa
        // because the BLAD library gives a syntax error if one of the
        // polynomials is multipled by a number like this
        //
        //     (11800 * 10^(2))
        //
        // in a system-experiment model like this
        //
        //     [A1[T] - ( (11800 * 10^(2))*U+THETA2*A2-THETA1*A1-A1*THETA3 ) / ( 1 ),
        //     A2[T] - ( -THETA2*A2+A1*THETA3 ) / ( 1 ),
        //     Y - ( A1 ) / ( THETA4 ), 
        //     THETA1[T], THETA2[T], THETA3[T], THETA4[T]]
        //
        // This commented out code prints out the new exponent
        // properly, but won't work with BLAD.
        /*
        printContext.s << " * 10^(";
        printContext.s << newExponent;
        printContext.s << ")";
        */
        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      }
    
      // Print the value enclosed in paranthesis.
      printContext.s << ")";
    }

  }

  /***********************************************************************
   *
   * Function: defaultNumericPrintFunc
   *
   * This function prints all GiNaC numeric values in their normal
   * default way.
   *
   * It is intended to be used with GiNaC's default output format
   * (print_dflt).
   *
  /***********************************************************************/

  void defaultNumericPrintFunc(
    const GiNaC::numeric&     x,
    const GiNaC::print_dflt&  printContext,
    unsigned                  parentOperatorLevel )
  { 
    using namespace GiNaC;
    
    // Print the numeric value in the normal way.
    x.print( printContext.s );
  }

} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: checkParamIdent
 *
 *//**
 * Attempts to determine the identifiability of the parameters
 * contained in the THETA vector using the system-experiment model
 * that is defined by the compartments' ordinary differential
 * equations and by the observation equations.
 *
 * See Audoly et al. (2001) for details on the identifiability
 * algorithm.
 *
 * This function attempts to calculate the Groebner basis or
 * bases that correspond to the system-experiment model differential
 * polyomial regular chain sysExpModelRegChain.
 * To be specific, this function attempts to calculate the Groebner
 * bases for the exhaustive summary polynomials that come from the
 * input/output relations' coefficients, which are evaluated at a
 * random value for the vector that will be determined to be
 * identifiable or not, THETA.
 *
 * Finally, this function attempts to solve each set of Groebner
 * basis equations, which are a nonlinear system of polynomials.
 *
 *
 * Reference:
 *
 * S. Audoly, G. Bella, L. D'Angio, M. P. Saccomani, and C. Cobelli,
 * "Global Identifiability of Nonlinear Models of Biological Systems,"
 * IEEE Transactions on Biomedical Engineering, Vol. 48, pp. 55 - 65,
 * January 2001.
 *
 * F. Boulier, "The bad library (version 1.7.2)", pp. 2-6, 2004.
 *
 *
 * @param level
 * 
 * If level = 0, then no intermediate information from the
 * identifiability calculation will be printed to standard output.
 * If level = 1, then the intermediate information will be printed.
 * 
 * 
 * @param nTheta
 * 
 * The number of elements in the THETA vector, which are the parameters
 * that will be checked to be identifiable or not.
 * 
 * 
 * @param thetaName
 *
 * This vector of strings contains the names for each element of the
 * THETA vector.
 *
 * Note that each element of THETA is allowed to have any name, not
 * just THETA1, THETA2, etc.
 *
 * For example, it could be that
 *
\verbatim
 
     thetaName = ( "K21", "K12", "V", "CL" )  .
 
\endverbatim
 *
 * or
 *
\verbatim
 
    thetaName = ( "THETA1", ... , "THETAR" )  ,
 
\endverbatim
 * where R = nTheta.
 *
 * 
 * @param thetaSeed
 * 
 * The value to use to seed the random number generator used to generate
 * the random value for THETA.
 * 
 * 
 * @param nIdentComp
 * 
 * The number of compartments that have ordinary differential equations
 * associated with them.
 * The masses in the compartments are functions of the time variable T
 * and are labeled A1, A2, ... , AP, where P = nIdentComp .
 *
 * 
 * @param nObservType
 * 
 * The number of observation types, i.e., the number of different data
 * streams that have measured values.
 * If there is more than one type of observation, then they will be
 * labeled Y1, Y2, ..., YV, in the differential polynomials that make
 * up the system-experiment model, where V = nObservType . If there is
 * only one observation type, then it will be simply labeled Y.
 *
 * 
 * @param nDoseType
 * 
 * The number of dose types, i.e., the number of different compartments
 * that will receive doses.
 * If there is more than one type of dose, then they will be labeled
 * U1, U2, ..., UQ , in the differential polynomials that make up the
 * system-experiment model, where Q = nDoseType . If there is only
 * dose type, then it will be simply labeled U.
 *
 * 
 * @param compOde
 * 
 * This vector of strings contains the ordinary differential equations
 * (ODE's) that are associated with each compartment.
 *
 * Some example compartment ODE strings are
\verbatim

    A1[T] = THETA2*A2 

\endverbatim
and
\verbatim

    A2[T] = THETA1*A1

\endverbatim
 * where
\verbatim
 
    thetaName = ( "THETA1", "THETA2" )  .
 
\endverbatim
 * 
 * 
 * @param observEqn
 * 
 * This vector of strings contains the equations that define each of
 * the observation types.
 *
 * An example observation equation string is
\verbatim

    Y1 = A1  .

\endverbatim
 * 
 * 
 * @param identStatus
 * 
 * On output from this function this string will contain a
 * description of the status of the identifiability calculation,
 * e.g. "Globally (Uniquely) Identifiable", "Locally (Nonuniquely )
 * Identifiable", "Nonidentifiable (Infinite Solutions)",
 * "Nonidentifiable (No Solutions)", or "Identifiability could not
 * be determined"
 * 
 * 
 * @return
 *
 * Returns the total number of solutions of the nonlinear system of
 * equations that make up all of the Groebner basis equations.
 *
 * If the number of solutions is equal to 0, then the
 * identifiability of the individual's THETA parameter could not be
 * determined.
 *
 * If the number of solutions is equal to 1, then the individual's
 * THETA parameter is globally (uniquely) identifiable.
 *
 * If the number of solutions is greater than 1, then the individual's
 * THETA parameter is locally (nonuniquely) identifiable.
 *
 * If the number of solutions is equal to -1, then the individual's
 * THETA parameter has an infinite number of solutions and is
 * nonidentifiable.
 *
 * If the number of solutions is equal to -2, then the individual's
 * THETA parameter had multiple Groebner bases.
 *
/*************************************************************************/

int checkParamIdent( int                                level,
                     int                                nTheta,
                     const std::vector< std::string >&  thetaName,
                     int                                thetaSeed,
                     int                                nIdentComp,
                     int                                nObservType,
                     int                                nDoseType,
                     const std::vector< std::string >&  compOde,
                     const std::vector< std::string >&  observEqn,
                     std::string&                       identStatus )
{
  //----------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------

  using namespace std;

  using namespace GiNaC;

  // Send the output to standard cout.
  ostream& outputStream = cout;

  // Replace the method for the default output of numeric values
  // with a version that prints double values as integers.
  set_print_func<numeric, print_dflt>( defaultNumericPrintFuncToPrintDoublesAsIntegers );


  //----------------------------------------------------------
  // Set the symbols for the compartment amounts.
  //----------------------------------------------------------

  std::ostringstream paramSymbol;

  // This list will contain all of the parameters that can appear on
  // the right hand sides of the compartments' ODE's and the
  // observation equations.
  GiNaC::lst allPar;

  // These will hold the values for each compartment amount.
  std::vector<GiNaC::ex> compAmount( nIdentComp );

  int p;

  // Because there are not vectors of parameters for the BLAD library,
  // set the symbols for the compartments' amounts to be 
  //
  //     A1, A2, ...
  //
  for ( p = 0; p < nIdentComp; p++ )
  {
    // Set this element's symbol.
    paramSymbol.str( "" );
    paramSymbol << "A" << p + 1;
    compAmount[p] = symbol( paramSymbol.str() );

    // Add this compartment amount to the list of all parameters.
    allPar.append( compAmount[p] );
  }


  //----------------------------------------------------------
  // Prepare the parameters that will be checked to be identifiable.
  //----------------------------------------------------------

  // This list will contain the parameters that will be checked to
  // be identifiable, i.e., the elements of theta.
  GiNaC::lst identPar;

  // These will hold the values for each element of theta.
  std::vector<GiNaC::ex> theta( nTheta );

  // These will hold the C style strings with the names for each
  // element of theta.
  //
  // Note that each element of theta is allowed to have any name, not
  // just THETA1, THETA2, etc.
  char** thetaNameCStr = new char*[ nTheta ];

  int r;

  // Set the symbols for the elements of the theta vector.
  for ( r = 0; r < nTheta; r++ )
  {
    // Set this element's symbol.
    theta[r] = symbol( thetaName[r] );

    // Add this theta element's symbol to the parameter lists.
    allPar.  append( theta[r] );
    identPar.append( theta[r] );

    // Allocate enough memory for this C style string, including the
    // terminating null character.
    thetaNameCStr[r] = new char( thetaName[r].length() + 1 );

    // Set this theta element's name.
    strcpy( thetaNameCStr[r], thetaName[r].c_str() );
  }


  //----------------------------------------------------------
  // Set the symbols for the doses.
  //----------------------------------------------------------

  int q;

  // This will contain all of the dose types.
  //
  // Each dose type is considered to be a known function of time.
  std::vector<GiNaC::ex> doseType( nDoseType );
 
  // Because there are not vectors of parameters for the BLAD library,
  // set the symbols for each element of the U vector.
  //
  // If there is more than one type of dose, then they will be
  // labeled
  //
  //     U1, U2, ..., UQ  ,
  //
  // in the differential polynomials that make up the
  // system-experiment model, where
  //
  //     Q = nDoseType  .
  //
  // If there is only dose type, then it will be simply labeled
  //
  //     U.
  //
  GiNaC::symbol doseTypeWithoutIndex( "U" );
  if ( nDoseType == 1 )
  {
    doseType[0] = doseTypeWithoutIndex;
  }
  else
  {
    for ( q = 0; q < nDoseType; q++ )
    {
      // Set this element's symbol.
      paramSymbol.str( "" );
      paramSymbol << "U" << q + 1;
      doseType[q] = symbol( paramSymbol.str() );
    }
  }

  // Add the dose types to the list of all parameters.
  for ( q = 0; q < nDoseType; q++ )
  {
    allPar.append( doseType[q] );
  }


  //----------------------------------------------------------
  // Prepare the differential polynomial for each ODE.
  //----------------------------------------------------------

  std::ostringstream odeLHS;

  // This regular chain will contain all of the differential
  // polynomials that describe the system-experiment model.
  std::ostringstream sysExpModelRegChain;

  if ( level > 0 )
  {
    outputStream << endl;
    outputStream << "Begin parameter identifiability calculation." << endl;
    outputStream << endl;
    outputStream << endl;
    outputStream << "System-experiment model = {" << endl;
    outputStream << endl;
  }

  sysExpModelRegChain << "[";

  // This will contain the left and right hand sides of the ODE's
  // string.
  std::string odeLHSStr;
  std::string odeRHSStr;

  // This will contain the right hand side of the ODE.
  GiNaC::ex odeRHS;

  int odeEqualSignPos;
  int odeLHSBegin;
  int odeRHSBegin;
  int odeRHSLength;

  // Prepare the differential polynomials that correspond to each of
  // the compartments.
  for ( p = 0; p < nIdentComp; p++ )
  {
    // Find the equal sign in this compartment's ODE string.
    odeEqualSignPos = compOde[p].find( "=", 0 );
    if ( odeEqualSignPos == string::npos )
    {
      std::ostringstream message;
      message << "Differential equation "
              << p + 1
              << " did not have an equal sign.";
      throw IdentException( message.str() );
    }

    // Set a string that contains the expected (BLAD notation format)
    // left hand side of this compartment's ODE,
    //
    //     An[T]  ,
    //
    // where n is the compartment number.
    odeLHS.str( "" );
    odeLHS << "A" << p + 1 << "[T]";
    odeLHSStr = odeLHS.str();

    // Check the left hand side of this compartment's ODE string to
    // see if it contains the proper derivative term.
    odeLHSBegin = compOde[p].find( odeLHSStr, 0 );
    if ( odeLHSBegin == string::npos )
    {
      std::ostringstream message;
      message << "The left hand side of differential equation "
              << p + 1
              << " was not "
              << odeLHSStr
              << ".";
      throw IdentException( message.str() );
    }

    //  Find the first character in the right hand side that is not a
    //  space character.
    odeRHSBegin = compOde[p].find_first_not_of( " ", odeEqualSignPos + 1 );

    // Get the string that contains the right hand side of this
    // compartment's ODE.
    odeRHSLength = compOde[p].length() - odeRHSBegin;
    odeRHSStr.assign( compOde[p], odeRHSBegin, odeRHSLength );

    // Set the expression that is equal to the right hand side of this
    // compartment's ODE.
    try
    {
      odeRHS = GiNaC::ex( odeRHSStr, allPar );
    }
    catch( exception& e )
    {
      std::ostringstream message;
      message << "Differential equation "
              << p + 1
              << " had the following problem: \n\n"
              << e.what()
              << "\n\n";
      throw IdentException( message.str() );
    }

    // Add this differential polynomial to the set of differential
    // polynomials that make up the regular chain.
    //
    // In order to eliminate the occurrences of values exponentiated
    // to the minus one (-1) power, which are used by GiNaC when
    // printing fractions and which are not accepted by BLAD, set this
    // differential polynomial using the normal form for the right
    // hand side of the equation.
    //
    // The normal form for a rational function is an equivalent rational
    // function of the form numerator/denominator, where the numerator
    // and denominator are coprime.
    //
    // Therefore, print the numerator and the denominator for the ODE
    // separated by the division operator (/).
    //
    // Note that the derivative of An(T) with respect to T is denoted
    // An[T] in BLAD notation.
    sysExpModelRegChain << "A" << p + 1 << "[T] - ( " 
                        << odeRHS.normal().numer() << " ) / ( "
                        << odeRHS.normal().denom() << " ), ";

    // Print this differential polynomial using the normal GiNaC format.
    if ( level > 0 )
    {
      outputStream << "A" << p + 1 << "[T] = " << odeRHS;
      if ( nObservType > 0 )
      {
        outputStream << "," << endl;
      }
      else
      {
        if ( p < nIdentComp - 1 )
        {
          outputStream << "," << endl;
        }
        else
        {
          outputStream << " }" << endl;
        }
      }
      outputStream << endl;
    }
  }


  //----------------------------------------------------------
  // Prepare the differential polynomial for each observation type.
  //----------------------------------------------------------

  std::ostringstream eqnLHS;

  // This will contain the left and right hand sides of the
  // observation equation's string.
  std::string eqnLHSStr;
  std::string eqnRHSStr;

  // This will contain the right hand side of the observation
  // equation.
  GiNaC::ex eqnRHS;

  int eqnEqualSignPos;
  int eqnLHSBegin;
  int eqnRHSBegin;
  int eqnRHSLength;

  int v;

  // Prepare the differential polynomials that correspond to each of
  // the observation types.
  for ( v = 0; v < nObservType; v++ )
  {
    // Find the equal sign in this observation equation's string.
    eqnEqualSignPos = observEqn[v].find( "=", 0 );
    if ( eqnEqualSignPos == string::npos )
    {
      std::ostringstream message;
      message << "Observation equation "
              << v + 1
              << " did not have an equal sign.";
      throw IdentException( message.str() );
    }

    // Set a string that contains the expected left hand side of this
    // observation type's equation.
    //
    // If there is more than one type of observation, then they will be
    // labeled
    //
    //     Y1, Y2, ..., YV,
    //
    // in the differential polynomials that make up the
    // system-experiment model, where
    //
    //     V = nObservType  .
    //
    // If there is only one observation type, then it will be simply
    // labeled
    //
    //     Y.
    //
    eqnLHS.str( "" );
    eqnLHS << "Y";
    if ( nObservType > 1 )
    {
      eqnLHS << v + 1;
    }
    eqnLHSStr = eqnLHS.str();

    // Check the left hand side of this observation equation's string
    // to see if it contains the proper term.
    eqnLHSBegin = observEqn[v].find( eqnLHSStr, 0 );
    if ( eqnLHSBegin == string::npos )
    {
      std::ostringstream message;
      message << "The left hand side of observation equation "
              << v + 1
              << " was not "
              << eqnLHSStr
              << ".";
      throw IdentException( message.str() );
    }

    //  Find the first character in the right hand side that is not a
    //  space character.
    eqnRHSBegin = observEqn[v].find_first_not_of( " ", eqnEqualSignPos + 1 );

    // Get the string that contains the right hand side of this
    // observation's equation.
    eqnRHSLength = observEqn[v].length() - eqnRHSBegin;
    eqnRHSStr.assign( observEqn[v], eqnRHSBegin, eqnRHSLength );

    // Set the expression that is equal to the right hand side of this
    // observation's equation.
    try
    {
      eqnRHS = GiNaC::ex( eqnRHSStr, allPar );
    }
    catch( exception& e )
    {
      std::ostringstream message;
      message << "Observation equation "
              << v + 1
              << " had the following problem: \n\n"
              << e.what()
              << "\n\n";
      throw IdentException( message.str() );
    }

    // Add this differential polynomial to the set of differential
    // polynomials that make up the regular chain.
    //
    // In order to eliminate the occurrences of values exponentiated to
    // the minus one (-1) power, which are used by GiNaC when printing
    // fractions and which are not accepted by BLAD, print this out
    // using the normal form for the right hand side of the equation.
    //
    // The normal form for a rational function is an equivalent rational
    // function of the form numerator/denominator, where the numerator
    // and denominator are coprime.
    //
    // Therefore, print the numerator and the denominator for this
    // observation type separated by the division operator (/).
    sysExpModelRegChain << eqnLHSStr;
    sysExpModelRegChain << " - ( "
                        << eqnRHS.normal().numer() << " ) / ( "
                        << eqnRHS.normal().denom() << " ), ";
  
    // Print this differential polynomial using the normal GiNaC format.
    if ( level > 0 )
    {
      outputStream << eqnLHSStr;
      outputStream << " = " << eqnRHS;
      if ( v < nObservType - 1 )
      {
        outputStream << "," << endl;
      }
      else
      {
        outputStream << " }" << endl;
      }
      outputStream << endl;
    }
  }


  //----------------------------------------------------------
  // Prepare the differential polynomial for each theta element.
  //----------------------------------------------------------

  // Prepare the differential polynomials that correspond to the time
  // derivatives of each of the elements of the theta vector.
  //
  // These time derivatives are all equal to zero because the theta
  // parameters are constant in time.
  for ( r = 0; r < nTheta; r++ )
  {
    // Add this differential polynomial to the set of differential
    // polynomials that make up the regular chain.
    //
    // Note that the derivative of THETAn(T) with respect to T is denoted
    // THETAn[T] in BLAD notation.
    //
    // Also, note that each element of THETA is allowed to have any
    // name, not just THETA1, THETA2, etc.
    sysExpModelRegChain << thetaName[r] << "[T]";

    if ( r < nTheta - 1 )
    {
      sysExpModelRegChain << ", ";
    }
  }
  sysExpModelRegChain << "]";


  //----------------------------------------------------------
  // Print some information about BLAD's notation.
  //----------------------------------------------------------

  if ( level > 0 )
  {
    outputStream << endl;
    outputStream << "Note:  the derivatives with respect to T are denoted as " << endl;
    outputStream << endl;
    outputStream << "A1[T] = d/dT A1(T) ,   A1[T,T] = d/dT [ d/dT  A1(T) ] ,  ..." << endl;
    outputStream << "A2[T] = d/dT A2(T) ,   A2[T,T] = d/dT [ d/dT  A2(T) ] ,  ..." << endl;
    outputStream << "     ...                      ...                        ..." << endl;
    outputStream << endl;
    outputStream << endl;
  }


  //----------------------------------------------------------
  // Prepare the natural ordering for the variables.
  //----------------------------------------------------------

  std::ostringstream naturalOrdering;

  // Prepare the natural ordering for the variables, which is the
  // variable order that makes the set of the differential polynomials
  // that were just created be a regular chain,
  //
  //   [[Y1, ... , YV, A1, ... , AP, U1, ... , UQ], [THETA1, ... , THETAR]]
  //
  // where
  //
  //     V = nObservType ,
  //     P = nIdentComp ,
  //     Q = nDoseType ,
  //     R = nTheta .
  //
  // If there is only one observation type, then it will be simply
  // labeled
  //
  //     Y.
  //
  // If there is only dose type, then it will be simply labeled
  //
  //     U.
  //
  // Note that each element of THETA is allowed to have any name, not
  // just THETA1, THETA2, etc.
  //
  naturalOrdering << "[[";

  // Add the variable names for the observation types.
  for ( v = 0; v < nObservType; v++ )
  {
    naturalOrdering << "Y";
    if ( nObservType > 1 )
    {
      naturalOrdering << v + 1;
    }
    naturalOrdering << ", ";
  }

  // Add the variable names for the compartments.
  for ( p = 0; p < nIdentComp; p++ )
  {
    naturalOrdering << "A" << p + 1;
    if ( p < nIdentComp - 1 )
    {
      naturalOrdering << ", ";
    }
  }
  if ( nDoseType > 0 )
  {
    naturalOrdering << ", ";
  }

  // Add the variable names for the dose types.
  for ( q = 0; q < nDoseType; q++ )
  {
    naturalOrdering << "U";
    if ( nDoseType > 1 )
    {
      naturalOrdering << q + 1;
    }
    if ( q < nDoseType - 1 )
    {
      naturalOrdering << ", ";
    }
  }

  naturalOrdering << "], [";

  // Add the variable names for each theta element.
  for ( r = 0; r < nTheta; r++ )
  {
    naturalOrdering << thetaName[r];

    if ( r < nTheta - 1 )
    {
      naturalOrdering << ", ";
    }
  }

  naturalOrdering << "]]";


  //----------------------------------------------------------
  // Prepare the variable ordering for the characteristic set.
  //----------------------------------------------------------

  std::ostringstream charSetOrdering;

  // Prepare the characteristic set ordering for the variables, which
  // is the variable order that eliminates the compartment amounts
  // from the set of differential polynomials and leaves the
  // characteristic set,
  //
  //   [[A1, ... , AP], [Y1, ... , YV, U1, ... , UQ], [THETA1, ..., THETAR]]
  //
  // where
  //
  //     V = nObservType ,
  //     P = nIdentComp ,
  //     Q = nDoseType ,
  //     R = nTheta .
  //
  // If there is only one observation type, then it will be simply
  // labeled
  //
  //     Y.
  //
  // If there is only dose type, then it will be simply labeled
  //
  //     U.
  //
  // Note that each element of THETA is allowed to have any name, not
  // just THETA1, THETA2, etc.
  //
  charSetOrdering << "[[";

  // Add the variable names for the compartments.
  for ( p = 0; p < nIdentComp; p++ )
  {
    charSetOrdering << "A" << p + 1;
    if ( p < nIdentComp - 1 )
    {
      charSetOrdering << ", ";
    }
  }

  charSetOrdering << "], [";

  // Add the variable names for the observation types.
  for ( v = 0; v < nObservType; v++ )
  {
    charSetOrdering << "Y";
    if ( nObservType > 1 )
    {
      charSetOrdering << v + 1;
    }
    if ( v < nObservType - 1 )
    {
      charSetOrdering << ", ";
    }
  }
  if ( nDoseType > 0 )
  {
    charSetOrdering << ", ";
  }

  // Add the variable names for the dose types.
  for ( q = 0; q < nDoseType; q++ )
  {
    charSetOrdering << "U";
    if ( nDoseType > 1 )
    {
      charSetOrdering << q + 1;
    }
    if ( q < nDoseType - 1 )
    {
      charSetOrdering << ", ";
    }
  }

  charSetOrdering << "], [";

  // Add the variable names for each theta element.
  for ( r = 0; r < nTheta; r++ )
  {
    charSetOrdering << thetaName[r];

    if ( r < nTheta - 1 )
    {
      charSetOrdering << ", ";
    }
  }

  charSetOrdering << "]]";


  //----------------------------------------------------------
  // Calculate the exhaustive summary Groebner bases polynomials.
  //----------------------------------------------------------

  // Set C++ strings that contain the differential polynomials for the
  // system-experiment model and the orderings.
  std::string sysExpModelRegChainStr = sysExpModelRegChain.str();
  std::string naturalOrderingStr     = naturalOrdering .str();
  std::string charSetOrderingStr     = charSetOrdering .str();

  // Set C style strings for the differential polynomials and the
  // orderings.
  const char* sysExpModelRegChainCStr = sysExpModelRegChainStr.c_str();
  const char* naturalOrderingCStr     = naturalOrderingStr .c_str();
  const char* charSetOrderingCStr     = charSetOrderingStr .c_str();

  // This will be the number of Groebner bases that were found.
  int nGroebnerBasis;

  // This will be the number of polynomial for each of the Groebner
  // bases found.
  int* nGroebnerBasisPolyEachOut;

  // This will be the total number of polynomials for all of the
  // Groebner bases found.
  int nGroebnerBasisPolyTotalOut;

  // This pointer to a C style string will be used like an array of
  // C style strings that will contain the polynomials for all of
  // the Groebner bases after the call to calcGroebnerBasis().
  //
  // Note that calcGroebnerBasis() uses malloc() to allocate the
  // memory for the polynomials, which means that the allocated memory
  // must be freed by this function after it is no longer needed.
  char** groebnerBasisPolyAllCStrOut = 0;

  // Calculate the Groebner basis or bases for the exhaustive
  // summary.
  nGroebnerBasis = calcGroebnerBasis(
    level,
    nTheta,
    thetaNameCStr,
    thetaSeed,
    nIdentComp,
    nObservType,
    nDoseType,
    sysExpModelRegChainCStr,
    naturalOrderingCStr,
    charSetOrderingCStr,
    &nGroebnerBasisPolyEachOut,
    &nGroebnerBasisPolyTotalOut,
    &groebnerBasisPolyAllCStrOut );

  // Free this memory before any exceptions are thrown.
  for ( r = 0; r < nTheta; r++ )
  {
    delete thetaNameCStr[r];
  }
  delete[] thetaNameCStr;

  // If the Groebner bases could not be calculated, then this
  // calculation cannot continue.
  if ( nGroebnerBasis == 0 )
  {
    throw IdentException( "The parameter identifiability calculation failed because the Groebner basis for the \nexhaustive summary could not be determined." );
  }

  // Reset the default methods for the output of numeric values.
  set_print_func<numeric, print_dflt>( defaultNumericPrintFunc );


  //----------------------------------------------------------
  // Set the exhaustive summary Groebner bases equations.
  //----------------------------------------------------------

  std::string groebnerBasisEqnStr_m;

  GiNaC::ex  groebnerBasisEqn_m;
  GiNaC::lst groebnerBasisEqn;

  int m;

  // Set an equation for each of the Groebner bases polynomials
  //
  //     poly   =  0  
  //         m
  //
  // and free its C style string.
  for ( m = 0; m < nGroebnerBasisPolyTotalOut; m++ )
  {
    // Initially set the equation just equal to the polynomial.
    groebnerBasisEqnStr_m = std::string( groebnerBasisPolyAllCStrOut[m] );

    // Add the equality operator and the equations right-hand side to
    // get the full equation.
    groebnerBasisEqnStr_m += " == 0";

    // Set the expression containing the equation.    
    groebnerBasisEqn_m = GiNaC::ex( groebnerBasisEqnStr_m, identPar );
    
    // Add this equation to the list.
    groebnerBasisEqn.append( groebnerBasisEqn_m );

    // Free the memory for this polynomial's C style string.
    free( groebnerBasisPolyAllCStrOut[m] );
  }

  // Free the memory for pointers to the C style strings.
  free( groebnerBasisPolyAllCStrOut );

  // Free the memory for this C array.
  free( nGroebnerBasisPolyEachOut );


  //----------------------------------------------------------
  // Solve the Groebner bases equations.
  //----------------------------------------------------------

  // This will be the number of unique solutions of the Groebner
  // bases equations.
  int nGroebnerBasisSoln = 0;

  // This will be the number of equations for one of the unique
  // solutions of the Groebner bases equations.
  int nGroebnerBasisSoln_lEqn;

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Multiple Groebner Bases are not Currently Solved - Mitch]
  // Right now, this does not try to solve the Groebner bases if
  // there are more than one of them.
  //
  // Once we have decided upon the proper messaging and
  // interpretation of this situation, then this function should
  // probably try to solve each of the Groebner bases.
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  // See if multiple Groebner bases were calculated.
  if ( nGroebnerBasis > 1 )
  {
    // Set the value used to indicate that there were multiple
    // bases.
    nGroebnerBasisSoln = -2;

    // Set the status string.
    identStatus = "Multiple Groebner Bases - Identifiability not Determined (Possible Multiple Solutions)";

    if ( level > 0 )
    {
      outputStream << "This system-experiment model had multiple Groebner bases." << endl;
      outputStream << "This program cannot currently determine its identifiability." << endl;
      outputStream << endl;
    }

    // Return the number of unique solutions of the Groebner bases
    // equations.
    return nGroebnerBasisSoln;  
  }

  int l;
  int n;

  GiNaC::ex groebnerBasisSoln;

  // Try to solve the Groebner bases equations.
  try
  {
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - A Nonlinear System of Equations Solver is Needed - Mitch]
    // The Groebner bases equations should really be solved using a
    // nonlinear systems of equations solver.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //
    // Try to solve the Groebner bases equations as a linear system
    // of equations that are functions of the parameters that will
    // be checked to be identifiable.
    //
    // If the system of equations is nonlinear, then the linear
    // solver will throw an exception.
    groebnerBasisSoln = lsolve( groebnerBasisEqn, identPar );

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    // [Revisit - A Nonlinear System of Equations Solver is Needed - Mitch]
    // Once a nonlinear systems of equations solver is used there
    // can be multiple solutions and this number can be larger than
    // one
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //
    // If the linear solver is able to solve the equations, set the
    // number the equations equal to one to indicate there were not
    // multiple solutions.
    nGroebnerBasisSoln = 1;

    // Print the solution of the Groebner bases equations.
    if ( level > 0 )
    {
      outputStream << "Groebner basis solution = {" << endl;
      outputStream << endl;
      for ( l = 0; l < nGroebnerBasisSoln; l++ )
      {
        // Get the number of equations in this solution.
        nGroebnerBasisSoln_lEqn = groebnerBasisSoln.nops();

        // Print the equations in this solution.
        for ( n = 0; n < nGroebnerBasisSoln_lEqn; n++ )
        {
          outputStream << groebnerBasisSoln[n];
          if ( n < nGroebnerBasisSoln_lEqn - 1 )
          {
            outputStream << "," << endl;
          }
          else
          {
            outputStream << " }" << endl;
          }
          outputStream << endl;
        }
      }
      outputStream << endl;
    }

    // Reset the number of solutions if the number of Groebner basis
    // polynomials does not match the number paramters (THETA's).
    if ( groebnerBasisEqn.nops() == identPar.nops() )
    {
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      // [Revisit - A Nonlinear System of Equations Solver is Needed - Mitch]
      // Once a nonlinear systems of equations solver is used there
      // can be multiple solutions and this number can be larger than
      // one, which would make the status string be:  
      //
      //     identStatus = "Locally (Nonuniquely) Identifiable";
      //
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      //
      // If there are the same number of equations as unknowns, and
      // the equations are linear, then the solution is unique.
      nGroebnerBasisSoln = 1;

      // Set the status string.
      identStatus = "Globally (Uniquely) Identifiable";

      if ( level > 0 )
      {
        outputStream << "This system-experiment model is globally (uniquely) identifiable." << endl;
      }
    }
    else if ( groebnerBasisEqn.nops() < identPar.nops() )
    {
      // If there are less equations than unknowns, then there are
      // an infinite number of solutions.
      nGroebnerBasisSoln = -1;

      // Set the status string.
      identStatus = "Nonidentifiable (Infinite Solutions)";

      if ( level > 0 )
      {
        outputStream << "There are fewer polynomials in the Groebner basis (" 
                     << groebnerBasisEqn.nops() << ") than there are parameters (" 
                     << identPar.nops() << ")." << endl;
        outputStream << endl;
        outputStream << "This system-experiment model is nonidentifiable." << endl;
        outputStream << "Its Groebner basis has an infinite number of solutions." << endl;
      }
    }
    else
    {
      // If there are more equations than unknowns, then there are
      // no solutions.
      nGroebnerBasisSoln = 0;

      // Set the status string.
      identStatus = "Nonidentifiable (No Solutions)";

      if ( level > 0 )
      {
        outputStream << "There are more polynomials in the Groebner basis (" 
                     << groebnerBasisEqn.nops() << ") than there are parameters (" 
                     << identPar.nops() << ")." << endl;
        outputStream << endl;
        outputStream << "This system-experiment model is nonidentifiable." << endl;
        outputStream << "Its Groebner basis has no solutions." << endl;
      }
    }

    if ( level > 0 )
    {
      outputStream << endl;
    }

    // Return the number of unique solutions of the Groebner bases
    // equations.
    return nGroebnerBasisSoln;  
  }
  catch( const std::exception& stde )
  {
    if ( level > 0 )
    {
      std::string message = "The Groebner basis is not a linear system of polynomials.  \nThis program cannot currently solve such a nonlinear system. \n";
      message += stde.what();
      outputStream << message << endl;
      outputStream << endl;

      outputStream << "Try solving this Groebner basis by inspection or by using a nonlinear system \nof equations solver to determine if the system-experiment model is identifiable." << endl;
      outputStream << endl;
    }

    // Set the status string.
    identStatus = "Groebner Basis Nonlinear - Identifiability not Determined (Possible Multiple Solutions)";

    // Return a value of 0 to indicate that the identifiability
    // of the individual's THETA parameter could not be determined.
    return 0;  
  }
  catch( ... )
  {
    throw IdentException( "An unknown exception was thrown during the attempt to solve the Groebner basis as a \nlinear system of equations." );
  }

}


