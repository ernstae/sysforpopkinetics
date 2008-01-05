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
#include "calcExhaustSummary.h"
#include "checkParamIdent.h"
#include "IdentException.h"

// Standard library header files.
#include <cmath>
#include <cstdio>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

// CMP big number library header files.
#include <gmp.h>

// GiNaC computer algebra library header files.
#include <ginac/ginac.h>

// Xerces XML parser library header files.
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>

#if defined(XERCES_NEW_IOSTREAMS)
#include <iostream>
#else
#include <iostream.h>
#endif

XERCES_CPP_NAMESPACE_USE 
  

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
      // Get a double versions of x.
      double xDouble = x.to_double();

      // See if the value is negative.
      bool isNeg = false;
      if ( xDouble < 0.0 )
      {
        xDouble = -1.0 * xDouble;
        isNeg = true;
      }
    
      // Print the value.
      //
      // First handle the case where the value is an integer.
      if ( floor( xDouble ) == xDouble )
      {
        printContext.s << ( isNeg ? "-" : "" );
        printContext.s << static_cast<int>( floor( xDouble ) );
      }
      else
      {
        // Print the non-integer values enclosed in paranthesis.
        printContext.s << "(";
        printContext.s << ( isNeg ? "-" : "" );
    
        // Handle the case where the value is a multiple of 1/2.
        if ( floor( 2.0 * xDouble ) == ( 2.0 * xDouble ) )
        {
          printContext.s << static_cast<int>( floor( 2.0 * xDouble ) ) << "/2";
        }
        else
        {
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

          int i;

          // Print
          //
          //                     newExponent
          //     newMantissa * 10             .
          //
          if ( newExponent == 0 )
          {
            printContext.s << newMantissa;
          }
          else
          {
            std::string powersOfTenStr;

            for ( i = 0; i < abs( newExponent); i++)
            {
              if ( newExponent > 0 )
              {
                // Create a string equal to 10 raised to the
                // newExponent power,
                //
                //     *10*10 ... *10  .
                //
                // where there are newExponent 10's.
                powersOfTenStr += "*10";
              }
              else
              {
                // Create a string equal to 10 raised to the
                // negative newExponent power,
                //
                //     /10/10 ... /10
                //
                // where there are newExponent 10's.
                powersOfTenStr += "/10";
              }
            }

            printContext.s << newMantissa << powersOfTenStr;
          }
        }

        // Print the value enclosed in paranthesis.
        printContext.s << ")";
      }
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


  /*************************************************************************
   *
   * Function: getElementText
   *
   *//**
   * Returns the text from an XML element like this
   *
   *     <element>text</element>
   *
   * The argument pDOMElementRoot points to the root element of the XML 
   * file that has currently been parsed.
   *
  /*************************************************************************/

  std::string getElementText( 
    const DOMElement* pDOMElementRoot,
    const std::string& elementName )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;


    //----------------------------------------------------------
    // Find the element.
    //----------------------------------------------------------

    XMLCh tempXMLCh[100];

    // Set the element's name XML string.
    XMLString::transcode( elementName.c_str(), tempXMLCh, 99 );

    // Get a pointer to a list containing the elements that match the
    // element's name.
    DOMNodeList* pDOMNodeList = pDOMElementRoot->getElementsByTagName( tempXMLCh );

    // Check that there is at least one element.
    if ( pDOMNodeList->getLength() < 1 )
    {
      std::string message = "There was no " 
                         + elementName
                         + " element in the SINGULAR output file.";

      throw IdentException( message );
    }

    // Check that there is no more than one element.
    if ( pDOMNodeList->getLength() > 1 )
    {
      std::string message = "There was more than one " 
                         + elementName
                         + " element in the SINGULAR output file.";

      throw IdentException( message );
    }


    //----------------------------------------------------------
    // Get the element's text.
    //----------------------------------------------------------

    // Get a pointer to the element in the DOM document tree.
    DOMElement* pDOMELement = dynamic_cast<DOMElement*>( pDOMNodeList->item( 0 ) );

    // Get the element's text as an XML string.
    const XMLCh* nodeTextXMLCh = pDOMELement->getFirstChild()->getNodeValue();

    // Convert the element's text to be a C style string.
    char* nodeTextCStr =  XMLString::transcode( nodeTextXMLCh );

    // Set the element's text.
    std::string elementTextStrOut = nodeTextCStr;

    // Free the memory allocated for this C style string.
    XMLString::release( &nodeTextCStr );


    //----------------------------------------------------------
    // Finish up.
    //----------------------------------------------------------

    return elementTextStrOut;
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
 * Finally, this function attempts to solve the nonlinear systems of
 * polynomials that correspond to each of Groebner basis.
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
 * Returns the total number of solutions of the nonlinear systems of
 * polynomials that correspond to each of the Groebner bases.
 *
 * If the number of solutions is equal to 0, then the individual's
 * THETA parameter is not identifiable.
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
 * If the number of solutions is equal to -2, then the
 * system-experiment model is not algebraically observable.
 *
 * If the number of solutions is equal to -3, then the identifiability
 * of the individual's THETA parameter could not be determined.
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
  // Calculate the exhaustive summary polynomials.
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

  // This will be the number of polynomials in the exhaustive summary.
  int nExhaustSummPoly;

  // This pointer to a C style string will be used like an array of C
  // style strings that will contain the polynomials in the exhaustive
  // summary after the call to calcExhaustSummary().
  //
  // Note that calcExhaustSummary() uses malloc() to allocate the
  // memory for the polynomials, which means that the allocated memory
  // must be freed by this function after it is no longer needed.
  char** exhaustSummPolyCStrOut = 0;

  // Calculate the exhaustive summary polynomials.
  nExhaustSummPoly = calcExhaustSummary(
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
    &exhaustSummPolyCStrOut );

  // Free this memory before any exceptions are thrown.
  for ( r = 0; r < nTheta; r++ )
  {
    delete thetaNameCStr[r];
  }
  delete[] thetaNameCStr;

  // Reset the default methods for the output of numeric values.
  set_print_func<numeric, print_dflt>( defaultNumericPrintFunc );


  //----------------------------------------------------------
  // Handle the case where the model is not algebraically observable.
  //----------------------------------------------------------

  // If the system-experiment model is not algebraically observable,
  // then there is no need to solve its Groebner basis.
  if ( nExhaustSummPoly == -1 )
  {
    // Set the proper status string.
    identStatus = "Nonidentifiable (Not Algebraically Observable)";

    if ( level > 0 )
    {
      outputStream << "This system-experiment model is not algebraically" << endl;
      outputStream << "observable because their are derivatives of the" << endl;
      outputStream << "compartment amounts in its characteristic set." << endl;
    }

    // Return zero to indicate there were no solutions of the systems
    // of nonlinear polynomials for each of the Groebner bases.
    return -2;
  }


  //----------------------------------------------------------
  // Set the exhaustive summary polynomials.
  //----------------------------------------------------------

  // Each of these strings will contain one of the exhaustive summary
  // polynomials in the following format,
  //
  //     11*THETA1^2-32*THETA1-3
  //
  std::vector< std::string > exhaustSummPolyStr( nExhaustSummPoly );

  std::string exhaustSummPolyStr_m;

  int m;

  // Set each of the exhaustive summary polynomial strings and free
  // its C style string.
  for ( m = 0; m < nExhaustSummPoly; m++ )
  {
    // Get the next polynomial in the list.
    exhaustSummPolyStr_m = std::string( exhaustSummPolyCStrOut[m] );

    // Set this polynomial's string.
    exhaustSummPolyStr[m] += exhaustSummPolyStr_m;

    // Free the memory for this polynomial's C style string.
    free( exhaustSummPolyCStrOut[m] );
  }

  // Free the memory for pointers to the C style strings.
  free( exhaustSummPolyCStrOut );

  // If the exhaustive summary is empty, the model is not
  // identifiable.  There is no need to solve its Groebner basis.
  if ( nExhaustSummPoly == 0 )
  {
    // Set the proper status string.
    identStatus = "Nonidentifiable (No Solutions)";

    if ( level > 0 )
    {
      outputStream << "This system-experiment model is nonidentifiable." << endl;
      outputStream << "Its exhaustive summary is empty." << endl;
    }

    // Return zero to indicate there were no solutions of the systems
    // of nonlinear polynomials for each of the Groebner bases.
    return 0;
  }


  //----------------------------------------------------------
  // Prepare to run SINGULAR to calculate and solve the Groebner bases.
  //----------------------------------------------------------

  // Set the SINGULAR input and output file names.
  const string singularInputFileName  = "calc_and_solve_groebner_basis.sng";
  const string singularOutputFileName = "calc_and_solve_groebner_basis.xml";

  // Open the SINGULAR input file.
  ofstream singularInputStream;
  singularInputStream.open( singularInputFileName.c_str(), ios::trunc );
  if ( !singularInputStream )
  {
    std::string message = "The SINGULAR input file could not be opened.";

    throw IdentException( message );
  }

  //----------------------------------------------------------
  // Write the contents of the SINGULAR input file. 
  //----------------------------------------------------------

  singularInputStream << "//*****************************************************************" << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "// File:  " << singularInputFileName << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "// This SINGULAR script calculates the Groebner basis for the" << endl;
  singularInputStream << "// exhaustive summary specified below and then solves the system" << endl;
  singularInputStream << "// of nonlinear polynomials that make up the Groebner basis." << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "// The solutions for the Groebner basis are written to an XML file," << endl;
  singularInputStream << "// " << singularOutputFileName << "." << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "//*****************************************************************" << endl;
  singularInputStream << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << "// Local procedure definition." << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << endl;
  singularInputStream << "//*************************************************************" << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "// Procedure:  finishUp" << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "// Prints the final information to the XML output file and " << endl;
  singularInputStream << "// then closes the link associated with that file." << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "//*************************************************************" << endl;
  singularInputStream << endl;
  singularInputStream << "proc finishUp( link singularOutputFileName, int nExhaustSummGroebnerBasisSoln )" << endl;
  singularInputStream << "{" << endl;
  singularInputStream << "  write( singularOutputFileName, \"</groebner_basis_solution_details>\" );" << endl;
  singularInputStream << endl;
  singularInputStream << "  write( singularOutputFileName, \"<number_of_groebner_basis_solutions>\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, nExhaustSummGroebnerBasisSoln );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"</number_of_groebner_basis_solutions>\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"</singular_output>\" );" << endl;
  singularInputStream << endl;
  singularInputStream << "  // Close the link." << endl;
  singularInputStream << "  close( singularOutputFileName );" << endl;
  singularInputStream << "}" << endl;
  singularInputStream << endl;
  singularInputStream << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << "// Preliminaries." << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << endl;
  singularInputStream << "// Load SINGULAR libraries." << endl;
  singularInputStream << "LIB \"triang.lib\";" << endl;
  singularInputStream << "LIB \"solve.lib\";" << endl;
  singularInputStream << endl;
  singularInputStream << "// Set this flag so that monomials will not be printed in the short format;" << endl;
  singularInputStream << "short = 0;" << endl;
  singularInputStream << endl;
  singularInputStream << "// The output XML file will be accessed via an ASCII link that is writeable." << endl;
  singularInputStream << "link singularOutputFileName = \"ASCII:w " << singularOutputFileName << "\";" << endl;
  singularInputStream << endl;
  singularInputStream << "// The first call to write opens the link." << endl;
  singularInputStream << "write( singularOutputFileName, \"<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\" standalone=\\\"no\\\"?>\" );" << endl;
  singularInputStream << "write( singularOutputFileName, \"<singular_output>\" );" << endl;
  singularInputStream << "write( singularOutputFileName, \"<groebner_basis_solution_details>\" );" << endl;
  singularInputStream << endl;
  singularInputStream << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << "// Prepare the rings used in the calculation." << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << endl;
  singularInputStream << "// Declare a ring in the variables with lexicographical ordering." << endl;
  singularInputStream << "ring variableRing = 0,( ";
  for ( r = 0; r < nTheta; r++ )
  {
    singularInputStream << thetaName[r];
    if ( r < nTheta - 1 )
    {
      singularInputStream << ", ";
    }
  }
  singularInputStream << " ), lp;" << endl;
  singularInputStream << endl;
  singularInputStream << "// Declare another ring that will be used below." << endl;
  singularInputStream << "ring polySolnRing;" << endl;
  singularInputStream << endl;
  singularInputStream << "// Reset the current ring." << endl;
  singularInputStream << "setring variableRing;" << endl;
  singularInputStream << endl;
  singularInputStream << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << "// Calculate the Groebner basis for the exhaustive summary." << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << endl;
  singularInputStream << "// Set the exhaustive summary." << endl;
  singularInputStream << "ideal exhaustSummary = ";
  for ( m = 0; m < nExhaustSummPoly; m++ )
  {
    singularInputStream << exhaustSummPolyStr[m];
    if ( m < nExhaustSummPoly - 1 )
    {
      singularInputStream << ", ";
    }
  }
  singularInputStream << ";" << endl;
  singularInputStream << endl;
  singularInputStream << "// Get the number of polynomials in the exhaustive summary." << endl;
  singularInputStream << "int nExhaustSummPoly = size( exhaustSummary );" << endl;
  singularInputStream << endl;
  singularInputStream << "// This will be the number of polynomials in the exhaustive summary" << endl;
  singularInputStream << "// Groebner basis." << endl;
  singularInputStream << "int nExhaustSummGroebnerBasisPoly;" << endl;
  singularInputStream << endl;
  singularInputStream << "// Set the maximum calculation time equal to the number of seconds." << endl;
  singularInputStream << "// in 30 minutes." << endl;
  singularInputStream << "int maxTimeInSec = 30 * 60;" << endl;
  singularInputStream << endl;
  singularInputStream << "// Set the option for computing a reduced Groebner basis" << endl;
  singularInputStream << "option( redSB );" << endl;
  singularInputStream << endl;
  singularInputStream << "int nExhaustSummGroebnerBasisSoln;" << endl;
  singularInputStream << endl;
  singularInputStream << "// Calculate the Groebner basis for the exhaustive summary" << endl;
  singularInputStream << "// with a time limit for the calculation." << endl;
  singularInputStream << "ideal exhaustSummGroebnerBasis = groebner( exhaustSummary, maxTimeInSec );" << endl;
  singularInputStream << endl;
  singularInputStream << "// If the Groebner basis could not be calculated in the maximum number " << endl;
  singularInputStream << "// of seconds, then issue an error message." << endl;
  singularInputStream << "if ( exhaustSummGroebnerBasis == 0 || defined( groebner_error ) )" << endl;
  singularInputStream << "{" << endl;
  singularInputStream << "  write( singularOutputFileName, \"The Groebner basis for the exhaustive summary could not be calculated\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"in less than 30 minutes.\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << endl;
  singularInputStream << "  // Set the total number of exhaustive summary Groebner basis solutions equal to" << endl;
  singularInputStream << "  // -3 to indicate that the identifiability could not be determined." << endl;
  singularInputStream << "  nExhaustSummGroebnerBasisSoln = -3;" << endl;
  singularInputStream << endl;
  singularInputStream << "  // Print the final information to the output file and close it." << endl;
  singularInputStream << "  finishUp( singularOutputFileName, nExhaustSummGroebnerBasisSoln );" << endl;
  singularInputStream << endl;
  singularInputStream << "  exit;" << endl;
  singularInputStream << "}" << endl;
  singularInputStream << "else" << endl;
  singularInputStream << "{" << endl;
  singularInputStream << "  // Get the number of polynomials in the exhaustive summary" << endl;
  singularInputStream << "  // Groebner basis." << endl;
  singularInputStream << "  nExhaustSummGroebnerBasisPoly = size( exhaustSummGroebnerBasis );" << endl;
  singularInputStream << endl;
  singularInputStream << "  // If the exhaustive summary Groebner basis was empty," << endl;
  singularInputStream << "  // then issue an error message." << endl;
  singularInputStream << "  if ( nExhaustSummGroebnerBasisPoly == 0 )" << endl;
  singularInputStream << "  {" << endl;
  singularInputStream << "    write( singularOutputFileName, \"The Groebner basis for the exhaustive summary was empty.\" );" << endl;
  singularInputStream << "    write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << endl;
  singularInputStream << "    // Set the total number of exhaustive summary Groebner basis solutions equal to" << endl;
  singularInputStream << "    // 0 to indicate that there are no solutions." << endl;
  singularInputStream << "    nExhaustSummGroebnerBasisSoln = 0;" << endl;
  singularInputStream << endl;
  singularInputStream << "    // Print the final information to the output file and close it." << endl;
  singularInputStream << "    finishUp( singularOutputFileName, nExhaustSummGroebnerBasisSoln );" << endl;
  singularInputStream << endl;
  singularInputStream << "    exit;" << endl;
  singularInputStream << "  }" << endl;
  singularInputStream << "}" << endl;
  singularInputStream << endl;
  singularInputStream << "int j;" << endl;
  singularInputStream << endl;
  singularInputStream << "string polyString_j;" << endl;
  singularInputStream << endl;
  singularInputStream << "// Print all of the polynomials in the Groebner basis." << endl;
  singularInputStream << "write( singularOutputFileName, \"Groebner basis = {\" );" << endl;
  singularInputStream << "write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << "for ( j = 1; j <= nExhaustSummGroebnerBasisPoly; j = j + 1 )" << endl;
  singularInputStream << "{" << endl;
  singularInputStream << "  // Set this polynomial." << endl;
  singularInputStream << "  if ( j < nExhaustSummGroebnerBasisPoly )" << endl;
  singularInputStream << "  {" << endl;
  singularInputStream << "    polyString_j = string( exhaustSummGroebnerBasis[j] ) + \",\";" << endl;
  singularInputStream << "  }" << endl;
  singularInputStream << "  else" << endl;
  singularInputStream << "  {" << endl;
  singularInputStream << "    polyString_j = string( exhaustSummGroebnerBasis[j] ) + \" }\";" << endl;
  singularInputStream << "  }" << endl;
  singularInputStream << endl;
  singularInputStream << "  write( singularOutputFileName, polyString_j );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << "}" << endl;
  singularInputStream << "write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << endl;
  singularInputStream << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << "// Calculate the Groebner basis for the exhaustive summary Groebner basis." << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << endl;
  singularInputStream << "int k;" << endl;
  singularInputStream << "int m;" << endl;
  singularInputStream << endl;
  singularInputStream << "// Get the number of variables in the ring." << endl;
  singularInputStream << "int nVariable = nvars( variableRing );" << endl;
  singularInputStream << endl;
  singularInputStream << "ideal exhaustSummGroebnerBasisGroebnerBasis;" << endl;
  singularInputStream << endl;
  singularInputStream << "int nExhaustSummGroebnerBasisGroebnerBasisPoly;" << endl;
  singularInputStream << endl;
  singularInputStream << "// Set the option for computing a reduced Groebner basis" << endl;
  singularInputStream << "option( redSB );" << endl;
  singularInputStream << endl;
  singularInputStream << "// Calculate the Groebner basis for the ideal generated by the system" << endl;
  singularInputStream << "// of polynomials from the exhaustive summary Groebner basis with a " << endl;
  singularInputStream << "// time limit for the calculation." << endl;
  singularInputStream << "exhaustSummGroebnerBasisGroebnerBasis = groebner( exhaustSummGroebnerBasis, maxTimeInSec );" << endl;
  singularInputStream << endl;
  singularInputStream << "// If the Groebner basis for the exhaustive summary Groebner basis could not" << endl;
  singularInputStream << "// be calculated in the maximum number of seconds, then issue an error message." << endl;
  singularInputStream << "if ( exhaustSummGroebnerBasisGroebnerBasis == 0 || defined( groebner_error ) )" << endl;
  singularInputStream << "{" << endl;
  singularInputStream << "  write( singularOutputFileName, \"The Groebner basis for the exhaustive summary Groebner basis\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"could not be calculated in less than 30 minutes.\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << endl;
  singularInputStream << "  // Set the total number of exhaustive summary Groebner basis solutions equal to" << endl;
  singularInputStream << "  // -3 to indicate that the identifiability could not be determined." << endl;
  singularInputStream << "  nExhaustSummGroebnerBasisSoln = -3;" << endl;
  singularInputStream << endl;
  singularInputStream << "  // Print the final information to the output file and close it." << endl;
  singularInputStream << "  finishUp( singularOutputFileName, nExhaustSummGroebnerBasisSoln );" << endl;
  singularInputStream << endl;
  singularInputStream << "  exit;" << endl;
  singularInputStream << "}" << endl;
  singularInputStream << endl;
  singularInputStream << "//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << endl;
  singularInputStream << "// [Revisit - Groebner Basis Calculation with Time Limit Changes Result - Mitch]" << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "// Because the previous call to the function groebner with a time" << endl;
  singularInputStream << "// limit returns a Groebner basis that the function triangMH does think" << endl;
  singularInputStream << "// is a proper Groebner basis, repeat the call to groebner here without a" << endl;
  singularInputStream << "// time limit" << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "// It is fine to do the calculation without a time limit here, because" << endl;
  singularInputStream << "// to get here the first call to groebner must have taken less than 30" << endl;
  singularInputStream << "// minutes." << endl;
  singularInputStream << "//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "// Calculate the Groebner basis for the ideal generated by the system" << endl;
  singularInputStream << "// of polynomials from the exhaustive summary Groebner basis without " << endl;
  singularInputStream << "// a time limit for the calculation." << endl;
  singularInputStream << "exhaustSummGroebnerBasisGroebnerBasis = groebner( exhaustSummGroebnerBasis );" << endl;
  singularInputStream << endl;
  singularInputStream << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << "// Solve the exhaustive summary Groebner basis polynomials." << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << endl;
  singularInputStream << "list exhaustSummGroebnerBasisSolnPolyList;" << endl;
  singularInputStream << endl;
  singularInputStream << "int nExhaustSummGroebnerBasisSolnPoly_k;" << endl;
  singularInputStream << endl;
  singularInputStream << "string relnString_m;" << endl;
  singularInputStream << "string solnString_k_m;" << endl;
  singularInputStream << endl;
  singularInputStream << "// Get the number of polynomials in the exhaustive summary Groebner basis," << endl;
  singularInputStream << "// the number of polynomials in the Groebner basis for the exhaustive summary" << endl;
  singularInputStream << "// Groebner basis." << endl;
  singularInputStream << "nExhaustSummGroebnerBasisPoly              = size( exhaustSummGroebnerBasis );" << endl;
  singularInputStream << "nExhaustSummGroebnerBasisGroebnerBasisPoly = size( exhaustSummGroebnerBasisGroebnerBasis );" << endl;
  singularInputStream << endl;
  singularInputStream << "// Print the number of solutions for the exhaustive summary Groebner basis." << endl;
  singularInputStream << "if ( nVariable > nExhaustSummPoly )" << endl;
  singularInputStream << "{" << endl;
  singularInputStream << "  write( singularOutputFileName, \"The Groebner basis has infinite solutions because there are fewer \" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"polynomials in the exhaustive summary (\" + string( nExhaustSummPoly ) + \") than there are parameters (\" + string( nVariable ) + \").\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << "}" << endl;
  singularInputStream << "else" << endl;
  singularInputStream << "{" << endl;
  singularInputStream << "  if ( nVariable > nExhaustSummGroebnerBasisPoly )" << endl;
  singularInputStream << "  {" << endl;
  singularInputStream << "    write( singularOutputFileName, \"The Groebner basis has infinite solutions because there are fewer \" );" << endl;
  singularInputStream << "    write( singularOutputFileName, \"polynomials in the basis (\" + string( nExhaustSummGroebnerBasisPoly ) + \") than there are parameters (\" + string( nVariable ) + \").\" );" << endl;
  singularInputStream << "    write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << "  }" << endl;
  singularInputStream << "  else" << endl;
  singularInputStream << "  {" << endl;
  singularInputStream << "    if ( nVariable > nExhaustSummGroebnerBasisGroebnerBasisPoly )" << endl;
  singularInputStream << "    {" << endl;
  singularInputStream << "      write( singularOutputFileName, \"The Groebner basis has infinite solutions because there are fewer \" );" << endl;
  singularInputStream << "      write( singularOutputFileName, \"polynomials in its solution (\" + string( nExhaustSummGroebnerBasisGroebnerBasisPoly ) + \") than there are parameters (\" + string( nVariable ) + \").\" );" << endl;
  singularInputStream << "      write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << "    }" << endl;
  singularInputStream << "    else" << endl;
  singularInputStream << "    {" << endl;
  singularInputStream << "      if ( nExhaustSummGroebnerBasisPoly > nExhaustSummGroebnerBasisGroebnerBasisPoly )" << endl;
  singularInputStream << "      {" << endl;
  singularInputStream << "        write( singularOutputFileName, \"The Groebner basis has infinite solutions because there are fewer \" );" << endl;
  singularInputStream << "        write( singularOutputFileName, \"polynomials in its solution (\" + string( nExhaustSummGroebnerBasisGroebnerBasisPoly ) + \") than there are in the basis (\" + string( nExhaustSummGroebnerBasisPoly ) + \").\" );" << endl;
  singularInputStream << "        write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << "      }" << endl;
  singularInputStream << "    }" << endl;
  singularInputStream << "  }" << endl;
  singularInputStream << "}" << endl;
  singularInputStream << endl;
  singularInputStream << "// If there are more variables than polymials in the exhaustive summary" << endl;
  singularInputStream << "// or its Groebner basis (an underdetermined system), or if there are" << endl;
  singularInputStream << "// more polymials than exhaustive summary Groebner basis polynomials, or" << endl;
  singularInputStream << "// if there are more variables than exhaustive summary Groebner basis" << endl;
  singularInputStream << "// polynomials, then the numerical solution finder triang_solve won't be" << endl;
  singularInputStream << "// able to find a solution." << endl;
  singularInputStream << "//" << endl;
  singularInputStream << "// In this case, use the Groebner basis for the exhaustive summary Groebner" << endl;
  singularInputStream << "// basis as the parameter relationships." << endl;
  singularInputStream << "if ( nVariable                     > nExhaustSummPoly                           ||" << endl;
  singularInputStream << "     nVariable                     > nExhaustSummGroebnerBasisPoly              ||" << endl;
  singularInputStream << "     nExhaustSummGroebnerBasisPoly > nExhaustSummGroebnerBasisGroebnerBasisPoly ||" << endl;
  singularInputStream << "     nVariable                     > nExhaustSummGroebnerBasisGroebnerBasisPoly )" << endl;
  singularInputStream << "{" << endl;
  singularInputStream << "  write( singularOutputFileName, \"The following parameter relationships satisfy the Groebner basis\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"polynomials and may be useful to determine identifiable parameter\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"combinations.\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << endl;
  singularInputStream << "  write( singularOutputFileName, \"Parameter relationships = {\" );" << endl;
  singularInputStream << "  write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << endl;
  singularInputStream << "  // Print all of the polynomials in the Groebner basis for the exhaustive" << endl;
  singularInputStream << "  // summary Groebner basis." << endl;
  singularInputStream << "  for ( m = 1; m <= nExhaustSummGroebnerBasisGroebnerBasisPoly; m = m + 1 )" << endl;
  singularInputStream << "  {" << endl;
  singularInputStream << "    // Set an equation for this polynomial." << endl;
  singularInputStream << "    relnString_m =" << endl;
  singularInputStream << "      string( exhaustSummGroebnerBasisGroebnerBasis[m] - jet( exhaustSummGroebnerBasisGroebnerBasis[m], 0 ) )" << endl;
  singularInputStream << "      + \" = \"" << endl;
  singularInputStream << "      + string( - jet( exhaustSummGroebnerBasisGroebnerBasis[m], 0 ) );" << endl;
  singularInputStream << endl;
  singularInputStream << "    if ( m < nExhaustSummGroebnerBasisGroebnerBasisPoly )" << endl;
  singularInputStream << "    {" << endl;
  singularInputStream << "      relnString_m = relnString_m + \",\";" << endl;
  singularInputStream << "    }" << endl;
  singularInputStream << "    else" << endl;
  singularInputStream << "    {" << endl;
  singularInputStream << "      relnString_m = relnString_m + \" }\";" << endl;
  singularInputStream << "    }" << endl;
  singularInputStream << endl;
  singularInputStream << "    write( singularOutputFileName, relnString_m );" << endl;
  singularInputStream << "    write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << "  }" << endl;
  singularInputStream << endl;
  singularInputStream << "  // Set the total number of exhaustive summary Groebner basis solutions equal to" << endl;
  singularInputStream << "  // -1 to indicate this basis has infinite solutions." << endl;
  singularInputStream << "  nExhaustSummGroebnerBasisSoln = -1;" << endl;
  singularInputStream << "}" << endl;
  singularInputStream << "else" << endl;
  singularInputStream << "{" << endl;
  singularInputStream << "  // Solve the system of nonlinear polynomials that corresponds to the" << endl;
  singularInputStream << "  // the exhaustive summary Groebner basis." << endl;
  singularInputStream << "  //" << endl;
  singularInputStream << "  // Calculate a list of triangular systems of factorized polynomials," << endl;
  singularInputStream << "  // such that the disjoint union of their solutions equals the solution of" << endl;
  singularInputStream << "  // system of polynomials for the exhaustive summary Groebner basis." << endl;
  singularInputStream << "  exhaustSummGroebnerBasisSolnPolyList = triangMH( exhaustSummGroebnerBasisGroebnerBasis, 2 );" << endl;
  singularInputStream << "  " << endl;
  singularInputStream << "  // Get the number of solutions for the exhaustive summary" << endl;
  singularInputStream << "  // Groebner basis." << endl;
  singularInputStream << "  nExhaustSummGroebnerBasisSoln = size( exhaustSummGroebnerBasisSolnPolyList );" << endl;
  singularInputStream << endl;
  singularInputStream << "  if ( nExhaustSummGroebnerBasisSoln == 1 )" << endl;
  singularInputStream << "  {" << endl;
  singularInputStream << "    write( singularOutputFileName, \"The Groebner basis has 1 solution.\" );" << endl;
  singularInputStream << "  }" << endl;
  singularInputStream << "  else" << endl;
  singularInputStream << "  {" << endl;
  singularInputStream << "    write( singularOutputFileName, \"The Groebner basis has \" + string( nExhaustSummGroebnerBasisSoln ) + \" solutions.\" );" << endl;
  singularInputStream << "  }" << endl;
  singularInputStream << "  write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << endl;
  singularInputStream << "  // Solve the exhaustive summary Groebner basis polynomials and put the." << endl;
  singularInputStream << "  // solutions into a ring." << endl;
  singularInputStream << "  polySolnRing = triang_solve( exhaustSummGroebnerBasisSolnPolyList, 6 );" << endl;
  singularInputStream << "  setring polySolnRing;" << endl;
  singularInputStream << endl;
  singularInputStream << "  // Print the numeric solutions for the exhaustive summary Groebner basis." << endl;
  singularInputStream << "  for ( k = 1; k <= nExhaustSummGroebnerBasisSoln; k = k + 1 )" << endl;
  singularInputStream << "  {" << endl;
  singularInputStream << "    if ( nExhaustSummGroebnerBasisSoln == 1 )" << endl;
  singularInputStream << "    {" << endl;
  singularInputStream << "      write( singularOutputFileName, \"Groebner basis solution = {\" );" << endl;
  singularInputStream << "    }" << endl;
  singularInputStream << "    else" << endl;
  singularInputStream << "    {" << endl;
  singularInputStream << "      write( singularOutputFileName, \"Groebner basis solution \" + string( k ) + \" = {\" );" << endl;
  singularInputStream << "    }" << endl;
  singularInputStream << "    write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << endl;
  singularInputStream << "    // Set the number of polynomials in this solution." << endl;
  singularInputStream << "    nExhaustSummGroebnerBasisSolnPoly_k = size( rlist[k] );" << endl;
  singularInputStream << endl;
  singularInputStream << "    // Print all of the polynomials in this solution." << endl;
  singularInputStream << "    for ( m = 1; m <= nExhaustSummGroebnerBasisSolnPoly_k; m = m + 1 )" << endl;
  singularInputStream << "    {" << endl;
  singularInputStream << "      // Set an equation for this polynomial." << endl;
  singularInputStream << "      solnString_k_m =" << endl;
  singularInputStream << "        string( varstr( variableRing, m ) ) + \" = \"" << endl;
  singularInputStream << "        + string( repart( rlist[k][m] ) )" << endl;
  singularInputStream << "        + \" + \"" << endl;
  singularInputStream << "        + string( impart( rlist[k][m] ) )" << endl;
  singularInputStream << "        + \" * i\";" << endl;
  singularInputStream << endl;
  singularInputStream << "      if ( m < nExhaustSummGroebnerBasisSolnPoly_k )" << endl;
  singularInputStream << "      {" << endl;
  singularInputStream << "        solnString_k_m = solnString_k_m + \",\";" << endl;
  singularInputStream << "      }" << endl;
  singularInputStream << "      else" << endl;
  singularInputStream << "      {" << endl;
  singularInputStream << "        solnString_k_m = solnString_k_m + \" }\";" << endl;
  singularInputStream << "      }" << endl;
  singularInputStream << endl;
  singularInputStream << "      write( singularOutputFileName, solnString_k_m );" << endl;
  singularInputStream << "      write( singularOutputFileName, \"\" );" << endl;
  singularInputStream << "    }" << endl;
  singularInputStream << "  }" << endl;
  singularInputStream << "}" << endl;
  singularInputStream << endl;
  singularInputStream << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << "// Finish up." << endl;
  singularInputStream << "//------------------------------------------------------------" << endl;
  singularInputStream << endl;
  singularInputStream << "// Print the final information to the output file and close it." << endl;
  singularInputStream << "finishUp( singularOutputFileName, nExhaustSummGroebnerBasisSoln );" << endl;
  singularInputStream << endl;
  singularInputStream << "exit;" << endl;
  singularInputStream << endl;

  // Close the SINGULAR input file. 
  singularInputStream.close();


  //----------------------------------------------------------
  // Run SINGULAR to calculate and solve the Groebner bases.
  //----------------------------------------------------------

  // Delete any existing version of the SINGULAR output file.
  int removeReturnValue = remove( singularOutputFileName.c_str() );

  // Use the system command to make SINGULAR execute the input file
  // with all output suppressed, no startup banner, and no messages
  // when loading libraries.
  string systemCommand = "Singular --no-out --quiet " + singularInputFileName;
  system( systemCommand.c_str() );


  //----------------------------------------------------------
  // Prepare the XML parser.
  //----------------------------------------------------------

  // Initialize the Xerces XML parser.
  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch ( ... )
  {
    throw IdentException( "The SINGULAR output file parser could not be initialized." );
  }


  //----------------------------------------------------------
  // Read in the SINGULAR output file with the Groebner bases' solutions.
  //----------------------------------------------------------

  // Instantiate the XML parser.
  XercesDOMParser* pXMLParser = new XercesDOMParser();

  // Set some options for the XML parser.
  pXMLParser->setValidationScheme            ( XercesDOMParser::Val_Always );
  pXMLParser->setDoNamespaces                ( true );
  pXMLParser->setDoNamespaces                ( true );
  pXMLParser->setDoSchema                    ( true );
  pXMLParser->setValidationSchemaFullChecking( true );
  pXMLParser->setCreateEntityReferenceNodes  ( true );

  // Parse the XML SINGULAR output file specified by the first C
  // style string argument for this function.
  try
  {
    pXMLParser->parse( singularOutputFileName.c_str() );
  }
  catch ( ... )
  {
    throw IdentException( "The SINGULAR output file could not be parsed." );
  }


  //----------------------------------------------------------
  // Prepare to get the SINGULAR outputs for the Groebner bases solution.
  //----------------------------------------------------------

  DOMDocument* pDOMDocument;

  // Get the DOM document tree that contains all of the elements and
  // text nodes from the SINGULAR output file
  try
  {
    pDOMDocument = pXMLParser->getDocument();
  }
  catch ( ... )
  {
    throw IdentException( "The SINGULAR output information could not be processed." );
  }


  // Get the root (first) element of the DOM document tree.
  DOMElement* pDOMElementRoot = pDOMDocument->getDocumentElement();

  // Check that there are elements in the DOM document tree.
  if ( !pDOMElementRoot )
  {
    throw IdentException( "The SINGULAR output file was empty." );
  }

  XMLCh tempXMLCh[100];

  // Check the name of the root element.
  XMLString::transcode( "singular_output", tempXMLCh, 99 );
  if ( !XMLString::equals( pDOMElementRoot->getTagName(), tempXMLCh ) ) 
  {
    throw IdentException(
      "The first element of the SINGULAR output file had the wrong name." );
  }


  //---------------------------------------------------------
  // Get the Groebner bases solution details.
  //----------------------------------------------------------

  string groebner_basis_solution_detailsStr;

  const string groebner_basis_solution_detailsErrorStr =
    "The Groebner solution details could not be determined from the SINGULAR output file.";

  try
  {
    groebner_basis_solution_detailsStr = getElementText( pDOMElementRoot, "groebner_basis_solution_details" );
  }
  catch ( const IdentException& e )
  {
    throw IdentException( groebner_basis_solution_detailsErrorStr + "\n\n" + e.what() );
  }
  catch ( ... )
  {
    throw IdentException( groebner_basis_solution_detailsErrorStr + "\n\n" 
      + "The reason for this problem is unknown." );
  }

  // Print the details of the solution of the Groebner bases
  // polynomials.
  if ( level > 0 )
  {
    outputStream << groebner_basis_solution_detailsStr << endl;
    outputStream << endl;
  }


  //---------------------------------------------------------
  // Get the number of solutions for the Groebner bases.
  //----------------------------------------------------------

  string number_of_groebner_basis_solutionsStr;

  const string number_of_groebner_basis_solutionsErrorStr =
    "The number of solutions could not be determined from the SINGULAR output file.";

  try
  {
    number_of_groebner_basis_solutionsStr = getElementText( pDOMElementRoot, "number_of_groebner_basis_solutions" );
  }
  catch ( const IdentException& e )
  {
    throw IdentException( number_of_groebner_basis_solutionsErrorStr + "\n\n" + e.what() );
  }
  catch ( ... )
  {
    throw IdentException( number_of_groebner_basis_solutionsErrorStr + "\n\n" 
      + "The reason for this problem is unknown." );
  }

  // Get the number of solutions of the system of nonlinear
  // polynomials that corresponds to the Groebner basis for the
  // exhaustive summary.
  int nGroebnerBasisSoln = atoi( number_of_groebner_basis_solutionsStr.c_str() );

  // Set the proper status string based on the number of solutions.
  if ( nGroebnerBasisSoln == 1 )
  {
    // Set the status string.
    identStatus = "Globally (Uniquely) Identifiable";

    if ( level > 0 )
    {
      outputStream << "This system-experiment model is globally (uniquely) identifiable." << endl;
    }
  }
  else if ( nGroebnerBasisSoln > 1 )
  {
    // Set the status string.
    identStatus = "Locally (Nonuniquely) Identifiable";

    if ( level > 0 )
    {
      outputStream << "This system-experiment model is locally (nonuniquely) identifiable." << endl;
    }
  }
  else if ( nGroebnerBasisSoln == -1 )
  {
    // Set the status string.
    identStatus = "Nonidentifiable (Infinite Solutions)";

    if ( level > 0 )
    {
      outputStream << "This system-experiment model is nonidentifiable." << endl;
    }
  }
  else if ( nGroebnerBasisSoln == -3 )
  {
    // Set the status string.
    identStatus = "Identifiability Not Determined";

    if ( level > 0 )
    {
      outputStream << "The identifiability of this system-experiment model" << endl;
      outputStream << "could not be determined." << endl;
    }
  }
  else if ( nGroebnerBasisSoln == 0 )
  {
    // Set the status string.
    identStatus = "Nonidentifiable (No Solutions)";

    if ( level > 0 )
    {
      outputStream << "This system-experiment model is nonidentifiable." << endl;
      outputStream << "Its Groebner basis has no solutions." << endl;
    }
  }
  else
  {
    throw IdentException( "An unexpected number of Groebner bases solutions was returned." );
  }

  if ( level > 0 )
  {
    outputStream << endl;
  }


  //----------------------------------------------------------
  // Finish up.
  //----------------------------------------------------------

  // Delete the SINGULAR input and output files.
  removeReturnValue = remove( singularInputFileName .c_str() );
  removeReturnValue = remove( singularOutputFileName.c_str() );

  // Return the number of solutions of the systems of nonlinear
  // polynomials for each of the Groebner bases.
  return nGroebnerBasisSoln;

}


