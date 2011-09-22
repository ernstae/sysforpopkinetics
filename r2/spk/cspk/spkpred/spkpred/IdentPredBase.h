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
/*************************************************************************
 *//**
 * @file IdentPredBase.h
 * 
 * 
 * Declares IdentPredBase class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef IDENTPREDBASE_H
#define IDENTPREDBASE_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "calcGroebnerBasis.h"
#include "OdePredBase.h"

// Standard library header files.
#include <vector>

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
 * Class: IdentPredBase
 *
 *//**
 * This abstract base class is a specialization of OdePredBase that
 * provides additional interfaces and support functions to determine
 * the identifiability of ordinary differential equation (ODE) based
 * compartmental models.
 *//*
 *************************************************************************/

template <class Value>
class IdentPredBase : public OdePredBase<Value>
{
  //------------------------------------------------------------
  // Constructors.
  //------------------------------------------------------------

  /***********************************************************************
   *
   * Function: IdentPredBase
   *
   *//**
   * Constructor that must be called by constructors for concrete
   * subclasses of this abstract base class.
   */
  /***********************************************************************/

public:
  IdentPredBase(
    int                            nThetaIn,
    int                            nEtaIn,
    bool                           isPkBlockAFuncOfTIn,
    int                            nCompIn,
    int                            defaultDoseCompIn,
    int                            defaultObservCompIn,
    const SPK_VA::valarray<bool>&  compInitialOffIn,
    const SPK_VA::valarray<bool>&  compNoOffIn,
    const SPK_VA::valarray<bool>&  compNoDoseIn )
    :
    OdePredBase<Value>(
      isPkBlockAFuncOfTIn,
      nCompIn,
      defaultDoseCompIn,
      defaultObservCompIn,
      compInitialOffIn,
      compNoOffIn,
      compNoDoseIn,
      Value( 0 ) ),
    nTheta      ( nThetaIn ),
    nEta        ( nEtaIn ),
    nIdentComp  ( nCompIn - 1 )
  {
  }


  //------------------------------------------------------------
  // Miscellaneous constant information.
  //------------------------------------------------------------

private:
  const int nTheta;                ///< Number of theta parameters.
  const int nEta;                  ///< Number of eta parameters.

  const int nIdentComp;            ///< Number of compartments for determinining identifiability (does not include the ouput compartment).


  //------------------------------------------------------------
  // Functions related to determining parameter identifiability.
  //------------------------------------------------------------

  /*************************************************************************
   *
   * Function: checkIndParamIdent
   *
   *//**
   * Attempts to determine the identifiability of an individual's
   * THETA parameter using the system-experiment model that is defined
   * by the expressions from the PK, DES, and ERROR blocks and that is
   * also defined by the data records from the data file.
   *
   * See Audoly et al. (2001) for details on the identifiability
   * algorithm.
   *
   * First, this function examines all of the data records to
   * determine the unique observation types and the compartments that
   * receive doses.  The identifiability calculation will be performed
   * using the data values and covariates for the last data record for
   * the individual.
   * 
   * Next, this function attempts to calculate the Groebner basis or
   * bases that correspond to the system-experiment model differential
   * polyomial regular chain sysExpModelRegChainIn.
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
   * @param i
   * 
   * If index for the individual for which to perform the
   * identifiability calculation.
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
   * @param nEta
   * 
   * The number of elements in the ETA vector.
   * 
   * 
   * @param thetaSeed
   * 
   * The value to use to seed the random number generator used to generate
   * the random value for THETA.
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
   */
  /*************************************************************************/

public:
  int checkIndParamIdent(
    int i,
    int level,
    int nTheta,
    int nEta,
    int thetaSeed,
    std::string& identStatus )
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
      OdePredBase<Value>::setCompAmount( p, symbol( paramSymbol.str() ) );
    }
  

    //----------------------------------------------------------
    // Prepare the parameters that will be checked to be identifiable.
    //----------------------------------------------------------
  
    // This list will contain the parameters that will be checked to
    // be identifiable, i.e., the elements of theta.
    GiNaC::lst identPar;
  
    // These will hold the values for each element of theta.
    std::vector<Value> theta( nTheta );

    int r;

    // Because there are not vectors of parameters for the BLAD library,
    // set the symbols for the elements of the theta vector to be 
    //
    //     THETA1, THETA2, ...
    //
    for ( r = 0; r < nTheta; r++ )
    {
      // Set this element's symbol.
      paramSymbol.str( "" );
      paramSymbol << "THETA" << r + 1;
      theta[r] = symbol( paramSymbol.str() );
  
      // Add this theta element's symbol to the list of parameters.
      identPar.append( theta[r] );
    }
  
  
    //----------------------------------------------------------
    // Prepare the rest of the parameters.
    //----------------------------------------------------------
  
    // These will hold the values for each element of eta.
    //
    // Since these parameters will not be checked to be identifiable,
    // set all of the elements of eta equal to zero, which is the mean
    // value for these random variables.
    //
    // This is equivalent to assuming that the experiment is perfect
    // and has noiseless data.
    std::vector<Value> eta( nEta, 0 );
  

    //----------------------------------------------------------
    // Prepare the vectors of independent and dependent variables.
    //----------------------------------------------------------

    // Set quantities related to the vector of independent variables
    // theta and eta, which are combined into a single vector of
    // independent variables,
    //
    //            -       -
    //           |  theta  |
    //     z  =  |         |  .
    //           |   eta   |
    //            -       -
    //
    const int nZ             = nTheta + nEta;    // Number of independent variables.
    const int thetaOffsetInZ = 0;                // Offset for theta in the vector of independent variables.
    const int etaOffsetInZ   = nTheta;           // Offset for eta in the vector of independent variables.
  
    // There are no eps variables at the individual level.
    const int nEps           = 0;
    const int epsOffsetInZ   = nTheta + nEta;    // Offset for eps in the vector of independent variables.

    // Set the number of data and observation records for this
    // individual.
    int nDataRec   = OdePredBase<Value>::getNRecords( i );
    int nObservRec = this->getNObservs( i );

    // Set quantities related to the vector of dependent variables
    // for the current individual,
    //
    //                 -                 -
    //                |  f( theta )       |
    //     w( z )  =  |                   |  .
    //                |  y( theta, eta )  |
    //                 -                 -
    //
    const int nF         = nObservRec;        // Number of elements in f.
    const int nY         = nObservRec;        // Number of elements in y.
    const int nW         = 2 * nObservRec;    // Number of dependent variables for current individual.
    const int fOffsetInW = 0;                 // Offset for f in the vector of dependent variables.
    const int yOffsetInW = nObservRec;        // Offset for y in the vector of dependent variables.
  
    // Initialize the independent and dependent variables with the
    // proper number of elements.
    std::vector<Value> indepVar( nZ );
    std::vector<Value> depVar  ( nW );
  
    // Set the values for the theta independent variables.
    for ( r = 0; r < nTheta; r++ )
    {
      indepVar[r + thetaOffsetInZ] = theta[r];
    }
    
    // Set the values for the eta independent variables.
    for ( r = 0; r < nEta; r++ )
    {
      indepVar[r + etaOffsetInZ] = eta[r];
    }

  
    //----------------------------------------------------------
    // Do preparations related to new individuals.
    //----------------------------------------------------------
  
    int j = 0;
  
    // Perform initializations required for new values of i before
    // calls to evalPk(), evalDes(), and evalError() will work.
    initUserEnv(
      thetaOffsetInZ,
      nTheta,
      etaOffsetInZ,
      nEta,
      epsOffsetInZ,
      nEps,
      fOffsetInW,
      nF,
      yOffsetInW,
      nY,
      i,
      j,
      indepVar,
      depVar);
  
  
    //----------------------------------------------------------
    // Get the observation types and determine compartments with doses.
    //----------------------------------------------------------

    int v;

    int cmt_i_j;
    int evid_i_j;

    // These need to be references so that when they change here,
    // their value in the base class changes also.
    Value& f_i_j = OdePredBase<Value>::getF();
    Value& y_i_j = OdePredBase<Value>::getY();

    Value compAmount_p;
    Value compAmount_t_p;
    Value compBioavailFrac_p;
    Value compScaleParam_p;
  
    int observComp;
    int defaultObservComp;
    int doseComp;
    int defaultDoseComp;
  
    // Get the default observation and dose compartments.
    defaultObservComp = OdePredBase<Value>::getDefaultObservComp();
    defaultDoseComp   = OdePredBase<Value>::getDefaultDoseComp  ();
  
    // This will keep track of whether or not the compartment gets a
    // dose sometime during the experiment.
    std::vector<bool> compGetsDose( nIdentComp, false );

    // This will contain all of the unique observation types, which
    // are the intra-individual error models that are defined by the
    // expressions in the ERROR block and that are different from any
    // other observation type.
    //
    // Each observation type is considered to be a known function of
    // time.
    std::vector<Value> observType;
  
    int nObservType = 0;
  
    // Read all of this individual's data records in order to get all
    // of the observation and dose types.
    for ( j = 0; j < nDataRec; j++ )
    {
      //----------------------------------------------------------
      // Prepare the data items and PK parameters.
      //----------------------------------------------------------
  
      // Get the data items for the current data record.
      this->readDataRecord( i, j );
  
      // Evaluate the current values for the PK parameters because the
      // rates and durations of zero-order bolus doses can be set in
      // the PK block.
      evalPk(
        thetaOffsetInZ,
        nTheta,
        etaOffsetInZ,
        nEta,
        i,
        j,
        indepVar );
  
      // Get the values for some of the data items.
      cmt_i_j  = OdePredBase<Value>::getCMT ();
      evid_i_j = OdePredBase<Value>::getEVID();
  
  
      //--------------------------------------------------------
      // Handle observation events.
      //--------------------------------------------------------
  
      // Evaluate the ERROR blocks for all of the observation events in
      // the data file order to determine the functional form for all of
      // the observation types.
      //
      // The reason that all of the observation events must be exercised
      // is that the C++ code that defines some of the functional forms
      // might be located in if-then blocks and only be exercised for some
      // of the data records.
      if ( evid_i_j == OdePredBase<Value>::OBSERV_EVENT )
      {
        // If this is an observation event, then get its
        // intra-individual error model.
        //
        // Set the compartment.
        if ( cmt_i_j == 0 )
        {
          // If the compartment is not specified, then the observation
          // will come from the default compartment.
          observComp =  defaultObservComp;
        }
        else
        {
          // If the compartment is specified, then the observation
          // will come from that compartment.
          observComp = cmt_i_j;
        }
  
        // Get the compartment index for the observation compartment.
        p = OdePredBase<Value>::compIndex( observComp );
  
        // Set current values for any variables defined in the DES block.
        evalDes(
          thetaOffsetInZ,
          nTheta,
          i,
          j,
          indepVar );
  
        // Get the amount and scale parameter for this compartment.
        OdePredBase<Value>::getCompAmount    ( p, compAmount_p );
        OdePredBase<Value>::getCompScaleParam( p, compScaleParam_p );
    
        // Set F equal to the scaled amount in the observation compartment.
        f_i_j = compAmount_p / compScaleParam_p;
    
        // Evaluate the intra-individual error model.
        evalError( 
          thetaOffsetInZ,
          nTheta,
          etaOffsetInZ,
          nEta,
          epsOffsetInZ,
          nEps,
          i,
          j,
          indepVar );
  
        // Get the value for the intra-individual error model that was
        // just set in the ERROR block expressions.
        //
        //                      |
        //     y( theta, eta )  |          .
        //                      | eta = 0
        //
        OdePredBase<Value>::getY( y_i_j );

        // See if this is a new intra-individual error model.
        for ( v = 0; v <= nObservType; v++ )
        {
          if ( v == nObservType )
          {
            // If this is a new intra-individual error model, then put
            // it into the list.
            observType.push_back( y_i_j );
            nObservType++;
            break;
          }
          else if ( observType[v] == y_i_j )
          {
            // If this intra-individual error model already exists,
            // then there is no need to check the others.
            break;
          }
        }
      }
  
  
      //----------------------------------------------------------
      // Handle dose events.
      //----------------------------------------------------------
  
      if ( evid_i_j == OdePredBase<Value>::DOSE_EVENT )
      {
        // If this is a dose, then update the flag that indicates this
        // compartment gets a dose.
        //
        // Set the compartment that gets the dose.
        if ( cmt_i_j == 0 )
        {
          // If the compartment is not specified, then the dose goes in
          // the default comparment.
          doseComp = defaultDoseComp;
        }
        else
        {
          // If the compartment is specified, then the dose goes in
          // that compartment.
          doseComp = cmt_i_j;
        }
  
        // Set this to indicate that the compartment gets a dose.
        compGetsDose[ OdePredBase<Value>::compIndex( doseComp ) ] = true;
      }
  

      //----------------------------------------------------------
      // Save the user environment information at the last data record.
      //----------------------------------------------------------
    
      // Save information for the environment for subclasses of this
      // base class that are required for the current values of i and j.
      saveUserEnv(
        thetaOffsetInZ,
        nTheta,
        etaOffsetInZ,
        nEta,
        epsOffsetInZ,
        nEps,
        fOffsetInW,
        nF,
        yOffsetInW,
        nY,
        i,
        j,
        indepVar,
        depVar );
  
    }
  
  
    //----------------------------------------------------------
    // Set the symbols for the doses.
    //----------------------------------------------------------

    int q;

    // This will contain all of the dose types.
    //
    // Each dose type is considered to be a known function of time.
    std::vector<Value> doseType;

    int nDoseType = 0;

    // Get the dose types.
    for ( p = 0; p < nIdentComp; p++ )
    {
      // If this compartment gets a dose, then put a dose type for it
      // into the list.
      if ( compGetsDose[p] )
      {
        doseType.push_back( Value() );
        nDoseType++;
      }
    }

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


    //----------------------------------------------------------
    // Prepare the differential polynomial for each ODE.
    //----------------------------------------------------------
  
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
  
    // This will contain the right hand side of the ODE.
    GiNaC::ex odeRHS;
  
    int doseTypeCount = 0;
  
    // Prepare the differential polynomials that correspond to each of
    // the ODE's in the DES block.
    //
    // There should be one for every compartment except the output
    // compartment.
    for ( p = 0; p < nIdentComp; p++ )
    {
      // Get the derivative of this compartment's amount.
      OdePredBase<Value>::getCompAmount_t( p, compAmount_t_p );

      // Get this compartment's bioavailability.  
      OdePredBase<Value>::getCompBioavailFrac( p, compBioavailFrac_p );
  
      // Set the right hand side of the ODE.
      if ( compGetsDose[p] )
      {
        // If this compartment receives a dose, then the dose will be
        // multiplied by the bioavailabilty for the compartment, i.e,
        // the dose term that is appended to the ODE will be
        //
        //     Fm * Un  ,
        //
        // where Fm is the bioavailability for the m-th compartment and
        // Un is the n-th dose.
        //
        // Note that the compartment number and the dose number are not
        // necessarily the same.
        odeRHS = compAmount_t_p + compBioavailFrac_p * doseType[doseTypeCount++];
      }
      else
      {
        // Set the right hand side of the ODE for compartments that
        // don't receive doses.
        odeRHS = compAmount_t_p;
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
        outputStream << "A" << p + 1 << "[T] - ( " 
                     << odeRHS << " )";
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
  
    assert( doseTypeCount == nDoseType );
  
  
    //----------------------------------------------------------
    // Prepare the differential polynomial for each observation type.
    //----------------------------------------------------------
  
    // Prepare the differential polynomials that correspond to each of
    // the observation types.
    for ( v = 0; v < nObservType; v++ )
    {
      // Add this differential polynomial to the set of differential
      // polynomials that make up the regular chain.
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
      sysExpModelRegChain << "Y";
      if ( nObservType > 1 )
      {
        sysExpModelRegChain << v + 1;
      }
      sysExpModelRegChain << " - ( "
                          << observType[v].normal().numer() << " ) / ( "
                          << observType[v].normal().denom() << " ), ";
    
      // Print this differential polynomial using the normal GiNaC format.
      if ( level > 0 )
      {
        outputStream << "Y";
        if ( nObservType > 1 )
        {
          outputStream << v + 1;
        }
        outputStream << " - ( " << observType[v] << " )";
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
      sysExpModelRegChain << "THETA" << r + 1 << "[T]";
  
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
      naturalOrdering << "THETA" << r + 1;
  
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
      charSetOrdering << "THETA" << r + 1;
  
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
  
    // If the Groebner bases could not be calculated, then this
    // calculation cannot continue.
    if ( nGroebnerBasis == 0 )
    {
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR, 
        "The parameter identifiability calculation failed because the Groebner basis for the \nexhaustive summary could not be determined.",
        __LINE__, 
        __FILE__ );
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
        outputStream << "SPK cannot currently determine its identifiability." << endl;
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
        std::string message = "The Groebner basis is not a linear system of polynomials.  \nSPK cannot currently solve such a nonlinear system. \n";
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
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR, 
        "An unknown exception was thrown during the attempt to solve the Groebner basis as a \nlinear system of equations.",
        __LINE__, 
        __FILE__ );
    }

  }
  

  //------------------------------------------------------------
  // Destructors.
  //------------------------------------------------------------

public:
  virtual ~IdentPredBase(){}


  //------------------------------------------------------------
  // Constructors and operators that should not be called.
  //------------------------------------------------------------

private:
  IdentPredBase();
  IdentPredBase( const IdentPredBase& );
  IdentPredBase & operator=( const IdentPredBase& );
};
#endif
