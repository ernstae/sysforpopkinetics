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
 * @file calcGroebnerBasis.c
 * 
 * 
 * Implements calcGroebnerBasis() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// BLAD differential polynomial library header files.
#include <bad.h>

// CMP big number library header files.
#include <gmp.h>


/*************************************************************************
 *
 * Function: calcGroebnerBasis
 *
 *//**
 * Attempts to calculate the Groebner basis that corresponds to the
 * system-experiment model differential polyomial regular chain
 * sysExpModelRegChainIn.
 *
 * To be specific, this function attempts to calculate the Groebner
 * basis for the exhaustive summary polynomials that come from the
 * input/output relations' coefficients, which are evaluated at a
 * random value for the vector that will be determined to be
 * identifiable or not, THETA.  See Audoly et al. (2001) for details
 * on the identifiability algorithm.
 *
 * Note that if the Groebner basis is calculated successfully, then
 * memory will be allocated by this function for an array of C strings
 * containing each of the polynomials in the basis.  When these
 * polynomials are no longer needed, the memory required to store them
 * and the memory required for the array should be freed by the caller
 * of this function.
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
 * If level = 1, then all of the intermediate information will be
 * printed except for the system-experiment model.
 * If level = 2, then all of the intermediate information will be
 * printed including the system-experiment model.
 * 
 * 
 * @param nTheta
 * 
 * The number of elements in the THETA vector, which are the parameters
 * that will be checked to be identifiable or not.
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
 * @param sysExpModelRegChainIn
 * 
 * This string is the sytem-experiment model regular chain, which
 * contains all of the differential polynomials that describe the
 * system and experiment.
 * An example sytem-experiment model regular chain is
\verbatim

    [A1[T] - THETA2*A2, A2[T] - THETA1*A1, Y1 - A1, THETA1[T], THETA2[T]]

\endverbatim
 * The first and last characters of the string should be left and
 * right square brackets, respectively, and each of the differential
 * polynomials should be separted by a comma.
 * This function assumes that the ordering for the parameters that
 * appear in this regular chain is given by naturalOrderingIn.
 * A regular chain is defined as a triangular set of polynomials,
 * i.e., a set of polynomials having distinct leading variables.
 * Every triangular set of polynomials describes an ideal of
 * polynomials.  See the BLAD library documentation, Boulier (2004),
 * for more details on regular chains.
 * Note that in BLAD library notation, the derivatives with respect to
 * T are denoted
 * \f[
 *   \begin{array}{ccc}
 *     A1[T] = \partial / \partial T A1(T), & A1[T,T] = \partial / \partial T [ \partial / \partial T  A1(T) ], & \cdots \\
 *     A2[T] = \partial / \partial T A2(T), & A2[T,T] = \partial / \partial T [ \partial / \partial T  A2(T) ], & \cdots \\
 *           .             &         .                       &    .   \\
 *           .             &         .                       &    .   \\
 *           .             &         .                       &    .
 *     \mbox{}
 *   \end{array}
 * \f]
 * 
 * 
 * @param naturalOrderingIn
 * 
 * This string is the natural ordering for the variables, which is the
 * variable order that makes the set of the differential polynomials
 * for the system-experiment model be a regular chain.  It should be
 * of the form
\verbatim
 
    [[Y1, ... , YV, A1, ... , AP, U1, ... , UQ], [THETA1, ... , THETAR]]
 
\endverbatim
 * where V = nObservType, P = nIdentComp, Q = nDoseType, and r = nTheta.
 * If there is only one observation type, then it should be simply
 * labeled Y.
 * If there is only dose type, then it should be simply labeled U.
 * The first and last characters of the string should be left and
 * right square brackets, respectively.
 * 
 * @param charSetOrderingIn
 * 
 * This string is the characteristic set ordering for the variables,
 * which is the variable order that eliminates the compartment amounts
 * from the set of differential polynomials and leaves the
 * characteristic set.  It should be of the form
\verbatim

  [[A1, ... , AP], [Y1, ... , YV, U1, ... , UQ], [THETA1, ..., THETAR]]

\endverbatim
 * where V = nObservType, P = nIdentComp, Q = nDoseType, and r = nTheta.
 * If there is only one observation type, then it should be simply
 * labeled Y.
 * If there is only dose type, then it should be simply labeled U.
 * The first and last characters of the string should be left and
 * right square brackets, respectively.
 * 
 * @param groebnerBasisPolyOut
 * 
 * If the Groebner basis was calculated successfully, then the memory
 * pointed to by this pointer to a pointer to a char pointer (char***)
 * will contain an array with separate C strings for each of the
 * polynomials in the basis.
 * An example Groebner basis polynomial is
\verbatim

    THETA1*THETA2 - 28

\endverbatim
 * When these polynomials are no longer needed, the memory required to
 * store them should be released by the caller of this function.
 * No memory should be allocated by the caller before calling this
 * function, however, because this function does the allocation.
 * It is the responsibility of the caller of the function to free the
 * memory allocated by this function.
 * The memory pointed to by groebnerBasisOut will be allocated using
 * malloc() as a C array of nGroebnerBasisPoly pointers to C style
 * strings, each of which contains a polynomial from the Groebner
 * basis and each of which will be allocated using malloc() with
 * enough memory to hold the polynomial.
 * The following code shows how to get a pointer to the C string that
 * contains the m-th polynomial:
 * \code
 *
 *     char* poly_m = (*groebnerBasisPolyOut)[m];
 *
 * \endcode
 * When the polynomials are no longer needed, then the memory for each
 * C string must be freed using free() and the memory for the array of
 * pointers to the polynomials' C strings must be freed also using
 * free().
 * The following code shows how to free all of the memory allocated by
 * this function:
 * \code
 *
 *   for ( m = 0; m < nGroebnerBasisPoly; m++)
 *   {
 *     // Free the memory for this polynomial's C style string.
 *     free( (*groebnerBasisPolyOut)[m] );
 *   }
 * 
 *   // Free the memory for the pointers to the C style strings.
 *   free( (*groebnerBasisPolyOut) );
 *
 * \endcode
 * The above code should be executed by the caller of this function.
 *
 *
 * @return
 *
 * If the Groebner basis could not be calculated, then the return
 * value will be equal to 0.
 *
 * If the Groebner basis was calculated successfully, then the return
 * value will be the number of polynomials in the basis.
 *
 * If the calculated Groebner basis contained multiple regular chains,
 * then the return value will be equal to -1 to indicate that this is
 * an error condition.
 *
 */
/*************************************************************************/

int calcGroebnerBasis( int         level,
                       int         nTheta,
                       int         thetaSeed,
                       int         nIdentComp,
                       int         nObservType,
                       int         nDoseType,
                       const char* sysExpModelRegChainIn,
                       const char* naturalOrderingIn,
                       const char* charSetOrderingIn,
                       char***     groebnerBasisPolyOut )
{
  //----------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - No Time or Memory Limits for Identifiability Runs - Mitch]
  // Right now there are no limits for the time or memory that can be
  // used during the identifiability calculations.
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  // Call the BLAD library initialization function with no limits for
  // the time or memory that can be used during the calculations.
  ba0_int_p timeLimit   = 0;  // Seconds.
  ba0_int_p memoryLimit = 0;  // Megabytes.
  bad_restart( timeLimit, memoryLimit );

  // This is a marker used by the BLAD library to manage memory
  // dynamically.
  struct ba0_mark memoryMarker;

  // This function records the value of the free pointer of the
  // current stack.
  ba0_record( &memoryMarker );


  //----------------------------------------------------------
  // Set the natural ordering for the variables.
  //----------------------------------------------------------

  bav_Iordering naturalOrdering;

  int nNaturalOrderingStringChar = strlen( naturalOrderingIn ) + 100;

  // Allocate more than enough memory for the natural ordering string
  // with all of its BLAD library qualifiers.
  char* naturalOrderingString = 
    (char*) malloc( nNaturalOrderingStringChar * sizeof( char ) );

  // Set the string using snprintf() to make sure the allocated buffer
  // is not overwritten.
  snprintf(
    naturalOrderingString,
    nNaturalOrderingStringChar,
    "ordering( derivations = [T], blocks = %s )",
    naturalOrderingIn );

  // Set the natural ordering.
  ba0_sscanf2(
    naturalOrderingString,
    "%ordering",
    &naturalOrdering );

  // Add the natural ordering to the BLAD global variable bav_R, which
  // Add the natural ordering to the BLAD global variable bav_R, which
  // contains information of the defined orderings and variables.
  bav_R_push_ordering( naturalOrdering );

  // Free the string.
  free( naturalOrderingString );


  //----------------------------------------------------------
  // Set the sytem-experiment model regular chain.
  //----------------------------------------------------------

  // The sytem-experiment model regular chain contains all of the
  // differential polynomials that describe the system and experiment.
  struct bad_regchain sysExpModelRegChain;
  bad_init_regchain( &sysExpModelRegChain );

  int nSysExpModelRegChainStringChar = strlen(sysExpModelRegChainIn ) + 100;

  // Allocate more than enough memory for the sytem-experiment model
  // regular chain string with all of its BLAD library qualifiers.
  char* sysExpModelRegChainString = 
    (char*) malloc( nSysExpModelRegChainStringChar * sizeof( char ) );

  // Set the string using snprintf() to make sure the allocated buffer
  // is not overwritten.
  snprintf(
   sysExpModelRegChainString,
    nSysExpModelRegChainStringChar,
    "regchain( %s, [prime, differential, autoreduced, squarefree, primitive] )",
    sysExpModelRegChainIn );

  // Set the sytem-experiment model regular chain.
  ba0_sscanf2(
    sysExpModelRegChainString,
    "%regchain",
    &sysExpModelRegChain );

  // Free the string.
  free( sysExpModelRegChainString );

  int r;

  // Set the number of polynomials in the system-experiment model.
  int nSysExpModelPoly = sysExpModelRegChain.decision_system.size;

  // Print the polynomials in the system-experiment model.
  if ( level > 1 )
  {
    printf( "System-experiment model = {\n" );
    printf( "\n" );
    for ( r = 0; r < nSysExpModelPoly; r++ )
    {
      // Reset the output driver to avoid spurious line breaks.
      ba0_reset_output();

      ba0_printf( "%Az", sysExpModelRegChain.decision_system.tab[r] );

      if ( r < nSysExpModelPoly - 1 )
      {
        printf( ",\n" );
      }
      else
      {
        printf( " }\n" );
      }
      printf( "\n" );
    }
    printf( "\n" );
  }


  //----------------------------------------------------------
  // Set the variable ordering used to get the characteristic set.
  //----------------------------------------------------------

  bav_Iordering charSetOrdering;

  int nCharSetOrderingStringChar = strlen( charSetOrderingIn ) + 100;

  // Allocate more than enough memory for the characteristic set
  // ordering string with all of its BLAD library qualifiers.
  char* charSetOrderingString = 
    (char*) malloc( nCharSetOrderingStringChar * sizeof( char ) );

  // Set the string using snprintf() to make sure the allocated buffer
  // is not overwritten.
  snprintf(
    charSetOrderingString,
    nCharSetOrderingStringChar,
    "ordering( derivations = [T], blocks = %s )",
    charSetOrderingIn );

  // Set the characteristic set ordering.
  ba0_sscanf2(
    charSetOrderingString,
    "%ordering",
    &charSetOrdering );

  // Free the string.
  free( charSetOrderingString );


  //----------------------------------------------------------
  // Get the characteristic set.
  //----------------------------------------------------------

  struct bad_regchain charSetRegChain;

  bad_init_regchain( &charSetRegChain );

  ba0_sscanf2( "regchain( [], [autoreduced, squarefree, primitive])",
               "%regchain", &charSetRegChain );

  // Change the ordering for the variables in order to get the
  // characteristic set.
  bad_pardi( &charSetRegChain, charSetOrdering, &sysExpModelRegChain );

  // Set the number of polynomials in the characteristic set.
  int nCharSetPoly = charSetRegChain.decision_system.size;

  int k;

  // Print the polynomials in the characteristic set.
  if ( level > 0 )
  {
    printf( "Characteristic set = {\n" );
    printf( "\n" );
    for ( k = 0; k < nCharSetPoly; k++ )
    {
      // Reset the output driver to avoid spurious line breaks.
      ba0_reset_output();

      ba0_printf( "%Az", charSetRegChain.decision_system.tab[k] );

      if ( k < nCharSetPoly - 1 )
      {
        printf( ",\n" );
      }
      else
      {
        printf( " }\n" );
      }
      printf( "\n" );
    }
    printf( "\n" );
  }


  //----------------------------------------------------------
  // Set the variables for the I/O relations' polynomials.
  //----------------------------------------------------------

  bav_variable lastVar;

  int nLastVarChar = 20;
  char lastVarString[nLastVarChar];

  // With the new ordering the I/O relations appear after the
  // differential polynonimials for each of the parameters, which do
  // not change in time.
  //
  // There is one I/O relation for each type of observation, i.e. for
  // each data stream that has measured values.
  //
  // The terms for the I/O relations are polynomials in the variables
  // up to the lastVar variable in the characteristic set ordering.
  //
  // So, set lastVar equal to the variable that comes just before
  // THETA1 in the ordering.
  if ( nDoseType == 1 )
  {
    // If there is only one dose type, then set the lastVar
    // variable equal to
    //
    //     U  .
    //
    ba0_sscanf2( "U", "%v", &lastVar );
  }
  else if ( nDoseType > 0 )
  {
    // If there is more than one dose type, then set the lastVar
    // variable equal to
    //
    //     UQ  ,
    //
    // where Q = nDoseType.
    snprintf( lastVarString, nLastVarChar, "U%i", nDoseType );

    ba0_sscanf2( lastVarString, "%v", &lastVar );
  }
  else
  {
    // If there is no dose type, then set the lastVar variable equal
    // to
    //
    //     AP  ,
    //
    // where  P = nIdentComp.
    snprintf( lastVarString, nLastVarChar, "A%i", nIdentComp );

    ba0_sscanf2( lastVarString, "%v", &lastVar );
  }


  //----------------------------------------------------------
  // Set a random value for the exhaustive summary parameters.
  //----------------------------------------------------------

  struct bav_ev_point inOutRelCoeffEvalPoint;
  bav_init_ev_point( &inOutRelCoeffEvalPoint );
  bav_realloc_ev_point( &inOutRelCoeffEvalPoint, nTheta );

  // Initialize the random number generator state variable.
  gmp_randstate_t randomNumberState;
  gmp_randinit_default( randomNumberState );

  // Set the seed value for the random number generator.
  gmp_randseed_ui( randomNumberState, thetaSeed );

  mpz_t thetaRandom_r;
  mpz_t thetaRandom_rStartsAtZero;
  mpz_t thetaRandomMaxArg;

  mpz_init( thetaRandom_r  );
  mpz_init( thetaRandom_rStartsAtZero );
  mpz_init( thetaRandomMaxArg );

  // Set the maximum random theta value and the argument that controls
  // the upper bound for the random number generator.
  int thetaRandomMax = 30;
  mpz_set_si( thetaRandomMaxArg, thetaRandomMax );

  int nEvalPointFormatChar = 50;

  // Allocate more than enough memory for the evaluation point format
  // string.
  char* evalPointFormatString_r = 
    (char*) malloc( nEvalPointFormatChar * sizeof( char ) );

  // Choose a random value for the parameters that appear in the
  // exhaustive summary.
  for ( r = 0; r < nTheta; r++)
  {
    // Generate a uniform random integer in the range
    //
    //     [0, thetaRandomMax - 1]  ,
    //
    mpz_urandomm( thetaRandom_rStartsAtZero, randomNumberState, thetaRandomMaxArg );

    // Add one to that number so that it is in the range
    //
    //     [1, thetaRandomMax]  ,
    //
    mpz_add_ui( thetaRandom_r, thetaRandom_rStartsAtZero, 1 );

    // Prepare the format string for setting this element of the
    // random THETA value.
    gmp_snprintf(
      evalPointFormatString_r,
      nEvalPointFormatChar,
      "THETA%d = %Zd",
      r + 1,
      thetaRandom_r );

    // Set this element of the random THETA value.
    ba0_sscanf2(
      evalPointFormatString_r,
      "%v = %d",
      &inOutRelCoeffEvalPoint.tab[r].var,
      &inOutRelCoeffEvalPoint.tab[r].value );
  }

  // Reset the size to account for the elements that were just added
  // to the table.
  inOutRelCoeffEvalPoint.size = nTheta;

  // Free the string.
  free( evalPointFormatString_r );

  // Print the random THETA vector.
  if ( level > 0 )
  {
    printf( "Random THETA value = {" );

    // Reset the output driver to avoid spurious line breaks.
    ba0_reset_output();

    for ( r = 0; r < nTheta; r++ )
    {
      ba0_printf( "%d", inOutRelCoeffEvalPoint.tab[r].value );

      if ( r < nTheta - 1 )
      {
        printf( ", " );
      }
      else
      {
        printf( " }\n" );
      }
    }
    printf( "\n" );
    printf( "\n" );
  }


  //----------------------------------------------------------
  // Get the exhaustive summary.
  //----------------------------------------------------------

  struct bap_itercoeff_mpz inOutRelPolyCoeffIter;
  struct bap_polynom_mpz   inOutRelPolyCoeff;
  struct bav_term          inOutRelPolyTerm;

  bap_init_readonly_polynom_mpz( &inOutRelPolyCoeff );
  bav_init_term( &inOutRelPolyTerm );

  mpz_t inOutRelCoeffVal;
  mpz_t inOutRelCoeffValAbsVal;
  mpz_t inOutRelCoeffValTimesMinusOne;

  mpz_init( inOutRelCoeffVal );
  mpz_init( inOutRelCoeffValAbsVal );
  mpz_init( inOutRelCoeffValTimesMinusOne );

  bap_tableof_polynom_mpz exhaustSumm;
  exhaustSumm =( bap_tableof_polynom_mpz)ba0_new_table();
  int nExhaustSummPoly = 0;

  struct bap_polynom_mpz exhaustSummPoly;
  bap_init_polynom_mpz( &exhaustSummPoly );

  int v;

  // Get the polynomials for the exhaustive summary that correspond
  // to each of the monomials from the I/O relations.
  //
  // There is one I/O relation for each type of observation, i.e. for
  // each data stream that has measured values.
  for ( v = 0; v < nObservType; v++ )
  {
    // Get an iterator over the coefficents of the differential
    // polynomial for this observation type's I/O relation.
    bap_begin_itercoeff_mpz(
      &inOutRelPolyCoeffIter, 
      charSetRegChain.decision_system.tab[nTheta + v], lastVar );

    // Check each of the monomials in this I/O relation's polynomial.
    while ( !bap_outof_itercoeff_mpz( &inOutRelPolyCoeffIter ) )
    {
      // Get the term and coefficient.
      bap_term_itercoeff_mpz( &inOutRelPolyTerm, &inOutRelPolyCoeffIter );
      bap_coeff_itercoeff_mpz( &inOutRelPolyCoeff, &inOutRelPolyCoeffIter );
  
      // Only add a polynomial to the exhaustive summary that
      // corresponds to this I/O relation coefficent if the
      // coefficient depends on the parameters.
      if ( !bap_is_numeric_polynom_mpz( &inOutRelPolyCoeff ) )
      {
        // Prepare the element in the table that will get this
        // polynomial.
        nExhaustSummPoly++;
        ba0_realloc_table( (ba0_table)exhaustSumm, nExhaustSummPoly );
        exhaustSumm->size = nExhaustSummPoly;
        exhaustSumm->tab[nExhaustSummPoly-1] = bap_new_polynom_mpz();

        // Get the negative value of the coefficent.
        bap_eval_polynom_ev_point_mpz(
          &inOutRelCoeffVal,
          &inOutRelPolyCoeff,
          &inOutRelCoeffEvalPoint );
        mpz_neg( inOutRelCoeffValTimesMinusOne, inOutRelCoeffVal );
  
        // Set the exhaustive summary polynomial equal to the 
        // I/O relation coefficient minus the coefficient's
        // numeric value,
        //
        //     exhaustSummPoly  =
        //                                                         |
        //         inOutRelCoeff( THETA ) - inOutRelCoeff( THETA ) |                       .
        //                                                         | THETA = thetaRandom
        //
        bap_add_polynom_numeric_mpz( 
          &exhaustSummPoly,
          &inOutRelPolyCoeff,
          inOutRelCoeffValTimesMinusOne );

        // Set the value for this exhaustive summary polynomial in the
        // table.
        bap_set_polynom_mpz(
          exhaustSumm->tab[nExhaustSummPoly-1],
          &exhaustSummPoly );
      }
  
      bap_next_itercoeff_mpz( &inOutRelPolyCoeffIter );
    }
  }

  bap_close_itercoeff_mpz( &inOutRelPolyCoeffIter );
 
  int i;

  // Print the polynomials in the exhaustive summary.
  if ( level > 0 )
  {
    printf( "Exhaustive summary = {\n" );
    printf( "\n" );
    for ( i = 0; i < nExhaustSummPoly; i++ )
    {
      // Reset the output driver to avoid spurious line breaks.
      ba0_reset_output();

      ba0_printf( "%Az", exhaustSumm->tab[i] );

      if ( i < nExhaustSummPoly - 1 )
      {
        printf( ",\n" );
      }
      else
      {
        printf( " }\n" );
      }
      printf( "\n" );
    }
    printf( "\n" );
  }


  //----------------------------------------------------------
  // Get the Groebner basis for the exhaustive summary.
  //----------------------------------------------------------

  bad_intersectof_regchain exhaustSummGroebnerBasis;
  exhaustSummGroebnerBasis = bad_new_intersectof_regchain();

  // Prepare the intersection of regular chains for the exhaustive
  // summary that will contain the Groebner basis.
  ba0_sscanf2(
    "intersectof_regchain( [], [normalized, primitive, autoreduced])",
    "%intersectof_regchain",
    exhaustSummGroebnerBasis );

  // Calculate the intersection of regular chains.
  bad_Rosenfeld_Groebner( 
    exhaustSummGroebnerBasis,
    exhaustSumm,
    (bap_tableof_polynom_mpz)0,
    (bav_tableof_variable)0 );

  // Set the number of regular chains in the intersection.
  int nGroebnerBasisRegChain = exhaustSummGroebnerBasis->inter.size;

  // See if there are any regular chains in the intersection.
  if ( nGroebnerBasisRegChain == 0 )
  {
    // If there are no regular chains in the intersection, then don't
    // change the Groebner basis output string and return zero for the
    // number of polynomials in the Groebner basis.
    return 0;
  }

  // See if there are multiple regular chains in the intersection.
  if ( nGroebnerBasisRegChain > 1 )
  {
    // If there is more than one regular chain in the intersection,
    // then don't change the Groebner basis output string and return
    // minus one for the number of polynomials in the Groebner basis 
    // to indicate that this is an error condition.
    return -1;
  }

  // Set the number of polynomials in the Groebner basis.
  int nGroebnerBasisPoly =
    (*exhaustSummGroebnerBasis->inter.tab[0]).decision_system.size;

  // Allocate enough elements in the output array to hold all of the
  // of polynomials in the Groebner basis.
  *groebnerBasisPolyOut = 
      (char**) malloc( nGroebnerBasisPoly * sizeof( char* ) );

  char* groebnerBasisPolyString;
  int nGroebnerBasisPolyChar;

  int m;

  // Set the output array of Groebner basis polynomials.
  for ( m = 0; m < nGroebnerBasisPoly; m++ )
  {
    // Get a string that contains this Groebner basis polynomial.
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Possible BLAD Library Memory Leak - Mitch]
    // Note that this string is allocated by the BLAD library function
    // ba0_malloc and it is assumed that the BLAD memory clean up
    // functions will free this memory.
    //
    // Even if the memory does not get freed, it is probably not a big
    // problem since the calculation of the Groebner basis happens
    // only once during the identifiability calculation, and the
    // amount of memory for the basis polynomials is relatively small.
    //
    // If this is a problem, consider (i.) making a fixed length
    // buffer that is much longer than any polynomials are likely to
    // be (1000?) and checking to be sure that it is not overwritten;
    // or (ii.)  writing the polynomials to a temporary file and using
    // BLAD's output counter to get the length, which would then be
    // used to allocate the proper amount of memory.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //
    groebnerBasisPolyString = ba0_new_printf(
      "%Az",
      (*exhaustSummGroebnerBasis->inter.tab[0]).decision_system.tab[m] );

    // Because the function strlen() gives the number of characters in
    // the string not including the null termination, allocate enough
    // memory for the Groebner basis polynomial output string with an
    // extra character for the null termination.
    nGroebnerBasisPolyChar = strlen( groebnerBasisPolyString ) + 1;
    (*groebnerBasisPolyOut)[m] = 
      (char*) malloc( nGroebnerBasisPolyChar * sizeof( char ) );
    
    // Set the Groebner basis output string.
    strncpy(
      (*groebnerBasisPolyOut)[m],
      groebnerBasisPolyString,
      nGroebnerBasisPolyChar );
  }

  // Print the polynomials in the Groebner basis.
  if ( level > 0 )
  {
    printf( "Groebner basis = {\n" );
    printf( "\n" );
    for ( m = 0; m < nGroebnerBasisPoly; m++ )
    {
      // Reset the output driver to avoid spurious line breaks.
      ba0_reset_output();

      ba0_printf( "%Az",
       (*exhaustSummGroebnerBasis->inter.tab[0]).decision_system.tab[m] );

      if ( m < nGroebnerBasisPoly - 1 )
      {
        printf( ",\n" );
      }
      else
      {
        printf( " }\n" );
      }
      printf( "\n" );
    }
    printf( "\n" );
  }


  //----------------------------------------------------------
  // Finish up.
  //----------------------------------------------------------

  // Free the memory allocated for all of the GMP integers.
  mpz_clear( thetaRandom_r );
  mpz_clear( thetaRandom_rStartsAtZero );
  mpz_clear( thetaRandomMaxArg );
  mpz_clear( inOutRelCoeffVal );
  mpz_clear( inOutRelCoeffValAbsVal );
  mpz_clear( inOutRelCoeffValTimesMinusOne );

  // Undo the last call to bav_R_push_ordering().
  bav_R_pull_ordering();

  // This function restores the value of the free pointer.
  ba0_restore( &memoryMarker );

  // Call the BLAD library termination function.
  bad_terminate( ba0_init_level );

  // Return the number of polynomials in the Groebner basis.
  return nGroebnerBasisPoly;
}


