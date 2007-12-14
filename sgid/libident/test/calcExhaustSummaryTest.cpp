/*************************************************************************
 *
 * File: calcExhaustSummaryTest.cpp
 *
 *
 * Unit test for the function calcExhaustSummary.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// Identifiability unit tests header files.
#include "calcExhaustSummaryTest.h"

// Identifiability library header files.
#include "../src/calcExhaustSummary.h"
#include "../src/IdentException.h"

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <string>

using namespace CppUnit;


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void calcExhaustSummaryTest::setUp()
{
    // initializations
}


/*************************************************************************
 *
 * Function: tearDown
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void calcExhaustSummaryTest::tearDown()
{
    // clean up
}


/*************************************************************************
 *
 * Function: suite
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

Test* calcExhaustSummaryTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite("calcExhaustSummaryTest");

    suiteOfTests->addTest(new TestCaller<calcExhaustSummaryTest>(
      "paperTwoCompExample_Test", &calcExhaustSummaryTest::paperTwoCompExample_Test));

    suiteOfTests->addTest(new TestCaller<calcExhaustSummaryTest>(
      "oralAbsorpTwoCompModel_twoSolutions_Test", &calcExhaustSummaryTest::oralAbsorpTwoCompModel_twoSolutions_Test));

    return suiteOfTests;
}


/*************************************************************************
 *
 * Function: paperTwoCompExample_Test
 *
 *
 * This two compartment test is based on Example 1 from the following
 * paper:
 *
 *     S. Audoly, G. Bella, L. D'Angio, M. P. Saccomani, and C. Cobelli,
 *     "Global Identifiability of Nonlinear Models of Biological Systems,"
 *     IEEE Transactions on Biomedical Engineering, Vol. 48, pp. 55 - 65,
 *     January 2001.
 *
 *
 *************************************************************************/

void calcExhaustSummaryTest::paperTwoCompExample_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //----------------------------------------------------------
  // Prepare the parameters that will be checked to be identifiable.
  //----------------------------------------------------------

  // Set the number of THETA elements.
  int nTheta = 7;

  // Set the name for each THETA element.
  char* thetaName[] = { "THETA1", "THETA2", "THETA3", "THETA4", "THETA5", "THETA6", "THETA7" };

  // Set the seed to use for the random generator that will be used to
  // calculate a random value for THETA.
  int thetaSeed = 0;


  //----------------------------------------------------------
  // Prepare quantities related to observations and dosing.
  //----------------------------------------------------------

  // Set the number of observation types, i.e., the number of
  // different data streams that have measured values.
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
  int nObservType = 1;

  // Set the number of dose types, i.e., the number of different
  // compartments that will receive doses.
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
  int nDoseType = 1;
  

  //----------------------------------------------------------
  // Set the system-experiment model and the orderings.
  //----------------------------------------------------------

  // This regular chain will contain all of the differential
  // polynomials that describe the system-experiment model.
  const char* sysExpModelRegChainCStr = 
    "[A1[T] - ( THETA2*THETA4*A2+THETA7*U*A1-THETA1*A1^2-THETA3*A1-THETA1*THETA4*A1+THETA7*U*THETA4+THETA2*A2*A1 ) / ( THETA4+A1 ), \
      A2[T] - ( THETA1*A1-THETA5*A2-THETA2*A2 ) / ( 1 ), \
      Y - ( THETA6*A1 ) / ( 1 ), \
      THETA1[T], \
      THETA2[T], \
      THETA3[T], \
      THETA4[T], \
      THETA5[T], \
      THETA6[T], \
      THETA7[T]]";

  // Prepare the natural ordering for the variables, which is the
  // variable order that makes system-experiment mode differential
  // polynomials be a regular chain,
  //
  //   [[Y1, ... , YV, A1, ... , AP, U1, ... , UQ], [THETA1, ... , THETAJ]]
  //
  // where
  //
  //     V = nObservType ,
  //     P = nComp ,
  //     Q = nDoseType ,
  //     J = nTheta .
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
  const char* naturalOrderingCStr = 
    "[[Y, A1, A2, U], [THETA1, THETA2, THETA3, THETA4, THETA5, THETA6, THETA7]]";

  // Prepare the characteristic set ordering for the variables, which
  // is the variable order that eliminates the compartment amounts
  // from the set of differential polynomials and leaves the
  // characteristic set,
  //
  //   [[A1, ... , AP], [Y1, ... , YV, U1, ... , UQ], [THETA1, ..., THETAJ]]
  //
  // where
  //
  //     V = nObservType ,
  //     P = nComp ,
  //     Q = nDoseType ,
  //     J = nTheta .
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
  const char* charSetOrderingCStr = 
    "[[A1, A2], [Y, U], [THETA1, THETA2, THETA3, THETA4, THETA5, THETA6, THETA7]]";

  // This pointer to a C style string will be used like an array of C
  // style strings that will contain the polynomials for the
  // exhaustive summary that were found during the call to
  // calcExhaustSummary().
  //
  // Note that calcExhaustSummary() uses malloc() to allocate the
  // memory for the polynomials, which means that the allocated memory
  // must be freed by this function after it is no longer needed.
  char** exhaustSummaryPolyCStrOut = 0;


  //----------------------------------------------------------
  // Prepare the remaining inputs.
  //----------------------------------------------------------

  // Set this so that intermediate quantities and the exhaustive summary
  // are not printed.
  int level = 0;

  // Set the number of compartments.
  int nComp = 2;


  //------------------------------------------------------------
  // Calculate the exhaustive summary.
  //------------------------------------------------------------

  // This will be the number of exhaustive summary polynomials that
  // were found.
  int nExhaustSummPoly;

  try
  {
    // Calculate the exhaustive summary.
    nExhaustSummPoly = calcExhaustSummary(
      level,
      nTheta,
      thetaName,
      thetaSeed,
      nComp,
      nObservType,
      nDoseType,
      sysExpModelRegChainCStr,
      naturalOrderingCStr,
      charSetOrderingCStr,
      &exhaustSummaryPolyCStrOut );
  }
  catch( const IdentException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "calcExhaustSummary failed!", false );
  }
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE( "calcExhaustSummary failed for unknown reasons!", false);
  }


  //----------------------------------------------------------
  // Get the exhaustive summary' polynomials and free their memory.
  //----------------------------------------------------------

  // These C++ style strings will contain the calculated polynomials
  // for the exhaustive summary.
  vector<string> exhaustSummaryPolyStr( nExhaustSummPoly );

  int m;

  // Store each of the exhaustive summary polynomials in a C++ string and
  // free its C style string.
  for ( m = 0; m < nExhaustSummPoly; m++)
  {
    // Initially set the equation just equal to the polynomial.
    exhaustSummaryPolyStr[m] = string( exhaustSummaryPolyCStrOut[m] );

    // Free the memory for this polynomial's C style string.
    free( exhaustSummaryPolyCStrOut[m] );
  }

  // Free the memory for pointers to the C style strings.
  free( exhaustSummaryPolyCStrOut );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  // Set the known number of exhaustive summary polynomials for this
  // system-experiment model.
  //
  // Note the both algebraic and numeric values of these polynomials
  // are different from those that appear in Equation (22) of the
  // paper:
  //
  //     S. Audoly, G. Bella, L. D'Angio, M. P. Saccomani, and C. Cobelli,
  //     "Global Identifiability of Nonlinear Models of Biological Systems,"
  //     IEEE Transactions on Biomedical Engineering, Vol. 48, pp. 55 - 65,
  //     January 2001.
  //
  // This is because (i) the exhaustive summary calculated by the
  // SINGULAR computer algebra system and the Reduce prototype are
  // different but algebraically equal, and (ii.) the random values
  // generated by the random number generators are also different.
  int nExhaustSummPolyKnown = 14;


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  // Compare the calculated and known number of exhaustive summary polynomials.
  CPPUNIT_ASSERT_MESSAGE( 
    "The calculated and known values for the number of exhaustive summary polynomials do not agree.",
    nExhaustSummPolyKnown == nExhaustSummPoly );

}


/*************************************************************************
 *
 * Function: oralAbsorpTwoCompModel_twoSolutions_Test
 *
 *
 * This test is based on the oral absorption two-compartment model
 * that has a Groebner basis which is known to have two solutions
 * and three polynomials in its exhaustive summary when it is run
 * in the Reduce prototype.
 *
 *************************************************************************/

void calcExhaustSummaryTest::oralAbsorpTwoCompModel_twoSolutions_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //----------------------------------------------------------
  // Prepare the parameters that will be checked to be identifiable.
  //----------------------------------------------------------

  // Set the number of THETA elements.
  int nTheta = 3;

  // Set the name for each THETA element.
  char* thetaName[] = { "THETA1", "THETA2", "THETA3" };

  // Set the seed to use for the random generator that will be used to
  // calculate a random value for THETA.
  int thetaSeed = 0;


  //----------------------------------------------------------
  // Prepare quantities related to observations and dosing.
  //----------------------------------------------------------

  // Set the number of observation types, i.e., the number of
  // different data streams that have measured values.
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
  int nObservType = 1;

  // Set the number of dose types, i.e., the number of different
  // compartments that will receive doses.
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
  int nDoseType = 1;
  

  //----------------------------------------------------------
  // Set the system-experiment model and the orderings.
  //----------------------------------------------------------

  // This regular chain will contain all of the differential
  // polynomials that describe the system-experiment model.
  const char* sysExpModelRegChainCStr = 
    "[A1[T] - ( -THETA2*A1+U ) / ( 1 ), \
      A2[T] - ( THETA2*A1-THETA1*A2 ) / ( 1 ), \
      Y - ( A2/THETA3 ) / ( 1 ), \
      THETA1[T], \
      THETA2[T], \
      THETA3[T]]";

  // Prepare the natural ordering for the variables, which is the
  // variable order that makes system-experiment mode differential
  // polynomials be a regular chain,
  //
  //   [[Y1, ... , YV, A1, ... , AP, U1, ... , UQ], [THETA1, ... , THETAJ]]
  //
  // where
  //
  //     V = nObservType ,
  //     P = nComp ,
  //     Q = nDoseType ,
  //     J = nTheta .
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
  const char* naturalOrderingCStr = 
    "[[Y, A1, A2, U], [THETA1, THETA2, THETA3]]";

  // Prepare the characteristic set ordering for the variables, which
  // is the variable order that eliminates the compartment amounts
  // from the set of differential polynomials and leaves the
  // characteristic set,
  //
  //   [[A1, ... , AP], [Y1, ... , YV, U1, ... , UQ], [THETA1, ..., THETAJ]]
  //
  // where
  //
  //     V = nObservType ,
  //     P = nComp ,
  //     Q = nDoseType ,
  //     J = nTheta .
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
  const char* charSetOrderingCStr = 
    "[[A1, A2], [Y, U], [THETA1, THETA2, THETA3]]";

  // This pointer to a C style string will be used like an array of C
  // style strings that will contain the polynomials for the
  // exhaustive summary that were found during the call to
  // calcExhaustSummary().
  //
  // Note that calcExhaustSummary() uses malloc() to allocate the
  // memory for the polynomials, which means that the allocated memory
  // must be freed by this function after it is no longer needed.
  char** exhaustSummaryPolyCStrOut = 0;


  //----------------------------------------------------------
  // Prepare the remaining inputs.
  //----------------------------------------------------------

  // Set this so that intermediate quantities and the exhaustive summary
  // are not printed.
  int level = 0;

  // Set the number of compartments.
  int nComp = 2;


  //------------------------------------------------------------
  // Calculate the exhaustive summary.
  //------------------------------------------------------------

  // This will be the number of exhaustive summary polynomials that
  // were found.
  int nExhaustSummPoly;

  try
  {
    // Calculate the exhaustive summary.
    nExhaustSummPoly = calcExhaustSummary(
      level,
      nTheta,
      thetaName,
      thetaSeed,
      nComp,
      nObservType,
      nDoseType,
      sysExpModelRegChainCStr,
      naturalOrderingCStr,
      charSetOrderingCStr,
      &exhaustSummaryPolyCStrOut );
  }
  catch( const IdentException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "calcExhaustSummary failed!", false );
  }
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE( "calcExhaustSummary failed for unknown reasons!", false);
  }


  //----------------------------------------------------------
  // Get the exhaustive summary' polynomials and free their memory.
  //----------------------------------------------------------

  // These C++ style strings will contain the calculated polynomials
  // for the exhaustive summary polynomials.
  vector<string> exhaustSummaryPolyStr( nExhaustSummPoly );

  int m;

  // Store each of the exhaustive summary polynomials in a C++ string and
  // free its C style string.
  for ( m = 0; m < nExhaustSummPoly; m++)
  {
    // Initially set the equation just equal to the polynomial.
    exhaustSummaryPolyStr[m] = string( exhaustSummaryPolyCStrOut[m] );

    // Free the memory for this polynomial's C style string.
    free( exhaustSummaryPolyCStrOut[m] );
  }

  // Free the memory for pointers to the C style strings.
  free( exhaustSummaryPolyCStrOut );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  // Set the known number of exhaustive summary polynomials for this
  // system-experiment model, which is known to have 3 polynomials in
  // its exhaustive summary when it is run using the Reduce prototype.
  //
  // Note the both algebraic and numeric values of these polynomials
  // are different from those that are calculated using the Reduce
  // prototype.
  //
  // This is because (i) the exhaustive summary calculated by the
  // SINGULAR computer algebra system and the Reduce prototype are
  // different but algebraically equal, and (ii.) the random values
  // generated by the random number generators are also different.
  int nExhaustSummPolyKnown = 3;


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  // Compare the calculated and known number of exhaustive summary polynomials.
  CPPUNIT_ASSERT_MESSAGE( 
    "The calculated and known values for the number of exhaustive summary polynomials do not agree.",
    nExhaustSummPolyKnown == nExhaustSummPoly );

}


