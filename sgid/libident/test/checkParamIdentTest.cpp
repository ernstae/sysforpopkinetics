/*************************************************************************
 *
 * File: checkParamIdentTest.cpp
 *
 *
 * Unit test for the function checkParamIdent.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// Identifiability unit tests header files.
#include "checkParamIdentTest.h"

// Identifiability library header files.
#include "../src/checkParamIdent.h"
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

void checkParamIdentTest::setUp()
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

void checkParamIdentTest::tearDown()
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

Test* checkParamIdentTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite("checkParamIdentTest");

    suiteOfTests->addTest(new TestCaller<checkParamIdentTest>(
      "paperTwoCompExample_Test", &checkParamIdentTest::paperTwoCompExample_Test));

    suiteOfTests->addTest(new TestCaller<checkParamIdentTest>(
      "oralAbsorpTwoCompModel_twoSolutions_Test", &checkParamIdentTest::oralAbsorpTwoCompModel_twoSolutions_Test));

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

void checkParamIdentTest::paperTwoCompExample_Test()
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

  char* thetaNameCStr[] = { "THETA1", "THETA2", "THETA3", "THETA4", "THETA5", "THETA6", "THETA7" };

  std::vector< std::string > thetaName( nTheta );

  int r;

  // Set the name for each THETA element.
  for ( r = 0; r < nTheta; r++ )
  {
    thetaName[r] = thetaNameCStr[r];
  }

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
  // Set the differential equations and observation equations.
  //----------------------------------------------------------

  // Set the number of compartments.
  int nComp = 2;

  std::vector< std::string > compOde( nComp );

  // Set the differential equations for each compartment.
  compOde[0] = "A1[T] = ( THETA2*THETA4*A2+THETA7*U*A1-THETA1*A1^2-THETA3*A1-THETA1*THETA4*A1+THETA7*U*THETA4+THETA2*A2*A1 ) / ( THETA4+A1 )";
  compOde[1] = "A2[T] = ( THETA1*A1-THETA5*A2-THETA2*A2 ) / ( 1 )";

  std::vector< std::string > observEqn( nObservType );

  // Set the observation equations.
  observEqn[0] = "Y = ( THETA6*A1 ) / ( 1 )";


  //----------------------------------------------------------
  // Prepare the remaining inputs.
  //----------------------------------------------------------

  // Set this so that intermediate quantities and the Groebner basis
  // are not printed.
  int level = 0;

  // This will be the status string for the indentifiability
  // calculation.
  std::string identStatus;


  //------------------------------------------------------------
  // Calculate the Groebner basis solutions.
  //------------------------------------------------------------

  // This will be the number of Groebner basis solutions that were
  // found.
  int nGroebnerBasisSoln;

  try
  {
    // Calculate the number of Groebner basis solutions for the
    // exhaustive summary.
    nGroebnerBasisSoln = checkParamIdent(
      level,
      nTheta,
      thetaName,
      thetaSeed,
      nComp,
      nObservType,
      nDoseType,
      compOde,
      observEqn,
      identStatus );
  }
  catch( const IdentException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "checkParamIdent failed!", false );
  }
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE( "checkParamIdent failed for unknown reasons!", false);
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  // Set the known number of Groebner basis solutions for 
  // this system-experiment model.
  int nGroebnerBasisSolnKnown = 1;

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - A Nonlinear System of Equations Solver is Needed - Mitch]
  //
  // Currently, the Groebner bases equations can not be solved when
  // they are nonlinear as in this unit test.
  //
  // For now, set the number of unique solutions of the Groebner bases
  // equations equal to the value that is used to indicate the
  // solutions could not be found.
  //
  nGroebnerBasisSolnKnown = 0;
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  // Compare the calculated and known number of Groebner basis.
  CPPUNIT_ASSERT_MESSAGE( 
    "The calculated and known values for the number of Groebner bases do not agree.",
    nGroebnerBasisSolnKnown == nGroebnerBasisSoln );

}


/*************************************************************************
 *
 * Function: oralAbsorpTwoCompModel_twoSolutions_Test
 *
 *
 * This test is based on the oral absorption two-compartment model
 * that has a Groebner basis which is known to have two solutions.
 *
 *************************************************************************/

void checkParamIdentTest::oralAbsorpTwoCompModel_twoSolutions_Test()
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

  char* thetaNameCStr[] = { "THETA1", "THETA2", "THETA3" };

  std::vector< std::string > thetaName( nTheta );

  int r;

  // Set the name for each THETA element.
  for ( r = 0; r < nTheta; r++ )
  {
    thetaName[r] = thetaNameCStr[r];
  }

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
  // Set the differential equations and observation equations.
  //----------------------------------------------------------

  // Set the number of compartments.
  int nComp = 2;

  std::vector< std::string > compOde( nComp );

  // Set the differential equations for each compartment.
  compOde[0] = "A1[T] = ( -THETA2*A1+U ) / ( 1 )";
  compOde[1] = "A2[T] = ( THETA2*A1-THETA1*A2 ) / ( 1 )";

  std::vector< std::string > observEqn( nObservType );

  // Set the observation equations.
  observEqn[0] = "Y = ( A2/THETA3 ) / ( 1 )";


  //----------------------------------------------------------
  // Prepare the remaining inputs.
  //----------------------------------------------------------

  // Set this so that intermediate quantities and the Groebner basis
  // are not printed.
  int level = 0;

  // This will be the status string for the indentifiability
  // calculation.
  std::string identStatus;


  //------------------------------------------------------------
  // Calculate the Groebner basis solutions.
  //------------------------------------------------------------

  // This will be the number of Groebner basis solutions that were
  // found.
  int nGroebnerBasisSoln;

  try
  {
    // Calculate the number of Groebner basis solutions for the
    // exhaustive summary.
    nGroebnerBasisSoln = checkParamIdent(
      level,
      nTheta,
      thetaName,
      thetaSeed,
      nComp,
      nObservType,
      nDoseType,
      compOde,
      observEqn,
      identStatus );
  }
  catch( const IdentException& e )
  {
    CPPUNIT_ASSERT_MESSAGE( "checkParamIdent failed!", false );
  }
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE( "checkParamIdent failed for unknown reasons!", false);
  }


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  // Set the known number of Groebner basis solutions for 
  // this system-experiment model.
  int nGroebnerBasisSolnKnown = 1;

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - A Nonlinear System of Equations Solver is Needed - Mitch]
  //
  // Currently, the Groebner bases equations can not be solved when
  // they are nonlinear as in this unit test.
  //
  // For now, set the number of unique solutions of the Groebner bases
  // equations equal to the value that is used to indicate the
  // solutions could not be found.
  //
  nGroebnerBasisSolnKnown = 0;
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  // Compare the calculated and known number of Groebner basis.
  CPPUNIT_ASSERT_MESSAGE( 
    "The calculated and known values for the number of Groebner bases do not agree.",
    nGroebnerBasisSolnKnown == nGroebnerBasisSoln );

}


