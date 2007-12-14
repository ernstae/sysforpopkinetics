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
      "paperTwoCompExample_globallyIdent_Test", &checkParamIdentTest::paperTwoCompExample_globallyIdent_Test));

    suiteOfTests->addTest(new TestCaller<checkParamIdentTest>(
      "paperTwoCompExample_realNumberPrinting_Test", &checkParamIdentTest::paperTwoCompExample_realNumberPrinting_Test));

    suiteOfTests->addTest(new TestCaller<checkParamIdentTest>(
      "oralAbsorpTwoCompModel_twoSolutions_Test", &checkParamIdentTest::oralAbsorpTwoCompModel_twoSolutions_Test));

    suiteOfTests->addTest(new TestCaller<checkParamIdentTest>(
      "multipleGroebnerBasis_eightSolutions_Test", &checkParamIdentTest::multipleGroebnerBasis_eightSolutions_Test));

    suiteOfTests->addTest(new TestCaller<checkParamIdentTest>(
      "fourCompModel_sixSolutions_Test", &checkParamIdentTest::fourCompModel_sixSolutions_Test));

    suiteOfTests->addTest(new TestCaller<checkParamIdentTest>(
      "noExhaustSummModel_zeroSolutions_Test", &checkParamIdentTest::noExhaustSummModel_zeroSolutions_Test));

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Unit Test is not Currently being Run - Mitch]
    // Once this test works, uncomment it.
    /*
    suiteOfTests->addTest(new TestCaller<checkParamIdentTest>(
      "threeCompModel_unknownSolutions_Test", &checkParamIdentTest::threeCompModel_unknownSolutions_Test));
    */
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  // This test is Example 1 from the paper:
  //
  //     S. Audoly, G. Bella, L. D'Angio, M. P. Saccomani, and C. Cobelli,
  //     "Global Identifiability of Nonlinear Models of Biological Systems,"
  //     IEEE Transactions on Biomedical Engineering, Vol. 48, pp. 55 - 65,
  //     January 2001.
  //
  // It has infinite solutions, which means a value of -1 here.
  int nGroebnerBasisSolnKnown = -1;


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
 * Function: paperTwoCompExample_globallyIdent_Test
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
 * The original example has infinite solutions, but this test is made
 * uniquely identifiable by setting
 *
 *     THETA7 = 1  .
 *
 *************************************************************************/

void checkParamIdentTest::paperTwoCompExample_globallyIdent_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //----------------------------------------------------------
  // Prepare the parameters that will be checked to be identifiable.
  //----------------------------------------------------------

  // Set the number of THETA elements.
  int nTheta = 6;

  char* thetaNameCStr[] = { "THETA1", "THETA2", "THETA3", "THETA4", "THETA5", "THETA6" };

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
  compOde[0] = "A1[T] = ( THETA2*THETA4*A2+1*U*A1-THETA1*A1^2-THETA3*A1-THETA1*THETA4*A1+1*U*THETA4+THETA2*A2*A1 ) / ( THETA4+A1 )";
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

  // This test is Example 1 from the paper:
  //
  //     S. Audoly, G. Bella, L. D'Angio, M. P. Saccomani, and C. Cobelli,
  //     "Global Identifiability of Nonlinear Models of Biological Systems,"
  //     IEEE Transactions on Biomedical Engineering, Vol. 48, pp. 55 - 65,
  //     January 2001.
  //
  // The original example has infinite solutions, but this test is made
  // uniquely identifiable (1 solution) by setting
  //
  //     THETA7 = 1  .
  //
  int nGroebnerBasisSolnKnown = 1;


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
 * Function: paperTwoCompExample_realNumberPrinting_Test
 *
 *
 * This test is used to check the way that real numbers are printed in
 * the GiNaC expressions that are passed to BLAD.
 *
 * Since BLAD only works for integers, the real numbers have to be
 * converted to an integer representation.
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

void checkParamIdentTest::paperTwoCompExample_realNumberPrinting_Test()
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
  compOde[0] = "A1[T] = ( 123456789012345.12345*THETA2*THETA4*A2+1.0*THETA7*U*A1-1.0*THETA1*A1^2-THETA3*A1+10.3*THETA1*THETA4*A1-0.00384*THETA7*U*THETA4+THETA2*A2*A1 ) / ( THETA4+A1 )";
  compOde[1] = "A2[T] = 0.0";

  std::vector< std::string > observEqn( nObservType );

  // Set the observation equations.
  observEqn[0] = "Y = ( -5.0*THETA6^2.0*A1 + 1/100 - 2.5*THETA1 ) / ( 1 )";


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

  // This test is Example 1 from the paper:
  //
  //     S. Audoly, G. Bella, L. D'Angio, M. P. Saccomani, and C. Cobelli,
  //     "Global Identifiability of Nonlinear Models of Biological Systems,"
  //     IEEE Transactions on Biomedical Engineering, Vol. 48, pp. 55 - 65,
  //     January 2001.
  //
  // It has infinite solutions, which means a value of -1 here.
  int nGroebnerBasisSolnKnown = -1;


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
  //
  // It has two solutions in the Reduce prototype.
  int nGroebnerBasisSolnKnown = 2;


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
 * Function: multipleGroebnerBasis_eightSolutions_Test
 *
 *
 * This test has two Groebner bases (using BLAD) each with 4 solutions.
 *
 *************************************************************************/

void checkParamIdentTest::multipleGroebnerBasis_eightSolutions_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //----------------------------------------------------------
  // Prepare the parameters that will be checked to be identifiable.
  //----------------------------------------------------------

  // Set the number of THETA elements.
  int nTheta = 13;

  char* thetaNameCStr[] = { "MTOT", "KI", "KA", "K8", "K7", "K6", "K5", "K4", "K3", "K2", "K1", "I1", "A" };

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
  int nObservType = 3;

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
  int nComp = 3;

  std::vector< std::string > compOde( nComp );

  // Set the differential equations for each compartment.
  compOde[0] = "A1[T] = U + K1*((1+A*A1/KA)/(1+A1/KA))*A2*(MTOT - A1)/(K2+(MTOT - A1)) - K3*A1*A3/(K4+A1)";
  compOde[1] = "A2[T] = K5*(1+A1/KI)/(1+I1*A1/KI) - K6*A2";
  compOde[2] = "A3[T] = K7 - K8*A3";

  std::vector< std::string > observEqn( nObservType );

  // Set the observation equations.
  observEqn[0] = "Y1 = A1";
  observEqn[1] = "Y2 = A2";
  observEqn[2] = "Y3 = A3";


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
  //
  // It has eight solutions in the Reduce prototype.
  int nGroebnerBasisSolnKnown = 8;


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
 * Function: fourCompModel_sixSolutions_Test
 *
 *
 * This test's model is known to have 6 solutions.
 *
 *************************************************************************/

void checkParamIdentTest::fourCompModel_sixSolutions_Test()
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

  char* thetaNameCStr[] = { "K01", "K12", "K13", "K14", "K21", "K31", "K41" };

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
  int nComp = 4;

  std::vector< std::string > compOde( nComp );

  // Set the differential equations for each compartment.
  compOde[0] = "A1[T] = -A1*K31-A1*K21+U+A3*K13+A2*K12-A1*K41+A4*K14-A1*K01";
  compOde[1] = "A2[T] = A1*K21-A2*K12";
  compOde[2] = "A3[T] = A1*K31-A3*K13";
  compOde[3] = "A4[T] = A1*K41-A4*K14";

  std::vector< std::string > observEqn( nObservType );

  // Set the observation equations.
  observEqn[0] = "Y1 = A1";


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
  //
  // It is known to have six solutions.
  int nGroebnerBasisSolnKnown = 6;


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
 * Function: noExhaustSummModel_zeroSolutions_Test
 *
 *
 * This test's model has no exhaustive summary and therefore has zero
 * solutions.
 *
 *************************************************************************/

void checkParamIdentTest::noExhaustSummModel_zeroSolutions_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //----------------------------------------------------------
  // Prepare the parameters that will be checked to be identifiable.
  //----------------------------------------------------------

  // Set the number of THETA elements.
  int nTheta = 1;

  char* thetaNameCStr[] = { "P1" };

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
  compOde[0] = "A1[T]=U*A1/(A2*A2)";
  compOde[1] = "A2[T]= 1";

  std::vector< std::string > observEqn( nObservType );

  // Set the observation equations.
  observEqn[0] = "Y=P1*A1";


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
  //
  // It is known to have zero solutions because its exhaustive summary
  // is empty.
  int nGroebnerBasisSolnKnown = 0;


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
 * Function: threeCompModel_unknownSolutions_Test
 *
 *
 * This test's model has three compartments and an unknown number of
 * solutions.
 *
 *************************************************************************/

void checkParamIdentTest::threeCompModel_unknownSolutions_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //----------------------------------------------------------
  // Prepare the parameters that will be checked to be identifiable.
  //----------------------------------------------------------

  // Set the number of THETA elements.
  int nTheta = 6;

  char* thetaNameCStr[] = { "K10", "K12", "K21", "K23", "K32", "VOL" };

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
  int nComp = 3;

  std::vector< std::string > compOde( nComp );

  // Set the differential equations for each compartment.
  compOde[0] = "A1[T] = -(K10 +K12)*A1 +A2*K21";
  compOde[1] = "A2[T] = +A1*K12 -(K21 + K23)*A2 + A3*K32";
  compOde[2] = "A3[T] = +K23*A2 - A3*K32  + U";

  std::vector< std::string > observEqn( nObservType );

  // Set the observation equations.
  observEqn[0] = "Y = A1/VOL";


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
  //
  // It has an unknown number of solutions.
  int nGroebnerBasisSolnKnown = 0;


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  // Compare the calculated and known number of Groebner basis.
  CPPUNIT_ASSERT_MESSAGE( 
    "The calculated and known values for the number of Groebner bases do not agree.",
    nGroebnerBasisSolnKnown == nGroebnerBasisSoln );

}


