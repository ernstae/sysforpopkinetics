#include <iostream>
#include <vector>
#include <string>

#include "CompModelInfoTest.h"
#include "../../spkcompiler/nonmem/CompModelInfo.h"
#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

using namespace std;
using namespace CppUnit;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// testDefaultConst
//
// Tests if the non-default constructors initialize the object properly.
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void CompModelInfoTest::testConstructorsWithCompartments()
{
  int nComps       = 2;
  int nParams      = 3;
  int nEquilibrims = 1;
  CompartmentInfo comp1( "dummy1" );
  CompartmentInfo comp2( "dummy2", 
                         true, /* initial_off */
                         true, /* no_off */
                         true, /* no_dose */
                         true, /* equilibrim */
                         true, /* exclude */
                         true, /* default_observation */
                         true  /* default_doese */ );
  vector<CompartmentInfo> compartments;
  compartments.push_back( comp1 );
  compartments.push_back( comp2 );
  CompModelInfo model_no_equilibrim  ( nComps, nParams, compartments );
  CompModelInfo model_with_equilibrim( nComps, nParams, nEquilibrims, compartments );
  
  CPPUNIT_ASSERT_EQUAL( nComps,  model_no_equilibrim.getNCompartments() );
  CPPUNIT_ASSERT_EQUAL( nParams, model_no_equilibrim.getNParameters() );
  CPPUNIT_ASSERT_EQUAL( 0,       model_no_equilibrim.getNEquilibrims() );
  //CPPUNIT_ASSERT( strcmp( "dummy1", model_no_equilibrim[0].getName() ) == 0 );
  //CPPUNIT_ASSERT( strcmp( "dummy2", model_no_equilibrim[1].getName() ) == 0 );
  CPPUNIT_ASSERT( "dummy1" == model_no_equilibrim[0].getName() );
  CPPUNIT_ASSERT( "dummy2" == model_no_equilibrim[1].getName() );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_initial_off()          == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_initial_off()          == true );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_no_off()               == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_no_off()               == true );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_no_dose()              == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_no_dose()              == true );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_equilibrim()           == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_equilibrim()           == true );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_exclude()              == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_exclude()              == true );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_default_observation()  == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_default_observation()  == true );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_default_dose()         == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_default_dose()         == true );

  CPPUNIT_ASSERT_EQUAL( nComps,       model_with_equilibrim.getNCompartments() );
  CPPUNIT_ASSERT_EQUAL( nParams,      model_with_equilibrim.getNParameters() );
  CPPUNIT_ASSERT_EQUAL( nEquilibrims, model_with_equilibrim.getNEquilibrims() );
}
void CompModelInfoTest::testConstructorsWithoutCompartments()
{
  int nComps       = 2;
  int nParams      = 3;
  int nEquilibrims = 1;
  CompModelInfo model_no_equilibrim  ( nComps, nParams );
  CompModelInfo model_with_equilibrim( nComps, nParams, nEquilibrims );
  
  CPPUNIT_ASSERT_EQUAL( nComps,  model_no_equilibrim.getNCompartments() );
  CPPUNIT_ASSERT_EQUAL( nParams, model_no_equilibrim.getNParameters() );
  CPPUNIT_ASSERT_EQUAL( 0,       model_no_equilibrim.getNEquilibrims() );
  //CPPUNIT_ASSERT( strcmp( "COMP1", model_no_equilibrim[0].getName() ) == 0 );
  //CPPUNIT_ASSERT( strcmp( "COMP2", model_no_equilibrim[1].getName() ) == 0 );
  //CPPUNIT_ASSERT( "COMP1" == model_no_equilibrim[0].getName() );
  //CPPUNIT_ASSERT( "COMP2" == model_no_equilibrim[1].getName() );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_initial_off()          == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_initial_off()          == false );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_no_off()               == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_no_off()               == false );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_no_dose()              == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_no_dose()              == false );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_equilibrim()           == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_equilibrim()           == false );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_exclude()              == false );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_exclude()              == false );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_default_observation()  == true );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_default_observation()  == false );
  CPPUNIT_ASSERT( model_no_equilibrim[0].is_default_dose()         == true );
  CPPUNIT_ASSERT( model_no_equilibrim[1].is_default_dose()         == false );

  CPPUNIT_ASSERT_EQUAL( nComps,       model_with_equilibrim.getNCompartments() );
  CPPUNIT_ASSERT_EQUAL( nParams,      model_with_equilibrim.getNParameters() );
  CPPUNIT_ASSERT_EQUAL( nEquilibrims, model_with_equilibrim.getNEquilibrims() );
}
void CompModelInfoTest::testCopy()
{
  int nComps       = 2;
  int nParams      = 3;
  int nEquilibrims = 1;
  CompartmentInfo comp1( "dummy1" );
  CompartmentInfo comp2( "dummy2", 
                         true, /* initial_off */
                         true, /* no_off */
                         true, /* no_dose */
                         true, /* equilibrim */
                         true, /* exclude */
                         true, /* default_observation */
                         true  /* default_doese */ );
  vector<CompartmentInfo> compartments;
  compartments.push_back( comp1 );
  compartments.push_back( comp2 );
  CompModelInfo model1( nComps, nParams, nEquilibrims, compartments );
  CompModelInfo model_copy( model1 );
 
  CPPUNIT_ASSERT_EQUAL( model1.getNCompartments(), model_copy.getNCompartments() );
  CPPUNIT_ASSERT_EQUAL( model1.getNParameters(),   model_copy.getNParameters() );
  CPPUNIT_ASSERT_EQUAL( model1.getNEquilibrims(),  model_copy.getNEquilibrims() );
  //CPPUNIT_ASSERT( strcmp( model1[0].getName(), model_copy[0].getName() ) == 0 );
  //CPPUNIT_ASSERT( strcmp( model1[1].getName(), model_copy[1].getName() ) == 0 );
  CPPUNIT_ASSERT( model1[0].getName() == model_copy[0].getName() );
  CPPUNIT_ASSERT( model1[1].getName() == model_copy[1].getName() );
  CPPUNIT_ASSERT( model1[0].is_initial_off()          == model_copy[0].is_initial_off() );
  CPPUNIT_ASSERT( model1[1].is_initial_off()          == model_copy[1].is_initial_off() );
  CPPUNIT_ASSERT( model1[0].is_no_off()               == model_copy[0].is_no_off() );
  CPPUNIT_ASSERT( model1[1].is_no_off()               == model_copy[1].is_no_off() );
  CPPUNIT_ASSERT( model1[0].is_no_dose()              == model_copy[0].is_no_dose() );
  CPPUNIT_ASSERT( model1[1].is_no_dose()              == model_copy[1].is_no_dose() );
  CPPUNIT_ASSERT( model1[0].is_equilibrim()           == model_copy[0].is_equilibrim() );
  CPPUNIT_ASSERT( model1[1].is_equilibrim()           == model_copy[1].is_equilibrim() );
  CPPUNIT_ASSERT( model1[0].is_exclude()              == model_copy[0].is_exclude() );
  CPPUNIT_ASSERT( model1[1].is_exclude()              == model_copy[1].is_exclude() );
  CPPUNIT_ASSERT( model1[0].is_default_observation()  == model_copy[0].is_default_observation() );
  CPPUNIT_ASSERT( model1[1].is_default_observation()  == model_copy[1].is_default_observation() );
  CPPUNIT_ASSERT( model1[0].is_default_dose()         == model_copy[0].is_default_dose() );
  CPPUNIT_ASSERT( model1[1].is_default_dose()         == model_copy[1].is_default_dose() );
}
void CompModelInfoTest::testDestruct()
{
  int nComps       = 2;
  int nParams      = 3;
  int nEquilibrims = 1;
  CompartmentInfo comp1( "dummy1" );
  CompartmentInfo comp2( "dummy2", 
                         true, /* initial_off */
                         true, /* no_off */
                         true, /* no_dose */
                         true, /* equilibrim */
                         true, /* exclude */
                         true, /* default_observation */
                         true  /* default_doese */ );
  vector<CompartmentInfo> compartments;
  compartments.push_back( comp1 );
  compartments.push_back( comp2 );
  {
     CompModelInfo *model = new CompModelInfo( nComps, nParams, nEquilibrims, compartments );
     CompModelInfo model_copy( *model );
     CPPUNIT_ASSERT_EQUAL( nComps,        model_copy.getNCompartments() );
     CPPUNIT_ASSERT_EQUAL( nParams,       model_copy.getNParameters() );
     CPPUNIT_ASSERT_EQUAL( nEquilibrims,  model_copy.getNEquilibrims() );
     //CPPUNIT_ASSERT( strcmp( model_copy[0].getName(), "dummy1" ) == 0 );
     //CPPUNIT_ASSERT( strcmp( model_copy[1].getName(), "dummy2" ) == 0 );
     CPPUNIT_ASSERT( model_copy[0].getName() == "dummy1" );
     CPPUNIT_ASSERT( model_copy[1].getName() == "dummy2" );
     CPPUNIT_ASSERT( model_copy[0].is_initial_off()          == false );
     CPPUNIT_ASSERT( model_copy[1].is_initial_off()          == true );
     CPPUNIT_ASSERT( model_copy[0].is_no_off()               == false );
     CPPUNIT_ASSERT( model_copy[1].is_no_off()               == true );
     CPPUNIT_ASSERT( model_copy[0].is_no_dose()              == false );
     CPPUNIT_ASSERT( model_copy[1].is_no_dose()              == true );
     CPPUNIT_ASSERT( model_copy[0].is_equilibrim()           == false );
     CPPUNIT_ASSERT( model_copy[1].is_equilibrim()           == true );
     CPPUNIT_ASSERT( model_copy[0].is_exclude()              == false );
     CPPUNIT_ASSERT( model_copy[1].is_exclude()              == true );
     CPPUNIT_ASSERT( model_copy[0].is_default_observation()  == false );
     CPPUNIT_ASSERT( model_copy[1].is_default_observation()  == true );
     CPPUNIT_ASSERT( model_copy[0].is_default_dose()         == false );
     CPPUNIT_ASSERT( model_copy[1].is_default_dose()         == true );

     delete model;
     CPPUNIT_ASSERT_EQUAL( nComps,        model_copy.getNCompartments() );
     CPPUNIT_ASSERT_EQUAL( nParams,       model_copy.getNParameters() );
     CPPUNIT_ASSERT_EQUAL( nEquilibrims,  model_copy.getNEquilibrims() );
     //CPPUNIT_ASSERT( strcmp( model_copy[0].getName(), "dummy1" ) == 0 );
     //CPPUNIT_ASSERT( strcmp( model_copy[1].getName(), "dummy2" ) == 0 );
     CPPUNIT_ASSERT( model_copy[0].getName() == "dummy1" );
     CPPUNIT_ASSERT( model_copy[1].getName() == "dummy2" );
     CPPUNIT_ASSERT( model_copy[0].is_initial_off()          == false );
     CPPUNIT_ASSERT( model_copy[1].is_initial_off()          == true );
     CPPUNIT_ASSERT( model_copy[0].is_no_off()               == false );
     CPPUNIT_ASSERT( model_copy[1].is_no_off()               == true );
     CPPUNIT_ASSERT( model_copy[0].is_no_dose()              == false );
     CPPUNIT_ASSERT( model_copy[1].is_no_dose()              == true );
     CPPUNIT_ASSERT( model_copy[0].is_equilibrim()           == false );
     CPPUNIT_ASSERT( model_copy[1].is_equilibrim()           == true );
     CPPUNIT_ASSERT( model_copy[0].is_exclude()              == false );
     CPPUNIT_ASSERT( model_copy[1].is_exclude()              == true );
     CPPUNIT_ASSERT( model_copy[0].is_default_observation()  == false );
     CPPUNIT_ASSERT( model_copy[1].is_default_observation()  == true );
     CPPUNIT_ASSERT( model_copy[0].is_default_dose()         == false );
     CPPUNIT_ASSERT( model_copy[1].is_default_dose()         == true );

  }
}
void CompModelInfoTest::setUp()
{
}
void CompModelInfoTest::tearDown()
{
}
CppUnit::Test * CompModelInfoTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "CompModelInfoTest"  );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<CompModelInfoTest>(
         "testConstructorsWithCompartments", 
	 &CompModelInfoTest::testConstructorsWithCompartments ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<CompModelInfoTest>(
         "testConstructorsWithoutCompartments", 
	 &CompModelInfoTest::testConstructorsWithoutCompartments ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<CompModelInfoTest>(
         "testCopy", 
	 &CompModelInfoTest::testCopy ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<CompModelInfoTest>(
         "testDestruct", 
	 &CompModelInfoTest::testDestruct ) );
  return suiteOfTests;

}

