#include <iostream>
#include <vector>
#include <string>
#include <cmath>

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
  double relTol    = pow( 10.0, -2 );
  bool isPkFunctionOfT = true;
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
  CompModelInfo model( nComps, nParams, nEquilibrims, relTol );
  model.getCompartment(0) = comp1;
  model.getCompartment(1) = comp2;
  model.setPkFunctionOfT ( isPkFunctionOfT );
  
  vector<bool> initial_off( nComps );
  model.getInitialOff( initial_off );
  CPPUNIT_ASSERT( initial_off[0] == false );
  CPPUNIT_ASSERT( initial_off[1] == true );
  vector<bool> no_off( nComps );
  model.getNoOff( no_off );
  CPPUNIT_ASSERT( no_off[0] == false );
  CPPUNIT_ASSERT( no_off[1] == true );
  vector<bool> no_dose( nComps );
  model.getNoDose( no_dose );
  CPPUNIT_ASSERT( no_dose[0] == false );
  CPPUNIT_ASSERT( no_dose[1] == true );

  CPPUNIT_ASSERT_EQUAL( nComps,           model.getNCompartments() );
  CPPUNIT_ASSERT_EQUAL( nParams,          model.getNParameters() );
  CPPUNIT_ASSERT_EQUAL( nEquilibrims,     model.getNEquilibrims() );
  CPPUNIT_ASSERT_EQUAL( isPkFunctionOfT,  model.isPkFunctionOfT() );

  CPPUNIT_ASSERT( "dummy1" == model[0].getName() );
  CPPUNIT_ASSERT( "dummy2" == model[1].getName() );
  CPPUNIT_ASSERT( model.isPkFunctionOfT()            == true  );
  CPPUNIT_ASSERT( model[0].is_initial_off()          == false );
  CPPUNIT_ASSERT( model[1].is_initial_off()          == true );
  CPPUNIT_ASSERT( model[0].is_no_off()               == false );
  CPPUNIT_ASSERT( model[1].is_no_off()               == true );
  CPPUNIT_ASSERT( model[0].is_no_dose()              == false );
  CPPUNIT_ASSERT( model[1].is_no_dose()              == true );
  CPPUNIT_ASSERT( model[0].is_equilibrim()           == false );
  CPPUNIT_ASSERT( model[1].is_equilibrim()           == true );
  CPPUNIT_ASSERT( model[0].is_exclude()              == false );
  CPPUNIT_ASSERT( model[1].is_exclude()              == true );
  CPPUNIT_ASSERT( model[0].is_default_observation()  == false );
  CPPUNIT_ASSERT( model[1].is_default_observation()  == true );
  CPPUNIT_ASSERT( model[0].is_default_dose()         == false );
  CPPUNIT_ASSERT( model[1].is_default_dose()         == true );

  CPPUNIT_ASSERT( "dummy1" == model.getCompartment(0).getName() );
  CPPUNIT_ASSERT( "dummy2" == model.getCompartment(1).getName() );
  CPPUNIT_ASSERT( model.isPkFunctionOfT()            == true  );
  CPPUNIT_ASSERT( model.getCompartment(0).is_initial_off()          == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_initial_off()          == true );
  CPPUNIT_ASSERT( model.getCompartment(0).is_no_off()               == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_no_off()               == true );
  CPPUNIT_ASSERT( model.getCompartment(0).is_no_dose()              == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_no_dose()              == true );
  CPPUNIT_ASSERT( model.getCompartment(0).is_equilibrim()           == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_equilibrim()           == true );
  CPPUNIT_ASSERT( model.getCompartment(0).is_exclude()              == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_exclude()              == true );
  CPPUNIT_ASSERT( model.getCompartment(0).is_default_observation()  == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_default_observation()  == true );
  CPPUNIT_ASSERT( model.getCompartment(0).is_default_dose()         == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_default_dose()         == true );

}
void CompModelInfoTest::testConstructorsWithoutCompartments()
{
  int nComps          = 2;
  int nParams         = 3;
  int nEquilibrims    = 1;
  double relTol    = pow( 10.0, -2 );
  int isPkFunctionOfT = true;
  CompModelInfo model( nComps, nParams, nEquilibrims, relTol );
  model.setPkFunctionOfT( isPkFunctionOfT );
  
  CPPUNIT_ASSERT_EQUAL( nComps,          model.getNCompartments() );
  CPPUNIT_ASSERT_EQUAL( nParams,         model.getNParameters() );
  CPPUNIT_ASSERT_EQUAL( nEquilibrims,    model.getNEquilibrims() );
  CPPUNIT_ASSERT_EQUAL( 1,               model.getDefaultObservation() );
  CPPUNIT_ASSERT_EQUAL( 1,               model.getDefaultDose() );

  CPPUNIT_ASSERT( isPkFunctionOfT == model.isPkFunctionOfT() );

  vector<bool> initial_off( nComps );
  model.getInitialOff( initial_off );
  CPPUNIT_ASSERT( initial_off[0] == false );
  CPPUNIT_ASSERT( initial_off[1] == true );
  vector<bool> no_off( nComps );
  model.getNoOff( no_off );
  CPPUNIT_ASSERT( no_off[0] == false );
  CPPUNIT_ASSERT( no_off[1] == false );
  vector<bool> no_dose( nComps );
  model.getNoDose( no_dose );
  CPPUNIT_ASSERT( no_dose[0] == false );
  CPPUNIT_ASSERT( no_dose[1] == false );

  CPPUNIT_ASSERT( "COMP1" == model[0].getName() );
  CPPUNIT_ASSERT( "COMP2" == model[1].getName() );

  CPPUNIT_ASSERT( model[0].is_initial_off()          == false );
  CPPUNIT_ASSERT( model[1].is_initial_off()          == true );
  CPPUNIT_ASSERT( model[0].is_no_off()               == false );
  CPPUNIT_ASSERT( model[1].is_no_off()               == false );
  CPPUNIT_ASSERT( model[0].is_no_dose()              == false );
  CPPUNIT_ASSERT( model[1].is_no_dose()              == false );
  CPPUNIT_ASSERT( model[0].is_equilibrim()           == false );
  CPPUNIT_ASSERT( model[1].is_equilibrim()           == false );
  CPPUNIT_ASSERT( model[0].is_exclude()              == false );
  CPPUNIT_ASSERT( model[1].is_exclude()              == false );
  CPPUNIT_ASSERT( model[0].is_default_observation()  == true );
  CPPUNIT_ASSERT( model[1].is_default_observation()  == false );
  CPPUNIT_ASSERT( model[0].is_default_dose()         == true );
  CPPUNIT_ASSERT( model[1].is_default_dose()         == false );

  CPPUNIT_ASSERT( model.getCompartment(0).is_initial_off()          == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_initial_off()          == true );
  CPPUNIT_ASSERT( model.getCompartment(0).is_no_off()               == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_no_off()               == false );
  CPPUNIT_ASSERT( model.getCompartment(0).is_no_dose()              == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_no_dose()              == false );
  CPPUNIT_ASSERT( model.getCompartment(0).is_equilibrim()           == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_equilibrim()           == false );
  CPPUNIT_ASSERT( model.getCompartment(0).is_exclude()              == false );
  CPPUNIT_ASSERT( model.getCompartment(1).is_exclude()              == false );
  CPPUNIT_ASSERT( model.getCompartment(0).is_default_observation()  == true );
  CPPUNIT_ASSERT( model.getCompartment(1).is_default_observation()  == false );
  CPPUNIT_ASSERT( model.getCompartment(0).is_default_dose()         == true );
  CPPUNIT_ASSERT( model.getCompartment(1).is_default_dose()         == false );
}
void CompModelInfoTest::testCopy()
{
  int nComps           = 2;
  int nParams          = 3;
  int nEquilibrims     = 1;
  double relTol    = pow( 10.0, -2 );
  bool isPkFunctionOfT = false;
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
  CompModelInfo model1( nComps, nParams, nEquilibrims, relTol );
  model1.getCompartment(0) = comp1;
  model1.getCompartment(1) = comp2;
  model1.setPkFunctionOfT( isPkFunctionOfT );

  CompModelInfo model_copy( model1 );
  CPPUNIT_ASSERT( model1.isPkFunctionOfT() ==  model_copy.isPkFunctionOfT() );

  vector<bool> initial_off( nComps );
  model1.getInitialOff( initial_off );
  CPPUNIT_ASSERT( initial_off[0] == false );
  CPPUNIT_ASSERT( initial_off[1] == true );
  vector<bool> no_off( nComps );
  model1.getNoOff( no_off );
  CPPUNIT_ASSERT( no_off[0] == false );
  CPPUNIT_ASSERT( no_off[1] == true );
  vector<bool> no_dose( nComps );
  model1.getNoDose( no_dose );
  CPPUNIT_ASSERT( no_dose[0] == false );
  CPPUNIT_ASSERT( no_dose[1] == true );

  CPPUNIT_ASSERT( "dummy1" == model1[0].getName() );
  CPPUNIT_ASSERT( "dummy2" == model1[1].getName() );

  CPPUNIT_ASSERT_EQUAL( model1.getNCompartments(), model_copy.getNCompartments() );
  CPPUNIT_ASSERT_EQUAL( model1.getNParameters(),   model_copy.getNParameters() );
  CPPUNIT_ASSERT_EQUAL( model1.getNEquilibrims(),  model_copy.getNEquilibrims() );
  CPPUNIT_ASSERT( model1.isPkFunctionOfT() == model_copy.isPkFunctionOfT()  );
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
  double relTol    = pow( 10.0, -2 );
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
     CompModelInfo *model = new CompModelInfo( nComps, nParams, nEquilibrims, relTol );
     model->getCompartment(0) = comp1;
     model->getCompartment(1) = comp2;
     CompModelInfo model_copy( *model );
     CPPUNIT_ASSERT_EQUAL( nComps,        model_copy.getNCompartments() );
     CPPUNIT_ASSERT_EQUAL( nParams,       model_copy.getNParameters() );
     CPPUNIT_ASSERT_EQUAL( nEquilibrims,  model_copy.getNEquilibrims() );
     CPPUNIT_ASSERT( model_copy.isPkFunctionOfT()           == true );
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
     CPPUNIT_ASSERT( model_copy.isPkFunctionOfT()           == true );
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

