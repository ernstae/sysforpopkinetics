#include <iostream>
#include <vector>
#include <string>

#include "CompartmentInfoTest.h"
#include "../../spkcompiler/nonmem/CompartmentInfo.h"
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
// Tests if the default constructor initializes the object properly.
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void CompartmentInfoTest::testDefaultConst()
{
   bool initial_off         = false;
   bool no_off              = false;
   bool no_dose             = false;
   bool equilibrim          = false;
   bool exclude             = false;
   bool default_observation = false;
   bool default_dose        = false;
   CompartmentInfo comp;
   CPPUNIT_ASSERT_MESSAGE( "initial_off",         initial_off         == comp.is_initial_off() ); 
   CPPUNIT_ASSERT_MESSAGE( "no_off",              no_off              == comp.is_no_off() );
   CPPUNIT_ASSERT_MESSAGE( "no_dose",             no_dose             == comp.is_no_dose() );
   CPPUNIT_ASSERT_MESSAGE( "equilibrim",          equilibrim          == comp.is_equilibrim() );
   CPPUNIT_ASSERT_MESSAGE( "exclude",             exclude             == comp.is_exclude() );
   CPPUNIT_ASSERT_MESSAGE( "default_observation", default_observation == comp.is_default_observation() );
   CPPUNIT_ASSERT_MESSAGE( "default_dose",        default_dose        == comp.is_default_dose() );
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
// testDefaultConst
//
// Tests if the non-default constructors initialize the object properly.
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void CompartmentInfoTest::testConsts()
{
   const char * const name  = "COMP1";
   bool initial_off         = true;
   bool no_off              = true;
   bool no_dose             = true;
   bool equilibrim          = true;
   bool exclude             = true;
   bool default_observation = true;
   bool default_dose        = true;
   CompartmentInfo comp1( name, initial_off, no_off, no_dose, equilibrim, exclude, default_observation, default_dose );

   //CPPUNIT_ASSERT( strcmp( name, comp1.getName() ) == 0 );
   CPPUNIT_ASSERT( name == comp1.getName() );
   CPPUNIT_ASSERT_MESSAGE( "initial_off",         initial_off         == comp1.is_initial_off() ); 
   CPPUNIT_ASSERT_MESSAGE( "no_off",              no_off              == comp1.is_no_off() );
   CPPUNIT_ASSERT_MESSAGE( "no_dose",             no_dose             == comp1.is_no_dose() );
   CPPUNIT_ASSERT_MESSAGE( "equilibrim",          equilibrim          == comp1.is_equilibrim() );
   CPPUNIT_ASSERT_MESSAGE( "exclude",             exclude             == comp1.is_exclude() );
   CPPUNIT_ASSERT_MESSAGE( "default_observation", default_observation == comp1.is_default_observation() );
   CPPUNIT_ASSERT_MESSAGE( "default_dose",        default_dose        == comp1.is_default_dose() );

   CompartmentInfo comp2( name );
   //CPPUNIT_ASSERT( strcmp( name, comp2.getName() ) == 0 );
   CPPUNIT_ASSERT( name == comp2.getName() );
   CPPUNIT_ASSERT_MESSAGE( "initial_off",         false == comp2.is_initial_off() ); 
   CPPUNIT_ASSERT_MESSAGE( "no_off",              false == comp2.is_no_off() );
   CPPUNIT_ASSERT_MESSAGE( "no_dose",             false == comp2.is_no_dose() );
   CPPUNIT_ASSERT_MESSAGE( "equilibrim",          false == comp2.is_equilibrim() );
   CPPUNIT_ASSERT_MESSAGE( "exclude",             false == comp2.is_exclude() );
   CPPUNIT_ASSERT_MESSAGE( "default_observation", false == comp2.is_default_observation() );
   CPPUNIT_ASSERT_MESSAGE( "default_dose",        false == comp2.is_default_dose() );
}
void CompartmentInfoTest::testCopy()
{ 
   const char * const name  = "COMP1";
   bool initial_off         = true;
   bool no_off              = true;
   bool no_dose             = true;
   bool equilibrim          = true;
   bool exclude             = true;
   bool default_observation = true;
   bool default_dose        = true;
   CompartmentInfo comp1( name, initial_off, no_off, no_dose, equilibrim, exclude, default_observation, default_dose );

   CompartmentInfo comp1_copy( comp1 ); 
   //CPPUNIT_ASSERT( strcmp( name, comp1_copy.getName() ) == 0 );
   CPPUNIT_ASSERT( name == comp1_copy.getName() );
   CPPUNIT_ASSERT_MESSAGE( "initial_off",         initial_off         == comp1_copy.is_initial_off() ); 
   CPPUNIT_ASSERT_MESSAGE( "no_off",              no_off              == comp1_copy.is_no_off() );
   CPPUNIT_ASSERT_MESSAGE( "no_dose",             no_dose             == comp1_copy.is_no_dose() );
   CPPUNIT_ASSERT_MESSAGE( "equilibrim",          equilibrim          == comp1_copy.is_equilibrim() );
   CPPUNIT_ASSERT_MESSAGE( "exclude",             exclude             == comp1_copy.is_exclude() );
   CPPUNIT_ASSERT_MESSAGE( "default_observation", default_observation == comp1_copy.is_default_observation() );
   CPPUNIT_ASSERT_MESSAGE( "default_dose",        default_dose        == comp1_copy.is_default_dose() );

   CompartmentInfo comp1_assign( comp1 ); 
   //CPPUNIT_ASSERT( strcmp( name, comp1_assign.getName() ) == 0 );
   CPPUNIT_ASSERT( name == comp1_assign.getName() );
   CPPUNIT_ASSERT_MESSAGE( "initial_off",         initial_off         == comp1_assign.is_initial_off() ); 
   CPPUNIT_ASSERT_MESSAGE( "no_off",              no_off              == comp1_assign.is_no_off() );
   CPPUNIT_ASSERT_MESSAGE( "no_dose",             no_dose             == comp1_assign.is_no_dose() );
   CPPUNIT_ASSERT_MESSAGE( "equilibrim",          equilibrim          == comp1_assign.is_equilibrim() );
   CPPUNIT_ASSERT_MESSAGE( "exclude",             exclude             == comp1_assign.is_exclude() );
   CPPUNIT_ASSERT_MESSAGE( "default_observation", default_observation == comp1_assign.is_default_observation() );
   CPPUNIT_ASSERT_MESSAGE( "default_dose",        default_dose        == comp1_assign.is_default_dose() );
}
void CompartmentInfoTest::testDestruct()
{
   {
     CompartmentInfo *comp1 = new CompartmentInfo( "dummy" );
     CompartmentInfo comp2( *comp1 );

     delete comp1;
   }
   
}
void CompartmentInfoTest::setUp()
{
}
void CompartmentInfoTest::tearDown()
{
}
CppUnit::Test * CompartmentInfoTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "CompartmentInfoTest"  );

  suiteOfTests->addTest( 
     new CppUnit::TestCaller<CompartmentInfoTest>(
         "testDefaultConst", 
	 &CompartmentInfoTest::testDefaultConst ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<CompartmentInfoTest>(
         "testConsts", 
	 &CompartmentInfoTest::testConsts ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<CompartmentInfoTest>(
         "testCopy", 
	 &CompartmentInfoTest::testCopy ) );
  suiteOfTests->addTest( 
     new CppUnit::TestCaller<CompartmentInfoTest>(
         "testDestruct", 
	 &CompartmentInfoTest::testDestruct ) );
  return suiteOfTests;

}

