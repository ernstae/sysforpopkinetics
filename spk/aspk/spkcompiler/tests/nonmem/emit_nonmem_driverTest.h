/**
 * @file emit_nonmem_driverTest.h
 * 
 * @brief A test suite for emit_nonmem_driver() function.
 *
 */
#ifndef TEST_EMIT_NONMEM_DRIVER_H
#define TEST_EMIT_NONMEM_DRIVER_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>
#include <libspkcompiler/SpkParameters.h>
#include <libspkcompiler/NonmemTranslator.h>

/**
 * A class that organizes a suite of tests for emit_nonmem_driver() function.
 */
class emit_nonmem_driverTest : public CppUnit::TestFixture {
  void prepInfo( struct SpkParameters &, struct NonmemParameters & );
public:
    virtual void setUp();
    virtual void tearDown();
    
    void test();
    void testPopSimOnly( char file_name[],
			 char modelClass_name[],
			 char modelObject_init_block[],
			 struct SpkParameters & spk,
		         struct NonmemParameters & nonmem );

    void testPopEstOnly( char file_name[],
			 char modelClass_name[],
			 char modelObject_init_block[],
			 struct SpkParameters & spk,
		         struct NonmemParameters & nonmem );

    void testPopSimEst ( char file_name[],
			 char modelClass_name[],
			 char modelObject_init_block[],
			 struct SpkParameters & spk,
		         struct NonmemParameters & nonmem );

    void testPopSimEstStat( 
			   char file_name[],
			   char modelClass_name[],
			   char modelObject_init_block[],
			   struct SpkParameters & spk,
			   struct NonmemParameters & nonmem );

    /*
    void testPopSimEstStat( char file_name[],
			 char model_name[], 
			 struct NonmemParameters & spk );
    */
    /*
    void testIndSimEst ( char file_name[],
			 char model_name[], 
			 struct SpkParameters & spk );
    void testIndSimOnly( char file_name[],
			 char model_name[], 
			 struct SpkParameters & spk );
    void testIndEstOnly( char file_name[],
			 char model_name[], 
			 struct SpkParameters & spk );
    */
    static CppUnit::Test * suite();
};

#endif
