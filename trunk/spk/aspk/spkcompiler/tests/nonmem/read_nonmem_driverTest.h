/**
 * \file read_nonmem_driverTest.h
 * \brief A test suite for the read_nonmem_driver() function.
 */
#ifndef TEST_READ_NONMEM_DRIVER_H
#define TEST_READ_NONMEM_DRIVER_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>
#include <nonmem/read_nonmem_driver.h>

class read_nonmem_driverTest : public CppUnit::TestFixture {

public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void test();
};

#endif
