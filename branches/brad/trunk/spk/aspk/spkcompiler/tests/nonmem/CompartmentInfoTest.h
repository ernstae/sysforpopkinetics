/**
 * \file CompartmentInfoTest
 * \brief Tests a class representation of a compartment.
 */
#ifndef COMPARTMENTINFO_TEST_H
#define COMPARTMENTINFO_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>

class CompartmentInfoTest
: public CppUnit::TestFixture
 {

public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void testDefaultConst();
    void testConsts();
    void testCopy();
    void testDestruct();
};

#endif
