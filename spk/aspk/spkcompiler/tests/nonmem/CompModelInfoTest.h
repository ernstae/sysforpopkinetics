/**
 * \file CompModelInfoTest
 * \brief Tests a class representation of a compartment model (system).
 */
#ifndef COMPMODELINFO_TEST_H
#define COMPMODELINFO_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>

class CompModelInfoTest
: public CppUnit::TestFixture
 {

public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void testConstructorsWithCompartments();
    void testConstructorsWithoutCompartments();
    void testCopy();
    void testDestruct();
};

#endif
