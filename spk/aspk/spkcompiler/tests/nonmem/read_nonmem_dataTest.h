/**
 * \file read_nonmem_dataTest.h
 * \brief A test suite for the read_nonmem_data() function.
 */
#ifndef TEST_READ_NONMEM_DATA_H
#define TEST_READ_NONMEM_DATA_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>
#include <spkcompiler/nonmem/read_nonmem_data.h>
#include <xercesc/dom/DOM.hpp>

class read_nonmem_dataTest : public CppUnit::TestFixture {

  xercesc::DOMDocument * doc;
  xercesc::DOMImplementation * impl;
  void createDataTree(
        xercesc::DOMDocument*,
	int, char *[], char *[], char *[],
	int, char *[], char *[],
	int, char *[]);
public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void testOrderMixedUp();
};

#endif
