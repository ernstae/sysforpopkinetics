/**
 * \file NonmemDataReaderTest.h
 * \brief A test suite for NonmemDataReader.
 */
#ifndef TEST_NONMEM_DATAREADER_H
#define TEST_NONMEM_DATAREADER_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

class NonmemDataReaderTest : public CppUnit::TestFixture {
  char input[128];
  xercesc::XercesDOMParser * parser;
  xercesc::DOMDocument     * dataTree;
  std::vector<const char*> translate_data( xercesc::DOMDocument* dataTree );

public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void testNoSkip();
    void testWithSkip();
};

#endif
