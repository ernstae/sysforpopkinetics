/**
 * \file NonmemTranslatorPopTest.h
 * \brief A population level test suite for the NonmemTranslator class.
 */
#ifndef TEST_NONMEMTRANSLATOR_POP_TEST_H
#define TEST_NONMEMTRANSLATOR_POP_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>

class NonmemTranslatorPopTest : public CppUnit::TestFixture {

  char *gSource;

  xercesc::DOMDocument *source;
  xercesc::DOMDocument *data;
public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void testParsePopSource();
};

#endif
