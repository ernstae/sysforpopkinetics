/**
 * \file ind_onlysim_NonmemTranslatorTest.h
 * \brief A test suite for the NonmemTranslator class.
 */
#ifndef IND_ONLYSIM_NONMEMTRANSLATOR_TEST_H
#define IND_ONLYSIM_NONMEMTRANSLATOR_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>

class ind_onlysim_NonmemTranslatorTest : public CppUnit::TestFixture {

  char *gSource;

  xercesc::DOMDocument *source;
  xercesc::DOMDocument *data;
  xercesc::DOMDocument *report;
  
  bool okToClean;
public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void createDataML();
    void createSourceML();
    void parse();
    void testIndDataClass();
    void testDataSetClass();
    void testPredClass();
    void testDriver();
    void testReportML();
};

#endif
