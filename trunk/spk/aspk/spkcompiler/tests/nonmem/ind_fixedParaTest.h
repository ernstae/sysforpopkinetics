/**
 * \file ind_fixedParaTest.h
 * \brief Tests the NonmemTranslator's ability to handle
 * data sets containing the ID values.
 */
#ifndef IND_FIXEDPARA_TEST_H
#define IND_FIXEDPARA_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>

class ind_fixedParaTest : public CppUnit::TestFixture {

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
    void testNonmemPars_h();
    void testDriver();
    void testReportML();
};

#endif
