/**
 * \file pop_monteTest.h
 * \brief Tests the NonmemTranslator's ability to handle
 * data sets containing the ID values.
 */
#ifndef POP_MONTE_TEST_H
#define POP_MONTE_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>

class pop_monteTest : public CppUnit::TestFixture {

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
    void testMontePars_h();
    void testDriver();
    void testReportML();
};

#endif
