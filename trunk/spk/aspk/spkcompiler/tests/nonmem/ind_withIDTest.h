/**
 * \file ind_withIDTest.h
 * \brief Tests the NonmemTranslator's ability to handle
 * data sets containing the ID values.
 */
#ifndef IND_WITHID_TEST_H
#define IND_WITHID_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/NonmemTranslator.h"

class ind_withIDTest 
: public CppUnit::TestFixture,
  public NonmemTranslator
{

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
    void testIndDataClass();
    void testDataSetClass();
    void testPredClass();
    void testDriver();
    void testReportML();
};

#endif
