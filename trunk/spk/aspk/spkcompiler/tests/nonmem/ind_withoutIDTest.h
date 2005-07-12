/**
 * \file ind_withoutIDTest.h
 * \brief Tests the NonmemTranslator's ability to handle
 * data sets containing no ID.
 */
#ifndef IND_WITHOUTID_TEST_H
#define IND_WITHOUTID_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/NonmemTranslator.h"

class ind_withoutIDTest 
: public CppUnit::TestFixture,
  public NonmemTranslator {

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
    void testDriver();
};

#endif
