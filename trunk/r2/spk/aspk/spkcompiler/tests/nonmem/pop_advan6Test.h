/**
 * \file pop_advan6Test.h
 * \brief Tests the NonmemTranslator's ability to handle the
 * ADVAN6 (general differential quation) model specification.
 */
#ifndef POP_ADVAN6_TEST_H
#define POP_ADVAN6_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/XmlConstants.h"

class pop_advan6Test 
: public CppUnit::TestFixture
{

  xercesc::DOMDocument *source;
  xercesc::DOMDocument *data;
  xercesc::DOMDocument *report;
  
  XmlConstants XML;

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
    void testNonmemPars();
    void testODEPredClass();
    void testDriver();
    void testReportML();
};

#endif
