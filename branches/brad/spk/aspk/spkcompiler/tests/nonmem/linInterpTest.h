/**
 * \file linInterpTest.h
 * \brief Tests the NonmemTranslator's ability to handle the
 * LININTERP (general differential quation) model specification.
 */
#ifndef POP_LININTERP_TEST_H
#define POP_LININTERP_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/XmlConstants.h"

class linInterpTest 
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
