/**
 * \file ind_identTest.h
 * \brief Tests the NonmemTranslator's ability to handle an
 * individual identifiability calculation.
 */
#ifndef IND_IDENT_TEST_H
#define IND_IDENT_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/XmlConstants.h"

class ind_identTest 
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
    void testIdentPredClass();
    void testDriver();
    void testReportML();
};

#endif
