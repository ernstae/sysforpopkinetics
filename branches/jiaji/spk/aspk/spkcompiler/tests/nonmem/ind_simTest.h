/**
 * \file ind_simTest.h
 * \brief Tests the NonmemTranslator's ability to handle
 * data sets containing the ID values.
 */
#ifndef IND_SIM_TEST_H
#define IND_SIM_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/XmlConstants.h"

class ind_simTest 
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
    void testDataSetClass();
    void testDriver();
};

#endif