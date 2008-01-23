/**
 * \file ind_subprobTest.h
 * \brief Tests the NonmemTranslator's ability to handle
 * repeating the whole process (simulation, opt, stat).
 */
#ifndef IND_SUBPROB_TEST_H
#define IND_SUBPROB_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/XmlConstants.h"

class ind_subprobTest 
: public CppUnit::TestFixture {

  xercesc::DOMDocument *source;
  xercesc::DOMDocument *data;
  
  XmlConstants XML;

  bool okToClean;
public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void createDataML();
    void createSourceML();
    void parse();
    void testDriver();
    void testReportML();
};

#endif
