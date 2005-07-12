/**
 * \file pop_subprobTest.h
 * \brief Tests the NonmemTranslator's ability to handle
 * data sets containing the ID values.
 */
#ifndef POP_SUBPROB_TEST_H
#define POP_SUBPROB_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/NonmemTranslator.h"

class pop_subprobTest 
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
    void testDriver();
    void testReportML();
};

#endif
