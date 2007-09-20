/**
 * \file pop_diffeqnTest.h
 * \brief Tests the NonmemTranslator's ability to handle
 * $DES (Differential Equation) model specifications 
 * as opposed to $PRED model.
 */
#ifndef POP_DIFFEQN_TEST_H
#define POP_DIFFEQN_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/NonmemTranslator.h"

class pop_diffeqnTest 
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
    void testIndDataClass();
    void testDataSetClass();
    void testODEPredClass();
    void testDriver();
    void testReportML();
};

#endif
