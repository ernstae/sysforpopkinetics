/**
 * \file pop_nonparamMethodRandomUniformTest.h
 * \brief Tests the NonmemTranslator's ability to handle requests for 
 * the nonparametric method.
 */
#ifndef POP_NONPARAMMETHODRANDOMUNIFORM_TEST_H
#define POP_NONPARAMMETHODRANDOMUNIFORM_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/XmlConstants.h"

class pop_nonparamMethodRandomUniformTest 
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
    void testPredClass();
    void testDriver();
    void testReportML();
};

#endif
