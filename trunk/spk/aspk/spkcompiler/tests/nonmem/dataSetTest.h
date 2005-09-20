/**
 * \file dataSetTest.h
 * \brief Tests the geneated DataSet class member functions
 */
#ifndef DATASET_TEST_H
#define DATASET_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/NonmemTranslator.h"

class dataSetTest 
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
    void testExpand();
    void testGetMeasurementIndex();
};

#endif
