/**
 * \file ind_dataSetTest.h
 * \brief Tests the geneated DataSet class member functions
 */
#ifndef IND_DATASET_TEST_H
#define IND_DATASET_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include "../../spkcompiler/nonmem/XmlConstants.h"

class ind_dataSetTest 
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
    void testExpand();
    void testGetMeasurementIndex();
};

#endif
