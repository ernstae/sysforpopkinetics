/**
 * \file read_nonmem_modelTest.h
 * \brief A test suite for the read_nonmem_model() function.
 */
#ifndef TEST_READ_NONMEM_MODEL_H
#define TEST_READ_NONMEM_MODEL_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>
#include <spkcompiler/nonmem/read_nonmem_model.h>
#include <xercesc/dom/DOM.hpp>

class read_nonmem_modelTest : public CppUnit::TestFixture {

  xercesc::DOMDocument * doc;
  xercesc::DOMImplementation * impl;
  void create_advan2_trans2( xercesc::DOMDocument * doc, SymbolTable * table );
  void create_pred( xercesc::DOMDocument * doc, SymbolTable * table );
  public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void test();
};

#endif
