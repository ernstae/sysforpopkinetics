/**
 * \file NonmemDataReaderTest.h
 * \brief A test suite for NonmemDataReader.
 */
#ifndef TEST_NONMEM_DATAREADER_H
#define TEST_NONMEM_DATAREADER_H

#include <valarray>
#include <string>
#include <map>

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include "SymbolTable.h"

class NonmemDataReaderTest : public CppUnit::TestFixture {

  // LABEL data type:
  // The reason for the key data type being "string"
  // instead of "char*" is that, if I use "char*" type, 
  // the memory address is rather used as a key, causing multiple entries
  // getting registered in the map for the same string/name.
  typedef std::string LABEL;

  // ALIAS data type:
  // The reason for the key data type being "string"
  // instead of "char*" is to be consistent with LABEL data type.
  typedef std::string ALIAS;

  // MEASUREMENT data type:
  // Let's just make them all expressed in double-precision.
  typedef std::valarray<double> MEASUREMENT;

  char input[128];
  xercesc::XercesDOMParser * parser;
  xercesc::DOMDocument     * dataTree;
  void read_data( 
		 xercesc::DOMDocument* dataTree, 
		 int nIndividuals,
		 SymbolTable* table,
		 std::map<LABEL, ALIAS> label_alias_mapping,
		 std::map< LABEL, MEASUREMENT > data_for[],
		 const char* order_id_pair[]
		 );
  std::vector<std::string> emit_data( 		
		 int nIndividuals,
		 SymbolTable* table,
		 const std::map<LABEL, ALIAS> label_alias_mapping,
		 const std::map< LABEL, MEASUREMENT > data_for[],
		 const char* const order_id_pair[]
		 );
public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void testNoSkip();
    void testWithSkip();
};

#endif
