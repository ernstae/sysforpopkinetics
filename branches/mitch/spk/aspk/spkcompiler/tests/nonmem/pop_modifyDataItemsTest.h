/**
 * \file pop_modifyDataItemsTest.h
 * \brief Tests the NonmemTranslator's ability to handle the
 * data sets missing AMT, MDV or/and EVID.
 */
#ifndef POP_MODIFYDATAITEMS_TEST_H
#define POP_MODIFYDATAITEMS_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include "../../spkcompiler/nonmem/XmlConstants.h"

#include <vector>

class pop_modifyDataItemsTest 
: public CppUnit::TestFixture
{

  xercesc::DOMDocument *source;
  xercesc::DOMDocument *data;
  xercesc::DOMDocument *report;

  xercesc::XercesDOMParser * dataParser;
  xercesc::XercesDOMParser * sourceParser;
  
  XmlConstants XML;

  bool okToClean;
public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void createDataML  ( const char * fDataML, 
			 int nIndividuals,
			 int nItems, 
			 const char* label[],
			 int nRecords, 
			 const std::vector< std::vector<double> > & set );
    void parseDataML   ( xercesc::XercesDOMParser *dataParser, 
			 const char * fDataML,
			 int nIndividuals );
    void createSourceML( const char * fSourceML,
			 int nIndividuals,
			 int nLabels, 
			 const char* label[] );
    void parseSourceML ( xercesc::XercesDOMParser *sourceParser, 
			 const char* fSourceML,
			 int nIndividuals );

    void noAMT_noMDV_noEVID();
    void noAMT_noMDV_yesEVID();
    void noAMT_yesMDV_noEVID();
    void noAMT_yesMDV_yesEVID();
    void yesAMT_noMDV_noEVID();
    void yesAMT_noMDV_yesEVID();
    void yesAMT_yesMDV_noEVID();
    void yesAMT_yesMDV_yesEVID();
    void drop();
    void skip();
};

#endif
