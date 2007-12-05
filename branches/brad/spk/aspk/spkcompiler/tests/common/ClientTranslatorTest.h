/**
 * @file clientTest.h
 * @brief A test suite for components decleared/defined in
 * the "client" namespace.
 *
 */
#ifndef TEST_CLIENTTRANSLATOR_H
#define TEST_CLIENTTRANSLATOR_H

#include <fstream>

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <xercesc/dom/DOMDocument.hpp>

#include "spkcompiler/ClientTranslator.h"
/**
 *
 */
class ClientTranslatorTest : public CppUnit::TestFixture {
    char * gData;
    xercesc::DOMDocument* source;
    xercesc::DOMDocument* data;
    std::ifstream iData;

    void createDataWithID( const char * );
    void createDataNoID( const char * );
    void createDataIDShuffled( const char * );
    void createDataWithDuplicateLabel( const char * );
    void createDataWithMdv( const char * );
    void createDataWithEvid( const char * );
    void createDataNoMdvNoEvid( const char * );
    void createDataWithMdvWithEvid( const char * );

public:
    virtual void setUp();
    virtual void tearDown();

    void testDataWithID(); 
    void testDataShuffledID();
    void testIndDataNoID();
    void testPopDataNoID();
    void testDuplicateLabel();
    void testMdv();
    void testEvid();
    void testNoMdvNoEvid();
    void testWithMdvWithEvid();

    static CppUnit::Test * suite();
};

#endif
