#include <iostream>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/util/XMLString.hpp>

#include <spkcompiler/ClientTranslator.h>
#include <spkcompiler/SymbolTable.h>
#include "ClientTranslatorTest.h"

using namespace std;
using namespace CppUnit;
using namespace xercesc;

static const unsigned int maxChars = 2047;
/**
 * This test suite tests only the NON virtual member functions.
 * The virtual members should be tested in the test suites of real subclasses
 * of ClientTranslator.
 */
class ClientTranslatorSubclass : public ClientTranslator
{
  int myPopSize;
public:
  ClientTranslatorSubclass( DOMDocument * sourceIn, DOMDocument* dataIn, int popSize )
    : ClientTranslator( sourceIn, dataIn ), myPopSize( popSize )
  {
  }
  virtual int detAnalysisType()
  {
    if( myPopSize > 0 )
      ourTarget = POP;
    else
      ourTarget = IND;
    return myPopSize;
  }
  virtual void parseSource(){}
};
void ClientTranslatorTest::createDataWithID( const char *dataFile )
{
   ofstream oData( dataFile );
   if( oData.good() )
     {
       oData << "<spkdata version=\"0.1\">\n" << endl;
       oData << "<table columns=\"3\" rows=\"7\">\n" << endl;
       oData << "<description>ClientTranslator Test Data Set</description>\n" << endl;
       oData << "<row position=\"1\">\n" << endl;
       oData << "<value type=\"string\">ID</value>\n" << endl;
       oData << "<value type=\"string\">TIME</value>\n" << endl;
       oData << "<value type=\"string\">DV</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"2\">" << endl;
       oData << "<value type=\"string\">Sachiko</value>\n" << endl;
       oData << "<value type=\"numeric\">0.0</value>\n" << endl;
       oData << "<value type=\"numeric\">0.0</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"3\">" << endl;
       oData << "<value type=\"string\">Sachiko</value>\n" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"4\">" << endl;
       oData << "<value type=\"string\">Sachiko</value>\n" << endl;
       oData << "<value type=\"numeric\">0.2</value>\n" << endl;
       oData << "<value type=\"numeric\">0.2</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"5\">" << endl;
       oData << "<value type=\"string\">Minoru</value>\n" << endl;
       oData << "<value type=\"numeric\">10.0</value>\n" << endl;
       oData << "<value type=\"numeric\">10.0</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"6\">" << endl;
       oData << "<value type=\"string\">Minoru</value>\n" << endl;
       oData << "<value type=\"numeric\">10.1</value>\n" << endl;
       oData << "<value type=\"numeric\">10.1</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"7\">" << endl;
       oData << "<value type=\"string\">Noriko</value>\n" << endl;
       oData << "<value type=\"numeric\">20.0</value>\n" << endl;
       oData << "<value type=\"numeric\">20.0</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "</table>" << endl;
       oData << "</spkdata>" << endl;
       oData.close();
     }
   else
     {
       CPPUNIT_ASSERT_MESSAGE( "Faild to open a new file for writing a data set!", false );
     }
}
void ClientTranslatorTest::createDataWithIDShaffuled( const char *dataFile )
{
   ofstream oData( dataFile );
   if( oData.good() )
     {
       oData << "<spkdata version=\"0.1\">\n" << endl;
       oData << "<table columns=\"3\" rows=\"7\">\n" << endl;
       oData << "<description>ClientTranslator Test Data Set</description>\n" << endl;
       oData << "<row position=\"1\">\n" << endl;
       oData << "<value type=\"string\">TIME</value>\n" << endl;
       oData << "<value type=\"string\">DV</value>\n" << endl;
       oData << "<value type=\"string\">ID</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"2\">" << endl;
       oData << "<value type=\"numeric\">0.0</value>\n" << endl;
       oData << "<value type=\"numeric\">0.0</value>\n" << endl;
       oData << "<value type=\"string\">Sachiko</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"3\">" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "<value type=\"string\">Sachiko</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"4\">" << endl;
       oData << "<value type=\"numeric\">0.2</value>\n" << endl;
       oData << "<value type=\"numeric\">0.2</value>\n" << endl;
       oData << "<value type=\"string\">Sachiko</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"5\">" << endl;
       oData << "<value type=\"numeric\">10.0</value>\n" << endl;
       oData << "<value type=\"numeric\">10.0</value>\n" << endl;
       oData << "<value type=\"string\">Minoru</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"6\">" << endl;
       oData << "<value type=\"numeric\">10.1</value>\n" << endl;
       oData << "<value type=\"numeric\">10.1</value>\n" << endl;
       oData << "<value type=\"string\">Minoru</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"7\">" << endl;
       oData << "<value type=\"numeric\">20.0</value>\n" << endl;
       oData << "<value type=\"numeric\">20.0</value>\n" << endl;
       oData << "<value type=\"string\">Noriko</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "</table>" << endl;
       oData << "</spkdata>" << endl;
       oData.close();
     }
   else
     {
       CPPUNIT_ASSERT_MESSAGE( "Faild to open a new file for writing a data set!", false );
     }
}
void ClientTranslatorTest::createDataNoID( const char* dataFile )
{
   ofstream oData( dataFile );
   if( oData.good() )
     {
       oData << "<spkdata version=\"0.1\">\n" << endl;
       oData << "<table columns=\"2\" rows=\"4\">\n" << endl;
       oData << "<description>ClientTranslator Test Data Set</description>\n" << endl;
       oData << "<row position=\"1\">\n" << endl;
       oData << "<value type=\"string\">TIME</value>\n" << endl;
       oData << "<value type=\"string\">DV</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"2\">" << endl;
       oData << "<value type=\"numeric\">0.0</value>\n" << endl;
       oData << "<value type=\"numeric\">0.0</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"3\">" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"4\">" << endl;
       oData << "<value type=\"numeric\">0.2</value>\n" << endl;
       oData << "<value type=\"numeric\">0.2</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "</table>" << endl;
       oData << "</spkdata>" << endl;
       oData.close();
     }
   else
     {
       CPPUNIT_ASSERT_MESSAGE( "Faild to open a new file for writing a data set!", false );
     }
}

void ClientTranslatorTest::setUp()
{
}

void ClientTranslatorTest::tearDown()
{
}

void ClientTranslatorTest::testDataWithID()
{
   gData   = new char[256];
   strcpy( gData, "withID.dat" );
   createDataWithID( gData );

   try
   {
      XMLPlatformUtils::Initialize();
   }
   catch( const XMLException& toCatch )
   {
      char buf[maxChars + 1];
      sprintf( buf, "Error during Xerces-c initialization.\nException message: %s.\n", 
               XMLString::transcode( toCatch.getMessage() ) );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
   }

   xercesc::XercesDOMParser *parser = new xercesc::XercesDOMParser;
   parser->setValidationScheme( XercesDOMParser::Val_Auto );
   parser->setDoNamespaces( true );
   parser->setDoSchema( true );
   parser->setValidationSchemaFullChecking( true );
   parser->setCreateEntityReferenceNodes( true );

    try{
       ifstream iData( gData );
       if( !iData.good() )
       {
          XMLPlatformUtils::Terminate();
          char buf[maxChars + 1];
          sprintf( buf, "Failed to open %s!\n", gData );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
       }
       parser->parse( gData );
       data = parser->getDocument();
    }
    catch( const XMLException& e )
    {
       XMLPlatformUtils::Terminate();
       char buf[maxChars + 1];
       sprintf( buf, "An error occurred during parsing\n   Message: %s\n",
                XMLString::transcode(e.getMessage() ) );
       CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
    catch( const DOMException& e )
    {
       XMLCh errText[maxChars + 1]; 
       if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
       {
          XMLPlatformUtils::Terminate();
          char buf[maxChars + 1];
          sprintf( buf, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   gData, e.code, XMLString::transcode(errText) );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
       }
    }
    catch( ... )
    {
       XMLPlatformUtils::Terminate();
       char buf[maxChars + 1];
       sprintf( buf, "An unknown error occurred during parsing.\n" );
       CPPUNIT_ASSERT_MESSAGE( buf, false );
    }


   ClientTranslatorSubclass xlator( source, data, 3 );
   xlator.parseData();
   const SymbolTable * table = xlator.getSymbolTable();
   //   cout << *table << endl;

   const Symbol *pId, *pTime, *pDv;
   CPPUNIT_ASSERT( (pId   = table->findi( "id" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pTime = table->findi( "time" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pDv   = table->findi( "dv" ) )   != Symbol::empty() );

   CPPUNIT_ASSERT( pId->initial[0][0]   == "Sachiko" );
   CPPUNIT_ASSERT( pId->initial[0][1]   == "Sachiko" );
   CPPUNIT_ASSERT( pId->initial[0][2]   == "Sachiko" );
   CPPUNIT_ASSERT( pTime->initial[0][0] == "0.0" );
   CPPUNIT_ASSERT( pTime->initial[0][1] == "0.1" );
   CPPUNIT_ASSERT( pTime->initial[0][2] == "0.2" );
   CPPUNIT_ASSERT( pDv->initial[0][0]   == "0.0" );
   CPPUNIT_ASSERT( pDv->initial[0][1]   == "0.1" );
   CPPUNIT_ASSERT( pDv->initial[0][2]   == "0.2" );

   CPPUNIT_ASSERT( pId->initial[1][0]   == "Minoru" );
   CPPUNIT_ASSERT( pId->initial[1][1]   == "Minoru" );
   CPPUNIT_ASSERT( pTime->initial[1][0] == "10.0" );
   CPPUNIT_ASSERT( pTime->initial[1][1] == "10.1" );
   CPPUNIT_ASSERT( pDv->initial[1][0]   == "10.0" );
   CPPUNIT_ASSERT( pDv->initial[1][1]   == "10.1" );

   CPPUNIT_ASSERT( pId->initial[2][0]   == "Noriko" );
   CPPUNIT_ASSERT( pTime->initial[2][0] == "20.0" );
   CPPUNIT_ASSERT( pDv->initial[2][0]   == "20.0" );
  remove( gData );
  delete [] gData;
  XMLPlatformUtils::Terminate();
}
void ClientTranslatorTest::testDataNoID()
{
   gData   = new char[256];
   strcpy( gData, "noID.dat" );
   createDataNoID( gData );

   try
   {
      XMLPlatformUtils::Initialize();
   }
   catch( const XMLException& toCatch )
   {
      char buf[maxChars + 1];
      sprintf( buf, "Error during Xerces-c initialization.\nException message: %s.\n", 
               XMLString::transcode( toCatch.getMessage() ) );
      CPPUNIT_ASSERT_MESSAGE( buf, false );
   }

   xercesc::XercesDOMParser *parser = new xercesc::XercesDOMParser;
   parser->setValidationScheme( XercesDOMParser::Val_Auto );
   parser->setDoNamespaces( true );
   parser->setDoSchema( true );
   parser->setValidationSchemaFullChecking( true );
   parser->setCreateEntityReferenceNodes( true );

    try{
       ifstream iData( gData );
       if( !iData.good() )
       {
          XMLPlatformUtils::Terminate();
          char buf[maxChars + 1];
          sprintf( buf, "Failed to open %s!\n", gData );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
       }
       parser->parse( gData );
       data = parser->getDocument();
    }
    catch( const XMLException& e )
    {
       XMLPlatformUtils::Terminate();
       char buf[maxChars + 1];
       sprintf( buf, "An error occurred during parsing\n   Message: %s\n",
                XMLString::transcode(e.getMessage() ) );
       CPPUNIT_ASSERT_MESSAGE( buf, false );
    }
    catch( const DOMException& e )
    {
       XMLCh errText[maxChars + 1]; 
       if (DOMImplementation::loadDOMExceptionMsg(e.code, errText, maxChars))
       {
          XMLPlatformUtils::Terminate();
          char buf[maxChars + 1];
          sprintf( buf, "DOM Error during parsing \"%s\".\nDOMException code is: %d.\nMessage is: %s.\n",
                   gData, e.code, XMLString::transcode(errText) );
          CPPUNIT_ASSERT_MESSAGE( buf, false );
       }
    }
    catch( ... )
    {
       XMLPlatformUtils::Terminate();
       char buf[maxChars + 1];
       sprintf( buf, "An unknown error occurred during parsing.\n" );
       CPPUNIT_ASSERT_MESSAGE( buf, false );
    }


   ClientTranslatorSubclass xlator( source, data, 1 );
   xlator.parseData();
   const SymbolTable * table = xlator.getSymbolTable();
   //   cout << *table << endl;

   const Symbol *pId, *pTime, *pDv;
   CPPUNIT_ASSERT( (pId   = table->findi( "id" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pTime = table->findi( "time" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pDv   = table->findi( "dv" ) )   != Symbol::empty() );

   CPPUNIT_ASSERT( pId->initial[0][0]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][1]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][2]   == "1" );
   CPPUNIT_ASSERT( pTime->initial[0][0] == "0.0" );
   CPPUNIT_ASSERT( pTime->initial[0][1] == "0.1" );
   CPPUNIT_ASSERT( pTime->initial[0][2] == "0.2" );
   CPPUNIT_ASSERT( pDv->initial[0][0]   == "0.0" );
   CPPUNIT_ASSERT( pDv->initial[0][1]   == "0.1" );
   CPPUNIT_ASSERT( pDv->initial[0][2]   == "0.2" );

  remove( gData );
  delete [] gData;
  XMLPlatformUtils::Terminate();
}
CppUnit::Test * ClientTranslatorTest::suite()
{
  CppUnit::TestSuite *suiteOfTests 
    = new CppUnit::TestSuite( "ClientTranslatorTest" );


  suiteOfTests->addTest( new CppUnit::TestCaller<ClientTranslatorTest>
			 ("testDataWithID", &ClientTranslatorTest::testDataWithID ) );

  suiteOfTests->addTest( new CppUnit::TestCaller<ClientTranslatorTest>
			 ("testDataNoID", &ClientTranslatorTest::testDataNoID ) );

 return suiteOfTests;
}

