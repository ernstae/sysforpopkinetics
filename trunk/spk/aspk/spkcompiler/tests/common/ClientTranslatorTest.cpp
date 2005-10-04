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

#include "../../spkcompiler/ClientTranslator.h"
#include "../../spkcompiler/SymbolTable.h"
#include "../../spkcompiler/SpkCompilerException.h"
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

  virtual void parseData()
  {
  }
  virtual int detAnalysisType()
  {
    setPopSize( myPopSize );
    if( myPopSize > 1 )
      setTarget( POP );
    else
      setTarget( IND );

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

void ClientTranslatorTest::createDataIDShuffled( const char *dataFile )
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
       oData << "<value type=\"numeric\">0.5</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"3\">" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "<value type=\"numeric\">0.6</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"4\">" << endl;
       oData << "<value type=\"numeric\">0.3</value>\n" << endl;
       oData << "<value type=\"numeric\">0.7</value>\n" << endl;
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

void ClientTranslatorTest::createDataWithDuplicateLabel( const char *dataFile )
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
       oData << "<value type=\"string\">TIME</value>\n" << endl;
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
       oData << "</table>" << endl;
       oData << "</spkdata>" << endl;
       oData.close();
     }
   else
     {
       CPPUNIT_ASSERT_MESSAGE( "Faild to open a new file for writing a data set!", false );
     }
}
void ClientTranslatorTest::createDataWithMdv( const char *dataFile )
{
   ofstream oData( dataFile );
   if( oData.good() )
     {
       oData << "<spkdata version=\"0.1\">\n" << endl;
       oData << "<table columns=\"3\" rows=\"4\">\n" << endl;
       oData << "<description>ClientTranslator Test Data Set</description>\n" << endl;
       oData << "<row position=\"1\">\n" << endl;
       oData << "<value type=\"string\">TIME</value>\n" << endl;
       oData << "<value type=\"string\">DV</value>\n" << endl;
       oData << "<value type=\"string\">MDV</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"2\">" << endl;
       oData << "<value type=\"numeric\">0.0</value>\n" << endl;
       oData << "<value type=\"numeric\">0.5</value>\n" << endl;
       oData << "<value type=\"numeric\">0</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"3\">" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "<value type=\"numeric\">0.6</value>\n" << endl;
       oData << "<value type=\"numeric\">1</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"4\">" << endl;
       oData << "<value type=\"numeric\">0.3</value>\n" << endl;
       oData << "<value type=\"numeric\">0.7</value>\n" << endl;
       oData << "<value type=\"numeric\">1</value>\n" << endl;
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
void ClientTranslatorTest::createDataWithEvid( const char *dataFile )
{
   ofstream oData( dataFile );
   if( oData.good() )
     {
       oData << "<spkdata version=\"0.1\">\n" << endl;
       oData << "<table columns=\"3\" rows=\"4\">\n" << endl;
       oData << "<description>ClientTranslator Test Data Set</description>\n" << endl;
       oData << "<row position=\"1\">\n" << endl;
       oData << "<value type=\"string\">TIME</value>\n" << endl;
       oData << "<value type=\"string\">DV</value>\n" << endl;
       oData << "<value type=\"string\">Evid</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"2\">" << endl;
       oData << "<value type=\"numeric\">0.0</value>\n" << endl;
       oData << "<value type=\"numeric\">0.5</value>\n" << endl;
       oData << "<value type=\"numeric\">0</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"3\">" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "<value type=\"numeric\">0.6</value>\n" << endl;
       oData << "<value type=\"numeric\">1</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"4\">" << endl;
       oData << "<value type=\"numeric\">0.3</value>\n" << endl;
       oData << "<value type=\"numeric\">0.7</value>\n" << endl;
       oData << "<value type=\"numeric\">2</value>\n" << endl;
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
void ClientTranslatorTest::createDataNoMdvNoEvid( const char *dataFile )
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
       oData << "<value type=\"numeric\">0.5</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"3\">" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "<value type=\"numeric\">0.6</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"4\">" << endl;
       oData << "<value type=\"numeric\">0.3</value>\n" << endl;
       oData << "<value type=\"numeric\">0.7</value>\n" << endl;
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
void ClientTranslatorTest::createDataWithMdvWithEvid( const char *dataFile )
{
   ofstream oData( dataFile );
   if( oData.good() )
     {
       oData << "<spkdata version=\"0.1\">\n" << endl;
       oData << "<table columns=\"4\" rows=\"4\">\n" << endl;
       oData << "<description>ClientTranslator Test Data Set</description>\n" << endl;
       oData << "<row position=\"1\">\n" << endl;
       oData << "<value type=\"string\">TIME</value>\n" << endl;
       oData << "<value type=\"string\">DV</value>\n" << endl;
       oData << "<value type=\"string\">MDV</value>\n" << endl;
       oData << "<value type=\"string\">EVID</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"2\">" << endl;
       oData << "<value type=\"numeric\">0.0</value>\n" << endl;
       oData << "<value type=\"numeric\">0.5</value>\n" << endl;
       oData << "<value type=\"numeric\">0</value>\n" << endl;
       oData << "<value type=\"numeric\">0</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"3\">" << endl;
       oData << "<value type=\"numeric\">0.1</value>\n" << endl;
       oData << "<value type=\"numeric\">0.6</value>\n" << endl;
       oData << "<value type=\"numeric\">1</value>\n" << endl;
       oData << "<value type=\"numeric\">1</value>\n" << endl;
       oData << "</row>" << endl;
       oData << "<row position=\"4\">" << endl;
       oData << "<value type=\"numeric\">0.3</value>\n" << endl;
       oData << "<value type=\"numeric\">0.7</value>\n" << endl;
       oData << "<value type=\"numeric\">1</value>\n" << endl;
       oData << "<value type=\"numeric\">2</value>\n" << endl;
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
   xlator.detAnalysisType();
   xlator.parseData();
   const SymbolTable * table = xlator.getSymbolTable();
   //   cout << *table << endl;

   const Symbol *pId, *pTime, *pDv, *pMdv;
   CPPUNIT_ASSERT( (pId   = table->findi( "id" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pTime = table->findi( "time" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pDv   = table->findi( "dv" ) )   != Symbol::empty() );

   CPPUNIT_ASSERT( pId->initial[0][0]   == "Sachiko" );
   CPPUNIT_ASSERT( pId->initial[0][1]   == "Sachiko" );
   CPPUNIT_ASSERT( pId->initial[0][2]   == "Sachiko" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pTime->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, atof( pTime->initial[0][1].c_str() ), 0.1 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.2, atof( pTime->initial[0][2].c_str() ), 0.2 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pDv  ->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, atof( pDv  ->initial[0][1].c_str() ), 0.1 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.2, atof( pDv  ->initial[0][2].c_str() ), 0.2 );

   CPPUNIT_ASSERT( pId->initial[1][0]   == "Minoru" );
   CPPUNIT_ASSERT( pId->initial[1][1]   == "Minoru" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 10.0, atof( pTime->initial[1][0].c_str() ), 10.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 10.1, atof( pTime->initial[1][1].c_str() ), 10.1 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 10.0, atof( pDv  ->initial[1][0].c_str() ), 10.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 10.1, atof( pDv  ->initial[1][1].c_str() ), 10.1 );

   CPPUNIT_ASSERT( pId->initial[2][0]   == "Noriko" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 20.0, atof( pTime->initial[2][0].c_str() ), 20.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 20.0, atof( pDv  ->initial[2][0].c_str() ), 20.0 );

  remove( gData );
  delete [] gData;
  XMLPlatformUtils::Terminate();
}
void ClientTranslatorTest::testDataShuffledID()
{
   gData   = new char[256];
   strcpy( gData, "withID.dat" );
   createDataIDShuffled( gData );

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
   xlator.detAnalysisType();
   xlator.parseData();
   const SymbolTable * table = xlator.getSymbolTable();
   //   cout << *table << endl;

   const Symbol *pId, *pTime, *pDv, *pMdv;
   CPPUNIT_ASSERT( (pId   = table->findi( "id" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pTime = table->findi( "time" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pDv   = table->findi( "dv" ) )   != Symbol::empty() );

   CPPUNIT_ASSERT( pId->initial[0][0]   == "Sachiko" );
   CPPUNIT_ASSERT( pId->initial[0][1]   == "Sachiko" );
   CPPUNIT_ASSERT( pId->initial[0][2]   == "Sachiko" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pTime->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, atof( pTime->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.2, atof( pTime->initial[0][2].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pDv  ->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, atof( pDv  ->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.2, atof( pDv  ->initial[0][2].c_str() ), 0.0 );
   
   CPPUNIT_ASSERT( pId->initial[1][0]   == "Minoru" );
   CPPUNIT_ASSERT( pId->initial[1][1]   == "Minoru" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 10.0, atof( pTime->initial[1][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 10.1, atof( pTime->initial[1][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 10.0, atof( pDv  ->initial[1][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 10.1, atof( pDv  ->initial[1][1].c_str() ), 0.0 );
		   
   CPPUNIT_ASSERT( pId->initial[2][0]   == "Noriko" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 20.0, atof( pTime->initial[2][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 20.0, atof( pDv  ->initial[2][0].c_str() ), 0.0 );
   
   remove( gData );
   delete [] gData;
   XMLPlatformUtils::Terminate();
}
void ClientTranslatorTest::testIndDataNoID()
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
   xlator.detAnalysisType();
   xlator.parseData();
   const SymbolTable * table = xlator.getSymbolTable();
   //   cout << *table << endl;

   const Symbol *pId, *pTime, *pDv, *pMdv;
   CPPUNIT_ASSERT( (pId   = table->findi( "id" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pTime = table->findi( "time" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pDv   = table->findi( "dv" ) )   != Symbol::empty() );

   CPPUNIT_ASSERT( pId->initial[0][0]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][1]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][2]   == "1" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pTime->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, atof( pTime->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.3, atof( pTime->initial[0][2].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.5, atof( pDv  ->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.6, atof( pDv  ->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.7, atof( pDv  ->initial[0][2].c_str() ), 0.0 );

  remove( gData );
  delete [] gData;
  XMLPlatformUtils::Terminate();
}
void ClientTranslatorTest::testPopDataNoID()
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


   ClientTranslatorSubclass xlator( source, data, 3 );
   xlator.detAnalysisType();
   xlator.parseData();
   const SymbolTable * table = xlator.getSymbolTable();
   //   cout << *table << endl;

   const Symbol *pId, *pTime, *pDv, *pMdv;
   CPPUNIT_ASSERT( (pId   = table->findi( "id" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pTime = table->findi( "time" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pDv   = table->findi( "dv" ) )   != Symbol::empty() );

   CPPUNIT_ASSERT( pId->initial[0][0]   == "1" );
   CPPUNIT_ASSERT( pId->initial[1][0]   == "2" );
   CPPUNIT_ASSERT( pId->initial[2][0]   == "3" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pTime->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, atof( pTime->initial[1][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.3, atof( pTime->initial[2][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.5, atof( pDv  ->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.6, atof( pDv  ->initial[1][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.7, atof( pDv  ->initial[2][0].c_str() ), 0.0 );

  remove( gData );
  delete [] gData;
  XMLPlatformUtils::Terminate();
}
void ClientTranslatorTest::testDuplicateLabel()
{
   gData   = new char[256];
   strcpy( gData, "duplicateLabel.dat" );
   createDataWithDuplicateLabel( gData );

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
   xlator.detAnalysisType();
   try{
     xlator.parseData();
   }
   catch( const SpkCompilerException& e )
     {
       string m = e[0].message();
       if( m.find( "TIME is already defined" ) == string::npos )
	 CPPUNIT_ASSERT( true );
       else
	 CPPUNIT_ASSERT_MESSAGE( "parseData() should have complained about duplicated labels", false );
     }

  remove( gData );
  delete [] gData;
  XMLPlatformUtils::Terminate();
}
void ClientTranslatorTest::testMdv()
{
   gData   = new char[256];
   strcpy( gData, "withMdv.dat" );
   createDataWithMdv( gData );

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
   xlator.detAnalysisType();
   xlator.parseData();
   const SymbolTable * table = xlator.getSymbolTable();
   //   cout << *table << endl;

   const Symbol *pId, *pTime, *pDv, *pMdv;
   CPPUNIT_ASSERT( (pId   = table->findi( "id" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pTime = table->findi( "time" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pDv   = table->findi( "dv" ) )   != Symbol::empty() );

   CPPUNIT_ASSERT( pId->initial[0][0]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][1]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][2]   == "1" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pTime->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, atof( pTime->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.3, atof( pTime->initial[0][2].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.5, atof( pDv  ->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.6, atof( pDv  ->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.7, atof( pDv  ->initial[0][2].c_str() ), 0.0 );

  remove( gData );
  delete [] gData;
  XMLPlatformUtils::Terminate();
}
void ClientTranslatorTest::testEvid()
{
   gData   = new char[256];
   strcpy( gData, "withEvidNoMdv.dat" );
   createDataWithEvid( gData );

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
   xlator.detAnalysisType();
   xlator.parseData();
   const SymbolTable * table = xlator.getSymbolTable();
   //   cout << *table << endl;

   const Symbol *pId, *pTime, *pDv, *pMdv, *pEvid;
   CPPUNIT_ASSERT( (pId   = table->findi( "id" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pTime = table->findi( "time" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pDv   = table->findi( "dv" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pEvid = table->findi( "evid" ) ) != Symbol::empty() );

   CPPUNIT_ASSERT( pId->initial[0][0]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][1]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][2]   == "1" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pTime->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, atof( pTime->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.3, atof( pTime->initial[0][2].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.5, atof( pDv  ->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.6, atof( pDv  ->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.7, atof( pDv  ->initial[0][2].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pEvid->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, atof( pEvid->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 2.0, atof( pEvid->initial[0][2].c_str() ), 0.0 );

  remove( gData );
  delete [] gData;
  XMLPlatformUtils::Terminate();
}
void ClientTranslatorTest::testWithMdvWithEvid()
{
   gData   = new char[256];
   strcpy( gData, "withMdvWithEvid.dat" );
   createDataWithMdvWithEvid( gData );

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
   xlator.detAnalysisType();
   xlator.parseData();
   const SymbolTable * table = xlator.getSymbolTable();
   //   cout << *table << endl;

   const Symbol *pId, *pTime, *pDv, *pMdv, *pEvid;
   CPPUNIT_ASSERT( (pId   = table->findi( "id" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pTime = table->findi( "time" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pDv   = table->findi( "dv" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pEvid = table->findi( "evid" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pMdv  = table->findi( "mdv" ) )  != Symbol::empty() );

   CPPUNIT_ASSERT( pId->initial[0][0]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][1]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][2]   == "1" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pTime->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, atof( pTime->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.3, atof( pTime->initial[0][2].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.5, atof( pDv  ->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.6, atof( pDv  ->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.7, atof( pDv  ->initial[0][2].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pEvid->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, atof( pEvid->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 2.0, atof( pEvid->initial[0][2].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pMdv ->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, atof( pMdv ->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 1.0, atof( pMdv ->initial[0][2].c_str() ), 0.0 );

  remove( gData );
  delete [] gData;
  XMLPlatformUtils::Terminate();
}
void ClientTranslatorTest::testNoMdvNoEvid()
{
   gData   = new char[256];
   strcpy( gData, "noMdvNoEvid.dat" );
   createDataNoMdvNoEvid( gData );

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
   xlator.detAnalysisType();
   xlator.parseData();
   const SymbolTable * table = xlator.getSymbolTable();
   //   cout << *table << endl;

   const Symbol *pId, *pTime, *pDv, *pMdv, *pEvid;
   CPPUNIT_ASSERT( (pId   = table->findi( "id" ) )   != Symbol::empty() );
   CPPUNIT_ASSERT( (pTime = table->findi( "time" ) ) != Symbol::empty() );
   CPPUNIT_ASSERT( (pDv   = table->findi( "dv" ) )   != Symbol::empty() );

   CPPUNIT_ASSERT( pId->initial[0][0]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][1]   == "1" );
   CPPUNIT_ASSERT( pId->initial[0][2]   == "1" );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.0, atof( pTime->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, atof( pTime->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.3, atof( pTime->initial[0][2].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.5, atof( pDv  ->initial[0][0].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.6, atof( pDv  ->initial[0][1].c_str() ), 0.0 );
   CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.7, atof( pDv  ->initial[0][2].c_str() ), 0.0 );

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
			 ("testDataShuffledID", &ClientTranslatorTest::testDataShuffledID ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ClientTranslatorTest>
			 ("testIndDataNoID", &ClientTranslatorTest::testIndDataNoID ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ClientTranslatorTest>
			 ("testPopDataNoID", &ClientTranslatorTest::testPopDataNoID ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ClientTranslatorTest>
			 ("testDuplicateLabel", &ClientTranslatorTest::testDuplicateLabel ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ClientTranslatorTest>
			 ("testMdv", &ClientTranslatorTest::testMdv ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ClientTranslatorTest>
			 ("testEvid", &ClientTranslatorTest::testEvid ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ClientTranslatorTest>
			 ("testNoMdvNoEvid", &ClientTranslatorTest::testNoMdvNoEvid ) );
  suiteOfTests->addTest( new CppUnit::TestCaller<ClientTranslatorTest>
			 ("testWithMdvWithEvid", &ClientTranslatorTest::testWithMdvWithEvid ) );
 return suiteOfTests;
}

