/**
 * @file SpkCompilerError.cpp
 * Define SpkCompilerError class.
 */
#include <map>

#include <iostream>
#include <exception>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <cassert>
#include <fstream>
#include <cmath>

#include "SpkCompilerError.h"

/*------------------------------------------------------------------------
 * Namespaces used
 *------------------------------------------------------------------------*/
using namespace std;
/*------------------------------------------------------------------------
 * Static functions
 *------------------------------------------------------------------------*/
//
// Flush the buffer and copy the chars from begin to end to the buffer.
//
static const char *const substr(const char * begin, 
				const char * end, 
				char * buf, 
				int bufsize) throw()
{
    try{
        std::fill(buf, buf+bufsize, '\0');
        std::copy(begin, end, buf);
    }
    catch(...)
    {
        cerr << "substr() shall not throw... terminating..." << endl;
        abort();
    }
    return buf;
}
/*------------------------------------------------------------------------
 * Static member variables
 *------------------------------------------------------------------------*/
  const unsigned int SpkCompilerError::ERRORCODE_FIELD_LEN = 8;
  const unsigned int SpkCompilerError::ERRORCODE_DESCRIPTION_FIELD_LEN = 128;
  const unsigned int SpkCompilerError::LINENUM_FIELD_LEN   = 6;
  const unsigned int SpkCompilerError::FILENAME_FIELD_LEN  = 128;
  const unsigned int SpkCompilerError::MESSAGE_FIELD_LEN   = 256;

  const unsigned int SpkCompilerError::ERROR_SIZE        
      = SpkCompilerError::FILENAME_FIELD_LEN + 1
      + SpkCompilerError::ERRORCODE_DESCRIPTION_FIELD_LEN + 1
      + SpkCompilerError::MESSAGE_FIELD_LEN + 1
      + SpkCompilerError::ERRORCODE_FIELD_LEN + 1
      + SpkCompilerError::LINENUM_FIELD_LEN + 1
      //+ strlen("errorcode\n") + strlen("description\n") + strlen("linenum\n") + strlen("filename\n") + strlen("message\n");
      + 10 + 12 + 8 + 9 + 8;

  const char SpkCompilerError::ERRORCODE_FIELD_NAME[]             = "errorcode";
  const char SpkCompilerError::ERRORCODE_DESCRIPTION_FIELD_NAME[] = "description";
  const char SpkCompilerError::FILENAME_FIELD_NAME[]              = "filename";
  const char SpkCompilerError::LINENUM_FIELD_NAME[]               = "linenum";
  const char SpkCompilerError::MESSAGE_FIELD_NAME[]               = "message";


/*------------------------------------------------------------------------
 * Static member functions
 *------------------------------------------------------------------------*/
const SpkCompilerError::ErrorMap SpkCompilerError::mapping = SpkCompilerError::initErrorMap();
const char* SpkCompilerError::describe( enum ErrorCode key )
{
  ErrorMap::const_iterator ptr = mapping.find(key);
  if( ptr != mapping.end() )
    return (*ptr).second;
  else
    return "No description registered.";
}
unsigned int SpkCompilerError::maxErrorcode() throw()
{
  return static_cast<unsigned int>( pow( 2.0, (double)sizeof( int ) /*bytes*/ * 7 /* 8 bits - a signed bit*/ ) - 1 );
  /*
    try{
        unsigned int max = 0;
        for(unsigned int i=1, multiplier=1; i<=ERRORCODE_FIELD_LEN; i++, multiplier*=10)
        {
            max += multiplier*9;
        }
        return max;
    }
    catch(...)
    {
        cerr << "SpkCompilerError::maxErrorcode() shall not throw... terminating..." << endl;
        abort();
    }
  */
}
unsigned int SpkCompilerError::maxLinenum() throw()
{
    try{
        unsigned int max = 0;
        for(unsigned int i=1, multiplier=1; i<=LINENUM_FIELD_LEN; i++, multiplier*=10)
        {
            max += multiplier*9;
        }
        return max;
    }
    catch(...)
    {
        cerr << "SpkCompilerError::maxLinenum() shall not throw... terminating..." << endl;
        abort();
    }
}

unsigned int SpkCompilerError::maxFilenameLen() throw()
{
    return FILENAME_FIELD_LEN;
}

unsigned int SpkCompilerError::maxMessageLen() throw()
{
    return MESSAGE_FIELD_LEN;
}

/*------------------------------------------------------------------------
 * Constructors & destructor
 *------------------------------------------------------------------------*/
SpkCompilerError::SpkCompilerError() throw()
: myErrorCode(), myLineNum(0)
{
/*
    try{
        std::fill(myFileName, myFileName+FILENAME_FIELD_LEN+1, '\0');
        std::fill(myMessage, myMessage+MESSAGE_FIELD_LEN+1, '\0');
    }
    catch(...)
    {
        cerr << "SpkCompilerError::SpkCompilerError() shall not throw... terminating..." << endl;
        abort();
    }
*/
}
SpkCompilerError::SpkCompilerError(enum ErrorCode ecode, const char* mess, unsigned int line, const char* file) throw() 
: myErrorCode(ecode), myLineNum(line)
{
    try{
      if( strlen(file) > maxFilenameLen() )
      {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "The length of a filename must be less than " << strlen(myFileName) << endl;
        cerr << "System terminates..." << endl;
        abort();
        
      }
        strcpy(myFileName, file);
        strcpy(myMessage, mess);
    }
    catch( ... )
    {
        cerr << "SpkCompilerError::operator= shall not throw... terminating..." << endl;
        abort();
    }
}
SpkCompilerError::SpkCompilerError( const std::exception& e, const char* mess, unsigned int line, const char* file) throw()
: myErrorCode(SpkCompilerError::ASPK_STD_ERR), myLineNum(line)
{
    try{
      if( strlen(mess) > maxMessageLen() )
      {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "The length of a message must be less than " << strlen(myMessage) << endl;
        cerr << "System terminates..." << endl;
        abort();
        
      }
      if( strlen(file) > maxFilenameLen() )
      {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "The length of a filename must be less than " << strlen(myFileName) << endl;
        cerr << "System terminates..." << endl;
        abort();
        
      }
        strcpy(myFileName, file);
        strcpy(myMessage, mess);
        strcat(myMessage, "\n");
        strcat(myMessage, e.what());
        strcat(myMessage, "\n");
    }
    catch( ... )
    {
        cerr << "SpkCompilerError::operator= shall not throw... terminating..." << endl;
        abort();
    }
}


SpkCompilerError::SpkCompilerError( const SpkCompilerError& e ) throw()
: myErrorCode(e.myErrorCode), myLineNum(e.myLineNum)
{
    // This is doing deep copy
    try{
        strcpy(myFileName, e.myFileName);
        strcpy(myMessage, e.myMessage);
    }
    catch( ... )
    {
        cerr << "SpkCompilerError::operator= shall not throw... terminating..." << endl;
        abort();
    }
}
SpkCompilerError::~SpkCompilerError() throw()
{
}
/*------------------------------------------------------------------------
 * Public member functions
 *------------------------------------------------------------------------*/
const SpkCompilerError& SpkCompilerError::operator=(const SpkCompilerError& right) throw()
{
    // This is doing deep copy
    try{
        myErrorCode      = right.myErrorCode;
        myLineNum        = right.myLineNum;
        strcpy(myFileName, right.myFileName);
        strcpy(myMessage,  right.myMessage);
        return *this;
    }
    catch( ... )
    {
        cerr << "SpkCompilerError::operator= shall not throw... terminating..." << endl;
        abort();
    }
}

std::string& operator<<(std::string& str, const SpkCompilerError& e)
{
  std::ostringstream stream;
  stream << e;
  str = stream.str();
  return str;
}
std::ostream& operator<<(std::ostream& stream, const SpkCompilerError& e)
{
    stream.flush();

    stream << SpkCompilerError::ERRORCODE_FIELD_NAME << endl;
    stream << e.myErrorCode << endl;

    stream << SpkCompilerError::ERRORCODE_DESCRIPTION_FIELD_NAME << endl;
    stream << SpkCompilerError::describe( e.myErrorCode ) << endl;

    stream << SpkCompilerError::LINENUM_FIELD_NAME   << endl;
    stream << e.myLineNum   << endl;
    
    stream << SpkCompilerError::FILENAME_FIELD_NAME  << endl;
    stream << e.myFileName  << endl;
    
    stream << SpkCompilerError::MESSAGE_FIELD_NAME   << endl;
    stream << e.myMessage << endl; 
   
    stream.flush();
    return stream;
}
const std::string SpkCompilerError::getXml() const
{
    ostringstream o;
    string m = myMessage;

    //
    // escape XML sensitive characters { <, >, & }
    // '&' must be replaced first because < and > will be replaced by
    // words containing '&'.
    // 
    for( int i = m.find( '&', 0 ); i != string::npos; i = m.find( '&', i+1 ) )
      {
	m.erase( i, 1 );
	m.insert( i, "&amp;" );
      }
    for( int i = m.find( '<', 0 ); i != string::npos; i = m.find( '<', i ) )
      {
	m.erase( i, 1 );
	m.insert( i, "&lt;" );
      }
    for( int i = m.find( '>', 0 ); i != string::npos; i = m.find( '>', i ) )
      {
	m.erase( i, 1 );
	m.insert( i, "&gt;" );
      }

    o << "<error>"          << endl;
    o << "   <code>"        << this->describe(myErrorCode) << "</code>"        << endl;
    o << "   <file_name>"   << myFileName  << "</file_name>"   << endl;
    o << "   <line_number>" << myLineNum   << "</line_number>" << endl;
    o << "   <message>"     << m   << "</message>"     << endl;
    o << "</error>"         << endl;
    return o.str();
}

enum SpkCompilerError::ErrorCode SpkCompilerError::code() const throw()
{
    return myErrorCode;
}
unsigned int SpkCompilerError::linenum() const throw()
{
    return myLineNum;
}

const char* SpkCompilerError::filename() const throw()
{
    return myFileName;
}

const char* SpkCompilerError::message() const throw()
{
    return myMessage;
}
const SpkCompilerError::ErrorMap SpkCompilerError::initErrorMap()
{
    //
    // Filling the error code-message map
    //
    ErrorMap tmpMap;
    
    tmpMap.insert( ErrorMap::value_type(ASPK_STD_ERR,        "ASPK_STD_ERR") );
    tmpMap.insert( ErrorMap::value_type(ASPK_SOURCEML_ERR,   "ASPK_SOURCEML_ERR") );
    tmpMap.insert( ErrorMap::value_type(ASPK_DATAML_ERR,     "ASPK_DATAML_ERR") );
    tmpMap.insert( ErrorMap::value_type(ASPK_FTOC_ERR,       "ASPK_FOTC_ERR") );
    tmpMap.insert( ErrorMap::value_type(ASPK_UNKNOWN_ERR,    "ASPK_UNKNOWN_ERR") );
    tmpMap.insert( ErrorMap::value_type(ASPK_PROGRAMMER_ERR, "ASPK_PROGRAMMER_ERR") );
    tmpMap.insert( ErrorMap::value_type(ASPK_XMLDOM_ERR,     "ASPK_XMLDOM_ERR") );
    tmpMap.insert( ErrorMap::value_type(ASPK_USER_ERR,       "ASPK_USER_ERR" ) );
    return tmpMap;
}
