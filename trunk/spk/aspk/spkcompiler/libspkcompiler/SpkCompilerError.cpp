#include <map>

#include <iostream>
#include <exception>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <cassert>
#include <fstream>

#include "SpkCompilerError.h"

/*------------------------------------------------------------------------
 * Namespaces used
 *------------------------------------------------------------------------*/
//using namespace SpkCompilerError_const;
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
  const unsigned int SpkCompilerError::ERRORCODE_FIELD_LEN = 4;
  const unsigned int SpkCompilerError::LINENUM_FIELD_LEN   = 6;
  const unsigned int SpkCompilerError::FILENAME_FIELD_LEN  = 128;
  const unsigned int SpkCompilerError::MESSAGE_FIELD_LEN   = 256;

  const unsigned int SpkCompilerError::ERROR_SIZE        
      = SpkCompilerError::FILENAME_FIELD_LEN + 1
      + SpkCompilerError::MESSAGE_FIELD_LEN + 1
      + SpkCompilerError::ERRORCODE_FIELD_LEN + 1
      + SpkCompilerError::LINENUM_FIELD_LEN + 1
      //+ strlen("errorcode\n") + strlen("linenum\n") + strlen("filename\n") + strlen("message\n");
      + 10 + 8 + 9 + 8;

  const char SpkCompilerError::ERRORCODE_FIELD_NAME[] = "errorcode";
  const char SpkCompilerError::ERRORCODE_DESCRIPTION_FIELD_NAME[] = "description";
  const char SpkCompilerError::FILENAME_FIELD_NAME[]  = "filename";
  const char SpkCompilerError::LINENUM_FIELD_NAME[]   = "linenum";
  const char SpkCompilerError::MESSAGE_FIELD_NAME[]   = "message";


/*------------------------------------------------------------------------
 * Static member functions
 *------------------------------------------------------------------------*/
const SpkCompilerError::ErrorMap SpkCompilerError::mapping = SpkCompilerError::fillErrorMap();
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
    try{
        std::fill(myFileName, myFileName+FILENAME_FIELD_LEN+1, '\0');
        std::fill(myMessage, myMessage+MESSAGE_FIELD_LEN+1, '\0');
    }
    catch(...)
    {
        cerr << "SpkCompilerError::SpkCompilerError() shall not throw... terminating..." << endl;
        abort();
    }
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
: myErrorCode(SpkCompilerError::SPK_COMPILER_STD_ERR), myLineNum(line)
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
std::string& operator<<(std::string& s, const SpkCompilerError& e) 
{
    ostringstream stream;
    stream << e;
    s = stream.str();
    return s;
}
std::string& operator>>(std::string& s, SpkCompilerError& e)
{
    std::istringstream stream(s);
    stream >> e;
    s = stream.str();
    return s;
}

std::ostream& operator<<(std::ostream& stream, const SpkCompilerError& e)
{
    stream << SpkCompilerError::ERRORCODE_FIELD_NAME << endl;
    stream << e.myErrorCode << endl;

    stream << SpkCompilerError::ERRORCODE_DESCRIPTION_FIELD_NAME << endl;
    stream << SpkCompilerError::describe( e.myErrorCode ) << endl << '\r';

    stream << SpkCompilerError::LINENUM_FIELD_NAME   << endl;
    stream << e.myLineNum   << endl;
    
    stream << SpkCompilerError::FILENAME_FIELD_NAME  << endl;
    stream << e.myFileName  << endl;
    
    stream << SpkCompilerError::MESSAGE_FIELD_NAME   << endl;
    stream << e.myMessage;
    stream.put('\0');
   
    return stream;
}
std::istream& operator>>(std::istream& stream, SpkCompilerError& e)
{
    char buf[256];

    stream >> buf;
    assert(strcmp(buf, SpkCompilerError::ERRORCODE_FIELD_NAME)==0);
    stream >> buf;
    e.myErrorCode = static_cast<SpkCompilerError::ErrorCode>(atoi(buf));

    stream >> buf;
    assert(strcmp(buf, SpkCompilerError::ERRORCODE_DESCRIPTION_FIELD_NAME)==0);
    stream.getline( buf, 256 ); // eat the trailing '\n'
    stream.getline( buf, 256, '\r');
    // don't do anything

    stream >> buf;
    assert(strcmp(buf, SpkCompilerError::LINENUM_FIELD_NAME)==0);
    stream >> buf;
    e.myLineNum = atoi(buf);

    stream >> buf;
    assert(strcmp(buf, SpkCompilerError::FILENAME_FIELD_NAME)==0);
    stream >> e.myFileName;

    stream >> buf;
    assert(strcmp(buf, SpkCompilerError::MESSAGE_FIELD_NAME)==0);
    //
    // istream::getline(...) does not eat leading whitespaces, 
    // while istream::opeartor>>(...) does eat and gets the first relevant text string.
    // Since the serialized SpkCompilerError::myFileName is terminated with a new line charactor, '\n',
    // that new line charactor must be eaten first.  Otherwise,
    // since getline() reads from whereever istream::seek() returns to 
    // a new line charactor, the first attempt gets only empty (or just '\n') in the
    // buffer.  So, the first getline among the two is to eat the '\n' and the 
    // leading whitespaces.  The second getline is really getting a relevent text string.
    //
    // The second getline reads till a special charactor, '\r', appears.
    // When the stream object contains more than one serialized SpkCompilerError objects,
    // each object must be separated by '\r'.  Otherwise it reads till NULL appears.
    //
    stream.getline(buf, 256);
    stream.getline(buf, 256, '\r');

    strcpy(e.myMessage, buf);
    return stream;
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
const SpkCompilerError::ErrorMap SpkCompilerError::fillErrorMap()
{
    //
    // Filling the error code-message map
    //
    ErrorMap tmpMap;
    
    tmpMap.insert( ErrorMap::value_type(SPK_COMPILER_STD_ERR,        "SPK_COMPILER_STD_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_COMPILER_INSUFFICIENT_MEM_ERR,"SPK_COMPILER_INSUFFICIENT_MEM_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_COMPILER_DRIVER_ERR,     "SPK_COMPILER_DRIVER_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_COMPILER_SOURCEML_ERR,   "SPK_COMPILER_SOURCEML_ERR"));
    tmpMap.insert( ErrorMap::value_type(SPK_COMPILER_DATAML_ERR,     "SPK_COMPILER_DATAML_ERR"));
    tmpMap.insert( ErrorMap::value_type(SPK_COMPILER_FORTRAN_ERR,    "SPK_COMPILER_FORTRAN_ERR"));

    tmpMap.insert( ErrorMap::value_type(SPK_COMPILER_UNKNOWN_ERR,    "SPK_COMPILER_UNKNOWN_ERR") );

    tmpMap.insert( ErrorMap::value_type(SPK_COMPILER_USER_INPUT_ERR, "SPK_COMPILER_USER_INPUT_ERR"));

    tmpMap.insert( ErrorMap::value_type(SPK_COMPILER_XMLDOM_ERR,     "SPK_COMPILER_XMLDOM_ERR"));
    return tmpMap;
}
