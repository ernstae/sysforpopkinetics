/**
 * @file SpkCompilerException.cpp
 * Define SpkCompilerException class.
 */

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <string>
#include <sstream>
#include <cassert>

#include "SpkCompilerException.h"

/*------------------------------------------------------------------------
 * Namespaces used
 *------------------------------------------------------------------------*/
using namespace std;
/*------------------------------------------------------------------------
 * Static global
 *------------------------------------------------------------------------*/
//
// Flush the buffer and copy the string from begin to end to the buffer.
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
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkCompilerError::substr() shall not throw." << endl;
      cerr << "System terminates..." << endl;
        abort();
    }
    return buf;
}
/*------------------------------------------------------------------------
 * Static member variables
 *------------------------------------------------------------------------*/
const int SpkCompilerException::MAX_ERRORS        = 16;
const int SpkCompilerException::MAX_ERRORS_DIGITS =  2;
const int SpkCompilerException::EXCEPTION_SIZE    = SpkCompilerException::MAX_ERRORS_DIGITS 
                                                  + /*strlen("count\n")*/ 6 
                                                  + ( SpkCompilerException::MAX_ERRORS * SpkCompilerError::ERROR_SIZE );

/*------------------------------------------------------------------------
 * Class definition
 *------------------------------------------------------------------------*/

unsigned int SpkCompilerException::maxErrors() throw()
{
    return MAX_ERRORS;
}
SpkCompilerException::SpkCompilerException() throw()
: myCnt(0)
{
    //constructFormat();
}
SpkCompilerException::SpkCompilerException( enum SpkCompilerError::ErrorCode code, const char* message, unsigned int line, const char* filename) throw()
: myCnt(0)
{
    if( full() )
    {
      cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
      cerr << "The error list is full." << endl;
      cerr << "System terminates..." << endl;
      abort();
    }
    SpkCompilerError e(code, message, line, filename);
    myError_list[myCnt] = e;
    ++myCnt;
}

SpkCompilerException::SpkCompilerException( const std::exception& stde, const char* message, unsigned int line, const char* filename) throw()
: myCnt(0)
{
    if( full() )
    {
      cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
      cerr << "The error list is full." << endl;
      cerr << "System terminates..." << endl;
      abort();
    }
    SpkCompilerError e(stde, message, line, filename);
    myError_list[myCnt] = e;
    ++myCnt;
}
SpkCompilerException::SpkCompilerException( const SpkCompilerError& e ) throw()
: myCnt(0)
{
    if( full() )
    {
      cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
      cerr << "The error list is full." << endl;
      cerr << "System terminates..." << endl;
      abort();
    }
    myError_list[myCnt] = e;
    ++myCnt;
}
SpkCompilerException::SpkCompilerException( const SpkCompilerException& e ) throw()
: myCnt(0)
{
    try{
        //strcpy(_format, e._format);
        for(int i=0; i<e.myCnt; i++ )
        {
            myError_list[i] = e.myError_list[i];
        }
    }
    catch(...)
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkCompilerError::SpkCompilerException(const SpkCompilerException&) shall not throw." << endl;
        cerr << "System terminates..." << endl;
        abort();
    }
    myCnt = e.myCnt;
}
SpkCompilerException::~SpkCompilerException() throw()
{
}
const SpkCompilerException& SpkCompilerException::operator=(const SpkCompilerException& right) throw()
{
    try{
        //strcpy(_format, right._format);
        for(int i=0; i<right.myCnt; i++ )
        {
            this->myError_list[i] = right.myError_list[i];
        }
    }
    catch(...)
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkCompilerError::operator=() shall not throw." << endl;
        cerr << "System terminates..." << endl;
        abort();
    }
    this->myCnt = right.myCnt;
    return *this;
}
SpkCompilerException& SpkCompilerException::push( const SpkCompilerError& e ) throw()
{
    if( full() )
    {
      cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
      cerr << "The error list is full." << endl;
      cerr << "System terminates..." << endl;
      abort();
    }
    try{
        myError_list[myCnt] = e;
    }
    catch(...)
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkCompilerError::push() shall not throw." << endl;
        cerr << "System terminates..." << endl;
        abort();
    }
    ++myCnt;
    return *this;
}

SpkCompilerException& SpkCompilerException::push( enum SpkCompilerError::ErrorCode code, const char* message, unsigned int line, const char* filename) throw()
{
    SpkCompilerError e(code, message, line, filename);
    return push(e);
}

const SpkCompilerError SpkCompilerException::pop() throw()
{
    if( empty() )
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkCompilerError::pop() tried to pop from an empty list...";
        cerr << "terminating..." << endl;
        abort();
    }
    return myError_list[--myCnt];
}
const SpkCompilerError& SpkCompilerException::operator[](int index) const throw()
{
    if( index > myCnt || index < 0 )
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkCompilerException::operator[] failed." << endl;
        cerr << "System terminates..." << endl;
        abort();
    }
    return myError_list[index];
}

unsigned int SpkCompilerException::size() const throw()
{
    return myCnt;
}
bool SpkCompilerException::full() const throw()
{
    if( myCnt >= SpkCompilerException::maxErrors() )
        return true;
    else 
        return false;
}
bool SpkCompilerException::empty() const throw()
{
    if( myCnt == 0 )
        return true;
    else
        return false;
}
const char* SpkCompilerException::what() const throw()
{
    return "SpkCompilerException";
}
int SpkCompilerException::find( const enum SpkCompilerError::ErrorCode code ) const throw()
{
    for( int i=0; i<myCnt; i++ )
    {
        if( myError_list[i].code() == code )
        {
            return i;
        }
    }
    return -1;
}
int SpkCompilerException::findFile( const char* filename ) const throw()
{
    for( int i=0; i<myCnt; i++ )
    {
        if( strcmp(myError_list[i].filename(), filename) == 0 )
        {
            return i;
        }
    }
    return -1;
}

const std::string SpkCompilerException::getXml() const
{
    ostringstream o;
    o << "<error_list length=\"" << this->size() << "\">" << endl;
    for( int i=0; i<this->size(); i++)
    {
        o << myError_list[i].getXml();
    }
    o << "</error_list>" << endl;
    return o.str();
}

std::ostream& operator<<(std::ostream& stream, const SpkCompilerException& e)
{
    stream.flush();
    stream << "count" << endl;
    stream << e.size() << endl;
    for( int i=0; i<e.size(); i++)
    {
      stream << e.myError_list[i];
    }
    stream.flush();
    return stream;
}
