/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
/*************************************************************************
 *
 * File: SpkException.cpp
 *
 *
 * Defines SpkException class.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 * <The First Cut!>
 * 1st Generation Design & Implementation Decisions - Overall
 * 
 * Rule 1:
 * An exception, when it's thrown, never throw yet another exception
 * while stack unwinding.
 *
 * To prevent an exception object from throwing yet another exception
 * while stack unwinding, any constructor and assignment operator
 * had to be implemented carefully so that they don't propagate
 * yet another exception.  For consistency and strict exception safety,
 * all member functions were implemented under this strict policy.
 *
 * This meant no dynamic memory allocation.
 *
 * As a result, all strings are maintained in fixed size char arrays,
 * allowing memory allocations at compilation time.
 * Convenient standard library components like std::string or 
 * std::stringbuf are, therefore, not allowed to be used here
 * since they can generate exceptions.
 *
 *
 * To achive complete exception safetiness, the error object class
 * is also developed under the same policy.  Read the documentation
 * of SpkError class for further details.
 *
 *
 * Rule 2:
 * An exception object must travel through C routines without
 * dropping any relevent information.  In C is no *class*
 * or no exception handling.
 *
 * To achieve this goal without necessarily imposing Clients
 * to implement complex member functions such as serialization and
 * unserialization, the exception class had to designed without
 * inheritance.  "Clients" in this sense include Spk internal developers
 * and the external end users who may also provide User-provided Models.
 *************************************************************************/
/*************************************************************************
 * <The Second Cut!>
 * 2nd Generation Design & Implementation Decisions - Overall
 * 
 * Modification to Rule 1:
 * > This meant no dynamic memory allocation.
 *
 * This caused an SpkException object grows quite large.
 * Mover over, when such an exception is attempted to
 * be caught more than, say, a couple times during an execution,
 * it quickly chewed up the exception-specific heap.
 *
 * One alternative we could think of was minimize the size
 * of an SpkException object.  To accomplish that,
 * we had to give up the strict "no throw" policy on
 * serializing and unserializing functions.
 * Our justification was since these functions are
 * called by clients, which is outside of heap,
 * to perform IO operations, it's impossible to be
 * perfectly exception safe anyway.
 *
 * Modification to Rule 2:
 * N/A.
 *
 *************************************************************************/

/*************************************************************************
 *
 * Class: SpkException
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin SpkException$$
$spell
    Code code 
    Spk
    const
    errorcode
    linenum
    std
    runtime
    cout 
    iostream
    istream
    ostream
    namespace
    instantiate
    cpp
    unserialize
    nd
    st
    str
    bool
    maxErrorcodes
    enum
    instanciate
    filename
$$

$section SpkException Class$$

$index SpkException class$$
$index exception, SpkException$$
$index error handling, SpkException$$

$table
$bold Header:$$ $cend
SpkException.h $rend

$bold Constructor:$$
$cend $syntax/SpkException() throw() /$$ $rend
$cend $syntax/SpkException(const SpkError& /e/) throw()/$$ $rend
$cend $syntax/SpkException(const SpkException& /right/) throw() /$$ $rend
$cend $syntax/SpkException(enum SpkError::ErrorCode /code/, const char* /message/, unsigned int /line/, const char* /filename/) throw()/$$ $rend
$tend

$bold See also: $$ $xref/SpkError//SpkError/$$.
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This is the exception the Spk library throws when it detects errors during a session.  
A SpkException object
maintains a list of $xref/SpkError//SpkError/$$ objects that are added 
by client during the course of execution.
$pre

$$
A SpkException should be thrown when an error is detected and be caught by a client that
knows what to do with it, either partially or completely; no intermediate
clients shall attempt to catch the exception.


$head Constructors$$
$syntax/
SpkException() throw()
/$$
Default constructor.
$syntax/

SpkException(const SpkError& /e/) throw()
/$$
Instantiate SpkException with $italic e$$ as the first error.
$syntax/

SpkException( enum SpkError::ErrorCode /code/, const char* /message/, unsigned int /line/, const char* /filename/) throw()
/$$
Create a $xref/SpkError//SpkError/$$ object, instanciate SpkException and adds to the head of the error list.
$syntax/

SpkException(const SpkException& /right/) throw()
/$$
(deep) Copy constructor.


$head Public Interfaces$$
$syntax/
const SpkException& operator=(const SpkException& /right/) throw() 
/$$
Assignment operator (performing deep copy).
$syntax/

SpkException& push( const SpkError& /e/) throw()
/$$
Appends $italic e$$ to the list of errors.  If the list is already full,
it aborts the execution.  You can determine the maximum
number of errors an exception object can hold with
$code static SpkException::maxErrorcodes()$$ or test whether it is full or not with
$code SpkException::full()$$.
$syntax/

SpkException& push( enum SpkError::ErrorCode /code/, const char* /message/, unsigned int /line/, const char* /filename/) throw()
/$$
Create a $xref/SpkError//SpkError/$$ object and append it to the list of errors.  If the list is already full,
it aborts the execution.  You can determine the maximum
number of errors an exception object can hold with
$code static SpkException::maxErrorcodes()$$ or test whether it is full or not with
$code SpkException::full()$$.
$syntax/

const SpkError pop() throw()
/$$
Returns the most recently added Error object and remove the object from the list
of errors.  If the list is already empty, it aborts the execution.
You can test whether it is full or not with
$code SpkException::empty()$$.
$syntax/

const SpkError& operator[](int /index/) const throw()
/$$
Returns the $italic index$$-th $xref/SpkError//SpkError/$$ object found in the
list of errors. If an invalid value is given, it aborts the execution.
$syntax/

int size() const throw()
/$$
Returns the number of $italic SpkError$$ objects accumulated so far.
$syntax/

bool full() const throw()
/$$
Returns true if the error list is full.
$syntax/

bool empty() const throw()
/$$
Returns true if the error list is empty.
$syntax/

int find(const enum SpkError::ErrorCode code) const
/$$
Returns the position of the first occurrence of $italic SpkError$$ object that contains $italic code$$
as the error code in the error list (0 <= position < size()) if there is a match.
If there is no match, returns a negative number.

$syntax/

int findFile(const char* filename) const
/$$
Returns the position of the first occurrence of $italic SpkError$$ object that contains $italic filename$$
in the error list (0 <= position < size()) if there is an $bold exact$$ match.
If there is no exact match, returns a negative number.

$syntax/

const char* what() const throw()
/$$
Returns the name of this class.
$syntax/

friend std::ostream& operator<<(std::ostream& stream, const SpkException& e);
/$$
Returns a NULL terminated string that represents 
this $code SpkException$$ object in the format below:
$syntax/

    count\n
    /number of errors/\n
    /1st error/\r
    /2nd error/\r
    /.../\r
    /n-th error/\r
    \0

/$$
where the blue characters indicate exact string literals and
the italic words are to be replaced by actual values.
$italic number of errors$$ specifies the number of
SpkError objects accumulated so far, $italic 1st error$$ the firstly 
caught error object, $italic 2nd error$$ the secondly caught error and 
so on, each separated by the special $code \r$$ character.
$pre

$$
where each error object is expressed in the format 
described in the $xref/SpkError//SpkError/$$ section.

$syntax/

friend std::string& operator<<(std::string& /s/, const SpkException& /e/)
/$$
Behaves the same as $code 
std::ostream& operator<<(std::ostream&, const SpkException&)$$ does,
except this version takes a std::string object in place of std::ostream. 
$syntax/
 
friend std::istream& operator>>(std::istream& /stream/, SpkException& /e/)
/$$
Read information from e and extract to stream in the format 
described in the std::istream& SpkException::operator>>(std::istream&, SpkException&) section
$syntax/

friend std::string& operator>>(std::string& /s/, SpkException& /e/);
/$$
Behaves the same as $code 
std::ostream& operator>>(std::istream&, SpkException&)$$ does,
except this version takes a std::string object in place of std::istream. 



$head Class Member Functions$$
$syntax/

static unsigned int maxErrors() throw()
/$$
Returns the maximum number of SpkError objects a SpkException can 
hold.


$head Example$$
If you save the following program as $code main.cpp$$, compile, link, and run:
$codep
    #include <iostream>
    #include "SpkException.h"

    double f(double a, double b)
    { 
        if( b == 0.0 )
        {
            throw SpkException( SpkError::SPK_FP_ZERODIVIDE_ERR, "divide by zero", __LINE__, __FILE__ );
        }
        return a / b;
    }
    using namespace std;
    int main()
    {
        double a = 3;
        double b = 0;
        double c = 0;

        try{
           c = f(a,b);
        }
        catch( const SpkException& e )
        {
            cout << e;
            return -1;
        }

        return 0;
    }
$$
then it will display the following when it is run:
$codep

errorcode
$$
$italic ... an integer defined as SpkError::SPK_FP_ZERODIVIDE_ERR will appear here (e.x. 3)...$$
$codep
linenum
8
filename
main.cpp
message
divide by zero
$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 *
 *------------------------------------------------------------------------*/
/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#pragma warning ( disable : 4786 )

#include <iostream>
#include <string>
#include <sstream>
#include <cassert>

#include "SpkException.h"

/*------------------------------------------------------------------------
 * Namespaces used
 *------------------------------------------------------------------------*/
using namespace SpkException_const;
using namespace std;
/*------------------------------------------------------------------------
 * Static global
 *------------------------------------------------------------------------*/
//
// Flush the buffer and copy the string from begin to end to the buffer.
//
static const char *const substr(const char * begin, const char * end, char * buf, int bufsize) throw()
{
    try{
        std::fill(buf, buf+bufsize, '\0');
        std::copy(begin, end, buf);
    }
    catch(...)
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkError::substr() shall not throw." << endl;
      cerr << "System terminates..." << endl;
        abort();
    }
    return buf;
}

/*------------------------------------------------------------------------
 * Class definition
 *------------------------------------------------------------------------*/

unsigned int SpkException::maxErrors() throw()
{
    return MAX_ERRORS;
}
SpkException::SpkException() throw()
: _cnt(0)
{
    //constructFormat();
}
SpkException::SpkException( enum SpkError::ErrorCode code, const char* message, unsigned int line, const char* filename) throw()
: _cnt(0)
{
    if( full() )
    {
      cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
      cerr << "The error list is full." << endl;
      cerr << "System terminates..." << endl;
      abort();
    }
    SpkError e(code, message, line, filename);
    _error_list[_cnt] = e;
    ++_cnt;
}

SpkException::SpkException( const std::exception& stde, const char* message, unsigned int line, const char* filename) throw()
: _cnt(0)
{
    if( full() )
    {
      cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
      cerr << "The error list is full." << endl;
      cerr << "System terminates..." << endl;
      abort();
    }
    SpkError e(stde, message, line, filename);
    _error_list[_cnt] = e;
    ++_cnt;
}
SpkException::SpkException( const SpkError& e ) throw()
: _cnt(0)
{
    if( full() )
    {
      cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
      cerr << "The error list is full." << endl;
      cerr << "System terminates..." << endl;
      abort();
    }
    _error_list[_cnt] = e;
    ++_cnt;
}
SpkException::SpkException( const SpkException& e ) throw()
: _cnt(0)
{
    try{
        //strcpy(_format, e._format);
        for(int i=0; i<e._cnt; i++ )
        {
            _error_list[i] = e._error_list[i];
        }
    }
    catch(...)
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkError::SpkException(const SpkException&) shall not throw." << endl;
        cerr << "System terminates..." << endl;
        abort();
    }
    _cnt = e._cnt;
}
SpkException::~SpkException() throw()
{
}
const SpkException& SpkException::operator=(const SpkException& right) throw()
{
    try{
        //strcpy(_format, right._format);
        for(int i=0; i<right._cnt; i++ )
        {
            this->_error_list[i] = right._error_list[i];
        }
    }
    catch(...)
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkError::operator=() shall not throw." << endl;
        cerr << "System terminates..." << endl;
        abort();
    }
    this->_cnt = right._cnt;
    return *this;
}
SpkException& SpkException::push( const SpkError& e ) throw()
{
    if( full() )
    {
      cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
      cerr << "The error list is full." << endl;
      cerr << "System terminates..." << endl;
      abort();
    }
    try{
        _error_list[_cnt] = e;
    }
    catch(...)
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkError::push() shall not throw." << endl;
        cerr << "System terminates..." << endl;
        abort();
    }
    ++_cnt;
    return *this;
}

SpkException& SpkException::push( enum SpkError::ErrorCode code, const char* message, unsigned int line, const char* filename) throw()
{
    SpkError e(code, message, line, filename);
    return push(e);
}

const SpkError SpkException::pop() throw()
{
    if( empty() )
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkError::pop() tried to pop from an empty list...";
        cerr << "terminating..." << endl;
        abort();
    }
    return _error_list[--_cnt];
}
const SpkError& SpkException::operator[](int index) const throw()
{
    if( index > _cnt || index < 0 )
    {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "SpkException::operator[] failed." << endl;
        cerr << "System terminates..." << endl;
        abort();
    }
    return _error_list[index];
}

unsigned int SpkException::size() const throw()
{
    return _cnt;
}
bool SpkException::full() const throw()
{
    if( _cnt >= SpkException::maxErrors() )
        return true;
    else 
        return false;
}
bool SpkException::empty() const throw()
{
    if( _cnt == 0 )
        return true;
    else
        return false;
}
const char* SpkException::what() const throw()
{
    return "SpkException";
}
int SpkException::find( const enum SpkError::ErrorCode code ) const throw()
{
    for( int i=0; i<_cnt; i++ )
    {
        if( _error_list[i].code() == code )
        {
            return i;
        }
    }
    return -1;
}
int SpkException::findFile( const char* filename ) const throw()
{
    for( int i=0; i<_cnt; i++ )
    {
        if( strcmp(_error_list[i].filename(), filename) == 0 )
        {
            return i;
        }
    }
    return -1;
}

std::string& operator<<(std::string& s, const SpkException& e)
{
    std::ostringstream stream;
    stream << e;
    s = stream.str();
    return s;
}
std::ostream& operator<<(std::ostream& stream, const SpkException& e)
{
    std::string buf;

    stream << "count" << endl;
    stream << e.size() << endl;
    for( int i=0; i<e.size(); i++)
    {
        buf << e._error_list[i];
        stream << buf << '\r' << endl << endl;
    }
    stream.put('\0');
    return stream;
}

std::string& operator>>(std::string& s, SpkException& e)
{
    std::istringstream stream(s);
    stream >> e;
    s = stream.str();
    return s;
}
std::istream& operator>>(std::istream& stream, SpkException& e)
{
    char buf[256];

    stream >> buf;
    assert(strcmp(buf, "count")==0);
    stream >> buf;
    e._cnt = atoi(buf);

    for( int i=0; i<e.size(); i++ )
    {
        stream >> e._error_list[i];
    }
    return stream;
}
