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
 * class: SpkError
 *
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 * <First Cut>
 * 1st Generation Design & Implementation Decisions - Overall
 * 
 * Rule 1:
 * An exception, when it's thrown, never throw yet another exception
 * while stack unwinding.
 *
 * To prevent an exception object from throwing yet another exception
 * while stack unwinding, any constructor and assignment operator
 * had to be implemented carefully so that they don't propagate
 * an exception.  For consistency and strict exception safety,
 * all member functions were implemented in the same way.
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
 * Rule 2:
 * A string returned by serializig an object shall be easily parsable.
 *
 * To allow for user to specify an error message that
 * can contain any character (including NULL and escape sequences), 
 * the message string had to be a fixed size so that
 * a parser (ex. the class's unserializing function) has no
 * trouble reading it.
 *
 * This meant at least the message field in the serialized string had to 
 * be fixed sized.  For consistency, all other fields are alsow
 * fixed sized.
 * 
 *************************************************************************/
/*************************************************************************
 * <The Second Cut!>
 * 2nd Generation Design & Implementation Decisions - Overall
 * 
 * Modification to Rule 1:
 * > This meant no dynamic memory allocation.
 *
 * This caused an SpkException object which holds a list of
 * SpkError objects grows quite large.
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

/*------------------------------------------------------------------------
 * class Specification
 *------------------------------------------------------------------------*/

/*
$begin SpkError$$
$spell 
    Code code
    cpp
    const
    errorcode
    spkerrorcode
    linenum
    std
    Spk
    iostream
    namespace
    cout
    endl
    str
    unserialize
    ostream
    istream
    iostream
    Len
    trancated
    enum
    optimizer
    initializes
    denormal
    ind
    inv
    stde
    enumulator
$$

$section SpkError Class$$

$index SpkError$$
$index exception,$$
$index error handling,$$
$index error,$$

$table
$bold Header: $$ $cend 
SpkError.h $rend

$bold Constructor:$$ $rend
$cend $syntax/SpkError() throw()/$$   $rend
$cend $syntax/SpkError(enum ErrorCode /errorcode/, const char* /message/, unsigned int /line/, const char* /filename/) throw() /$$ $rend
$cend $syntax/SpkError(const std::exception /stde/, const char* /message/,unsigned int /line/, const char* /filename/) throw() /$$ $rend
$cend $syntax/SpkError(const SpkError& /right/) throw() /$$ $rend
$tend

$bold See also: $$ $xref/SpkException//SpkException/$$
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
An object of this class represents an error.  It can record an error code
describing the nature of the detected error, the line number at which the
error was detected, the name of the file in which the error was detected,
and an additional arbitrary information.
$pre

$$
An object of this class is not to be thrown by itself alone.  
Instead, $xref/SpkException//SpkException/$$ is designed to represent
an exception that can be thrown and caught.  
$pre

$$
This class should not be derived.  If derived, the pieces of information only
relevant to the derived class will not be carried out properly.


$head Constructor Arguments$$
$syntax/
/errorcode/
/$$
is a legal value defined in $code SpkError::ErrorCode$$ enumulator.
See the list of legal values under "Error Codes" in this document.

$syntax/

/message/
/$$
is a NULL terminated string that represents an error message.
The number of characters must be greater than 0 and shall not exceed 
the value returned by $code maxMessageLen()$$.
A message may not contain the $code \r$$ escape character.
The behavior when a value violating the above is given is undefined.
$syntax/

/linenum/
/$$
should indicate the line number at which the error was detected.  
You might find it convenient to use an ANSII macro, $code __LINE__$$.
$syntax/

/filename/
/$$
should indicate the filename in which the error was detected.
You might find it convenient to use an ANSII macro, $code __FILE__$$.

$syntax/

/stde/
/$$
is an std::exception object.  The error code is set to $code SpkError::SPK_STD_ERR$$.
$syntax/

/right/
/$$
is an $code SpkError$$ object.  The state and information held within $italic right$$
will be copied into the left hand side object.


$head Public Interfaces$$
$syntax/
const SpkError& operator=(const SpkError& /right/) 
/$$
Assignment operator.  The state and information held within the right hand side object
will be copied into the left hand side object.
$syntax/

unsigned int code() const throw()
/$$
Returns the error code.
$syntax/

unsigned int linenum() const throw()
/$$
Returns the line number.
$syntax/

const char* filename() const throw()
/$$
Returns the name of the file.
$syntax/

const char* message() const throw()
/$$
Returns the error message.
$syntax/

friend std::istream& operator>>(std::istream& /stream/, SpkError& /e/)
/$$
Read information from $italic stream$$ and insert it into $italic e$$.
$italic stream$$ may contain more than one serialized SpkError objects.
In such a case, each object must be separated by the special character,
$code \r$$.
$pre

$$
Each serialized SpkError object is in the format of:
$syntax/
    
    errorcode\n
    /error code/\n
    description\n
    /short default description of the error code/\n\r
    linenum\n
    /line number/\n
    filename\n
    /file name/\n
    message\n
    /extra info/\0

/$$
where the blue characters indicate exact string literals and the italic strings
are replaced by actual values.  $italic error code$$ is 
an error code, $italic line number$$ a line number, $italic file name$$
the name of a file which may not contain white spaces, 
and $italic extra info$$ an arbitrary text string which may not contain 
the $code \r$$ escape character.
$syntax/

friend std::string& operator>>(std::string& /s/, SpkError& /e/)
/$$
Behaves the same as $code std::istream& SpkError::operator>>(std::istream&, SpkError&)$$
does, except this version takes a std::string object in place of std::istream.


$syntax/

friend std::ostream& operator<<(std::ostream& /stream/, const SpkError& /e/)
/$$
Read information from $italic e$$ and extract to $italic stream$$
in the format described in the $code
std::istream& SpkError::operator>>(std::istream&, SpkError&)$$ section.
$syntax/

friend std::string& operator<<(std::string& /s/, const SpkError& /e/)
/$$
Behaves the same as $code std::ostream& SpkError::operator<<(std::ostream&, SpkError&)$$
does, except this version takes a std::string object in place of std::ostream.


$head Class Members$$
$subhead Error Codes$$
The following values have the type of $code static enum ErrorCode$$ and are published as public class members:

$table
SPK_TOO_MANY_ITER       $cend
See the description of $code NW_TOO_MANY_ITER$$ in $bold NAG C Library Mark 5$$.
$rend

SPK_NOT_CONVERGED       $cend
See the description of $code NW_NOT_CONVERTED$$ in $bold NAG C Library Mark 5$$.
$rend

SPK_KT_CONDITIONS       $cend
See the description of $code NW_KT_CONDITIONS$$ in $bold NAG C Library Mark 5$$.
$rend

SPK_LIN_NOT_FEASIBLE    $cend
See the description of $code NW_LIN_NOT_FEASIBLE$$ in $bold NAG C Library Mark 5$$.
$rend

SPK_NONLIN_NOT_FEASIBLE $cend
See the description of $code NW_NONLIN_NOT_FEASIBLE$$ in $bold NAG C Library Mark 5$$.
$rend

SPK_OPT_ERR     $cend
indicates a non-specific optimization-related error.
$rend

SPK_OPT_WARNING     $cend
indicates a non-specific optimization-related $bold warning$$.  
NOTE: This category of non-fatal errors should be later handled by
a mechanism different from the exception handling.
$rend

SPK_UNKNOWN_OPT_ERR     $cend
indicates an yet-to-be-identified error reported by the optimizer.  
This should be further classified as more errors are identified.
$rend

$cend
$rend

SPK_MODEL_NOT_IMPLEMENTED_ERR       $cend
indicates that the virtual function representing the model is
not implemented by user.
$rend

SPK_MODEL_SET_IND_ERR               $cend
indicates an error detected in $xref/SpkModel_setIndPar//setIndPar/$$.
$rend

SPK_MODEL_SET_POP_ERR               $cend
indicates an error detected in $xref/SpkModel_setPopPar//setPopPar/$$.
$rend

SPK_MODEL_SET_INDEX_ERR             $cend
indicates an error detected in $xref/SpkModel_selectIndividual//selectIndividual/$$.
$rend

SPK_MODEL_DATA_MEAN_ERR             $cend
indicates an error detected in $xref/SpkModel_dataMean//dataMean/$$.
$rend

SPK_MODEL_DATA_MEAN_POP_ERR         $cend
indicates an error detected in $xref/SpkModel_dataMean_popPar//dataMean_popPar/$$.
$rend

SPK_MODEL_DATA_MEAN_IND_ERR         $cend
indicates an error detected in $xref/SpkModel_dataMean_indPar//dataMean_indPar/$$.
$rend
SPK_MODEL_DATA_VARIANCE_ERR         $cend
indicates an error detected in $xref/SpkModel_dataVariance//dataVariance/$$.
$rend

SPK_MODEL_DATA_VARIANCE_POP_ERR     $cend
indicates an error detected in $xref/SpkModel_dataVariance_popPar//dataVariance_popPar/$$.
$rend

SPK_MODEL_DATA_VARIANCE_IND_ERR     $cend
indicates an error detected in $xref/SpkModel_dataVariance_indPar//dataVariance_indPar/$$.
$rend

SPK_MODEL_INV_DATA_VARIANCE_ERR     $cend
indicates an error detected in $xref/SpkModel_dataVarianceInv//dataVarianceInv/$$.
$rend

SPK_MODEL_INV_DATA_VARIANCE_POP_ERR $cend
indicates an error detected in $xref/SpkModel_dataVarianceInv_popPar//dataVarianceInv_popPar/$$.
$rend

SPK_MODEL_INV_DATA_VARIANCE_IND_ERR $cend
indicates an error detected in $xref/SpkModel_dataVarianceInv_indPar//dataVarianceInv_indPar/$$.
$rend

SPK_MODEL_IND_VARIANCE_ERR          $cend
indicates an error detected in $xref/SpkModel_indParVariance//indParVariance/$$.
$rend

SPK_MODEL_IND_VARIANCE_POP_ERR      $cend
indicates an error detected in $xref/SpkModel_indParVariance_popPar//indParVariance_popPar/$$.
$rend

SPK_MODEL_INV_IND_VARIANCE_ERR      $cend
indicates an error detected in $xref/SpkModel_indParVarianceInv//indParVarianceInv/$$.
$rend

SPK_MODEL_INV_IND_VARIANCE_POP_ERR  $cend
indicates an error detected in $xref/SpkModel_indParVarianceInv_popPar//indParVarianceInv_popPar/$$.
$rend

$cend
$rend

SPK_FP_UNDERFLOW_ERR    $cend
indicates an underflow has been detected.
$rend
SPK_FP_OVERFLOW_ERR     $cend
indicates an overflow has been detected.
$rend
SPK_FP_DENORMAL_ERR     $cend
indicates an denormal loss has been detected.
$rend
SPK_FP_INEXACT_ERR      $cend
indicates an inexact representation has been detected.
$rend
SPK_FP_INVALID_ERR      $cend
indicates an invalid representation has been detected.
$rend
SPK_FP_ZERODIVIDE_ERR   $cend
indicates a divide by zero error has been detected.
$rend

$cend
$rend

SPK_STD_ERR             $cend
indicates an error from the standard library.
$rend

SPK_DIFF_ERR            $cend
indicates an error occurred during differentiation.
$rend

SPK_NOT_INVERTABLE_ERR  $cend
indicates that the matrix is not invertible.
$rend

SPK_NOT_POS_DEF_ERR $cend
indicates that the matrix is not positive definite.
$rend

SPK_NOT_SYMMETRIC_ERR $cend
indicates that the matrix is not symmetric.
$rend

SPK_USER_INPUT_ERR      $cend
This constant value is to be used to indicate an invalid parameter value given by the end user.
$rend

SPK_PARALLEL_ERR        $cend
indicates an error occurred during the Master-Node communication.
$rend

SPK_PARALLEL_END_SIGNAL  $cend
indicates the end of Spk.  This is not an error; it is a signal generated in an exceptional circumstance. 
$rend

SPK_INSUFFICIENT_MEM_ERR     $cend
indicates insufficiency in the heap to dynamically allocate more memory.
$rend

$cend
$rend

SPK_UNKNOWN_ERR         $cend
This constant value is to be used to indicate an invalid parameter value given by the end user.
$rend
$tend

$subhead Methods Returning Class Properties$$
$syntax/

static unsigned int maxLinenum() const throw()
/$$
This class member function returns the maximum value allowed for a line number.
A line number greater than this value will lose digits.

$syntax/

static unsigned int maxFilenameLen() const throw()
/$$
This class member function returns the maximum number of characters allowed for a file name
A file name (with path) longer than this value will be trancated.

$syntax/

static unsigned int maxMessageLen() const throw()
/$$
This class member function returns the maximum number of characters allowed for a message.
A message longer than this value will be trancated.



$head Example$$
If you save the following as $code main.cpp$$, compile, link, and run:
$codep

    #include <iostream>
    #include "SpkException.h"

    using namespace std;

    int main()
    {
        SpkError e(SpkError::SPK_FP_ZERODIVIDE_ERR, "Divide by zero error", __LINE__, __FILE__);
        cout << e << endl;
        return 0;
    }

$$
then it will display the following when it is run:
$codep

errorcode
$$
$italic ... an integer defined as SpkError::SPK_FP_ZERODIVIDE_ERR (e.x. 3) ... $$
$codep
description
SPK_FP_ZERODIVIDE_ERR
linenum
8
filename
main.cpp
message
floating point error

$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * The set of error codes shall be added as more kinds of error are identified.
 *
 *------------------------------------------------------------------------*/
//
// These pragma are to disable "too long literals" warnings generated
// as a result of using STL components.
//
#pragma warning( disable : 4786 )
#include <map>

#include <iostream>
#include <exception>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <cassert>
#include <fstream>

#include "SpkError.h"

/*------------------------------------------------------------------------
 * Namespaces used
 *------------------------------------------------------------------------*/
using namespace SpkError_const;
using namespace std;
/*------------------------------------------------------------------------
 * Static functions
 *------------------------------------------------------------------------*/
//
// Flush the buffer and copy the chars from begin to end to the buffer.
//
static const char *const substr(const char * begin, const char * end, char * buf, int bufsize) throw()
{
    try{
        std::fill(buf, buf+bufsize, '\0');
        std::copy(begin, end, buf);
    }
    catch(...)
    {
        cerr << "SpkError::substr() shall not throw... terminating..." << endl;
        abort();
    }
    return buf;
}
/*------------------------------------------------------------------------
 * Static member functions
 *------------------------------------------------------------------------*/
const SpkError::ErrorMap SpkError::mapping = SpkError::fillErrorMap();
const char* SpkError::describe( enum ErrorCode key )
{
  ErrorMap::const_iterator ptr = mapping.find(key);
  if( ptr != mapping.end() )
    return (*ptr).second;
  else
    return "No description registered.";
}
unsigned int SpkError::maxErrorcode() throw()
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
        cerr << "SpkError::maxErrorcode() shall not throw... terminating..." << endl;
        abort();
    }
}
unsigned int SpkError::maxLinenum() throw()
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
        cerr << "SpkError::maxLinenum() shall not throw... terminating..." << endl;
        abort();
    }
}

unsigned int SpkError::maxFilenameLen() throw()
{
    return FILENAME_FIELD_LEN;
}

unsigned int SpkError::maxMessageLen() throw()
{
    return MESSAGE_FIELD_LEN;
}

/*------------------------------------------------------------------------
 * Constructors & destructor
 *------------------------------------------------------------------------*/
SpkError::SpkError() throw()
: _errorcode(), _linenum(0)
{
    try{
        std::fill(_filename, _filename+FILENAME_FIELD_LEN+1, '\0');
        std::fill(_message, _message+MESSAGE_FIELD_LEN+1, '\0');
    }
    catch(...)
    {
        cerr << "SpkError::SpkError() shall not throw... terminating..." << endl;
        abort();
    }
}
SpkError::SpkError(enum ErrorCode ecode, const char* mess, unsigned int line, const char* file) throw() 
: _errorcode(ecode), _linenum(line)
{
    try{
      if( strlen(file) > maxFilenameLen() )
      {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "The length of a filename must be less than " << strlen(_filename) << endl;
        cerr << "System terminates..." << endl;
        abort();
        
      }
        strcpy(_filename, file);
        strcpy(_message, mess);
    }
    catch( ... )
    {
        cerr << "SpkError::operator= shall not throw... terminating..." << endl;
        abort();
    }
}
SpkError::SpkError( const std::exception& e, const char* mess, unsigned int line, const char* file) throw()
: _errorcode(SpkError::SPK_STD_ERR), _linenum(line)
{
    try{
      if( strlen(mess) > maxMessageLen() )
      {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "The length of a message must be less than " << strlen(_message) << endl;
        cerr << "System terminates..." << endl;
        abort();
        
      }
      if( strlen(file) > maxFilenameLen() )
      {
        cerr << "Unrecoverable error occured at " << __LINE__ << " in " << __FILE__ << endl;
        cerr << "The length of a filename must be less than " << strlen(_filename) << endl;
        cerr << "System terminates..." << endl;
        abort();
        
      }
        strcpy(_filename, file);
        strcpy(_message, mess);
        strcat(_message, "\n");
        strcat(_message, e.what());
        strcat(_message, "\n");
    }
    catch( ... )
    {
        cerr << "SpkError::operator= shall not throw... terminating..." << endl;
        abort();
    }
}


SpkError::SpkError( const SpkError& e ) throw()
: _errorcode(e._errorcode), _linenum(e._linenum)
{
    // This is doing deep copy
    try{
        strcpy(_filename, e._filename);
        strcpy(_message, e._message);
    }
    catch( ... )
    {
        cerr << "SpkError::operator= shall not throw... terminating..." << endl;
        abort();
    }
}
SpkError::~SpkError() throw()
{
}
/*------------------------------------------------------------------------
 * Public member functions
 *------------------------------------------------------------------------*/
const SpkError& SpkError::operator=(const SpkError& right) throw()
{
    // This is doing deep copy
    try{
        _errorcode      = right._errorcode;
        _linenum        = right._linenum;
        strcpy(_filename, right._filename);
        strcpy(_message,  right._message);
        return *this;
    }
    catch( ... )
    {
        cerr << "SpkError::operator= shall not throw... terminating..." << endl;
        abort();
    }
}
std::string& operator<<(std::string& s, const SpkError& e) 
{
    ostringstream stream;
    stream << e;
    s = stream.str();
    return s;
}
std::string& operator>>(std::string& s, SpkError& e)
{
    std::istringstream stream(s);
    stream >> e;
    s = stream.str();
    return s;
}

std::ostream& operator<<(std::ostream& stream, const SpkError& e)
{
    stream << ERRORCODE_FIELD_NAME << endl;
    stream << e._errorcode << endl;

    stream << ERRORCODE_DESCRIPTION_FIELD_NAME << endl;
    stream << SpkError::describe( e._errorcode ) << endl << '\r';

    stream << LINENUM_FIELD_NAME   << endl;
    stream << e._linenum   << endl;
    
    stream << FILENAME_FIELD_NAME  << endl;
    stream << e._filename  << endl;
    
    stream << MESSAGE_FIELD_NAME   << endl;
    stream << e._message;
    stream.put('\0');
   
    return stream;
}
std::istream& operator>>(std::istream& stream, SpkError& e)
{
    char buf[256];

    stream >> buf;
    assert(strcmp(buf, ERRORCODE_FIELD_NAME)==0);
    stream >> buf;
    e._errorcode = static_cast<SpkError::ErrorCode>(atoi(buf));

    stream >> buf;
    assert(strcmp(buf, ERRORCODE_DESCRIPTION_FIELD_NAME)==0);
    stream.getline( buf, 256 ); // eat the trailing '\n'
    stream.getline( buf, 256, '\r');
    // don't do anything

    stream >> buf;
    assert(strcmp(buf,LINENUM_FIELD_NAME)==0);
    stream >> buf;
    e._linenum = atoi(buf);

    stream >> buf;
    assert(strcmp(buf,FILENAME_FIELD_NAME)==0);
    stream >> e._filename;

    stream >> buf;
    assert(strcmp(buf,MESSAGE_FIELD_NAME)==0);
    //
    // istream::getline(...) does not eat leading whitespaces, 
    // while istream::opeartor>>(...) does eat and gets the first relevant text string.
    // Since the serialized SpkError::_filename is terminated with a new line charactor, '\n',
    // that new line charactor must be eaten first.  Otherwise,
    // since getline() reads from whereever istream::seek() returns to 
    // a new line charactor, the first attempt gets only empty (or just '\n') in the
    // buffer.  So, the first getline among the two is to eat the '\n' and the 
    // leading whitespaces.  The second getline is really getting a relevent text string.
    //
    // The second getline reads till a special charactor, '\r', appears.
    // When the stream object contains more than one serialized SpkError objects,
    // each object must be separated by '\r'.  Otherwise it reads till NULL appears.
    //
    stream.getline(buf, 256);
    stream.getline(buf, 256, '\r');

    strcpy(e._message, buf);
    return stream;
}

enum SpkError::ErrorCode SpkError::code() const throw()
{
    return _errorcode;
}
unsigned int SpkError::linenum() const throw()
{
    return _linenum;
}

const char* SpkError::filename() const throw()
{
    return _filename;
}

const char* SpkError::message() const throw()
{
    return _message;
}
const SpkError::ErrorMap SpkError::fillErrorMap()
{
    //
    // Filling the error code-message map
    //
    ErrorMap tmpMap;
    tmpMap.insert( ErrorMap::value_type(SPK_TOO_MANY_ITER, 
      "SPK_TOO_MANY_ITER") );
    tmpMap.insert( ErrorMap::value_type(SPK_NOT_CONVERGED, 
      "SPK_NOT_CONVERGED") );
    tmpMap.insert( ErrorMap::value_type(SPK_KT_CONDITIONS, 
      "SPK_KT_CONDITIONS") );
    tmpMap.insert( ErrorMap::value_type(SPK_LIN_NOT_FEASIBLE, 
      "SPK_LIN_NOT_FEASIBLE") );
    tmpMap.insert( ErrorMap::value_type(SPK_NONLIN_NOT_FEASIBLE , 
      "SPK_NONLIN_NOT_FEASIBLE") );
    tmpMap.insert( ErrorMap::value_type(SPK_OPT_ERR, 
      "SPK_OPT_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_OPT_WARNING, 
      "SPK_OPT_WARNING") );
    tmpMap.insert( ErrorMap::value_type(SPK_UNKNOWN_OPT_ERR, 
      "SPK_UNKNOWN_OPT_ERR") );
	tmpMap.insert( ErrorMap::value_type(SPK_NOT_READY_WARM_START_ERR, 
      "SPK_NOT_READY_WARM_START_ERR") );

    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_NOT_IMPLEMENTED_ERR, 
      "SPK_MODEL_NOT_IMPLEMENTED_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_SET_IND_ERR, 
      "SPK_MODEL_SET_IND_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_SET_POP_ERR, 
      "SPK_MODEL_SET_POP_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_SET_INDEX_ERR, 
      "SPK_MODEL_SET_INDEX_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_DATA_MEAN_ERR, 
      "SPK_MODEL_DATA_MEAN_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_DATA_MEAN_POP_ERR, 
      "SPK_MODEL_DATA_MEAN_POP_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_DATA_MEAN_IND_ERR, 
      "SPK_MODEL_DATA_MEAN_IND_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_DATA_VARIANCE_ERR, 
      "SPK_MODEL_DATA_VARIANCE_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_DATA_VARIANCE_POP_ERR, 
      "SPK_MODEL_DATA_VARIANCE_POP_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_DATA_VARIANCE_IND_ERR, 
      "SPK_MODEL_DATA_VARIANCE_IND_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_INV_DATA_VARIANCE_ERR, 
      "SPK_MODEL_INV_DATA_VARIANCE_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_INV_DATA_VARIANCE_POP_ERR, 
      "SPK_MODEL_INV_DATA_VARIANCE_POP_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_INV_DATA_VARIANCE_IND_ERR, 
      "SPK_MODEL_INV_DATA_VARIANCE_IND_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_IND_VARIANCE_ERR, 
      "SPK_MODEL_IND_VARIANCE_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_IND_VARIANCE_POP_ERR, 
      "SPK_MODEL_IND_VARIANCE_POP_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_INV_IND_VARIANCE_ERR, 
      "SPK_MODEL_INV_IND_VARIANCE_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_MODEL_INV_IND_VARIANCE_POP_ERR, 
      "SPK_MODEL_INV_IND_VARIANCE_POP_ERR") );

    tmpMap.insert( ErrorMap::value_type(SPK_FP_UNDERFLOW_ERR,   "SPK_FP_UNDERFLOW_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_FP_OVERFLOW_ERR,    "SPK_FP_OVERFLOW_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_FP_DENORMAL_ERR,    "SPK_FP_DENORMAL_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_FP_INEXACT_ERR,     "SPK_FP_INEXACT_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_FP_INVALID_ERR,     "SPK_FP_INVALID_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_FP_ZERODIVIDE_ERR,  "SPK_FP_ZERODIVIDE_ERR") );
    
    tmpMap.insert( ErrorMap::value_type(SPK_STD_ERR,            "SPK_STD_ERR") );

    tmpMap.insert( ErrorMap::value_type(SPK_DIFF_ERR,           "SPK_DIFF_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_NOT_INVERTABLE_ERR, "SPK_NOT_INVERTABLE_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_NOT_POS_DEF_ERR,    "SPK_NOT_POS_DEF_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_NOT_SYMMETRIC_ERR,  "SPK_NOT_SYMMETRIC_ERR") );

    tmpMap.insert( ErrorMap::value_type(SPK_USER_INPUT_ERR,     "SPK_USER_INPUT_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_PARALLEL_ERR,       "SPK_PARALLEL_ERR") );
    tmpMap.insert( ErrorMap::value_type(SPK_PARALLEL_END_SIGNAL,"SPK_PARALLEL_END_SIGNAL") );

    tmpMap.insert( ErrorMap::value_type(SPK_INSUFFICIENT_MEM_ERR,"SPK_INSUFFICIENT_MEM_ERR") );

    tmpMap.insert( ErrorMap::value_type(SPK_UNKNOWN_ERR,        "SPK_UNKNOWN_ERR") );

    return tmpMap;
}
