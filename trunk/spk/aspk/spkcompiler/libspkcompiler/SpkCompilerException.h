#ifndef SPKCOMPILEREXCEPTION_H
#define SPKCOMPILEREXCEPTION_H

//
// Necessary component headers for SpkCompilerException class declaration.
//
#include <string>
#include "SpkCompilerError.h"

//
// A namespace used within SpkCompilerException class members.
//
/*
namespace SpkCompilerException_const
{
    using namespace SpkCompilerError_const;
    const int MAX_ERRORS        = 16;
    const int MAX_ERRORS_DIGITS =  2;
    const int EXCEPTION_SIZE    = MAX_ERRORS_DIGITS +  6 + ( MAX_ERRORS * ERROR_SIZE );
}
*/
//
// SpkCompilerException class declaration
//
class SpkCompilerException
{
public:	
    static unsigned int maxErrors() throw();
    static const int MAX_ERRORS;
    static const int MAX_ERRORS_DIGITS;
    static const int EXCEPTION_SIZE;
    // default constructor
    SpkCompilerException() throw();


    // the constructor that takes an error object to be added to the head of the list
    SpkCompilerException( const SpkCompilerError& ) throw();
    SpkCompilerException( enum SpkCompilerError::ErrorCode, 
			  const char* message, 
			  unsigned int line, 
			  const char* filename ) throw();
    SpkCompilerException( const std::exception&, 
			  const char* message, 
			  unsigned int line, 
			  const char* filename ) throw();

    // copy constructor which performs deep copy
    SpkCompilerException( const SpkCompilerException& ) throw();

    // destructor which does nothing
    ~SpkCompilerException() throw();

    // assingment operator which performs deep copy
    const SpkCompilerException& operator=( const SpkCompilerException& ) throw();

    // append an error object to the list
    SpkCompilerException& push( const SpkCompilerError& ) throw();
    SpkCompilerException& push( enum SpkCompilerError::ErrorCode, 
				const char* message, 
				unsigned int, 
				const char* filename ) throw();


    // remove and return the most recent Error object
    const SpkCompilerError pop() throw();

    // return the number of error objects accumulated so far
    unsigned int size() const throw();

    // returns the const reference to the specified error obejct
    const SpkCompilerError& operator[]( int index ) const throw();

    // returns this class name
    const char * what() const throw();

    bool full    () const throw();
    bool empty   () const throw();
    int  find    ( const enum SpkCompilerError::ErrorCode code ) const throw();
    int  findFile( const char* filename ) const throw();

    // serialize
    const std::string getXml() const;    
    friend std::ostream& operator<<(std::ostream& stream, const SpkCompilerException& e);


 private:
    // a fixed length list of Error objects
    SpkCompilerError myError_list[16]; //[SpkCompilerException::MAX_ERRORS];

    // counting the number of errors added to the list so far
    int myCnt;


};

#endif
