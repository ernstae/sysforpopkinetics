/**
 * @file SpkCompilerException.h
 * Declare SpkCompilerException class.
 */
#ifndef SPKCOMPILEREXCEPTION_H
#define SPKCOMPILEREXCEPTION_H

//
// Necessary component headers for SpkCompilerException class declaration.
//
#include <string>
#include "SpkCompilerError.h"

/** 
 * Representation of an exception object.
 *
 * An instance of this class will maintain a list of errors representaed by SpkCompilerError.
 */ 
class SpkCompilerException
{
public:	
    /**
     * Determines the maximum nubmer of errors that can be held within this exception.
     * @return The maximum number of errors that can be held within this exception.
     */
    static unsigned int maxErrors() throw();

    /** The maximum number of errors that can be held within this object. */
    static const int MAX_ERRORS;
  
    /** The maximum number of digits that can be used to express MAX_ERRORS. */
    static const int MAX_ERRORS_DIGITS;
 
    /** The total fixed size (bytes) of this object. */
    static const int EXCEPTION_SIZE;

    /** Default constructor **/
    SpkCompilerException() throw();


    /** Constructor 
     * @param e An error object that will be added to the end of the error list. 
     */
    SpkCompilerException( const SpkCompilerError& e ) throw();

    /** Constructor. 
     * @param code     The error code.
     * @param message  The error message.
     * @param line     The line number at which the error was detected.
     * @param filename The file name in which the error was detected.
     */
    SpkCompilerException( enum SpkCompilerError::ErrorCode code, 
			  const char* message, 
			  unsigned int line, 
			  const char* filename ) throw();

    /**
     * Constructor
     * @param e An existing exception object to initialize this object and a new error is going to be appended.
     * @param message  The error message.
     * @param line     The line number at which the error was detected.
     * @param filename The file name in which the error was detected.
     */
    SpkCompilerException( const std::exception& e, 
			  const char* message, 
			  unsigned int line, 
			  const char* filename ) throw();

    /** Dopy constructor (deep copy) */
    SpkCompilerException( const SpkCompilerException& ) throw();

    /** Destructor */
    ~SpkCompilerException() throw();

    /** Assingment operator (deep copy) */
    const SpkCompilerException& operator=( const SpkCompilerException& ) throw();

    /** Append a new error object to the list .
     *
     * @return The reference to this object.
     * @param e A new error object.
     */
    SpkCompilerException& push( const SpkCompilerError& e ) throw();

    /** Append a new error object to the list.
     *
     * @return The reference to this object.
     * @param e       The error code.
     * @param message The error message.
     * @param line    The line number at which the error was detected.
     * @param file    The file name in which the error was detected.
     */
    SpkCompilerException& push( enum SpkCompilerError::ErrorCode e,
				const char* message, 
				unsigned int line, 
				const char* file ) throw();


    /** Remove and return the most recent error object from the list. */
    const SpkCompilerError pop() throw();

    /** Return the number of error objects accumulated so far */
    unsigned int size() const throw();

    /** returns the const reference to the specified error obejct */
    const SpkCompilerError& operator[]( int index ) const throw();

    /** Returns this class's name */
    const char * what() const throw();

    /** Test if the list of error objects is full. 
     * @return true if the list is full.
     * @return false if the list is not full.
     */
    bool full    () const throw();

    /** Test if the list of error objects is empty. 
     * @return true if the list is empty.
     * @return flase if the list is not empty.
     */
    bool empty   () const throw();

    /** Search the list for the error code.
     * @return The location of the match within the list (>=0) when it is found.
     * A negative number is returned when no entry is found.
     *
     * @param code The error code to be matched.
     */
    int  find    ( const enum SpkCompilerError::ErrorCode code ) const throw();

    /**
     * Search the list for the file name.
     * @return The location of the match within the list (>=0) when it is found.
     * A negative number is returned when no entry is found.
     */
    int  findFile( const char* filename ) const throw();

    /** Serialize in an XML format */
    const std::string getXml() const;    

    /** Extractor 
     * @param stream The stream to which contents are extracted.
     * @param e The exception object from which contents are extracted.
     */
    friend std::ostream& operator<<(std::ostream& stream, const SpkCompilerException& e);


 private:
    /** The fixed length list of error objects. */
    SpkCompilerError myError_list[16]; //[SpkCompilerException::MAX_ERRORS];

    /** The connter keeping track of how many errors are added to the list so far */
    int myCnt;


};

#endif
