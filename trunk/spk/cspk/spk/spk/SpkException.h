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
 * File: SpkException.h
 *
 *
 * Declares SpkException class, the encapsulation of error objects
 * and controls.
 * This header file also includes other headers for importing 
 * declarations of components related to SpkException objects.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef SPKEXCEPTION_H
#define SPKEXCEPTION_H

#pragma warning ( disable : 4786 )

//
// Necessary component headers for SpkException class declaration.
//
#include "SpkError.h"
#include <string>
#include <xercesc/parsers/XercesDOMParser.hpp>

//
// SpkException class declaration
//
class SpkException
{
public:	
    static unsigned int maxErrors() throw();

    static const int MAX_ERRORS;
    static const int MAX_ERRORS_DIGITS;
    static const int EXCEPTION_SIZE;

    // initializes xerces DOM parser
    void initXmlParser();

    // default constructor
    SpkException() throw();


    // the constructor that takes an error object to be added to the head of the list
    SpkException( const SpkError& ) throw();
    SpkException( enum SpkError::ErrorCode, 
		  const char* message, 
		  unsigned int line, 
		  const char* filename) throw();
    SpkException( const std::exception&, 
		  const char* message,
		  unsigned int line, 
		  const char* filename) throw();

    // copy constructor which performs deep copy
    SpkException( const SpkException& ) throw();

    // destructor which does nothing
    ~SpkException() throw();

    // assingment operator which performs deep copy
    const SpkException& operator=( const SpkException& ) throw();

    // append an error object to the list
    SpkException& push( const SpkError& ) throw();
    SpkException& push( enum SpkError::ErrorCode, 
			const char* message, 
			unsigned int, 
			const char* filename ) throw();
    SpkException& cat( const SpkException& e ) throw();


    // remove and return the most recent Error object
    const SpkError pop() throw();

    // return the number of error objects accumulated so far
    unsigned int size() const throw();

    // returns the const reference to the specified error obejct
    const SpkError& operator[](int index) const throw();

    // returns this class name
    const char * what() const throw();

    bool full() const throw();
    bool empty() const throw();
    int  find( const enum SpkError::ErrorCode code ) const throw();
    int  findFile( const char* filename ) const throw();

    // serialize
    friend std::ostream& operator<<(std::ostream& stream, const SpkException& e);
    friend std::string& operator<<(std::string& s, const SpkException& e);

    // unserialize 
    friend std::istream& operator>>(std::istream& stream, SpkException& e);
    friend const std::string& operator>>(const std::string& s, SpkException& e);

 private:
    // a fixed length list of Error objects
    SpkError _error_list[32]; // MAX_ERRORS=32

    // counting the number of errors added to the list so far
    int _cnt;

    xercesc::XercesDOMParser *parser;
};

//
// Headers that are not absolutely necessary for SpkException class declaration but
// for related components.
// 

#include "FpErrorChecker.h"

#endif
