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
#ifndef SPKERROR_H
#define SPKERROR_H
//
// These pragma are to disable "too long literals" warnings generated
// as a result of using STL components.
//
#pragma warning( disable : 4786 )  
#include <map>
#include <exception>
#include <string>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

class SpkError
{

public:
  static const unsigned int ERRORCODE_FIELD_LEN;
  static const unsigned int ERRORCODE_DESCRIPTION_FIELD_LEN;
  static const unsigned int LINENUM_FIELD_LEN;
  static const unsigned int FILENAME_FIELD_LEN;
  static const unsigned int MESSAGE_FIELD_LEN;
  static const unsigned int DESCRIPTION_FIELD_LEN;
  static const unsigned int ERROR_SIZE;

  static const char ERRORCODE_FIELD_NAME[];
  static const char ERRORCODE_DESCRIPTION_FIELD_NAME[];
  static const char FILENAME_FIELD_NAME[];
  static const char LINENUM_FIELD_NAME[];
  static const char MESSAGE_FIELD_NAME[];

  xercesc::XercesDOMParser * parser;
  //
  // Each error code is preceded by SPK_ in order to avoid name conflict.
  // Somewhere in the vender supplied libraries predefine OVERFLOW.
  // For consistency, all error codes here got SPK_ prefix.
  //
  enum ErrorCode {
      // C++ Standard library errors
      SPK_STD_ERR, 

      // Optimizer errors
      SPK_TOO_MANY_ITER, 
      SPK_NOT_CONVERGED,
      SPK_KT_CONDITIONS,
      SPK_LIN_NOT_FEASIBLE,
      SPK_NONLIN_NOT_FEASIBLE,
      SPK_OPT_ERR,
      SPK_OPT_WARNING,
      SPK_UNKNOWN_OPT_ERR,
      SPK_NOT_READY_WARM_START_ERR,

      // Errors occured during differentiation
      SPK_DIFF_ERR,

      // Matrix property errors
      SPK_NOT_INVERTABLE_ERR,
      SPK_NOT_POS_DEF_ERR,
      SPK_NOT_SYMMETRIC_ERR,

      // Ordinary Differential Equation (ODE) errors
      SPK_ODE_SOLN_ERR,

      // Errors occured during evaluation of User-provided models
      SPK_MODEL_NOT_IMPLEMENTED_ERR,

      SPK_MODEL_SET_IND_ERR,
      SPK_MODEL_SET_POP_ERR,
      SPK_MODEL_SET_INDEX_ERR,

      SPK_MODEL_DATA_MEAN_ERR,
      SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
      SPK_MODEL_DATA_MEAN_POP_ERR,
      SPK_MODEL_DATA_MEAN_IND_ERR,

      SPK_MODEL_DATA_VARIANCE_ERR,
      SPK_MODEL_DATA_VARIANCE_POP_ERR,
      SPK_MODEL_DATA_VARIANCE_IND_ERR,

      SPK_MODEL_INV_DATA_VARIANCE_ERR,
      SPK_MODEL_INV_DATA_VARIANCE_POP_ERR,
      SPK_MODEL_INV_DATA_VARIANCE_IND_ERR,

      SPK_MODEL_IND_VARIANCE_ERR,
      SPK_MODEL_IND_VARIANCE_POP_ERR,

      SPK_MODEL_INV_IND_VARIANCE_ERR,
      SPK_MODEL_INV_IND_VARIANCE_POP_ERR,


      // Floating point arithmatics errors
      SPK_FP_UNDERFLOW_ERR,
      SPK_FP_OVERFLOW_ERR,
      SPK_FP_DENORMAL_ERR,
      SPK_FP_INEXACT_ERR,
      SPK_FP_INVALID_ERR,
      SPK_FP_ZERODIVIDE_ERR,

      // Invalid User input errors
      SPK_USER_INPUT_ERR,

      // Programmer errors
      SPK_PROGRAMMER_ERR,

      // Parallel communication error
      SPK_PARALLEL_ERR,

      // The end-of-SPK signal; this is not really an error actually.
      SPK_PARALLEL_END_SIGNAL,

      // Insufficient memory error
      SPK_INSUFFICIENT_MEM_ERR,

      // Errors during parameter estimation
      SPK_IND_PAR_AT_BOUNDS_ERR,
      SPK_IND_OBJ_HESS_ERR,

      // Errors during statistics computation
      SPK_STATISTICS_ERR,

      // Errors during data simulation
      SPK_SIMULATION_ERR,
 
      // XML DOM related errors
      SPK_XMLDOM_ERR,

      // Error during non-parameteric calculation
      SPK_NON_PAR_ERR,

      // Unknown
      SPK_UNKNOWN_ERR
  };
  typedef std::map< enum ErrorCode, const char* > ErrorMap;

private:
  // The associated array [ErrorCode - Default Message]
  static const ErrorMap mapping;

  // filling the default error code vs message map
  static const ErrorMap fillErrorMap();

  // integer indicating the nature of this error
  enum ErrorCode _errorcode;

  // line number at which the error was detected
  unsigned int _linenum;

  // filename in which the error was detected
  char _filename[128+1]; //[FILENAME_FIELD_LEN+1];

  // error message added by client
  char _message[256+1]; //[MESSAGE_FIELD_LEN+1];

  void initXmlParser();

public:

  // Displays a default error message corresponding to the error code
  static const char* describe( enum ErrorCode key );

  // method that returns the max value for a linenum
  static unsigned int maxLinenum() throw();

  // method that returns the max number of characters for a filename
  static unsigned int maxFilenameLen() throw();

  // method that returns the max number of characters in a message
  static unsigned int maxMessageLen() throw();

  // returns the maximum value allowed for an error code
  static unsigned int maxErrorcode() throw();

  // returns the maximum length of characters allowed for a description
  static unsigned int maxDescriptionLen() throw();

  // default constructor
  SpkError() throw();

  // copy constructor which performs deep copy
  SpkError( const SpkError& ) throw();

  // constructor that takes a set of concrete information for an error
  SpkError( enum ErrorCode, const char* message, unsigned int, const char* filename) throw();


  // constructor that takes std::exception as error object
  SpkError( const std::exception& e, const char* message, unsigned int, const char* filename) throw();

  // destructor which does nothing
  ~SpkError() throw();

  // assignment operator which performs deep copy
  const SpkError& operator=(const SpkError&) throw();

  // method that returns the error code
  enum ErrorCode code() const throw();    

  // returns the line number
  unsigned int linenum() const throw();

  // method that returns the filename
  const char* filename() const throw();

  // returns the message
  const char* message() const throw();

  friend std::string&  operator<<( std::string& s, const SpkError& e );
  friend std::ostream& operator<<( std::ostream& stream, const SpkError& e);

  // unserialize
  friend const std::string& operator>>( const std::string& s, SpkError& e );
  friend std::istream& operator>>( std::istream& stream, SpkError& e );
  friend const xercesc::DOMElement* operator>>( const xercesc::DOMElement* error, SpkError& e );

  // formats an error message the same way as the serialize function 
  friend void formatLongError(
      enum ErrorCode  ecode,
      const std::string&        mess,
      unsigned int              line,
      const char*               file,
      std::string&              formattedError );
};

#endif
