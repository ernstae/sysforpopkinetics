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

namespace SpkError_const{
  const unsigned int ERRORCODE_FIELD_LEN = 4;
  const unsigned int LINENUM_FIELD_LEN   = 6;
  const unsigned int FILENAME_FIELD_LEN  = 128;
  const unsigned int MESSAGE_FIELD_LEN   = 256;

  const unsigned int ERROR_SIZE        
      = FILENAME_FIELD_LEN + 1
      + MESSAGE_FIELD_LEN + 1
      + ERRORCODE_FIELD_LEN + 1
      + LINENUM_FIELD_LEN + 1
      //+ strlen("errorcode\n") + strlen("linenum\n") + strlen("filename\n") + strlen("message\n");
      + 10 + 8 + 9 + 8;

  const char ERRORCODE_FIELD_NAME[] = "errorcode";
  const char ERRORCODE_DESCRIPTION_FIELD_NAME[] = "description";
  const char FILENAME_FIELD_NAME[]  = "filename";
  const char LINENUM_FIELD_NAME[]   = "linenum";
  const char MESSAGE_FIELD_NAME[]   = "message";
}
class SpkError
{

public:

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

      // Errors occured during evaluation of User-provided models
      SPK_MODEL_NOT_IMPLEMENTED_ERR,

      SPK_MODEL_SET_IND_ERR,
      SPK_MODEL_SET_POP_ERR,
      SPK_MODEL_SET_INDEX_ERR,

      SPK_MODEL_DATA_MEAN_ERR,
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

      // Parallel communication error
      SPK_PARALLEL_ERR,

      // The end-of-SPK signal; this is not really an error actually.
      SPK_PARALLEL_END_SIGNAL,

      // Insufficient memory error
      SPK_INSUFFICIENT_MEM_ERR,

      // Unknown
      SPK_UNKNOWN_ERR,

      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // [Revisit - SpdError Class Should be In Its Own Files - Mitch]
      // The classes SpkException and SpkError should be extended so that
      // users of SPK can create their own error codes that will still work
      // with the SpkException class.  The problem right now is that error
      // codes (SpkError::ErrorCode) are enum's which can means they can't be
      // derived from like they could if they were an error code base class.
      // For now, each error code is preceded by SPD_ in order to avoid name
      // conflict.
      //
      // Errors occured during evaluation of User-provided models
      SPD_MODEL_NOT_IMPLEMENTED_ERR,
      SPD_MODEL_SET_DES_ERR,
      SPD_MODEL_DATA_MEAN_DES_ERR,
      SPD_MODEL_DATA_MEAN_IND_DES_ERR,
      SPD_MODEL_DATA_MEAN_IND_POP_ERR,
      SPD_MODEL_DATA_VARIANCE_DES_ERR,
      SPD_MODEL_INV_DATA_VARIANCE_DES_ERR,
      SPD_MODEL_IND_VARIANCE_DES_ERR,
      SPD_MODEL_POP_PRIOR_ERR,
      SPD_MODEL_POP_PRIOR_POP_ERR
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
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
  char _filename[SpkError_const::FILENAME_FIELD_LEN+1];

  // error message added by client
  char _message[SpkError_const::MESSAGE_FIELD_LEN+1];

  // returns the maximum value allowed for error code
  unsigned int SpkError::maxErrorcode() throw();



public:

  // Displays a default error message corresponding to the error code
  static const char* describe( enum ErrorCode key );

  // method that returns the max value for a linenum
  static unsigned int maxLinenum() throw();

  // method that returns the max number of characters for a filename
  static unsigned int maxFilenameLen() throw();

  // method that returns the max number of characters in a message
  static unsigned int maxMessageLen() throw();

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

  // serialize
  friend std::string& operator<<(std::string& s, const SpkError& e);
  friend std::ostream& operator<<(std::ostream& stream, const SpkError& e);

  // unserialize
  friend std::string& operator>>(std::string& s, SpkError& e);
  friend std::istream& operator>>(std::istream& stream, SpkError& e);
};

#endif
