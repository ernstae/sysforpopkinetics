/**
 * @file SpkCompilerError.h
 * Declare SpkCompilerError class
 */
#ifndef SPKCOMPILERERROR_H
#define SPKCOMPILERERROR_H

#include <map>
#include <exception>
#include <string>

/**
 * 
 */
class SpkCompilerError
{

 public:
  
  /**
   * The length of the field that contains an error code.
   * Error codes are of tppe of ErrorCode enumulation.
   * The size of an enumulation cannot be larger than
   * sizeof( int ).  The largest number expressed in the
   * integral type is obtained by an ANSII macro INT_MAX 
   * defined in <filename>limits.h</filename>.
   */
  static const unsigned int ERRORCODE_FIELD_LEN;
  static const unsigned int ERRORCODE_DESCRIPTION_FIELD_LEN;
  static const unsigned int LINENUM_FIELD_LEN;
  static const unsigned int FILENAME_FIELD_LEN;
  static const unsigned int MESSAGE_FIELD_LEN;

  static const unsigned int ERROR_SIZE;

  static const char ERRORCODE_FIELD_NAME[];
  static const char ERRORCODE_DESCRIPTION_FIELD_NAME[];
  static const char FILENAME_FIELD_NAME[];
  static const char LINENUM_FIELD_NAME[];
  static const char MESSAGE_FIELD_NAME[];

  enum ErrorCode {
    /** C++ Standard library errors */
    ASPK_STD_ERR,

    /** XML DOM parser error */
    ASPK_XMLDOM_ERR,

    /** SpkSourceML->C++ compilation error */
    ASPK_SOURCEML_ERR,

    /** SpkDataML->C++ compilation error */
    ASPK_DATAML_ERR,

    /** Fortran->C++ compilation error */
    ASPK_FTOC_ERR,

    /** ASPK Compiler implemntator's programming error */
    ASPK_PROGRAMMER_ERR,

    /** Unknown */
    ASPK_UNKNOWN_ERR
  };
  typedef std::map< enum ErrorCode, const char* > ErrorMap;

 private:
  // Map: ErrorCode vs. Default Message
  static const ErrorMap mapping;

  // filling the default error code vs message map
  static const ErrorMap fillErrorMap();

  // integer indicating the nature of this error
  enum ErrorCode myErrorCode;

  // line number at which the error was detected
  unsigned int myLineNum;

  // filename in which the error was detected
  char myFileName[128+1]; //[SpkCompilerError::FILENAME_FIELD_LEN+1];

  // error message added by client
  char myMessage[256+1]; //[MESSAGE_FIELD_LEN+1];

  // returns the maximum value allowed for error code
  unsigned int maxErrorcode() throw();

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
  SpkCompilerError() throw();

  // copy constructor which performs deep copy
  SpkCompilerError( const SpkCompilerError& ) throw();

  // constructor that takes a set of concrete information for an error
  SpkCompilerError( enum ErrorCode, const char* message, unsigned int, const char* filename) throw();


  // constructor that takes std::exception as error object
  SpkCompilerError( const std::exception& e, const char* message, unsigned int, const char* filename) throw();

  // destructor which does nothing
  ~SpkCompilerError() throw();

  // assignment operator which performs deep copy
  const SpkCompilerError& operator=(const SpkCompilerError&) throw();
	
  // method that returns the error code
  enum ErrorCode code() const throw();    

  // returns the line number
  unsigned int linenum() const throw();

  // method that returns the filename
  const char* filename() const throw();

  // returns the message
  const char* message() const throw();

  // serialize
  friend std::string& operator<<(std::string& s, const SpkCompilerError& e);
  friend std::ostream& operator<<(std::ostream& stream, const SpkCompilerError& e);

  // unserialize
  friend std::string& operator>>(std::string& s, SpkCompilerError& e);
  friend std::istream& operator>>(std::istream& stream, SpkCompilerError& e);
};

#endif
