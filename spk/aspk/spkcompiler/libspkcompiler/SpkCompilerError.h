#ifndef SPKCOMPILERERROR_H
#define SPKCOMPILERERROR_H

#include <map>
#include <exception>
#include <string>
/*
namespace SpkCompilerError_const{
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
*/
class SpkCompilerError
{

public:

  static const unsigned int ERRORCODE_FIELD_LEN;
  static const unsigned int LINENUM_FIELD_LEN;
  static const unsigned int FILENAME_FIELD_LEN;
  static const unsigned int MESSAGE_FIELD_LEN;

  static const unsigned int ERROR_SIZE;

  static const char ERRORCODE_FIELD_NAME[];
  static const char ERRORCODE_DESCRIPTION_FIELD_NAME[];
  static const char FILENAME_FIELD_NAME[];
  static const char LINENUM_FIELD_NAME[];
  static const char MESSAGE_FIELD_NAME[];

  //
  // Each error code is preceded by SPK_ in order to avoid name conflict.
  // Somewhere in the vender supplied libraries predefine OVERFLOW.
  // For consistency, all error codes here got SPK_ prefix.
  //
  enum ErrorCode {
      // C++ Standard library errors
      SPK_COMPILER_STD_ERR, 

      // User input error
      SPK_COMPILER_USER_INPUT_ERR,

      // Insufficient memory error
      SPK_COMPILER_INSUFFICIENT_MEM_ERR,

      // SPK Compiler Driver error
      SPK_COMPILER_DRIVER_ERR,

      // XML DOM error
      SPK_COMPILER_XMLDOM_ERR,

      // SpkSourceML->C++ compilation error
      SPK_COMPILER_SOURCEML_ERR,

      // SpkDataML->C++ compilation error
      SPK_COMPILER_DATAML_ERR,

      // Fortran->C++ compilation error
      SPK_COMPILER_FORTRAN_ERR,

      // Unknown
      SPK_COMPILER_UNKNOWN_ERR
  };
  typedef std::map< enum ErrorCode, const char* > ErrorMap;

private:
  // The associated array [ErrorCode - Default Message]
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
