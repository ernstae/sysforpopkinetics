
# include "Gsl2SpkError.h"
# include <cassert>
# include <cstring>
# include <spk/SpkException.h>
# include <iostream>

# define MaxErrorNumberLen 20

namespace {
	char * Buffer  = 0;
	char * Message = 0;
	char * File    = 0;
	int    Line    = 0;
}

extern "C" void Gsl2SpkError(
	const char * reason,
	const char * file  ,
	int line           ,
	int gsl_errno      )
{
	// can only handel one error
	assert( Message == 0 );

	// buffer for error message
	int messageLen = MaxErrorNumberLen + strlen(reason);
	assert( messageLen < SpkError::maxMessageLen() );
	Message = new char[messageLen + 1];

	// form error message
	sprintf(Message, "GSL Error %d: %s", gsl_errno, reason);
	assert( strlen(Message) <= messageLen );

	// copy the file name
	File = new char[ strlen(file) + 1 ];
	sprintf(File, "%s", file);
	assert( strlen(File) == strlen(file) );

	// copy the line number
	Line = line; 
}

void ThrowGsl2SpkError(
	const char * routine               ,
	enum SpkError::ErrorCode errorCode )
{
	// can only handel one error
	assert( Buffer == 0 );

	int Len = strlen(routine) + strlen(Message) + 4;
	Buffer = new char[Len];
	sprintf(Buffer, "\n%s:\n%s", routine, Message);
	throw SpkException(
		errorCode                    , 
		Buffer                       , 
		Line                         , 
		File 
	);
}
