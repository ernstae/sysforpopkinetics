
# include "Gsl2SpkError.h"
# include <cassert>
# include <cstring>
# include <spk/SpkException.h>
# include <iostream>

# define MaxErrorNumberLen 20

namespace {
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

	// write message to cerr
	std::cerr << Message << std::endl;
}

void ThrowGsl2SpkError()
{
	throw SpkException(
		SpkError::SPK_UNKNOWN_ERR    , 
		Message                      , 
		Line                         , 
		File 
	);
}
