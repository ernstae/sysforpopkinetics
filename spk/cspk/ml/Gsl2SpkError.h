# ifndef Gsl2SpkErrorIncluded
# define Gsl2SpkErrorIncluded

# include <spk/SpkException.h>

extern "C" void Gsl2SpkError(
	const char * reason,
	const char * file  ,
	int line           ,
	int gsl_errno      );

extern void ThrowGsl2SpkError(void);

# endif
