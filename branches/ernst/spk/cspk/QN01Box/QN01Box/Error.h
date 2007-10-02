# ifndef QN01Box_Error
# define QN01Box_Error

/*
-----------------------------------------------------------------------
From:   Resource Facility for Population Kinetics
          Department of Bioengineering Box 352255
          University of Washington
          Seattle, WA 98195-2255

This file is part of the System for Population Kinetics (SPK), which
was developed with support from NIH grants RR-12609 and P41-
EB001975. Please cite these grants in any publication for which this
software is used and send a notification to the address given above.

SPK is Copyright (C) 1998-2005, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution.
-----------------------------------------------------------------------
Author: Brad Bell (brad@apl.washington.edu)
*/

namespace QN01Box {
	typedef void (*ErrorHandler) 
		(int line, char *file, char *routine, char *message);

	extern ErrorHandler LinkErrorHandler(ErrorHandler handler = 0);
	extern void defaultErrorHandler
		(int line, char *file, char *routine, char *message);
}

# ifdef NDEBUG
# define QN01BoxUnknownError(expression, routine)
# else
# define QN01BoxUnknownError(expression, routine)           \
if( ! (expression) )                                        \
{	QN01Box::ErrorHandler handler =	LinkErrorHandler(); \
	handler(__LINE__, __FILE__, routine, "");           \
}
# endif // ifdef NDEBUG

# define QN01BoxUsageError(expression, routine, message)    \
if( ! (expression) )                                        \
{	QN01Box::ErrorHandler handler =	LinkErrorHandler(); \
	handler(__LINE__, __FILE__, routine, message);      \
}


# endif 
