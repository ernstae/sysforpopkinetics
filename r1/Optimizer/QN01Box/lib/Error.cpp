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

# include <cassert>
# include <iostream>
# include <climits>
# include <cstddef>
# include <cstdlib>
# include <cstring>
/*
$begin Error$$
$spell
	namespace
	cpp
	ifndef
	ifdef
	std
	cerr
	endl
	endif
$$

$section QN01Box Error Messages and Traps$$
$index error, handler$$
$index handler, error$$
$index trap, error$$
$index assert$$
$index exception$$

$table
$bold Syntax$$ $cnext 
$syntax%# include <QN01Box/Error.h>%$$
$rnext $cnext
$syntax%LinkErrorHandler(%handler%)%$$
$rnext $cnext
$syntax%%handler%(%line%, %file%, %routine%, %message%)%$$
$tend
$fend 25$$

$head Description$$
The default $code QN01Box$$ error handler prints messages
to standard error and generates an assert (as an aid to using the debugger).
If you do not want this action you will have to define your own 
error handler and link it to the $code QN01Box$$ package.

$head LinkErrorHandler$$
The prototype for the argument $italic handler$$ is
$syntax%
	void %handler%(int line, char *file, char *routine, char *message)
%$$ 
If you define a handler,
you can call $code LinkErrorHandler$$ to link it to the QN01Box package.
You can restore the default error handler using the syntax
$codep 
	QN01Box::LinkErrorHandler(QN01Box::defaultErrorHandler);
$$
Note that if you are using $code QN01Box$$ namespace,
you need not have it present in the command above.

$head line$$
The argument $italic line$$ to $italic handler$$ specifies the source
code line number where the error was detected.

$head file$$
The argument $italic file$$ to $italic handler$$ specifies the source
code file where the error was detected.
It is a $code '\0'$$ terminated character string.

$head routine$$
The argument $italic routine$$ to $italic handler$$ specifies the routine,
in the $code QN01Box$$ namespace, where the error was detected.

$head message$$
The argument $italic message$$ to $italic handler$$ is a message
describing the cause of the error (as a misuse of QN01Box).
It is a $code '\0'$$ terminated character string.
If the message is empty, $syntax%%message%[0] == '\0'%$$,
the cause of the error is unknown.

$head Example$$
The file $code MemoryError.cpp$$ 
(source code below) is an example of how to do this replacement:
$code
$verbatim%Test/MemoryError.cpp%$$
$$

$end
*/

# include <QN01Box/Error.h>
# include <iostream>
# include <cassert>

namespace QN01Box {

	void defaultErrorHandler(
		int line      , 
		char *file    ,
		char *routine ,
		char *message )
	{
		std::cerr << "QN01Box::" << routine << ": ";
		if( strcmp(message, "" ) != 0 )
		{	std::cerr 
			<< "usage error detected at line "
			<< __LINE__ 
			<< " of file " 
			<< __FILE__
			<< std::endl 
			<< message 
			<< std::endl;
		}
		else
		{	std::cerr 
			<< "unknown error detected at line "
			<< __LINE__ 
			<< " of file " 
			<< __FILE__
			<< std::endl;
		}
		assert(0);
	}

	ErrorHandler LinkErrorHandler(ErrorHandler handler_ )
	{	static ErrorHandler handler = defaultErrorHandler;

		if( handler_ != 0 )
			handler = handler_;	

		return handler;
	}

}
