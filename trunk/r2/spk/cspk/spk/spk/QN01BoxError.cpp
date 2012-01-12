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

# include <QN01Box/Error.h>
# include <cstring>
# include "SpkException.h"
# include <iostream>
# include <stdio.h>

void QN01BoxErrorHandler(
	int       line ,
	char     *file ,
	char  *routine ,
	char  *message )
{
	char *buffer;
	if( strlen(message) > 0 )
	{	int Len = strlen(routine) + strlen(message) + 4;
		buffer = new char[Len];
		snprintf(buffer, Len, "\n%s: %s", routine, message);
	}
	else	buffer = message;

	throw SpkException(
		SpkError::SPK_UNKNOWN_OPT_ERR,
		buffer                       , 
		line                         , 
		file 
	);
}

void QN01BoxErrorSetup(void)
{	QN01Box::LinkErrorHandler(QN01BoxErrorHandler); 
}
