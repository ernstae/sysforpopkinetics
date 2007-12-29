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
// BEGIN PROGRAM


# include <QN01Box/Error.h>
# include <QN01Box/Memory.h>
# include <string>

namespace {

	size_t handlerCount = 0;
	void handler(
		int      line       ,
		char    *file       ,
		char *routine       , 
		char *message       )
	{	handlerCount++;	
	}

	void MemoryRight(void)
	{	// proper use of Memory
		size_t n1   = 10;
		size_t n2   = 20;
		size_t ntot = n1 + n2;
		QN01Box::Memory<double> buffer(ntot);
		double *x1 = buffer(n1);
		double *x2 = buffer(n2);

		x1[0]    = 1.;
		x1[n1-1] = 2.;
		x2[0]    = 1.;
		x2[n2-1] = 2.;
	}
	void MemoryWorng(void)
	{	// improper use of Memory
		size_t n1   = 10;
		size_t n2   = 20;
		size_t ntot = n1 + n2;
		QN01Box::Memory<double> buffer(ntot);
		double *x1 = buffer(n1);

		*(x1-1)   = 1.; // error 1, write before allocated memory
		x1[n1+n2] = 2.; // error 2, write after allocated memory
		// error 3, never ask for the last n2 elements from buffer
	}
}

bool MemoryError(std::string&Msg)
{	bool ok = true;

	MemoryRight();

	// switch from default reporting to just counting errors
	QN01Box::LinkErrorHandler(handler);

	// make the mistakes
	MemoryWorng();

	// switch back to default error reporting 
	QN01Box::LinkErrorHandler(QN01Box::defaultErrorHandler);

	// check that 3 errors occurred during call to MemoryWorng
	ok &= handlerCount == 3;

	Msg = "";
	return ok;
}
