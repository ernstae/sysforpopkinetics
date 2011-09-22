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

SPK is Copyright (C) 1998-2003, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution.
-----------------------------------------------------------------------
Author: Brad Bell (brad@apl.washington.edu)

*/
// BEGIN PROGRAM

# include "QuadBox.h"
# include <string>
# include <sstream>

bool Test_04_02_18(std::string &Msg)
{	bool ok = true;

	size_t kMax    = 100;
	int    level   =   0;
	size_t n       =   1;
	double epsilon = 1e-16;
	double Q[]     = { 5e-12 };
	double r[]     = { -5e-14 };
	double l[]     = { -.5 };
	double u[]     = { .4 };
	//
	size_t k       = 0;
	double x[]     = { -.05 };
	double a[]     = { 2e-14 };
	double b[]     = { 2e-14 };

	double eps = epsilon;
	Msg = QuadBox(
		std::cout, 
		kMax, 
		level, 
		n, 
		Q, 
		r, 
		l, 
		u, 
		k, 
		eps, 
		x, 
		a, 
		b 
	);
	if( epsilon != eps )
		Msg = " :QuadBox could not obtain desired accuracy";
	if( Msg == "ok" )
	{	std::ostringstream buf;
		buf << " :QuadBox Iteration Count = " << k;
		Msg = buf.str();
	}
	else	ok = false;

	double Qxi;
	size_t i;
	size_t j;
	for(i = 0; i < n; i++)
	{	Qxi = 0;
		for(j = 0; j < n; j++)
			Qxi += Q[i * n + j] * x[j];

		ok &= l[i] < x[i];
		ok &= x[i] < u[i];
		ok &= 0. < a[i]; 
		ok &= 0. < b[i]; 

		ok &= fabs( Qxi + r[i] - a[i] + b[i] ) <= epsilon;
		ok &= a[i] * (x[i] - l[i]) <= epsilon;
		ok &= b[i] * (u[i] - x[i]) <= epsilon;

	}

	return ok;
}


// END PROGRAM
