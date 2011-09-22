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
Software:   Brad Bell (brad@apl.washington.edu)
Mathematics: Brad Bell & Jim Burke (burke@math.washington.edu)

*/
# include "Bfgs.h"
# include "Memory.h"

# include <cstddef>
# include <iostream>
# include <cassert>
# include <string>
# include <math.h>

bool SingularUpdate(std::string &Msg)
{	bool ok = true;

	size_t n = 3;
	double epsilon = 1e-09;
	double xk[] ={
		0., 
		0.,
		0.,
	};
	double gk[] = {
		-1e8, 
		-1e8, 
		-1e8
	};
	double xp[] = {
		1e-4,
		1e-4, 
		1e-4
	};
	double gp[] = {
		0.,
		0., 
		0.
	};
	double H[] = {
		1., 0., 0.,
		0., 1., 0., 
		0., 0., 1.
	};
	Msg = Bfgs(n, epsilon, xk, gk, xp, gp, H);
	if( Msg == "ok" )
		Msg = "";
	else	ok = false;
	//
	// check update equation
	size_t i;
	for(i = 0; i < n; i++)
	{	double Hs = 0;
		size_t j;
		for(j = 0; j < n; j++)
			Hs += H[i * n + j] * (xp[j] - xk[j]);

		ok &= fabs( Hs - gp[i] + gk[i]  ) <= 1e-5;
	}

	return ok;
}
