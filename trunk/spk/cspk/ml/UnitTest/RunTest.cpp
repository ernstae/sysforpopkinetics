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

# include <stdio.h>
# include <string>
# include <iostream>

extern bool MapBayTest(void);
extern bool MapMonteTest(void);
extern bool MontePopObjTest(void);
extern bool GridIntegralTest(void);

static bool RunTest(bool TestOk(void), char *name)
{	bool ok;

	ok = TestOk();
	if( ok )
		std::cout << "Ok:    ";
	else	std::cout << "Error: ";
	std::cout << name << std::endl;

	return ok;
}
int main(void)
{	bool ok = true;

	ok &= RunTest(MapBayTest     ,  "MapBay");
	ok &= RunTest(MapMonteTest   ,  "MapMonte");
	ok &= RunTest(MontePopObjTest,  "MontePopObj");
	ok &= RunTest(GridIntegralTest, "GridIntegralTest");

	if( ok )
		return 0;
	return 1;
}

