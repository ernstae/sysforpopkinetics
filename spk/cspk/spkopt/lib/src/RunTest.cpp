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

# include "Residual.h"
# include <stdio.h>
# include <string>
# include <iostream>

extern bool ResidualTest(std::string &msg);
extern bool DeltaTest(std::string &msg);
extern bool NextTest(std::string &msg);
extern bool MaxAbsTest(std::string &msg);
extern bool QuadBoxTest(std::string &msg);
extern bool QuadFixedTest(std::string &msg);
extern bool BfgsTest(std::string &msg);
extern bool QuasiNewton01Box(std::string &msg);
extern bool Degenerate(std::string &msg);
extern bool Test_03_12_16(std::string &msg);
extern bool ExpLeastSquares(std::string &msg);
extern bool Test_04_01_21(std::string &msg);
extern bool Test_04_02_18(std::string &msg);
extern bool LinearObjective(std::string &msg);
extern bool SingularUpdate(std::string &msg);
extern bool ResidualIncrease(std::string &msg);
extern bool PlusInfinity(std::string &msg);

static bool RunTest(bool TestOk(std::string &msg), char *name)
{	bool ok;
	std::string msg;

	ok = TestOk(msg);
	if( ok )
		std::cout << "Ok:    ";
	else	std::cout << "Error: ";
	std::cout << name <<  msg << std::endl;

	return ok;
}
int main(void)
{	bool ok = true;

	ok &= RunTest(ResidualTest,      "Residual         ");
	ok &= RunTest(DeltaTest,         "Delta            ");
	ok &= RunTest(NextTest,          "Next             ");
	ok &= RunTest(MaxAbsTest,        "MaxAbs           ");
	ok &= RunTest(QuadBoxTest,       "QuadBox          ");
	ok &= RunTest(QuadFixedTest,     "QuadFixed        ");
	ok &= RunTest(BfgsTest,          "Bfgs             ");
	ok &= RunTest(QuasiNewton01Box,  "QuasiNewton01Box ");
	ok &= RunTest(Degenerate,        "Degenerate       ");
	ok &= RunTest(Test_03_12_16,     "Test_03_12_16    ");
	ok &= RunTest(ExpLeastSquares,   "ExpLeastSquares  ");
	ok &= RunTest(Test_04_01_21,     "Test_04_01_21    ");
	ok &= RunTest(Test_04_02_18,     "Test_04_02_18    ");
	ok &= RunTest(LinearObjective,   "LinearObjective  ");
	ok &= RunTest(SingularUpdate,    "SingularUpdate   ");
	ok &= RunTest(ResidualIncrease,  "ResidualIncrease ");
	ok &= RunTest(PlusInfinity,      "PlusInfinity     ");

	if( ok )
		return 0;
	return 1;
}

