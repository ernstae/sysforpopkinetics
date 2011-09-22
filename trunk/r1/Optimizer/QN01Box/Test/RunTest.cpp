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

# include <QN01Box/Residual.h>
# include <stdio.h>
# include <string>
# include <iostream>

extern bool BfgsTest(std::string &msg);
extern bool DanWood(std::string &msg);
extern bool Degenerate(std::string &msg);
extern bool Degenerate2(std::string &msg);
extern bool DeltaTest(std::string &msg);
extern bool is_symmetric_test(std::string &msg);
extern bool ExpLeastSquares(std::string &msg);
extern bool LinearObjective(std::string &msg);
extern bool MaxAbsTest(std::string &msg);
extern bool MemoryError(std::string &msg);
extern bool NextTest(std::string &msg);
extern bool Bennet5(std::string &msg);
extern bool PlusInfinity(std::string &msg);
extern bool PositiveMatrixTest(std::string &msg);
extern bool QuadBoxTest(std::string &msg);
extern bool QuasiNewton01Box(std::string &msg);
extern bool ResidualIncrease(std::string &msg);
extern bool ResidualTest(std::string &msg);
extern bool ScaleProjGradTest(std::string &msg);
extern bool SingularUpdate(std::string &msg);
extern bool SumAbsTest(std::string &msg);
extern bool Test_04_02_18(std::string &msg);
extern bool TwoCompartment(std::string &msg);
extern bool zero_one_scale_(std::string &msg);

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

	ok &= RunTest(Bennet5,           "Bennet5          ");
	ok &= RunTest(BfgsTest,          "Bfgs             ");
	ok &= RunTest(DanWood,           "DanWood          ");
	ok &= RunTest(Degenerate,        "Degenerate       ");
	ok &= RunTest(Degenerate2,       "Degenerate2      ");
	ok &= RunTest(DeltaTest,         "Delta            ");
	ok &= RunTest(is_symmetric_test, "is_symmetric     ");
	ok &= RunTest(ExpLeastSquares,   "ExpLeastSquares  ");
	ok &= RunTest(LinearObjective,   "LinearObjective  ");
	ok &= RunTest(MaxAbsTest,        "MaxAbs           ");
	ok &= RunTest(MemoryError,       "MemoryError      ");
	ok &= RunTest(NextTest,          "Next             ");
	ok &= RunTest(PlusInfinity,      "PlusInfinity     ");
	ok &= RunTest(PositiveMatrixTest,"PositiveMatrix   ");
	ok &= RunTest(QuadBoxTest,       "QuadBox          ");
	ok &= RunTest(QuasiNewton01Box,  "QuasiNewton01Box ");
	ok &= RunTest(ResidualIncrease,  "ResidualIncrease ");
	ok &= RunTest(ResidualTest,      "Residual         ");
	ok &= RunTest(ScaleProjGradTest, "ScaleProjGradTest");
	ok &= RunTest(SingularUpdate,    "SingularUpdate   ");
	ok &= RunTest(SumAbsTest,        "SumAbs           ");
	ok &= RunTest(Test_04_02_18,     "Test_04_02_18    ");
	ok &= RunTest(TwoCompartment,    "TwoCompartment   ");
	ok &= RunTest(zero_one_scale_,   "zero_one_scale_  ");

	std::cout << std::endl;
	if( ok )
	{	std::cout << "All the correctness tests passed." << std::endl;
		return 0;
	}
	std::cout << "At least one correctness tests failed." << std::endl;
	return 1;
}
