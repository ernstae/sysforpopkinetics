# ifndef CppADUtilIncluded
# define CppADUtilIncluded
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
Version: 03-12-12

$begin CppADUtil$$
$dollar @$$
$spell
	xref
	Cpp
	iostream
$$

$section CppAD Utilities that are Used by QuasiNewton01Box$$

The following files included below copied directly from version 03-12-11
of the CppAD distribution with the following changes:
$codep
	@xref     -> @code
	@children -> @code
$$
$contents%
	ErrMacro.h%
	ADDefine.h%
	LuSolve.h%
	vector.h
%$$

$end
*/

# include <iostream>
# include "ADDefine.h"
# include "ErrMacro.h"
# include "vector.h"
# include "LuSolve.h"

# endif
