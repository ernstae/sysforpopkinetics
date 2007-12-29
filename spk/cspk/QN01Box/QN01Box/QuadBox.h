# ifndef QN01Box_QuadBoxIncluded
# define QN01Box_QuadBoxIncluded
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

# include <cstddef>
# include <iostream>

namespace QN01Box {
	extern const char * QuadBox(
		// Input Arguments
		std::ostream    &os,
		size_t         kMax,
		size_t        level,
		size_t            n,
		double          eIn,
		const double     *Q, // length n * n
		const double     *r, // length n 
		const double     *l, // ...
		const double     *u, // ...
		// Input and Output Arguments
		size_t           &k,
		double           *x, // length n
		double           *a, // ...
		double           *b, // ...
		// Output Arguments
		double        &eOut
	);
}

# endif
