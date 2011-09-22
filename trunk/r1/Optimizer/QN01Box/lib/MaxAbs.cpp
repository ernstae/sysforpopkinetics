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

$begin MaxAbs$$
$spell
	namespace
	std
	MaxAbs
	const
	cstddef
	bool
	sizeof
$$

$section Maximum Absolute Value of an Element of a Vector$$

$table
$bold Syntax$$ $cnext
$syntax%double MaxAbs(size_t %n%, const double *%x%)%$$
$tend

$fend 20$$

$head Exceptions$$
The routine $code MaxAbs$$ is $xref/glossary/Exception Safe/exception safe/$$.

$head Description$$
Returns the maximum with respect to the elements of $italic x$$
of the absolute value of each element; i.e., the
$xref/glossary/Infinity Norm/infinity norm/$$ of $italic x$$.
The argument $latex n > 0$$ is the length of the vector $italic x$$.

$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$codep */

# include <QN01Box/MaxAbs.h>
# include <cstddef>
# include <string>

bool MaxAbsTest(std::string &msg)
{	bool ok = true;
	using namespace QN01Box;

	double x[] = { -1., 3., -2., 2., -3., 1., -4., 0.};
	size_t n   = sizeof(x) / sizeof(x[0]);

	ok &= MaxAbs(n, x) == 4.;

	msg = "";

	return ok;
}	
/* $$

$end
*/

# include <cstddef>
# include <QN01Box/max.h>
# include <QN01Box/Error.h>

namespace QN01Box{
double MaxAbs(size_t n, const double *x)
{	double r = x[0];

	QN01BoxUsageError( n > 0, "MaxAbs", "vector has length zero" );

	while(n)
	{	--n;
		if( x[n] > 0 )
			r = max(r, x[n]);
		else	r = max(r, -x[n]);
	}
	return r;
}
} // End QN01Box namespace
