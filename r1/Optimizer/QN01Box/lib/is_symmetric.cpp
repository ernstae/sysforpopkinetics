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

$begin is_symmetric$$
$spell
$$

$index symmetric, check$$
$index is_symmetric$$

$section Check if a Matrix is Symmetric$$

$head Syntax$$
$syntax%bool is_symmetric(size_t  %n%, const double  *%Q%)%$$

$head Purpose$$
The return value is true, 
for $latex i = 0 , \ldots , n-1$$ and
$latex j = 0 , \ldots , n-1$$ 
$latex \[
| Q[ i * n + j ] -  Q[ j * n + i ] | 
\leq 
10^{10} * \max_{k,\ell} |  Q[ k * n + \ell ] 
\] $$
where the maximum is taken for
$latex k = 0 , \ldots , n-1$$ and
$latex \ell = 0 , \ldots , n-1$$.


$head Exceptions$$
The routine $code is_symmetric$$ 
is $xref/glossary/Exception Safe/exception safe/$$.


$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.

$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$codep */

# include <QN01Box/is_symmetric.h>
# include <string>

bool is_symmetric_test(std::string &msg)
{	bool ok = true;
	using namespace QN01Box;

	int     n = 3;
	double  Q[] = { 
		1., 2., 3., 
		2., 4., 5.,
		3., 5., 6.
	};

	ok &= is_symmetric(n, Q);

	Q[1 * n + 0 ] += 1e-11;
	ok &= is_symmetric(n, Q);

	Q[0 * n + 1 ] += 1e-1;
	ok &= (! is_symmetric(n, Q) );

	msg = "";
	return ok;
}
/*
$end
*/

# include <QN01Box/MaxAbs.h>
# include <cmath>

namespace QN01Box {
	bool is_symmetric(size_t  n, const double  *Q)
	{	bool symmetric = true;
		double abserr = 1e-10 * MaxAbs(n * n, Q) * n;
		double diff;
		size_t i, j;
		for(i = 0; i < n; i++)
		{	for(j = 0; j < n; j++)
			{	diff = Q[ i * n + j ] - Q[ j * n + i];
				symmetric &= std::fabs(diff) < abserr;
			}
		}
		return symmetric;
	}
} 
