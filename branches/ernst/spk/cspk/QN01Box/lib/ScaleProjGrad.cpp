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

$begin ScaleProjGrad$$
$spell
	Proj
	const
	cstddef
	bool
	std
	namespace
$$

$section Compute the Scaled Projected Gradient$$
$index scale, project$$
$index scale, gradient$$
$index project, scale$$
$index project, gradient$$
$index gradient, scale$$
$index gradient, project$$

$table
$bold Syntax$$ $cnext
$syntax%void ScaleProjGrad(
	double       *%p%,
	size_t        %n%, 
	const double *%x%, 
	const double *%g%, 
	const double *%l%,
	const double *%u%)
%$$
$tend

$fend 20$$

$head Exceptions$$
The routine $code ScaleProjGrad$$ 
is $xref/glossary/Exception Safe/exception safe/$$.

$head Description$$
Computes a 
$xref/glossary/p: Scaled Projected Gradient/scaled projected gradient/$$.

$head n$$
specifies the dimension of the domain space for the objective function
in the definition of the scaled projected gradient. 

$head x$$
the vector $italic x$$ has length $italic n$$ and specifies
the argument value at which we are computing the scaled projected gradient.

$head g$$
the vector $italic g$$ has length $italic n$$ and contains
the gradient of the objective function a the point $italic x$$.

$head l$$
This argument is optional. If it is not
present, the value zero is used for the lower limits.
If it is present,
the vector $italic l$$ has length $italic n$$ and specifies
the lower limits for the argument in the definition of the scaled
projected gradient. 


$head u$$
This argument is optional. If it is not
present, the value one is used for the upper limits.
If it is present,
the vector $italic u$$ has length $italic n$$ and specifies
the upper limits for the argument in the definition of the scaled
projected gradient.

$head Example$$
The following example is also a test.
It returns true if it succeeds and false otherwise.
$codep */

# include <QN01Box/ScaleProjGrad.h>
# include <cstddef>
# include <string>

bool ScaleProjGradTest(std::string &msg)
{	bool ok = true;
	using namespace QN01Box;

	size_t   n = 2;
	double l[] = { -1., -1.};
	double u[] = { 2., 2. };
	double x[] = { .5, .5 };
	double g[] = { 2., -3.};
	double p[2];

	// check formula for case where g[0] >= 0 and g[1] <= 0
	ScaleProjGrad(p, n, x, g, l, u);
	ok &= p[0] == (x[0]-l[0])*g[0];
	ok &= p[1] == (u[1]-x[1])*g[1];

	// use default values for lower and upper limits 
	ScaleProjGrad(p, n, x, g);
	l[0] = 0.; l[1] = 0.;
	u[0] = 1.; u[1] = 1.;
	ok &= p[0] == (x[0]-l[0])*g[0];
	ok &= p[1] == (u[1]-x[1])*g[1];

	msg = "";

	return ok;
}	
/* $$
$end
*/

namespace QN01Box { 
	void ScaleProjGrad(
		double       *p,
		size_t        n, 
		const double *x, 
		const double *g, 
		const double *l,
		const double *u)
	{	while(n)
		{	--n;
			if( g[n] >= 0. )
				p[n] = (x[n] - l[n]) * g[n];
			else 	p[n] = (u[n] - x[n]) * g[n];
		}
	}
	void ScaleProjGrad(
		double       *p,
		size_t        n, 
		const double *x, 
		const double *g) 
	{	while(n)
		{	--n;
			if( g[n] >= 0. )
				p[n] = (x[n] - 0.) * g[n];
			else 	p[n] = (1. - x[n]) * g[n];
		}
	}
}
