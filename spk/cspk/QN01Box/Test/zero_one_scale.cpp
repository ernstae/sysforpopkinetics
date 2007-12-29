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
// BEGIN PROGRAM

# include <QN01Box/QuasiNewton01Box.h>
# include <QN01Box/zero_one_scale.h>
# include <QN01Box/Memory.h>
# include <iostream>
# include <cmath>
# include <string>
# include <sstream>

// begin empty namespace
namespace { 
//
// Function object class
class Fun {
private:
	const size_t            m;
	QN01Box::Memory<double> dmemory;
	double                 *y;

	size_t                  fcount;
	size_t                  gcount;
	size_t                  Hcount;
public:
	// constructor for original function object
	Fun(size_t m_) :  m(m_), dmemory(m_), fcount(0), gcount(0), Hcount(0)
	{	y = dmemory(m);
	}
	// destructor for a function object
	~Fun(void)
	{}
	// evaluate the function
	const char * function(const double *y_, double &f)
	{	size_t i;
		for(i = 0; i < m; i++)
			y[i] = y_[i];
		f = 0.;
		for(i = 0; i < m; i++)
			f += .5 * (y[i] - i - 1.) * (y[i] - i - 1.);
		++fcount;
		return "ok";
	}
	// evaluate the gradient
	const char * gradient(double *f_y)
	{	size_t i;
		for(i = 0; i < m; i++)
			f_y[i] = y[i] - i - 1.;
		++gcount;
		return "ok";
	}
	// evaluate the Hessian
	const char * Hessian(double *f_yy)
	{	++Hcount;
		return "not available";
	}
	// return function counter
	size_t functionCount(void)
	{	return fcount; }

	// return gradient counter
	size_t gradientCount(void)
	{	return gcount; }
	// return Hessian counter
	size_t HessianCount(void)
	{	return Hcount; }
};

} 
// end empty namespace

bool zero_one_scale_(std::string &msg)
{	bool ok = true;
	using namespace QN01Box;
	size_t i, j;

	// dimension of domain of h
	size_t m = 3;
	Fun h(m);

	// lower and upper limits for domain of h
	double a[] = { -5., 1., -4. };
	double b[] = { +5., 1., +4. };

	std::ostream    &os       = std::cout;
	int              level    = 0;
	size_t           itr_max  = 50;
	size_t           quad_max = 40;
	size_t           n        = m - 1;
	ConvergeNorm     norm     = GradMaxAbs;
	double           delta    = 1e-7;
	zero_one_scale<Fun> obj(&h, m, n, a, b);
	bool             sok_cur  = false;
	size_t           itr_cur  = 0;
	size_t           quad_cur = 0;
	size_t           bfgs_cur = 0;
	double           r_cur    = .25;
	double           f_cur;

	Memory<double>  dmemory(m + n + n + n + n * n );
	double          *y_cur    = dmemory(m);
	double          *x_cur    = dmemory(n);
	double          *s_cur    = dmemory(n);
	double          *g_cur    = dmemory(n);
	double          *H_cur    = dmemory(n * n);

	for(i = 0; i < m; i++)
	{	y_cur[i] = 0.;
		y_cur[i] = std::max(y_cur[i], a[i]);
		y_cur[i] = std::min(y_cur[i], b[i]);
	}

	// convert from y to x coordinates
	obj.to_zero_one(x_cur, y_cur);
	// evaluate the initial objective
	msg = obj.function(x_cur, f_cur);
	ok &= (msg == "ok");

	// evaluate the initial gradient
	msg = obj.gradient(g_cur);
	ok &= (msg == "ok");

	// initialize Hessian to identity
	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
			H_cur[i * n + j] = 0.;
		H_cur[i * n + i] = 1.;
	}

	msg = QuasiNewton01Box(
		// Input Arguments
		os,
		level,
		itr_max,
		quad_max,
		n,
		norm,
		delta,
		obj,
		// Input+Output Arguments
		sok_cur,
		itr_cur,
		quad_cur,
		bfgs_cur,
		r_cur,
		f_cur,
		x_cur,
		s_cur,
		g_cur,
		H_cur 
	);
	// check the reutrn message
	if( msg == "ok" )
	{	std::ostringstream buf;
		buf << " :functionCount = " << h.functionCount();
		buf << " :gradientCount = " << h.gradientCount();
		msg = buf.str();
	}
	else	ok = false;
	// convert to y coordinates
	obj.from_zero_one(x_cur, y_cur);
	// check solution
	for(i = 0; i < m; i++)
	{	if( a[i] == b[i] )
			ok &= ( y_cur[1] == a[1] );
		else	ok &= std::fabs( y_cur[i] - i - 1. ) <= 1e-6; 
	}
	return ok;
}

// END PROGRAM
