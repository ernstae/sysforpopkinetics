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
// BEGIN PROGRAM

# include "QuasiNewton01Box.h"
# include "Memory.h"
# include "PlusInfinity.h"
# include <iostream>
# include <cmath>
# include <string>
# include <sstream>

// begin empty namespace
namespace {

// Function object class
class Fun {
public:
	// constructor for a function object
	Fun(double a_) : fcount(0), gcount(0), infinitycount(0), a(a_)
	{	// minimum occurs when a * (x - .5) - 1 / (x - .5) = 0.; i.e.,
		// x = .5 + 1./sqrt(a) 
		assert( .5 + 1. / sqrt(a) < 1. );   
	}

	// destructor for a function object
	~Fun(void)
	{ }

	// evaluate the function
	const char * function(const double *x_, double &f_ )
	{	++fcount;
		xm = *x_ - .5;
		if( xm > 0. )
			f_ =  .5 * a * xm * xm + 1. / xm;
		else
		{	f_ = PlusInfinity(double(0));
			++infinitycount;
		}
		return (const char *)("ok");
	}
	// evaluate the gradient
	const char * gradient(double *g_ )
	{	++gcount;
		assert( xm > 0. );
		*g_ = a * xm - 1. / (xm * xm); 
		return (const char *)("ok");
	}
	// return function counter
	size_t functionCount(void)
	{	return fcount; }

	// return gradient counter
	size_t gradientCount(void)
	{	return gcount; }

	// return infinity counter
	size_t infinityCount(void)
	{	return infinitycount; }
private:
	size_t fcount;
	size_t gcount;
	size_t infinitycount;
	double xm;
	double a;
};

}
// end empty namespace

bool PlusInfinity(std::string &Msg)
{	bool                   ok = true;
	const char * msg;
	std::ostream            &os = std::cout;
	const int             level = 0;
	const size_t         ItrMax = 10;
	const size_t              n = 1;
	const size_t        QuadMax = 20 * ItrMax;
	const double          delta = 1e-7;
	//
	Memory<double> dMemory(4 * n +  n * n );
	double  *xCur = dMemory(n);
	double  *sCur = dMemory(n);
	double  *gCur = dMemory(n);
	double  *gOut = dMemory(n);
	double  *HCur = dMemory(n * n);

	// construct function object
	double a = 100.;
	Fun obj(a);
	/*
	Current iterate values
	*/
	bool          sOkCur = false;
	size_t        ItrCur = 0;
	size_t       QuadCur = 0;
	size_t       BfgsCur = 0;
	double          rCur = .5;
	double          fCur;
	/*
	Output values
	*/
	double         fOut;

	// initial xCur and sCur
	size_t i;
	for(i = 0; i < n; i++)
		xCur[i] = .8;

	// fCur is objective function value at xCur
	msg = obj.function(xCur, fCur); 
	ok &= (msg == "ok");

	// gCur is gradient at xCur
	msg = obj.gradient(gCur); 
	ok &= (msg == "ok");

	// initialize the HCur as diag( .4 * abs(gCur) ) so step overshoots
	size_t j;
	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
			HCur[i * n + j ] = 0.;
		HCur[i * n + i] = .4 * gCur[i];
	}
	
	Msg = QuasiNewton01Box(
		// Input Arguments
		os,
		level,
		ItrMax,
		QuadMax,
		n,
		delta,
		obj,
		// Input+Output Arguments
		sOkCur,
		ItrCur,
		QuadCur,
		BfgsCur,
		rCur,
		fCur,
		xCur,
		sCur,
		gCur,
		HCur 
	);
	if( Msg == "ok" )
	{	std::ostringstream buf;
		buf << " :functionCount = " << obj.functionCount();
		buf << " :gradientCount = " << obj.gradientCount();
		Msg = buf.str();
	}
	else	ok = false;
	//
	// make sure we have tested the inifity return value
	ok &= obj.infinityCount() > 0;
	//
	// evaluate the objective 
	msg = obj.function(xCur, fOut);
	ok &= (msg == "ok");
	ok &= fCur == fOut;
	//
	// evaluate gradient
	msg = obj.gradient(gOut);
	for(i = 0; i < n; i++)
		ok &= gCur[i] == gOut[i];
	//
	// check the scaled projected gradient
	for(i = 0; i < n; i++)
	{	double p = gOut[i];
		if( p > 0. )
			p *= (xCur[i] - 0.);
		else	p *= (1. - xCur[i]);

		ok &= fabs(p) <= delta;
	}
	return ok;
}

// END PROGRAM
