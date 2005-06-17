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
	Fun(size_t n_ ):  fcount(0), gcount(0), n( n_ )
	{	g = new double[n_]; 
	}
	// destructor for a function object
	~Fun(void)
	{	delete [] g; 
	}
	// evaluate the function
	const char * function(const double *x, double &f )
	{	size_t i;

		double bUp = .5 * (n - 1);
		double bLow = - bUp;

		f = 0;
		for(i = 0; i < n; i++)
		{	double bi = bLow + (bUp - bLow) * x[i];
			f    += (bi - i - 1) * (bi - i - 1) + bi * bi;
			g[i]  = ( 2. * (bi - i - 1) + 2. * bi ) ;
			g[i] *= (bUp - bLow);
		}
		// g[i] = 0 <= 2 * bi - i - 1 = 0 
		//          <= bi = (i + 1) / 2 
		//          <= x[i] = ( (i + 1) / 2 - bLow ) / (bUp - bLow)
		//          <= x[i] = (i + n) / (2 * ( n - 1) )

		++fcount;
		return (const char *)("ok");
	}
	// evaluate the gradient
	const char * gradient(double *g_ )
	{	size_t i;
		size_t j;

		for(i = 0; i < n; i++)
			g_[i] = g[i];

		++gcount;
		return (const char *)("ok");
	}
	// return function counter
	size_t functionCount(void)
	{	return fcount; }

	// return gradient counter
	size_t gradientCount(void)
	{	return gcount; }
private:
	size_t     fcount;
	size_t     gcount;

	// data that defines the function
	const size_t n;
	double      *g;
};

}
// end empty namespace

bool Test_03_12_16(std::string &Msg)
{	bool                   ok = true;
	const char * msg;
	std::ostream            &os = std::cout;
	const int             level = 0;
	const size_t         ItrMax = 20;
	const size_t              n = 4;
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
	Fun obj(n);
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
		xCur[i] = .5;

	// fCur is objective function value at xCur
	msg = obj.function(xCur, fCur); 
	ok &= (msg == "ok");

	// gCur is gradient at xCur
	msg = obj.gradient(gCur); 
	ok &= (msg == "ok");

	// initialize the HCur as the identity matrix
	size_t j;
	for(i = 0; i < n; i++)
		for(j = 0; j < n; j++)
			HCur[i * n + j ] = static_cast<double>( i == j );
	
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
