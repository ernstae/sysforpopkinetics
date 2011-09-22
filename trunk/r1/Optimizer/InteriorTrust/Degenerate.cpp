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
/*
Mitches Problem: fourParamQuadratic_isWithinTolTest

Brad                       Mitch
(n-1) * (2 * x[i-1] - 1)   X[i]
0 <= x[i-1] <= 1           (1-n) <= X[i] <= (n-1)
f(x)                       F(X)

F(X) = sum_i=1^n .5 * i^2 * [ X[i-1] + i * (-1)^i ]^2
f(x) = sum_i=1^n .5 * i^2 * [ (n-1) * (2 * x[i] - 1) + i * (-1)^i ]^2
     = sum_i=1^n .5 * i^2 * [ 2 * (n-1) * x[i-1] - (n-1) + i * (-1)^i ]^2
*/


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
	{	x = new double[n_]; 
		g = new double[n_]; 
	}
	// destructor for a function object
	~Fun(void)
	{	delete [] x; 
		delete [] g; 
	}
	/*
	evaluate the function
	g[i-1] = i^2 * [ 2 * (n-1) * x[i-1] - (n-1) + i * (-1)^i ] * 2 * (n-1)
	g[i-1] = 0 <=> 2 * (n-1) * x[i-1] - (n-1) + i * (-1)^i = 0
                   <=> x[i-1] = .5 * [ 1. - (-1)^i * i /(n-1) ] 
                              = .5  - (-1)^i * i /(2 * n - 2) 
                              = [ n - 1  - (-1)^i * i ] /(2 * n - 2) 

	Hence minimizer of f w.r.t. x in R is
	// x[i-1]  =  (n + i - 1) / (2 * n - 2)  if i odd
	// x[i-1]  =  (n - i - 1) / (2 * n - 2)  if i even
	// 
	// Note that for minimizing f w.r.t x in [0,1]:
	// the constraint corresponding to x[n-2] is degererate,
	// the constraint corresponding to x[n-1] is active and not degenerate,
	// none of the other constraints are active.
	//
	// Also note that the Hessian H of f is diagonal with element 
	// H[i-1, i-1] = i^2 * [ 2 * (n-1) ]^2
	*/
	const char * function(const double *x_, double &f_ )
	{	size_t i;
		double sign;
		double diff;
		double diff_x;

		for(i = 0; i < n; i++)
			x[i] = x_[i];

		f_ =   0.;
		sign = 1.;
		for(i = 1; i <= n; i++)
		{	// (-1)^i
			sign  *= -1.;
			diff   = 2. * (n-1) * x[i-1] -  (n-1) + i * sign;
			diff_x = 2. * (n-1) ;
			f_    += .5 * i * i * diff * diff;
			g[i-1] = i * i * diff * diff_x;
		}
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
	size_t      fcount;
	size_t      gcount;

	// data that defines the function
	const size_t n;
	double      *x;	
	double      *g;
};

}
// end empty namespace

bool Degenerate(std::string &Msg)
{	bool                   ok = true;
	std::ostream            &os = std::cout;
	const int             level = 0;
	const size_t         ItrMax = 20;
	const size_t              n = 3;
	const size_t        QuadMax = 20 * ItrMax;
	const double          delta = 1e-8;
	//
	Memory<double> dMemory(4 * n +  n * n );
	double  *xCur = dMemory(n);
	double  *sCur = dMemory(n);
	double  *gCur = dMemory(n);
	double  *gOut = dMemory(n);
	double  *HCur = dMemory(n * n);
	//
	std::string msg;

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
