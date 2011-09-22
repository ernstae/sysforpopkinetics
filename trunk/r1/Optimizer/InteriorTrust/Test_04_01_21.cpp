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
# include <cassert>
# include <string>
# include <sstream>

// begin empty namespace
namespace {

// measurement values
double Y[] = { 
	1.1176, 
	2.1484, 
	4.2609,
	4.5914, 
	4.6107, 
	5.3898,
	4.5736, 
	2.7052, 
	3.6934, 
	3.3069, 
	1.5283,
	1.3597, 
	1.3304, 
	1.3405, 
	-0.67294, 
	-0.1583
};

// number of measurement values
size_t N = sizeof(Y) / sizeof(Y[0]);

// time corresponding to each measurement value
double T[] = {
	2.5000E-01,
	5.7000E-01,
	1.1200E+00,
	2.0200E+00,
	3.0000E+00,
	3.8200E+00,
	5.1000E+00,
	7.0300E+00,
	9.0500E+00,
	1.2120E+01,
	1.5000E+01,
	1.7000E+01,
	2.0000E+01,
	2.4370E+01,
	3.0000E+01,
	3.6000E+01 
};

// amout of does
double Dose = 320.;

// weight of the subject
double Weight = 79.6;

// fixed effects in the model
double Alpha[] = { 
	3., 
	0.080000000000000002,
	0.040000000000000001 
};

// covariance of the random effects
double D[] = {
	1., 
	0.00020000000000000001, 
  	0.40000000000000002
};

// initial value for the random effects
double bIni[] = {
	0.,
	0., 
	0
};

// lower limit for the random effects
double bLow[] = { 
	-10.,
	-10., 
	-10.
};

// upper limit for the random effects
double bUp[] = { 
	10.,
	10., 
	10.
};


double R = .4;

void Residual(size_t  j, const double *b, double &r, double *r_b)
{	size_t i;
	double t = T[j];

	double a     = Alpha[0] + b[0];
	double a_b[] = {1., 0., 0.};

	double s = Alpha[1] + b[1];
	double s_b[] = {0., 1., 0.};

	double c = Alpha[2] * Weight + b[2];
	double c_b[] = {0., 0., 1.};

	double factor = Dose * a * s;
	double factor_b[3];
	for(i = 0; i < 3; i++)
		factor_b[i] = Dose * a * s_b[i] 
		            + Dose * a_b[i] * s;

	factor = factor / c;
	for(i = 0; i < 3; i++)
		factor_b[i] = factor_b[i] / c 
			    - (factor / c ) * c_b[i];

	double numerator = exp(-s * t) - exp(-a * t);
	double numerator_b[3];
	for(i = 0; i < 3; i++)
		numerator_b[i] = t * exp(-a * t) * a_b[i]
		               - t * exp(-s * t) * s_b[i];	

	double denominator = a - s;
	double denominator_b[3];
	for(i = 0; i < 3; i++)
		denominator_b[i] = a_b[i] - s_b[i];

	double ratio = numerator / denominator;
	double ratio_b[3];
	for(i = 0; i < 3; i++)
		ratio_b[i] = numerator_b[i] / denominator
		           - ( ratio / denominator ) * denominator_b[i];

	r = factor * ratio  - Y[j];
	for(i = 0; i < 3; i++)
		r_b[i] = factor_b[i] * ratio
		       + factor * ratio_b[i];
}


// Function object class
class Fun {
public:
	// constructor for a function object
	Fun(void) : fcount(0), gcount(0)
	{ }

	// destructor for a function object
	~Fun(void)
	{ }

	// evaluate the function
	const char * function(const double *x, double &f)
	{	size_t i;
		size_t j;
		double r;
		double b[3];
		double r_b[3];

		double pi = 3.141592653;


		f = 0.;
		for(i = 0; i < 3; i++)
		{
			b[i] = bLow[i] + x[i] * (bUp[i] - bLow[i]);	
			f += .5 * b[i] * b[i] / D[i];
			f += .5 * log( 2. * pi * D[i] );
			g[i] = b[i] * (bUp[i] - bLow[i]) / D[i] ;
		}

		for(j = 0; j < N; j++)
		{	Residual(j, b, r, r_b);		
			f += .5 * r * r / R ;
			f += .5 * log( 2. * pi * R );
			for(i = 0; i < 3; i++)
				g[i] += r * r_b[i] * (bUp[i] - bLow[i]) / R;
		}
		++fcount;
		return (const char *)("ok");
	}

	// evaluate the gradient
	const char * gradient(double *g_ )
	{	size_t i;
		for(i = 0; i < 3; i++)
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
	double      g[3];
};

}
// end empty namespace

bool Test_04_01_21(std::string &Msg)
{	bool                   ok = true;
	const double         step = 1e-3;
	const double      epsilon = 1e-10;
	const char * msg;
	std::ostream            &os = std::cout;
	const int             level = 0;
	const size_t         ItrMax = 100;
	const size_t              n = 3;
	const size_t        QuadMax = 20 * ItrMax;
	const double          delta = 1e-7;
	//
	Memory<double> dMemory(4 * n +  n * n + 2 * n );
	double  *xCur = dMemory(n);
	double  *sCur = dMemory(n);
	double  *gCur = dMemory(n);
	double  *gOut = dMemory(n);
	double  *HCur = dMemory(n * n);

	double *gStep = dMemory(n);
	double *xStep = dMemory(n);

	// construct function object
	Fun obj;
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

	// temporaries
	size_t i;
	size_t j;

	// initial xCur 
	for(i = 0; i < n; i++)
	{	xCur[i]  = (bIni[i] - bLow[i]) / (bUp[i] - bLow[i]);
		xStep[i] = xCur[i];
	}

	// fCur is objective function value at xCur
	msg = obj.function(xCur, fCur); 
	ok &= (msg == "ok");

	// gCur is gradient at xCur
	msg = obj.gradient(gCur); 
	ok &= (msg == "ok");

	// initialize the HCur so good approximate Hessian
	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
			HCur[i * n + j ] = 0.;
		HCur[i * n + i] = 100. * MaxAbs(n, gCur);
	}
	for(i = 0; i < n; i++)
	{	xStep[i] = xCur[i] + step;
		msg = obj.gradient(gStep); 
		xStep[i] = xCur[i];
		ok &= (msg == "ok");

		Bfgs(n, epsilon, xCur, gCur, xStep, gStep, HCur);
		BfgsCur += (msg == "ok");
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
