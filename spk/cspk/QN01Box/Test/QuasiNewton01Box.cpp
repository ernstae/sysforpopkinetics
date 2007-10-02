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
public:
	// constructor for a function object
	Fun(bool exponential_, size_t n_, double *Q_, double *b_) : 
	fcount(0), gcount(0), Hcount(0), exponential( exponential_ ), n( n_ )
	{	size_t i;
		size_t nsq( n_ * n_ );

		x = new double[n_]; 
		Q = new double[nsq];
		b = new double[n_]; 
		g = new double[n_];

		for(i = 0; i < n; i++)
			b[i] = b_[i];

		for(i = 0; i < nsq; i++)
			Q[i] = Q_[i];
	}
	// destructor for a function object
	~Fun(void)
	{	delete [] x; 
		delete [] Q;
		delete [] b;
		delete [] g;
	}
	// evaluate the function
	const char * function(const double *x_, double &f_ )
	{	size_t i;
		size_t j;

		for(i = 0; i < n; i++)
			x[i] = x_[i];

		sum = 0.;
		for(i = 0; i < n; i++)
		{	sum += b[i] * x[i];
			for(j = 0; j < n; j++)
				sum += .5 * x[i] * Q[i * n + j] * x[j]; 
		}
		if( exponential )
			f_  = exp(sum);
		else	f_  = sum;

		++fcount;
		return (const char *)("ok");
	}
	// evaluate the gradient
	const char * gradient(double *g_ )
	{	size_t i;
		size_t j;

		for(i = 0; i < n; i++)
		{	g[i] = b[i];
			for(j = 0; j < n; j++)
				g[i] += Q[i * n + j] * x[j]; 
			if( exponential )
				g[i] *= exp(sum);
			g_[i] = g[i];
		}
		++gcount;
		return (const char *)("ok");
	}
	// evaluate the Hessian
	const char * Hessian(double *H_ )
	{	size_t i;
		size_t j;

		for(i = 0; i < n; i++) 
		{	for(j = 0; j < n; j++)
			{	H_[i * n + j] = Q[i * n + j]; 
				if( exponential )
				{	H_[i + n + j] *= exp(sum);
					H_[i * n + j] += g[i] * g[j]; 
				}
			}
		}
		++Hcount;
		return (const char *)("ok");
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
private:
	size_t      fcount;
	size_t      gcount;
	size_t      Hcount;

	// data that defines the function
	const bool   exponential;
	const size_t n;
	double     sum;
	double      *x;	
	double      *Q;
	double      *b;
	double      *g;
};

} 
// end empty namespace

bool QuasiNewton01Box(std::string &Msg)
{	bool ok = true;
	using namespace QN01Box;

	std::ostream            &os = std::cout;
	int                   level = 0;
	const size_t         ItrMax = 50;
	const size_t              m = 7;
	const size_t              n = 5;
	const size_t        QuadMax = 40;
	const bool      exponential = false;
	const double          delta = 1e-7;
	//
	double           sum;
	double       apxnorm;
	size_t             i;
	size_t             j;
	size_t             k;
	std::string      msg;

	Memory<double> dMemory(6 * n + 3 * n * n + m * n);
	double  *xOut = dMemory(n);
	double  *xCur = dMemory(n);
	double  *sCur = dMemory(n);
	double  *gCur = dMemory(n);
	double  *gOut = dMemory(n);
	double  *b    = dMemory(n);
	double  *HCur = dMemory(n * n);
	double  *Q    = dMemory(n * n);
	double  *HOut = dMemory(n * n);
	double  *A    = dMemory(m * n);

	// A is a random n x m matrix
	for(i = 0; i < n * m; i++)
		A[i] = rand() / (double) RAND_MAX - .5;


	// Q is a normalized version of A * A^T 
	sum     = 0.;
	apxnorm = 0.;
	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
		{	Q[i * n + j] = 0.;
			for(k = 0; k < m; k++)
				Q[i * n + j] += A[i * m + k] * A[j * m + k];
			sum += Q[i * n + j] * Q[i * n + j];
		}
		apxnorm += sqrt(sum / double(n) );
	}
	apxnorm /= double(n);
	for(i = 0; i < n * n; i++)
		Q[i] = Q[i] / apxnorm;

	// set solution to center of box; i.e., .1 * e
	for(i = 0; i < n; i++)
	{	b[i] = 0.;
		for(j = 0; j < n; j++)
			b[i] -= Q[i * n + j] / 10.;
	}

	// special case when n == 1
	if( n == 1 )
	{	Q[0] = 8.;
		b[0] = -4.;
	}

	// construct function object
	Fun obj(exponential, n, Q, b);

	// State values
	bool      sOkCur;
	size_t    ItrCur;
	size_t    QuadCur;
	size_t    BfgsCur;
	double    rCur;
	double    fCur;

	// Output values
	double fOut;

	size_t run;
	for(run = 1; run <= 2; run++)
	{	// Initialize the state of the optimizer ---------------------
		sOkCur = false;
		ItrCur = 0;
		QuadCur = 0;
		BfgsCur = 0;
		rCur = .5;

		// initial xCur 
		for(i = 0; i < n; i++)
			xCur[i] = .75;

		// fCur is objective function value at xCur
		msg = obj.function(xCur, fCur); 
		ok &= (msg == "ok");

		// gCur is gradient at xCur
		msg = obj.gradient(gCur); 
		ok &= (msg == "ok");

		// initialize the HCur as the identity matrix
		for(i = 0; i < n; i++)
			for(j = 0; j < n; j++)
				HCur[i * n + j ] = static_cast<double>( i == j );

		// Solve the optimization problem ------------------------
		Msg = "";
		while( Msg == "" && ItrCur < ItrMax )
		{	size_t ItrPrev = ItrCur;
			size_t ItrPlus;
			if( run == 0 )
				ItrPlus = ItrCur + ItrMax;
			else	ItrPlus = ItrCur + 1;
			Msg = QuasiNewton01Box(
				// Input Arguments
				os,
				level,
				ItrCur+ItrMax,
				QuadMax,
				n,
				QN01Box::GradMaxAbs,
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
			if( run == 1 )
			{	assert( Msg != "" );
				ok &= Msg == "ok";
				// store solution corresponding to 
				// all iterations at once
				for(i = 0; i < n; i++)
					xOut[i] = xCur[i];
				// tracing for one iteration at a time solution
				level  = -level;
			}
			else if( Msg == "ok" )
			{	// trace the last iterate
				Msg = QuasiNewton01Box(
					// Input Arguments
					os,
					abs(level),
					ItrCur,
					QuadMax,
					n,
					QN01Box::GradMaxAbs,
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
			}
			else if( ItrCur > ItrPrev )
				Msg = "";
		}
	}
	// check the reutrn message
	if( Msg == "ok" )
	{	std::ostringstream buf;
		buf << " :functionCount = " << obj.functionCount();
		buf << " :gradientCount = " << obj.gradientCount();
		buf << " :HessianCount  = " << obj.HessianCount();
		Msg = buf.str();
	}
	else	ok = false;
	// check that the all at once solution is equal to
	// the once iteration at a time solution
	for(i = 0; i < n; i++)
		ok &= xOut[i] == xCur[i];
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
		if( p >= 0. )
			p *= (xCur[i] - 0.);
		else	p *= (1. - xCur[i]);
		
		ok &= fabs(p) <= delta;
	}
	if( abs(level) >= 2 )
	{	std::cout << "fOut = " << fOut << std::endl;
		std::cout << "xOut[0] = " << xCur[0];
		for(i = 1; i < n; i++)
		{	if( i % 5 == 0 )
				std::cout << std::endl;
			std::cout << ", [" << i << "] = " << xCur[i];
		}
		std::cout << std::endl;
		obj.Hessian(HOut);
		for(i = 0; i < n; i++)
		{	for(j = 0; j < n; j++)
			{	std::cout << 
				"HCur(" << i << "," << j << ") = " 
				<< HCur[i * n + j];

				std::cout << 
				", HOut(" << i << "," << j << ") = " 
				<< HOut[i * n + j];

				std::cout << 
				", Q(" << i << "," << j << ") = " 
				<< Q[i * n + j];

				std::cout << std::endl;
			}
		}
	}
	return ok;
}

// END PROGRAM
