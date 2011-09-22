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

Reference for this example problem:     
Bennett, L., L. Swartzendruber, and H. Brown, 
NIST (1994).  
Superconductivity Magnetization Modeling.
*/
// BEGIN PROGRAM

# include <QN01Box/QuasiNewton01Box.h>
# include <QN01Box/PlusInfinity.h>
# include <iostream>
# include <cmath>
# include <string>
# include <sstream>

# define Nb 3

// begin empty namespace
namespace { 

double Data [] = {
      2.138E0,       1.309E0,
      3.421E0,       1.471E0,
      3.597E0,       1.490E0,
      4.340E0,       1.565E0,
      4.882E0,       1.611E0,
      5.660E0,       1.680E0
};
//
// Ojbective function corresponding to NIST DanWood problem
// http://www.itl.nist.gov/div898/strd/nls/data/daniel_wood.shtml
class Fun {
private:

	// values defined in constructor
	size_t       m_;
	double      *x_;	
	double      *y_;
	double       betaLow_[Nb];
	double       betaUp_[Nb];

	// other values
	double       beta_[Nb];
	size_t       fcount_;
	size_t       gcount_;
public:
	// constructor for negative log likelihood function
	Fun(const double *betaLow, const double *betaUp) 
	: fcount_(0), gcount_(0)
	{	size_t i, j;
		m_   = sizeof(Data) / (2 * sizeof(Data[0]) );
		x_   = new double[m_]; 
		y_   = new double[m_]; 
		for(j = 0; j < Nb; j++)
		{	betaLow_[j] = betaLow[j];
			betaUp_[j]  = betaUp[j];
		}
		for(i = 0; i < m_; i++)
		{	x_[i] = Data[2*i+1];
			y_[i] = Data[2*i];
		};
	}
	// destructor for a function object
	~Fun(void)
	{	delete [] x_; 
		delete [] y_;
	}
	// evaluate the negative log likelihood function
	const char * function(const double *b, double &negloglike)
	{	size_t i, j;
		double pi = 4. * atan(1.);

		// original coordinates for parameters
		for(j = 0; j < Nb; j++)
		{	beta_[j] = betaLow_[j] 
			        + b[j] * (betaUp_[j] - betaLow_[j]);
		}

		double r, f;
		double sumsq = 0.;
		negloglike   = 0.;
		for(i = 0; i < m_; i++)
		{	// model for the mean of this measurement
			f = beta_[0] * pow(x_[i], beta_[1] );
			// residual for this measurement
			r = (f - y_[i]);
			// update negative log likelihood
			sumsq      += r * r;
			negloglike += .5 * r * r / beta_[2];
			negloglike += .5 * log(2 * pi * beta_[2]); 
		}
		++fcount_;
		return (const char *)("ok");
	}
	// evaluate the gradient
	const char * gradient(double *g)
	{	size_t i;
		double f, f_0, f_1, r;

		g[0] = 0.;
		g[1] = 0.;
		g[2] = 0.;
		for(i = 0; i < m_; i++)
		{	// exponent in power
			f   = beta_[0] * pow(x_[i], beta_[1] );
			f_0 = pow(x_[i] , beta_[1]);
			f_1 = log(x_[i]) * f;
			r   = (f - y_[i]);
			g[0] += r * f_0 / beta_[2];
			g[1] += r * f_1 / beta_[2];
			g[2] += - .5 * r * r / (beta_[2] * beta_[2]);
			g[2] += .5 / beta_[2];
		}
		size_t j;
		for(j = 0; j < Nb; j++)
			g[j] *=  (betaUp_[j] - betaLow_[j]);

		++gcount_;
		return (const char *)("ok");
	}
	// evaluate Hessian
	const char * Hessian(double *H)
	{	const char *msg = "not available";
		return msg;
	}

	// return function counter
	size_t functionCount(void)
	{	return fcount_; }

	// return gradient counter
	size_t gradientCount(void)
	{	return gcount_; }
};

} 
// end empty namespace

bool DanWood(std::string &Msg)
{	bool ok = true;
	using namespace QN01Box;

	std::ostream            &os = std::cout;
	const size_t         ItrMax = 50;
	const size_t              n = Nb;
	const size_t        QuadMax = 40;
	int                   level = 0;
	double                delta = 1e-5;
	//
	size_t             i;
	size_t             j;
	std::string      msg;

	// certified parameter values
	double number_of_observations = 6.;
	double residual_sum_squares   = 4.3173084083E-03;
	double betaTrue[Nb], betaStart[Nb];
	betaTrue[0] = 7.6886226176E-01;
	betaTrue[1] = 3.8604055871E+00;
	betaTrue[2] = residual_sum_squares / number_of_observations;
	double betaLow[Nb], betaUp[Nb];
	for(j = 0; j < Nb; j++)
	{	betaLow[j] = betaTrue[j] / 10.;
		betaUp[j]  = 10. * betaTrue[j];
	}
	betaStart[0] = 1.;
	betaStart[1] = 5.;
	betaStart[2] = .0009;

	// construct function object
	Fun obj(betaLow, betaUp);

	// State values
	bool      sOkCur;
	size_t    ItrCur;
	size_t    QuadCur;
	size_t    BfgsCur;
	double    rCur;
	double    objCur;
	double    bCur[Nb];
	double    sCur[Nb];
	double    gCur[Nb];
	double    HCur[Nb * Nb];

	// Output values
	double objOut;

	sOkCur  = false;
	ItrCur  =     0;
	QuadCur =     0;
	BfgsCur =     0;
	rCur    =    .01;

	// initial bCur 
	for(j = 0; j < n; j++)
		bCur[j] = (betaStart[j] - betaLow[j])/(betaUp[j] - betaLow[j]);

	// objCur is negative log likelihood at bCur
	msg = obj.function(bCur, objCur); 
	ok &= (msg == "ok");

	// gCur is gradient at bCur
	msg = obj.gradient(gCur); 
	ok &= (msg == "ok");

	// determine the maximum absolute component of the gradient
	double gMax = 0.;
	for(j = 0; j < n; j++)
		gMax = std::max(gMax, fabs(gCur[j]));

	// initialize the Hessian
	for(i = 0; i < n; i++)
	{	for(j = 0; j < n; j++)
			HCur[i * n + j] = 0.;
		HCur [i * n + i ] = gMax;
	} 

	// Solve the optimization problem -----------------------------------
	msg = QuasiNewton01Box(
		// Input Arguments
		os,
		level,
		ItrMax,
		QuadMax,
		n,
		QN01Box::StepMaxAbs,
		delta,
		obj,
		// Input+Output Arguments
		sOkCur,
		ItrCur,
		QuadCur,
		BfgsCur,
		rCur,
		objCur,
		bCur,
		sCur,
		gCur,
		HCur 
	);
	// check the reutrn message
	if( msg == "ok" )
	{	std::ostringstream buf;
		buf << " :functionCount = " << obj.functionCount();
		buf << " :gradientCount = " << obj.gradientCount();
		Msg = buf.str();
	}
	else	ok = false;
	//
	// evaluate the objective 
	msg = obj.function(bCur, objOut);
	ok &= (msg == "ok");
	ok &= objCur == objOut;

	// check the convergence against the certified values
	double beta[Nb];
	for(j = 0; j < n; j++)
	{	beta[j] = betaLow[j] + bCur[j] * (betaUp[j] - betaLow[j]);
		ok     &= fabs( beta[j] / betaTrue[j] - 1. ) <= 1e-3;
	} 
	return ok;
}

// END PROGRAM
