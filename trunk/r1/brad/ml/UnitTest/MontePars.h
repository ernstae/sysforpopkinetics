//==============================================================================
// 
// BRAD_LINEAR_MODEL
// 
// The namespace MontePars exports the values needed by monteDriver.cpp.
// 
// The user requested the population analysis.
// 
//==============================================================================
#ifndef MONTEPARS_H
#define MONTEPARS_H

# include <valarray>

namespace MontePars {
   // possible choices for numerical integration method
   enum METHOD { 
	analytic, // analytic solution when FO model and one random effect
	grid,     // numerical integration using a uniform grid
	plain,    // plain and simple monte carlo sampling of integrand
	miser     // miser algorithm (see Seciton 7.8 of Numerical Recipies)
   };

   // users choice for this numerical integration run
   const enum METHOD method = miser;

   // number of components to number of function evaluations
   // If method = grid, this must be the number of random effects
   // otherwise this must be one
   const int nEval = 1;

   // number of function evaluations
   // If method == grid, numberEval[i] is number of grid points in i-th
   // random effect, the corresponding total number of function evaluations is
   // the product of the elements of numberEval.
   // If method != grid, numberEval[0] is the total number of functions 
   // evaluations.
   const int c_numberEval[nEval] = { 1000 };
   const std::valarray<int> numberEval(c_numberEval, nEval);
};

#endif
