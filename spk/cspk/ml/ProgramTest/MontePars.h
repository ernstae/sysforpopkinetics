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

namespace MontePars{
   enum METHOD { monte, analytic, grid };
   const enum METHOD method = monte;
   const int numberEval = 10000;
};

#endif
