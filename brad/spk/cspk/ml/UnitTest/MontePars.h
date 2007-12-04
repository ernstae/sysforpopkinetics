//==============================================================================
// 
// Population Fit of slope and intercept.
// 
// The namespace MontePars exports the values needed by monteDriver.cpp.
// 
// The user requested the population analysis.
// 
//==============================================================================
#ifndef MONTEPARS_H
#define MONTEPARS_H

#include <spk/SpkValarray.h>

namespace MontePars{
   enum METHOD { adapt, grid, plain, miser, monte, vegas };
   const enum METHOD method = vegas;
   const int nEval = 1;
   const int c_numberEval[ nEval ] = { 1000 };
   const SPK_VA::valarray<int> numberEval( c_numberEval, nEval );
};

#endif
