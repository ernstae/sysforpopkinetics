#ifndef MONTEPARS_H
#define MONTEPARS_H

namespace MontePars
{
  enum Method { analytic, grid, monte };
  
  const Method method = analytic;

  const int numberEval = 100;
};

#endif
