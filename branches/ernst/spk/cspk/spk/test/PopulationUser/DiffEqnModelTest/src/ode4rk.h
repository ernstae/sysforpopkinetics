#ifndef ODE4RK_H
#define ODE4RK_H

#include <spk/SpkValarray.h>

const SPK_VA::valarray<double> ode4rk(
    void  *info,
    void   G(void *info, const double z[], double g[]),
    double r0,
    double r1,
    double step,
    const  SPK_VA::valarray<double>& z0
);

#endif
