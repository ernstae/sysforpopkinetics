#ifndef READ_NONMEM_THEOPHYLLINE_H
#define READ_NONMEM_THEOPHYLLINE_H

#include <spk/SpkValarray.h>

int readNonmemTheophylline(
          const char *const name,
          int&              M,
          int&              Nsum,
          SPK_VA::valarray<int>&    N,
          SPK_VA::valarray<double>& gamma,
          SPK_VA::valarray<double>& w,
          SPK_VA::valarray<double>& t,
          SPK_VA::valarray<double>& y );

#endif
