#ifndef READ_NONMEM_DATAFILE_GAMMA_WT_TIME_Y_H
#define READ_NONMEM_DATAFILE_GAMMA_WT_TIME_Y_H

#include <spk/SpkValarray.h>

int read_nonmem_datafile_gamma_wt_time_y(
          const char * const name,
          int              &M,
          int              &Nsum,
          SPK_VA::valarray<int>&    N,
          SPK_VA::valarray<double>& gamma,
          SPK_VA::valarray<double>& wt,
          SPK_VA::valarray<double>& time,
          SPK_VA::valarray<double>& y );

#endif
