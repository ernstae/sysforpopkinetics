#ifndef WRES_H
#define WRES_H

#include "SpkValarray.h"

// compute weighted residuals
void wres( const SPK_VA::valarray<double>& y, 
	   const SPK_VA::valarray<double>& yHat,
	   const SPK_VA::valarray<double>& R,
	   SPK_VA::valarray<double>& r,
	   SPK_VA::valarray<double>& wres );
#endif
