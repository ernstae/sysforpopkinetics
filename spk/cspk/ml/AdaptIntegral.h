# ifndef AdaptIntegralIncluded
# define AdaptIntegralIncluded

# include <cstddef>
# include <valarray>
# include "f2c.h"

extern void AdaptIntegral(
	double (*Functn)(integer *ndim, doublereal *z)     ,
	size_t                            ndim             ,
        size_t                            maxpts           ,
	const std::valarray<double>      &L                ,
	const std::valarray<double>      &U                ,
	double                           &integralEstimate ,
	double                           &estimateStd      
);

# endif
