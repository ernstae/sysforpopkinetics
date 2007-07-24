# ifndef GridIntegralIncluded
# define GridIntegralIncluded

# include <cstddef>
# include <valarray>

void GridIntegral(
	double (*F)(double *X, size_t m, void *p)  ,
	size_t                                 m   ,
	void                                  *p   ,
	const std::valarray<int>              &N   ,
	const std::valarray<double>           &L   ,
	const std::valarray<double>           &U   ,
	double                                &integralEstimate,
	double                                &estimateStd 
);

# endif
