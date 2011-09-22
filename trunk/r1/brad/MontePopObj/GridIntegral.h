# ifndef GridIntegralIncluded
# define GridIntegralIncluded

# include <cstddef>
# include <valarray>

double GridIntegral(
	double (*F)(double *X, size_t m, void *p)  ,
	size_t                                 m   ,
	void                                  *p   ,
	const std::valarray<size_t>           &N   ,
	const std::valarray<double>           &L   ,
	const std::valarray<double>           &U
);

# endif
