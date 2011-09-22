# ifndef NewtonStepIncluded
# define NewtonStepIncluded

# ifdef __cplusplus
# define externC extern "C"
# else
# define externC
# endif

externC void NewtonStep(
	int           MaxStatus   , 
	char         *status      , 
	double       *lambdaStep  , 
	double       *wStep       ,
	int           M           ,
	int           N           ,
	const double *psi         ,
	const double *lambda      , 
	const double *w           , 
	const double *t
);

# endif
