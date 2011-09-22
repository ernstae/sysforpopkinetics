# ifndef METROPOLIS_HASTINGS_INCLUDED
# define METROPOLIS_HASTINGS_INCLUDED

void metropolis_hastings(
	double *x                        , 
	double *pi_x                     , 
	double  (*pi)(double x)          ,
	double  (*q)(double y, double x) ,
	double  (*s)(double x)           ,
	double  (*u)(void)               
);

# endif
