# ifndef c2dsyevIncluded
# define c2dsyevIncluded

extern "C" void c2dsyev_( 
	int    *IJOBZ, 
	int    *IUPLO, 
	int    *N, 
	double *A, 
	int    *LDA, 
	double *W, 
	double *WORK, 
	int    *LWORK, 
	int    *INFO 
);

# endif
