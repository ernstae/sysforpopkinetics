# ifndef QN01Box_c2dsyevIncluded
# define QN01Box_c2dsyevIncluded

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
