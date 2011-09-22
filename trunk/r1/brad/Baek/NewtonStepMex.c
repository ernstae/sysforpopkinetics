/*  Link to Matlab Mex file routines */
# include "mex.h"

/* Link to Cpp NewtonStep routine */
# include "NewtonStep.h"

/* maximum length of return message */
# define MaxStatus  100

/*  Entry point for the Mex function  
	[status, lambdaStep, wStep] = NewtonStepMex(Psi, lambda, w, t)
see NewtonStep.tex for documentation
*/

void mexFunction(
	int nlhs             ,  /*  number of left hand sides */
	mxArray *plhs[]      ,  /*  the left hand sides  */
	int nrhs             ,  /*  number of right hand sides */
	const mxArray *prhs[])  /*  the right hand sides  */
{
	/*  input arugments */
	const double *t, *lambda, *w, *Psi;

	/*  output arugments */
	char   status[100];
	double *lambdaStep, *wStep;

	/*  number of rows and columns in Psi */
	int M, N; 

	/* argument index */
	int i;

	/*  is everything ok  */
	bool ok;

	/*  check for proper number of arguments */
	ok = true;
	if( ok && nrhs != 4 )
	{	sprintf(status, "NewtonStep: requires four input arguments");
		ok      = false;
	}
	if( ok && nlhs != 3 )
	{	sprintf(status, "NewtonStep: requires three output arguments");
		ok      = false;
	}

	/*  determine if all the input arguments are real matrices */
	for(i = 0; i < 4; i++ ) 
	if( ok && ( mxIsComplex(prhs[i]) || ! mxIsDouble(prhs[i]) ) ) 
	{	sprintf(status, 
			"NewtonStep: input argument %i is not a real matrix",
			i+1
		);
		ok      = false;
	}

	if( ok )
	{	/*  get the dimensions of Psi */
		M = mxGetM( prhs[0] );
		N = mxGetN( prhs[0] );
	}

	/* check dimension of lambda */
	if( ok && ( mxGetM(prhs[1]) != N || mxGetN(prhs[0]) != 1 ) )
	{	sprintf(status, "NewtonStep: lambda is not an N by 1 matrix");
		ok      = false;
	}

	/* check dimension of w */
	if( ok && ( mxGetM(prhs[2]) != M || mxGetN(prhs[0]) != 1 ) )
	{	sprintf(status, "NewtonStep: lambda is not an M by 1 matrix");
		ok      = false;
	}

	/* check dimension of t */
	if( ok && ( mxGetM(prhs[3]) != 1 || mxGetN(prhs[3]) != 1 ) )
	{	sprintf(status, "NewtonStep: t is not a 1 by 1 matrix");
		ok      = false;
	}

	if( ok )
	{
		/* get the input argument pointers */
		Psi    = mxGetPr(prhs[0]);
		lambda = mxGetPr(prhs[1]);
		w      = mxGetPr(prhs[2]);
		t      = mxGetPr(prhs[3]);

		/* create the output arguments */
		plhs[1] = mxCreateDoubleMatrix(N, 1, mxREAL);
		plhs[2] = mxCreateDoubleMatrix(M, 1, mxREAL);

		/* pointers to output arguments */
		lambdaStep = mxGetPr(plhs[1]);
		wStep      = mxGetPr(plhs[2]);
		NewtonStep(
			MaxStatus, 
			status, 
			lambdaStep, 
			wStep, 
			M,
			N,
			Psi,
			lambda, 
			w, 
			t 
		);
		sprintf(status, "NewtonStep: ok");

	}
	else
	{	/* create empty output arguments */
		plhs[1] = mxCreateDoubleMatrix(0, 1, mxREAL);
		plhs[2] = mxCreateDoubleMatrix(0, 1, mxREAL);
	}
	/* return status message */
	plhs[0] = mxCreateString(status);
}
