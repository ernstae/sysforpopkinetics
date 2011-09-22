#include <stdlib.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_monte.h>
#include <gsl/gsl_monte_plain.h>
#include <gsl/gsl_monte_miser.h>
#include <gsl/gsl_monte_vegas.h>

/* 
$begin MonteExample$$
$spell
	const
	rng
	env
	alloc
	gsl
	xl
	xu
	res
	chisq
	dof
	printf
	fabs
	Itzykson
	Drouffe
	Zucker
	Acad
	params
	cos
$$

$index example, monte$$
$index monte, example$$
$index integrate, monte$$
$section Example Usage of Gsl Monte Carlo Integrals$$

$head Problem Definition$$
We are given the problem of computing the integral
$latex \[
\begin{array}{rcl}
I 
& = &	
\frac{1}{2 \pi} 
	\int_{-\pi}^{+\pi} 
	\int_{-\pi}^{+\pi}
	\int_{-\pi}^{+\pi}
	\frac{dx \; dy \; dz}{1 - \cos(x) \cos(y) \cos(z)}
\\
& = & 
\frac{1}{4 \pi^3} \Gamma \left( \frac{1}{4} \right)^4 
\end{array}
\] $$
which is given in more digits than double-precision by
$codep */
double exact = 1.3932039296856768591842462603255;

/* $$
This example is taken from C.Itzykson, J.M.Drouffe, 
$italic Statistical Field Theory$$ 
Volume 1, Section 1.1, p21, 
which cites the original paper M.L.Glasser, I.J.Zucker, 
Proc.Natl.Acad.Sci.USA 74 1800 (1977) 
$pre

$$
For simplicity we compute the integral over the region 
(0,0,0) -> (pi,pi,pi) and multiply by 8 

$head Integrand$$
The integrand function 
$syntax%
	double (* %f%) (double *%x%, size_t %dim%, void * %params%)
%$$	
should return the value $syntax%%f%(%x%, %params%)%$$ 
for argument $italic x$$ and parameters $italic params$$, 
where $italic x$$ is an array of size $italic dim$$
giving the coordinates of the point where the function 
is to be evaluated.
$codep */
double
g (double *k, size_t dim, void *params)
{
  double A = 1.0 / (M_PI * M_PI * M_PI);
  return A / (1.0 - cos (k[0]) * cos (k[1]) * cos (k[2]));
}
/* $$

$head Displaying Results$$
The following routine is used to display the results:
$codep */
void
display_results (char *title, double result, double sigma)
{
  printf ("%s ==================\n", title);
  printf ("result = % .6f\n", result);
  printf ("sigma  = % .6f\n", sigma);
  printf ("exact  = % .6f\n", exact);
  printf ("error  = % .6f = %.1g sigma\n", result - exact,
          fabs (result - exact) / sigma);
}
/* $$
 *
$childtable%
	gsl_rng.omh%
	gsl_monte_vegas.omh
%$$

$head Driver Program$$
The main program is follows:
$codep */
int
main (void)
{
  double res, err;

  // lower and upper limits for integration
  double xl[3] = { 0, 0, 0 };
  double xu[3] = { M_PI, M_PI, M_PI };

  // declare T as a type of random number generator
  const gsl_rng_type *T;

  // declare r as a random number generator
  gsl_rng *r;

  // First element of the structure is the integrand described above
  // Second element is the value of dim for this problem
  // Third element contains the parameter values for this problem
  gsl_monte_function G = { &g, 3, 0 };

  // number of function calls to use
  size_t calls = 500000;

  // default set up of random number environment
  gsl_rng_env_setup ();

  // default type of random number generator
  T = gsl_rng_default;

  // default random number generator
  r = gsl_rng_alloc (T);

  // Plain
  { // create a plain 3 dimensional Monte-Carlo integrator
    gsl_monte_plain_state *s = gsl_monte_plain_alloc (3);
    // integrate over the region defined by xl, and xu
    gsl_monte_plain_integrate (&G, xl, xu, 3, calls, r, s, &res, &err);
    // return the memory used by the integrator
    gsl_monte_plain_free (s);
    // display the results
    display_results ("plain", res, err);
  }

  // Miser
  { // create a Miser 3 dimensional Monte-Carlo integrator
    gsl_monte_miser_state *s = gsl_monte_miser_alloc (3);
    // integrate over the region defined by xl, and xu
    gsl_monte_miser_integrate (&G, xl, xu, 3, calls, r, s, &res, &err);
    // return the memory used by the integrator
    gsl_monte_miser_free (s);
    // display the results
    display_results ("miser", res, err);
  }

  // Vegas
  { // create a Vegas 3 dimensional Monte-Carlo integrator
    gsl_monte_vegas_state *s = gsl_monte_vegas_alloc (3);
    // initial integration over the region defined by xl, and xu
    gsl_monte_vegas_integrate (&G, xl, xu, 3, 10000, r, s, &res, &err);
    // display the results
    display_results ("vegas warm-up", res, err);

    printf ("converging...\n");

    do
      { // next integration over the region defined by xl, and xu
        gsl_monte_vegas_integrate (&G, xl, xu, 3, calls/5, r, s, &res, &err);
        // display the results including chi-squared / degrees of freedom
        printf ("result = % .6f sigma = % .6f "
                "chisq/dof = %.1f\n", res, err, s->chisq);
      }
    // repeat until chi-squared / degrees of freedom is within in .5 of 1
    while (fabs (s->chisq - 1.0) > 0.5);

    // label final result
    display_results ("vegas final", res, err);

    // return the memory used by the integrator
    gsl_monte_vegas_free (s);
  }
  return 0;
}
/* $$
$end
--------------------------------------------------------------------------
*/
