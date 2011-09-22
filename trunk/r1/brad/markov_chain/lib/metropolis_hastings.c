/*
$begin metropolis_hastings$$
$spell
	num
	Hastings
	Spiegelhalter
$$

$section Metropolis-Hastings Algorithm$$

$head Syntax$$
$syntax%metropolis_hastings(%x%, %pi_x%, %pi%, %q%, %s%, %u%)%$$

$head x$$
The argument $italic x$$ has prototype
$syntax%
	double *%x%
%$$
The input value of $italic x$$
specifies the previous point in the Markov chain
and the output value specifies the next point.

$head pi_x$$
The argument $italic pi_x$$ has prototype
$syntax%
	double *%pi_x%
%$$
The input (output) value of $italic pi_x$$
is the value of $italic pi$$ at the input (output) value of $italic x$$.

$head pi$$
The argument $italic pi$$ has prototype
$syntax%
	double (*%pi%)(double %x%)
%$$
It returns a value proportional to
the target density at the specified argument value.

$head q$$
The argument $italic q$$ has prototype
$syntax%
	double (*%q%)(double %y%, double %x%)
%$$
It returns a value proportional to the
proposal density for the next point having value $italic y$$
given that the current point has value $italic x$$.

$head s$$
The argument $italic s$$ has prototype
$syntax%
	double (*%s%)(double %x%)
%$$
It returns a single sample from the proposal distribution 
given that the current point has value $italic x$$.

$head u$$
The argument $italic u$$ has prototype
$syntax%
	double (*%u%)(void)
%$$
It returns a single sample from a uniform distribution on (0, 1).

$head Reference$$
Page 7 of Markov Chain Monte Carlo in Practice,
W.R. Bilks,
S. Richardson,
D.J. Spiegelhalter,
Chapman & Hall,
1996.

$head Example$$
$code
$verbatim%test/metropolis_hastings_ok.c%$$
$$

$head Source Code$$
$codep */
void metropolis_hastings(
	double   *x                      , 
	double   *pi_x                   , 
	double  (*pi)(double x)          ,
	double  (*q)(double y, double x) ,
	double  (*s)(double x)           ,
	double  (*u)(void)               )
{
	double y, w, q_y, q_x, pi_y, num, den, alpha;

	// sample a point y from q(* | x)
	y = s(*x);

	// target density at y
	pi_y = pi(y);

	// compute alpha
	num   =    pi_y * q(*x, y);
	den   = (*pi_x) * q(y, *x);
	alpha = num / den;

	// check if we are accepting the new point
	if( den == 0. ||  u() <= alpha ) 
	{	*x    = y;
		*pi_x = pi_y;
	}
	return;
}
/* $$
$end
*/
