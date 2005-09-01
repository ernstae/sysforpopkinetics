/*
$begin linearInterpolate$$
$latex \newcommand{\R}{{\bf R}}$$
$spell
	vector vector
	const
$$

$section Linear Interpolation$$

$table
$bold Syntax$$ $cnext
$syntax%%z% = linearInterpolate(%t%, %x%, %y%)%$$
$tend

$fend 20$$

$head Description$$
We are given a vector $latex x \in \R^n$$ 
of independent variable values
and a vector $latex y \in \R^n$$ of dependent variable values.
The vector vector $latex x$$ is monotone increasing; i.e.,
$latex x_i < x_{i+1}$$ for $latex i = 0, \ldots , n-2$$.
We define the piecewise linear function $latex z$$, 
that interpolates this data set.
by setting $latex z( x_i ) = y_i$$ for $latex i = 0 , \ldots , n-2$$
and for $latex t : [ x_0 , x_{n-1} ]$$ 
and $latex x_i < t < x_{i+1}$$
$latex \[
	z(t) = y_i \frac{x_{i+1} - t}{x_{i+1} - x_i} 
	     + y_{i+1} \frac{t - x_i}{x_{i+1} - x_i} 
\] $$

$head Scalar$$
If $italic a$$ and $italic b$$ are of type $italic Scalar$$
and $italic j$$ is of type $code size_t$$, 
$table
$bold Operation$$    $cnext $bold Description$$
$rnext $syntax%%Scalar%(%j%)%$$ $cnext 
	constructs a $italic Scalar$$ object with value equal to $italic j$$.
$rnext $syntax%%a% = %b%$$ $cnext 
	assigns the variable $italic a$$ the current value of $italic b$$.
$rnext $syntax%%a% < %b%$$ $cnext 
	returns true if $italic a$$ is less than $italic b$$.
$rnext $syntax%%a% <= %b%$$ $cnext 
	returns true if $italic a$$ is less than or equal $italic b$$.
$rnext $syntax%%a% + %b%$$ $cnext 
	returns a $italic Scalar$$ equal to $italic a$$ plus $italic b$$
$rnext $syntax%%a% - %b%$$ $cnext 
	returns a $italic Scalar$$ equal to $italic a$$ minus $italic b$$
$rnext $syntax%%a% * %b%$$ $cnext 
	returns a $italic Scalar$$ equal to $italic a$$ times $italic b$$
$rnext $syntax%%a% / %b%$$ $cnext 
	returns a $italic Scalar$$ equal to $italic a$$ divided by $italic b$$
$tend

$head Vector$$
If $italic u$$ is an object with type $italic Vector$$
and $italic j$$ is of type $code size_t$$, 
$table
$bold Operation$$    $cnext $bold Description$$ 
$rnext $syntax%%u%[%j%]%$$ $cnext 
	returns the $th j$$ element of $italic u$$ as a $italic Scalar$$.
$rnext $syntax%%u%.size()%$$ $cnext 
	returns the number of elements in the vector $italic u$$.
$tend

$head t$$
The argument $italic t$$ has prototype
$syntax%
	%Scalar% const &%t%
%$$
It specifies the argument value at which to evaluate the 
function $latex z$$ and must be in the interval $latex [ x_0 , x_{n-1} ]$$.

$head x$$
The argument $italic x$$ has prototype
$syntax%
	%Vector% const &%x%
%$$
It specifies the independent variable values corresponding to 
the measurement pairs.
The value $syntax%%x%.size()%$$ determines the number of paris; 
i.e. $latex n$$.

$head y$$
The argument $italic y$$ has prototype
$syntax%
	%Vector% const &%y%
%$$
It specifies the dependent variable values corresponding to 
the measurement pairs.
The value $syntax%%y%.size()%$$ must equal the number of paris; 
i.e. $latex n$$.

$head z$$
The return value $italic z$$ has prototype
$syntax%
	%Scalar% z
%$$
It is the value of the function we are interpolating
at the specified point $italic t$$.


$head Assumptions$$
We use $italic n$$ for the value $syntax%%x%.size()%$$
and $italic i$$ for an integer value between zero and $syntax%%n%-2%$$.
$syntax%
	%x%.size() %%>=  2
	%x%.size() %%== %y%.size()
	%x%[0]     %%<= %t%
	%t%        %%<= %x%[%n%-1]
	%x%[%i%]     <  %x%[%i%+1]
%$$

$head Side Effects$$
This routine remembers the interval corresponding to the previous
interpolation and hence the interval search is faster if near equal
values for $italic t$$ are used in sequential calls.

$children%
	linearInterpolate.cpp
%$$
$head Example$$
The routine $xref/linearInterpolate/$$ is an example and test
of $code linearInterpolate$$. 
It returns true if the test passes and false otherwise.


$end
*/

# include <cstdlib>
# include <cassert>

template <typename Scalar, typename Vector>
Scalar linearInterpolate(
	const Scalar &t, 
	const Vector &x, 
	const Vector &y)
{	
	// previous interval index
	static size_t i_last = 0;

	// number of data points for this call
	size_t n = x.size();

	assert( n      >= 2 );
	assert( n      == y.size() );
	assert( x[0]   <= t );
	assert( t      <= x[n-1] );

	// initialize as previous interval index (maximum allowable is n-2)
	size_t i = i_last;
	if( i > n-2 )
		i = 0;

	// make sure i is small enough so that x[i] <= t 
	while( t < x[i] )
	{	assert( i > 0 );
		i--;
		assert( x[i] < x[i+1] );
	}

	// make sure i is large enough so that t <= x[i+1] or i = n-1
	while( x[i+1] < t )
	{	assert( i < n-1 );
		i++;
		assert( x[i] < x[i+1] );
	}

	// interpolate i-th interval value
	Scalar dx = x[i+1] - x[i];
	assert( dx > Scalar(0) );
	Scalar z  = y[i]*(x[i+1]-t)/dx + y[i+1]*(t-x[i])/dx;

	// remember previous value of i to speed up search
	i_last = i;

	return z;
}
