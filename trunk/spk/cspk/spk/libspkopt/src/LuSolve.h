# ifndef CppADLuSolveIncluded
# define CppADLuSolveIncluded

// BEGIN SHORT COPYRIGHT
/* -----------------------------------------------------------------------
CppAD Copyright (C) 2003 Bradley M. Bell

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
------------------------------------------------------------------------ */
// END SHORT COPYRIGHT

/*
$begin LuSolve$$
$escape #$$
$spell
	fabs
	Geq
	CppADvector
	Ns
	Bradley
	cassert
	namespace
	emax
	etmp
	inline
	nrow
	ncol
	pelement
	jpivot
	ip
	jp
	vecn
	vecm
	cmath
	bool
	exp
	Lu
	Rhs
	Gauass
	logdet
	Leq
	signdet
	const
	std
$$

$index LuSolve$$
$mindex Lu eliminate pivot solve linear equation$$
$section Lu Factor and Solve with Total Pivoting$$

$pre
$$

$table
$bold Syntax$$ $cnext 
$syntax%int LuSolve( 
	size_t               %n%, 
	size_t               %m%, 
	CppADvector<%Type%> &%Matrix%, 
	CppADvector<%Type%> &%Rhs%, 
	CppADvector<%Type%> &%Result%, 
	%Type% &%logdet%)%$$
$tend

$fend 20$$

$head Description$$
Solves the linear equation
$latex \[
	Matrix * Result = Rhs
\] $$
where $italic Matrix$$ is an $latex n \times n$$ matrix,
$italic Rhs$$ is an $latex n x m$$ matrix, and
$italic Result$$ is an $latex n x m$$ matrix.

$head Type Requirements$$
The following operations must be defined for $italic Type$$ objects
$italic x$$ and $italic y$$:

$table
$bold Operation$$ $cnext $bold Description$$  $rnext
$syntax%%Type% %x%(%i%)%$$ $cnext
	constructs a $italic Type$$ object from the $code int$$ $italic i$$
$rnext
$syntax%log(%x%)%$$ $cnext
	returns the logarithm of $italic x$$ as a $italic Type$$ object
$rnext
$syntax%%x% = %y%$$ $cnext
	assignment operator
$rnext
$syntax%%x% += %y%$$ $cnext
	addition computed assignment operator
$rnext
$syntax%%x% -= %y%$$ $cnext
	subtraction computed assignment operator
$rnext
$syntax%%x% /= %y%$$ $cnext
	division computed assignment operator
$rnext
$syntax%%x% * %y%$$ $cnext
	multiplication operator (returns a $italic Type$$ object)
$tend


$head Storage Convention$$
The matrices stored in row major order.
To be specific, if $latex A$$ contains the vector storage for an
$latex n x m$$ matrix,
$latex i$$ is between zero and $latex  n-1$$,
and $latex j$$ is between zero and $latex m-1$$,
$latex \[

	A_{i,j} = A[ i * m + j ]
\] $$
(The length of $latex A$$ must be equal to $latex  n * m $$.)

$head LeqZero$$
There is a template function definition of the form
$syntax%
	template (class %Type%>
	bool LeqZero<%Type%>(const %Type% &%x%)
%$$
defined in the $code CppAD$$ namespace and used by $code LuSolve$$.
This function returns true if $italic x$$ is less than or equal to zero.
It assumes that the operator $code <=$$ is defined for $italic Type$$ objects. 
This is used to avoid taking the log of a negative real value.
If this operator is not defined for your use of $italic Type$$
you will need to specialize this template so that it works for your
use of $code LuSolve$$.

There is a specialization of $code LeqZero$$ for the case
where $italic Type$$ is $code AD< std::complex<double> >$$
in the 
$code /GradLu.cpp//GradLu Example/$$, 

$head AbsGeq$$
There is a template function definition of the form
$syntax%
	template (class %Type%>
	bool AbsGeq<%Type%>(const %Type% &%x%, const %Type% &%y%)
%$$
defined in the $code CppAD$$ namespace and used by $code LuSolve$$.
This function returns true if the absolute value of $italic x$$
is greater than or equal the absolute value of $italic y$$.
It assumes that the operators $code <=$$ and $code >=$$ are
defined for $italic Type$$ objects. 
If these operators are not defined for your use of $italic Type$$
you will need to specialize this template so that it works for your
use of $code LuSolve$$.

There is a template specialization of $code AbsGeq$$ for the case
where $italic Type$$ is $code std::complex<double>$$
in the 
$code /LuSolve.cpp//LuSolve Example/$$, 

$head n$$
is the number of rows in 
$italic Matrix$$,
$italic Rhs$$,
and $italic Result$$.

$head m$$
is the number of columns in 
$italic Rhs$$
and $italic Result$$.
It is ok for $italic m$$ to be zero which is reasonable when
you are only interested in the determinant of $italic Matrix$$.


$head Matrix$$
On input, this is an
$latex n \times n$$ matrix containing the variable coefficients for 
the equation we wish to solve.
On output, the elements of $italic Matrix$$ have been overwritten
and are not specified.

$head Rhs$$
On input, this is an
$latex n \times m$$ matrix containing the right hand side
for the equation we wish to solve.
On output, the elements of $italic Rhs$$ have been overwritten
and are not specified.
If $italic m$$ is zero, $italic Rhs$$ is not used.

$head Result$$
On input, this is an
$latex n \times m$$ matrix and the value of its elements do not matter.
On output, the elements of $italic Rhs$$ contain the solution
of the equation we wish to solve
(unless the value returned by $code LuSolve$$ is equal to zero).
If $italic m$$ is zero, $italic Result$$ is not used.

$head logdet$$
On input, the value of $italic logdet$$ does not matter.
On output, it has been set to the 
log of the determinant of $italic Matrix$$ (but not quite).
To be more specific,
if $italic signdet$$ is the value returned by $code LuSolve$$,
the determinant of $italic Matrix$$ is given by the formula
$latex \[
	det = signdet \exp( logdet )
\] $$
This enables $code LuSolve$$ to use logs of absolute values
in the case where $italic Type$$ corresponds to a real number.


$code%
	Example/LuSolve.cpp
%$$

$head Example$$
The file
$code /LuSolve.cpp/$$
contains an example and a test of $code LuSolve$$.
It returns true if it succeeds and false otherwise.


$end
--------------------------------------------------------------------------
*/

// BEGIN CppAD namespace
namespace CppAD { // -------------------------------------------

template <class Type>
static inline bool LeqZero(const Type &x)
{	return x <= Type(0);
}


template <class Type>
static inline bool AbsGeq(const Type &x, const Type &y)
{	Type xabs = x;
	Type yabs = y;

	if( LeqZero<Type>(xabs) )
		xabs = - xabs;

	if( LeqZero<Type>(yabs) )
		yabs = - yabs;

	return xabs >= yabs;
}

template <class Type>
int LuSolve(
	size_t  n,
	size_t  m,
	CppADvector<Type> &Matrix,
	CppADvector<Type> &Rhs,
	CppADvector<Type> &Result,
	Type   &logdet)
{
	// make standard definition of log function visible

	// some constants
	const Type zero( 0 );

	// sign multiplier for the determinant

	// index and maximum element value
	size_t imax;
	size_t jmax;
	Type   emax;

	// some temporary indices
	size_t  i;
	size_t  j;
	size_t  k;

	// count pivots
	size_t p;

	// sign of the determinant
	int   signdet;

	// temporary element
	Type    etmp;

	// pivot element
	Type    pivot;

	// pivot row and column order in the matrix
	CppADvector<size_t> ip(n);
	CppADvector<size_t> jp(n);

	// -------------------------------------------------------

	// initialize row and column order in matrix not yet pivoted
	for(i = 0; i < n; i++)
	{	ip[i] = i;
		jp[i] = i;
	}

	// initialize the log determinant
	logdet  = zero;
	signdet = 1;

	for(p = 0; p < n; p++)
	{
		// determine row and column corresponding to element of 
		// maximum absolute value in remaining part of Matrix
		imax = n;
		emax = zero;
		for(i = p; i < n; i++)
		{	for(j = p; j < n; j++)
			{	CppADInternalAssert(
					(ip[i] < n) & (jp[j] < n)
				);
				etmp = Matrix[ ip[i] * n + jp[j] ];

				// check if maximum absolute value so far
				if( AbsGeq<Type>(etmp, emax) )
				{	imax = i;
					jmax = j;
					emax = etmp;
				}
			}
		}
		CppADExternalAssert( 
			(imax < n) & (jmax < n) ,
			"AbsGeq must return true when second arg is zero"
		);

		if( imax != p )
		{	// switch rows so max absolute element is in row p
			i        = ip[p];
			ip[p]    = ip[imax];
			ip[imax] = i;
			signdet  = -signdet;
		}
		if( jmax != p )
		{	// switch columns so max absolute element is in column p
			j        = jp[p];
			jp[p]    = jp[jmax];
			jp[jmax] = j;
			signdet  = -signdet;
		}
	
		// pivot using the max absolute element
		pivot   = Matrix[ ip[p] * n + jp[p] ];

		// check for determinant equal to zero
		if( pivot == zero )
		{	// abort the mission
			logdet = zero;
			return   0;
		}

		// update the determinant
		if( LeqZero<Type>( pivot ) )
		{	logdet += log( - pivot );
			signdet = - signdet;
		}
		else	logdet += log( pivot );

		/*
		Reduce by the elementary transformations that maps 
		Matrix( ip[p], jp[p] ) to one and Matrix( ip[i], jp[p] )
		to zero for i = p+1, ... , n-1
		*/

		// divide row number ip[p] by pivot element
		for(j = p+1; j < n; j++)
			Matrix[ ip[p] * n + jp[j] ] /= pivot;

		// not used anymore so no need to set to 1
		// Matrix[ ip[p] * n + jp[p] ] = Type(1);

		// divide corresponding row of right hand side by pivot element
		for(k = 0; k < m; k++)
			Rhs[ ip[p] * m + k ] /= pivot;

		for(i = p+1; i < n; i++ )
		{
			etmp = Matrix[ ip[i] * n + jp[p] ];

			for(j = p+1; j < n; j++)
			{	Matrix[ ip[i] * n + jp[j] ] -= 
					etmp * Matrix[ ip[p] * n + jp[j] ];
			}

			for(k = 0; k < m; k++)
				Rhs[ ip[i] * m + k ] -= 
					etmp * Rhs[ ip[p] * m + k];

			// not used any more so no need to set to zero
			// Matrix[ ip[i] * n + jp[p] ] = zero;
		}

	}

	// loop over equations
	for(k = 0; k < m; k++)
	{	// loop over variables
		p = n;
		while( p > 0 )
		{	--p;
			etmp                    = Rhs[ ip[p] * m + k ];
			Result[ jp[p] * m + k ] = etmp;
			for(i = 0; i < p; i++ )
				Rhs[ ip[i] * m + k ] -= etmp *
					Matrix[ ip[i] * n + jp[p] ];
		}
	}
	return signdet;
}

} // End CppAD namespace -----------------------------------------------------

// END PROGRAM

# endif
