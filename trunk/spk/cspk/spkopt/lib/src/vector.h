# ifndef CppADvectorIncluded
# define CppADvectorIncluded

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
$begin vector$$
$spell
	hpp
	http
	ublas
	Makefile
	mak
	std
	CppADvector
	const
	resize
$$

$mindex vector CppADvector$$
$section The CppAD Vector Template Class$$


$head Default Constructor$$
$mindex default vector construct$$
The default constructor
$syntax%
	CppADvector<%Type%> %x%;
%$$
creates a vector with no elements
that can later contain elements of the specified type.

$head Sizing Constructor$$
$mindex size vector construct$$
The sizing constructor
$syntax%
	CppADvector<%Type%> %x%(size_t %n%)
%$$
creates a vector with $italic n$$ elements
each of the specified type.

$head Copy Constructor$$
$mindex copy vector constructor$$
The copy constructor
$syntax%
	CppADvector<%Type%> %y<%Type%>%(const CppADvector<%Type%> &%x%)
%$$
creates a vector with the same type and number of elements
as $italic x$$.
In addition, the $italic Type$$ assignment operator ( $code =$$ )
is used to set each element of $italic y$$
equal to the corresponding element of $italic x$$.

$head Element Constructor and Destructor$$
$mindex vector destructor$$
The constructor for every element in a vector is called
when the vector element is created and
the corresponding destructor is called when it is removed
from the vector (this includes when the vector is destroyed).


$head Assignment$$
The assignment operation
$syntax%
	CppADvector<%Type%> & CppADvector<%Type%>::operator=(
		const CppADvector<%Type%> &%x%
	)
%$$
uses the $italic Type$$ assignment operator ( $code =$$ )
to set each element of the target ( $code *this$$) equal to 
the corresponding element of $italic x$$.
It is assumed that the target and $italic x$$ 
have the same number of elements.

$subhead Optional$$
The $code CppAD::vector$$ template class will check that
the size of the vectors is equal before doing the assignment.
If the size is not equal, $code CppAD::vector$$ will use
$code /ErrMacro/CppADExternalAssert/CppADExternalAssert/$$
to generate an appropriate error message.

$head Size$$
$mindex vector size$$
The size function
$syntax%
	size_t CppADvector<%Type%>::size(void) const
%$$
returns the number of elements contained in the vector. 

$head Resize$$
$mindex vector size$$
The resize function
$syntax%
	void CppADvector<%Type%>::resize(size_t %n%)
%$$
changes the number of elements contained in the vector 
to be $italic n$$.
None of the element values in the vector
are specified after this operation; i.e.,
any values previously stored in the vector are lost.


$head Non Constant Element Access$$
The operation
$syntax%
	Type & CppADvector<%Type%>::operator [](size_t %i%)
%$$
returns a non constant
reference to the element of the vector with index $italic i$$
were $italic i$$ is less than the
number of elements in the vector.

$subhead Optional$$
The $code CppAD::vector$$ template class will check that
the $italic i$$ is less than the size of the vector being indexed.
If it is not, $code CppAD::vector$$ will use
$code /ErrMacro/CppADExternalAssert/CppADExternalAssert/$$
to generate an appropriate error message.


$head Constant Element Access$$
The operation
$syntax%
	const Type & CppADvector<%Type%>::operator [](size_t %i%) const
%$$
returns a constant
reference to the element of the vector with index $italic i$$
were $italic i$$ is less than the
number of elements in the vector.


$subhead Optional$$
The $code CppAD::vector$$ template class will check that
the $italic i$$ is less than the size of the vector being indexed.
If it is not, $code CppAD::vector$$ will use
$code /ErrMacro/CppADExternalAssert/CppADExternalAssert/$$
to generate an appropriate error message.

$head Selecting A Vector Class$$
The default for the preprocessor symbol $code CppADvector$$
is specified by the following source code
$codep */

// The next line is C++ source code and its replacement is discussed below
# define CppADvector CppAD::vector

/* $$
You can replace this default definition and thereby
choose another vector template class.
Any vector template class that you chose must support the operations
specified above
(except for the properties of $code CppAD::vector$$ 
that are listed as optional).

$subhead std::vector$$
If in the file $code CppAD/include/vector.h$$
you replace the definition of $code CppADvector$$ by
$codep

# include <vector>
# define CppADvector std::vector

$$
then $code CppAD$$ will use $code std::vector$$ for its vector class.

$subhead boost::numeric::ublas::vector$$
If in the file $code CppAD/include/vector.h$$
you replace the definition of $code CppADvector$$ by
$codep

# include <boost/numeric/ublas/vector.hpp>
# define CppADvector boost::numeric::ublas::vector

$$
then $code CppAD$$ will use 
$code boost::numeric::ublas::vector$$ for its vector class.
If you do this you will also need to change the definition
of $code BOOST$$ in $code CppAD/Example/Makefile$$ 
and $code CppAD/Speed/Makefile$$
to be the directory where you installed 
$href%http://www.boost.org%boost%$$.
(In the windows case you will need to change
$code CppAD/Example/Example.mak$$ and
$code CppAD/Speed/Speed.mak$$ or make the corresponding change
in the project files.)

$end
------------------------------------------------------------------------ 
*/

# ifndef CppADvector
# define CppADvector CppAD::vector
# endif


//  BEGIN CppAD namespace
namespace CppAD {

template <class Type>
class vector {

public:
	// default constructor
	inline vector(void) : length(0) , data(CppADNull)
	{ }

	// constructor with a specfied size
	inline vector(size_t n) : length(n)
	{	if( length == 0 )
			data = CppADNull;
		else	data = new Type[length]; 
	}

	// copy constructor
	inline vector(const vector &x) : length(x.length)
	{	size_t i;
		if( length == 0 )
			data = CppADNull;
		else	data = new Type[length]; 

		for(i = 0; i < length; i++)
			data[i] = x.data[i];
	}

	// destructor
	~vector(void)
	{	delete [] data; }

	// size function
	inline size_t size(void) const
	{	return length; }

	// resize function
	inline void resize(size_t n)
	{	if( length > 0 )
			delete [] data;
		length = n;
		if( length > 0 )
			data = new Type[length];
		else	data = CppADNull;
	}

	// assignment operator
	inline vector & operator=(const vector &x)
	{	size_t i;

		CppADExternalAssert(
			length == x.length ,
			"size miss match in assignment operation"
		);
	
		for(i = 0; i < length; i++)
			data[i] = x.data[i];

		return *this;
	}

	// non-constant element access
	Type & operator[](size_t i)
	{	CppADExternalAssert(
			i < length,
			"vector index greater than or equal vector size"
		);
		return data[i]; 
	}

	// constant element access
	const Type & operator[](size_t i) const
	{	CppADExternalAssert(
			i < length,
			"vector index greater than or equal vector size"
		);
		return data[i]; 
	}


private:
	size_t length;
	Type   * data;
};

} // END CppAD namespace

# endif
