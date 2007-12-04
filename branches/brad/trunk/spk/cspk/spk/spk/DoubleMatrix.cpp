/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
/*
 *************************************************************************************
 * 
 * DoubleMatrix.cpp
 *
 *************************************************************************************
 */
/*
-------------------------------------------------------------
   Constructor with no memory allocation and no dimension
-------------------------------------------------------------
$begin DoubleMatrixConstructor0 $$

$spell 
   const 
   int 
   ptr 
   initializes 
   valarray
   Spk
$$

$section Constructor with no memory allocation$$

$index matrix, DoubleMatrix constructor$$
$index DoubleMatrix, constructor$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix::DoubleMatrix /A//$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Default Constructor.  This constructor initializes 
the numbers of rows and columns to be 0 and sets the data
pointer point to null.

$head UnitTest Plan$$
$table
Check if *ptr* points to null. 
$rend
Check if an attempt to access the numbers of rows and columns is treated legal. 
$rend
$tend

$end
*/


/*
 * Construct a matrix with specified row and column dimensions
 * but without specific filling values
 * 
 * When memory allocation fails, it throws bad_alloc.
 */
/*
-------------------------------------------------------------
   Constructor with memory allocation
-------------------------------------------------------------
$begin DoubleMatrixConstructor2 $$

$spell 
   const 
   int 
   nr 
   nc 
   Sachiko 
   Goddard 
   uninitialized 
   sutter 
   alloc 
   xn 
   nxn 
   initializes
   Spk
$$

$section Constructor with memory allocation$$

$index matrix, DoubleMatrix constructor$$
$index DoubleMatrix, constructor$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix::DoubleMatrix /A/(int /nr/, int /nc/)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Constructor with two parameters.  This constructor initializes 
the numbers of rows and columns to the values specified 
by $italic nr$$ and $italic nc$$ and allocates memory for a matrix.  
If there is no enough resource available in the heap, it throws $xref/SpkException//SpkException/$$ 
of which the last $xref/SpkError//SpkError/$$ is set to $code SPK_INSUFFICIENT_MEM_ERR$$.

$head Revisit$$
revisit-Sachiko: The constructors use "new" for memory allocation. 
When new fails, it throws an exception.  The exception is not
handled at this point.

$head UnitTest Plans$$
$table
Check if the number of rows and columns are set properly.
$rend
Check if a right amount of memory has been allocated.
$rend
Check things work for a 0x0, 0x1, 0xn, 1x0, 1xn, nxn matrix.
$tend
$end
*/

/*
-------------------------------------------------------------
   Constructor for static object
-------------------------------------------------------------
$begin DoubleMatrixConstructorStatic $$

$spell 
   const 
   int 
   nr 
   nc 
   uninitialized 
   sutter 
   alloc
   Spk
$$

$section Constructor for static object$$

$index matrix, DoubleMatrix constructor$$
$index DoubleMatrix, constructor for static object$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix::DoubleMatrix /A/(const char* /identity/)/$$
$tend

$fend 25$$
$bold Deprecated$$
$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This constructor should be used to construct an empty matrix as
a static matrix object.  It keeps track of the number of the
objects.  If other constructors are used to create a 
static matrix object, it will appear as a memory leak.
$pre

$$
$italic identity$$ specifies the filename in which the static object,
$italic A$$, appears.  You can use an ANSI macro, $code __FILE__$$
to automatically obtain the information.

$end
*/

/*
-------------------------------------------------------------
   Copy constructor with no memory allocation 
-------------------------------------------------------------
$begin DoubleMatrixConstructor1 $$

$spell 
   const 
   int 
   nr 
   nc 
   xn 
   nxn 
   Sachiko 
   ptr
   Spk
$$

$section Copy Constructor$$

$index matrix, DoubleMatrix copy constructor$$
$index DoubleMatrix, copy constructor$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix::DoubleMatrix /A/(const DoubleMatrix &/B/)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Copy Constructor. Constructs    // return a pointer to a double precision vector of length
    // nr * nc containing the elements of the matrix
	// return null if the matrix is empty a copy of a matrix with the same dimensions 
and the same data elements as the matrix passed in as an argument.  
Note that when the copy constructor is called in the case of a matrix 
being passed into a function by value without const, its data pointer
$italic ptr$$ previously returned by data() becomes invalid.  Therefore, 
it is recommended that calling 
data() to return the pointer immediately before it is used.

$head UnitTest Plans$$
$table
Check if the numbers of rows and columns match to the original.
$rend
Make sure this object points to a correct location in memory even after destroying the original.
$rend
Check things work for a 0x0, 0x1, 0xn, 1x0, 1xn, nxn matrix.
$rend
$tend
$end
*/


/*
-------------------------------------------------------------
   Constructor that takes a scalar double value.
-------------------------------------------------------------
$begin DoubleMatrixConstructor3 $$

$spell 
   const 
   int 
   nr 
   nc 
   Sachiko
$$

$section Constructor that Takes a Double Scalar$$

$index matrix, DoubleMatrix constructor that takes a double$$
$index DoubleMatrix, constructor that takes a double$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix::DoubleMatrix /A/(double &/b/)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Constructs a one-by-one matrix that contains the double value b.


$head UnitTest Plans$$
$table
Check if a 1 by 1 matrix is successfully created for any value.
$rend
$tend
$end
*/


/*
-------------------------------------------------------------
   Constructor that takes an valarray object.
-------------------------------------------------------------
$begin DoubleMatrixConstructor4 $$

$spell 
   const 
   int 
   nc 
   valarray
   Spk
$$

$section Constructor that Takes an Valarray Object$$

$index matrix, valarray, DoubleMatrix constructor that takes an valarray$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix::DoubleMatrix /A/(const valarray<double>& /array/, int /nc/=1)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Constructs a (size of array)/(nc) by nc matrix that contains the double values 
that are contained in the valarray object, array.  The number of columns, nc, must 
be evenly divide the size of the valarray object, array.  The order of the values 
copied to the matrix are arranged in column-major.  If the size of array is zero,
a zero size DoubleMatrix object is created.  
$pre

$$
If there is no enough resource available in the heap, it throws $xref/SpkException//SpkException/$$ 
of which the last $xref/SpkError//SpkError/$$ is set to $code SPK_INSUFFICIENT_MEM_ERR$$.


$head UnitTest Plans$$
$table
Check if a finite size matrix is successfully created from a finite size valarray object.
Check if a zero size matrix is successfully created from a zero size valarray object.
$rend
$tend
$end
*/


/*
-------------------------------------------------------------
   Assignment operator that takes an valarray object.
-------------------------------------------------------------
$begin AssignmentOperatorValarray $$

$spell 
   const 
   int 
   nc 
   valarray
   Spk
$$

$section Assignment operator that Takes an Valarray Object$$

$index matrix, valarray, DoubleMatrix assignment operator that takes an valarray$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix& DoubleMatrix::operator=(const valarray<double>& /array/)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This overloaded assignment operator assigns the element values and the size of 
an valarray object, array, to an column DoubleMatrix object if the size of array
is not zero.   If the size of array is zero, the operator assigns the left side 
DoubleMatrix object to a zero size DoubleMatrix object. 
$pre

$$
If there is no enough resource available in the heap, it throws $xref/SpkException//SpkException/$$ 
of which the last $xref/SpkError//SpkError/$$ is set to $code SPK_INSUFFICIENT_MEM_ERR$$.

$head UnitTest Plans$$
$table
Check if a finite size column matrix is successfully assigned from a finite size valarray object.
Check if a zero size matrix is successfully assigned from a zero size valarray object.
$rend
$tend
$end
*/


/*
-------------------------------------------------------------
   Function that converts an valarray object to a matrix.
-------------------------------------------------------------
$begin ConvertValarrayToMatrix $$

$spell 
  const 
  int 
  nc 
  valarray
  cols
  Matrix matrix
  Spk
$$

$section Function that Converts an Valarray Object to a Matrix$$

$index matrix, convert, valarray, DoubleMatrix$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix& DoubleMatrix::fromValarray(const valarray<double>& /array/ int /nc/=1)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This member function returns a (size of array)/(nc) by nc matrix.  The element 
values of the matrix are copied from the element values of the valarray object,
array, in column-major order.  If the size of the valarray object is zero, the 
function returns a zero size DoubleMatrix object. The product of #rows and #cols
of the original DoubleMatrix matrix does not have to match the size of
$italic array$$.
$pre

$$
If there is no enough resource available in the heap, it throws $xref/SpkException//SpkException/$$ 
of which the last $xref/SpkError//SpkError/$$ is set to $code SPK_INSUFFICIENT_MEM_ERR$$.

$head UnitTest Plans$$
$table
Check if a (size of array)/(nc) by nc matrix is successfully returned from this function.
Check if a zero size matrix can be successfully returned from this function..
$rend
$tend
$end
*/


/*
-------------------------------------------------------------
   Function that converts itself (matrix) to an valarray object
   and empties itself.
-------------------------------------------------------------
$begin ConvertToValarray $$

$spell 
   const 
   int 
   nc 
   nr 
   valarray
   Spk
$$

$section Function that Converts Itself (matrix) to an Valarray Object$$

$index matrix, convert, valarray, DoubleMatrix$$

$table
$bold Prototype:$$ $cend
$syntax/void DoubleMatrix::ToValarray(valarray<double>& /array/)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This member function takes an valarray object, array, as an argument 
and returns it through the same argument.  The size of the returned 
array is the product of nr and nc of the calling DoubleMatrix object.
The calling DoubleMatrix object will be destroyed and 
empty upon the successful operation.
The element values of the returned array are copied from the element 
values of the calling DoubleMatrix object in column-major order.  If 
the size of the calling DoubleMatrix object is zero, the size of the 
returned array is also zero.  If the size of the DoubleMatrix object
is initially not zero, it becomes zero after the function returns.

$head UnitTest Plans$$
$table
Check if a nr*nc valarray object is successfully returned from this function argument.
Check if a zero size valarray object can be successfully returned from this function.
Check if the size of the calling DoubleMatrix object becomes zero after the function returns.
$rend
$tend
$end
*/

/*
-------------------------------------------------------------
   Function that creates a valarray object whose element
   values are copied from itself (matrix).
-------------------------------------------------------------
$begin CreateValarray $$

$spell 
  const
  valarray
  namespace
  std
  iostream
  cout
  endl
  nr
  nc
  vec
  Spk
$$

$section Creates an Valarray Object Out Of Itself$$

$index matrix, self-convert to valarray$$
$index DoubleMatrix, self-convert to valarray$$
$index matrix, create a valarray out of itself$$
$index DoubleMatrix, create a valarray out of itself$$

$table
$bold Prototype:$$ $cend
$syntax/const valarray<double> DoubleMatrix::ToValarray() const/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This member function creates and return a valarray object out of
itself.  The size of the returned 
array is the product of nr and nc of the calling DoubleMatrix object.
The element values of the returned array are copied from the element 
values of the calling DoubleMatrix object in column-major order.  If 
the size of the calling DoubleMatrix object is zero, the size of the 
returned array is also zero.

$head Example$$
If you compile and link the following program:
$codep

  #include "DoubleMatrix.h"
  #include "SpkValarray.h"
  #include <iostream>

  using namespace std;
  void main()
  {
    int i;

    //
    // mat3x2 = [ 0   3 ]
    //          [ 1   4 ]
    //          [ 2   5 ]
    //
    DoubleMatrix mat3x2(3,2);
    for( i=0; i<3*2; i++ )
      mat3x2.data()[i] = i;

    valarray<double> vec = mat3x2.toValarray();
    cout << "valarray = " << endl;
    for( i=0; i<vec.size(); i++ )
    {
      cout << vec[i] << ", ";
    }
    cout << endl;

    cout << "original matrix = " << mat3x2 << endl;
  }
$$
It produces the following result:
$codep
    valarray = 
    0, 1, 2, 3, 4, 5

    original matrix =
    [  0   3  ]
    [  1   4  ]
    [  2   5  ]
$$
$end
*/

/*
 * Destroy a matrix
 */
/*
-------------------------------------------------------------
   Destructor
-------------------------------------------------------------
$begin DoubleMatrixDestructor $$

$spell 
  const 
  int 
  nr 
  nc 
  ie 
  destructor 
  destructs
  Spk
$$

$section Destructor$$

$index matrix, DoubleMatrix destructor$$
$index DoubleMatrix, destructor$$

$table
$bold Prototype: $$ $cend
$italic not applicable $$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Free resources (ie. memory allocated for the data) and 
destroys itself.

$end
*/

/*
-------------------------------------------------------------
   Initializer with memory allocation after construct
-------------------------------------------------------------
$begin DoubleMatrixResize$$

$spell 
	const 
    int 
    nr 
    nc 
    Goddard
    Sachiko 
    const 
    na 
    init
	non 
    unexpressive 
    xn 
    nxn 
    resize
    Spk
$$

$section Resize Dimension$$

$index matrix, Resize dimension$$
$index DoubleMatrix,Resize(int, int)$$

$table
$bold Prototype:$$ $cend
$syntax/void DoubleMatrix::resize(int /nr/, int /nc/)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Changes the dimensions of the matrix.  $italic nr$$ and
$italic nc$$ must be greater than or equal to zero.
$pre

$$
If the product of $italic nr$$ and $italic nc$$ is
different from of the original, it will re-allocate
the data memory field.
$pre

$$
If there is no enough resource available in the heap, it throws $xref/SpkException//SpkException/$$ 
of which the last $xref/SpkError//SpkError/$$ is set to $code SPK_INSUFFICIENT_MEM_ERR$$.

$end
*/

/*
-------------------------------------------------------------
   Get the number of rows in the matrix
-------------------------------------------------------------
$begin nr$$

$spell 
  const 
  nr 
  int 
  iostream 
  namespace 
  cout 
  std 
  endl 
  xn 
  nxn
  Spk
$$

$section Get the number of rows$$

$index matrix, number of rows$$
$index DoubleMatrix, nr()$$ 

$table
$bold Prototype$$ $cend
$syntax/int DoubleMatrix::nr()/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code nr()$$ returns the number of rows in the matrix as 
an integer value.

$head UnitTest Plans$$
$table
Check things work for a 0x0, 0x1, 0xn, 1x0, 1xn, nxn matrix.
$rend
$tend

$head Example$$
If you compile, link and run the following program
$codep

#include <iostream>
#include "DoubleMatrix.h"
using namespace std;

void main(){

	DoubleMatrix A(3, 2);

	cout << A.nr() << endl;
}

$$
an integer value
$math% 
3 
%$$
will be printed.

$end
*/

/* 
-------------------------------------------------------------
   Get the number of columns in the matrix
-------------------------------------------------------------
$begin nc$$

$spell
	nc 
    int 
    iostream 
    cout 
    namespace 
    std 
    endl 
    const 
    xn 
    nxn
    Spk
$$

$section Get the number of columns$$

$index matrix, number of columns$$
$index DoubleMatrix, nc()$$

$table
$bold Prototype$$ $cend
$syntax/int DoubleMatrix::nc()/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code nc()$$ returns the number of columns in the matrix as 
an integer value.

$head UnitTest Plans$$
$table
Check things work for a 0x0, 0x1, 0xn, 1x0, 1xn, nxn matrix.
$rend
$tend

$head Example$$
If you compile, link and run the following program
$codep

#include <iostream>
#include "DoubleMatrix.h"
using namespace std;

void main(){

	DoubleMatrix A(3, 2);

	cout << A.nc() << endl;
}

$$
an integer value
$math% 
2 
%$$
will be printed.

$end
*/

/*
-------------------------------------------------------------
   Get the pointer to the first element of the matrix
-------------------------------------------------------------
$begin data$$

$spell
   nc 
   int 
   iostream 
   namespace 
   std 
   cout 
   const 
   endl 
   xn 
   nxn
   Sachiko 
   Goddard 
   Double double
   Spk
$$

$section Get a pointer to the first element in a matrix$$

$index matrix, pointer$$
$index DoubleMatrix, data()$$

$table
$bold Prototype$$ $cend
$syntax/*double DoubleMatrix::data()/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code data()$$ returns the pointer to the first element 
of the matrix.  When the matrix is empty, a null is returned.
Note that after an assignment operator is called or after the 
matrix is passed into a function without const, the pointer
becomes invalid.  Therefore, it is recommended that calling 
data() to return the pointer immediately before it is used.

$head UnitTest Plans$$
$table
Check things work for a 0x0, 0x1, 0xn, 1x0, 1xn, nxn matrix.
$rend
$tend

$head Example$$
If you compile, link and run the following program
$codep

#include <iostream>
#include "DoubleMatrix.h"
using namespace std;

void main(){

	DoubleMatrix A(3, 2);
	double *pA = A.data();

	// Initialize A to a matrix
	// [ 1  4 ]
	// [ 2  5 ]
	// [ 3  6 ]
	for( int i=0; i<6; i++ )
		pA[i] = i+1;

	A.print();
}

$$
a matrix 
$math% 
	[ 1  4 ]
	[ 2  5 ]
	[ 3  6 ]

%$$
will be printed.

$end
*/


/*
---------------------------------------------------------------------------
   Fill the matrix with the specified double-precision number
---------------------------------------------------------------------------
$begin DMfill$$

$spell
	int 
    val 
    const 
    Goddard 
    Sachiko 
    Mitch 
    pre 
    xn 
    nxn
    Spk
$$

$section Fill the matrix with a scalar$$

$index matrix, fill$$
$index DoubleMatrix, fill()$$

$table
$bold Prototype$$ $cend
$syntax/void DoubleMatrix::fill(double /val/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Fill the matrix with a specified double-precision number.  

$head UnitTest Plans$$
$table
Check things work for a 0x0, 0x1, 0xn, 1x0, 1xn, nxn matrix.
$rend
$tend

$head Example$$
If you compile, link and run the following program
$codep

#include "DoubleMatrix.h"

void main(){

	DoubleMatrix A(3, 2);

	// Initialize A to a matrix
	// [ 0.1  0.1 ]
	// [ 0.1  0.1 ]
	// [ 0.1  0.1 ]
	A.fill( 0.1 );

	A.print();
}

$$
a matrix 
$math% 
	[ 0.1  0.1 ]
	[ 0.1  0.1 ]
	[ 0.1  0.1 ]

%$$
will be printed.

$end
*/

/*
-----------------------------------------------------------------------------------------
   Returns true if the matrix is empty (a by b, where either/both a or b is zero)
-----------------------------------------------------------------------------------------
$begin DMisEmpty$$

$spell
	int 
    val 
    const 
    bool 
    iostream 
    namespace 
    std 
    cout 
    endl
    Spk
$$

$section Check whether matrix is empty$$

$index matrix, empty?$$
$index DoubleMatrix, isEmpty()?$$

$table
$bold Prototype$$ $cend
$syntax/bool DoubleMatrix::isEmpty()/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns true if the matrix is empty, where "empty" is defined as 
a matrix whose either or both dimension(s) is zero .  Otherwise returns false.

$head Example$$
If you compile, link and run the following program
$codep
#include <iostream>
#include "DoubleMatrix.h"

void main(){

	using namespace std;
	DoubleMatrix A;
	cout << "A is " << (A.isEmpty()? "empty" : "not empty") << "." << endl;
	}

$$
The following text appears.
$math% 
	A is empty.

%$$
will be printed.

$end
*/


/*
-------------------------------------------------------------
   Display the elements of the matrix (WYSIWYG)
-------------------------------------------------------------
$begin DMprint$$

$spell
	int 
    const 
    STDIO 
    Goddard 
    Sachiko 
    namespace 
    std 
    cout 
    endl 
    iostream 
    stdio
    Spk
$$

$section Output the matrix to the standard IO$$

$index matrix, output to stdio$$
$index DoubleMatrix, print()$$


$table
$bold Prototype$$ $cend
$syntax/void DoubleMatrix::print()/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Print the elements of the matrix proceeded by the dimensions to STDIO.
If the matrix is empty, it prints nothing.

$head Example$$
If you compile, link and run the following program
$codep
#include <iostream>
#include "DoubleMatrix.h"

void main(){

	using namespace std;

	DoubleMatrix A(3, 2);
	double *pA = A.data();
	DoubleMatrix Empty;

	// Initialize A to a matrix
	// [ 1  4 ]
	// [ 2  5 ]
	// [ 3  6 ]
	for( int i=0; i<6; i++ )
		pA[i] = i+1;

	cout << "A = " << endl;
	A.print();
	cout << endl;
	cout << "empty matrix = " << endl;
	Empty.print();
}

$$
a matrix 
$math% 
	A =
	[ 1 4 ]
	[ 2 5 ]
	[ 3 6 ]

	empty matrix =
    []

%$$
will be printed.

$end
*/


/*
-------------------------------------------------------------
   The inserter
-------------------------------------------------------------
$begin DMinserter$$

$spell
	inserter 
    ostream 
    os 
    cout 
    const 
    ob
    Spk
$$

$section Inserter <<$$

$index DoubleMatrix, operator<<$$
$index matrix, output the matrix to ostream$$

$table
$bold Prototype$$ $cend
$syntax/ostream& DoubleMatrix::operator<<(ostream& /os/, const DoubleMatrix& /ob/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The operator inserts a $code DoubleMatrix$$ object, $italic ob$$, to the left hand side
$code ostream&$$, $italic os$$, in the format shown below.
If the matrix is empty, it inserts nothing.

For a 3 by 2 matrix, $italic A$$, $code cout << "A = " << A;$$ produces:
$codep
	3 by 2
	[ a11  a12 ]
	[ a21  a22 ]
	[ a31  a32 ]
$$
If $italic A$$ were empty, then the same code produces:
$codep
	0 by 0
	[ ]
$$
$end
*/


/*
-------------------------------------------------------------
   The extractor
-------------------------------------------------------------
$begin DMextractor$$

$spell
	cout 
    ob 
    istream 
    ostream
    Spk
$$

$section Extractor >>$$

$index DoubleMatrix, operator>>$$
$index matrix, extract a matrix from istream$$

$table
$bold Prototype$$ $cend
$syntax/istream& DoubleMatrix::operator>>(istream& /is/, DoubleMatrix& /ob/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The operator extracts a $code DoubleMatrix$$ structure from $italic is$$ and 
puts it into $italic ob$$.  $italic is$$ is assumed to have the same exact format
as a DoubleMatrix object for the matrix inserted to ostream by $code DoubleMatrix::operator<<$$.
If $italic is$$ does not obey the format, the program may terminate.

For a column-major 3 by 2 matrix, the following input is expected:
$codep
		2 by 3
		[ a11  a12 ]
		[ a21  a22 ]
		[ a31  a32 ]
$$
For an empty matrix, $italic it$$ should contain nothing except for the dimensions:
$codep
		0 by 0
		[ ]
$$
$end
*/

/*
-------------------------------------------------------------
   Get the current value of the reference counter
-------------------------------------------------------------
$begin DMgetReferenceCount$$

$spell
	op 
    int
$$
$section Get the current value of the reference counter$$

$index DoubleMatrix, getRefCount()$$
$index matrix, counter$$

$table
$bold Prototype$$ $cend
$syntax/static int DoubleMatrix::getRefCount()/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns the current value of the reference counter.  The counter 
indicates the number of DoubleMatrix objects that are sharing the 
same data stored in memory.  

$end
*/

/*
-------------------------------------------------------------
   operator*
-------------------------------------------------------------
$begin DMmultiply$$

$spell
	dmat 
    const 
    Goddard 
    Sachiko 
    nc 
    nr 
    int 
    non
$$

$section Matrix-Matrix Multiplication$$

$index multiply$$
$index matrix, matrix multiplication$$
$index DoubleMatrix, multiply$$

$table
$bold DEPLICATED!!!$$ $cend Use $xref/multiply//multiply()/$$ instead. $rend
$bold Prototype$$ $cend
$syntax/const DoubleMatrix operator*(const DoubleMatrix &/A/, const DoubleMatrix &/B/)
/$$ 
$rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$bold DEPLICATED!$$  This routine merely wraps $xref/multiply//multiply()/$$.
$pre

$$
Returns matrix product $italic A$$ times $italic B$$
where $italic A$$ is a m by p matrix and $italic B$$
is a p by n matrix, for m, n and p >= 0.  
The product is a m by n matrix if neither of A or B was empty.
If either A or B was empty, an empty matrix will be returned.
If A and B's dimensions do not match, then the program terminates.


$head Example$$
If you compile, link and run the following program
$codep

#include "DoubleMatrix.h"

void main(){

	DoubleMatrix A(2, 2);
	DoubleMatrix B(2, 2);
	DoubleMatrix C;

	double *pA = A.data();
	double *pB = B.data();
	int i;

	// Set A to a matrix:
	// [ 1 3 5 ]
	// [ 2 4 6 ]
	//
	// set B to a matrix:
	// [ 1 ]
	// [ 2 ]
	// [ 3 ]
	//
	for(i=0; i<6; i++)
		pA[i] = i+1;
	for(i=0; i<3; i++)
		pB[i] = i+1;

	// Compute
	// [ 22 ]     [ 1 3 5 ]   [ 1 ]
	//         =            * [ 2 ]
	// [ 28 ]     [ 2 4 6 ]   [ 3 ]
	C = multiply(A, B);
	C.print();

}

$$
the matrix 
$math%
	[ 22 ]
	[ 28 ]
%$$
will be printed.

$end
*/

/*
-------------------------------------------------------------
   Operator=
-------------------------------------------------------------
$begin DMassign$$

$spell
	dmat 
    const 
    op 
    Goddard 
    Sachiko 
    int 
    const 
    cols 
    ptr
    Spk
$$

$section Assignment$$

$index matrix, assignment$$
$index DoubleMatrix, operator=$$

$table
$bold Prototype$$ $cend
$syntax/DoubleMatrix &/A/ = DoubleMatrix &/B/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Assigns the right hand matrix B to the left hand matrix A,
where $italic A$$ and $italic B$$ have the type of 
$code DoubleMatrix$$.  Note that after an assignment operator 
is called the pointer $italic ptr$$ previously returned by data()
becomes invalid.  Therefore, it is recommended that calling 
data() to return the pointer immediately before it is used.
If there is no enough resource available in the heap, it throws $xref/SpkException//SpkException/$$ 
of which the last $xref/SpkError//SpkError/$$ is set to $code SPK_INSUFFICIENT_MEM_ERR$$.


$head Example$$
If you compile, link and run the following program

$codep

#include "DoubleMatrix.h"
#include "multiply.h"

void main(){

	DoubleMatrix A(2, 2);
	DoubleMatrix B(2, 2);
	DoubleMatrix C;

	double *pA = A.data();
	double *pB = B.data();
	int i;

	// Set A to a matrix:
	// [ 1 3 5 ]
	// [ 2 4 6 ]
	//
	// set B to a matrix:
	// [ 1 ]
	// [ 2 ]
	// [ 3 ]
	//
	for(i=0; i<6; i++)
		pA[i] = i+1;
	for(i=0; i<3; i++)
		pB[i] = i+1;

	// Compute
	// [ 22 ]     [ 1 3 5 ]   [ 1 ]
	//         =            * [ 2 ]
	// [ 28 ]     [ 2 4 6 ]   [ 3 ]
	C = multiply(A, B);
	C.print();

}

$$
the matrix 
$math%
	[ 22 ]
	[ 28 ]
%$$
will be printed.

$end
*/
#pragma warning ( disable : 4786 )

#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <string>
#include <cmath>
#include <cfloat>
#include <iomanip>
#include "DoubleMatrix.h"

using SPK_VA::valarray;

//
// Static members initialization
//
struct Memory
{
	~Memory()
	{
		BlockReturn();
	}
};
static Memory m;

const double DoubleMatrix::RFPK_MAT_LOWER   = -1;
const double DoubleMatrix::RFPK_MAT_UPPER   = -2;

DoubleMatrix::DoubleMatrix()
: rows(0), cols(0), ptr(0), nref(0)
{
}

DoubleMatrix::DoubleMatrix(const char* filename) 
: rows(0), cols(0), ptr(0), nref(0)
{
}

DoubleMatrix::DoubleMatrix(const DoubleMatrix &dmatA)
: rows(dmatA.rows), cols(dmatA.cols), ptr(dmatA.ptr), nref(dmatA.nref)
{
	if(ptr)
	    (*nref)++;
}
DoubleMatrix::DoubleMatrix(int nr, int nc)
: rows(nr), cols(nc), ptr(0), nref(0)
{
    assert(nr>=0);
    assert(nc>=0);
	int n = nr * nc;
	if( n == 0 )
	{
		ptr = 0;
		nref = 0;
	}
	else
		create(n);
}
DoubleMatrix::DoubleMatrix(double a)
: rows(1), cols(1), ptr(0), nref(0)
{
    create(1, a);
}

DoubleMatrix::DoubleMatrix( const valarray<double>& array, int ncols )
: rows(0), cols(0), ptr(0), nref(0)
{
	int arraySize = array.size();
	assert( ncols >= 0 );
	if( arraySize > 0 )
	{
		assert( arraySize % ncols == 0 ); 
        rows = arraySize / ncols;
		create( arraySize, array );
	}
	cols = ncols;
}
// For column vector only
DoubleMatrix& DoubleMatrix::operator=( const valarray<double>& array )
{
	int arraySize = array.size();
    rows = arraySize;

	if(ptr)
	{
		assert( *nref > 0 );
		(*nref)--;
		if( *nref == 0 )
		{
			BlockFree( (void *) ptr);
		}
	}

	if( arraySize == 0 )
	{
		cols = 0;
		ptr = 0;
	}
	else
	{
	    cols = 1;
		create( arraySize, array );
	}

    return *this;
}
DoubleMatrix& DoubleMatrix::fromValarray( const valarray<double>& array, int ncols )
{
    int arraySize = array.size();
	assert( ncols >= 0 );

	if(ptr)
	{
		assert( *nref > 0 );
		(*nref)--;
		if( *nref == 0 )
		{
			BlockFree( (void *) ptr);
		}
	}

	if( arraySize == 0 )
		ptr = 0;
	else
	{
		assert( arraySize % ncols == 0 ); 
        rows = arraySize / ncols;
		create( arraySize, array );
	}
	cols = ncols;

    return *this;
}
void DoubleMatrix::toValarray( valarray<double>& array )
{
  	int size = rows * cols;

    if(ptr)
	{
		array.resize( size );
        for( int i = 0; i < size; i++ )
            array[ i ] = ptr[ i + 1 ];  
	
		assert( *nref > 0 );
		(*nref)--;
		if( *nref == 0 )
		{
			BlockFree( (void *) ptr);
		}
		ptr = 0;
		rows = 0;
		cols = 0;
	}
	else
    	array.resize( 0 );
}
const valarray<double> DoubleMatrix::toValarray() const
{
  int size = rows * cols;

  valarray<double> array( size );

  for( int i=0; i<size; i++ )
    array[ i ] = ptr[ i + 1 ];

  return array;
}
// Destructors
DoubleMatrix::~DoubleMatrix() throw()
{
	if(ptr)
	{
		assert( ptr[0] == RFPK_MAT_LOWER );
		assert( ptr[rows * cols + 1] == RFPK_MAT_UPPER );
		assert( *nref > 0 );
		(*nref)--;
		if( *nref == 0)
		{
			BlockFree( (void *) ptr);
		}
	}
}

void DoubleMatrix::resize(int nr, int nc)
{
    if( this->rows == nr && this->cols == nc )
        return;

    else if( nr*nc != this->rows*this->cols )
    {
        *this = DoubleMatrix(nr, nc);
    }
    else
    {
        this->rows = nr;
        this->cols = nc;
    }
}
DoubleMatrix &DoubleMatrix::operator=( const DoubleMatrix &dmatB)
{
    rows = dmatB.rows;
	cols = dmatB.cols;

    if( ptr == dmatB.ptr )
		return *this;

	if(ptr)
	{
		assert( *nref > 0 );
		(*nref)--;
		if( *nref == 0 )
		{
			BlockFree( (void *) ptr);
		}
	}
	    
    ptr  = dmatB.ptr;
	nref = dmatB.nref;
	if( ptr )
	{	assert( *nref > 0 );
		(*nref)++;
	}

    return *this;
}


int DoubleMatrix::getRefCount() const
{ 
	if(!ptr)
		return 1;
	return *nref; 
}

void DoubleMatrix::fill( double val )
{
    int     n   = nr() * nc();
    if( n > 0 )
    {
		double *pData = data();
        std::fill(pData, pData+n, val);
    }
}

bool DoubleMatrix::isEmpty() const
{

    if( this->rows == 0 || this->cols == 0 )
        return true;
    else
        return false;
}
void DoubleMatrix:: print() const 
{
    using namespace std;

	int i, j;
	const double *p = this->data();
	if( isEmpty() )
        cout << "[]" << endl;
	else
	{
		for(i = 0; i < this->rows; i++){
			cout << "[ ";
			for(j = 0; j < this->cols-1; j++){
				cout << p[j * this->rows + i] << " ";
			}
			cout << p[this->rows * j + i];
			cout << " ]" << endl;
		}
	}

	return;
}
std::ostream& operator<<(std::ostream& stream, const DoubleMatrix& ob)
{    
    using namespace std;
	stream << setiosflags(ios::scientific) << setprecision(DBL_DIG+1);
	int i, j;

	stream << ob.nr() << " by " << ob.nc() << endl;

	if(ob.nr() * ob.nc() != 0)
	{
		const double *p = ob.data();
		
		for(i = 0; i < ob.nr(); i++){
			stream << "[ ";
			for(j = 0; j < ob.nc()-1; j++){
				stream << p[j * ob.nr() + i] << " ";
			}
			stream << p[ob.nr() * j + i];
			stream << " ]" << endl;
		}
	}
    else
    {
        stream << "[ ]" << endl;
    }
	return stream;
}
std::istream& operator>>(std::istream& stream, DoubleMatrix& ob)
{
    using namespace std;

	DoubleMatrix tmp;
	int nRows, nCols;
	char by[5], openBracket[2], closeBracket[2];
	int i, j;

	stream >> nRows >> by >> nCols;
	if( ob.nr() != nRows || ob.nc() != nCols )
	{
		tmp.resize(nRows,nCols);
		ob = tmp;
	}
	if(ob.nr() * ob.nc() !=0)
	{	
		double *p = ob.data();
	
	
		for(i = 0; i < ob.nr(); i++){
			stream >> openBracket;
			for(j = 0; j < ob.nc()-1; j++){
				stream >> p[j * ob.nr() + i];
			}
			stream >> p[ob.nr() * j + i];
			stream >> closeBracket;
		}
	}
    else
    {
        stream >> openBracket >> closeBracket;
    }
	
	return stream;
}

#include "multiply.h"
const DoubleMatrix operator*(const DoubleMatrix& A, const DoubleMatrix& B)
{
	return multiply(A,B);
}

