/*************************************************************************
 * 
 * From:   Resource Facility for Population Kinetics
 *         Department of Bioengineering Box 352255
 *         University of Washington
 *         Seattle, WA 98195-2255
 *
 * Copyright (C) 2002, University of Washington,
 * Resource Facility for Population Kinetics. All Rights Reserved.
 *
 * This software was developed with support from NIH grant RR-12609.
 * Please cite this grant in any publication for which this software
 * is used and send a notification to the address given above.
 *
 * Check for updates and notices at:
 * http://www.rfpk.washington.edu
 *
 *************************************************************************/

/*************************************************************************
 *
 * File: printInMatrix.h
 *
 *
 * Print out the contents of an array in a matrix format.
 * The function is a template function.  VC++ compiler requires
 * template definitions to be in its header file.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: printInMatrix
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin printInMatrix$$
$spell 
   valarray
   Spk
   const
   cols
   iostream
   std
   namespace
   cout
$$

$section Print Array In Matrix Form$$

$index printInMatrix$$
$index print, print array in matrix form$$

$table
$bold Prototype:$$   $cend  
template <class /T/>
$syntax/void printInMatrix( const SPK_VA::valarray</T/>& /a/, int /nCols/ )/$$
$tend

See also: 
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$

$head Description$$
Print out an array in a matrix format to the standard output.
The array is read in the column-major order.

$head Arguments$$

$syntax/

/T/ (Optional)
/$$
is a data type of the elements in the array, $italic a$$.
The data type must have $code cout$$ defined to print out an element.

$syntax/

/a/
/$$
is an $code SPK_VA::valarray$$ object containing an array of elements
of type $italic T$$.  

$syntax/
/nCols/
/$$  
specifies the number of columns in the resulting matrix.
The value must be greater than zero unless the size of the array is zero.
The size of the array must be divisible by $italic nCols$$.
Otherwise, it will terminate the program.

$head Example$$
If you compile, link, and run the following program:
$codep

   #include <iostream>
   #include "SpkValarray.h"
   #include "printInMatrix.h"

   void main()
   {
      using SPK_VA::valarray;
      using namespace std;

      double data[] = { 1, 2, 3, 4, 5, 6, 7, 8 };
      valarray<double> array( data, 8 );

      printInMatrix( array, 2 );
   }
$$
then it will display the following when it is run:
$codep

[ 1 3 5 7 ]
[ 2 4 6 8 ]

$$
$end
*/

#ifndef PRINT_IN_MATRIX_H
#define PRINT_IN_MATRIX_H
#pragma warning( disable : 4786 )
/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <cassert>
#include "SpkValarray.h"
#include "SpkException.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

#include "SpkValarray.h"

template <class T>
void printInMatrix( const SPK_VA::valarray<T>& array, int nCols, std::ostream &o = std::cout )
{
  using namespace std;

  if( array.size() == 0 )
  {
    o << "[]" << endl;
    return;
  }

  assert( nCols > 0 );
  const int nRows = array.size() / nCols;
  assert( nRows * nCols == array.size() );

  for(int i = 0; i < nRows; i++)
  {
    o << "[ ";
    int j;
    for(j = 0; j < nCols-1; j++)
    {
      o << array[j * nRows + i] << " ";
    }
    o << array[nRows * j + i];
    o << " ]" << endl;
  }

  return;
}


#endif
