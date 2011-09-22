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
/*************************************************************************
 *
 * File: SpkValarray.h
 *
 *
 * Declares a specific valarray, slice and gslice depending on
 * a pre-defined macro specified by the user.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Specification
 *------------------------------------------------------------------------*/

/*
$begin SpkValarray$$
$spell 
  Spk
  valarray
  bb
  std
  dsp
  typedef
  namespace
  iostream
  const
  ed
  endl
  cout
  iarray
  ostream
  setiosflags
  setprecision
  cfloat
  iomanip
  ios
$$

$section SpkValarray.h: Central Location to Declare valarray$$

$index valarray, which valarray to use$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code SpkValarray.h$$ is the central place to control which implementation of
$code valarray$$ to use throughout the Spk library.  In the header file, a namespace
that imports a particular implementation of $code valarray$$ is typedef-ed to
$bold SPK_VA$$.  
$pre

$$
Spk library implementors should be able to switch between different implementations
of $code valarray$$ by defining a specific macro at the compilation time and the
chosen $code valarray$$ must be thoroughly used throughout the library.
$pre

$$
By default, $bold std$$ is typedef-ed to $bold SPK_VA$$.
$pre

$$
In this header is also a function defined:
$syntax/
      template <class T>
      std::ostream& operator<<( std::ostream& stream, const SPK_VA::valarray<T>& a )
/$$
This print function displays the contents of the array $italic a$$ enclosed by
square brackets, separated by a comma in between.  i.e.:
$codep
      { a[0], a[1], a[2], ... a[n] }
$$

Currently, $bold BB::valarray$$ is supported as an alternative to std::valarray.  
To use BB::valarray, define $code BB_VALARRAY$$ macro at the compilation time, for example.
A preprocessor macro can be defined in a project (.dsp) settings dialog box or
by directly adding to a compiler option list.

$bold From Visual Studio$$
$list number$$
From the main menu, go Project -> Settings....  "Project Settings" dialog box will appear.

$lnext
Click on a project to build from the list in the left pane.
$pre

$$

$lnext
Click the C++ tab in the right pane.
$pre

$$

$lnext
Choose General from the Category field at the top of the tab if not chosen yet.
$pre

$$

$lnext
Add "BB_VALARRAY" to the list of macros in the Preprocessor Definitions field, 
if you are going to switch to BB::valarray.
$pre

$$

$lnext
Click OK.  The change will take in effect next time you compile (full or incremental) the project.
$pre

$$

$lend

$bold From a command line$$
$list number$$
Add "/D BB_VALARRAY" (without the double quotes) to the compiler option when you compile & link
the project.

$lend

$head Example$$
If you compile and link the following program,

  $codep
    #include <iostream>
    #include <iomanip>
    #include <cfloat>
    #include "SpkValarray.h"
    
    int main()
    {
        using namespace std;

        const int size = 3;

        //
        // You may also say, "using SPK_VA" at the top of this main() body and use valarray
        // without specifying SPK_VA:: on every incident.
        //
        SPK_VA::valarray<int> iArray( size );
        iArray[0] = 1;
        iArray[1] = 2;
        iarray[2] = 3;

        cout << setiosflags(std::ios::scientific) << setprecision(DBL_DIG+1);
        cout << iArray << endl;
        return 0;
    }
  $$

it will display:

  $codep
  {  0.0000000000000000e+000, 0.0000000000000000e+000, 0.0000000000000000e+000 }

  $$

$end
*/

#ifndef SPK_VALARRAY_H
#define SPK_VALARRAY_H
#pragma warning( disable : 4786 )

  #ifdef BB_VALARRAY
     #include "Bb/valarray.h"
     #define SPK_VA std
  #else
     #include <valarray>
     #define SPK_VA std
  #endif


#include <iostream>

template <class T>
std::ostream& operator<<( std::ostream& stream, const SPK_VA::valarray<T>& a )
{    
  using namespace std;

  int i;
  stream << "{ ";
  for( i = 0; i<a.size()-1; i++ )
    stream << a[i] << ", ";
  stream << a[i] << " }";
  return stream;
}
#include "printInMatrix.h"
#endif
