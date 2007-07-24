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
 * File: gval.cpp
 *
 *
 * Evaluates g(a, b) = (exp(a) - exp(b)) / (a - b) for each element pair
 * in the input matrices A and B.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: gval
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin gval$$
$spell exp
  gval
  const
  dmat
  pdmat
  da
  db
  dg
  dvec
  cout
  endl
  cstdlib
  iostream
  cmath
  int
  namespace
  std
  valarray
$$

$section Element-wise Evaluation of g(a, b) = (exp(a) - exp(b)) / (a - b) $$

$index gval$$
$cindex Element-wise \Evaluation \of g(a\,\ \b) \= \(exp(a) \- \exp(b)) \/ \(a \- \b)$$

$table
$bold Prototype:$$   $cend  
$syntax/void gval( const DoubleMatrix&    /dmatA/, 
           const DoubleMatrix&    /dmatB/, 
           const DoubleMatrix&    /dmatDA/, 
           const DoubleMatrix&    /dmatDB/, 
           DoubleMatrix*          /pdmatGOut/, 
           DoubleMatrix*          /pdmatDGOut/ )
/$$
$tend

$bold See also: $$ $xref/gvalVA//valarray version/$$.

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

Given $math%m%$$ by $math%n%$$ matrices $math%A%$$ and $math%B%$$, 
this function computes another $math%m%$$ by $math%n%$$ matrix 
$italic G$$ that results from evaluating 
$math%

    g(a, b) = (exp(a) - exp(b)) / (a - b)

%$$
for every pair of elements in $math%A%$$ and $math%B%$$.
$pre

$$
To be specific, for all integers $math%i%$$ and $math%j%$$ such that 
$math%m > i \ge 0%$$ and $math%n > j \ge 0%$$, 
$math%

    G(i,j)  =  g( A(i,j), B(i,j) )  .
  
%$$
Note that in the limit as one of the elements of $math%A%$$ or 
$math%B%$$ goes to negative infinity, its exponential goes to zero. 
Furthermore, as $math%(a - b)%$$ goes to zero, $math%g(a, b)%$$ 
converges to one.

$head Arguments$$
$syntax/
/dmatA/
/$$
The $code DoubleMatrix$$ $italic dmatA$$ contains the matrix 
$math%A%$$.  
All elements of $italic dmatA$$ must be less than or equal to zero.

$syntax/

/dmatB/
/$$
The $code DoubleMatrix$$ $italic dmatB$$ contains the matrix 
$math%B%$$. 
It must have the same dimensions as $math%A%$$, 
and all of its elements must be less than or equal to zero.

$syntax/

/dmatDA/
/$$
The $code DoubleMatrix$$ $italic dmatDA$$ contains the matrix 
$math%dA%$$, which is the $xref/glossary/Differential/differential/$$ 
in the $math%A%$$ direction.
It must have the same dimensions as $math%A%$$.

$syntax/

/dmatDB/
/$$
The $code DoubleMatrix$$ $italic dmatDB$$ contains the matrix 
$math%dB%$$, which is the $xref/glossary/Differential/differential/$$ 
in the $math%B%$$ direction.
It must have the same dimensions as $math%A%$$.

$syntax/

/pdmatGOut/
/$$
The $code DoubleMatrix$$ pointed to by $italic pdmatGOut$$ 
is the $xref/glossary/Output Value/output value/$$ for 
the matrix $math%G%$$, 
which is the element-wise evaluation of $math%g(a, b)%$$.  
It must have the same dimensions as $math%A%$$.

$syntax/

/pdmatDGOut/
/$$
The $code DoubleMatrix$$ pointed to by $italic pdmatDGOut$$ 
is the $xref/glossary/Output Value/output value/$$ for 
the matrix $math%dG%$$, 
which is the element-wise evaluation of $math%dg(a, b)%$$, 
the $xref/glossary/Differential/differential/$$ of 
$math%g(a, b)%$$ with respect to $math%a%$$ and $math%b%$$.
It must have the same dimensions as $math%A%$$.

$head Example$$

$escape #$$

Suppose that
$math%

           /       \            /     \
    A   =  |  - 1  |  ,  B   =  |  0  |  ,
           \       /            \     /

%$$
and
$math%
           /       \             /     \
    DA  =  |    1  |  ,   DB  =  |  0  |  .
           \       /             \     /

%$$
Then
$math%

           /                     \ 
    G   =  |  - ( #exp(-1) - 1 )  |  ,
           \                     / 

%$$
and
$math%

           /                  \ 
    DG  =  |  2 #exp(-1) +  1  |  .
           \                  / 

%$$


If you compile, link, and run the following program:
$codep

//--------------------------------------------------------------
// Include files
//--------------------------------------------------------------

#include <cstdlib>
#include <iostream>
#include <cmath>
#include <spk/DoubleMatrix.h>
#include "gval.h"


//--------------------------------------------------------------
//
// Function: main
//
//--------------------------------------------------------------

int main()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;
  

  //------------------------------------------------------------
  // Compute g(a, b) and dg(a, b).
  //------------------------------------------------------------
  
  DoubleMatrix dvecA    ( 1, 1 );
  DoubleMatrix dvecDA   ( 1, 1 );
  DoubleMatrix dvecB    ( 1, 1 );
  DoubleMatrix dvecDB   ( 1, 1 );
  DoubleMatrix dvecGOut ( 1, 1 );
  DoubleMatrix dvecDGOut( 1, 1 );

  double* a     = dvecA .data();
  double* da    = dvecDA.data();
  double* b     = dvecB .data();
  double* db    = dvecDB.data();
  double* gOut  = dvecGOut .data();
  double* dgOut = dvecDGOut.data();

  dvecA.fill( -1.0 );
  dvecB.fill(  0.0 );

  // Set the differentials, da(1) and db(1), so that 
  //
  //                d 
  //     dg(1)  =  -----  g(a, b)  .
  //               da(1)
  //
  dvecDA.fill( 1.0 );
  dvecDB.fill( 0.0 );

  gval( dvecA, dvecB, dvecDA, dvecDB, &dvecGOut, &dvecDGOut );


  //------------------------------------------------------------
  // Compare g(a, b) and dg(a, b) to known values.
  //------------------------------------------------------------
  
  double gKnown  = ( exp(a[0]) - exp(b[0]) ) / ( a[0] - b[0] );
  double dgKnown = ( exp(a[0]) - gKnown    ) / ( a[0] - b[0] );

  cout << "Computed g(a[0], b[0]) = " << gOut[0] << endl;
  cout << "Known    g(a[0], b[0]) = " << gKnown  << endl;
  cout << endl;
  cout << "Computed dg(a[0], b[0]) = " << dgOut[0] << endl;
  cout << "Known    dg(a[0], b[0]) = " << dgKnown  << endl;
  cout << endl;

  return EXIT_SUCCESS;
}

$$
then it will display the following when it is run:
$codep

Computed g(a[0], b[0]) = 0.632121
Known    g(a[0], b[0]) = 0.632121

Computed dg(a[0], b[0]) = 0.264241
Known    dg(a[0], b[0]) = 0.264241

$$

$end
*/

/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * None.
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// Standard include files.
#include <cassert>
#include <cmath>
#include <limits>

// SPK include files.
#include "../../../../spk/DoubleMatrix.h"
#include "../../../../spk/matmax.h"

// POP3CM include files.
#include "gval.h"


/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  double expWithoutUnderflow( double a );

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void gval( const DoubleMatrix&    dmatA, 
           const DoubleMatrix&    dmatB, 
           const DoubleMatrix&    dmatDA, 
           const DoubleMatrix&    dmatDB, 
           DoubleMatrix*          pdmatGOut, 
           DoubleMatrix*          pdmatDGOut ) 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Return if there are no output values to compute.
  if ( pdmatGOut == 0 && pdmatDGOut == 0 )
  {
    return;
  }

  int nARows = dmatA.nr();
  int nACols = dmatA.nc();

  const double* a  = dmatA .data();
  const double* b  = dmatB .data();
  const double* da = dmatDA.data();
  const double* db = dmatDB.data();


  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  assert( dmatB .nr() == nARows );
  assert( dmatDA.nr() == nARows );
  assert( dmatDB.nr() == nARows );

  assert( dmatB .nc() == nACols );
  assert( dmatDA.nc() == nACols );
  assert( dmatDB.nc() == nACols );

  if ( pdmatGOut )
  {
    assert( pdmatGOut ->nr() == nARows );
    assert( pdmatGOut ->nc() == nACols );
  }

  if ( pdmatDGOut )
  {
    assert( pdmatDGOut->nc() == nACols );
    assert( pdmatDGOut->nr() == nARows );
  }

  // All values in A and B must be negative.
  //
  // [ Revisit - Sachiko - 11/21/02 ]
  //
  // This test fails when running DiffEqnModel with the analytical fi() and fi_x().  
  // The fact was not discovered until the debug version was run, which was today.
  // I traced back as far as to a 11/01/02 version in which all code associated
  // with the analytical derivatives were still in the original form 
  // using DoubleMatrix.  The same failure occurs in the original version too.
  //
  assert( matmax( dmatA ) <= 0.0 );
  assert( matmax( dmatB ) <= 0.0 );


  //------------------------------------------------------------
  // Prepare temporary values to hold the output values.
  //------------------------------------------------------------

  DoubleMatrix dmatG ( nARows, nACols );
  DoubleMatrix dmatDG( nARows, nACols );

  double* g  = dmatG .data();
  double* dg = dmatDG.data();


  //------------------------------------------------------------
  //  Evaluate g(a, b) for each element pair in A and B.
  //------------------------------------------------------------

  //************************************************************
  // Strategy: if a[i] < b[i], then express g(a[i], b[i]) as 
  //
  //                                    exp( a[i] - b[i] ) - 1
  //     g(a[i], b[i])  =  exp(b[i]) * ------------------------
  //                                         a[i] - b[i]
  //       
  // or if a[i] >= b[i], then express g(a[i], b[i]) as 
  //
  //                                    exp( b[i] - a[i] ) - 1 
  //     g(a[i], b[i])  =  exp(a[i]) * ------------------------  .
  //                                         b[i] - a[i]
  //
  //************************************************************

  bool altb, small;
  double c, dc, expa, expb, expc, dexpa, dexpb, dexpc, exact, dexact, apx, dapx, term, dterm;
  for ( int i = 0; i < nARows * nACols; i++ )
  {
    altb = a[i] < b[i];

    // Set c to be the negative of the absolute difference  
    // of these two elements.

      // 
      // Comment by Sachiko, 03/13/2002
      // 
      // The following block has been modified so that they short cut
      // unnecessary (multiply) calculations though it has introduced 
      // if-then-else statements.
      // 
      c  = ( altb? (a[i]  - b[i] ) : (b[i]  - a[i] ) );
      dc = ( altb? (da[i] - db[i]) : (db[i] - da[i]) );

    // Compute the exponentials.  Note that c and the elements 
    // of A and B are all less than or equal to zero, and thus
    // an exponential function that avoids underflow is used.
    expa  = expWithoutUnderflow( a[i] );
    expb  = expWithoutUnderflow( b[i] );
    expc  = expWithoutUnderflow( c    );
    dexpa = expa * da[i];
    dexpb = expb * db[i];
    dexpc = expc * dc;

    // If c is small enough, then its exponential will be approximated.
    small = fabs( c ) < 1.0e-3;

    // This is the exact expression for (exp(c) - 1) / c.  It is only
    // used when c is not small, since it is not correct for small c.
    exact  = ( expc - 1.0 ) / ( c + small );
    dexact = dexpc / ( c + small) - exact * dc / ( c + small );

    // The approximate expression for (exp(c) - 1) / c is
    //
    //                                           2        3     
    //                       ~         c        c        c
    //     (exp(c) - 1) / c  =  1  +  ----  +  ----  +  ----  .
    //                                 2        6        24
    //
      // 
      // Comment by Sachiko, 03/13/2002
      // 
      // The following block has been modified so that they short cut
      // unnecessary (multiply) calculations though it has introduced 
      // if-then-else statements.
      // 
      apx  = ( c==0? 1.0      : c * c * c / 24.0 + c * c / 6.0 + c / 2.0 + 1.0);
      dapx = ( c==0? 0.5 * dc : ( c * c / 8.0 + c / 3.0 + 0.5 ) * dc );

    // Set this equal to the appropriate expression for (exp(c) - 1) / c.
      // 
      // Comment by Sachiko, 03/13/2002
      // 
      // The following block has been modified so that they short cut
      // unnecessary (multiply) calculations though it has introduced 
      // if-then-else statements.
      // 

      term  = ( small? apx  : exact  );
      dterm = ( small? dapx : dexact );

    // Set g( a[i], b[i] ).
      // 
      // Comment by Sachiko, 03/13/2002
      // 
      // The following block has been modified so that they short cut
      // unnecessary (multiply) calculations though it has introduced 
      // if-then-else statements.
      // 
      g[i]  = ( altb?  expb * term                 : expa * term );
      dg[i] = ( altb?  dexpb * term + expb * dterm : dexpa * term + expa * dterm );
  }


  //------------------------------------------------------------
  // Set the return values.
  //------------------------------------------------------------

  if ( pdmatGOut )
  {
    *pdmatGOut  = dmatG;
  }

  if ( pdmatDGOut )
  {
    *pdmatDGOut = dmatDG;
  }


  return;
}

/*************************************************************************
 *
 * File: gval.cpp (valarray version)
 *
 *
 * Evaluates g(a, b) = (exp(a) - exp(b)) / (a - b) for each element pair
 * in the input vectors A and B.
 *
 * Author: Sachiko Honda 
 * based on the DoubleMatarix version written by Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: gval (valarray version)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin gvalVA$$
$spell exp
  gval
  const
  dmat
  pdmat
  da
  db
  dg
  dvec
  cout
  endl
  cstdlib
  iostream
  cmath
  int
  namespace
  std
  Spk
  valarray
$$

$section valarray Version: Element-wise Evaluation of g(a, b) = (exp(a) - exp(b)) / (a - b) $$

$index gvalVA$$
$cindex Element-wise \Evaluation \of g(a\,\ \b) \= \(exp(a) \- \exp(b)) \/ \(a \- \b) - valarray version$$

$table
$bold Prototype:$$   $cend  
$syntax/void gval( 
           const SPK_VA::valarray<double>&    /A/, 
           const SPK_VA::valarray<double>&    /B/, 
           const SPK_VA::valarray<double>&    /dA/, 
           const SPK_VA::valarray<double>&    /dB/, 
           SPK_VA::valarray<double>&          /gOut/, 
           SPK_VA::valarray<double>&          /dGOut/ )
/$$
$tend

$bold See also: $$ $xref/gval//DoubleMatrix version/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

Given $math%n%$$ sized vectors $math%A%$$ and $math%B%$$, 
this function computes another $math%n%$$ vector 
$italic G$$ that results from evaluating 
$math%

    g(a, b) = (exp(a) - exp(b)) / (a - b)

%$$
for every pair of elements in $math%A%$$ and $math%B%$$.
$pre

$$
To be specific, for all integers $math%i%$$ such that 
$math%n > i \ge 0%$$, 
$math%

    G(i,j)  =  g( A(i,j), B(i,j) )  .
  
%$$
Note that in the limit as one of the elements of $math%A%$$ or 
$math%B%$$ goes to negative infinity, its exponential goes to zero. 
Furthermore, as $math%(a - b)%$$ goes to zero, $math%g(a, b)%$$ 
converges to one.

$head Arguments$$
$syntax/
/A/
/$$
The $code SPK_VA::valarray<double>$$ $italic A$$ contains the vector 
$math%A%$$.  
All elements of $italic A$$ must be less than or equal to zero.

$syntax/

/B/
/$$
The $code SPK_VA::valarray<double>$$ $italic B$$ contains the vector 
$math%B%$$. 
It must have the same dimensions as $math%A%$$, 
and all of its elements must be less than or equal to zero.

$syntax/

/dA/
/$$
The $code SPK_VA::valarray<double>$$ $italic dA$$ contains the vector 
$math%dA%$$, which is the $xref/glossary/Differential/differential/$$ 
in the $math%A%$$ direction.
It must have the same length as $math%A%$$.

$syntax/

/dmatDB/
/$$
The $code SPK_VA::valarray<double>$$ $italic dB$$ contains the vector 
$math%dB%$$, which is the $xref/glossary/Differential/differential/$$ 
in the $math%B%$$ direction.
It must have the same length as $math%A%$$.

$syntax/

/gOut/
/$$
The $code SPK_VA::valarray<double>$$ pointed to by $italic gOut$$ 
is the $xref/glossary/Output Value/output value/$$ for 
the vector $math%G%$$, 
which is the element-wise evaluation of $math%g(a, b)%$$.  
It must have the same length as $math%A%$$.

$syntax/

/pdmatDGOut/
/$$
The $code SPK_VA::valarray<double>$$ pointed to by $italic dGOut$$ 
is the $xref/glossary/Output Value/output value/$$ for 
the matrix $math%dG%$$, 
which is the element-wise evaluation of $math%dg(a, b)%$$, 
the $xref/glossary/Differential/differential/$$ of 
$math%g(a, b)%$$ with respect to $math%a%$$ and $math%b%$$.
It must have the same length as $math%A%$$.

$head Example$$

$escape #$$

Suppose that
$math%

           /       \            /     \
    A   =  |  - 1  |  ,  B   =  |  0  |  ,
           \       /            \     /

%$$
and
$math%
           /       \             /     \
    DA  =  |    1  |  ,   DB  =  |  0  |  .
           \       /             \     /

%$$
Then
$math%

           /                     \ 
    G   =  |  - ( #exp(-1) - 1 )  |  ,
           \                     / 

%$$
and
$math%

           /                  \ 
    DG  =  |  2 #exp(-1) +  1  |  .
           \                  / 

%$$


If you compile, link, and run the following program:
$codep

//--------------------------------------------------------------
// Include files
//--------------------------------------------------------------

#include <cstdlib>
#include <iostream>
#include <cmath>
#include "SpkValarray.h"
#include "gval.h"


//--------------------------------------------------------------
//
// Function: main
//
//--------------------------------------------------------------

int main()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using SPK_VA::valarray;
  using namespace std;

  //------------------------------------------------------------
  // Compute g(a, b) and dg(a, b).
  //------------------------------------------------------------
  
  valarray<double> a    ( -1.0, 1 );
  valarray<double> b    (  0.0, 1 );
  valarray<double> gOut ( 1 );
  valarray<double> dGOut( 1 );


  // Set the differentials, da(1) and db(1), so that 
  //
  //                d 
  //     dg(1)  =  -----  g(a, b)  .
  //               da(1)
  //
  valarray<double> da   ( 1.0, 1 );
  valarray<double> db   ( 0.0, 1 );

  gval( a, b, da, db, gOut, dGOut );


  //------------------------------------------------------------
  // Compare g(a, b) and dg(a, b) to known values.
  //------------------------------------------------------------
  
  double gKnown  = ( exp(a[0]) - exp(b[0]) ) / ( a[0] - b[0] );
  double dgKnown = ( exp(a[0]) - gKnown    ) / ( a[0] - b[0] );

  cout << "Computed g(a[0], b[0]) = " << gOut[0] << endl;
  cout << "Known    g(a[0], b[0]) = " << gKnown  << endl;
  cout << endl;
  cout << "Computed dg(a[0], b[0]) = " << dgOut[0] << endl;
  cout << "Known    dg(a[0], b[0]) = " << dgKnown  << endl;
  cout << endl;

  return EXIT_SUCCESS;
}

$$
then it will display the following when it is run:
$codep

Computed g(a[0], b[0]) = 0.632121
Known    g(a[0], b[0]) = 0.632121

Computed dg(a[0], b[0]) = 0.264241
Known    dg(a[0], b[0]) = 0.264241

$$

$end
*/
#include <spk/SpkValarray.h>

void gval( const SPK_VA::valarray<double>&    a, 
           const SPK_VA::valarray<double>&    b, 
           const SPK_VA::valarray<double>&    da, 
           const SPK_VA::valarray<double>&    db, 
           SPK_VA::valarray<double>&          GOut, 
           SPK_VA::valarray<double>&          dGOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Return if there are no output values to compute.
  if ( GOut.size() == 0 && dGOut.size() == 0 )
  {
    return;
  }

  const int n = a.size();
  int i;

  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  assert( b .size() == n );
  assert( da.size() == n );
  assert( db.size() == n );

  if ( GOut.size() > 0 )
  {
    assert( GOut.size() == n );
  }

  if ( dGOut.size() > 0 )
  {
    assert( dGOut.size() == n );
  }

  // All values in A and B must be negative.
#ifdef _DEBUG
  //
  // [ Revisit - Sachiko - 11/21/02 ]
  //
  // This test fails when running DiffEqnModel with the analytical fi() and fi_x().  
  // The fact was not discovered until the debug version was run, which was today.
  // I traced back as far as to a 11/01/02 version in which all code associated
  // with the analytical derivatives were still in the original form 
  // using DoubleMatrix.  The same failure occurs in the original version too.
  //
  for( i=0; i<n; i++ )
  {
    assert( a[i] <= 0.0 );
    assert( b[i] <= 0.0 );
  }
#endif

  //------------------------------------------------------------
  // Prepare temporary values to hold the output values.
  //------------------------------------------------------------

  valarray<double> g ( n );
  valarray<double> dg( n );

  //------------------------------------------------------------
  //  Evaluate g(a, b) for each element pair in A and B.
  //------------------------------------------------------------

  //************************************************************
  // Strategy: if a[i] < b[i], then express g(a[i], b[i]) as 
  //
  //                                    exp( a[i] - b[i] ) - 1
  //     g(a[i], b[i])  =  exp(b[i]) * ------------------------
  //                                         a[i] - b[i]
  //       
  // or if a[i] >= b[i], then express g(a[i], b[i]) as 
  //
  //                                    exp( b[i] - a[i] ) - 1 
  //     g(a[i], b[i])  =  exp(a[i]) * ------------------------  .
  //                                         b[i] - a[i]
  //
  //************************************************************

  bool altb, small;
  double c, dc, expa, expb, expc, dexpa, dexpb, dexpc, exact, dexact, apx, dapx, term, dterm;
  for ( i = 0; i < n; i++ )
  {
    altb = a[i] < b[i];

    // Set c to be the negative of the absolute difference  
    // of these two elements.

      // 
      // Comment by Sachiko, 03/13/2002
      // 
      // The following block has been modified so that they short cut
      // unnecessary (multiply) calculations though it has introduced 
      // if-then-else statements.
      // 
      c  = ( altb? (a[i]  - b[i] ) : (b[i]  - a[i] ) );
      dc = ( altb? (da[i] - db[i]) : (db[i] - da[i]) );

    // Compute the exponentials.  Note that c and the elements 
    // of A and B are all less than or equal to zero, and thus
    // an exponential function that avoids underflow is used.
    expa  = expWithoutUnderflow( a[i] );
    expb  = expWithoutUnderflow( b[i] );
    expc  = expWithoutUnderflow( c    );
    dexpa = expa * da[i];
    dexpb = expb * db[i];
    dexpc = expc * dc;

    // If c is small enough, then its exponential will be approximated.
    small = fabs( c ) < 1.0e-3;

    // This is the exact expression for (exp(c) - 1) / c.  It is only
    // used when c is not small, since it is not correct for small c.
    exact  = ( expc - 1.0 ) / ( c + small );
    dexact = dexpc / ( c + small) - exact * dc / ( c + small );

    // The approximate expression for (exp(c) - 1) / c is
    //
    //                                           2        3     
    //                       ~         c        c        c
    //     (exp(c) - 1) / c  =  1  +  ----  +  ----  +  ----  .
    //                                 2        6        24
    //
      // 
      // Comment by Sachiko, 03/13/2002
      // 
      // The following block has been modified so that they short cut
      // unnecessary (multiply) calculations though it has introduced 
      // if-then-else statements.
      // 
      apx  = ( c==0? 1.0      : c * c * c / 24.0 + c * c / 6.0 + c / 2.0 + 1.0);
      dapx = ( c==0? 0.5 * dc : ( c * c / 8.0 + c / 3.0 + 0.5 ) * dc );

    // Set this equal to the appropriate expression for (exp(c) - 1) / c.
      // 
      // Comment by Sachiko, 03/13/2002
      // 
      // The following block has been modified so that they short cut
      // unnecessary (multiply) calculations though it has introduced 
      // if-then-else statements.
      // 

      term  = ( small? apx  : exact  );
      dterm = ( small? dapx : dexact );

    // Set g( a[i], b[i] ).
      // 
      // Comment by Sachiko, 03/13/2002
      // 
      // The following block has been modified so that they short cut
      // unnecessary (multiply) calculations though it has introduced 
      // if-then-else statements.
      // 
      g[i]  = ( altb?  expb * term                 : expa * term );
      dg[i] = ( altb?  dexpb * term + expb * dterm : dexpa * term + expa * dterm );
  }


  //------------------------------------------------------------
  // Set the return values.
  //------------------------------------------------------------

  if ( GOut.size() > 0 )
  {
    GOut  = g;
  }

  if ( dGOut.size() > 0 )
  {
    dGOut = dg;
  }

  return;
}
/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

namespace // [Begin: unnamed namespace]
{


/*************************************************************************
 *
 * Function: expWithoutUnderflow
 *
 * 
 * Returns exp(a).  If the value will be less than the smallest 
 * double value min, i.e., if underflow will occur, then exp(a) 
 * is set equal to min.
 *
 *************************************************************************/

const double minLog = log( std::numeric_limits<double>::min() );

inline double expWithoutUnderflow( double a )
{
  // Check to see that the exponential will not result 
  // in an underflow error.
  bool ok = a >= minLog;
  
      // 
      // Comment by Sachiko, 03/13/2002
      // 
      // The following block has been modified so that they short cut
      // unnecessary (multiply) calculations though it has introduced 
      // if-then-else statements.
      // 
      double y = (ok? a : minLog );
  
  return exp( y );
}
  

} // [End: unnamed namespace]
