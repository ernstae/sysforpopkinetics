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
 * File: FpErrorChecker.cpp
 *
 *
 * Tool to detect floating point errors without relying on the
 * asynchronous trap mechanism.
 *
 * This tool set may depend on platform specific routines.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Class: FpErrorChecker
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin FpErrorChecker$$
$spell 
    Checker checker
    Spk 
    Fp 
    const 
    denormalization 
    cpp 
    iostream 
    errorcode 
    namespace 
    linenum
    std
    instanciated
$$

$section Floating Point Error Detection$$

$index FpErrorChecker$$
$index error, floating point error detection$$
$index exception, floating point error$$

$table
$bold Header: $$ $cend
FpErrorChecker.h $rend

$bold Public Interfaces:$$   $cend  
$syntax/FpErrorChecker::FpErrorChecker()/$$ $rend
$cend
$syntax/FpErrorChecker::~FpErrorChecker()/$$ $rend
$cend
$syntax/static unsigned int FpErrorChecker::check()/$$ $rend
$cend
$syntax/static unsigned int FpErrorChecker::clear()/$$ $rend
$cend
$syntax/static void FpErrorChecker::print()/$$ $rend
$tend

See also: $xref/SpkError//SpkError/$$, $xref/SpkException//SpkException/$$
$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This class provides tools to detect floating point errors.
When this class is instanciated, it clears the state of the $bold universal$$ floating point error flags and
set the $bold universal$$ floating-point control word to be able to detect a certain set of floating point errors.
As the object goes out of scope, the error flags are cleared and the control word is restored as before.

$head Constructors$$
$syntax/
FpErrorChecker()
/$$
is the default constructor.  It clears the state of the universal floating point error flags
and reset the universal floating-point control word to detect the following floating point errors:
$pre

$$
$table
Overflow $cend
Tripping the highest bit in mantissa.
$rend
Invalid operation $cend
Attempting an arithmetic operation on an invalid floating-point number such as NAN.
$rend
Denormalization loss  $cend
Attempting representing a too small number in mantissa.
$rend
Divide by zero  $cend
Divide a number by zero.
$rend
Inexact result  $cend
Disabled (truncate to exact zero): Too small or too large to represent in a given precision.
$rend
Underflow $cend
Disabled (truncate to exact zero): Tripping the lowest bit in mantissa.
$rend

$tend

$syntax/

~FpErrorChecker()
/$$
is the destructor which clears the state of the universal floating point error flags and
restores the universal floating point control word as before.

$head Public Methods$$

$syntax/
static unsigned int check(unsigned int line, const char* filename)
/$$
determines whether floating point errors have occurred since the universal error flags were cleared most
recently.  If errors were detected, it will throw an object of $xref/SpkException//SpkException/$$ that
may contain one or more $xref/SpkError//SpkError/$$ errors with $italic line$$ and $italic filename$$
as a location at which the error causing operation appeared. Otherwise, it returns the current state of
the flags.

$pre

$$
The following table summarizes the correspondence between errors and Spk Error Codes (of type of $code SpkError::ErrorCode$$):
$table
Overflow 
$cend
SPK_FP_OVERFLOW_ERR 
$rend
Invalid operation 
$cend
SPK_FP_INVALID_ERR
$rend
Denormalization loss 
$cend
SPK_FP_DENORMAL_ERR
$rend
Divide by zero 
$cend
SPK_FP_ZERODIVIDE_ERR
$rend
(Disabled, truncate to exact zero) Inexact representation
$cend
SPK_FP_INEXACT_ERR
$rend
(Disabled, truncate to exact zero) Underflow 
$cend
SPK_FP_UNDERFLOW_ERR 
$rend

$tend
$syntax/

static unsigned int clear()
/$$
clears the universal floating point error flags and returns the state before the action.

$syntax/

static void print()
/$$
prints out the current status of the error flags in hexadecimal notation.


$head Example$$
If you compile, link, and run the following program named as $code X:\Temp\Honda\FpErrorChecker\main.cpp$$,
$codep

  #include <iostream>
  #include "FpErrorChecker.h"
  #include "SpkException.h"

  using namespace std;
  int main()
  {
       double a = 1e-40, b;
       float  y;

       FpErrorChecker checkerON;

       // Since a is too small to be presented exactly, 
       // the following assignment triggers "Inexact" and "Underflow" errors.
       try{
           y = a;
           FpErrorChecker::check(__LINE__, __FILE__);
       }
       catch( const SpkException& e )
       {
           e.print();
           return -1;
       }
       return 0;
        
  }
$$
then it will display the following when it is run:
$codep

    count
    1
    errorcode
    26
    linenum
    15
    filename
    X:\Temp\Honda\FpErrorChecker\main.cpp
    message
    Underflow

$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Currently this class does not restore the state of the floating-point
 * status word before an instanciation.  VC++ does not provide 
 * a tool for restoration.  If the feature should be required, 
 * then we might to code up in assembly.  - Sachiko
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <cfloat>
#include <cstdio>
#include <cassert>
#include "SpkException.h"
#include "FpErrorChecker.h"


// PORTABLILITY ISSUE
// THESE BIT FIELDS ARE NON-POSIX, NON-ANSI.  THEY ARE PROBABLY 8087 SPECIFIC.
const unsigned int _EM_INVALID    = 1;
const unsigned int _EM_DENORMAL   = 2;
const unsigned int _EM_ZERODIVIDE = 4;
const unsigned int _EM_OVERFLOW   = 8;
const unsigned int _EM_UNDERFLOW  = 16;
const unsigned int _EM_INEXACT    = 32;

const unsigned int FpErrorChecker::_default_control
= _EM_DENORMAL | _EM_OVERFLOW | _EM_ZERODIVIDE | _EM_INEXACT | _EM_UNDERFLOW | _EM_INVALID ;

const unsigned int FpErrorChecker::_default_mask
= FpErrorChecker::_default_control;

int FpErrorChecker::_cnt = 0;
unsigned int FpErrorChecker::_preserved_control = 0;
unsigned int FpErrorChecker::_preserved_status  = 0;

FpErrorChecker::FpErrorChecker()
{
    //
    // Preserve the current status word
    // and clear the status (to 0000x)
    //
    _preserved_status  = SpkClear();

    //
    // Preserve the current control word
    //
    _preserved_control = SpkControl(0,0);

    //
    // Make sure the control word is set to trap arithmatic errors.
    //
    SpkControl(_default_control, _default_mask);

    ++_cnt;
}
FpErrorChecker::~FpErrorChecker()
{
    SpkClear();
    SpkControl(_preserved_control, _default_mask );
    --_cnt;
}
void FpErrorChecker::print()
{
    printf( "Status = %.8i\n", SpkStatus() );
}

unsigned int FpErrorChecker::check(unsigned int line, const char* filename)
{

    unsigned int status = SpkStatus();
    SpkClear();
    if( status == 0 || status & _EM_INEXACT || status & _EM_UNDERFLOW )
        return status;


    //
    // Checking for floating point interrupt exceptions
    //
    SpkException e;
    if( status & _EM_ZERODIVIDE )
    {
        e.push(SpkError::SPK_FP_ZERODIVIDE_ERR, "Divide by zero", line, filename);
    }
    if( status & _EM_INVALID )
    {
        e.push(SpkError::SPK_FP_INVALID_ERR, "Invalid operation", line, filename);
    }
    if( status & _EM_OVERFLOW )
    {
        e.push(SpkError::SPK_FP_OVERFLOW_ERR, "Overflow", line, filename);
    }

    if( status & _EM_DENORMAL )
    {
        e.push(SpkError::SPK_FP_DENORMAL_ERR, "Denormalization loss", line, filename);
    }

    //
    // These two errors are often inevitable and least harmful.
    // Until the significance of these errors are detected, let's ignore.
    //
    /*
    if( status & _EM_INEXACT )
    {
        throw e.push(SpkError::SPK_FP_INEXACT_ERR, "Inexact representation", line, filename);
    }
    if( status & _EM_UNDERFLOW )
    {
        throw e.push(SpkError::SPK_FP_UNDERFLOW_ERR, "Underflow", line, filename);
    }      
    */
 
    if( e.size() > 0 )
        throw e;

    return status;
}

unsigned int FpErrorChecker::clear()
{
    return SpkClear();
}
