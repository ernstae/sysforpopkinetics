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
 * Definition of PI
 *
 * Sachiko Honda
 */

/*
$begin pi$$
$spell 
    iostream
    namespace
    std
    cout
    endl
    const
    bool
$$

$section PI constant$$

$index PI$$
$index constant, PI$$

$table
$bold Header:$$   $cend  
pi.h $rend
$bold Definition:$$ $cend
$syntax/const double PI = 3.1415926535897932384626433832795028841971693993751/$$
$tend

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
PI is a double-precision value defined as an approximation of $math%pi%$$. 

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "PI.h"

    bool testPI()
    {
        using namespace std;
        cout << "PI = " << pi << endl;
        return true;
    }

$$
then it will display the following when it is run:
$codep

PI = 3.14159

$$
$end
*/


#ifndef PI_H
#define PI_H

const double PI = 3.1415926535897932384626433832795028841971693993751;

#endif
