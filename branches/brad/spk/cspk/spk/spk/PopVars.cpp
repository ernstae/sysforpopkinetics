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
 * File: PopVars.cpp
 *
 *
 * Defines a data structure for values that vary during a population
 * analysis.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Class: PopVars
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/
/*
$begin PopVars$$
$spell 
   ostream istream const os inserter vars ob bool extracter xxx std
   rs 
   Spk
$$

$section Values that vary during a population analysis$$

$index PopVars class$$
$index data package, population level variables$$
$index parallel, data package$$

$table
$bold Prototype:$$   $cend  
$syntax/PopVars::PopVars()
/$$
$syntax/PopVars::PopVars(const PopVars& /right/)
/$$
$syntax/PopVars::PopVars(const DoubleMatrix& /pop/, bool /isHat/, bool /isTilde/, bool /isTilde_pop/)
/$$
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
PopVars class bundles values that vary during a population and provide user with methods
to access these data.

$head Constructors$$
$syntax/
PopVars::PopVars()
/$$
is the default constructor.  This may or may not initialize parameters kept internally.

$syntax/

PopVars::PopVars(const PopVars& /right/)
/$$
is the copy constructor which copies values from $italic right$$ to $code *this$$.

$syntax/

PopVars::PopVars(const DoubleMatrix& /pop/, bool /isHat/, bool /isTilde/, bool /isTilde_pop/)
/$$
is a constructor which merely puts values passed as arguments 
into internal place holders without any data validation.
$pre

$$
$italic pop$$ specifies the current population parameter.
$pre

$$
$italic isHat$$ requests $math%indHat%$$ to be computed.
$pre

$$
$italic isTilde$$ requests $math%indTilde%$$ to be computed.
$pre

$$
$italic isTilde_pop$$ requests $math%indTilde_pop%$$ to be computed.

$head Public Members$$
$syntax/
const DoubleMatrix getPop() const
/$$
returns $italic pop$$.

$syntax/

bool isHat() const
/$$
returns $italic isHat$$.

$syntax/

bool isTilde() const
/$$
returns $italic isTilde$$.

$syntax/

bool isTilde_pop() const
/$$

$head Operator Overloads$$
$syntax/
PopVars& operator=(const PopVars& /right/)
/$$
copies the contents of $italic right$$ to $code this$$.  If self-assignment is
attempted, the program may terminate.

$syntax/

bool operator!=(const PopVars& /left/, const PopVars& /right/) const
/$$
returns true whether $italic left$$ and $italic right$$ do not exactly match in data.
$syntax/

istream& operator>>(istream& /is/, PopVars& /ob/)
/$$
overloads the extractor.  The format of the output value is as followings (where $code xxx$$
corresponds to an actual value).

    Population_parameter
    xxx
    isHat
    x
    isTilde
    x
    isTilde_pop
    x
$syntax/

public ostream& operator<<(ostream& /os/, const PopVars& /ob/)
/$$
overloads the inserter.  This rs a $xref/SpkException//SpkException/$$ if
$italic ob$$ did not follow exactly the same format used
by this class's extracter.

$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Nothing particular.
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <cassert>
#include "PopVars.h"
#include "isDmatEpsEqual.h"
#include "DoubleMatrix.h"
#include "SpkException.h"

using namespace std;
/*------------------------------------------------------------------------
 * Member definition
 *------------------------------------------------------------------------*/
PopVars::PopVars()
: _pop(0,0), _isIndHat(true), _isIndTilde(true), _isIndTilde_pop(true)
{
}
PopVars::PopVars(const PopVars& right)
: _pop(right._pop), _isIndHat(right._isIndHat), _isIndTilde(right._isIndTilde), _isIndTilde_pop(right._isIndTilde_pop)
{
}
PopVars::PopVars(const DoubleMatrix& p, bool hat, bool tilde, bool tilde_pop)
: _pop(p), _isIndHat(hat), _isIndTilde(tilde), _isIndTilde_pop(tilde_pop)
{
}
PopVars::~PopVars()
{
}
const DoubleMatrix PopVars::getPop() const
{
    return _pop;
}
bool PopVars::isHat() const
{
    return _isIndHat;
}
bool PopVars::isTilde() const
{
    return _isIndTilde;
}
bool PopVars::isTilde_pop() const
{
    return _isIndTilde_pop;
}
bool PopVars::operator!=(const PopVars& right) const
{
    if( (_isIndHat != right._isIndHat) 
        || (_isIndTilde != right._isIndTilde) 
        || (_isIndTilde_pop != right._isIndTilde_pop) )
        return false;
        
    if( !isDmatEpsEqual(this->_pop, right._pop, this->_pop) )
        return true;
    return false;
}

PopVars& PopVars::operator=(const PopVars& right)
{
    assert( this != &right );
    _pop            = right._pop;
    _isIndHat       = right._isIndHat;
    _isIndTilde     = right._isIndTilde;
    _isIndTilde_pop = right._isIndTilde_pop;
    return *this;
}
ostream& operator<<(ostream& stream, const PopVars& right)
{
    stream << "Population_parameter" << endl;
    stream << right._pop << endl;
    stream << "isHat" << endl;
    stream << right._isIndHat << endl;
    stream << "isTilde" << endl;
    stream << right._isIndTilde << endl;
    stream << "isTilde_pop" << endl;
    stream << right._isIndTilde_pop;
    return stream;
}
#include <iostream>
#include <string>
istream& operator>>(istream& stream, PopVars& right)
{
	using namespace std;
    char title[50];

    stream >> title;
    if( strcmp(title, "Population_parameter") != 0 )
	{
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);
	}
    stream >> right._pop;

    stream >> title;
    if( strcmp(title, "isHat") != 0 )
	{
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);
	}

    stream >> right._isIndHat;

    stream >> title;
    if( strcmp(title, "isTilde") != 0 )
	{
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);
	}
    stream >> right._isIndTilde;

    stream >> title;
    if( strcmp(title, "isTilde_pop") != 0 )
	{
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);
	}
    stream >> right._isIndTilde_pop;

    return stream;
}
