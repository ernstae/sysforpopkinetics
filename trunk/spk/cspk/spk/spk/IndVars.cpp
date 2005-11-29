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
 * File: IndVars.cpp
 *
 *
 * Defines a data structure for values that are specific to an 
 * individual.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Class: IndVars
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/
/*
$begin IndVars$$
$spell 
   ostream istream const os inserter vars ob bool init val ind xxx th int
   inx std var
   Spk
$$

$section Initial values specific to an individual$$

$index IndVars class$$
$index data package, initial individual information$$
$index parallel, data package$$

$table
$bold Prototype:$$   $cend
$syntax/IndVars::()
/$$
$syntax/IndVars::IndVars(const IndVars& /right/)
/$$ 
$syntax/IndVars::IndVars(int /inxToIndividual/, const DoubleMatrix& /initVal/, const DoubleMatrix& /indData/)
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
IndVars class bundles initial values that are specific to an individual.

$head Constructors$$
$syntax/
IndVars::IndVars()
/$$
is the default constructor.  This creates an empty IndVar object.

$syntax/

IndVars::IndVars(const IndVars& /right/)
/$$
is the copy constructor which copies values from $italic right$$ to $code *this$$.

$syntax/

IndVars::IndVars(int /inxToIndividual/, const DoubleMatrix& /initVal/, const DoubleMatrix& /indData/)
/$$
is a constructor which merely puts values passed as arguments into 
internal place holders without any data validation.
$pre

$$
$italic inxToIndividual$$ specifies the index to a specific individual in the
population.
$pre

$$
$italic initVal$$ specifies the initial values for the i-th individual's
parameter analysis.
$pre

$$
$italic indData$$ is a column vector containing the individual's measurement data.

$head Public Members$$
$syntax/
int who() const
/$$
returns the index to an individual.

$syntax/

const DoubleMatrix getIn() const
/$$
returns $italic initVal$$.

$syntax/

const DoubleMatrix getData() const
/$$
return $italic indData$$.


$head Operator Overloads$$
$syntax/
IndVars& operator=(const IndVars& /right/)
/$$
copies the contents of $italic right$$ to $code this$$.  If self-assignment is
attempted, the program may terminate.

$syntax/

bool operator!=(const IndVars& /right/) const
/$$
returns true whether $italic left$$ and $italic right$$ do not exactly match in data.

$syntax/

istream& operator>>(istream& /is/, IndVars& /ob/)
/$$
overloads the extractor.  This spits out values contained in the object
each followed by a specific text title.
$pre

$$
The output format is as following (where each $code xxx$$
corresponds to an actual value).

    index_to_individual
    xxx
    initial_individual_parameter
    xxx
    individual_data
    xxx

$syntax/

ostream& operator<<(ostream& /os/, const IndVars& /ob/)
/$$
overloads the inserter.  This throws an $xref/SpkException//SpkException/$$
if $italic ob$$'s format does not exactly match 
the format specified by the extractor.

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
#include "IndVars.h"
#include "isDmatEpsEqual.h"
#include "DoubleMatrix.h"
#include "SpkException.h"

using namespace std;

IndVars::IndVars() : _who(0), _indIn(0,0), _indData(0,0)
{
}
IndVars::IndVars(const IndVars& right) 
: _indIn(right._indIn), _who(right._who), _indData(right._indData)
{
}
IndVars::~IndVars()
{
}
IndVars::IndVars(int inx, const DoubleMatrix &initVal, const DoubleMatrix &measurements)
: _indIn(initVal), _who(inx), _indData(measurements)
{
}

const DoubleMatrix IndVars::getIn() const
{
    return _indIn;
}
const DoubleMatrix IndVars::getData() const
{
    return _indData;
}
int IndVars::who() const
{
    return _who;
}
IndVars& IndVars::operator=(const IndVars& right)
{
    assert( this != &right );
    _indIn       = right._indIn;
    _who         = right._who;
    _indData     = right._indData;

    return *this;
}
ostream& operator<<(ostream& stream, const IndVars& right)
{
    stream << "index_to_individual" << endl;
    stream << right._who << endl;
    stream << "initial_individual_parameter"   << endl;
    stream << right._indIn << endl;
    stream << "individual_data" << endl;
    stream << right._indData;
    return stream;
}
istream& operator>>(istream& stream, IndVars& right)
{
    char title[40];
    stream >> title;
    if(strcmp(title, "index_to_individual")!=0)
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);
    stream >> right._who;

    stream >> title;
    if(strcmp(title, "initial_individual_parameter")!=0)
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);

    stream >> right._indIn;

    stream >> title;
    if(strcmp(title, "individual_data")!=0)
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);

    stream >> right._indData;
    return stream;
}
bool IndVars::operator!=(const IndVars& right) const
{
    if( _who == right._who ){
        if( isDmatEpsEqual( _indIn, right._indIn, _indIn ) ){
            if( isDmatEpsEqual( _indData, right._indData, _indData ) ){
                return false;
            }
        }
    }
    return true;
}
