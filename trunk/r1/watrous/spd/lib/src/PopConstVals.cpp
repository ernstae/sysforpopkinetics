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
 * File: PopConstVals.cpp
 *
 *
 * Define the data structure that bundles values that are constant
 * throughout the lifetime of a population analysis.
 *
 * Author: Sachiko
 *
 *************************************************************************/

/*************************************************************************
 *
 * Class: PopConstVals
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin PopConstVals$$
$spell 
   const vals int max ind bool itrs ostream istream os insertor
   mitr Dincluded popsize ie xxx std
   Spk
   ltilde
   Optimizer
$$

$section Values Constant During a Population Analysis$$

$index PopConstVals$$
$index data package, population level constant values$$
$index parallel, data package$$

$table
$bold Prototype:$$   $cend  
$syntax/PopConstVals::PopConstVals()
/$$
$syntax/PopConstVals::PopConstVals(const PopConstVals& /right/)
/$$
$syntax/PopConstVals::PopConstVals(int /popsize/, Optimizer& /optimizer/, 
    const DoubleMatrix& /indLow/, const DoubleMatrix& /indUp/, const DoubleMatrix& /indStep/, bool /includeD/)
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
PopConstVals class bundles a set of values that are constant throughout the lifetime of a population
analysis and provides user with basic operations to access these data.

$head Constructors$$
$syntax/
PopConstVals::PopConstVals()
/$$
is the default constructor.  This may or may not initialize parameters kept internally.

$syntax/

PopConstVals::PopConstVals(const PopConstVals& /right/)
/$$
is the copy constructor which copies values from $italic right$$ to $code *this$$.

$syntax/

PopConstVals::PopConstVals(int /popsize/, Optimizer /optimizer/, int /objective/,
    const DoubleMatrix& /indLow/, const DoubleMatrix& /indUp/, const DoubleMatrix& /indStep/, 
    bool /includeD/)
/$$
is a constructor which merely puts values passed as arguments into internal place holders without
any data validation.

$head Constructor Arguments$$
$syntax/int /popsize/
/$$
specifies the size of a population (ie. the number of individuals in a population).
$pre

$$
$syntax/Optimizer /optimizer/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer used in the individual 
level optimization.
$pre

$$
$syntax/int /objective/
/$$
corresponds to $italic objective$$ specified in $xref/lTilde//lTilde()/$$ routine.
$pre

$$$syntax/const DoubleMatrix& /indLow/
/$$
corresponds to $italic bLow$$ specified in $xref/lTilde//lTilde()/$$ routine.
$pre

$$
$syntax/const DoubleMatrix& /indUp/
/$$
corresponds to $italic bUp$$ specified in $xref/lTilde//lTilde()/$$ routine.
$pre

$$
$syntax/const DoubleMatrix& /indStep/
/$$
corresponds to $italic bStep$$ specified in $xref/lTilde//lTilde()/$$ routine.

$pre

$$


$head Public Members$$

$syntax/
int getSize() const
/$$
returns the number of individuals in this population.

$syntax/

Optimizer getOptimizer() const
/$$
returns $italic optimizer$$.

$syntax/

int getObjective() const
/$$
returns $italic objective$$.

$syntax/

const DoubleMatrix getLow() const
/$$
returns $italic indLow$$.

$syntax/

const DoubleMatrix getUp() const
/$$
returns $italic indUp$$.

$syntax/

const DoubleMatrix getStep() const
/$$
returns $italic indStep$$.

$head Operator Overloads$$
$syntax/
PopConstVals& operator=(const PopConstVals& /right/)
/$$
copies the contents of $italic right$$ to $code this$$.  If self-assignment is
attempted, the program terminates.

$syntax/

bool operator!=(const PopConstVals& /right/) const
/$$
returns true if $italic *this$$ and $italic right$$ do not exactly match in data values.

$syntax/

friend ostream& operator<<(ostream& /os/, const PopConstVals& /right/)
/$$
overloads the extractor.  The format of the output value is as following 
(where each $code xxx$$ corresponds to an actual value).
$codep

    size
    xxx
    mitr
    xxx
    epsilon
    xxx
    level
    xxx
    includeD
    xxx
    low
    xxx
    up
    xxx
    step
    xxx
$$

$syntax/
friend istream& operator>>(istream& /os/, PopConstVals& /right/)
/$$
overloads the insertor.  This throws a $xref/SpkException//SpkException/$$ if $italic right$$ did not
follow exactly the same as format used
by the extractor.  

$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Chose *class* as opposed to *struct* for potential flexibility in
 * the future and for data protection.
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <cassert>
#include "PopConstVals.h"
#include "isDmatEpsEqual.h"
#include "DoubleMatrix.h"
#include "SpkException.h"

using namespace std;

static Optimizer defaultOpt;

PopConstVals::PopConstVals()
: _size(-1), _optimizer(defaultOpt), _objective(-1), _indLow(0,0), _indUp(0,0), _indStep(0,0)
{
}
PopConstVals::PopConstVals(int n, Optimizer& opt, int obj,
    const DoubleMatrix& low, const DoubleMatrix& up, const DoubleMatrix& step)
: _size(n), _optimizer(opt), _objective(obj), _indLow(low), _indUp(up), _indStep(step)
{
}
PopConstVals::PopConstVals(const PopConstVals& right)
: _size(right._size), _optimizer(right._optimizer), _objective(right._objective),
  _indLow(right._indLow), _indUp(right._indUp), _indStep(right._indStep)
{
}
PopConstVals::~PopConstVals()
{
}
PopConstVals& PopConstVals::operator=(const PopConstVals& right)
{
    assert(this != &right);
    _size      = right._size;
    _optimizer = right._optimizer;
    _objective = right._objective;
    _indLow    = right._indLow;
    _indUp     = right._indUp;
    _indStep   = right._indStep;
    
    return *this;
}
ostream& operator<<(ostream& stream, const PopConstVals& right)
{
    stream << "size"                               << endl;
    stream << right._size                          << endl;
    stream << right._optimizer                     << endl;
    stream << "objective"                          << endl;
    stream << right._objective                     << endl;
    stream << "low"                                << endl;
    stream << right._indLow;
    stream << "up"                                 << endl;
    stream << right._indUp;
    stream << "step"                               << endl;
    stream << right._indStep;

    return stream;
}
istream& operator>>(istream& stream, PopConstVals& right)
{
    char title[100];
    stream >> title >> right._size;
    if( strcmp(title, "size") != 0 )
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);
 
	stream >> right._optimizer;

    stream >> title >> right._objective;
    if( strcmp(title, "objective") != 0 )
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);

    stream >> title >> right._indLow;
    if( strcmp(title, "low") != 0 )
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);

    stream >> title >> right._indUp;
    if( strcmp(title, "up") != 0 )
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);

    stream >> title >> right._indStep;
    if( strcmp(title, "step") != 0 )
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);

    return stream;
}
bool PopConstVals::operator!=(const PopConstVals& right) const
{
    if( _objective == right._objective )
        if( isDmatEpsEqual(_indLow, right._indLow, _indLow) )
            if( isDmatEpsEqual(_indUp, right._indUp, _indUp) )
                if( isDmatEpsEqual(_indStep, right._indStep, _indStep) )
                    return false;
    return true;
}
int PopConstVals::getSize() const
{
    return _size;
}
Optimizer& PopConstVals::getOptimizer() const
{
    return _optimizer;
}
void PopConstVals::setOptimizer( const Optimizer optimizer )
{
    _optimizer = optimizer;
}
const DoubleMatrix PopConstVals::getLow() const
{
    return _indLow;
}
const DoubleMatrix PopConstVals::getUp() const
{
    return _indUp;
}
const DoubleMatrix PopConstVals::getStep() const
{
    return _indStep;
}
int PopConstVals::getObjective() const
{
    return _objective;
}
