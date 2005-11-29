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
 * File: IndResults.cpp
 *
 *
 * Defines a data structure for results from an individual's analysis
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Class: IndResults
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/
/*
$begin IndResults$$
$spell 
   ostream istream const os inserter ind ob bool int xxx std
   ind Lambda2diff logdet
   Spk
$$

$section Results from an individual's analysis$$

$index IndResults class$$
$index data package, individual's analysis results$$
$index parallel, data package$$

$table
$bold Prototype:$$   $cend 
$syntax/IndResults::indResults()
/$$
$syntax/IndResults::indResults(const IndResults& /right/)
/$$ 
$syntax/IndResults::IndResults(
int /index/,
const DoubleMatrix& /indHat/,
const DoubleMatrix& /indTilde/,
const DoubleMatrix& /indTilde_pop/,
double /dLambda/, 
double /dLogdetLambda2diff/)
/$$
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
IndResults class bundles results from an individual's analysis and provides user of
methods to access these data.

$head Constructors$$
$syntax/
indResults::indResults()
/$$
is the default constructor.  This may or may not initialize parameters kept internally.

$syntax/
indResults::indResults(const indResults& /right/)
/$$
is the copy constructor which copies values from $italic right$$ to $code *this$$.

$syntax/

IndResults::IndResults(int /index/,const DoubleMatrix& /indHat/,const DoubleMatrix& /indTilde/,
const DoubleMatrix& /indTilde_pop/,double /indLambda/,double /indLogdetLambda2diff/)
/$$
is a constructor which merely puts values passed as arguments into internal place holders without any data validation.
$pre

$$
$italic index$$ specifies the index to the individual whose parameter has been estimated.
$pre

$$
$italic indHat$$ is an output value corresponding to $math%bHatOut%$$ 
specified in $xref/estimateB//estimateB()/$$ routine.
$pre

$$
$italic indTilde$$ is an output value corresponding to $math%bTildeOut%$$
specified in $xref/estimateB//estimateB()/$$ routine.
$pre

$$
$italic indTilde_pop$$ is an output value corresponding to $math%bTilde_alpOut%$$
specified in $xref/estimateB//estimateB()/$$ routine.
$pre

$$
$italic indLambda$$ is an output value corresponding to $math%Lambda%$$
specified in $xref/lTilde//lTilde()/$$ routine.
$pre

$$
$italic indLogdetLambda2diff$$ is a double precision log determinant of the central difference of 
the central difference of $math%Lambda%$$ with respect to the individual parameter,
with respect to the population parameter.  For details, see $xref/estimateB//estimateB()/$$ routine.


$head Operator Overloads$$
$syntax/
IndResults& operator=(const IndResults& /right/)
/$$
copies the contents of $italic right$$ to $code this$$.  If self-assignment is
attempted, the program may terminate.

$syntax/

bool operator!=(const IndResults& /right/) const
/$$
returns true whether $italic left$$ and $italic right$$ do not exactly match in data.

$syntax/

istream& operator>>(istream& /is/, IndResults& /ob/)
/$$
overloads the extractor.  The output format is as following (where each $code xxx$$
corresponds to an actual value).

$codep
    index
    xxx
    indHat
    xxx
    indTilde
    xxx
    indTilde_pop
    xxx
$$

$syntax/
ostream& operator<<(ostream& /os/, const IndResults& /ob/)
/$$
overloads the inserter.  This throws an $xref/SpkException//SpkException/$$ if 
$italic ob$$ did not follow exactly the same format
used by the extractor.

$head Public Members$$
$syntax/
int getIndex() const
/$$
returns the index to the individual whose parameter has been estimated.

$syntax/

const DoubleMatrix getHat() const
/$$
returns $italic indHat$$.

$syntax/

const DoubleMatrix getTilde() const
/$$
returns $italic indTilde$$.

$syntax/

public const DoubleMatrix getTilde_pop() const
/$$
returns $italic indTilde_pop$$.

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
#include <cfloat>
#include <cassert>
#include "IndResults.h"
#include "isDmatEpsEqual.h"
#include "isDblEpsEqual.h"
#include "DoubleMatrix.h"
#include "SpkException.h"

IndResults::IndResults()
: _index(-1), _indHat(0,0), _indTilde(0,0), _indTilde_pop(0,0),
  _indLambda(0.0), _indLogdetLambda2diff(0.0)
{
}
IndResults::IndResults(const IndResults& right)
: _index(right._index), _indHat(right._indHat), _indTilde(right._indTilde), _indTilde_pop(right._indTilde_pop),
  _indLambda(right._indLambda), _indLogdetLambda2diff(right._indLogdetLambda2diff)
{
}

IndResults::~IndResults()
{
}
IndResults::IndResults(int inx, const DoubleMatrix& bh, const DoubleMatrix& bt, const DoubleMatrix& bt_pop)
: _index(inx), _indHat(bh), _indTilde(bt), _indTilde_pop(bt_pop)
{
    
}
IndResults::IndResults(int inx, const DoubleMatrix& bh, const DoubleMatrix& bt, const DoubleMatrix& bt_pop,
               double dLambda, double dLogdetLambda2diff)
: _index(inx), _indHat(bh), _indTilde(bt), _indTilde_pop(bt_pop), _indLambda(dLambda), _indLogdetLambda2diff(dLogdetLambda2diff)
{
}
int IndResults::getIndex() const
{
    return _index;
}
const DoubleMatrix IndResults::getHat() const
{
    return _indHat;
}
const DoubleMatrix IndResults::getTilde() const
{
    return _indTilde;
}
const DoubleMatrix IndResults::getTilde_pop() const
{
    return _indTilde_pop;
}
const double IndResults::getLambda() const
{
    return _indLambda;
}
const double IndResults::getLogdetLambda2diff() const
{
    return _indLogdetLambda2diff;
}

IndResults& IndResults::operator=(const IndResults& right)
{
    assert(this != &right);
    _index                = right._index;
    _indHat               = right._indHat;
    _indTilde             = right._indTilde;
    _indTilde_pop         = right._indTilde_pop;
    _indLambda            = right._indLambda;
    _indLogdetLambda2diff = right._indLogdetLambda2diff;
    return *this;
}

std::ostream& operator<<(std::ostream& stream, const IndResults& right)
{
    stream << "index"             << std::endl;
    stream << right._index        << std::endl;
    stream << "indHat"            << std::endl;
    stream << right._indHat       << std::endl;
    stream << "indTilde"          << std::endl;
    stream << right._indTilde     << std::endl;
    stream << "indTilde_pop"      << std::endl;
    stream << right._indTilde_pop << std::endl;
    stream << "lambda"            << std::endl;
    stream << right._indLambda    << std::endl;
    stream << "logdetLambda2diff" << std::endl;
    stream << right._indLogdetLambda2diff << std::endl;
    return stream;
}
std::istream& operator>>(std::istream& stream, IndResults& right)
{
    char title[40];
    bool failed = false;

    stream >> title;
    if(strcmp(title, "index" ) == 0)
        stream >> right._index;
    else
        failed = true;

    stream >> title;
    if(strcmp(title, "indHat") == 0)
        stream >> right._indHat;
    else
        failed = true;
    
    stream >> title;
    if(strcmp(title, "indTilde") == 0)
        stream >> right._indTilde;
    else
        failed = true;

    stream >> title;
    if(strcmp(title, "indTilde_pop") == 0)
        stream >> right._indTilde_pop;
    else
        failed = true;

    stream >> title;
    if(strcmp(title, "lambda") == 0 )
        stream >> right._indLambda;
    else
        failed = true;

    stream >> title;
    if(strcmp(title, "logdetLambda2diff") == 0 )
        stream >> right._indLogdetLambda2diff;

    if(failed)
    {
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);
    }

    return stream;
}
bool IndResults::operator!=(const IndResults& right) const
{
    if( _index == right._index ){
        if( isDmatEpsEqual(_indHat, right._indHat, _indHat) ){
            if( isDmatEpsEqual(_indTilde, right._indTilde, _indTilde) ){
                if( isDmatEpsEqual(_indTilde_pop, right._indTilde_pop, _indTilde_pop) ){
                    if( isDblEpsEqual(_indLambda, right._indLambda, _indLambda) ){
                        if( isDblEpsEqual(_indLogdetLambda2diff, right._indLogdetLambda2diff, _indLogdetLambda2diff) ){
                            return false;
                        }
                    }
                }
            }
        }
    }
    return true;   
}
