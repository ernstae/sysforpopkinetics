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
 * File: IndInputDataPackage.cpp
 *
 *
 * A data structure for bundling PopConstVals, PopVars and IndVars 
 * that are sent to Node as input data set.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * struct: IndInputDataPackage
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Structure Specification
 *------------------------------------------------------------------------*/

/*
$begin IndInputDataPackage$$
$spell 
   const bool inserter ind vars vals popconst popvars indvars
   ostream istream endl ie int itr
$$

$section Input Data Package Sent to Node$$

$index IndInputDataPackage$$
$index data package, sent to Node$$
$index parallel, data package$$

$table
$bold Prototype:$$   $cend  
$syntax/IndInputDataPackage::IndInputDataPackage()
/$$
$syntax/IndInputDataPackage::IndInputDataPackage(const IndInputDataPackage& /right/)
/$$
$syntax/IndInputDataPackage::IndInputDataPackage(
const PopConstVals& /a/, const PopVars& /b/, const IndVars& /c/)/$$
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
A data structure that bundles information necessary to initiate an individual parameter analysis.
A structure corresponds to an individual and sent to Node.

$head Constructors$$
$syntax/
IndInputDataPackage::IndInputDataPackage()
/$$
is the default constructor.  This may or may not initialize parameters kept internally.

$syntax/

IndInputDataPackage::IndInputDataPackage(const IndInputDataPackage& /right/)
/$$
is the copy constructor.  This returns a copy of $italic right$$ as a separate object.
No data validation is attempted.

$syntax/

IndInputDataPackage::IndInputDataPackage
(int /curPopItr/, const PopConstVals& /a/, const PopVars& /b/, const IndVars& /c/)
/$$
is a constructor that takes information to be sent to Node.
This does not validate data; instead merely accepts and keep them internally.
$italic curPopItr$$ should indicate the iteration number of the population level analysis.
  For remaining arguments, see: 
$xref/PopConstVals//PopConstVals/$$, $xref/PopVars//PopVars/$$, $xref/IndVars//IndVars/$$.


$head Public Methods$$
$syntax/
bool empty() const
/$$
returns true if the object contains no assigned data (ie. an object
constructed through the default constructor followed by no
follow-up value assignment).

$head Public Variables$$
$syntax/
int /popItr/
/$$
is the iteration number of the population level analysis.
$syntax/

PopConstVals /popConstVals/
/$$
is a place holder for a value of $xref/PopConstVals//PopConstVals/$$ object.

$syntax/
PopVars /popVars/
/$$
is a place holder for a value of $xref/PopVars//PopVars/$$ object.

$syntax/
IndVars /indVars/
/$$
is a place holder for a value of $xref/IndVars//IndVars/$$ object.


$head Operator Overloads$$

$syntax/
IndInputDataPackage& operator=(const IndInputDataPackage& /right/)
/$$
copies the contents of $italic right$$ to $code *this$$.  If self-assignment is
attempted, the program terminates.

$syntax/

bool operator!=(const IndInputDataPackage& /right/)
/$$
returns true if $italic popconst$$, $italic popvars$$ and $italic indvars$$ all match between
$italic right$$ and $code *this$$.

$syntax/

friend istream& operator>>(istream&, IndInputDataPackage&)
/$$
overloads the extractor.  It spits out a boolean value indicating whether
$italic popConstVals$$, 
$italic popVars$$ and $italic indVars$$ have been given by user,
followed by these three values if has been indeed given; nothing if not.

$syntax/
friend ostream& operator<<(ostream& /stream/, const IndInputDataPackage& /right/)
/$$
overloads the inserter.  It expects the input to follow exactly the same format
used by the extractor.  If not, the program terminates.

$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * <INFORMATION_REQUIRED_TO_UNDERSTAND_THE_FUNCTION_IMPLEMENTATION>.
 *
 *------------------------------------------------------------------------*/
 /*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#pragma warning( disable : 4786 )
#include <iostream>
#include <iomanip>
#include <cfloat>
#include <cassert>
#include "IndDataPackage.h"
#include "PopConstVals.h"
#include "PopVars.h"
#include "IndVars.h"
#include "SpkException.h"

IndInputDataPackage::IndInputDataPackage() 
: popItr(-1)
{
    isEmpty = true;
    index   = -1;
}
IndInputDataPackage::~IndInputDataPackage(){
}
IndInputDataPackage::IndInputDataPackage(const IndInputDataPackage& right)
: popItr(right.popItr), popConstVals(right.popConstVals), popVars(right.popVars), indVars(right.indVars)
{
    isEmpty      = right.isEmpty;
    index        = right.index;
}
IndInputDataPackage::IndInputDataPackage(
   int curPopItr, const PopConstVals& popconst, const PopVars& popvars, const IndVars& indvars)
: popItr(curPopItr), popConstVals(popconst), popVars(popvars), indVars(indvars)
{
    isEmpty      = false;
    index        = indVars.who();
}
bool IndInputDataPackage::empty() const
{
    return isEmpty;
}
IndInputDataPackage& IndInputDataPackage::operator=(const IndInputDataPackage& right){
    
    isEmpty      = right.isEmpty;
    popItr       = right.popItr;
    index        = right.index;
    popConstVals = right.popConstVals;
    popVars      = right.popVars;
    indVars      = right.indVars;
    return *this;
}
bool IndInputDataPackage::operator!=(const IndInputDataPackage& right) const
{
    if( isEmpty == right.isEmpty ){
        if( popItr == right.popItr ){
            if( index == right.index ){
                if( !(popConstVals != right.popConstVals) ){
                    if( !(popVars != right.popVars) ){
                        if( !(indVars != right.indVars) ){
                            return false;
                        }
                    }
                }
            }
        }
    }
    return true;

}
std::ostream& operator<<(std::ostream& stream, const IndInputDataPackage& right)
{
  using namespace std;

    string isEmptyTitle("isEmpty");
    stream << setiosflags(ios::scientific) << setprecision(DBL_DIG+1);

    stream << "isEmpty"     << std::endl;
    stream << right.isEmpty << std::endl;
    if( !right.isEmpty )
    {
        stream << "popItr"           << std::endl;
        stream << right.popItr       << std::endl;
        stream << right.popConstVals << std::endl;
        stream << right.popVars      << std::endl;
        stream << right.indVars      << std::endl;
    }

    stream << resetiosflags(ios::scientific);
    return stream;
}
std::istream& operator>>(std::istream& stream, IndInputDataPackage& right)
{
  using namespace std;
  
    string title;
    stream >> title;
    if( title != "isEmpty" )
    {
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect input data format", __LINE__, __FILE__);
    }
    else
    {
        stream >> right.isEmpty;
    }
    if( !right.isEmpty ){
        stream >> title;
        if( title != "popItr" )
        {
            throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect input data format", __LINE__, __FILE__);
        }
        stream >> right.popItr >> right.popConstVals >> right.popVars >> right.indVars;
    }
    right.index = right.indVars.who();
    return stream;
}
