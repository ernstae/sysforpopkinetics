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
 * File: IndOutputDataPackage.cpp
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
 * struct: IndOutputDataPackage
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Structure Specification
 *------------------------------------------------------------------------*/

/*
$begin IndOutputDataPackage$$
$spell 
   const bool inserter ind vars vals popconst popvars indvars outputdata
   ostream istream endl ie std int itr
   Spk
$$

$section Data Package Sent Back to Master$$

$index IndOutputDataPackage$$
$index data package, sent back to Master$$
$index parallel, data package$$

$table
$bold Prototype:$$   $cend  
$syntax/IndOutputDataPackage::IndOutputDataPackage()
/$$
$syntax/IndOutputDataPackage::IndOutputdataPackage(const IndOutputDataPackage& /right/)
/$$
$syntax/IndOutputDataPackage::IndOutputDataPackage(int /curPopItr/, const IndResults& /results/)
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
A data structure that bundles information associated with an individual's parameter analysis results.
A structure object corresponds to an individual and is sent back from Node to Master.

$head Constructors$$
$syntax/
IndOutputDataPackage::IndOutputDataPackage()
/$$
is the default constructor.  This may or may not initialize parameters kept internally.

$syntax/

IndOutputDataPackage::IndOutputDataPackage(const IndOutputDataPackage& /right/)
/$$
is the copy constructor which copies values from $italic right$$
to $code *this$$.

$syntax/

IndOutputDataPackage::IndOutputDataPackage(int /curPopItr/, const IndResults& /results/)
/$$
is a constructor which merely puts the population level iteration number and the value of 
$xref/IndResults//IndResults/$$ passed as an argument 
into internal place holders without any data validation.

$head Public Methods$$
$syntax/
public bool empty() const
/$$
returns true if the object contains no assigned values (ie. an object
created via the default constructor with no follow-up value assignment). 

$head Public Variables$$
$syntax/
public IndResults /indResults/
/$$
is a place holder for a value of $xref/IndResults//IndResults/$$ object.


$head Operator Overloads$$

$syntax/
public IndOutputDataPackage& operator=(const IndOutputDataPackage& /right/)
/$$
copies the contents of $italic right$$ to $code *this$$.  If self-assignment
is attempted, the program terminates.

$syntax/

public bool operator!=(const IndOutputDataPackage& /right/)
/$$
returns true if $italic popconst$$, $italic popvars$$ and $italic indvars$$ all match between
$italic right$$ and $code *this$$.

$syntax/

public friend istream& operator>>(istream&, IndOutputDataPackage&)
/$$
overloads the extractor.  It spits out a boolean value indicating whether 
$italic results$$ has been given by user or not, followed by the value of
$italic results$$ if given; nothing if not given.

$syntax/

public friend ostream& operator<<(ostream& /stream/, const IndOutputDataPackage& /right/)
/$$
overloads the inserter.  It expects the input to follow the exactly same format used
by the extractor.  If the right data set does not match in the same format used
by IndOutputDataPackage::operator>>, an $xref/SpkException//SpkException/$$ is thrown.

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
#include "IndResults.h"
#include "SpkException.h"

IndOutputDataPackage::IndOutputDataPackage()
: popItr(-1)
{
    isEmpty = true;
    index   = -1;
}
IndOutputDataPackage::IndOutputDataPackage(const IndOutputDataPackage& right)
: popItr(right.popItr), indResults(right.indResults)
{
    isEmpty = right.isEmpty;
    index   = right.index;
}
IndOutputDataPackage::IndOutputDataPackage(int curPopItr, const IndResults& results)
: popItr(curPopItr), indResults(results)
{
    isEmpty = false;
    index   = indResults.getIndex();
}
IndOutputDataPackage::~IndOutputDataPackage()
{
}
bool IndOutputDataPackage::empty() const
{
    return isEmpty;
}

IndOutputDataPackage& IndOutputDataPackage::operator=(const IndOutputDataPackage& right)
{
    assert( &right != this );
    isEmpty    = right.isEmpty;
    popItr     = right.popItr;
    indResults = right.indResults;
    index      = right.index;
    return *this;
}
bool IndOutputDataPackage::operator!=(const IndOutputDataPackage& right) const
{
    if( isEmpty != right.isEmpty || popItr != right.popItr || indResults != right.indResults || index != right.index )
        return true;
    return false;
}
std::ostream& operator<<(std::ostream& stream, const IndOutputDataPackage& right)
{
  using namespace std;

#ifdef _DEBUG
    if( right.isEmpty )
    {
        std::cerr << "Warning! IndOutputDatapackage::operator<< received an empty rhs object!" << std::endl;
    }
#endif
    stream << setiosflags(ios::scientific) << setprecision(DBL_DIG+1);
    stream << "isEmpty"     << std::endl;
    stream << right.isEmpty << std::endl;
    if( !right.isEmpty )
    {
        stream << "popItr"     << std::endl;
        stream << right.popItr << std::endl;
        stream << right.indResults;
    }
    stream << resetiosflags(ios::scientific);
    return stream;
}
std::istream& operator>>(std::istream& stream, IndOutputDataPackage& right)
{
  using namespace std;
    string title;
    stream >> title;
    if( title != "isEmpty")
    {
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect output data format", __LINE__, __FILE__);
    }
    stream >> right.isEmpty;
    if( !right.isEmpty ){
        stream >> title;
        if( title != "popItr" )
        {
            throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect output data format", __LINE__, __FILE__);
        }
        stream >> right.popItr;
        stream >> right.indResults;
        right.index = right.indResults.getIndex();
    }
    return stream;
}
