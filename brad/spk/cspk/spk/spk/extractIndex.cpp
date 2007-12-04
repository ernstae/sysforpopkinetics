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
 * File: extractIndex.cpp
 *
 *
 * Extracts an index from a given filename (with or without file extension).
 * created by Master or Node during parallel computation.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: extractIndex
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin extractIndex$$
$spell 
   const int pre val ext std
   Spk
$$

$section Extract index (to an individual) from filename$$

$index extractIndex$$
$index parallel, extract index from filename$$

$table
$bold Prototype:$$   $cend  
$syntax/int extractIndex(const string& /prefix/, const string& /filename/)
/$$
$syntax/int extractIndex(const string& /prefix/, const char* /filename/)
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
extractIndex extracts an individual's index from $italic filename$$, 
assuming the index is proceeded by $italic prefix$$.
For example, the following two filenames are
valid, given $italic prefix$$ = $code pre_$$:

$codep
pre_032.ext
pre_312
$$

If $italic filename$$ does not contain $italic prefix$$, a $xref/SpkException//SpkException/$$ will
be thrown.

If $italic prefix$$ proceeds no index, then a negative number is returned.

$head Arguments$$

$syntax/
const string& /prefix/
/$$
is a string proceeding an index within the $italic filename$$.  
$italic prefix$$ must begin with an alphabet and cannot contain a white space;
otherwise, a $xref/SpkException//SpkException/$$ will be thrown.

$syntax/

const string& /filename/
/$$
or
$syntax/
const char* /filename/
/$$
is a string from which an index will be extracted.
$italic filename$$ must begin with an alphabet and cannot contain a white space.
The index contained within $italic filename$$ must be followed by nothing or
a period proceeding file extension.  Some examples are, $code pre_032$$, $code pre_$$,
$code pre_.ext$$ and $code pre_032.ext$$, given $italic prefix$$ = $code pre_$$.
Otherwise, a $xref/SpkException//SpkException/$$ will be thrown.

$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Nothing particular
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#pragma warning( disable : 4786 )

#include <iostream>
#include <string>
#include <cstdlib>
#include <cassert>
#include "extractIndex.h"
#include "SpkException.h"

using namespace std;

int extractIndex(const string& prefix, const string& filename)
{
    if( prefix.size() == 0 || prefix.find(" ") != string::npos )
    {
        string mess = "The filename prefix, ";
        mess += prefix;
        mess += ", may not contain a white space or may not have the length of zero.";
        throw SpkException(SpkError::SPK_PARALLEL_ERR, mess.c_str(), __LINE__, __FILE__);
    }
    if( filename.size() == 0 || filename.find(" ") != string::npos )
    {
        string mess = "The filename, ";
        mess += filename;
        mess += ", may not contain a white space or may not have the length of zero.";

        throw SpkException(SpkError::SPK_PARALLEL_ERR, mess.c_str(), __LINE__, __FILE__);
    }

    int prefix_begin_pos = filename.find(prefix);
    if( prefix_begin_pos == string::npos )
    {
        string mess = "extractIndex(string prefix, string filename) ---";
        mess += "<" + filename + ">";
        mess += " does not begin with ";
        mess += "<" + prefix + ">" + " prefix.";
        throw SpkException(SpkError::SPK_PARALLEL_ERR, mess.c_str(), __LINE__, __FILE__);
    }
    int prefix_end_pos   = prefix_begin_pos + prefix.size();

    int index_begin_pos  = prefix_end_pos;

    int index_end_pos;
    if( filename.find(".") == string::npos )
    {
        // assume the filename does not have a file extension part.
        index_end_pos = filename.size() - 1;
    }
    else
    {
        index_end_pos = filename.find(".") - 1;
    }

    int inxSize = index_end_pos - index_begin_pos + 1;
    if( inxSize <= 0 )
        return -1;

    char *strInx = new char[inxSize+1];
    strcpy(strInx, (filename.substr(index_begin_pos, inxSize)).c_str());

    int inx = atoi(strInx);

    delete [] strInx;
    return inx;
}
int extractIndex(const string& prefix, const char *filename)
{
    string strFilename(filename);
    return extractIndex(prefix, strFilename);
}
