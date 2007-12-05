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
 * File: broadCastEndOfSpk.cpp
 *
 *
 * broadCastEndOfSpk() raises a signal notifying all active Nodes of 
 * the termination of SPK.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: broadCastEndOfSpk
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin broadCastEndOfSpk$$
$spell 
   spk const
$$

$section Notifying All Node of the End of Spk$$

$index broadCastEndOfSpk$$
$index parallel, end of SPK session$$

$table
$bold Prototype:$$   $cend  
$syntax/void broadCastEndOfSpk(const File& /sharedDiskSpace/)
/$$
$tend

See also $xref/isEndOfSpk//isEndOfSpk()/$$.
$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code broadCastEndOfSpk$$ broadcasts the end of Spk to all
active Nodes.  A Node can then test whether the signal has been
raised or not by calling $xref/isEndOfSpk//isEndOfSpk()/$$
which returns true if it detects the signal.

$head Arguments$$
$syntax/
const File& /sharedDiskSpace/
/$$
is a $xref/File//File/$$ object that absolutely points to a shared directory used
for Master and Node to communicate.

$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * For Shared Disk Space version of parallel Spk, this is writing a 
 * file of specific name.
 *
 *------------------------------------------------------------------------*/
#include <fstream>
#include "broadCastEndOfSpk.h"
#include "System.h"
#include "File.h"
#include "PARALLEL_FILE_CONSTS.h"

void broadCastEndOfSpk(const File& sharedDiskSpace)
{
    using namespace parallel_const;
    File endFile(sharedDiskSpace.getPath(), SPK_ESPK+"."+SPK_MASTER_SUFFIX);
	std::ofstream ofs( endFile.getFullname().c_str() );
    ofs << true;
    ofs.close();
}
