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
 * File: WarningsManager.cpp
 *
 *
 * This class maintains a list of warning messages that is shared
 * by all instances of WarningsManager objects.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK include files.
#include "WarningsManager.h"

// Standard include files.
#include <sstream>
#include <string>

using std::ostringstream;
using std::string;


/*------------------------------------------------------------------------
 * Static variable initializations
 *------------------------------------------------------------------------*/

bool          WarningsManager::areThereAnyWarnings = false;
ostringstream WarningsManager::allWarnings;


/*************************************************************************
 *
 * Function: WarningsManager (Constructor)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin WarningsManager_constructor$$
$spell 
$$

$section Construct a Shared List of Warning Messages$$

$index WarningsManager$$
$cindex \Construct \a \Shared \List \of Warning Messages$$

$table
$bold Prototype:$$   $cend  
$syntax/protected WarningsManager::WarningsManager()/$$
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$

$head Description$$

The constructor for this class.  

This class maintains a list of warning messages that is declared
static so that there is only a single version that is shared by all
instances of WarningsManager objects.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

WarningsManager::WarningsManager()
{
}


/*************************************************************************
 *
 * Function: anyWarnings
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin WarningsManager_anyWarnings$$
$spell
$$

$section Determine if there are Currently Any Warnings$$

$index anyWarnings$$
$cindex \Determine \if \there \are \Currently \Any Warnings$$

$table
$bold Prototype:$$   $cend  
$syntax/bool WarningsManager::anyWarnings()
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

Returns true if there are currently any warnings in the list.
Otherwise, returns false.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

bool WarningsManager::anyWarnings()
{
  return areThereAnyWarnings;
}


/*************************************************************************
 *
 * Function: getAllWarnings
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin WarningsManager_getAllWarnings$$
$spell
$$

$section Get the Current List of Warnings$$

$index getAllWarnings$$
$cindex \Get \the \Current \List \of Warnings$$

$table
$bold Prototype:$$   $cend  
$syntax/void WarningsManager::getAllWarnings( string& /allWarningsOut/ )
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

Gets all of the warnings messages that have been added to the list.

$head Arguments$$
$syntax/

/allWarningsOut/ 
/$$
This string will be set equal to the list of all warnings.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void WarningsManager::getAllWarnings( string& allWarningsOut )
{
  allWarningsOut = allWarnings.str();
}


/*************************************************************************
 *
 * Function: addWarning
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin WarningsManager_addWarning$$
$spell
$$

$section Add a New Warning to the List of Warnings$$

$index addWarning$$
$cindex \Add \a \New \Warning \to \the \List \of Warnings$$

$table
$bold Prototype:$$   $cend  
$syntax/void WarningsManager::addWarning( string& /warningIn/ )
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

Adds a new warning to the list of warnings.

$head Arguments$$
$syntax/

/warningIn/
/$$
This string is the warning message to add to the list of warnings.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void WarningsManager::addWarning( string& warningIn )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  const string line = "************************************************************";


  //------------------------------------------------------------
  // Add this warning to the list of warnings.
  //------------------------------------------------------------

  if ( !areThereAnyWarnings )
  {
    allWarnings << line << endl;
  }
  allWarnings << "Warning:  " << warningIn << endl;
  allWarnings << line << endl;
  

  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  areThereAnyWarnings = true;
}


/*************************************************************************
 *
 * Function: clearAllWarnings
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin WarningsManager_clearAllWarnings$$
$spell
$$

$section Remove All of the Warnings from the List$$

$index clearAllWarnings$$
$cindex \Remove \All \of \the \Warnings \from \the List$$

$table
$bold Prototype:$$   $cend  
$syntax/void WarningsManager::clearAllWarnings()
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

Removes all of the warnings from the list.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void WarningsManager::clearAllWarnings()
{
  // Delete the list of warnings and reset the flag.
  areThereAnyWarnings = false;
  allWarnings.flush();
}
