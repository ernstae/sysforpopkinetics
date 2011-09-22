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

ostringstream WarningsManager::allWarnings;
int           WarningsManager::nWarnings = 0;


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
  bool
  const
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
 * Function: addWarning
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin WarningsManager_addWarning$$
$spell
  const
  bool
$$

$section Add a New Warning to the List of Warnings$$

$index addWarning$$
$cindex \Add \a \New \Warning \to \the \List \of Warnings$$

$table
$bold Prototype:$$   $cend  
$syntax/void WarningsManager::addWarning(
  const string& warningIn, 
  unsigned int  lineNumberIn,
  const char*   fileNameIn )/$$
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

$syntax/

/lineNumberIn/
/$$
This is the line number in the file where the warning message was
generated.

$syntax/

/fileNameIn/
/$$
This is the file where the warning message was generated.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void WarningsManager::addWarning( 
  const string& warningIn, 
  unsigned int  lineNumberIn,
  const char*   fileNameIn )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Add this warning to the list of warnings.
  //------------------------------------------------------------

  allWarnings << "<warning>" << endl;

  allWarnings << "<message>"  << endl;
  allWarnings << warningIn    << endl;
  allWarnings << "</message>" << endl;

  allWarnings << "<file_name>"  << endl;
  allWarnings << fileNameIn     << endl;
  allWarnings << "</file_name>" << endl;

  allWarnings << "<line_number>"  << endl;
  allWarnings << lineNumberIn     << endl;
  allWarnings << "</line_number>" << endl;

  allWarnings << "</warning>" << endl;
  

  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  nWarnings++;
}

/*************************************************************************
 *
 * Function: addWarningList
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin WarningsManager_addWarningList$$
$spell
  const
  bool
$$

$section Add a List of Warnings to the List of Warnings$$

$index addWarning$$
$cindex \Add \a \List \of \Warnings \to \the \List \of Warnings$$

$table
$bold Prototype:$$   $cend  
$syntax/void WarningsManager::addWarningList(
  const string& warningList, 
  unsigned int warnings )/$$
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

Adds a new list of warnings to the list of warnings.

$head Arguments$$
$syntax/

/warningList/
/$$
This string is the new list of warnings to add to the list of warnings.

$syntax/

/warnings/
/$$
This is the number of warnings in the new list of warnings.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void WarningsManager::addWarningList( const std::string& warningList,
                                      unsigned int warnings )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Add this list of warnings to the list of warnings.
  //------------------------------------------------------------

  allWarnings << warningList;
  

  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  nWarnings += warnings;
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
  // Clear the list of warnings and reset the counter.
  allWarnings.str( "" );
  nWarnings = 0;
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
  bool
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
  if ( nWarnings > 0 )
  {
    return true;
  }
  else
  {
    return false;
  }
}


/*************************************************************************
 *
 * Function: getWarningList
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin WarningsManager_getWarningList$$
$spell
$$

$section Get the Current Warning List$$

$index getAllWarnings$$
$cindex \Get \the \Current \Warning \List$$

$table
$bold Prototype:$$   $cend  
$syntax/int WarningsManager::getWarningList( string& /warningList/ )
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

Gets a string that contains all of the warnings messages that have been 
added to the list and the number of warnings.

Returns the number of warnings of the warning list.

$head Arguments$$
$syntax/

/warningList/ 
/$$
This string will be set equal to a list of the warnings.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

int WarningsManager::getWarningList( string& warningList )
{
  // Set the output value.
  warningList = allWarnings.str();

  return nWarnings;
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

Gets an XML formatted string that contains all of the warnings
messages that have been added to the list.

$head Arguments$$
$syntax/

/allWarningsOut/ 
/$$
This string will be set equal to an XML formatted list of all
of the warnings.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void WarningsManager::getAllWarnings( string& allWarningsOut )
{
  using namespace std;

  // Create a temporary output string stream.
  ostringstream allWarningsTemp;

  // Enclose all of the warnings in a pair of XML elements.
  allWarningsTemp << "<warning_list length=\"" << nWarnings << "\">" << endl;
  if ( nWarnings > 0 )
  {
    allWarningsTemp << allWarnings.str();
  }
  allWarningsTemp << "</warning_list>" << endl;

  // Set the output value.
  allWarningsOut = allWarningsTemp.str();
}

