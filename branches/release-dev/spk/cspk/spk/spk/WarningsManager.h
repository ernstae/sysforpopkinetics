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
 * File: WarningsManager.h
 *
 *
 * This class maintains a list of warning messages that is shared
 * by all instances of WarningsManager objects.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef WARNINGSMANAGER_H
#define WARNINGSMANAGER_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// Standard include files.
#include <sstream>
#include <string>

class WarningsManager
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

 public:
  WarningsManager();


  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

 public:
  static void addWarning( 
    const std::string& warningIn, 
    unsigned int       lineNumberIn,
    const char*        fileNameIn );

  static void addWarningList( const std::string& warningList,
                              unsigned int warnings );
  static void clearAllWarnings();


  //------------------------------------------------------------
  // Helper functions.
  //------------------------------------------------------------

  static bool anyWarnings();
  static int getWarningList( std::string& warningList );
  static void getAllWarnings( std::string& allWarningsOut );
  

  //------------------------------------------------------------
  // Static members.
  //------------------------------------------------------------

private:
  // These are declared static so that there is only a single
  // version of each of them that is shared by all instances 
  // of WarningsManager objects.
  static std::ostringstream allWarnings;
  static int                nWarnings;


  //------------------------------------------------------------
  // Disallowed, implicitly generated member functions.
  //------------------------------------------------------------

private:
  // These functions should not be called, so they are not
  // implemented in WarningsManager.cpp.
  WarningsManager( const WarningsManager& right );
  WarningsManager& operator=( const WarningsManager& right );
};



#endif
