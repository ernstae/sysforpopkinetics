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
 * File: DirbasedParallelControls.h
 *
 *
 * An encapsulation of the directory based parallel controls.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/



#ifndef DIRBASEDPARALLELCONTROLS_H
#define DIRBASEDPARALLELCONTROLS_H

#include "ParallelControls.h"

class DirBasedParallelControls : public ParallelControls
{
public:
	// Constructor
    DirBasedParallelControls( bool IsParallel, 
		                      const char* SharedDirectory, 
							  const char* CoNodeCommand );

	// Destructor
    virtual ~DirBasedParallelControls();

	// Copy constructor
    DirBasedParallelControls( const DirBasedParallelControls& right );

	// Assignment operator
    DirBasedParallelControls& operator=( const DirBasedParallelControls& right );

	// Getters
	inline const char* getSharedDirectory() const
    {
      return sharedDirectory; 
    }

	inline const char* getCoNodeCommand() const
    { 
      return coNodeCommand; 
    }

private:
    const char* sharedDirectory;
	const char* coNodeCommand;
};
         
#endif
