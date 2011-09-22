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
 * File: DirbasedParallelControls.cpp
 *
 *
 * An encapsulation of the directory based parallel controls.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/


/*************************************************************************
 *
 * Class: DirBasedParallelControls
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*

-------------------------------------------------------------
   Constructor 
-------------------------------------------------------------
$begin DirBasedParallelControls Constructor $$

$spell 
const 
int 
Dir 
bool 
initializes$$

$section Constructor$$

$index DirBasedParallelControls, constructor$$

$table
$bold Prototype:$$ $cend
$syntax/DirBasedParallelControls::DirBasedParallelControls /A/(  
                                                        bool /IsParallel/,            
                                                        char* /SharedDirectory/,          
				                                        char* /coNodeCommand/)/$$                
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
This constructor takes three arguments to initializes the member data, 
$italic sharedDirectory$$, $italic nodeCommand$$ and the member data inherited 
from the base class, $italic isParallel$$.

$head Argument$$

$syntax/

/SharedDirectory/
/$$
A directory on SPK Parallel Network that has been set to be $bold shared$$ by all
Participants and where Participants have both read and write access.  The string
can be terminated by a path delimiter.

$syntax/

/coNodeCommand/
/$$
A null-terminated exact string that invokes the driver as a node.
When this parameter is given, Master spawns a child process acting as a co-node.
It ensures that Master keeps going even if no remote nodes participates; for
example, the entire network goes down.

$head UnitTest Plans$$
$table
Check if all the member data, isParallel, sharedDirectory and 
coNodeCommand are set correctly.
$rend
$tend

$end
*/
/*
-------------------------------------------------------------
   Destructor
-------------------------------------------------------------
$begin DirBasedParallelControls Destructor $$

$spell 
const 
int 
destructor destructs
Dir
$$

$section Destructor$$

$index DirBasedParallelControls destructor$$

$table
$bold Prototype: $$ $cend
$italic not applicable $$
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
destroys itself.

$end

*/
/*
-------------------------------------------------------------
   Copy constructor
-------------------------------------------------------------
$begin DirBasedParallelControls Copy Constructor $$

$spell const int Dir$$

$section Copy constructor$$

$index DirBasedParallelControls copy constructor$$

$table
$bold Prototype:$$ $cend
$syntax/DirBasedParallelControls::DirBasedParallelControls A(const DirBasedParallelControls& /right/)/$$
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
Constructs a copy of the original object of the DirBasedParallelControls class.

$head UnitTest Plans$$
$table
Check if all the member data, $italic isParallel$$, $italic sharedDirectory$$ and 
$italic coNodeCommand$$ are identical with the original.
$rend
$tend
$end
*/

/*
-------------------------------------------------------------
   Assignment operator
-------------------------------------------------------------
$begin DirBasedParallelControls Assignment Operator$$

$spell
	const int const Dir
$$

$section Assignment operator$$

$index DirBasedParallelControls, assignment operator$$

$table
$bold Prototype$$ $cend
$syntax/DirBasedParallelControls &/A/ = DirBasedParallelControls &/B/$$ $rend
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
Assigns the right hand object B to the left hand object A,
where $italic A$$ and $italic B$$ have the type of 
$code DirBasedParallelControls$$.  

$head UnitTest Plans$$
$table
Check if the member data, $italic isParallel$$, $italic sharedDirectory$$ and
$italic coNodeCommand$$ are identical with the original.
$rend
$tend
$end
*/
/*
 
-------------------------------------------------------------
   Get the shared directory
-------------------------------------------------------------
$begin getSharedDirectory$$

$spell
   Dir getSharedDirectory, sharedDirectory, char*
$$

$section Get shared directory$$

$index DirBasedParallelControls, shared directory, sharedDirectory, getSharedDirectory$$

$table
$bold Prototype$$ $cend
$syntax/char* DirBasedParallelControls::getSharedDirectory()/$$ $rend
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
$code getSharedDirectory()$$ returns the shared directory path as a char*.

$head UnitTest Plans$$
$table
Check if the right value of $italic sharedDirectory$$ is returned.
$rend
$tend

$end
*/
/*
 
-------------------------------------------------------------
   Get the node command
-------------------------------------------------------------
$begin getNodeCommand$$

$spell
	Dir getNodeCommand, nodeCommand, char*
$$

$section Get node command$$

$index DirBasedParallelControls, node command, nodeCommand, getNodeCommand$$

$table
$bold Prototype$$ $cend
$syntax/char* DirBasedParallelControls::getNodeCommand()/$$ $rend
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
$code getCoNodeCommand()$$ returns the command to run a node driver as a char*.

$head UnitTest Plans$$
$table
Check if the right value of $italic coNodeCommand$$ is returned.
$rend
$tend

$end
*/

#include "DirBasedParallelControls.h"
#include "ParallelControls.h"

DirBasedParallelControls::DirBasedParallelControls( bool IsParallel, 
		                      const char* SharedDirectory, 
							  const char* CoNodeCommand ) 
		                    : ParallelControls( IsParallel ),
	                          sharedDirectory( SharedDirectory ), 
							  coNodeCommand( CoNodeCommand ) 
{
}

	// Destructor
DirBasedParallelControls::~DirBasedParallelControls() 
{
};

	// Copy constructor
DirBasedParallelControls::DirBasedParallelControls( const DirBasedParallelControls& right ) 
		: ParallelControls( right.isParallel ),
	      sharedDirectory( right.sharedDirectory ),
	      coNodeCommand( right.coNodeCommand )
    
{
}

	// Assignment operator
DirBasedParallelControls& DirBasedParallelControls::operator=( const DirBasedParallelControls& right )
{
	isParallel = right.isParallel;
	sharedDirectory = right.sharedDirectory;
    coNodeCommand = right.coNodeCommand;
	return *this;
}


