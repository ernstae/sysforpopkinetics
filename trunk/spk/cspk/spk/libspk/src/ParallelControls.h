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
 * File: ParallelControls.h
 *
 *
 * Declares ParallelControls class, the encapsulation of controls
 * related to parallel processing.
 * This header file also includes other headers for importing 
 * declarations of components necessary for the user to
 * develop a parallel-capable driver.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

/*************************************************************************
 *
 * Class: ParallelControls
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
------------------------------------------------------------
   Constructor 
-------------------------------------------------------------
$begin ParallelControls Constructor $$

$spell const int initializes bool$$

$section Constructor$$

$index ParallelControls, constructor$$

$table
$bold Prototype:$$ $cend
$syntax/ParallelControls::ParallelControls /A/(bool /IsParallel/)/$$
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
This constructor takes an argument to initializes the member data 
$italic isParallel$$.

$head Argument$$

$syntax/

/IsParallel/
/$$
This argument controls processing mode.  It is $math%true%$$ for
parallel processing mode or $math%false%$$ for single processing mode.

$head UnitTest Plans$$
$table
Check if the $italic isParallel$$ is set correctly.
$rend
$tend

$end
*/
/*
-------------------------------------------------------------
   Destructor
-------------------------------------------------------------
$begin ParallelControls Destructor $$

$spell const int destructor destructs$$

$section Destructor$$

$index ParallelControls destructor$$

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
$begin ParallelControls Copy Constructor $$

$spell const int$$

$section Copy constructor$$

$index ParallelControls copy constructor$$

$table
$bold Prototype:$$ $cend
$syntax/ParallelControls::ParallelControls /A/(const ParallelControls& /right/)/$$
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
Constructs a copy of the original object of the ParallelControls class.

$head UnitTest Plans$$
$table
Check if the $italic isParallel$$ is identical with the original.
$rend
$tend
$end
*/

/*
-------------------------------------------------------------
   Assignment operator
-------------------------------------------------------------
$begin ParallelControls Assignment Operator$$

$spell
	const int const 
$$

$section Assignment operator$$

$index ParallelControls, assignment operator$$

$table
$bold Prototype$$ $cend
$syntax/ParallelControls &/A/ = ParallelControls &/B/$$ $rend
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
$code ParallelControls$$.  

$head UnitTest Plans$$
$table
Check if the $italic isParallel$$ is identical with the original.
$rend
$tend
$end
*/

/* 
-------------------------------------------------------------
   Get isParallel
-------------------------------------------------------------
$begin getIsParallel$$

$spell
	getIsParallel, bool
$$

$section Get parallel processing mode flag$$

$index ParallelControls, getIsParallel$$

$table
$bold Prototype$$ $cend
$syntax/bool ParallelControls::getIsParallel()/$$ $rend
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
$code getIsParallel()$$ returns a boolean indicating the
processing mode.  It is $math%true%$$ for parallel processing 
mode or $math%false%$$ for single processing mode. 

$head UnitTest Plans$$
$table
Check if the right value of $italic isParallel$$ is returned.
$rend
$tend

$end
*/


#ifndef PARALLELCONTROLS_H
#define PARALLELCONTROLS_H

class ParallelControls
{
public:
	// Constructor
    ParallelControls( bool IsParallel ) : isParallel( IsParallel ) {}

	// Destructor
    virtual ~ParallelControls(){};

	// Copy constructor
    ParallelControls( const ParallelControls& right ) 
		: isParallel( right.isParallel ) {}

	// Assignment operator
    ParallelControls& operator=( const ParallelControls& right )
	{
		isParallel = right.isParallel;
		return *this;
	}

	// Getter
	inline bool getIsParallel() const { return isParallel; }

protected:
	bool isParallel;
};


//
// Headers that are not necessary for fitPopulation() but
// needed for the user to do the population analysis.
// 
#include "node.h"
#include "PARALLEL_FILE_CONSTS.h"
#include "File.h"
#include "System.h"

#endif
