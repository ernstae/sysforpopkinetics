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
 * File: StatusList.cpp
 *
 * Definition of StatusList class
 *
 * StatusList maintains a list of records, each corresponding 
 * to an individual's, for keeping track of process progress and
 * ultimately to determine timeout
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Class: StatusList
 *
 *************************************************************************/
/*
$begin StatusList$$
$spell 
  int 
  num 
  val 
  ostream 
  const 
  int 
  inx 
  bool 
  sec 
  th 
  timestamped
  instantiate 
  std 
  timestamps
  ind
  initializes
$$

$section Keeping Track of Node Activities$$

$index parallel, StatusList class$$
$index StatusList class$$

$table
$bold Prototype:$$   $cend  
$syntax/StatusList::StatusList( int /num_individuals/ )
/$$
$tend

$fend 10$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code StatusList$$ is a table-like object in which user can keep track of which
individual's analysis request has been issued, how long it has been since being checked out
or whether it has been already completed or not.
$pre

$$
$bold Master$$ should instantiate this object once per population level iteration.

$head Constructors$$
$syntax/
StatusList::StatusList( int num_individuals )
/$$
initializes an empty (internal)
table that is used to maintain the $italic num_individuals$$
number of records.

$head Public Members$$
$syntax/
void issued( int /inx/, const PackageHandle /handle/, const IndInputDataPackage& /pack/, bool /checkout/=true )
/$$
timestamps the package, $italic pack$$, containing initial values for 
the $italic inx$$-th individual as being $bold issued$$ (by the master)
and maintains $italic pack$$ and $italic handle$$ for later use when needed.
If $italic checkout$$ is true (default), the corresponding package is also
stamped as being picked up by a node at the same time.

$syntax/

void checkedOut( int /inx/ )
/$$
timestamps the $italic inx$$-th individual's package as being $bold checked out$$
by a node.

$syntax/

void completed( int /inx/ )
/$$
timestamps the $italic inx$$-th individual's parameter estimate as $bold completed$$
by Node.

$syntax/

bool isCheckedOut( int /inx/ ) const
/$$
returns true if the $italic inx$$-th individuals's package has been marked as 
being $bold checked out$$ (or picked up by some node).

$syntax/

bool isCompleted( int /inx/ ) const
/$$
returns true if $italic inx$$-th individual's parameter estimate has been already 
timestamped as being $bold completed$$.

$syntax/

int numCompleted() const
/$$
returns the number of individuals whose parameter estimates have been marked completed.
$syntax/

int nextOldest() const
/$$
returns the index to an individual whose parameter estimate process has been running
for the longest period of time among all other initiated processes.
It is determined by subtracting the moment at which the corresponding package was $bold checked out$$
from the current time.  If no more active process is found in the list, it returns a negative number. 

$syntax/

int size() const
/$$
returns the number of individuals in the population.

$head Operator Overloads$$
$syntax/
bool operator!=( const StatusList& /right/ )
/$$
returns true if $code *this$$ and $italic right$$ match in data values.

$end
*/
#include <iostream>
#include <string>
#include <ctime>
#include <cassert>
#include "StatusList.h"
#include "IndDataPackage.h"
#include "IndVars.h"
#include "Channel.h"
using namespace std;

/*-------------------------------------------------------------------------------
 *  Public Members
 *-------------------------------------------------------------------------------*/
StatusList::~StatusList()
{
}
StatusList::StatusList(const StatusList &right)
: nP(right.nP), timeout_sec(right.timeout_sec), lastExpired(right.lastExpired)
{
    records = right.records;
}

StatusList::StatusList(int num_individuals)
: nP(num_individuals), timeout_sec(0), lastExpired(0)
{
    if( num_individuals <= 0 )
    {
      const int max = SpkError::maxMessageLen();
      char message[max];
      snprintf( message, max, 
                "The value must be greater than 0 (received %d, instead).\0", num_individuals);
      throw SpkException( SpkError::SPK_USER_INPUT_ERR, message, __LINE__, __FILE__);
    }
    records.resize(nP);
}

void StatusList::checkedOut(int inx)
{
    assert(inx >= 0);
    assert(inx < nP);
    assert(records[inx].isIssued());
    records[inx].checkedOut(time(0));
}

void StatusList::issued(int inx, const PackageHandle& handler, const IndInputDataPackage& inpack, bool checkout)
{
    assert(inx >= 0);
    assert(inx < nP);
    assert(inx == inpack.indVars.who());
    records[inx].issued(time(0), handler, inpack);
    if( checkout )
      records[inx].checkedOut(time(0));
}

void StatusList::completed(int inx)
{
    assert(inx >= 0);
    assert(inx < nP);
    time_t t = time(0);
    assert(t > 0);

    if( !records[inx].isCheckedOut() )
    {
      records[inx].checkedOut(t);
    }
    
    records[inx].completed(t);
}
int StatusList::nextExpired() const
{
    int i;
    for(i=lastExpired; i<nP; i++)
    {
        if( records[i].ellapstedMoreThan(timeout_sec) )
        return i;
    }
    for(i=0; i<lastExpired; i++)
    {
        if( records[i].ellapstedMoreThan(timeout_sec) )
        return i;
    }
    return -1;
}

bool StatusList::isCheckedOut(int inx) const
{
    assert(inx >= 0);
    assert(inx < nP);
    return records[inx].isCheckedOut();
}

bool StatusList::isCompleted(int inx) const
{
    assert(inx >= 0);
    assert(inx < nP);
    return records[inx].isCompleted();
}
/*---------------------------------------
 * Implementation notes
 * by Sachiko
 *
 * This algorithm takes linear time.
 * If records are kept in a binary tree,
 * the big O can be improved to logN.
 *
 *---------------------------------------*/
int StatusList::nextOldest() const
{
    int i, sec, who, max;

    for(i=0, sec=-1, who=-1, max=-1; i<nP; i++)
    {
        sec = records[i].howLongInProcess();
        if( sec > max ){
            max = sec;
            who = i;
        }
    }
    return who;
}
const IndInputDataPackage StatusList::getInPack(int inx) const
{
    return records[inx].getInPack();
}
const PackageHandle StatusList::getHandle(int inx) const
{
    return records[inx].getHandle();
}

int StatusList::numCompleted() const
{
    int num = 0;
    for(int i=0; i<nP; i++)
    {
        if(records[i].isCompleted())
            num++;
    }
    return num;
}

int StatusList::size() const
{
    return nP;
}

StatusList& StatusList::operator=(const StatusList &right)
{
    assert( &right != this );
    records     = right.records;
    nP          = right.nP;
    timeout_sec = right.timeout_sec;
    return *this;
}
bool StatusList::operator!=(const StatusList &right)
{
    int i;
    if( nP == right.nP )
    {
        if( timeout_sec == right.timeout_sec )
        {
            if( lastExpired == right.lastExpired )
            {
                for(i=0; i<nP; i++)
                {
                    if( records[i] == right.records[i] )
                    {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

/*-------------------------------------------------------------------------------
 *  Private Members
 *-------------------------------------------------------------------------------*/
/*
 * Default constructor <prohibited>
 */
StatusList::StatusList()
: nP(0), timeout_sec(0), lastExpired(0)
{
    cerr << "StatusList::StatusList() ... default constructor prohibited." << endl;
}

/*
 * Private method
 * Returns true if the i-th individual's process has exceeded timeout.
 * Otherwise, returns false.
 */
const bool StatusList::isExpired(int inx) const
{
    assert(inx >= 0);
    assert(inx < nP);
    return records[inx].ellapstedMoreThan(timeout_sec);
}

const StatusList::StatusRecord StatusList::get(int inx) const
{
    return records[inx];
}


//////////////////////////////////////////////////////////////////////
// 
// class StatusRecord
//
// is an in-class class within StatusList class.
// An ActiviytRecord object records time-stamps for a
// particular individual.
//
//////////////////////////////////////////////////////////////////////

/*------------------------------------------------------------------------
 * Default constructor
 *------------------------------------------------------------------------*/
StatusList::StatusRecord::StatusRecord()
: issuedTime(0), checkedOutTime(0), completedTime(0)
{
}

/*------------------------------------------------------------------------
 * Copy constructor
 *------------------------------------------------------------------------*/
StatusList::StatusRecord::StatusRecord(const StatusRecord& right)
: issuedTime(right.issuedTime), checkedOutTime(right.checkedOutTime), completedTime(right.completedTime)
{
}

/*------------------------------------------------------------------------
 * Destructor
 *------------------------------------------------------------------------*/
StatusList::StatusRecord::~StatusRecord()
{
}

/*------------------------------------------------------------------------
 * Function: void StatusRecord::issued(const time_t& t)
 * 
 * This method stamps this object with a given value as a time when 
 * the corresponding individual's analysis was picked up by a Node.
 * This clears completedTime field.
 *------------------------------------------------------------------------*/
void StatusList::StatusRecord::checkedOut(const time_t& t)
{
    checkedOutTime = t;
}
void StatusList::StatusRecord::issued(const time_t& t, const PackageHandle& h, const IndInputDataPackage& inpack)
{
    issuedTime    = t;
    checkedOutTime= 0;
    completedTime = 0;
    handler       = h;
    inPack        = inpack;
}

/*------------------------------------------------------------------------
 * Function: void StatusRecord::completed(const time_t& t)
 * 
 * This method stamps this object with a given value as a time when 
 * the corresponding individual's analysis was completed by a Node.
 *------------------------------------------------------------------------*/
void StatusList::StatusRecord::completed(const time_t& t)
{
    assert(t > 0);
    completedTime = t;
}

/*------------------------------------------------------------------------
 * Function: bool StatusRecord::isIssued() const
 *
 * This method tests whether this individual's analysis has been issued
 * or not.  True if completed; false otherwise
 *------------------------------------------------------------------------*/
bool StatusList::StatusRecord::isIssued() const
{
    if( issuedTime == 0 )
        return false;
    return true;
}

/*------------------------------------------------------------------------
 * Function: bool StatusRecord::isCheckedOut() const
 *
 * This method tests whether this individual's analysis has been picked
 * up or not.  True if completed; false otherwise
 *------------------------------------------------------------------------*/
bool StatusList::StatusRecord::isCheckedOut() const
{
    if( checkedOutTime == 0 )
        return false;
    return true;
}

/*------------------------------------------------------------------------
 * Function: bool StatusRecord::isCompleted() const
 *
 * This method tests whether this individual's analysis has been completed
 * or not.  True if completed; false otherwise
 *------------------------------------------------------------------------*/
bool StatusList::StatusRecord::isCompleted() const
{
    if( completedTime == 0 )
        return false;
    return true;
}

/*------------------------------------------------------------------------
 * Function: const bool StatusRecord::ellapstedMoreThan(int timeout_sec) const
 *
 * This method tests whether the specified time has been ellapsted since
 * this individual's analysis has been picked up by a Node.
 * True if the difference between the issued time and the current time is 
 * greater than the given value in seconds.
 *------------------------------------------------------------------------*/
bool StatusList::StatusRecord::ellapstedMoreThan(int timeout_sec) const
{
    if( issuedTime == 0 )
        return false;
    if( checkedOutTime == 0 )
        return false;
    if( completedTime != 0 )
        return false;
    if( difftime(time(0), checkedOutTime) > timeout_sec )
        return true;
    return false;
}

/*------------------------------------------------------------------------
 * Functin: const int StatusList::StatusRecord::howLongEllapsted() const
 *
 * This methods tells how long, in seconds, it's been ellapsted since the
 * individual's process has been initiated.
 * If the process hasn't been initiated or has been completed already, return -1.
 *------------------------------------------------------------------------*/
int StatusList::StatusRecord::howLongInProcess() const
{
    if( completedTime == 0 && checkedOutTime > 0 )
        return static_cast<int>( difftime(time(0), checkedOutTime) );
    else
        return -1;
}
/*------------------------------------------------------------------------
 * Function: const IndInputDataPackage StatusRecord::getInPack() const
 *
 * Returns the saved input data package for this inidividual.
 *------------------------------------------------------------------------*/
const IndInputDataPackage StatusList::StatusRecord::getInPack() const
{
    return inPack;
}
/*------------------------------------------------------------------------
 * Function: const PackageHandle StatusRecord::getHandle() const
 *
 * Returns the saved handler for this inidividual's package
 *------------------------------------------------------------------------*/
const PackageHandle StatusList::StatusRecord::getHandle() const
{
  return handler;
}
/*------------------------------------------------------------------------
 * Function: StatusRecord& StatusRecord::operator=(const StatusRecord& right)
 *
 * Assign the values on the right hand side to the left hand side.
 *------------------------------------------------------------------------*/
StatusList::StatusRecord& StatusList::StatusRecord::operator=(const StatusRecord& right)
{
    issuedTime      = right.issuedTime;
    completedTime   = right.completedTime;
    inPack          = right.inPack;
    handler         = right.handler;
    return *this;
}

/*------------------------------------------------------------------------
 * Function: bool StatusList::StatusRecord::operator==(const StatusRecord& right)
 * 
 * Tests whether the contents of left and right sides are the same.
 * Returns true if they match in data.
 *------------------------------------------------------------------------*/
bool StatusList::StatusRecord::operator==(const StatusRecord& right) const
{

    return !(*this != right);
}

/*------------------------------------------------------------------------
 * Function: bool StatusList::StatusRecord::operator!=(const StatusRecord& right)
 * 
 * Tests whether the contents of left and right sides are the same.
 * Returns true if they don't match in any of their data.
 *------------------------------------------------------------------------*/
bool StatusList::StatusRecord::operator!=(const StatusRecord& right) const
{
    if( issuedTime == right.issuedTime )
    {
        if( completedTime == right.completedTime )
        {
            if(!(inPack != right.inPack) )
            {
                if( handler.getPath() == right.handler.getPath() )
                {
                    if( handler.getName() == right.handler.getName() )
                        return false;
                }
            }
        }
    }

    return true;
}
