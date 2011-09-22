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
 * File: Channel.cpp
 *
 * In this file, MaterEndChannel and NodeEndChannel classes are
 * defined.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
//
// These pragma are to disable "too long literals" warnings generated
// as a result of using STL components.
//
#pragma warning( disable : 4786 )  
#include <iostream>
#include <fstream>
#include <strstream>
#include <string>
#include <queue>
#include <vector>
#include <cassert>
#include <algorithm>
#include <set>

//#include <process.h>

#include "Channel.h"
#include "IndDataPackage.h"
#include "System.h"
#include "File.h"
#include "extractIndex.h"
#include "PARALLEL_FILE_CONSTS.h"
#include "SpkException.h"
#include "isEndOfSpk.h"

using namespace std;

//static std::deque<File> strToQueue(const std::string& long_string_delimited_by_endl);
/*************************************************************************
 *
 * typedef File PackageHandle
 *
 *************************************************************************/
/*
$begin PackageHandle$$
$spell 
$$

$section type PackageHandle$$

$index PackageHandle$$
$index Channel, package handler$$
$index Parallel, package handler$$

$table
$bold Header:$$ $cend
$code Channel.h$$ $rend
$tend

See also $xref/MasterEndChannel//MasterEndChannel/$$, $xref/NodeEndChannel//NodeEndChannel/$$,
$xref/parallel_const//PARALLEL_FILE_CONSTS.h/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$italic PackageHandle$$ is a data type that uniquely identifies a package posted by
either the master or a node.  An object of this type can be used to
determine if the corresponding package has been picked up by the receiver.
$end
*/

/*************************************************************************
 *
 * File: MasterEndChannel.cpp
 *
 * This class serves as the entry point to the communication
 * channel from Master end.
 *
 *
 * Master composes a package for each individual that contains
 * initial data necessesary for the corresponding individual's
 * random parameter analysis.
 *
 * Master writes each package in to a file.  The file has a special
 * prefix and suffix that indicate that it is a package to be picked
 * up by Node.  The Master-side post methods waits until some Node
 * picks it up.  How?  It is because when Node detects the input package,
 * it changes the filename imediately.  The new filename is not visible
 * to Master.  Master sees it as the input package dissapeared.  This
 * is when Master determines the input package has been picked up
 * by some Node.
 *
 * Master, then, records the time at which the even happened in a
 * table (ie. StatusList) for that paticular individual.
 *
 * Repeat the posting procedure for all individual.
 *
 * Then, Master starts searching for packages from Node that each
 * contains a particular individual's analysis results.
 * How? Node posts a result package as a file.  The file has
 * special prefix and suffix that indicate that it's a result package
 * from Node.  Master looks for such files.
 *
 * When Master detects a result package, it examines the contents.
 * If the contents appear corrupted, Mater re-issues the individual's
 * analysis so that some other (maybe the same) Node can re-compute.
 * When an un-corrupted result package is picked up,
 * Master records the fact in the table, and then process the contents.
 *
 * 
 *************************************************************************/
/*************************************************************************
 *
 * Class: MasterEndChannel
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin MasterEndChannel$$
$spell 
    spk util temp bool eo reimplement const sub ie ind int non
    std
$$

$section Master-end Channel$$

$index MasterEndChannel$$
$index Channel, master-end$$
$index Parallel, master-end communication channel$$

$table
$bold Header:$$ $cend
$code Channel.h$$ $rend
$bold Constructors:$$   $cend  
$syntax/MasterEndChannel(const File& /sharedDiskSpace/)
/$$
$tend

See also $xref/NodeEndChannel//NodeEndChannel/$$,
$xref/parallel_const//PARALLEL_FILE_CONSTS.h/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The class provides $bold Master$$ with components
necessary for communicating with Node.

$head Implementation Notes --- Shared Disk Space Version$$
In Shared Disk Space version, all intermediate files generated by Master,
therefore from this class, should have a distinct file extension that
identifies its origin.  Namely, the file extension is defined 
as $code SPK_MASTER_SUFFIX$$ in
$xref/parallel_const//PARALLEL_FILE_CONSTS.h/$$.
$pre

$$
For the first cut of this class, we used a queue to accumulate
and maintain a list of results files posted by Node.
When $code MasterEndChannel::get()$$ was called for the 
first time, it listed the shared directory contents and
puts the file names into the queue.  Then, popped one from
it, opened the corresponding file, extracted the file contents
and returned them as a package.  For the following calls,
the function first checked the queue if it has any items in it.
If it had, attempted to open the corresponding file.  If
not, fetched the directory contents like the first time.
It turned out that if the directory were empty (no node had
yet produced a result), the directory listing operation
throw an IO error.  The implementation expected this kind of
error and kept listing the directory until a result showed up.
However, having the OS throw exceptions and the software
catching them at this frequency was very costly.  For the
consequent version, we decided to eliminate the queue and
use a platform dependent component to grab a file from
the directory when it becomes available.  This change
eliminated the IO exception handling completely and,
as a result, improved the performance by a factor of
somewhere around 20%, together with the elimination of
queue uses from $xref/NodeEndChannel//NodeEndChannel/$$ class.
$pre

$$
Another change made in the latter cut is writing the initial
data set into an intermediate file and rename the filename
to the final name upon the completion of writing operation.
This eliminated the possibility that Node picks up an
incomplete package; As it is written, the file gets created
and becomes visible to Node.

$head Constructors$$
$syntax/
MasterEndChannel::MasterEndChannel(const File& /sharedDiskSpace/)
/$$
requires an absolute path to a shared directory to be used
for Master and Node to exchange information.
This validates the path; if the path is not writable by
the currently logged-on user, the system terminates.

$head Public Members$$
$syntax/
void flush() const
/$$
flushes the communication channel.

$syntax/

void open()
/$$
implements $code virtual void Channel::open()$$ for Master.
It flushes the communication channel and
then starts posting necessary information to initiate Node processes.

$syntax/

void close()
/$$
implements $code virtual void Channel::close()$$ for Master.
It cleans up the channel.

$syntax/

const PackageHandle post(const IndInputDataPackage& /package/, bool /wait/=true) const
/$$
posts a complete $bold non-empty$$ $italic IndInputDataPackage$$ object 
containing an arbitrary individual's initial value set
(see $tref IndInputDataPackage$$ for details) and 
returns a handler represented by a $xref/PackageHandle//PackageHandle/$$ object 
(i.e. a full path name that carried the package) immediately when $italic wait$$ is false
or after making sure the package has been picked up by some node when $italic wait$$ is true.
$pre

$$
For Shared Disk Space version, the generated file must have a
unique name, beginning with $code SPK_INDINPUT$$ prefix
and ending with $code SPK_MASTER_SUFFIX$$ suffix.
These values are defined in $xref/parallel_const//PARALLEL_FILE_CONSTS.h/$$.

$syntax/

bool MasterEndChannel::write(const std::string& filename, const std::string& message)
/$$
writes $italic message$$ to a file specified by $italic filename$$
and returns $code true$$ if the writing operation was successful.
This locks the file while writing and release it when it completes.

$syntax/

const IndOutputDataPackage get(bool /wait/=true) const throw(SpkException, SpkError)
/$$
looks for a result package returned by some node.  
It guarantees returning a complete $bold non-empty$$ $xref/IndOutputDataPackage//IndOutputDataPackage/$$ object 
when $italic wait$$ is true.  When $italic wait$$ is false, it attempts finding a package only once
and returns a complete $bold non-empty$$ $xref/IndOutputDataPackage//IndOutputDataPackage/$$ object
if it finds a complete set or returns an empty 
$xref/IndOutputDataPackage//IndOutputDataPackage/$$ object if it fails to find one.
$pre

$$
If this function finds it finds a serialized $xref/SpkException//SpkException/$$
object in the shared directory posted by a node, throws a $xref/SpkException//SpkException/$$ exception.
The exception thrown shall contain the errors reported by the node in the order they were caught
and an additional $xref/SpkError//SpkError/$$ whose
error code is set to $code SpkError::SPK_PARALLEL_ERR$$ at the end of the error list.
$pre 

$$
An integer corresponding to $math%i%$$-th individual, 
where i=0 points to the first individual, is thrown, 
if the contents of the i-th individual's package was found corrupted.

$syntax/

bool hasReached( const PackageHandle& handler ) const
/$$
returns true when the package identified by $italic handler$$ is determined to have 
been picked up by a node.
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * MasterEndChannel.
 *
 *------------------------------------------------------------------------*/
/*************************************************************************************/
/*
static deque<File> strToQueue(const string& long_string_delimited_by_endl)
{
    deque<File> filenames;
    if( long_string_delimited_by_endl.empty() )
        return filenames;

    string filename;
    istrstream istr(long_string_delimited_by_endl.c_str());
    while( !istr.eof() ){
        istr >> filename;
        if( !filename.empty() ){
            File f("", filename);
            filenames.push_back(f);
        }
    }
    return filenames;
}
*/
/*************************************************************************************/
int MasterEndChannel::_counter = 0;

using namespace parallel_const;
MasterEndChannel::MasterEndChannel()
: _id(-1), _strID("-1"), _workdir("", ""), _tempfile("",""), _getFilter("", "")
{
    ++_counter;
}
MasterEndChannel::MasterEndChannel(const File& sharedDiskSpace)
: _id(-1), _strID("-1"), _workdir("", ""), _tempfile("",""), _getFilter("", "")
{
    _id = System::pid();
    char buf[MAX_NODES];
    sprintf( buf, "%s_%d", _id, _counter );
    _strID = buf;

    _tempfile.setPath(sharedDiskSpace.getPath());
    _tempfile.setName(System::machine()+"_master"+_strID+"."+"tmp");
    ofstream ofs(_tempfile.getFullname().c_str());
    if( !ofs.is_open() )
    {
        cerr << "MasterEndChannel constructor failed!  The given " << sharedDiskSpace << " is not writable by the caller!" << endl;
        exit(-1);
    }
    ofs.close();
    try{
        System::del(_tempfile);
    }
    catch(...){}

    _workdir = sharedDiskSpace;
    _getFilter.setPath(_workdir.getPath());
    _getFilter.setName(SPK_INDOUTPUT+"*"+"."+SPK_NODE_SUFFIX);
    ++_counter;
}
MasterEndChannel::~MasterEndChannel()
{
    --_counter;
};
void  MasterEndChannel::flush() const
{
  File delme(_workdir.getPath(),"");
  string filename;

  //
  // Remove all files in the shared directory which
  // ends with the SPK_MASTER_SUFFIX file extension.
  //
  File filter(_workdir.getPath(), "*."+SPK_MASTER_SUFFIX);
  
  while(!(filename = System::findfirst(filter.getFullname().c_str())).empty())
  {
      delme.setName(filename);
      try{
          System::del(delme);
      }
      catch(...){}
  }

  //
  // Remove all files in the shared directory which
  // ends with the SPK_NODE_SUFFIX file extension.
  //
  filter.setName("*."+SPK_NODE_SUFFIX);
  
  while( !(filename = System::findfirst(filter.getFullname().c_str())).empty() )
  {
      delme.setName(filename);
      try{
          System::del(delme);
      }
      catch(...){}
  }

  /*
  //
  // Remove all temporary files.
  //
  filter.setName("*.tmp");
  
  while( !(filename = System::findfirst(filter.getFullname().c_str())).empty() )
  {
      delme.setName(filename);
      System::del(delme);
  }
  */
}
void  MasterEndChannel::open()
{
    flush();
}
void  MasterEndChannel::close()
{
  flush();
}
const IndOutputDataPackage MasterEndChannel::get(bool wait) const
{
  string filename;
  File fullname;

  string errorFilter;
  errorFilter = _workdir.getPath()+"/"+parallel_const::SPK_EXCEPTION+"*"+"."+SPK_NODE_SUFFIX;

  int index;
  IndOutputDataPackage pack;

  //
  // Look for a valid result package.
  //
  do
  {
    //
    // Check if any node has reported an optimization error.
    //
    if( !(filename = System::findfirst(errorFilter.c_str())).empty() )
    {
      SpkException e;
      fullname.setPath(_workdir.getPath());
      fullname.setName(filename);
      ifstream ifs(fullname.getFullname().c_str());
      ifs >> e;
      throw e;
    }

    //
    // Check for a result package.
    //
    filename = System::findfirst(_getFilter.getFullname().c_str());
    if(!wait && filename.empty())
        return pack;


  }while(filename.empty());

  fullname.setPath(_workdir.getPath());
  fullname.setName(filename);
  index = extractIndex(SPK_INDOUTPUT, filename);
  assert(index >= 0);

  bool nonEmptyPackageReceived = false;
  do{
    ifstream ifs(fullname.getFullname().c_str());
    if( ifs.good() )
    {
      try{
        ifs >> pack;
        ifs.close();
        nonEmptyPackageReceived = true;
      }
      catch(...)
      {
        ifs.close();
          //
          // This indicates that the file was just created by some
          // node but not written any contents.
          // Try opening it again: it might be ready by next time.
        continue;
      }
    }
  }while(!nonEmptyPackageReceived);

  assert( !pack.empty() );
  //assert( System::exist(fullname) );

  while( System::exist(fullname) )
  {
      try{
          System::del(fullname);
      }
      catch(...){}
  }

  return pack;

}

const PackageHandle MasterEndChannel::post(const IndInputDataPackage& inpack, bool wait) const
{
    assert( !inpack.empty() );
    int  index = inpack.indVars.who();
    strstream strIndex;
    strIndex.width(MAX_INDS_DIG);
    strIndex.fill('0');
    strIndex.flags(ios::right);
    strIndex << index;
    strIndex.put('\0');

    //File intermediate(_workdir.getPath(), SPK_INDOUTPUT+strIndex.str()+"_"+_tempfile.getName()+"."+"tmp");
    File outfile(_workdir.getPath(), SPK_INDINPUT+strIndex.str()+"."+SPK_MASTER_SUFFIX);
    bool infileCreated = false;
    while( !infileCreated ){
        try{

            //
            // Write into an intermediate file and rename the file to
            // the final name.  This ensures the final file
            // contains a complete set of information when Node
            // picks it up.  In other words, Node may detect
            // the final file as it is written contents if
            // writing operation is done directly to the final file.
            // 
            ofstream ofs(_tempfile.getFullname().c_str());
            ofs << inpack;
            ofs.close();
            System::ren(_tempfile, outfile.getName().c_str());
            infileCreated = true;
        }
        catch(...)
        {
          //
          // The control may reach here if the file access
          // is slow on the machine in which the shared directory
          // resides.
          //
          // Keep trying until succeeds.
          //
        }     
    }

    //
    // Wait until a Node picks up the derivery.
    // The file may be already gone if a node
    // has picked it up by the time the control
    // reaches this line of code.
    //
    if(wait)
    {
        while( System::exist(outfile) )
        {
        };
    }
    return outfile;
}
bool MasterEndChannel::hasReached( const PackageHandle& handler ) const
{
    if( System::exist(handler) )
    {
      return false;
    }
    else
    {
      return true;
    }
}

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#include <sys/locking.h>
#endif
bool MasterEndChannel::write(const std::string& filename, const std::string& message)
{
  bool ok = false;

#ifdef _WIN32
    File pathname(_workdir.getPath(), filename);
    FILE *file;
    while( (file = fopen(pathname.getFullname().c_str(), "w")) == NULL )
    {
      System::sleep(1);
    }
    _locking(_fileno(file), _LK_LOCK, message.size());
    if( fprintf(file, "%s\n", message.c_str()) > 0 )
      ok = true;
    _locking(_fileno(file), _LK_UNLCK, message.size());
    fclose(file);
#endif
    
  return ok;
}

/*************************************************************************
 *
 * File: NodeEndChannel.cpp
 *
 * This concrete Channel sub-class provides entry points to the communication
 * channel from Node end.
 *
 * Node keeps looking for two kinds of files in the shared directory:
 * One which indicates the end of transmission (the end of population analysis),
 * another which contains a particular individual's initial values.
 *
 * When Node detects an input file, it immediately changes the filename
 * to something else so that Master can no longer see.
 *
 * Node processes the input file and performs random parameter analysis.
 * 
 * When analysis is done, Node composes a package and writes it in to
 * a file.  The file has a filename with special suffix and prefix that
 * Master knows it's from Node.
 *
 * When Node detects the end-of-transmission file,
 * it termininates the execution normally.
 *
 *************************************************************************/
/*************************************************************************
 *
 * Class: NodeEndChannel
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/
/*
$begin NodeEndChannel$$
$spell 
    spk util temp bool eo reimplement const sub ie ind int non
    initializes
$$

$section Node-end Channel$$

$index NodeEndChannel$$
$index Channel, node-end$$
$index Parallel, node-end communication channel$$

$table
$bold Header:$$ $cend
$code Channel.h$$ $rend
$bold Prototype:$$   $cend  
$syntax/NodeEndChannel(const File& /sharedDiskSpace/)
/$$
$tend

See also $xref/MasterEndChannel//MasterEndChannel/$$,
$xref/parallel_const//PARALLEL_FILE_CONSTS.h/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The class provides $bold Node$$ with components
necessary for communicating with Master.

$head Implementation Notes --- Shared Disk Space Version $$
In Shared Disk Space version, all intermediate files generated by Node,
therefore from this class, should have a distinct file extension that
identifies its origin.  Namely, the file extension is defined 
as $code SPK_NODE_SUFFIX$$ in
$xref/parallel_const//PARALLEL_FILE_CONSTS.h/$$.
$pre

$$
For the first cut of this class, we used a queue to accumulate
and maintain a list of results files posted by Master.
When $code NodeEndChannel::get()$$ was called for the 
first time, it listed the shared directory contents and
puts the file names into the queue.  Then, popped one from
it, opened the corresponding file, extracted the file contents
and returned them as a package.  For the following calls,
the function first checked the queue if it has any items in it.
If it had, attempted to open the corresponding file.  If
not, fetched the directory contents like the first time.
It turned out that if the directory were empty (no node had
yet produced a result), the directory listing operation
throw an IO error.  The implementation expected this kind of
error and kept listing the directory until an input file showed up.
However, having the OS throw exceptions and the software
catching them at this frequency was very costly.  For the
consequent version, we decided to eliminate the queue and
use a platform dependent component to grab a file from
the directory when it becomes available.  This change
eliminated the IO exception handling completely and,
as a result, improved the performance by a factor of
somewhere around 20%, together with the elimination of
queue uses from $xref/MasterEndChannel//MasterEndChannel/$$ class.
$pre

$$
Another change made in the latter cut is writing the initial
data set into an intermediate file and rename the filename
to the final name upon the completion of writing operation.
This eliminated the possibility that Master picks up an
incomplete package; As it is written, the file gets created
and becomes visible to Master.


$head Constructors$$
$syntax/
NodeEndChannel::NodeEndChannel(const File& /sharedDiskSpace/)
/$$
requires an absolute path to a shared directory to be used
for Master and Node to exchange information.
This validates the path; if the path is not writable by
the logged-on user, the system terminates.

$head Public Members$$
$syntax/
void open()
/$$
implements $code virtual void Channel::open()$$ for Node.
It initializes the communication channel as necessary to make it
ready to receive an input package from Master.

$syntax/

void close()
/$$
implements $code virtual void Channel::close()$$ for Node.
It clean up the channel as necessary.

$syntax/

const IndInputDataPackage get() const
/$$
acquires a complete $bold non-empty$$ $xref/IndInputDataPackage//IndInputDataPackage/$$ 
object containing
an arbitrary individual's initial values set.
$pre

$$
If the end of Spk is detected during the operation, this function
throws an $xref/SpkError//SpkError/$$ object whose error code
is set to $code SpkError::SPK_PARALLEL_END_SIGNAL$$.

$syntax/

const PackageHandle post(const IndOutputDataPackage& /package/) const
/$$
posts a $bold non-empty$$ arbitrary individual's parameter estimates
(see $tref IndOutputDataPackage$$ for details) and returns an 
$xref/PackageHandle//PackageHandle/$$ object as a handler to the package.
If an empty package is given, the system terminates.
$pre

$$
For Shared Disk Space version, the generated file must have a
unique name, beginning with $code SPK_INDOUTPUT$$ prefix
and ending with $code SPK_NODE_SUFFIX$$ suffix.
These values are defined in $xref/parallel_const//PARALLEL_FILE_CONSTS.h/$$.

$syntax/

const PackageHandle post(const SpkException& err) const
/$$
posts a $xref/SpkException//SpkException/$$ object
and returns an 
$xref/PackageHandle//PackageHandle/$$ object as a handler to the package.
$pre

$$
For Shared Disk Space version, the generated file must have a
unique name, beginning with $code SPK_EXCEPTION$$ prefix
and ending with $code SPK_NODE_SUFFIX$$ suffix.
These values are defined in $xref/parallel_const//PARALLEL_FILE_CONSTS.h/$$.

$end
*/
/*------------------------------------------------------------------
 * NodeEndChannel
 *------------------------------------------------------------------*/
NodeEndChannel::NodeEndChannel()
: _id(-1), _strID("-1"), _tempfile("", ""), _workdir("", ""), _getFilter("", "")

{
}

NodeEndChannel::NodeEndChannel(const File& sharedDiskSpace)
: _id(-1), _strID("-1"), _tempfile("", ""), _workdir("", ""), _getFilter("","")
{
    _id = System::pid();
    char buf[MAX_NODES];
    sprintf( buf, "%d", _id );
    _strID = buf;

    _tempfile.setPath(sharedDiskSpace.getPath());
    _tempfile.setName(System::machine()+"_node"+_strID+".tmp");
    ofstream ofs(_tempfile.getFullname().c_str());
    if( !ofs.is_open() )
    {
        cerr << "NodeEndChannel constructor failed!  The given " << sharedDiskSpace << " is not writable by the caller!" << endl;
        exit(-1);
    }
    ofs.close();
    try{
        System::del(_tempfile);
    }
    catch(...){}

    _workdir = sharedDiskSpace;
    _getFilter.setPath(_workdir.getPath());
    _getFilter.setName(SPK_INDINPUT+"*."+SPK_MASTER_SUFFIX);
}
NodeEndChannel::~NodeEndChannel()
{
};

void  NodeEndChannel::open(){}

void  NodeEndChannel::close()
{
    //
    // Clean up own mess
    // The file may have been already removed
    // by Master.  So, try deleting it only if it exists.
    //
    if( System::exist(_tempfile) )
    {
        try{
            System::del(_tempfile);
        }
        catch(...){}
    }
}
//
// Pick up an individual's initial data package.
// As soon as picked up, change the filename to somethin
// else Master cannot see.
// Extract data and return it.
// If the end of Spk is detected, throws
// an enum ErrorCode object, SpkError::SPK_PARALLEL_END_SIGNAL.
//
const IndInputDataPackage NodeEndChannel::get() const 
{
  //
  // Check first of all if the directory is accessible.
  //
  IndInputDataPackage pack;
  //File inProcessName(_workdir.getPath(), "");

  File bIn(_workdir.getPath(), "");
  bool obtainedOneSuccessfully = false;

  while( !obtainedOneSuccessfully ){
      if( isEndOfSpk(_workdir) )
      {
          throw SpkError::SPK_PARALLEL_END_SIGNAL;
      }
      string infilename;
      while( infilename.empty())
      {
          if( isEndOfSpk(_workdir) )
          {
              throw SpkError::SPK_PARALLEL_END_SIGNAL;
          }

          try{
              infilename = System::findfirst(_getFilter.getFullname().c_str());
          }
          catch( ... )
          {
            //
            // If the control reaches here, it indicates
            // that no input data package for this node is available.
            // Go fetch another one.
          }
      }

      //
      // Setting a path in File is an expensive operation.  
      // It checks whether the user-given path ends with "/" or not and
      // if not, reallocate a char array and creates a new path that ends with "/".
      // The time interval between finding a file in the directory and 
      // an attempt to rename the filename must be minimized.
      // Otherwise, the file will be taken by other node, therefore unavailable, by the 
      // time it attempts to change the name.
      // So, the following two File objects (bIn and _tempfile) have already
      // the path set to the shared directory and the program makes an assumption
      // that the paths haven't been echanged by this point.
      //
      assert(bIn.getPath() == _workdir.getPath());
      assert(_tempfile.getPath() == _workdir.getPath());

      bIn.setName(infilename);

      try{
          System::ren( bIn, _tempfile.getName() );
      }
      catch(...)
      {
        //
        // If the control reaches here, it indicates that
        // some other node has already changed the file name
        // to an in-process name, which means
        // the individual's analysis has been in process.
        //
        // So, try fetching a different input package
        // if available.
        //
        continue;
      }

      while(System::exist(_tempfile) )
      {
        if( isEndOfSpk(_workdir) )
        {
            throw SpkError::SPK_PARALLEL_END_SIGNAL;
        }
        ifstream ifs(_tempfile.getFullname().c_str());
        if( ifs.good() )
        {
          try{
              ifs >> pack;
              ifs.close();
              while( System::exist(_tempfile) )
              {
                try{
                  System::del(_tempfile);
                }
                catch(...)
                {
                  //
                  // This may be because the directory is locked.
                  // Try until the file cheases to exist.
                  //
                }
              }
              obtainedOneSuccessfully = true;
          }

          catch( SpkException& e)
          {
              ifs.close();
              if( !pack.empty() )
              {
                throw e.push( SpkError::SPK_PARALLEL_ERR, "Corrupt data", __LINE__, __FILE__);
              }
          }

          catch( ... )
          {
              ifs.close();
              if( !pack.empty() )
              {
                throw SpkException( SpkError::SPK_PARALLEL_ERR, "File IO error", __LINE__, __FILE__ );
              }
          }
        }
      }
      assert( !System::exist(_tempfile) );

  }
  
  assert( !pack.empty() );
  return pack;

}
const PackageHandle NodeEndChannel::post(SpkException& err) const
{
  strstream mess;
  mess << "Process# " << _strID << " on ";
  mess << System::machine() << " caught a SpkException." << endl;
  mess << ends;

  err.push( SpkError::SPK_PARALLEL_ERR, mess.str(), __LINE__, __FILE__ );

  File errfile( _workdir.getPath(), parallel_const::SPK_EXCEPTION+"."+SPK_NODE_SUFFIX);
  ofstream ofs(errfile.getFullname().c_str());
  ofs << err;
  ofs.close();
  return errfile;
}
const PackageHandle NodeEndChannel::post(const IndOutputDataPackage& pack) const
{
    assert(!pack.empty());

    int index = pack.indResults.getIndex();
    strstream strIndex;
    strIndex.width(MAX_INDS_DIG);
    strIndex.fill('0');
    strIndex.flags(ios::right);
    strIndex << index;
    strIndex.put(NULL);

    // post the IndOutputDataPackage package.
    File outfile( _workdir.getPath(), SPK_INDOUTPUT+strIndex.str()+"."+SPK_NODE_SUFFIX);
    bool outfileCreated = false;
    while( !outfileCreated )
    {
        try{
            //
            // There may be a file whose name is the same as "outfile" in the directory.
            // This is possible because if some node had posted the file
            // after Master progressed to the next (pop) iteration and
            // missed removing it.
            // System::ren() fails if the target filename already existed.
            // So, remove it if exists.
            //
            if(System::exist(outfile) )
              System::del(outfile);

            //
            // Write into an intermediate file first and then
            // rename the file name to a real one which Master is waiting for.
            // This effort is to make sure when Master detects the file's existence,
            // the contents are already written completely.
            // This is because ofstream::open() operation creates
            // the file as it is called even before any contents is written.
            //
            ofstream ofs(_tempfile.getFullname().c_str());
            ofs << pack;
            ofs.close();
            System::ren(_tempfile, outfile.getName().c_str());
            outfileCreated = true;
        }
        catch(...)
        {
          //
          // The control can reach here if the OS locks the entire
          // directory in which the file, "outfile", exists while
          // other processes are modifying the directory list.
          //
          // So, this exception is OK.  Keep trying writing it.
          //
        }
    }        
    return outfile;
}
/*************************************************************************************/
