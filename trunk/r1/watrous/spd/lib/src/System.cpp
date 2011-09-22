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
 * File: System.cpp
 *
 *
 * This class encapsulate platform-dependent operations.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Class: System
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Macros should be used to switch between these platform-dependent 
 * operations at compilation time.  Don't switch at run-time!
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin System$$
$spell 
  const
  fullpath
  dir 
  outputfile 
  del 
  dos 
  csh 
  Holden 
  whoami 
  login 
  int 
  howmanyCPU
  bool 
  pwd 
  sub 
  ren 
  ie 
  Spk 
  Proto 
  Lib 
  pid 
  beginthread 
  cdecl 
  arglist 
  endthread
  async 
  sync 
  exe 
  argv 
  errno 
  platform 
  cmd 
  std
  cpp
  sys
  xyz
  wildcards
  findfirst
  fullname
  arg
  realtime
  unslept
$$

$section System Utilities Class$$

$index System class$$
$index OS, system utilities$$
$index platform, dependent utilities$$
$index utility, platform dependent$$

$table
$bold Prototype:$$  N/A (All members are static)
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
$code System$$ class provides safe environment to execute system dependent utilities.

$head Constructors $$
No constructors.  All members are static.

$head Public Methods$$
$syntax/

static std::string machine()
/$$
returns an all-lower-case string object indicating the name assigned to the machine.
$syntax/

static std::string whoami()
/$$
returns a case-sensitive string object indicating the current user's login name.

$syntax/

static int howmanyCPU()
/$$
returns the number of processors built in to the current machine.
$pre

$$
Implementation Notes:  This method originally had a name $code howmany$$.
It turned out that G++ uses this name internally.  So, to make our life easier
when it comes to port the library, we changed the name to something else
that is more specific.

$syntax/

static char pathDelimiter()
/$$
returns a character used as the path delimiter on the current OS.

$syntax/

static File pwd();
/$$
returns the current directory as a $xref/File//File/$$ object.
The File object will contain the directory path as its path and
empty string as its filename.

$syntax/

static void del( const File& /target/ )
/$$
$syntax/
static void del( const char* /target/ )
/$$

deletes a file specified by $italic target$$.  If the file does not exist or the write
privilege to the file is not granted to the user, $code del$$ throws 
a $code SpkException$$ exception with the error code set to $code SPK_STD_ERR$$ 
(see $tref SpkException$$ for details).

$syntax/

static void ren( const File& /source/, const string& /after/)
/$$
changes the name of the file specified by $italic source$$ to $italic after$$.
$italic after$$ must be a string object only specifying a new filename 
(ie. no path).  If $italic after$$ already exists, the existing file is
overwritten.  If the existing file is not writable by the user or
the path in which $italic after$$ (will) resides is
not, this method throws a SpkException whose last error code is 
set to $code SpkError::SPK_STD_ERR$$.

$syntax/

static std::string findfirst(const char* /filter/)
/$$
returns the filename of the file containing the specified string
found first in the directory embedded in $italic filter$$. 
For example, the filename part of $italic filter$$ can contain wildcards (*)
or a specific filename.  If no file is found, an empty string is returned.

$syntax/

static bool exist(const char* fullname)
/$$
return true if $italic fullname$$ exists.  $italic fullname$$ can be
a filename alone, a path alone or a full path name.  This method does not 
check the access permission on the file.  It merely checks the
existence of $italic fullname$$.

$syntax/

static int pid()
/$$
returns the current process id.

$syntax/

static int beginthread( void( __cdecl * /start_address/ )( void * ), unsigned /stack_size/, void * /arg/ )
/$$
starts a new thread for a function located at $italic start_address$$ with the $code void*$$ argument.  
It returns a non-negative number if successful.

$syntax/

static void endthread()
/$$
forcefully terminates all threads created with $code System::beginthread(...)$$.

$syntax/

static unsigned sleep( unsigned int /sec/ )
/$$

suspends the calling thread until either the number of realtime seconds specified by $italic sec$$
has elapsed or a signal is delivered to the calling thread and its action is to invoke a 
signal-catching function or to terminate the process.  
If sleep() returns because the requested time has elapsed, the value returned shall be 0. 
If sleep() returns due to delivery of a signal, the return 
value shall be the "unslept" amount (the requested time minus the time actually slept) in seconds.


$syntax/

static int spawnAsyncProcess( const string& /executable_name/, char* /argv/[] )
/$$
creates a new asynchronous process running with the calling process.
It returns immediately after a new process is created (no wait till completion).
$pre

$$
$italic executable_name$$ is an executable (not a function) file name and
$italic argv[]$$ is a null-terminated list of pointers to c-style strings that are passed to
$italic executable_name$$.  Note, however, argv[0] corresponds to the executable's name itself.
For example, if $italic executable_name$$ = $italic MyProgram.exe$$, then
argv[0] = $italic MyProgram.exe$$ and argv[1..n] are arguments to the program.
argv[n+1] = NULL.
$pre

$$
If failed to spawn a new process and executes $italic executable_name$$, 
this method returns a negated $code errno$$.  If succeeds, it 
returns a non-negative number.

$syntax/

static int spawnSyncProcess( const string& /executable_name/, char* /argv/[] )
/$$
creates a new process running with the calling process and returns when the running
process completes.
$pre

$$
$italic executable_name$$ is an executable (not a function) file name and
$italic argv[]$$ is a null-terminated list of pointers to c-style strings that are passed to
$italic executable_name$$.  Note, however, argv[0] corresponds to the executable's name itself.
For example, if $italic executable_name$$ = $italic MyProgram.exe$$, then
argv[0] = $italic MyProgram.exe$$ and argv[1..n] are arguments to the program.
argv[n+1] = NULL.

$pre

$$
If failed to spawn a new process and executes $italic executable_name$$, 
this method returns a negated $code errno$$.  If succeeds, it 
returns a non-negative number.

$syntax/

DEPLICATED!  static int System::exec(const std::string& /cmd/)
/$$
executes $italic cmd$$ at the command line.  It returns 0 if succeeds or non zero if fails.

$syntax/

static int System::createDirectory( const char* /path/ );
/$$
creates a new directory with the directory name specified by $italic path$$.
If the function succeeds, the return value is nonzero.  otherwise it is zero.
There is a default string size limit for paths of 248 characters.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#ifdef WIN32
  #include <direct.h> // for _getdcwd and _getdrive
  #include <io.h>     // for _findfirst and _access
  #include <windows.h>
  #include <time.h>   // for clock()
#else
  #include <sys/types.h>
  #include <unistd.h>
#endif

#include <iostream>
#include <string>
#include <strstream>
#include <fstream>
  //#include <process.h>
#include <cstdio>
#include <cerrno>

#include "System.h"
#include "SpkException.h"
#include <cstdlib>

/*------------------------------------------------------------------------
 * Namespaces
 *------------------------------------------------------------------------*/
using namespace std;
using namespace SpkException_const;
/*------------------------------------------------------------------------
 * Local functions
 *------------------------------------------------------------------------*/
static std::string tolowerstr(const std::string& s)
{
    std::string s2;
    std::string::const_iterator p = s.begin();
    while(p!=s.end())
    {
        s2 += tolower(*p);
        ++p;
    }
    return s2;
}

/*------------------------------------------------------------------------
 * System class properties
 *------------------------------------------------------------------------*/

#ifdef WIN32
    char System::_DELIMIT = '\\';
#else
    char System::_DELIMIT = '/';
#endif

std::string System::_MACHINE;
std::string System::_USER;
int System::_NUMBER_OF_PROCESSORS = 0;

/*------------------------------------------------------------------------
 * System::System()
 *------------------------------------------------------------------------*/
System::System()
{
#ifdef WIN32

    char* tmp = getenv("COMPUTERNAME");
    if( tmp == 0 )
    {
        std::cerr << "Failed! in System::System()" << std::endl;
        std::cerr << "\tThe system variable <COMPUTERNAME> is not available on your machine." << std::endl;
        exit(-1);
    }
    else
    {
        _MACHINE.assign(tolowerstr(tmp));
    }
    tmp = getenv("USERNAME");
    if( tmp == 0 )
    {
        std::cerr << "Failed! in System::System()" << std::endl;
        std::cerr << "\tThe system variable <USERNAME> is not available on your machine." << std::endl;
        exit(-1);
    }
    else
    {
        _USER.assign(tmp);
    }
    int num = atoi(getenv("NUMBER_OF_PROCESSORS"));
    if( tmp == 0 )
    {
        std::cerr << "Failed! in System::System()" << std::endl;
        std::cerr << "\tThe system variable <NUMBER_OF_PROCESSORS> is not available on your machine." << std::endl;
        exit(-1);
    }
    else
    {
        _NUMBER_OF_PROCESSORS = num;
    }
#else
    cerr << "Unsupported OS" << ", " << __LINE__ << " " << __FILE__ << endl;
    abort();
#endif
}

/*------------------------------------------------------------------------
 * System::~System()
 *------------------------------------------------------------------------*/
System::~System()
{
}

/*------------------------------------------------------------------------
 * System::System(const System&)
 *------------------------------------------------------------------------*/
System::System(const System& right)
{
}

/*------------------------------------------------------------------------
 * System::howmanyCPU()
 *------------------------------------------------------------------------*/
int System::howmanyCPU()
{
  if( _NUMBER_OF_PROCESSORS <= 0 )
    _NUMBER_OF_PROCESSORS = atoi(getenv("NUMBER_OF_PROCESSORS"));
  return _NUMBER_OF_PROCESSORS;
}

/*------------------------------------------------------------------------
 * System::pathDelimiter()
 *------------------------------------------------------------------------*/
char System::pathDelimiter()
{
    return _DELIMIT;
}

/*------------------------------------------------------------------------
 * System::machine()
 *------------------------------------------------------------------------*/
const std::string System::machine()
{
  if (_MACHINE.empty() )
    _MACHINE = string( getenv("COMPUTERNAME") );
    return _MACHINE;
}

/*------------------------------------------------------------------------
 * System::whoami()
 *------------------------------------------------------------------------*/
const std::string System::whoami()
{
  if( _USER.empty() )
    _USER = getenv("USERNAME");
  return _USER;
}


/*------------------------------------------------------------------------
 * System::del(const File&)
 *------------------------------------------------------------------------*/
void System::del( const File& target )
{
  del(target.getFullname().c_str());
}

/*------------------------------------------------------------------------
 * System::del(const char*)
 *------------------------------------------------------------------------*/
void System::del(const char *target)
{
  if( remove(target) == -1 )
  {
      std::string mess;
      mess += "Failed to delete ";
      mess += target;
      throw SpkException(SpkError::SPK_STD_ERR, mess.c_str(), __LINE__, __FILE__);
  }
}

/*------------------------------------------------------------------------
 * System::ren(const File&, const std::string&)
 *------------------------------------------------------------------------*/
void System::ren( const File& source, const std::string& after )
{
 
  File afterWithPath(source.getPath(), after);
  
  //
  // "rename(const char* orignal, const char* target)" is supported by ANSI
  //
  // (At least VC++'s implementation of) rename() does not
  // really re-name a filename.  It rather copies the contents of the original
  // in a different file.
  // 
  // For instance, "rename( "here/abc.cpp", "xyz.cpp")"
  // keeps abc.cpp in the subdirectory "here" and
  // puts a duplicate of abc.cpp in the current directory as "xyz.cpp".
  //
  // But, this is not the behavior we want in this funtion.
  // This function genuinely changes the name of the given file.
  //
  ren(source.getFullname().c_str(), 
    afterWithPath.getFullname().c_str());
}

/*------------------------------------------------------------------------
 * System::ren(const char*, const char*)
 *------------------------------------------------------------------------*/
void System::ren(const char* source, const char* after)
{
  if( rename(source, after) != 0 )
  {
      std::string mess;
      mess += "Failed to rename ";
      mess += source;
      mess += " to ";
      mess += after;
      throw SpkException(SpkError::SPK_STD_ERR, mess.c_str(), __LINE__, __FILE__);
  }

}

/*------------------------------------------------------------------------
 * System::pid()
 *------------------------------------------------------------------------*/
int System::pid()
{
  return getpid();
}

/*------------------------------------------------------------------------
 * System::findfirst(const char*)
 *------------------------------------------------------------------------*/
const std::string System::findfirst(const char* filter)
{
#ifdef WIN32
  // filter can contain widecard or specific filename
    struct _finddata_t file;
    long hFile;

    // Find first .c file in current directory
    if( (hFile = _findfirst( filter, &file )) != -1L )
    {
        _findclose(hFile);
       return string(file.name);
    }
    else
    {
        _findclose(hFile);
        return string();
    }
   
#else
    cerr << "Unsupported OS" << ", " << __LINE__ << " " << __FILE__ << endl;
    abort();
#endif
}

/*------------------------------------------------------------------------
 * System::exist(const char*)
 *------------------------------------------------------------------------*/
bool System::exist(const char* fullname)
{
#ifdef WIN32
  //
  // Each of these functions returns 0 if the file has the given mode. 
  // The function returns –1 if the named file does not exist or is 
  // not accessible in the given mode; in this case, errno is set as follows:
  //
  // EACCES ---
  // Access denied: file’s permission setting does not allow specified access.
  //
  // ENOENT ---
  // Filename or path not found.
  //
  //
  // mode Value Checks File For 
  // 00   Existence only 
  // 02   Write permission 
  // 04   Read permission 
  // 06   Read and write permission 
  //
  // NOTE: When a directory name is given, it checks only its existence.
  //       No permission check.
  //
  if( _access(fullname, 00) == 0 )
    return true;
  else
    return false;
#else
    cerr << "Unsupported OS" << ", " << __LINE__ << " " << __FILE__ << endl;
    abort();
#endif
}

/*------------------------------------------------------------------------
 * System::findfirst(const File&)
 *------------------------------------------------------------------------*/
bool System::exist(const File& fullname)
{
  return exist(fullname.getFullname().c_str());
}


/*------------------------------------------------------------------------
 * System::pwd()
 *------------------------------------------------------------------------*/
const File System::pwd()
{
#ifdef WIN32
    char buf[_MAX_PATH];
    char* path = _getdcwd(_getdrive(), buf, _MAX_PATH);
    if( path != 0 ){
        File current_dir(std::string(path),"");
        return current_dir;
    }
    else{
        std::cerr << "Failed! in System::pwd()" << std::endl;
        exit(-1);
    }
#elif UNIX
    char buf[MAXPATHLEN];
    char* path = getcwd(buf, MAXPATHLEN);
    if( path != 0 ){
        File current_dir( std::string(path).c_str(), "");
        return current_dir;
    }
    else{
        std::cerr << "Failed! in System::pwd()" << endl;
        exit(-1);
    }
#else
    std::cerr << "Failed! in System::pwd() --- The architecture is not supported!" << endl;
    exit(-1);
#endif

}

/*------------------------------------------------------------------------
 * System::beginthread(void( __cdecl * start_address )( void * ), unsigned stack_size, void * arglist)
 *------------------------------------------------------------------------*/
/*
int System::beginthread( void( __cdecl * start_address )( void * ), unsigned int stack_size, void * arglist )
{
#ifdef WIN32
    return _beginthread(start_address, stack_size, arglist);
#else
    std::cerr << "Failed! in System::beginthread() --- The architecture is not supported!" << endl;
    exit(-1);
#endif
}
*/

/*------------------------------------------------------------------------
 * System::pid()
 *------------------------------------------------------------------------*/
/*
void System::endthread()
{
#ifdef WIN32
    _endthread();
#else
    std::cerr << "Failed! in System::endthread() --- The architecture is not supported!" << endl;
    exit(-1);
#endif
}
*/
/*------------------------------------------------------------------------
 * System::sleep(long int ms)
 *------------------------------------------------------------------------*/
unsigned System::sleep( unsigned seconds )
{
#ifdef WIN32
  //
  // Window's Sleep() takes the given value as milliseconds
  //
  time_t begin = clock();
  Sleep( (unsigned int)(seconds * 1000) );
  time_t end   = clock();
  return 1 / (CLOCKS_PER_SEC * (end-begin) );
#else
    std::cerr << "Failed! in System::sleep() --- The architecture is not supported!" << endl;
    exit(-1);
#endif
}

/*------------------------------------------------------------------------
 * System::spawnAsyncProcess(const string&, char* [])
 *------------------------------------------------------------------------*/
int System::spawnAsyncProcess( const std::string& executable_name, char* argv[] )
{
#ifdef WIN32

    if( _spawnv( _P_NOWAIT, executable_name.c_str(), argv ) == -1 )
    {
        switch(errno){
        case E2BIG: 
            std::cerr << "Argument list exceeds 1024 bytes" << std::endl;
            break;
        case EINVAL: 
            std::cerr << "Some mode argument is invalid" << std::endl;
            break;
        case ENOENT:
            std::cerr << executable_name << " is not found" << std::endl;
            break;
        case ENOEXEC:
            std::cerr << executable_name;
            std::cerr << "  is not executable or has invalid executable-file format" << std::endl;
            break;
        case ENOMEM:
            std::cerr << "Not enough resources" << std::endl;
            break;
        default:
            std::cerr << "Unknown error" << std::endl;
            break;
        }
        return -errno;
    }
    return 0;
#else
    std::cerr << "Failed! in System::spawnAsyncProcess() --- The architecture is not supported!" << std::endl;
    exit(-1);
#endif
}

/*------------------------------------------------------------------------
 * System::spawnSyncProcess(const string&, char* [])
 *------------------------------------------------------------------------*/
int System::spawnSyncProcess( const std::string& executable_name, char* argv[] )
{
#ifdef WIN32
    if( _spawnv( _P_WAIT, executable_name.c_str(), argv ) == -1 )
    {
        switch(errno){
        case E2BIG: 
            std::cerr << "Argument list exceeds 1024 bytes" << std::endl;
            break;
        case EINVAL: 
            std::cerr << "Some mode argument is invalid" << std::endl;
            break;
        case ENOENT:
            std::cerr << executable_name << " is not found" << std::endl;
            break;
        case ENOEXEC:
            std::cerr << executable_name;
            std::cerr << "  is not executable or has invalid executable-file format" << std::endl;
            break;
        case ENOMEM:
            std::cerr << "Not enough resources" << std::endl;
            break;
        default:
            std::cerr << "Unknown error" << std::endl;
            break;
        }
        return -errno;
    }
    return 0;
#else
    std::cerr << "Failed! in System::spawnAsyncProcess() --- The architecture is not supported!" << std::endl;
    exit(-1);
#endif
}

/*------------------------------------------------------------------------
 * System::exec(string&)
 *------------------------------------------------------------------------*/
int System::exec(const std::string& commandline)
{
    return system(commandline.c_str());
}

/*------------------------------------------------------------------------
 * System::createDirectory(char*)
 *------------------------------------------------------------------------*/
int System::createDirectory( const char* path )
{
#ifdef WIN32
	return CreateDirectory( path, NULL );
#else
    std::cerr << "Failed! in System::createDirectory() --- The architecture is not supported!" << std::endl;
    exit(-1);
#endif
}
