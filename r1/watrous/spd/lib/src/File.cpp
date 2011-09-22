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
 * File: File.cpp
 *
 *
 * A container that holds a path and a filename.  The path
 * is not necessarily absolute or relative; it depends solely on
 * what was given by user.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Class: File
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin File$$
$spell
   const bool fullname concatenate ostream str wildcard abc
$$

$section File class$$

$index File$$

$table
$bold Prototype:$$   $cend  
$syntax/File::File()
/$$
$syntax/File::File(const string& /path/, const string& /filename/)
/$$
$syntax/File::File(const string& /path/, const char* /filename/)
/$$
$syntax/File::File(const char* /path/, const string& /filename/)
/$$
$syntax/File::File(const char* /path/, const char* /filename/)
/$$

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
$code File$$ provides a convenient way of maintaining and accessing the path and name of a file.
The class does not validate the name or path.
In this way, user can use $code File$$ object as a mere place holder for
accessing these strings (path and filename) in ways as you like.  
Either or both strings can be empty as well.

$head Constructors$$

$syntax/File::File()
/$$
is the default constructor.
$pre

$$
$syntax/File::File(const string& /path/, const string& /filename/)
/$$
$syntax/File::File(const string& /path/, const char* /filename/)
/$$
$syntax/File::File(const char* /path/, const string& /filename/)
/$$
$syntax/File::File(const char* /path/, const char* /filename/)

/$$
$italic path$$ specifies the path to $italic filename$$ that may or may not be
terminated by a path delimiter such as back-slush (\) for Windows_.  You can
obtain your local operation system's path delimiter by using a method in 
$tref System$$ (in $code System$$ class).
$pre

$$
If $italic path$$ is not terminated by a delimiter,
then the system always adds a delimiter to the tail automatically.
The file specified by $italic filename$$ does not have to exist physically.  It can
be, for example, empty ("") or wildcard (*.*, *, 80abc*...).


$head Public Members$$

$syntax/
public void setPath(const string& /str/)
/$$
sets (or replace) the file path with $italic str$$.  If $italic str$$ is terminated by a path delimiter,
it will be removed.  The path does not have to be necessarily valid.  
In other words, user can specify anything here such as empty ("").

$syntax/

public void setName(const string& /str/)
/$$
sets (or replace) the file name with $italic str$$.  The file name does not have to be necessarily valid.  
In other words, user can specify anything here such as empty ("") and wildcard (*.*, *, abc*...).

$syntax/

public const string getPath() const
/$$
returns the path to a file always terminated by a path delimiter (even if user did not provide it
with).

$syntax/

public const string getName() const
/$$
returns $italic filename$$ exactly as given at the time of construction.

$syntax/

public const string getFullname() const
/$$
returns a string concatenating the values returned by $code getPath()$$ (terminated by a path delimiter)
 and $code getName()$$, in the preserved order.

$head Operator Overloads$$
$syntax/
File& File::operator=(const File& /right/)
/$$
copies information from $italic right$$ into $code this$$.  Self-assignment is prohibited.
If such operation is attempted, the system terminates.

$syntax/

bool File::operator !=(const File& /right/)
/$$
returns true if either or both the path or/and filename does not exactly match 
(as strings) between $code this$$ and $italic right$$; false otherwise.

$syntax/

friend public ostream& operator<<(ostream& /stream/, const File& /file/) const
/$$
returns a reference to an ostream object that contains the value returned by $code getFullname()$$.

$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <string>
#include <fstream>
#include <cassert>
#include "File.h"

using namespace std;

char File::_delimiter = '/';


/*--------------------------------------------------------------------
 * Constructors
 *--------------------------------------------------------------------*/
File::File()
{
}
File::File(const File& right)
: _path(right._path), _filename(right._filename)
{
}
File::File(const string& p, const string& n)
: _filename(n), _path("")
{
    _path = buildPath(p);
}
File::File(const string& p, const char* n)
: _filename(n), _path("")
{
    _path = buildPath(p);
}
File::File(const char* c_p, const string& n)
: _filename(n), _path("")
{
    _path = buildPath(c_p);
}
File::File(const char* c_p, const char* c_n)
: _filename(c_n), _path("")
{
    _path = buildPath(c_p);
}

/*--------------------------------------------------------------------
 * Destructor
 *--------------------------------------------------------------------*/
File::~File()
{
}

/*--------------------------------------------------------------------
 * Public members
 *--------------------------------------------------------------------*/

void File::setName(const string& str)
{
    _filename = str;
}
void File::setPath(const string& str)
{
    _path = buildPath(str);
}
const string File::getName() const
{
    return _filename;
}
const string File::getPath() const
{
    return _path;
}
const string File::getFullname() const
{
    return _path + _filename;
}
/*--------------------------------------------------------------------
 * Operator overloads
 *--------------------------------------------------------------------*/
File& File::operator=(const File& right)
{
    assert( &right != this );

    _path     = right._path;
    _filename = right._filename;
    return *this;
}
bool File::operator !=(const File& right)
{
    if( _path != right._path || _filename != right._filename )
        return true;
    else
        return false;
}
ostream& operator<<(ostream& s, const File& f)
{
    s << (f._path.empty()? "" : f._path) << f._filename;
    return s;
}

/*--------------------------------------------------------------------
 * Private members
 *--------------------------------------------------------------------*/
const string File::buildPath(const string& p) const
{
  int endstr = p.size()-1;

  char mydelimiter;
  if( p.find("\\", 0) != string::npos)
    mydelimiter = '\\';
  else if( p.find("/", 0) != string::npos)
    mydelimiter = _delimiter;
  else
    mydelimiter = _delimiter;

  if(p.find(mydelimiter, endstr) != string::npos)
  {
    return p;
  }
  else
  {
    if( p.find(":", endstr) == string::npos )
      return p+mydelimiter;
    else
      return p;
  }
}
