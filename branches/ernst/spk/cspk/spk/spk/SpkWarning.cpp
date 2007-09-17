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
 * File: SpkWarning.cpp
 *
 *
 * Provide a mechanism to log warnings.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/


/*************************************************************************
 *
 * Class: SpkWarning
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin SpkWarning$$
$spell 
  Spk
  const
  linenum
  cpp
  iostream
  buf
  cout
  endl
$$

$section Logging Warnings$$

$index SpkWarning$$
$index warning$$
$index error, warning$$

$table
$bold Constructors:$$   $cend  
N/A (all methods are static)
$tend

See also: $xref/SpkError//SpkError/$$, $xref/SpkException//SpkException/$$
$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This class provides a mechanism to log warnings (as opposed to errors, which are fatal)
in a file created under the current directory from which the program is invoked.

$head Static Public Methods$$

$syntax/
static int push(const char * /message/, int /linenum/, const char * /filename/)
/$$
adds a new entry, composed of $italic message$$, $italic linenum$$ and $italic filename$$
to the end of the log.  It returns the number of warnings accumulated so far upon the
successful operation.  If, for some reason, the operation fails, it throws an
$xref/SpkException//SpkException/$$ object.

$syntax/

static int bytes()
/$$
returns the total size of warning information in bytes without counting the terminating NULL.

$syntax/

static const char* filename()
/$$
returns the pointer to the name of the warning log file if any warning has been generated.
Otherwise returns NULL.

$syntax/

static char * stream( char * const /message/, int /bytes/);
/$$
places the all warning information as a NULL-terminated string
in the chronological order (the oldest appears first) in which the warnings were detected.
$pre

$$
If $italic bytes$$ is smaller than $math%x%$$, where $math%x%$$ is the size of
warning information in bytes, then only the first $math%x%$$ bytes of
information is copied into $italic message$$.
If $math%x%$$ is smaller than $italic bytes$$, then only the first
$math%x%$$ elements of $italic message$$ will be occupied with meaningful 
data, with the $math%x+1%$$ -th element containing NULL.
If no warning has been generated, it returns immediately without disturbing $italic message$$.
$pre

$$
Upon the successful completion of the operation, $italic message$$
contains a series of warning data sets separated by an extra empty line.  
Each set is composed of a warning message, 
a line number at which the warning was generated and a filename in which
the warning was generated.  Each of these pieces of information is
led by a token (see below) and the pieces and token are each terminated by a new line character.

  $codep
  linenum
  ...line number at which the warning was generated...
  filename
  ...filename in which the warning was generated...
  warning
  ...actual warning message...
  $$

$syntax/

static void print()
/$$
sends the all warning information to the standard output in the same
format described in $code stream(char * const, int)$$ section.

$head Example$$
If you compile, link, and run the following program, 
from the directory named $italic C:\myExample$$,
The program's main is stored in a file called $italic C:\myExamples\myWarning.cpp$$
:
$codep

  #include <iostream>
  #include "SpkWarning.h"

  int main()
  {
    SpkWarning::push( "The sky is falling!", __LINE__, __FILE__ );
    SpkWarning::push( "The ground is splitting!", __LINE__, __FILE__ );

    char buf* = new char[ SpkWarning::bytes()+1 ];
    cout << SpkWarning::stream(buf, SpkWarning::bytes()) << endl;
    delete [] buf;
  }

$$
then it will display the following when it is run:
$codep

  linenum
  7
  filename
  myWarning.cpp
  warning
  The sky is falling!

  linenum
  8
  filename
  myWarning.cpp
  warning
  The ground is splitting!

$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * This is an mean-while solution to a permanent warning 
 * message passing mechanism, which should be revisited
 * after Beta 1.0 release.
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <fstream>
#include <string>
#include "SpkWarning.h"
#include "SpkException.h"
using namespace std;
/*------------------------------------------------------------------------
 * Class definition
 *------------------------------------------------------------------------*/
int SpkWarning::cnt   = 0;
int SpkWarning::size_in_bytes = 0;

const char * SpkWarning::logfile             = "warning.log";
const char * SpkWarning::MESSAGE_FIELD_TITLE = "warning";
const char * SpkWarning::LINENUM_FIELD_TITLE = "linenum";
const char * SpkWarning::FILENAME_FIELD_TITLE= "filename";

int SpkWarning::push(const char* message, int line, const char* filename)
{
  char buf[20];
  snprintf(buf, 20, "%d", line);
  size_in_bytes += strlen(MESSAGE_FIELD_TITLE) + 1 + strlen(message)           + 1
                + strlen(LINENUM_FIELD_TITLE)  + 1 + strlen(buf)               + 1
                + strlen(FILENAME_FIELD_TITLE) + 1 + strlen(filename)          + 1 + 1;

  FILE *file;
  try{ 
    if( cnt == 0 )
      file = fopen( logfile, "w" );
    else
      file = fopen(logfile, "a");
    if( file != NULL )
    {
      fprintf(file, "%s\n%d\n%s\n%s\n%s\n%s\n\n", 
        LINENUM_FIELD_TITLE,
        line,
        FILENAME_FIELD_TITLE,
        filename,
        MESSAGE_FIELD_TITLE,
        message);
      fclose(file);
      ++cnt;
    }
    else
    {
      fclose(file);
      throw SpkException(SpkError::SPK_STD_ERR, logfile, __LINE__, __FILE__); 
    }
  }
  catch( std::exception& e )
  {
    throw SpkException( e, logfile, __LINE__, __FILE__);
  }
  return cnt;
}

void SpkWarning::print(void)
{
  char buf[128];

  FILE *file = fopen(logfile, "r");
  if( file != NULL )
  {
    while( !feof(file) )
    {
      if( fgets(buf, 127, file) != NULL )
        cout << buf;
      std::fill(buf, buf+128, '\0');
    }

    fclose(file);
  }
}

char * SpkWarning::stream(char * const str, int size)
{
  FILE *file = fopen(logfile, "r");
  if( file != NULL )
  {
    char * buf = new char[size+1];
    std::fill(buf, buf+size, '\0');
    for( int i=0; i<size-1, i<size_in_bytes, feof(file)==0; i++ )
    {
      buf[i] = (char)fgetc(file);
    }
    buf[size] = '\0';
    strcpy(str, buf);
    delete [] buf;
  }
  return str;
}
const char * SpkWarning::filename(void)
{
  if( cnt == 0 )
    return NULL;
  else
    return logfile;
}
