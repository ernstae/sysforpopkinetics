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
#if !defined SPKWARNING_H
#define SPKWARNING_H

#include <iostream>

class SpkWarning
{
  static const char *MESSAGE_FIELD_TITLE;
  static const char *LINENUM_FIELD_TITLE;
  static const char *FILENAME_FIELD_TITLE;

  static const char *logfile;
  static int cnt;
  static int size_in_bytes;

  SpkWarning(){}
  ~SpkWarning(){}
  SpkWarning(const SpkWarning&){}
  SpkWarning& operator=(const SpkWarning&){}

public:
  static int push( const char* message, int line, const char* filename);
  static char * stream(char * const, int);
  static void print(void);
  static const char * filename(void);
  inline static int bytes(void)            { return size_in_bytes; }
  inline static void clean(void)           { remove(logfile); cnt=0; size_in_bytes=0; }
  inline static int count(void)            { return cnt; }

};

#endif
