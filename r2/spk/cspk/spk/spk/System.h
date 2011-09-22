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
#ifndef SYSTEM_H
#define SYSTEM_H

#include <string>
#include "File.h"

class System
{
public:
    static const std::string machine();
    static const std::string whoami();
    static int   howmanyCPU();
    static char  pathDelimiter();
    
    //static const File   work();

    static const File pwd();
    static void  del(  const File& file );
    static void  del(const char *target);
    static void  ren(  const File& before, const std::string& after );
    static void  ren(const char* source, const char* after);
    static bool  exist(const File& f);
    static bool  exist(const char* fullname);
    static const std::string findfirst(const char* filter);

    static unsigned sleep( unsigned seconds );
    static int   pid();
    //    static int   beginthread( void( __cdecl * start_address )( void * ), unsigned stack_size, void * arglist );
    //static void  endthread();
    static int   spawnAsyncProcess( const std::string& executable_name, char* argv[] );
    static int   spawnSyncProcess(  const std::string& executable_name, char* argv[] );
    static int   exec(const std::string& cmd);
	static int   createDirectory( const char* path );

private:
    static std::string _MACHINE;
    static std::string _USER;
    static char _DELIMIT;
    static int  _NUMBER_OF_PROCESSORS;

    //
    // Depreciated
    //
    System();
    System(const System& right);
    ~System();
};
#endif
