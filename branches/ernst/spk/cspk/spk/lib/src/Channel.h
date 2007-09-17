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
#ifndef CHANNEL_H
#define CHANNEL_H

#pragma warning( disable : 4786 )
#include "File.h"
#include "IndDataPackage.h"
#include "SpkException.h"

typedef File PackageHandle;
/*------------------------------------------------------------------
 * MasterEndChannel
 *------------------------------------------------------------------*/
class MasterEndChannel 
{
    static int _counter;
    int    _id;
    std::string _strID;
    File   _tempfile;
    File   _workdir;
    File   _getFilter;

    MasterEndChannel();
    MasterEndChannel(const MasterEndChannel& right);
    MasterEndChannel& operator=(const MasterEndChannel& right);
public:
    MasterEndChannel(const File& sharedDiskSpace);
    ~MasterEndChannel();
    void  flush() const;
    void  open();
    void  close();
    const IndOutputDataPackage get(bool wait=true) const;
    const PackageHandle post(const IndInputDataPackage& inpack, bool wait=false) const;
    bool  write(const std::string& filename, const std::string& message);
    bool  hasReached( const PackageHandle& packageID ) const;
};

/*------------------------------------------------------------------
 * NodeEndChannel
 *------------------------------------------------------------------*/
class NodeEndChannel 
{
    int    _id;
    std::string _strID;
    File   _tempfile;
    File   _workdir;
    File   _getFilter;

    NodeEndChannel();
    NodeEndChannel(const NodeEndChannel& right);
    NodeEndChannel& operator=(const NodeEndChannel& right);

public:
    NodeEndChannel(const File& sharedDiskSpace);
    ~NodeEndChannel();
    void  open();
    void  close();
    const IndInputDataPackage get() const;
    const PackageHandle post(const IndOutputDataPackage& pack) const;
    const PackageHandle post(SpkException& err) const;
};

#endif
