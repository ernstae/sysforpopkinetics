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
#ifndef STATUSLIST_H
#define STATUSLIST_H

#pragma warning( disable : 4786 )

#include <iostream>
#include <ostream>
#include <ctime>
#include <vector>
#include <string>
#include "IndDataPackage.h"
#include "Channel.h"

class StatusList  
{
	StatusList(const StatusList& right);
    StatusList& operator=(const StatusList& right);
public:
    StatusList(int num_individuals);
	~StatusList();

	void  issued(int inx, const PackageHandle& handle, const IndInputDataPackage& inpack, bool checkout=true);
    void  checkedOut(int inx);
	void  completed(int inx);

    bool  isCheckedOut(int) const;
    bool  isCompleted(int) const;

	int   numCompleted() const;
	int   nextExpired() const;
    int   nextOldest() const;

    int   size() const;
    const IndInputDataPackage getInPack(int inx) const;
    const PackageHandle getHandle(int inx) const;

    //
    // for debug use
    //
    bool operator!=(const StatusList &right); 

private:
    time_t zero;
    class StatusRecord
    {
    private:
        time_t issuedTime;
        time_t checkedOutTime;
        time_t completedTime;
        IndInputDataPackage inPack;
        PackageHandle handler;

    public:
        StatusRecord();
        StatusRecord(const StatusRecord&);
        StatusRecord(const time_t& created_time);
        ~StatusRecord();

        void checkedOut(const time_t&);
        void issued(const time_t&, const PackageHandle& handle, const IndInputDataPackage&);
        void completed(const time_t&);
        bool isIssued() const;
        bool isCheckedOut() const;
        bool isCompleted() const;
        bool ellapstedMoreThan(int timeout_sec) const;
        int  howLongInProcess() const;
        const IndInputDataPackage getInPack() const;
        const PackageHandle getHandle() const;

        StatusRecord& operator=(const StatusRecord& right);
        bool operator==(const StatusRecord& right) const;
        bool operator!=(const StatusRecord& right) const;
        friend std::ostream& operator<<(std::ostream& stream, const StatusRecord& rec)
        {
            char *strTM;

            strTM  = asctime(gmtime(&rec.issuedTime));
            stream << "Issued at:    " << (rec.issuedTime   ==0? "Never\n" : strTM);
            strTM  = asctime(gmtime(&rec.completedTime));
            stream << "Completed at: " << (rec.completedTime==0? "Never\n" : strTM);
            stream << std::ends;
            return stream;
        }
    };

    int nP;
    int timeout_sec;
    int lastExpired;
    std::vector<StatusRecord> records;

	StatusList();

	const bool isExpired(int inx) const;
    const StatusRecord get(int inx) const;
};

#endif
