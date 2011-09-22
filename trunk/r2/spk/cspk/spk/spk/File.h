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
#ifndef SPKFILE_H
#define SPKFILE_H

#include <string>

class File
{
    static char _delimiter;
	std::string _path;
    std::string _filename;
public:
    File();
    File(const File&);
    File(const std::string& p, const std::string& n);
    File(const std::string& p, const char* n);
    File(const char* p, const std::string& n);
    File(const char* p, const char* n);
    ~File();

    void setPath(const std::string&);
    void setName(const std::string&);

    const std::string getPath() const ;
    const std::string getName() const;
    const std::string getFullname() const;

    File&  operator=(const File&);
    bool   operator!=(const File& right);
    friend std::ostream& operator<<(std::ostream& s, const File& f);
private:
    const std::string buildPath(const std::string& p) const;

};
#endif
