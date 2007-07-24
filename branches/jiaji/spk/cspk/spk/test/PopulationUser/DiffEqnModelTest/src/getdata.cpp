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
// This source code is stored in the file getdata.cpp
# include "../../../../spk/SpkValarray.h"
# include <fstream>
# include <iostream>

# include "getdata.h"

using SPK_VA::valarray;

//
// This version of getdata() reads the data faile for the first pass.
// If fails to open the file specified, it returns false.
// 
bool getdata(
  const char *const name,
  int&         M,
  int&         Nsum)
{
	 std::ifstream file( name );
	 if( !file.good() )
     {
         std::cerr << "Failed to open " << name << std::endl;
         return false;
     }
	 int ilast = 0, inext = 0, k = 0;
	 double gammai, ti, yi, wi;
	 file >> inext >> gammai >> ti >> yi >> wi;
	 while( file.good() )
	 {	 assert( inext == ilast + 1 );
		 assert( ilast < M || M == 0);
		 int j = 0;
		 while( ! file.eof() && inext == ilast + 1 )
		 {	 assert( k < Nsum || Nsum == 0 );
			 k++;
			 j++;
			 file >> inext >> gammai >> ti >> yi >> wi;
		 }
		 ilast++;
	 }
	 assert( M == 0 );
	 assert( Nsum == 0 );
	 M  = ilast;
	 Nsum = k;

     return true;
}

//
// This version of getdata() reads the data faile for the second pass.
// If fails to open the file specified, it returns false.
// 
bool getdata(
  const char *const name,
  int           M,
  int           Nsum,
  valarray<int>&   N,
  valarray<double>& gamma,
  valarray<double>& w,
  valarray<double>& t,
  valarray<double>& y )
{
	 std::ifstream file( name );
	 if( !file.good() )
     {
         std::cerr << "Failed to open " << name << std::endl;
         return false;
     }
	 int ilast = 0, inext = 0, k = 0;
	 double gammai, ti, yi, wi;
	 file >> inext >> gammai >> ti >> yi >> wi;
	 while( file.good() )
	 {	 assert( inext == ilast + 1 );
		 {	 gamma[ilast] = gammai * wi;
			 w[ilast] = wi;
		 }
		 int j = 0;
		 while( ! file.eof() && inext == ilast + 1 )
		 {	 assert( k < Nsum || Nsum == 0 );
			 if( Nsum != 0 )
			 {	 t[k] = ti;
				 y[k] = yi;
			 }
			 k++;
			 j++;
			 file >> inext >> gammai >> ti >> yi >> wi;
		 }
			 N[ilast] = j;
		 ilast++;
	 }
	 assert( Nsum == 0 || Nsum == k );
	 M  = ilast;
	 Nsum = k;
     return true;
}

