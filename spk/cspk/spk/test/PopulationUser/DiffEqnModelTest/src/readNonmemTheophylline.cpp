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
 * Function: readNonmemTheophylline
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin readNonmemTheophylline$$
$spell 
    const
    Nsum
    spk
    valarray
    bool
    yi
    gammai
    readNonmemTheophylline
    Theophylline
    wi
    cout
    endl
    dat
    num
    std
    iostream
    cpp
$$

$section Reading Theophylline Data File$$

$index readNonmemTheophylline$$

$table
$bold Prototype:$$   $cend  
$syntax/int readNonmemTheophylline(
  const char const *        /name/,
  int&                      /M/,
  int&                      /Nsum/,
  SPK_VA::valarray<int>&    /N/,
  SPK_VA::valarray<double>& /gamma/,
  SPK_VA::valarray<double>& /w/,
  SPK_VA::valarray<double>& /t/,
  SPK_VA::valarray<double>& /y/ )
/$$
$tend

$fend 25$$


$head Description$$
The routine $code readNonmemTheophylline$$ reads a data file that has the following form:

$center
$table
    ------------------ $cend -------------- $cend   ------------------------------------------------------------- $rend
    $bold Name$$       $cend $bold Column$$ $cend   $bold Description$$                                           $rend
    ------------------ $cend -------------- $cend   ------------------------------------------------------------- $rend
    $math%i%$$         $cend      1         $cend   index for this individual                                     $rend
    $math%gammai/wi%$$ $cend      2         $cend   dose divided by weight for $math%i%$$th individual            $rend
    $math%ti(j)%$$     $cend      3         $cend   time for $math%j%$$th measurement for $math%i%$$th individual $rend
    $math%yi(j)%$$     $cend      4         $cend   value of $math%j%$$ measurement for $math%i%$$th individual   $rend
    $math%wi %$$       $cend      5         $cend   weight for $math%i%$$th individual                            $rend
$tend
$$
The individual indices are in increasing order and do not skip any values.
For each individual, the first line specifies the initial (dose/weight) and wight values, while
placing zeros in the other columns.  The following lines contains the time and measurement value.
In order to make it easier to read the file, zeros are placed for (dose/weight) and weight in the other
records for each individual.  The first thirteen lines of the file appear as follows:

$codep
         1      4.02      0.        0.0      79.6
         1      0.0       0.0       0.74      0.0
         1      0.0       0.25      2.84      0.0
         1      0.0       0.57      6.57      0.0
         1      0.0       1.12     10.5       0.0
         1      0.0       2.02      9.66      0.0
         1      0.0       3.82      8.58      0.0
         1      0.0       5.1       8.36      0.0
         1      0.0       7.03      7.47      0.0
         1      0.0       9.05      6.89      0.0
         1      0.0      12.12      5.94      0.0
         1      0.0      24.37      3.28      0.0
         2      4.4       0.        0.       72.4
$$

A specification for the arguments to $code readNonmemTheophylline$$ and the actual C++ code follow below.

$head Arguments$$

$syntax/

* /name/
/$$
This argument is an input to $code readNonmemTheophylline$$.
It is a $code NULL$$ terminated character array specifying the name of
the file that contains the data set (see description above).

$syntax/

& /M/
/$$
This argument is an output for $code readNonmemTheophylline$$.  
It is an integer passed by reference.
The referenced integer will be set to the number of individuals found in the data file.

$syntax/

& /Nsum/
/$$                                                     
This argument is an output for $code readNonmemTheophylline$$.
It is an integer passed by reference.
The referenced integer will be set to the number of lines found in the data file.
The value is assumed to be the same as the total number of measurements found in the data file.

$math%
                M
               ---
  i.e.         \\ 
       Nsum =  /   N(i)
               --- 
               i=1
%$$
where $math%N(i)%$$ is the number of measurements for $math%i%$$th individual.

$syntax/

& /N/
/$$
This argument is an output for $code readNonmemTheophylline$$.
The $code valarray$$ object will be re-sized to $math%M%$$ and 
the $math%i%$$th element of the $code valarray$$ is set to the number of
measurement values corresponding to the $math%i%$$th individual.
The original contents of the array will be lost.

$syntax/

& /gamma/
/$$
This argument is an output for $code readNonmemTheophylline$$.
The $code valarray$$ object will be re-sized to $math%M%$$ and 
the $math%i%$$th element of the $code valarray$$ is set to the dose
corresponding to the $math%i%$$th individual.
The original contents of the array will be lost.

$syntax/

& /w/
/$$
This argument is an output for $code readNonmemTheophylline$$.
The $code valarray$$ object will be re-sized to $math%M%$$ and 
the $math%i%$$th element of the $code valarray$$ is set to the weight
corresponding to the $math%i%$$th individual.
The original contents of the array will be lost.

$syntax/

& /t/
/$$
This argument is an output for $code readNonmemTheophylline$$.
The $code valarray$$ object will be re-sized to $math%Nsum%$$ and 
the $math%k%$$th element of the $code valarray$$ is set to the time
corresponding to the $math%k%$$th line in the data file.
The original contents of the array will be lost.

$syntax/

& /y/
/$$
This argument is an output for $code readNonmemTheophylline$$.
The $code valarray$$ object will be re-sized to $math%Nsum%$$ and 
the $math%k%$$th element of the $code valarray$$ is set to the measurement
corresponding to the $math%k%$$th line in the data file.
The original contents of the array will be lost.
  
$head Example$$
Given a data file named $italic test.dat$$ containing the following records:
$codep
         1      4.02      0.0       0.0      79.6
         1      0.0       0.0       0.74      0.0
         1      0.0       0.25      2.84      0.0
         1      0.0       0.57      6.57      0.0
$$
the following program when compiled, linked and run,
$codep

    #include <iostream>
    #include "SpkValarray.h"
    #include "readNonmemTheophylline.h"

    void main()
    {
        using SPK_VA::valarray;
        using std::cout;
        using std::endl;

        char filename[] = "test.dat";

        //
        // Prepare place holders.  The sizes and contents will be modified by readNonmemTheophylline().
        //
        int numIndividuals;
        int sumMeasurements;
        valarray<int> numMeasurements;
        valarray<double> gammas;
        valarray<double> weights;
        valarray<double> times;
        valarray<double> measurements;

        readNonmemTheophylline( filename, numIndividuals, sumMeasurements, numMeasurements, gammas, weights, times, measurements );

        cout << "# of individuals:                         " << numIndividuals  << endl;
        cout << "total # of measurements:                  " << sumMeasurements << endl;
        cout << "# of measurements for each individual:    " << numMeasurements << endl;
        cout << "gamma for each individual:                " << gammas          << endl;
        cout << "weight for each individual:               " << weights         << endl;
        cout << "measurement schedule for each individual: " << times           << endl;
        cout << "measurements for all individuals:         " << measurements    << endl; 
    }

$$
will display the following when it is run:
$codep

# of individuals:                         1
total # of measurements:                  3
# of measurements for each individual:    { 3 }
gamma for each individual:                { 319.992 }
weight for each individual:               { 79.6 }
measurement schedule for each individual: { 0, 0.25, 0.57 }
measurements for all individuals:         { 0.74, 2.84, 6.57 }
$$

$head Source Code$$
This following source code is stored in the file $italic readNonmemTheophylline.cpp$$

$code

$verbatim%readNonmemTheophylline.cpp%0%//BEGIN%//END%1%$$

$$
$end
*/
//BEGIN
# include "../../../../spk/SpkValarray.h"
# include "readNonmemTheophylline.h"
# include <fstream>
# include <iostream>
# include <cassert>

#pragma warning( disable : 4786 )
# include <vector>

using SPK_VA::valarray;

//
//
//
// Given 1 <= i <= M and Ni is the number of measurements taken from the i-th individual,
// the data set for a population is organized in a text file in the following format:
//
//     Individual Index    Gamma(i)/Weight(i)     Time         Measurement   Initial Weight
//     -------------------------------------------------------------------------------------
//     i                   gamma(i)               t0=0.0       y(i,t0)       w(i)
//     i                   0.0                    t1(i)        y(i,t1)       0.0
//     i                   0.0                    t2(i)        y(i,t2)       0.0
//     i                   0.0                    t3(i)        y(i,t3)       0.0
//                                        .                                     
//                                        .                                     
//                                        .                                     
//     i                   0.0                    tNi(i)       y(i,tNi)      0.0
//
//
// If fails to open the file specified, it returns false.
// 

int readNonmemTheophylline(
  const char *const name,
  int&              M,
  int&              Nsum,
  valarray<int>&    N,
  valarray<double>& gamma,
  valarray<double>& w,
  valarray<double>& t,
  valarray<double>& y )
{
  //
  // Temporary place holders when reading records from the data file.
  //
  double gammai;
  double ti;
  double yi;
  double wi;
  int numTotalMeasurements;
  int numMeasurements;
  int numInds;
  double curInd, nextInd;
  std::vector<double> tempGamma;
  std::vector<double> tempTime;
  std::vector<double> tempData;
  std::vector<double> tempWeight;
  std::vector<int>    tempN;

  //
  // Open the data file for read.  If fails, displays an error message
  // to the standard error and force quit the program.
  //
  std::ifstream file( name );
  if( !file.good() )
  {
    std::cerr << "Failed to open " << name << std::endl;
    exit(-1);
  }


  //-------------------------------------------------------------------------
  // The following loop goes through the file once and stores records into 
  // vectors as it goes.  It uses variable-length vectors 
  // because we do not know how many individuals'
  // data or how many records per individual are recoreded in the file.
  // At the end of the loop we know these numbers.
  //-------------------------------------------------------------------------
  //
  // Read the very first line from the file, which corresponds to
  // the first individual's heading.
  //
  file >> curInd >> gammai >> ti >> yi >> wi;
  for( nextInd=curInd+1, numInds=0, numTotalMeasurements=0; file.good(); numInds++, nextInd = curInd + 1 )
  {	
    assert( gammai != 0.0 );
    assert( ti == 0.0 );
    assert( yi == 0.0 );
    assert( wi != 0.0 );
    //
    // Save the heading information; i.e. gamma(i)/w(i) and w(i)
    //
    tempGamma. push_back( gammai * wi );
    tempWeight.push_back( wi );
    
    
    //
    // Now read lines following the heading.
    // At the last iteration of this loop contains the next individual's 
    // heading line.
    //
    file >> curInd >> gammai >> ti >> yi >> wi;
    for( numMeasurements=0; file.good() && ( curInd == nextInd - 1 ); numMeasurements++, numTotalMeasurements++  )
    {
        tempTime.push_back( ti );
        tempData.push_back( yi );
        file >> curInd >> gammai >> ti >> yi >> wi;
    }
    tempN.push_back( numMeasurements );
  }
  file.close();
  //
  // Make sure what we got is what expected.
  //
  assert( curInd == numInds );
  assert( tempGamma.size()  == curInd );
  assert( tempWeight.size() == curInd );
  assert( tempTime.size()   == numTotalMeasurements );
  assert( tempData.size()   == numTotalMeasurements );

  //
  // Store the number of individuals and total number of measurements
  // to the placeholders given by the caller.
  //
  M    = numInds;
  Nsum = numTotalMeasurements;
  assert( M    > 0 );
  assert( Nsum > 0 );
  
  //
  // Resize the user-given arrays to the sizes figured out during the file read process.
  //
  N.resize( M );
  gamma.resize( M );
  w.resize( M );
  t.resize( Nsum );
  y.resize( Nsum );

  //
  // Copy information from temporary arrays to the user-given place holders.
  //
  std::copy( tempN.     begin(), tempN.     end(), &N[0]     );
  std::copy( tempGamma. begin(), tempGamma. end(), &gamma[0] );
  std::copy( tempWeight.begin(), tempWeight.end(), &w[0]     );
  std::copy( tempTime.  begin(), tempTime.  end(), &t[0]     );
  std::copy( tempData.  begin(), tempData.  end(), &y[0]     );
  return M;
}
//END
