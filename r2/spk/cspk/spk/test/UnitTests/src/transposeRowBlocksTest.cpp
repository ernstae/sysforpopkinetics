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
 * File: transposeRowBlocksTest.cpp
 *
 *
 * Unit test for transposeRowBlocks.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/transposeRowBlocks.h"
#include "transposeRowBlocksTest.h"
#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/getMulRows.h"
#include "../../../spk/transpose.h"

using namespace CppUnit;
using SPK_VA::valarray;

static const int MAX = 9;

using namespace std;

void transposeRowBlocksTest::setUp()
{
    // initializations
}
void transposeRowBlocksTest::tearDown()
{
    // clean up
}

Test* transposeRowBlocksTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "transposeRowBlocksTest" );

    // duplicate the following example for each test case, replacing the case name.

    suiteOfTests->addTest(new TestCaller<transposeRowBlocksTest>(
                          "varyNcFixingNrN", 
                          &transposeRowBlocksTest::varyNcFixingNrN));
    suiteOfTests->addTest(new TestCaller<transposeRowBlocksTest>(
                          "varyNrFixingNcN", 
                          &transposeRowBlocksTest::varyNrFixingNcN));
    suiteOfTests->addTest(new TestCaller<transposeRowBlocksTest>(
                          "varyNFixingNcNr", 
                          &transposeRowBlocksTest::varyNFixingNcNr));
    suiteOfTests->addTest(new TestCaller<transposeRowBlocksTest>(
                          "accessByIndex", 
                          &transposeRowBlocksTest::accessByIndex));
    suiteOfTests->addTest(new TestCaller<transposeRowBlocksTest>(
                          "testValarrayVersion", 
                          &transposeRowBlocksTest::testValarrayVersion));

    return suiteOfTests;
}



void transposeRowBlocksTest::varyNcFixingNrN()
{
    int x=3;  // multiplier
    int nc; // A #columns
    int nr; // A #rows
    int n;  // a block #rows
    DoubleMatrix B;
    // Vary nc while fixing nr and n
    for(nr=x, nc=MAX, n=x; nr<=MAX; nr*=x)
    {
        DoubleMatrix A(nr, nc);
        nr = A.nr();
        nc = A.nc();
        double* a = A.data();

        for(int i=0; i<nr*nc; i++)
        {
            a[i]=i+1;
        }
        B = doit(A,n);
    }
}
void transposeRowBlocksTest::varyNrFixingNcN()
{
    int x=3;  // multiplier
    int nc; // A #columns
    int nr; // A #rows
    int n;  // a block #rows
    DoubleMatrix B;
        // vary nr while fixing nc and n
    for(nr=x, nc=MAX, n=x; nr<=MAX; nr*=x)
    {
        DoubleMatrix A(nr, nc);
        nr = A.nr();
        nc = A.nc();
        double* a = A.data();

        for(int i=0; i<nr*nc; i++)
        {
            a[i]=i+1;
        }

        B = doit(A,n);
    }
}
void transposeRowBlocksTest::varyNFixingNcNr()
{
    int x=3;  // multiplier
    int nc; // A #columns
    int nr; // A #rows
    int n;  // a block #rows
    DoubleMatrix B;

    //vary n while fixing nc and nr
    for(n=x, nc=x, nr=MAX; n<=MAX; n*=x)
    {
        DoubleMatrix A(nr, nc);
        nr = A.nr();
        nc = A.nc();
        double* a = A.data();

        for(int i=0; i<nr*nc; i++)
        {
            a[i]=i+1;
        }

        B = doit(A,n);
    }
}
void transposeRowBlocksTest::accessByIndex()
{
    int x=3;  // multiplier
    int nc; // A #columns
    int nr; // A #rows
    int n;  // a block #rows
    DoubleMatrix B;
    // Loop & access-by-index method
    for(int i=0; i<1000; i++)
    {
        for(nr=x, nc=MAX, n=x; nr<=MAX; nr*=x)
        {
            DoubleMatrix A(nr, nc);
            nr = A.nr();
            nc = A.nc();
            double* a = A.data();

            for(int i=0; i<nr*nc; i++)
            {
                a[i]=i+1;
            }
            B = compAgainst(A,n);
        }
    }
}
const DoubleMatrix transposeRowBlocksTest::doit(DoubleMatrix& A, int n)
{
    int nr = A.nr();
    int nc = A.nc();
    double* a = A.data();

    for(int i=0; i<nr*nc; i++)
    {
        a[i]=i+1;
    }
    
    DoubleMatrix B = transposeRowBlocks(A,n);
    DoubleMatrix C = compAgainst(A,n);


    for( int j=0; j<B.nr()*B.nc(); j++ )
    {
        CPPUNIT_ASSERT_EQUAL(B.data()[j], C.data()[j]);
    }
    return B;
}

const DoubleMatrix transposeRowBlocksTest::compAgainst(const DoubleMatrix &A, int n)
{
    // #of blocks
    int nb = A.nr() / n;
    int nc = A.nc();
    int nr = A.nr();

    DoubleMatrix C(nb*nc, n);
    C.fill(0);

    DoubleMatrix block(n,nc);
    block.fill(0);

    DoubleMatrix tblock(nc,n);
    tblock.fill(0);

    DoubleMatrix Mesh(nr,1);
    double* mesh=Mesh.data();
    const double* a = A.data();
    double* b = block.data();
    double* c = C.data();
    double* tb= tblock.data();

    // going over each block
    for(int k=0; k<nb; k++)
    {
        // obtain a subblock and transpose it
        Mesh.fill((double)false);
        for(int i=k*n; i<k*n+n; i++)
            mesh[i] = (double)true;

        block = getMulRows(A, Mesh);    // this is good
        tblock = transpose(block);
		tb= tblock.data();
        
        // marge the already-transposed subblock into the final matrix
        // the subblock has nc by nr dimensions
        for(int j=0; j<n; j++)
        {
            int cK = (nc*k)+(j*nc*nb);
            for(int i=0; i<nc; i++)
            {
                c[i+cK] = tb[i+j*nc];
            }
        }
    }
    return C;
}

void transposeRowBlocksTest::testValarrayVersion()
{
  int k_i, m_i, n_i;
  const int k = 2;
  const int m = 2;
  const int n = 3;

  //
  // A = 
  //  [ 1.1100000000000000e+002 1.1200000000000000e+002 1.1300000000000000e+002 ]
  //  [ 1.2100000000000000e+002 1.2200000000000000e+002 1.2300000000000000e+002 ]
  //  [ 2.1100000000000000e+002 2.1200000000000000e+002 2.1300000000000000e+002 ]
  //  [ 2.2100000000000000e+002 2.2200000000000000e+002 2.2300000000000000e+002 ]
  //
  DoubleMatrix A( k * m, n );
  double * a = A.data();
  for( k_i=0; k_i<k; k_i++ )
  {
    for( n_i=0; n_i<n; n_i++ )
    {
      for( m_i=0; m_i<m; m_i++ )
      {
        int index  = (m * k_i) + (m * k * n_i) + m_i;
        int val = (k_i+1) * 100 + (m_i+1) * 10 + (n_i+1);
        a[ index ] = val;
      }
    }
  }

  //
  // B =
  //  [ 1.1100000000000000e+002 1.2100000000000000e+002 ]
  //  [ 1.1200000000000000e+002 1.2200000000000000e+002 ]
  //  [ 1.1300000000000000e+002 1.2300000000000000e+002 ]
  //  [ 2.1100000000000000e+002 2.2100000000000000e+002 ]
  //  [ 2.1200000000000000e+002 2.2200000000000000e+002 ]
  //  [ 2.1300000000000000e+002 2.2300000000000000e+002 ]
  //
  valarray<double> B = transposeRowBlocks( A.toValarray(), n, m );

}
