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
 * File: UTranTimesSymKronSymTimesU_xTest.cpp
 *
 *
 * Test cases for expectedHessian
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cmath>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/UTranTimesSymKronSymTimesU_x.h"
#include "../../../spk/UTranTimesSymKronSymTimesU.h"
#include "../../../spk/symmetrize.h"
#include "../../../spk/rvec.h"
#include "../../../spk/AkronBtimesC.h"
#include "../../../spk/transpose.h"
#include "../../../spk/multiply.h"
#include "../../../spk/replaceJth.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/symmetrize.h"
#include "../../../spk/add.h"
#include "../../../spk/rvecInverse.h"
#include "../../../spk/getCol.h"
#include "../../../spk/subtract.h"
#include "../../../spk/identity.h"
#include "../../../spk/AkronItimesC.h"
#include "../../../spk/IkronBtimesC.h"
#include "../../../spk/transposeRowBlocks.h"

#include "UTranTimesSymKronSymTimesU_xTest.h"

using namespace std;
using namespace CppUnit;

void UTranTimesSymKronSymTimesU_xTest::setUp()
{
    _m.push_back(0);  _k.push_back(0);  _p.push_back(0);
    _m.push_back(0);  _k.push_back(0);  _p.push_back(1);
    _m.push_back(0);  _k.push_back(1);  _p.push_back(0);
    _m.push_back(0);  _k.push_back(1);  _p.push_back(1);
    _m.push_back(1);  _k.push_back(0);  _p.push_back(0);
    _m.push_back(1);  _k.push_back(0);  _p.push_back(1);
    _m.push_back(1);  _k.push_back(1);  _p.push_back(0);
    _m.push_back(1);  _k.push_back(1);  _p.push_back(1);

    _m.push_back(1);  _k.push_back(1);  _p.push_back(1);
    _m.push_back(1);  _k.push_back(1);  _p.push_back(3);
    _m.push_back(1);  _k.push_back(3);  _p.push_back(1);
    _m.push_back(1);  _k.push_back(3);  _p.push_back(3);
    _m.push_back(3);  _k.push_back(1);  _p.push_back(1);
    _m.push_back(3);  _k.push_back(1);  _p.push_back(3);
    _m.push_back(3);  _k.push_back(3);  _p.push_back(1);
    _m.push_back(3);  _k.push_back(3);  _p.push_back(3);

    _n = _m.size();
}
void UTranTimesSymKronSymTimesU_xTest::tearDown()
{
    // clean up
}

Test* UTranTimesSymKronSymTimesU_xTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("UTranTimesSymKronSymTimesU_xTest");

  suiteOfTests->addTest(new TestCaller<UTranTimesSymKronSymTimesU_xTest>(
		       "testUTranTimesSymKronSymTimesU_x", 
                       &UTranTimesSymKronSymTimesU_xTest::testUTranTimesSymKronSymTimesU_x));
  /*suiteOfTests->addTest(new TestCaller<UTranTimesSymKronSymTimesU_xTest>(
                       "testUTranTimesSymKronSymTimesU_xEx", 
                       &UTranTimesSymKronSymTimesU_xTest::testUTranTimesSymKronSymTimesU_xEx));
  */
    return suiteOfTests;
}

static void replaceIth( DoubleMatrix &target, int ith, const DoubleMatrix &data )
{
    assert(target.nc() == data.nc());
    assert( ith >= 0 && ith < target.nr());

    double *pTarget = target.data();
    const double *pData   = data.data();
    int j;

    for( j=0; j<target.nc(); j++ )
    {
        pTarget[j*target.nr()+ith] = pData[j];
    }
    return;
}

void UTranTimesSymKronSymTimesU_xTest::testUTranTimesSymKronSymTimesU_x()
{
    using namespace std;
    int i,j,l,o;
    for(int cnt=0; cnt<_n; cnt++)
    {
        int m = _m[cnt];
        int mm = m*m;
        int k = _k[cnt];
        int p = _p[cnt];
        if( m < 1 )
            continue;
        if( k < 1 )
            continue;
        if( p < 1 )
            continue;

        // Init V:symmetric m by m 
        DoubleMatrix V(m,m);
        V.fill(0);
        for( j=0; j<m; j++ )
        {
            for( i=j; i<m; i++ )
            {
                // fill the lower triangle and diagonal elements of V, not just diagonal
                V.data()[i+j*m] = i+j*m;
            }
        }
        symmetrize(V,V);

        // Init Uarray[]: k number of symmetric m by m
        vector<DoubleMatrix> Uarray(k);
        DoubleMatrix U(mm, k);
        for( l=0; l<k; l++ )
        {
            for( j=0; j<m; j++ )
            {
                for( i=j; i<m; i++ )
                {
                    Uarray[l].resize(m,m);
                    Uarray[l].data()[i+j*m] = i+j*m+l;
                }
            }
            symmetrize(Uarray[l], Uarray[l]);
            replaceJth(U, l, rvec(Uarray[l]));

        }

        // Create C: k by k
        DoubleMatrix C(k,k);
        C.fill(0);

        // Create A[]: k number of m by m
        DoubleMatrix A[k];
/*
cout << "V=" << V;
cout << "U=" << U;
*/
        UTranTimesSymKronSymTimesU(V, U, k, C, A);
    
        // Init V_x[]: p number of symmetric m by m
        vector<DoubleMatrix> V_xArray(p);
        DoubleMatrix V_x(mm, p);
        for(l=0; l<p; l++)
        {
            for( j=0; j<m; j++ )
            {
                for( i=j; i<m; i++ )
                {
                    V_xArray[l].resize(m,m);
                    V_xArray[l].data()[i+j*m] = i+j*m-l;
                }
            }
            symmetrize(V_xArray[l], V_xArray[l]);
            replaceJth(V_x, l, rvec(V_xArray[l]));
        }

        // Init U_xarray[]: k number of mm by p
        vector<DoubleMatrix> U_xarray(k);
        DoubleMatrix Ux_y(mm*p,k);
        DoubleMatrix Ui_x(mm,p);
        DoubleMatrix Ui_xj(m,m);
        for(l=0; l<k; l++)
        {
            for(o=0; o<p; o++)
            {
                for( j=0; j<m; j++ )
                {
                    for( i=j; i<m; i++ )
                    {
                        Ui_xj.resize(m,m);
                        Ui_xj.data()[i+j*m] = i+j*m-2*o;
                    }
                }
                symmetrize(Ui_xj, Ui_xj);
                replaceJth(Ui_x, o, rvec(Ui_xj));
            }
            U_xarray[l] = Ui_x;
            replaceJth(Ux_y,l,rvec(Ui_x));
        }
        DoubleMatrix Uy_x = transposeRowBlocks(Ux_y, p);
        CPPUNIT_ASSERT(Uy_x.nr() == mm*k);
        CPPUNIT_ASSERT(Uy_x.nc() == p);


        DoubleMatrix C_x(k*k,p);
        UTranTimesSymKronSymTimesU_x(V, V_x, U, Ux_y, p, A, C_x);


        DoubleMatrix expectedC_x(k*k,p);
        DoubleMatrix expectedC_o(k,k);

        std::vector<DoubleMatrix> expectedC_xArray;
        expectedC_xArray.resize(p);

        DoubleMatrix Im = identity(m);

        DoubleMatrix rvecV_o, U_o, U_x, Uj_o, Ui_o, Cij_o, rvecAjTran, rvecAiTran;
        DoubleMatrix term1, term2, term3, term4, term01, term02;
        
        for(o=0; o<p; o++)
        {
            //cout << "o=" << o << endl;
            double *dC_o = expectedC_o.data();

            rvecV_o = getCol(V_x, o);
            CPPUNIT_ASSERT(rvecV_o.nr() == mm);
            CPPUNIT_ASSERT(rvecV_o.nc() == 1);

            U_o = rvecInverse(getCol(Uy_x,o),k);
            CPPUNIT_ASSERT(U_o.nr() == mm);

            for(j=0; j<k; j++)
            {
                //cout << "j=" << j << endl;

                CPPUNIT_ASSERT(A[j].nr() == m);
                CPPUNIT_ASSERT(A[j].nc() == m);
                transpose(rvec(A[j]), rvecAjTran);
                CPPUNIT_ASSERT(rvecAjTran.nr() == 1);
                CPPUNIT_ASSERT(rvecAjTran.nc() == mm);

                Uj_o = getCol(U_o,j);
                CPPUNIT_ASSERT(Uj_o.nr() == mm);
                CPPUNIT_ASSERT(Uj_o.nc() == 1);

                term1 = AkronItimesC(V, Im, Uj_o);
                CPPUNIT_ASSERT(term1.nr() == mm);
                CPPUNIT_ASSERT(term1.nc() == 1);

                CPPUNIT_ASSERT(Uarray[j].nr() == m);
                CPPUNIT_ASSERT(Uarray[j].nc() == m);
                term2 = IkronBtimesC(Im, Uarray[j], rvecV_o);
                CPPUNIT_ASSERT(term2.nr() == mm);
                CPPUNIT_ASSERT(term2.nc() == 1);

                for(i=j; i<k; i++)
                {
                    //cout << "i=" << i << endl;

                    CPPUNIT_ASSERT(A[i].nr() == m);
                    CPPUNIT_ASSERT(A[i].nc() == m);
                    transpose(rvec(A[i]), rvecAiTran);
                    CPPUNIT_ASSERT(rvecAiTran.nr() == 1);
                    CPPUNIT_ASSERT(rvecAiTran.nc() == mm);

                    multiply(rvecAiTran, add(term1,term2), term01);
                    CPPUNIT_ASSERT(term01.nr() == 1);
                    CPPUNIT_ASSERT(term01.nc() == 1);

                    term3 = AkronItimesC(Uarray[i], Im, rvecV_o);
                    CPPUNIT_ASSERT(term3.nr() == mm);
                    CPPUNIT_ASSERT(term3.nc() == 1);

                    Ui_o = getCol(U_o,i);
                    term4 = IkronBtimesC(Im, V, Ui_o);
                    CPPUNIT_ASSERT(term4.nr() == mm);
                    CPPUNIT_ASSERT(term4.nc() == 1);

                    multiply(rvecAjTran, add(term3, term4), term02);
                    CPPUNIT_ASSERT(term02.nr() == 1);
                    CPPUNIT_ASSERT(term02.nc() == 1);

                    add(term01, term02, Cij_o);
                    CPPUNIT_ASSERT(Cij_o.nr() == 1);
                    CPPUNIT_ASSERT(Cij_o.nc() == 1);

                    // place each element (derivative of C with respect to x1, x2...xp).
                    dC_o[i+j*k] = Cij_o.data()[0];
                
                 }        
            }
            symmetrize(expectedC_o, expectedC_o);
            replaceJth(expectedC_x, o, rvec(expectedC_o));
        }

        for(j=0; j<p; j++)
        {
            for(i=0; i<k*k; i++)
            {
	      double expected = expectedC_x.data()[i+j*k*k];
	      double actual   = C_x.data()[i+j*k*k];
	      CPPUNIT_ASSERT_DOUBLES_EQUAL( expected, actual, (expected==0.0? 0.001 : fabs(expected)*0.001 ) );
            }
        }        
    }
}
