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
/*
 *************************************************************************************
 * DoubleMatrix.h
 *
 * This file contains declarations of two matrix class:
 * 1. DoubleMatrix class
 *    This is a home-grown matrix class.
 * 2. MTL based DoubleMatrix class
 *    The class name is the same as #1 but this is derived from MTL matrix class.
 *
 * Compiler determines which one to pick based on a preprossessor definition.
 * Unless _MTLMATRIX is not specified, the home-grown DoubleMatrix class 
 * will be selected.  
 *
 *************************************************************************************
 */
#ifndef DOUBLEMATRIX_H
#define DOUBLEMATRIX_H

#ifndef _MTLMATRIX
/*
 **********************************************************************************
 *
 * home-grown DoubleMatrix class
 *
 **********************************************************************************
*/


// Review Goddard 3/13/00: Where is the OMHelp source for the top-level
// "dmclass*.htm" pages? I suggest it should be here. I can't find it
// anywhere.

    // Respond Sachiko 4/3/00: Apply from now on: 
    // They are directly found in .omh files.

// Review Goddard 3/14/00: Suggest: If any operators +, +=, -, -=, /, /=, *=
// are provided, all should be here. Users will expect to see them.

    // Respond Sachiko 4/3/00: Defer: Should revisit during the design phase
    // for the distributed version.

// Review Goddard 3/14/00: Suggest: Declarations for nearly all of the 
// DoubleMatrix "Utility Functions" should be moved to this header.
// It doesn't matter that they aren't member functions (operator* doesn't
// need to be a member either), they are part of the class interface and
// hence belong in the same header.
// REF: Sutter, Exceptional C++, Item 32.

    // Respond Sachiko 4/3/00: Defer: Should revisit during the design phase
    // for the distributed version.

// Review Goddard 3/13/00: Suggest: Leave memory overrun guards to the
// library. They don't belong in a "business domain" class like this one.
// This usage amounts to mixed-domain cohesion.

    // Respond Sachiko 4/3/00: Defer: Check compiler's options and prepare
    // for the next version.
#pragma warning ( disable : 4786 )

#include <iostream>
#include <cassert>
#include <cfloat>

#include "SpkValarray.h"
#include "BlockAlloc.h"
#include "SpkException.h"

class DoubleMatrix{
private:
    int        rows;        // The number of rows
    int        cols;        // The number of columns
    double    *ptr;         // column-major matrix data
	int       *nref;        // number of references to ptr
	static const double RFPK_MAT_LOWER;
    static const double RFPK_MAT_UPPER;

    inline void create(int n, const double val=DBL_MIN)
	{
		assert(n > 0);
		ptr   = (double *) BlockAlloc( sizeof(double) * (n + 2) + sizeof(int) );
		
        //
        // [ Commented out by Sachiko, 09/26/2002 ]
        // assert( ptr != 0 );
        //
        if( ptr == NULL )
        {
            char errmsg[] = "BlockAlloc() failed to allocate more memory.";
            throw SpkException( SpkError::SPK_INSUFFICIENT_MEM_ERR, errmsg, __LINE__, __FILE__ );
        }
		ptr[0]      = RFPK_MAT_LOWER;
        std::fill(ptr+1, ptr+1+n, val);
        ptr[n + 1]  = RFPK_MAT_UPPER;
		nref        = (int*)(ptr + n + 2);
		*nref       = 1;
	}

    inline void create(int n, const double *vals)
	{
		assert(n > 0);
        ptr   = (double *) BlockAlloc( sizeof(double) * (n + 2) + sizeof(int) );
        //
        // [ Commented out by Sachiko, 09/26/2002 ]
        // assert( ptr != 0 );
        //
        if( ptr == NULL )
        {
            char errmsg[] = "BlockAlloc() failed to allocate more memory.";
            throw SpkException( SpkError::SPK_INSUFFICIENT_MEM_ERR, errmsg, __LINE__, __FILE__ );
        }
        std::copy(vals, vals+n+2, ptr);
		nref        = (int*)(ptr + n + 2);
		*nref       = 1;
	}

	inline void create(int n, const SPK_VA::valarray<double>& array)
	{
		assert(n > 0);
        ptr   = (double *) BlockAlloc( sizeof(double) * (n + 2) + sizeof(int) );
        //
        // [ Commented out by Sachiko, 09/26/2002 ]
        // assert( ptr != 0 );
        //
        if( ptr == NULL )
        {
            char errmsg[] = "BlockAlloc() failed to allocate more memory.";
            throw SpkException( SpkError::SPK_INSUFFICIENT_MEM_ERR, errmsg, __LINE__, __FILE__ );
        }
		ptr[0]      = RFPK_MAT_LOWER;
		for( int i = 0; i < n; i++ )
			ptr[ i + 1 ] = array[ i ];
		ptr[n + 1]  = RFPK_MAT_UPPER;
		nref        = (int*)(ptr + n + 2);
		*nref       = 1;
	}

public:
	// Constructors
    DoubleMatrix();
    explicit DoubleMatrix( const char* filename );
    DoubleMatrix( const DoubleMatrix &dmatA );
    DoubleMatrix( int nr, int nc );
    DoubleMatrix( double a );
	explicit DoubleMatrix( const SPK_VA::valarray<double>& array, int ncols = 1 );
    
	// Destructor
	~DoubleMatrix() throw();

	// For column vector only
	DoubleMatrix& operator=( const SPK_VA::valarray<double>& array );

	// Convert from SPK_VA::valarray
	DoubleMatrix& fromValarray( const SPK_VA::valarray<double>& array, int ncols = 1 );

	// Convert to SPK_VA::valarray and destroy itself.
    void toValarray( SPK_VA::valarray<double>& array );

    // Create a SPK_VA::valarray object while retaining the DoubleMatrix object itself.
    const SPK_VA::valarray<double> toValarray( ) const;

    // return a pointer to a double precision vector of length
    // nr * nc containing the elements of the matrix
	// return null if the matrix is empty
    inline double *data()
    {
        if(!ptr)
			return 0;
		assert( *nref > 0 );
		if( *nref > 1 )
		{
			(*nref)--;
			create(rows * cols, ptr);
		}
		return ptr + 1;
    }

	// return a pointer to a double precision vector of length
    // nr * nc containing the elements of the matrix
	// return null if the matrix is empty
    inline const double *data() const
    {
        if(!ptr) return 0;
        return ptr + 1;
    }

    // resize the matrix
    void resize(int nr, int nc);

    // number of rows in matrix
    inline int nr() const{ return rows; }

    // number of columns in matrix
    inline int nc() const{ return cols; }

    // Fill the matrix with the specified double-precision number
    void fill( double val );

    // displays the elements in a matrix
    void print() const;

    // true if the matrix is empty (0 by 0, 0 by n, n by 0)
    bool isEmpty() const;

    // returns the reference counter value
    int getRefCount() const;

    /*
     * Assign the right hand matrix to the left and match their dimensions
     * No alteration made to the original (right) matrix
     */
    DoubleMatrix &operator=( const DoubleMatrix &dmatB);
};

/**************************END OF HOME-GROWN MATRIX CLASS**********************************/

/*
 **********************************************************************************
 *
 * MTL based DoubleMatrix class
 *
 ***********************************, _MTLMATRIX***********************************************
*/
#else
#pragma warning( disable : 4786 )  

#include <iostream>
#include <list>


#include "mtl/mtl.h"
#include "mtl/matrix.h"


typedef mtl::matrix< double, 
                mtl::rectangle<>, 
                mtl::dense<>, 
                mtl::column_major>::type dMatrix;

class DoubleMatrix : public dMatrix{


private:
    static int cntall;      // counts the total number of instanciations so far

private:
    double    *ptr;         // column-major matrix data
    int        rows;        // The number of rows
    int        cols;        // The number of columns
    int        maxlen;      // The length of the data array (longest so far)
    unsigned int id;        // object id
    bool isStatic;          // indicates this object was declared as a static object

    inline void icrCntAll() throw()
    {
        id = cntall;
        cntall++;
    }

public:
    // Constructors
    DoubleMatrix();
    DoubleMatrix(const char* filename);
    DoubleMatrix(const DoubleMatrix &dmatA);
    DoubleMatrix(int nr, int nc);
    DoubleMatrix(double a);

    // Destructors
    ~DoubleMatrix() throw();

    // return a pointer to a double precision vector of length
    // nr * nc containing the elements of the matrix
    inline double *data()
    {
        /*
        if( isEmpty() )
            return 0;
        */
        return ptr;
        
    }

    inline const double *data() const
    {
        /*
        if( isEmpty() )
            return 0;
        */
        return ptr;
    }

    // resize the matrix
    void resize(int nr, int nc);

    // number of rows in matrix
    inline int nr() const{ return rows; }

    // number of columns in matrix
    inline int nc() const{ return cols; }

    // Fill the matrix with the specified double-precision number
    void fill( double val );

    // displays the elements in a matrix
    void print() const;

    // true if the matrix is empty (0 by 0, 0 by n, n by 0)
    bool isEmpty() const;

    // returns the counter value
    static int getCount();

    /*
     * Assign the right hand matrix to the left and match their dimensions
     * No alteration made to the original (right) matrix
     */
    DoubleMatrix &operator=( const DoubleMatrix &dmatB);

};
    
#endif

/**************************END OF MTL MATRIX CLASS**********************************/
std::ostream& operator<<(std::ostream&, const DoubleMatrix &);
std::istream& operator>>(std::istream&, DoubleMatrix &);
const DoubleMatrix operator*(const DoubleMatrix& A, const DoubleMatrix& B);

#endif

