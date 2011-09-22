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
 * File: EqIndModel.cpp
 *
 * Definitions of EqIndModel class members.
 *
 * Author: Jiaji Du
 *
 * Reviewed by Sachiko, 09/25/2002
 *
 *   Comment: Generally, using notations like alp and b in this model
 *            was very confusing.  
 *            Perhaps consider using different notations
 *            and clarify the mapping between them and conventional
 *            alp and b might help the reader understand little
 *            easier.  Refer my comment found in EqIndModel.omh too.
 *   Follow-up:
 *            Jiaji, sorry, I hope you didn't mind that I fixed it
 *            myself... It helped me going through the code.
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: EqIndModel  ( constructor )
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin EqIndModel_EqIndModel$$
$spell
    Model model
const
EqInd
initializes 
Spk
valarray
bool
$$

$section Constructor$$

$index EqIndModel$$
$cindex \Constructor$$

    $comment [ Sachiko, 09/25/2002 ]
      The arguments, N, does not logically belong to the model.
      The model is a function of alp and b, but not of mesurements.
      The size of a measurement vector for an individual
      can be derived indirectly from other things.
      Providing extra (non-logically connected) arguments to
      a model constructor would be okay if the model is a user-defined
      subclass of SpkModel.  But, you may want to give a careful
      thought on an argument list when it comes to a core
      library component.
    $$

    $comment [ Sachiko, 09/25/2002 ]
      The arguments, nA, does not logically belong to the model.
      It can be derived from the alp vector itself when it is given
      or derived from something else (which may not be the most
      efficient way).
      Providing extra (non-logically connected) arguments to
      a model constructor would be okay if the model is a user-defined
      subclass of SpkModel.  But, you may want to give a careful
      thought on an argument list when it comes to a core library
      component.
    $$

$table
$bold Prototype:$$   $cend  
$syntax/EqIndModel::EqIndModel(
                         SpkModel*                          /pModel/,
                         const valarray<int>&       /N/,
                         const valarray<double>&    /bStep/,
                         int                                /nA/ )
/$$
$tend

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This constructor takes four arguments, 
$italic pModel$$, $italic N$$, $italic bStep$$ and $italic nA$$,
to initialize class member data.
$pre

$$
$head Arguments$$
$syntax/
/pModel/ 
/$$
   $comment [ Sachiko, 09/25/2002 ]
      The kind of SpkModel that can be passed into here
      must be dependent on all three parameters, alp/b/i.
      It has to be stated here.  Look the specification for
      the SpkModel argument passed to ppkaOpt() as an example;
      it referes to a section of glossary.
   $$

The $code SpkModel*$$ $italic pModel$$ is a pointer to a 
$xref/SpkModel//SpkModel/$$ class 
instance that is used for population analysis. 

$syntax/

/N/ 
/$$
    $comment [ Sachiko, 09/25/2002 ]
        This argument does not seem to belong to the model.  Look the comment
        in the Prototype section above.
    $$

The $code valarray<int>$$ $italic N$$ contains an array of 
int values, which specifies the number of data measurements for each 
individual.  The number of elements in $italic N$$ must be equal to 
$math%nInd%$$, the number of individuals in the original 
$xref/SpkModel//SpkModel/$$ class object.

$syntax/

/bStep/ 
/$$

The $code valarray<double>$$ $italic bStep$$ contains an array of 
double values, which specifies the step size used for approximating

     $comment [ Sachiko, 09/25/2002 ]
         "... with respect to the individual parameters" is 
         confusing.  This whole model treats alp as the individual
         parameter.  It took a while to understood which b you are talking about.
         Mentioning that "the length of this bStep should be the same
         as the size of individual parameter" does not clarify it.
         It could be taken that User can chop/stretch any vector just
         to match the size.
     $$
the derivatives with respect to the individual parameters.
The size of $italic bStep$$ is equal to the length of 
the individual parameter vector of the original 
$xref/SpkModel//SpkModel/$$ class object.

$syntax/

/nA/ 
/$$
    $comment [ Sachiko, 09/25/2002 ]
        This argument does not seem to belong to the model.  
        Look the comment
        in the Prototype section above.
    $$

The $code int$$ $italic nA$$ specifies the number of population 
parameters of the original $xref/SpkModel//SpkModel/$$ class object.

$end
*/

/*************************************************************************
 *
 * Function: doSetIndPar( const valarray<double>& )
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin EqIndModel_setIndPar$$
$spell
    Model model
const 
Ind
EqInd
Spk
valarray
$$

$section Set Individual Parameter$$

$index EqIndModel, doSetIndPar$$
$index Equivalent individual model, set individual parameter$$

$table
$bold Prototype:$$   $cend 
$syntax/
void EqIndModel::doSetIndPar( const valarray<double>&  /b/ ) 
/$$ 
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This function provides a means of setting individual parameters for the 
EqIndModel class object.  The individual parameters of the EqIndModel class object are the  
population parameters of the original $xref/SpkModel//SpkModel/$$ class object.
$pre

$$
$head Arguments$$
$syntax/
b
/$$
is an array of double values.  The size of the array is the number of the 
population parameters of the original $xref/SpkModel//SpkModel/$$ class object. 

$end
*/

/*************************************************************************
 *
 * Function: doDataMean( valarray<double>& f ) const
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin EqIndModel_dataMean$$
$spell
    Model model
const
EqInd 
fn
Spk
valarray
cmath
$$

$section Model for the Data Mean$$

$index EqIndModel, doDataMean$$
$index Equivalent individual model, mean of data$$

$table
$bold Prototype:$$   $cend
$syntax/
void EqIndModel::doDataMean( valarray<double>& /f/ ) const 
/$$ 
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This function evaluates the mean of all individual's data at
a currently set population parameters of the original 
$xref/SpkModel//SpkModel/$$ class object
and at zero value of individual parameters of the original 
$xref/SpkModel//SpkModel/$$ class object.
$pre

$$
$head Arguments$$
$syntax/
f
/$$
shall, upon a successful completion of evaluation,
contain a resulting vector of size $math%nY%$$, where $math%nY%$$ is the
number of all individual's data, in the following form.  

  $comment [ Sachiko, 09/25/2002 ]
  In the overall description found in EqIndModel.omh, you say:

                                  ~
        f  (alp, b) = f(alp, 0) + d f(alp, 0)b,
         FO                        b 

  Now, all the sudden, f is a function of only alp and the right hand side
  is different from the above approximation.  The relationship, 
  which can be explained by giving an explanation for this
  particular implementation (treating a population as a big individual problem),
  has to be explained.
  $$

$math%

          /  f1(alp, 0)  \\
f(alp) =  |   :          |
          \\  fn(alp, 0)  /

%$$
where f1,...,fn are the data means of individual 1,...,n of the original 
$xref/SpkModel//SpkModel/$$ class object, respectively. 
alp is the population parameter vector of the original
$xref/SpkModel//SpkModel/$$ class object and n is the number of 
individuals of the original $xref/SpkModel//SpkModel/$$ class object.

$end
*/

/*************************************************************************
 *
 * Function: doDataMean_indPar( valarray<double>& f_b ) const
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin EqIndModel_dataMean_indPar$$
$spell
    Model model 
bool
const
EqInd
fn
ind
Spk
valarray
cmath
$$

$section Model for the Derivative of Data Mean with respect to Individual Parameter$$

$index EqIndModel, doDataMean_indPar$$
$index Equivalent individual model, data mean derivative$$

$table
$bold Prototype:$$   $cend
$syntax/
bool EqIndModel::doDataMean_indPar( valarray<double>& /f_b/ ) const 
/$$ 
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This function evaluates the derivative of the mean of all individual's data 
with respect to the population parameter vector of the 
$xref/SpkModel//SpkModel/$$ class object 
at a currently set population parameters of the original 
$xref/SpkModel//SpkModel/$$ class object
and at zero value of individual parameters of the original 
$xref/SpkModel//SpkModel/$$ class object.
$pre

$$
The evaluation result of $code doDataMean_indPar()$$
shall be placed in $italic f_b$$.  It returns $math%true%$$ if $italic f_b$$ 
contains at least one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$head Arguments$$
$syntax/
f
/$$
shall, upon a successful completion of evaluation,
contain a resulting matrix of size $math%nY%$$ by $math%nAlp%$$, where 
$math%nY%$$ is the number of all individual's data and $math%nAlp%$$ is the 
number of population parameters of the original
$xref/SpkModel//SpkModel/$$ class object, in the following form. 

  $comment [ Sachiko, 09/25/2002 ]
  In the overall description found in EqIndModel.omh, you say:

                                  ~
        f  (alp, b) = f(alp, 0) + d f(alp, 0)b,
         FO 
         b 
  Now, all the sudden, f is a function of only alp and the right hand side
  is different from the above approximation.  The relationship, 
  which can be explained by giving an explanation for this
  particular implementation (treating a population as a big individual problem),
  has to be explained.
  $$

$math%

            /  f1_alp(alp, 0)  \\
f_b(alp) =  |   :              |
            \\  fn_alp(alp, 0)  /

%$$
where f1_alp,...,fn_alp are the derivatives of data mean of individual 1,...,n of 
the original $xref/SpkModel//SpkModel/$$ class object with respect to alp, 
respectively.  alp is the population parameter vector of the original
$xref/SpkModel//SpkModel/$$ class object and n is the number of 
individuals of the original $xref/SpkModel//SpkModel/$$ class object.

$end
*/


/*************************************************************************
 *
 * Function: doDataVariance( valarray<double>& R ) const
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin EqIndModel_dataVariance$$
$spell
    Model model
bn 
const
fn
EqInd
Spk
valarray
$$

$section Model for the Data Variance$$

$index EqIndModel, doDataVariance$$
$index Equivalent individual model, variance of data$$

$table
$bold Prototype:$$   $cend
$syntax/
void EqIndModel::doDataVariance( valarray<double>& /R/ ) const 
/$$ 
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This function evaluates the equivalent data variance of the EqIndMode 
class object at a currently set population parameters of the original 
$xref/SpkModel//SpkModel/$$ class object
and at zero value of individual parameters of the original 
$xref/SpkModel//SpkModel/$$ class object.
$pre

$$
$head Arguments$$
$syntax/
f
/$$
shall, upon a successful completion of evaluation,
contain a resulting matrix of size $math%nY%$$ by $math%nY%$$, 
where $math%nY%$$ is the
number of all individual's data, in the following form.  

  $comment [ Sachiko, 09/25/2002 ]
  In the overall description found in EqIndModel.omh, you say:

        R  (alp, b) = R(alp, 0).
         FO

  Now, all the sudden, R is a function of only alp and the right hand side
  is different from the above approximation.  The relationship, 
  which can be explained by giving an explanation for this
  particular implementation (treating a population as a big individual problem),
  has to be explained.
  $$

$math%

          /  ~                   ~             T                                                              \\
          |  d  f1(alp, 0)D(alp)[d  f1(alp, 0)]  + R1(alp, 0)                   0                             |
          |   b1                  b1                                                                          |
          |                                              .                                                    |
R(alp) =  |                                                 .                                                 |
          |                                                    .                                              |
          |                                                  ~                   ~             T              |
          |                0                                 d  fn(alp, 0)D(alp)[d  fn(alp, 0)]  + Rn(alp, 0) |
          \\                                                   bn                  bn                          /

%$$
      
where f1,...,fn are the data means of individual 1,...,n of the original 
$xref/SpkModel//SpkModel/$$ class object.  
b1,...,bn are the individual parameters of individual 1,...,n of the original
$xref/SpkModel//SpkModel/$$ class object. 
alp is the population parameter vector of the original
$xref/SpkModel//SpkModel/$$ class object and n is the number of 
individuals of the original $xref/SpkModel//SpkModel/$$ class object.
R1,...,Rn are data variances of individual 1,...,n of the original $xref/SpkModel//SpkModel/$$ class object
and D is the individual parameter variance of the original $xref/SpkModel//SpkModel/$$ class object.
Symbol ~ indicates taking central difference.

$end
*/

/*************************************************************************
 *
 * Function: doDataVariance_indPar( valarray<double>& R_b ) const
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin EqIndModel_dataVariance_indPar$$
$spell
    Model model
bn 
const
EqInd
fn
ind
Spk
valarray
bool
$$

$section Model for the Derivative of Data Variance with respect to Individual Parameter$$

$index EqIndModel, doDataVariance_indPar$$
$index Equivalent individual model, data variance derivative$$

$table
$bold Prototype:$$   $cend
$syntax/
bool EqIndModel::doDataVariance_indPar( valarray<double>& /R_b/ ) const 
/$$ 
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This function evaluates the derivative of the equivalent data variance 
with respect to the population parameter vector of the 
$xref/SpkModel//SpkModel/$$ class object 
at a currently set population parameters of the original 
$xref/SpkModel//SpkModel/$$ class object
and at zero value of individual parameters of the original 
$xref/SpkModel//SpkModel/$$ class object.
$pre
$$
The evaluation result of $code doDataMean_indPar()$$
shall be placed in $italic R_b$$.  It returns $math%true%$$ if $italic f_b$$ 
contains at least one non-zero value.  If all zeros, it returns $math%false%$$.
$pre

$$
$head Arguments$$
$syntax/
f
/$$
shall, upon a successful completion of evaluation,
contain a resulting matrix of size $math%nY*nY%$$ by $math%nAlp%$$, 
where $math%nY%$$ is the number of all individual's data and 
$math%nAlp%$$ is the number of population parameters of the original
$xref/SpkModel//SpkModel/$$ class object in the following form.  

$math%

                  /  ~                   ~             T                                                              \\
                  |  d  f1(alp, 0)D(alp)[d  f1(alp, 0)]  + R1(alp, 0)                   0                             |
                  |   b1                  b1                                                                          |
                  |                                              .                                                    |
R_alp(alp) = d    |                                                 .                                                 |
              alp |                                                    .                                              |
                  |                                                  ~                   ~             T              |
                  |                0                                 d  fn(alp, 0)D(alp)[d  fn(alp, 0)]  + Rn(alp, 0) |
                  \\                                                   bn                  bn                          /

%$$

where f1,...,fn are the data means of individual 1,...,n of the original 
$xref/SpkModel//SpkModel/$$ class object.  
b1,...,bn are the individual parameters of individual 1,...,n of the original
$xref/SpkModel//SpkModel/$$ class object. 
alp is the population parameter vector of the original
$xref/SpkModel//SpkModel/$$ class object and n is the number of 
individuals of the original $xref/SpkModel//SpkModel/$$ class object.
R1,...,Rn are data variances of individual 1,...,n of the original $xref/SpkModel//SpkModel/$$ class object
and D is the individual parameter variance of the original $xref/SpkModel//SpkModel/$$ class object.
Symbol ~ indicates taking central difference.

$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <cassert>
#include "EqIndModel.h"
#include "SpkException.h"
#include "replaceSubblock.h"
#include "transpose.h"
#include "centdiff.h"
#include "ABA_x.h"
#include "transposeDerivative.h"
#include "transposeRowBlocks.h"
#include "isSymmetric.h"
#include "multiply.h"
#include "inverse.h"

using SPK_VA::valarray;
using SPK_VA::slice;

/*------------------------------------------------------------------------
 * Local Function Declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
    bool isNotEqual(const valarray<double>& x, const valarray<double>& y );
} // [End: unnamed namespace]

/*------------------------------------------------------------------------
 * Equivalent individual model class member definition
 *------------------------------------------------------------------------*/

EqIndModel::EqIndModel( SpkModel* pModel, 
                        const valarray<int>& N, 
                        const valarray<double>& bStep, 
                        int nA )
              : _pModel( pModel ), _N( N ), _bStep( bStep ), _nB( nA )
			    , isCachedRValid( false ), isCachedRInvValid( false )
{
    _nb   = _bStep.size();  // This is the size of little b.
    _nInd = _N.size();

    // 
    // [ Comment by Sachiko, 09/25/2002 ]
    // You can use valarray<T>::sum() here instead.
    //
    _nY = _N.sum();

    _b0.resize( _nb, 0.0 );

    //
    // The number of individuals is always 1 in this version of FO.
    //
//    pModel->selectIndividual(0);
}

//
// [ Added by Sachiko, 09/25/2002 ]
// The caller calls this to set a parameter that is considered
// as the random effect in this FO individual model,
// which is the fixed effect in the original population model.
//
void EqIndModel::doSetIndPar( const valarray<double>& foB )
{
	if( isNotEqual( _B, foB ) )
	{
            _B.resize( foB.size() );
	    _B = foB;
            _pModel->setPopPar( foB );
	    isCachedRValid = false;
	    isCachedRInvValid = false;
	}
}
//
// [ Added by Sachiko, 09/25/2002 ]
// Computes f(B), where B is, in this case, the parameter
// set by EqIndModel::doSetIndPar(B).
//
void EqIndModel::doDataMean( valarray<double>& foFOut ) const
{
  //
  // [ Comment by Sachiko, 09/25/2002 ]
  // As stated earlier, nY should not belong to the model.
  // Though it is tempting and nice to be able to use it to
  // pre-allocate the size of the big vector and to check the size of a vector
  // returned by fi(b) against the corresponding value stored in N,
  // I still think these two, nY and N, do not belong to the model.
  //
    foFOut.resize( _nY );
    //foFOut = 0.0;   // is this really needed?
    valarray<double> fiOut( _nY );


    //
    // [ Added by Sachiko, 09/25/2002 ]
    // Compute fi(B,0) for each individual, 
    // where B is indeed alp but has been fed as the random effect
    // to this individual model and form
    // a big vector that contains these results
    // sequentially.
    //
    //_pModel->setPopPar( _B );   // [ Added by Sachiko, 09/25/2002 ]
    //                            // Conceptually this statement should be placed here
    //                            // but shouldn't in practice because we know for
    //                            // sure _pModel's popPar has been set to _B already
    //                            // and hasn't been disturbed ever since.
    //
    _pModel->setIndPar( _b0 );
    for( int i = 0, startRow = 0; i < _nInd; i++ )
    {
        _pModel->selectIndividual( i );

        //
        // _pModel->setIndPar( _b0 )    // [ Added by Sachiko, 09/25/2002 ]
        //                              // Conceptually this statement should be place here
        //                              // but shouldn't in practice because we know for
        //                              // sure _pModel's popPar has been set to _B already
        //                              // and hasn't been disturbed ever since.

        _pModel->dataMean(fiOut);
        assert( fiOut.size() == _N[i] );

        replaceSubblock( foFOut, 1, fiOut, 1, startRow, 0 );
        startRow += _N[ i ];
    }
}

bool EqIndModel::doDataMean_indPar( valarray<double>& foF_B ) const
{
    foF_B.resize( _nY * _nB ); 
    //foF_B = 0.0;    // is this really needed?
    valarray<double> fi_BOut( 0.0, _nY * _nB );
    bool someAreNonZeros = false;


    //
    // [ Added by Sachiko, 09/25/2002 ]
    // Compute fi_b(B,0) for each individual, 
    // where B is indeed alp but has been fed as the random effect
    // to this individual model.
    //
    //_pModel->setPopPar( _B );   // [ Added by Sachiko, 09/25/2002 ]
    //                            // Conceptually this statement should be placed here
    //                            // but shouldn't in practice because we know for
    //                            // sure _pModel's popPar has been set to _B already
    //                            // and hasn't been disturbed ever since.
    //
    _pModel->setIndPar( _b0 );
    for( int i = 0, startRow = 0; i < _nInd; i++ )
    {
        _pModel->selectIndividual( i );

        //
        // _pModel->setIndPar( _b0 )    // [ Added by Sachiko, 09/25/2002 ]
        //                              // Conceptually this statement should be place here
        //                              // but shouldn't in practice because we know for
        //                              // sure _pModel's popPar has been set to _B already
        //                              // and hasn't been disturbed ever since.

        someAreNonZeros |= _pModel->dataMean_popPar(fi_BOut);

        assert( fi_BOut.size() == _N[ i ] * _nB );

        replaceSubblock( foF_B, _nB, fi_BOut, _nB, startRow, 0 );
        startRow += _N[ i ];
    }

    return someAreNonZeros;
}

//
// [ Added by Sachiko, 09/25/2002 ]
// Computes R(B), where B is, in this case, the parameter
// set by EqIndModel::doSetIndPar(B).
//
void EqIndModel::doDataVariance( valarray<double>& foROut ) const
{
  if( isCachedRValid )
    {
      foROut = cachedR;
      return;
    }
  
  foROut.resize( _nY * _nY, 0.0 );
  valarray<double> DOut( _nb * _nb );
  _pModel->indParVariance(DOut);
  assert( DOut.size() == _nb * _nb );
  assert( isSymmetric( DOut, _nb ) );
  
  valarray<double> fi_bOut, fi_bDfi_bOutT, temp;
  
  EqIndModelFunction fiOb( _pModel );
  
  //
  // [ Added by Sachiko, 09/25/2002 ]
  // Compute Ri(B,0) + fi_b(B,0) * D(B) * fi_b(B,0)'
  // where B is indeed alp but has been fed as the random effect
  // to this individual model.
  // Form a big matrix contains
  // these results as diagonal elements.
  //
  //
  //_pModel->setPopPar( _B );   // [ Added by Sachiko, 09/25/2002 ]
  //                            // Conceptually this statement should be placed here
  //                            // but shouldn't in practice because we know for
  //                            // sure _pModel's popPar has been set to _B already
  //                            // and hasn't been disturbed ever since.
  //
  //	_pModel->setIndPar( _b0 );
  for( int i = 0, startRow = 0; i < _nInd; i++ )
    {
      _pModel->selectIndividual( i );
      _pModel->setIndPar( _b0 );
      //
      // _pModel->setIndPar( _b0 )    // [ Added by Sachiko, 09/25/2002 ]
      //                              // Conceptually this statement should be place here
      //                              // but shouldn't in practice because we know for
      //                              // sure _pModel's popPar has been set to _B already
      //                              // and hasn't been disturbed ever since.
      
      //
      // [ Added by Sachiko, 09/25/2002 ]
      // Compute the approximation for fi_b( B, b(=0) ).
      //
      // Note that the approximation is with respect to the little b.
      //
      fi_bOut.resize( _N[ i ] * _nb );
      fi_bOut = centdiff<EqIndModelFunction>( fiOb, 1, _b0, _bStep ); 
      assert( fi_bOut.size() == _N[ i ] * _nb );
      
      _pModel->setIndPar( _b0 );
      
      valarray<double> RiOut( _N[ i ] * _N[ i ] );
      _pModel->dataVariance(RiOut);
      assert( isSymmetric( RiOut, _N[ i ] ) );
      assert( RiOut.size() == _N[ i ] * _N[ i ] );
      
      fi_bDfi_bOutT.resize( _N[ i ] * _N[ i ] );
      fi_bDfi_bOutT = multiply( multiply( fi_bOut, _nb, DOut, _nb ), _nb, transpose( fi_bOut, _nb ), _N[ i ] );
      
      //
      // [ Added by Sachiko, 09/25/2002 ]
      // Since D is symmetric, it should follow that x * D * x^T is also symmetric.
      // In this particular case, x is fi_bOut.
      // In addition, Ri is supposed to be symmetric too.  Therefore,
      // x * Sym * x + Ri will be symmetric.
      // However, the following addition may yield in round-off error.
      // So, Make sure symmetrize it afterward.
      temp.resize( _N[ i ] * _N[ i ] );
      temp = fi_bDfi_bOutT + RiOut;
      
      //
      // Symmetrize based upon the lower triangle.
      //
      for( int j=0; j<_N[ i ]; j++ )
	{
	  // Copy the lower triangle data into the resulting matrix's upper triangle
	  for( int p=0; p<j; p++ )
	    {
	      temp[ p + j * _N[ i ] ] = temp[ j + p * _N[ i ] ];
	    }        
	}
      
      
      //
      // Forming the matrix R(B), where B = alp.
      //
      replaceSubblock( foROut, _nY,
		       temp, _N[ i ],
		       startRow, 
		       startRow );
      startRow += _N[ i ];
    }
  
  assert( isSymmetric( foROut, _nY ) );
  isCachedRValid = true;

  cachedR.resize( foROut.size() );
  cachedR = foROut;
}

void EqIndModel::doDataVarianceInv( valarray<double>& foRInvOut ) const
{
	if( isCachedRInvValid )
	{
	  foRInvOut.resize( cachedRInv.size() );
	  foRInvOut = cachedRInv;
	  return;
	}

    foRInvOut.resize( _nY * _nY, 0.0 );
    valarray<double> DOut( _nb * _nb );
    _pModel->indParVariance(DOut);
    assert( DOut.size() == _nb * _nb );
    assert( isSymmetric( DOut, _nb ) );

    valarray<double> fi_bOut, fi_bDfi_bOutT, temp;
    EqIndModelFunction fiOb( _pModel );

    _pModel->setIndPar( _b0 );
    for( int i = 0, startRow = 0; i < _nInd; i++ )
    {
        _pModel->selectIndividual( i );
	fi_bOut.resize( _N[ i ] * _nb );
        fi_bOut = centdiff<EqIndModelFunction>( fiOb, 1, _b0, _bStep ); 
        assert( fi_bOut.size() == _N[ i ] * _nb );

        _pModel->setIndPar( _b0 );
        valarray<double> RiOut( _N[i] * _N[i] );
        _pModel->dataVariance(RiOut);
        assert( isSymmetric( RiOut, _N[ i ] ) );
        assert( RiOut.size() == _N[ i ] * _N[ i ] );

        fi_bDfi_bOutT.resize( _N[i] * _N[i] );
        fi_bDfi_bOutT = multiply( multiply( fi_bOut, _nb, DOut, _nb ), _nb, transpose( fi_bOut, _nb ), _N[ i ] );

	temp.resize( _N[ i ] * _N[ i ] );
        temp = fi_bDfi_bOutT + RiOut;

        //
        // Symmetrize based upon the lower triangle.
        //
        for( int j=0; j<_N[ i ]; j++ )
        {
            // Copy the lower triangle data into the resulting matrix's upper triangle
            for( int p=0; p<j; p++ )
            {
                temp[ p + j * _N[ i ] ] = temp[ j + p * _N[ i ] ];
            }        
        }
        replaceSubblock( foRInvOut, _nY,
                         inverse( temp, _N[ i ] ), _N[ i ],
                         startRow, 
                         startRow );
        startRow += _N[ i ];
    }

	isCachedRInvValid = true;
	cachedRInv.resize( foRInvOut.size() );
	cachedRInv = foRInvOut;                                                                                                  
}

bool EqIndModel::doDataVariance_indPar( valarray<double>& foR_BOut ) const
{
    try{
      foR_BOut.resize( _nY * _nY * _nB, 0.0 );
    }
    catch( ... )
    {
      char errmsg[] = "Not enough memory available in the heap.\n \
        Try NAIVE_FIRST_ORDER instead of your currently requested FIRST_ORDER as Objective.";

      throw SpkException( SpkError::SPK_INSUFFICIENT_MEM_ERR, errmsg, __LINE__, __FILE__ );
    }

    valarray<double> fi_bOut, fi_b_BOut, fi_B_bOut, fi_bOutT;
    valarray<double> DOut, D_BOut;
    valarray<double> Ri_BOut;
    valarray<double> foRi_B;

    bool notAllZeros = false;
    double val;

    EqIndModelFunction   fiOb ( _pModel );
    EqIndModelDerivative fi_aOb( _pModel, _nB );

    //
    //_pModel->setPopPar( _B );   // Conceptually this statement should be placed here
    //                            // but shouldn't in practice because we know for
    //                            // sure _pModel's popPar has been set to _B already
    //                            // and hasn't been disturbed ever since.
    //
    for( int who = 0, startIndex = 0; who < _nInd; who++ )
    {
      _pModel->selectIndividual( who );

      
      _pModel->setIndPar( _b0 );

      //
      // Approximating for fi_b( B, b(=0) )
      //
      fi_bOut.resize( _N[ who] * _nb );
      fi_bOut = centdiff<EqIndModelFunction>( fiOb, 1, _b0, _bStep );
      assert( fi_bOut.size() == _N[ who ] * _nb );
      //
      // Approximating for fi_B_b( B, b(=0) ), where fi_B is the true derivative.
      //
      fi_B_bOut.resize( _N[ who ] * _nB  * _nb );
      fi_B_bOut = centdiff<EqIndModelDerivative>( fi_aOb, 1, _b0, _bStep );
      assert( fi_B_bOut.size() == ( _N[ who ] * _nB ) * _nb );

      //
      // Flipping the true derivative portion and the approximation portion to form.
      // fi_b_B( B, b(=0) ), where fi_b is the approximation.
      //
      fi_b_BOut.resize( _N[ who ] * _nb * _nB );
      fi_b_BOut = transposeRowBlocks( fi_B_bOut, _nb, _nB );
      assert( fi_b_BOut.size() == ( _N[ who ] * _nb ) * _nB );

      //
      // centdiff() routine sets _pModel's indPar during the computation.
      // So, make sure, reset to zero.
      //
      _pModel->setIndPar( _b0 );

      //
      // Compute D(B)
      //
      DOut.resize( _nb * _nb );
      _pModel->indParVariance( DOut );
      assert( DOut.size() == _nb * _nb );

      //
      // Compute D_B(B)
      //
      D_BOut.resize( ( _nb * _nb ) * _nB );
      _pModel->indParVariance_popPar( D_BOut );
      assert( D_BOut.size() == ( _nb * _nb ) * _nB );

      //
      // Compute Ri_B( B, b(=0) )
      //
      Ri_BOut.resize( ( _N[ who ] * _N[ who ] ) * _nB );
      _pModel->dataVariance_popPar( Ri_BOut );
      assert( Ri_BOut.size() == ( _N[ who ] * _N[ who ] ) * _nB );

      fi_bOutT.resize(_nb * _N[ who] );
      fi_bOutT  = transpose( fi_bOut, _nb );
      fi_B_bOut.resize(_N[ who ] * _nB * _nb );
      fi_B_bOut = transposeDerivative( fi_b_BOut, _N[ who ], _nb, _nB );

      foRi_B.resize( _N[ who ] * _N[ who ] * _nB );
      foRi_B = ABA_x( fi_bOutT, _N[ who ],
                        DOut, _nb,
                        fi_B_bOut,
                        D_BOut, 
                        _nB 
                        )
             + Ri_BOut;
      // Final =
      // 
      // [ rvec( R_x1 ) ]
      // [ rvec( R_x2 ) ]
      // [ rvec( R_x3 ) ]
      // [ ...          ]
      // [ rvec( R_xn ) ]
      // where n is the number of parameters in x.
      // 
      // R_xi =
      // [ R1_xi,     0,     0,     0,   ...,      0 ]
      // [     0, R2_xi,     0,     0,   ...,      0 ]
      // [     0,     0, R3_xi,     0,   ...,      0 ]
      // [ ......................................... ]
      // [     0,     0,     0,     0,   ...,  Rm_Xi ]
      // where m is the number of individuals.
      // 
      // where Rk_xi =
      //       [ Rk_xi(1,1), Rk_xi(1,2), Rk_xi(1,3),    ..., Rk_xi(1, kn) ]
      //       [ Rk_xi(2,1), Rk_xi(2,2), Rk_xi(2,3),    ..., Rk_xi(2 ,kn) ]
      //       [ Rk_xi(3,1), Rk_xi(3,2), Rk_xi(3,3),    ..., Rk_xi(3, kn) ]
      //       [ ........................................................ ]
      //       [          0,          0,          0,    ..., Rk_xi(km,kn) ]
      //       where km is the number of rows in Rk_xi,
      //       kn is the number of columns in Rk_xi
      // 
      // When R_xi is expanded:                                                                                                                                                                                                      \
      // /
      // |  [ R1_xi(1, 1), R1_xi(1, 2), R1_xi(1, 3),     ..., R1_xi(1, kn) ]             0,            0,           0,     ...,           0,               0,            0,           0,     ...,           0    |
      // |  [ R1_xi(2, 1), R1_xi(2, 2), R1_xi(2, 3),     ..., R1_xi(2 ,kn) ]             0,            0,           0,     ...,           0,               0,            0,           0,     ...,           0    |
      // |  [ R1_xi(3, 1), R1_xi(3, 2), R1_xi(3, 3),     ..., R1_xi(3, kn) ]             0,            0,           0,     ...,           0,               0,            0,           0,     ...,           0    |
      // |  [ ............................................................ ]             0,            0,           0,     ...,           0,               0,            0,           0,     ...,           0    |
      // |  [ R1_xi(km,1), R1_xi(km,2), R1_xi(km,3),     ..., R1_xi(km,kn) ]             0,            0,           0,     ...,           0,               0,            0,           0,     ...,           0    |
      // |              0,           0,           0,     ...,           0,    [ R2_xi(1, 1), R2_xi(1, 2), R2_xi(1, 3),     ..., R2_xi(1, kn) ]             0,            0,           0,     ...,           0    |
      // |              0,           0,           0,     ...,           0,    [ R2_xi(2, 1), R2_xi(2, 2), R2_xi(2, 3),     ..., R2_xi(2 ,kn) ]             0,            0,           0,     ...,           0    |
      // |              0,           0,           0,     ...,           0,    [ R2_xi(3, 1), R2_xi(3, 2), R2_xi(3, 3),     ..., R2_xi(3, kn) ]             0,            0,           0,     ...,           0    |
      // |              0,           0,           0,     ...,           0,    [ ............................................................ ]             0,            0,           0,     ...,           0    |
      // |              0,           0,           0,     ...,           0,    [ R2_xi(km,1), R2_xi(km,2), R2_xi(km,3),     ..., R2_xi(km,kn) ]             0,            0,           0,     ...,           0    |
      // |  ...................................................................................................................................................................................................  |
      // |              0,           0,           0,     ...,           0,                0,           0,           0,     ...,           0,    [ Rm_xi(1, 1), Rm_xi(1, 2), Rm_xi(1, 3),     ..., Rm_xi(1, kn) ] |
      // |              0,           0,           0,     ...,           0,                0,           0,           0,     ...,           0,    [ Rm_xi(2, 1), Rm_xi(2, 2), Rm_xi(2, 3),     ..., Rm_xi(2 ,kn) ] |
      // |              0,           0,           0,     ...,           0,                0,           0,           0,     ...,           0,    [ Rm_xi(3, 1), Rm_xi(3, 2), Rm_xi(3, 3),     ..., Rm_xi(3, kn) ] | 
      // |              0,           0,           0,     ...,           0,                0,           0,           0,     ...,           0,    [ ............................................................ ] |
      // |              0,           0,           0,     ...,           0,                0,           0,           0,     ...,           0,    [ Rm_xi(km,1), Rm_xi(km,2), Rm_xi(km,3),     ..., Rm_xi(km,kn) ] |
      // \                                                                                                                                                                                                      /  
      //
      //
      // In our case, foRi_B is basically rvec(Rk_xi).
      //      foRi_B = rvec(Rk_xi) =
      //      [ Rk_xi(1, 1) ]
      //      [ Rk_xi(1, 2) ]
      //      [ Rk_xi(1, 3) ]
      //      [ ...         ]
      //      [ Rk_xi(1,kn) ]
      //      ---------------
      //      [ Rk_xi(2, 1) ]
      //      [ Rk_xi(2, 2) ]
      //      [ Rk_xi(2, 3) ]
      //      [ ...         ]
      //      [ Rk_xi(2,kn) ]
      //      ---------------
      //      [ Rk_xi(3, 1) ]
      //      [ Rk_xi(3, 2) ]
      //      [ Rk_xi(3, 3) ]
      //      [ ...         ]
      //      [ Rk_xi(3,kn) ]
      //      ---------------
      //            ...
      //      ---------------
      //      [ Rk_xi(km, 1) ]
      //      [ Rk_xi(km, 2) ]
      //      [ Rk_xi(km, 3) ]
      //      [ ...          ]
      //      [ Rk_xi(km,kn) ]
      //
      // We will stuff in a bunch of 0s in between rows like this:
      //
      //      ---------------< 0s : 0 <= (The sum of cols in { R1_xi, R2_xi..., R(k-1)_xi } ) <= The total cols in R_xi
      //      [ Rk_xi(1, 1) ]
      //      [ Rk_xi(1, 2) ]
      //      [ Rk_xi(1, 3) ]
      //      [ ...         ]
      //      [ Rk_xi(1,kn) ]
      //      ---------------< 0s : The total cols in R_xi - (The sum of cols in { R1_xi, R2_xi..., Rk_xi } )
      //      ---------------< 0s : 0 <= (The sum of cols in { R1_xi, R2_xi..., R(k-1)_xi } ) <= The total cols in R_xi
      //      [ Rk_xi(2, 1) ]
      //      [ Rk_xi(2, 2) ]
      //      [ Rk_xi(2, 3) ]
      //      [ ...         ]
      //      [ Rk_xi(2,kn) ]
      //      ---------------< 0s : The total cols in R_xi - (The sum of cols in { R1_xi, R2_xi..., Rk_xi } )
      //      ---------------< 0s : 0 <= (The sum of cols in { R1_xi, R2_xi..., R(k-1)_xi } ) <= The total cols in R_xi
      //      [ Rk_xi(3, 1) ]
      //      [ Rk_xi(3, 2) ]
      //      [ Rk_xi(3, 3) ]
      //      [ ...         ]
      //      [ Rk_xi(3,kn) ]
      //      ---------------< 0s : The total cols in R_xi - (The sum of cols in { R1_xi, R2_xi..., Rk_xi } )
      //      ---------------< 0s : 0 <= (The sum of cols in { R1_xi, R2_xi..., R(k-1)_xi } ) <= The total cols in R_xi
      //            ...
      //      ---------------< 0s : 0 <= (The sum of cols in { R1_xi, R2_xi..., R(k-1)_xi } ) <= The total cols in R_xi
      //      [ Rk_xi(km, 1) ]
      //      [ Rk_xi(km, 2) ]
      //      [ Rk_xi(km, 3) ]
      //      [ ...          ]
      //      [ Rk_xi(km,kn) ]
      //      ---------------< 0s : The total cols in R_xi - (The sum of cols in { R1_xi, R2_xi..., Rk_xi } )
      //

      // 
      // [ Revisit --- Sachiko, 10/10/2002 ]
      // This indexing is not the intended use of valarray.  It should be done with glice/slice instead.
      // But, for the first move from DoubleMatrix to valarray, I keep as it is.  
      // 
      int rows = _N[ who ];
      for( int x_i = 0; x_i < _nB; x_i++ )
      {
        for( int j = 0; j < rows; j++ )
        {
          for( int i = 0; i < rows; i++ )
          {
            val = foRi_B[ i + j * rows + x_i * rows * rows ];
            notAllZeros |= ( val != 0 );
            foR_BOut[ i + startIndex + _nY * j + x_i * _nY * _nY ] 
               = val;
          }     
        }
      }
      startIndex += rows * _nY + rows;
    }

    return notAllZeros;
}

/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

namespace // [Begin: unnamed namespace]
{

/*************************************************************************
 *
 * Function: isNotEqual
 *
 *
 * Returns true if any of the elements in the vector x is 
 * not equal to the corresponding element in the vector y,
 * or if the vectors have different dimensions.
 * 
 *************************************************************************/

bool isNotEqual( const valarray<double>& x, const valarray<double>& y )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int n = x.size();

  // If the dimensions don't agree, then they're not equal.
  if ( y.size() != n )
  {
    return true;
  }


   //------------------------------------------------------------
  // Compare the elements in the two matrices.
  //------------------------------------------------------------
 
  // Find the first element that's different and then return true.
  for ( int i = 0; i < n; i++ )
  {
    if ( x[i] != y[i] )
    {
      return true;
    }
  }

  return false;
}


} // [End: unnamed namespace]
