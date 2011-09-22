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
 * File: NaiveFoModel.cpp
 *
 *
 * Defines NaiveFoModel class.
 * 
 * This class implements the straight translation of the
 * following equations.
 *
 *                              ~
 *  fi(alp, bi)   = fi(alp, 0)   + fi_b(alp, 0) * bi
 *  Ri(alp, bi)   = Ri(alp, 0)
 *  D(alp)        = D(alp)
 *
 * 
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Class: NaiveFoModel
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin NaiveFoModel$$
$spell 
  bi
  fi
  Ri
  Fo
  Spk
  pmodel
  const
  ppka
  valarray
  EqIndModel
  Fi
  Ri
  bool
  Bradley M. Bell
$$

$section First Order Approximation Model (Straight Translation)$$

$index FO$$
$index objective, first order (straight translation of the algorithm)$$
$index NaiveFoModel$$

$table
$bold Constructors:$$   $cend  
$syntax/NaiveFoModel::NaiveFoModel( SpkModel* /pmodel/, const SpkValarray::valarray<double>& /bStep/)/$$

$tend

See also: $xref/EqIndModel//Faster FO Model Class/$$, $xref/firstOrderOpt//firstOrderOpt/$$.
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This class assumes the following approximation is true:
$math%
                               ~
    fi(alp, bi) = fi(alp, 0) + fi_b(alp, 0) * bi
    Ri(alp, bi) = Ri(alp, 0)
%$$
The right hand sides of the above equations are first order
approximations of the corresponding left hand side equations.
For this case, the problem of optimizing in $math%b%$$  
is a linear least squares problem.  
See the section 8 "The modified first-order objective" in
"Approximating the marginal likelihood estimate for 
models with random parameters" by Bradley M. Bell
for details.
$pre

$$
This class's implementation is a straight translation of the above
first order approximations, which is not necessarily the
most efficient implementation of FO.  For better
performance, see $xref/EqIndModel//EqIndModel/$$.
$pre

$$
$code NaiveFoModel$$ class is a subclass of $xref/SpkModel//SpkModel/$$
and keeps a pointer to the original SpkModel object internally to
evaluate the approximations.
$codep

   --------------------------------------------
   |                 SpkModel                 |
   --------------------------------------------
   |  fi(alp,b), fi(alp,b)_alp, fi(alp,b)_bi  |
   |  Ri(alp,b), Ri(alp,b)_alp, Ri(alp,b)_bi  |
   |  D(alp),    D(alp)_alp                   |
   |  setPopPar(alp), setIndPar(bi)           |
   |  selectIndividual(i)                     |
   --------------------------------------------
                       /|\
                        |
   ----------------------------------------------
   |               NaiveFoModel                 |
   ----------------------------------------------
   |  ~          ~              ~               |
   |  fi(alp,bi), fi(alp,bi)_alp, fi(alp,bi)_bi |
   |  ~          ~              ~               |
   |  Ri(alp,bi), Ri(alp,bi)_alp, Ri(alp,bi)_bi |
   |  ~          ~              ~               |
   |  D(alp),    D(alp)_alp                     |
   |                                            |
   |  setPopPar(alp), setIndPar(bi)             |
   |  selectIndividual(i)                       |
   |--------------------------                  |
   |  * -> SpkModel object   |                  |  
   ----------------------------------------------
$$
(NOTE: $math%~%$$ on top of a function indicates approximation)

$head Constructors$$
$syntax/
NaiveFoModel( SpkModel* /pmodel/, const SPK_VA::valarray<double>& /bStep/)
/$$
This constructor takes a pointer to an
SpkModel, $italic pmodel$$, that is to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.
$pre

$$
The second argument $italic bStep$$ is a vector of step sizes, 
where each element corresponds to the individual parameter $math%b%$$ in the preserving order.
The step size vector will be used to obtain the central approximation for the second derivative of
$math%f%$$ with respect to $math%b%$$ to compute the approximation for the
derivative of $math%f(alp,b)%$$.

$head Implemented Virtual Members$$
$syntax/
void doSelectIndividual( int /who/ )
/$$
selects an individual in the population pointed by $math%who%$$,
where $math%0 <= who < n%$$ and $math%n%$$ is the number of individuals
in the population, in the model object pointed by $code &pmodel$$.
It calls $italic pmodel$$$code.selectIndividual()$$
which, in turn, calls the user implemented
$italic pmodel$$$code.doSelectIndividual()$$, with $italic who$$.

$syntax/

void doSetPopPar( const SPK_VA::valarray<double>& /popPar/ )
/$$
sets the current population parameter.
It also calls $italic pmodel$$$code.setPopPar()$$
which, in tern, calls $italic pmodel$$$code.doSetPopPar()$$
with $italic popPar$$.

$syntax/

void doSetIndPar( const SPK_VA::valarray<double>& /indPar/ )
/$$
sets the current individual parameter in the FO model
with $italic indPar$$.

$syntax/

void doDataMean( SPK_VA::valarray<double>& foFiOut ) const
/$$
evaluates the first order approximation for $math%fi(alp,b)%$$, 
which is:
$math%
                             ~
    foFiOut = fi( alp, 0 ) + fi_b( alp, 0 ) * bi
%$$
at the currently set $italic popPar$$ ($math%alp%$$) and 
$italic indPar$$ ($math%b%$$).  Note that the second term on the
right hand side equation is an approximation for the first derivative.

$syntax/

bool doDataMean_popPar( SPK_VA::valarray<double>& foFi_bOut ) const
/$$
evaluates the first derivative of the first order approximation for
$math%fi(alp,b)%$$ with respect to the population parameter
at the currently set $italic popPar$$ ($math%alp%$$) and 
$italic indPar$$ ($math%b%$$).

$syntax/

bool doDataMean_indPar( SPK_VA::valarray<double>& foFi_alpOut ) const
/$$
evaluates the first derivative of the first order approximation for
$math%fi(alp,b)%$$ with respect to the individual parameter
at the currently set $italic popPar$$ ($math%alp%$$) and 
$italic indPar$$ ($math%b%$$).

$syntax/

void doDataVariance( SPK_VA::valarray<double>& foRiOut ) const
/$$
evaluates the first order approximation for $math%Ri(alp,b)%$$,
which is:
$math%
  foRiOut = Ri(alp, 0)
%$$
at the currently set $italic popPar$$ ($math%alp%$$).

$syntax/

bool doDataVariance_popPar( SPK_VA::valarray<double>& foRi_alpOut ) const
/$$
evaluates the first derivative of the first order approximation for
$math%Ri(alp,b)%$$ with respect to the population parameter
at the currently set $italic popPar$$ ($math%alp%$$).

$syntax/

bool doDataVariance_indPar( SPK_VA::valarray<double>& foRi_bOut ) const
/$$
evaluates the first derivative of the first order approximation for
$math%Ri(alp,b)%$$ with respect to the individual parameter
at the currently set $italic indPar$$ ($math%b%$$).

$syntax/

void doIndParVariance( SPK_VA::valarray<double>& foDOut ) const
/$$
evaluates the variance among individual parameters, $math%D(alp)%$$.
at the currently set $italic popPar$$ ($math%alp%$$).

$syntax/

bool doIndParVariance_popPar( SPK_VA::valarray<double>& foD_alpOut ) const
/$$
evaluates the first derivative of $math%D(alp)%$$ with respect
to $italic popPar%$$ ($math%alp%$$)
at the currently set $italic popPar$$ ($math%alp%$$).

$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <functional>

#include "SpkValarray.h"
#include "NaiveFoModel.h"

#include "IkronBtimesC.h"
#include "transposeRowBlocks.h"
#include "identity.h"
#include "centdiff.h"
#include "add.h"
#include "transpose.h"
#include "allZero.h"
#include "inverse.h"
#include "AkronBtimesC.h"
#include "multiply.h"

using SPK_VA::valarray;
/*------------------------------------------------------------------------
 * NaiveFoModel Class definition
 *------------------------------------------------------------------------*/
NaiveFoModel::NaiveFoModel( SpkModel* pmodel, 
                  const valarray<double>& bStep ) 
                : _pmodel( pmodel ), _bStep( bStep )

{
  _nB = _bStep.size();
  _bZero.resize( _nB );
  _bZero = 0.0;
}

NaiveFoModel::~NaiveFoModel() {}

void NaiveFoModel::doSelectIndividual( int inx )
{
  _pmodel->selectIndividual( inx );
}
void NaiveFoModel::doSetPopPar( const valarray<double>& popPar )
{
  _alp.resize( popPar.size() );
  _alp = popPar;
  _nA = popPar.size();
  _pmodel->setPopPar( popPar );
}
void NaiveFoModel::doSetIndPar( const valarray<double>& indPar )
{
  _b.resize( indPar.size() );
  _b = indPar;
}
void NaiveFoModel::doDataMean( valarray<double> & foFiOut ) const
{
  using namespace std;
  _pmodel->setIndPar( _bZero );
  
  //
  // Evaluate f(alp.0);
  //
  valarray<double> fi0Out;
  _pmodel->dataMean( fi0Out );
  int nY = fi0Out.size();
  assert( nY > 0 );

  //
  //          ~
  // Evaluate f_b(alp,0)
  //
  valarray<double> fi_b0Out( nY * _nB );
  NaiveFoModelFunction fi( _pmodel );
  fi_b0Out = centdiff< binder1st<NaiveFoModelFunction> >( bind1st( fi, _alp ), 1, _bZero, _bStep );

  //                    ~
  // Compute f(alp,0) + f_b(alp,0) * b
  //
  foFiOut.resize( nY );
  valarray<double> temp = multiply( fi_b0Out, _nB, _b, 1 );
  foFiOut = fi0Out + temp;
}
bool NaiveFoModel::doDataMean_popPar( valarray<double> & foFi_aOut ) const
{
  using namespace std;
  valarray<double> fi_aOut;

  _pmodel->setIndPar( _bZero );
  _pmodel->dataMean_popPar( fi_aOut );

  int nYi = fi_aOut.size() / _nA;
  valarray<double> fi_trueA_approxBOut( nYi * _nA * _nB );
  DoubleMatrix DM_fi_approxB_trueAOut( nYi * _nA, _nB );

  //
  // First, compute the approximation for the derivative of the true derivative fi_alp(alp,b) wrt. b 
  //
  NaiveFoModelDeriv_popPar fi_a( _pmodel );
  fi_trueA_approxBOut = centdiff< binder1st<NaiveFoModelDeriv_popPar> >
    ( bind1st( fi_a, _alp ), _nA, _bZero, _bStep );

  //
  // And then, block transpose the f_a_bOut so that a new matrix forms
  // f_b_a which is the true derivative of the approximation for the derivative of f_b.
  // 
  DM_fi_approxB_trueAOut = transposeRowBlocks( DoubleMatrix( fi_trueA_approxBOut, _nB ), _nA );

  //                                          ~
  // Computer f_alp(alp,0) + I kron b * f_b_alp(alp,0)
  // 
  foFi_aOut.resize( nYi * _nA );
  foFi_aOut = fi_aOut 
    + IkronBtimesC( identity( nYi ), DoubleMatrix( _b, _nB ), DM_fi_approxB_trueAOut ).toValarray();

  return !allZero( foFi_aOut );
}
bool NaiveFoModel::doDataMean_indPar( valarray<double> & foFi_bOut ) const
{
  using namespace std;

  // Lemma 15, pg71, "Approximating the Marginal Likelihood estimate for
  // model with random parameters" by Bradley Bell:
  //  
  // If the true 2nd derivative of f(alp,b) with respect be and 
  // the derivative of R(alp,b) with respect to b are both zeros,
  //
  //    ~
  //    f_b(alp,b) = f_b(alp,b)
  //
  //    ~
  //    R_b(alp,b) = 0
  //
  _pmodel->setIndPar( _bZero );
  _pmodel->dataMean_indPar( foFi_bOut );

  return !allZero( foFi_bOut );

}
void NaiveFoModel::doDataVariance( valarray<double> & foRiOut ) const
{
  _pmodel->setIndPar( _bZero );
  _pmodel->dataVariance( foRiOut );
}
bool NaiveFoModel::doDataVariance_popPar( valarray<double> & foRi_aOut ) const
{
  _pmodel->setIndPar( _bZero );
  _pmodel->dataVariance_popPar( foRi_aOut );

  return !allZero(foRi_aOut);
}
bool NaiveFoModel::doDataVariance_indPar( valarray<double> & foRi_bOut ) const
{
  // Lemma 15, pg71, "Approximating the Marginal Likelihood estimate for
  // model with random parameters" by Bradley Bell:
  //  
  // If the true 2nd derivative of f(alp,b) with respect be and 
  // the derivative of R(alp,b) with respect to b are both zeros,
  //
  //    ~
  //    f_b(alp,b) = f_b(alp,b)
  //
  //    ~
  //    R_b(alp,b) = 0
  //
  valarray<double> RiOut;
  doDataVariance( RiOut );

  int nY2i = RiOut.size();

  foRi_bOut.resize( nY2i * _nB );
  foRi_bOut = 0.0;
  return false;
}
void NaiveFoModel::doIndParVariance( valarray<double> & foDOut ) const
{
  _pmodel->indParVariance( foDOut );
}
bool NaiveFoModel::doIndParVariance_popPar( valarray<double> & foD_aOut ) const
{
  _pmodel->indParVariance_popPar( foD_aOut );

  return !allZero( foD_aOut );
}

/*------------------------------------------------------------------------
 * NaiveFoModelFunction function object class definition
 *------------------------------------------------------------------------*/
//
// NaiveFoModelFunction class is an instance of the unary function class.
// It provides a unary operator( x1, x2 ) that evalutes f(alp, b).
//

//
// Section of an individual must be done outside of this class.  
// It is assumed that these parameters are set at the time of member function call.
//
NaiveFoModel::NaiveFoModelFunction::NaiveFoModelFunction( SpkModel* p ) 
: pModel( p )
{
}

NaiveFoModel::NaiveFoModelFunction::~NaiveFoModelFunction()
{
}
NaiveFoModel::NaiveFoModelFunction::NaiveFoModelFunction( const NaiveFoModel::NaiveFoModelFunction &right )
: pModel( right.pModel )
{
}

const valarray<double> NaiveFoModel::NaiveFoModelFunction::operator()
( const valarray<double> & alp,
  const valarray<double> & b) const
{
  //
  // Assumption: 1) An individual has been selected.
  //
  pModel->setPopPar( alp );
  pModel->setIndPar( b );
  pModel->dataMean( ret );
  return ret;
}

/*------------------------------------------------------------------------
 * NaiveFoModelDeriv_popPar function object class definition
 *------------------------------------------------------------------------*/
//
// NaiveFoModelDeriv_popPar class is an instance of the unary function class.
// It provides a unary operator( x1, x2 ) that evalutes 
// the true derivative of f(alp,b) with respect to alp, namely f_alp(alp, b).
//
NaiveFoModel::NaiveFoModelDeriv_popPar::NaiveFoModelDeriv_popPar( SpkModel* p ) 
: pModel( p ) 
{
}

NaiveFoModel::NaiveFoModelDeriv_popPar::~NaiveFoModelDeriv_popPar()
{
}
NaiveFoModel::NaiveFoModelDeriv_popPar
::NaiveFoModelDeriv_popPar( const NaiveFoModel::NaiveFoModelDeriv_popPar& right )
: pModel( right.pModel )
{
}

const valarray<double> NaiveFoModel
::NaiveFoModelDeriv_popPar::operator()
( const valarray<double>& alp,
  const valarray<double>& b ) const
{
  //
  // Assumption: 1) An individual has been selected.
  //
  pModel->setPopPar( alp );
  pModel->setIndPar( b );
  pModel->dataMean_popPar( ret );
  return ret;
}

