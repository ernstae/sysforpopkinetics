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
 * File: FoModel.cpp
 *
 *
 * Defins FoModel class.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

/*************************************************************************
 *
 * Class: FoModel
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin FoModel$$
$spell 
  Fo
  Spk
  pmodel
  const
  ppka
$$

$section First Order Model$$

$index FO$$
$index objective, first order$$
$index FoModel$$

$table
$bold Constructors:$$   $cend  
$syntax/FoModel::FoModel( SpkModel* /pmodel/, 
		     const DoubleMatrix& /alp/,
		     const DoubleMatrix& /bStep/, 
			 const int /nY/ )/$$
$tend

See also: $xref/firstOrderOpt//firstOrderOpt/$$.
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This class runs capable models in 
the way they are treated as first order models.
$codep

   ---------------------
   |      SpkModel     |
   ---------------------
            / \
             |
   ---------------------
   |      FoModel      |
   |                   |
   |  SpkModel* pModel |
   ---------------------

$$
It is provided solely for $bold testing purpose$$.  Its implementation is 
naive and slow.  The official way of running Spk with FO objective
is to enter from $xref/firstOrderOpt//firstOrderOpt()/$$ or 
$xref/ppkaOpt//ppkaOpt()/$$ with an instance of ordinary SpkModel.  
Do not use this FoModel!

$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * This class should not belong to the core Spk component space.
 * However, currently lTilde() and estimateB() instanciate this
 * when FO is specified in their arguments.
 * These two functions are not in the execution path of firstOrderOpt().
 * That is, when the user performs the population analysis with
 * FO objectve, firstOrderOpt() is called instead of ppkaOpt() and
 * those clients of FoModel never get executed.
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include "SpkValarray.h"
#include "FoModel.h"

#include "IkronBtimesC.h"
#include "transposeRowBlocks.h"
#include "identity.h"
#include "centdiff.h"
#include "add.h"
#include "transpose.h"
#include "allZero.h"
#include "inverse.h"
#include "AkronBtimesC.h"

using SPK_VA::valarray;

/*------------------------------------------------------------------------
 * Class definition
 *------------------------------------------------------------------------*/
const valarray<double> FoModel::FoModelFunction::operator()( const valarray<double>& var ) const
{
    pModel->setIndPar( var );
	if( !isF_alpOut )
    {
		pModel->dataMean(ret);
        return ret;
    }
    
	pModel->dataMean_popPar(ret);
    return ret;
}
FoModel::FoModelFunction::FoModelFunction( SpkModel* p, bool s ) 
: pModel( p ), isF_alpOut( s ) 
{
}
FoModel::FoModelFunction::~FoModelFunction()
{
}
FoModel::FoModelFunction::FoModelFunction( const FoModelFunction& right ) 
: pModel( right.pModel ), 
  isF_alpOut( right.isF_alpOut ) 
{
}

FoModel::FoModel( SpkModel* pmodel, 
				  const valarray<double>& alp,
				  const valarray<double>& bStep,
				  const int nY ) 
                : _pmodel( pmodel ), _bStep( bStep ), _nY( nY ) , _nA(0), _nB(0), _b(0),
                _F0(0), _F0_a(0), _R0(0), _R0_a(0), _D0(0), _D0_a(0), _F0_b(0), _F0_b_a(0)

{
    _nA = alp.size();
	_nB = _bStep.size();
	valarray<double> bZero( 0.0, _nB );

	_pmodel->setPopPar( alp );
	_pmodel->setIndPar( bZero );    
	_pmodel->dataMean(_F0);
	_pmodel->dataMean_popPar(_F0_a);
	_pmodel->dataVariance(_R0);
	_pmodel->dataVariance_popPar(_R0_a);
	_pmodel->indParVariance(_D0);
	_pmodel->indParVariance_popPar(_D0_a);
	FoModelFunction f( _pmodel, false );
	_F0_b   = centdiff<FoModelFunction>( f, 1, bZero, _bStep );
	FoModelFunction f_a( _pmodel, true );
    _F0_b_a = transposeRowBlocks( DoubleMatrix( centdiff<FoModelFunction>( f_a, _nA, bZero, _bStep ), _nB ), _nA ).toValarray();
}

FoModel::~FoModel() {}

void FoModel::doSelectIndividual( int inx )
{
	_pmodel->selectIndividual( inx );
}
void FoModel::doSetPopPar( const valarray<double>& popPar )
{
	_pmodel->setPopPar( popPar );
}
void FoModel::doSetIndPar( const valarray<double>& indPar )
{
  //
  // [ Question? by Sachiko ]
  // Why this is not calling _pmodel->setIndPar(dvecInd)?
  //
  _pmodel->setIndPar(indPar);
  _b = indPar;
}
void FoModel::doDataMean( valarray<double> & ret ) const
{
	DoubleMatrix DM_ret = add( DoubleMatrix(_F0, 1), DoubleMatrix(_F0_b, _nB) * DoubleMatrix( _b, 1 ) );
    ret = DM_ret.toValarray();
}
bool FoModel::doDataMean_popPar( valarray<double> & ret ) const
{
  DoubleMatrix DM_ret = add( DoubleMatrix(_F0_a, _nA), IkronBtimesC( identity( _nY ), DoubleMatrix( _b, _nB ), DoubleMatrix( _F0_b_a, _nA ) ) );
  ret = DM_ret.toValarray();
  return !allZero(ret);
}
bool FoModel::doDataMean_indPar( valarray<double> & ret ) const
{
	ret = _F0_b;
    return !allZero(ret);
}
void FoModel::doDataVariance( valarray<double> & ret ) const
{
	ret = _R0;
}
bool FoModel::doDataVariance_popPar( valarray<double> & ret ) const
{
	ret = _R0_a;
    return !allZero(ret);
}
bool FoModel::doDataVariance_indPar( valarray<double> & ret ) const
{
    ret.resize( _nY * _nY * _nB );
	ret = 0.0;
    return false;
}
void FoModel::doIndParVariance( valarray<double> & ret ) const
{
	ret = _D0;
}
bool FoModel::doIndParVariance_popPar( valarray<double> & ret ) const
{
	ret = _D0_a;

    return !allZero(ret);
}
void FoModel::doIndParVarianceInv( valarray<double> & ret ) const
{
  DoubleMatrix temp = inverse( DoubleMatrix( _D0, _nB ) );
  ret = temp.toValarray();
}
bool FoModel::doIndParVarianceInv_popPar( valarray<double> & ret ) const
{
  valarray<double> Dinv;
  doIndParVarianceInv(Dinv);
  valarray<double> D_a;
  doIndParVariance_popPar(D_a);
  DoubleMatrix DM_negated_ret = AkronBtimesC( DoubleMatrix( Dinv, _nB ), DoubleMatrix( Dinv, _nB ), DoubleMatrix( D_a, _nA ) );
  ret = DM_negated_ret.toValarray() * -1.0;

  return !allZero(ret);
}
