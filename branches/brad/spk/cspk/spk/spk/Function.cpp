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
 * File: Function.cpp
 *
 *
 * Template Function classes for SpkModel member functions.  
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Class: ModelFunction class
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin ModelFunction$$
$spell
	Model model 
    arg 
    typename 
    std 
    cend 
    proto 
    ob 
    int 
    endl 
    cout 
    bool 
    Ri 
    fi 
    inv 
    Rinv 
    ind 
    pop
    const 
    iostream 
    typedef 
    struct 
    nr 
    bval 
    instantiate 
    init
    fval 
    var 
    Rval 
    nagation 
    const 
    doSetIndPar 
    res 
    binary_function 
    st 
    centdiff 
    approx
    covariances
    valarray
    Spk
    resize
    Yi
    indpar
$$

$section Function Objects for Models Defined in SpkModel$$

$index function object, for user-provided models provided in the form of a SpkModel methods$$
$index model, function object$$

$table
$bold Prototype:$$   $cend  
$syntax/class ModelFunctionValarray : public std::binary_function< valarray<double>, valarray<double>, valarray<double> >/$$
$rend
$bold Constructor:$$ $cend
$syntax/ModelFunctionValarray::ModelFunctionValarray(ModelFunctionValarray::* /model_proto/, SpkModel<double>* /m/)/$$ $rend
$tend


See also: $xref/ModelDerivative//ModelDerivative/$$
$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
A template class for a user-provided model provided in the form of a SpkModel member function.
Generalized function objects allow themselves to be passed to another function and be evaluated in a flexible manner.
It eliminates the need for specializing the evaluating-function for each passed-function with a unique prototype.

$head Arguments$$
$syntax/
/model_proto/
/$$
is a function pointer to a user-provided model you are trying to evaluate.
For example, if you are trying to create a function object for your SpkModel<double>::doDataMean(),
you specify $code dataMean$$ as $italic model_proto$$.
$pre

$$
See the specification of the model of your interest.  The specification
page describes both the virtual member you implement and its
public interface.

$table
$tref SpkModel_dataMean$$      $rend
$tref SpkModel_dataMean_indPar$$    $rend
$tref SpkModel_dataMean_popPar$$    $rend
$rend
$tref SpkModel_dataVariance$$      $rend
$tref SpkModel_dataVariance_indPar$$    $rend
$tref SpkModel_dataVariance_popPar$$    $rend
$rend
$tref SpkModel_dataVarianceInv$$   $rend
$tref SpkModel_dataVarianceInv_indPar$$ $rend
$tref SpkModel_dataVarianceInv_popPar$$ $rend
$rend 
$tref SpkModel_indParVariance$$       $rend
$tref SpkModel_indParVariance_popPar$$     $rend
$rend
$tref SpkModel_indParVarianceInv$$    $rend
$tref SpkModel_indParVarianceInv_popPar$$  $rend

$tend
$syntax/

* /m/
/$$
is a pointer to a SpkModel object.

$head Public Members$$
$syntax/
const valarray<double>/y/ operator(const valarray<double>& /x1/, const valarray<double>& /x2/) const
/$$

This operator re-defines $italic model_proto$$ as a binary function that takes $italic x1$$
and $italic x2$$ as variables and maps to $italic y$$.


$head Example$$
If you compile, link, and run the following program:
$codep
  #include "SpkValarray.h"
  #include "SpkModel.h"

  class UserPopModel : public SpkModel<double>
  {
      valarray<double> _alp;
      valarray<double> _b;
      valarray<int>    _nY;
      int              _who;
      
      int              _nYi;
      int              _nIndividuals;
      const int        _nAlp;
      const int        _nB;
  public:
      UserPopModel(int nAlp, int nB, valarray<int> nY)
      : _nAlp(nAlp), _nB(nB), _nY(nY)
      {
        _nIndividuals = _nY.size();
      };
      ~UserPopModel() throw() 
      {
      };
  protected:
      virtual void doSelectIndividual(int who)
      {
          _who = who;
      }
      virtual void doSetPopPar(const valarray<double>& alp)
      {
          _alp = alp;
      }
      virtual void doSetIndPar(const valarray<double>& b)
      {
          _b = b;
      }
      virtual void doDataMean( valarray<double>& ret ) const
      {
          //
          // f(alp, b) = [ 2.0 * b(1) ]
          //             [ 2.0 * b(1) ]
          //
          ret.resize( _nY[_who] );
          ret[0] = 2.0 * _b[0];
          ret[1] = 2.0 * _b[0];
      }
      virtual bool doDataMean_indPar( valarray<double>& ret ) const
      {
          //
          // f_b(alp, b) = [ 2.0   0.0 ]
          //               [ 2.0   0.0 ]
          //
          ret.resize( _nY[_who], _nB);
          ret[0] = 2.0;
          ret[1] = 2.0;
          ret[2] = 0.0;
          ret[3] = 0.0;
      }
      virtual bool doDataMean_popPar( valarray<double>& ret ) const
      {
          //
          // f_alp(alp, b) = [ 0.0   0.0 ]
          //                 [ 0.0   0.0 ]
          //
          ret.resize( _nY[_who], _nAlp);
          for( int i=0; i<_nY[_who]*_nAlp; i++ )
            ret[i] = 0.0;
          return false;
      }
      virtual void doDataVariance( valarray<double>& ret ) const
      {
          //
          // Ri(alp, b) = [ b(2)  0.0  ]
          //              [ 0.0   b(2) ]
          //
          ret.resize( _nY[_who], _y[_who]);
          ret[0] = _b[1];
          ret[1] = 0.0;
          ret[2] = 0.0;
          ret[3] = _b[1];
      }
      virtual bool doDataVariance_indPar( valarray<double>& ret ) const
      {
          //
          // Ri_b(alp,b) = [ 0.0    1.0 ]
          //               [ 0.0    0.0 ]
          //               [ 0.0    0.0 ]
          //               [ 0.0    1.0 ]
          //          
          ret.resize(_nY[_who]*_nY[_who], _nB);
          for( int i=0; i<_nY[_who]*_nY[_who]*_nB; i++ )
            ret[i] = 0.0;
          
          ret[4] = 1.0;
          ret[7] = 1.0;
          return true;
      }
      virtual bool doDataVariance_popPar( valarray<double>& ret ) const
      {
          //
          // Ri_alp(alp,b) = [ 0.0    0.0 ]
          //                 [ 0.0    0.0 ]
          //                 [ 0.0    0.0 ]
          //                 [ 0.0    0.0 ]
          //          
          ret.resize(_nY[_who]*_nY[_who], _nAlp);
          for( int i=0; i<_nY[_who]*_nY[_who]*_nAlp; i++ )
            ret[i] = 0.0;
          
          return false;
      }
      virtual void doDataVarianceInv( valarray<double>& ret ) const
      {
          //
          // Ri(alp, b)^(-1) = [ 1.0 / b(2)  0.0        ]
          //                   [ 0.0         1.0 / b(2) ]
          //
          ret.resize(_nY[_who], _nY[_who]);
          ret[0] = 1.0 / _b[1];
          ret[1] = 0.0;
          ret[2] = 0.0;
          ret[3] = 1.0 / _b[1];
      }
      virtual bool doDataVarianceInv_indPar( valarray<double>& ret ) const
      {
          //
          // Ri^(-1)_b(alp,b) = [ 0.0    -1.0 / b(2)^2 ]
          //                    [ 0.0     0.0          ]
          //                    [ 0.0     0.0          ]
          //                    [ 0.0    -1.0 / b(2)^2 ]
          //          
          ret.resize(_nY[_who]*_nY[_who], _nB);
          for( int i=0; i<_nY[_who]*_nY[_who]*_nB; i++ )
            ret[i] = 0.0;
          
          ret[4] = -1.0 / ( _b[1] * _b[1] );
          ret[7] = -1.0 / ( _b[1] * _b[1] );
          return true;
      }
      virtual bool doDataVarianceInv_popPar( valarray<double>& ret ) const
      {
          //
          // Ri^(-1)_alp(alp,b) = [ 0.0    0.0 ]
          //                      [ 0.0    0.0 ]
          //                      [ 0.0    0.0 ]
          //                      [ 0.0    0.0 ]
          //          
          ret.resize(_nY[_who]*_nY[_who], _nAlp);
          for( int i=0; i<_nY[_who]*_nY[_who]*_nAlp; i++ )
            ret[i] = 0.0;
          
          return false;
      }
      virtual void doIndParVariance( valarray<double>& ret ) const
      {
          //
          // D(alp) = [ 1.0  0.0 ]
          //          [ 0.0  1.0 ]
          //
          ret.resize(_nB, _nB);
          ret[0] = 1.0;
          ret[1] = 0.0;
          ret[2] = 0.0;
          ret[3] = 1.0;
      }
      virtual void doIndParVarianceInv( valarray<double>& ret ) const
      {
          //
          // D(alp)^(-1) = [ 1.0  0.0 ]
          //               [ 0.0  1.0 ]
          //
          ret.resize(_nB, _nB);
          ret[0] = 1.0;
          ret[1] = 0.0;
          ret[2] = 0.0;
          ret[3] = 1.0;
          return true;
      }
      virtual bool doIndParVariance_popPar( valarray<double>& ret ) const
      {
          //
          // D_alp(alp) = [ 0.0    0.0 ]
          //              [ 0.0    0.0 ]
          //              [ 0.0    0.0 ]
          //              [ 0.0    0.0 ]
          //          
          ret.resize(_nB*_nB, _nAlp);
          for( int i=0; i<_nB*_nB*_nAlp; i++ )
            ret[i] = 0.0;
          
          return false;
      }
      virtual bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
      {
          //
          // D^(-1)_alp(alp) = [ 0.0    0.0 ]
          //                   [ 0.0    0.0 ]
          //                   [ 0.0    0.0 ]
          //                   [ 0.0    0.0 ]
          //          
          ret.resize(_nB*_nB, _nAlp);
          for( int i=0; i<_nB*_nB*_nAlp; i++ )
            ret[i] = 0.0;
          
          return false;
      }
  };

  void PopModelFunctionTest()
  {
      const int nIndPar = 2;
      const int nPopPar = 2;
      const int nY1     = 2;
      const int nIndividuals = 1;

      //
      // Initialize a vector containing the numbers of measurements per individual.
      //
      //   nY = [ nY1 ]
      //
      valarray<int> nY(nIndividuals);
      nY[0] = nY1;

      //
      // Instantiate a PopModelFunctionTest object
      //
      UserPopModel model(nPopPar, nIndpar, nY);

      //
      // popPar = [ 1.0 ]
      //          [ 1.0 ]
      //
      valarray<double> popPar( nPopPar );
      popPar[0] = 1.0;
      popPar[1] = 1.0;

      //
      // indPar = [ 1.0 ]
      //          [ 1.0 ]
      //
      valarray<double> indPar( nIndPar );
      indPar[0] = 1.0;
      indPar[1] = 1.0;

      //
      // Instantiate a function object for SpkModel<double>::dataMean( valarray& )
      //
      ModelFunctionValarray fOb( SpkModel<double>::dataMean, &model );

      //
      // Let the function object computes and return the function value.
      //
      valarray<double> fObOut = fOb( popPar, indPar );

      //
      // Directly evaluate the function.
      //
      valarray<double> fOut;
      model.dateMean( fOut );

      cout << "Function object returned: { ";
      for( int i=0; i<fObOut.size(); i++ )
      {
          cout << fObOut[i] << " ";
      }
      cout << "}" << endl;

      cout << "SpkModel<double>::dataMean() returned: { ";
      for( int i=0; i<fOut.size(); i++ )
      {
          cout << fOut[i] << " ";
      }
      cout << "}" << endl;
$$

then it will display the following when it is run:
$codep
      Function object returned: { 2.0 2.0 }
      SpkModel<double>::dataMean() returned: { 2.0 2.0 }
$$
  
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <cassert>
#include <functional>

#include "Function.h"
#include "SpkValarray.h"
#include "SpkModel.h"
#include "DoubleMatrix.h"

using SPK_VA::valarray;

ModelFunction::ModelFunction(const model_proto f, SpkModel<double>* m)
    :    fun(f), model(m)
{
}
ModelFunction::~ModelFunction() throw() 
{
}
ModelFunction::ModelFunction(const ModelFunction& right)
    :    fun(right.fun), model(right.model)
{
}
const DoubleMatrix ModelFunction::operator()(const DoubleMatrix& X1, const DoubleMatrix& X2) const
{
  const valarray<double> x1 = X1.toValarray();
  const valarray<double> x2 = X2.toValarray();

  model->setPopPar(x1);
  model->setIndPar(x2);
  (model->*fun)(ret);
  return ret;
}

ModelFunctionValarray::ModelFunctionValarray(const model_proto f, SpkModel<double>* m)
    :    fun(f), model(m)
{
}
ModelFunctionValarray::~ModelFunctionValarray() throw() {}
ModelFunctionValarray::ModelFunctionValarray(const ModelFunctionValarray& right)
    :    fun(right.fun), model(right.model)
{
}
const valarray<double> ModelFunctionValarray::operator()(const valarray<double>& x1, const valarray<double>& x2) const
{
    model->setPopPar(x1);
    model->setIndPar(x2);
    (model->*fun)(ret);
    return ret;
}


/*************************************************************************
 *
 * Class: ModelFunction class
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin ModelDerivative$$
$spell
	Model model 
    arg 
    typename 
    std 
    cend 
    proto 
    ob 
    int 
    endl 
    cout 
    bool 
    Ri 
    fi 
    inv 
    Rinv 
    ind 
    pop
    const 
    iostream 
    typedef 
    struct 
    nr 
    bval 
    instantiate 
    init
    fval 
    var 
    Rval 
    nagation 
    const  
    res 
    binary_function 
    st 
    centdiff 
    approx
    covariances
    valarray
    Spk
    resize
    Yi
    Indpar
$$

$section Function Objects for Derivative Models Defined in SpkModel$$

$index function object, for user-provided derivative models provided in the form of a SpkModel methods$$
$index model, function object$$

$table
$bold Prototype:$$   $cend  
$syntax/class ModelDerivative : public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>/$$
$rend
$bold Constructor:$$ $cend
$syntax/ModelDerivative::ModelDerivative(ModelDerivative::* /model_proto/, SpkModel<double>* /m/)/$$ $rend
$syntax/ModelDerivativeValarray::ModelDerivativeValarray(ModelDerivativeValarray::* /model_proto/, SpkModel<double>* /m/)/$$ $rend
$tend

See also: $xref/ModelFunction//ModelFunction/$$
$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
A template class for a user-provided derivative model provided in the form of a SpkModel member function.
Generalized function objects allow themselves to be passed to another function and be evaluated in a flexible manner.
It eliminates the need for specializing the evaluating-function for each passed-function with a unique prototype.

$head Arguments$$
$syntax/
/model_proto/
/$$
is a function pointer to a user-provided model you are trying to evaluate.
For example, if you are trying to create a function object for your void SpkModel<double>::doDataMean(DoubleMatrix&),
you specify $code dataMean$$ as $italic model_proto$$.
$pre

$$
See the specification of the model of your interest.  The specification
page describes both the virtual member you implement and its
public interface.

$table
$tref SpkModel_dataMean$$      $rend
$tref SpkModel_dataMean_indPar$$    $rend
$tref SpkModel_dataMean_popPar$$    $rend
$rend
$tref SpkModel_dataVariance$$      $rend
$tref SpkModel_dataVariance_indPar$$    $rend
$tref SpkModel_dataVariance_popPar$$    $rend
$rend
$tref SpkModel_dataVarianceInv$$   $rend
$tref SpkModel_dataVarianceInv_indPar$$ $rend
$tref SpkModel_dataVarianceInv_popPar$$ $rend
$rend 
$tref SpkModel_indParVariance$$       $rend
$tref SpkModel_indParVariance_popPar$$     $rend
$rend
$tref SpkModel_indParVarianceInv$$    $rend
$tref SpkModel_indParVarianceInv_popPar$$  $rend

$tend
$syntax/

* /m/
/$$
is a pointer to a SpkModel object.

$head Public Members$$
$syntax/
const DoubleMatrix operator(const DoubleMatrix& /A/, const DoubleMatrix& /B/) const
/$$

$head Example$$
If you compile, link, and run the following program:
$codep
  #include "SpkValarray.h"
  #include "SpkModel.h"
  #include "SpkValarray.h"

  class UserPopModel : public SpkModel<double>
  {
      valarray<double> _alp;
      valarray<double> _b;
      valarray<int>    _nY;
      int              _who;
      
      int              _nYi;
      int              _nIndividuals;
      const int        _nAlp;
      const int        _nB;
  public:
      UserPopModel(int nAlp, int nB, valarray<int> nY)
      : _nAlp(nAlp), _nB(nB), _nY(nY)
      {
        _nIndividuals = _nY.size();
      };
      ~UserPopModel() throw() 
      {
      };
  protected:
      virtual void doSelectIndividual(int who)
      {
          _who = who;
      }
      virtual void doSetPopPar(const valarray<double>& alp)
      {
          _alp = alp;
      }
      virtual void doSetIndPar(const valarray<double>& b)
      {
          _b = b;
      }
      virtual void doDataMean( valarray<double>& ret ) const
      {
          //
          // f(alp, b) = [ 2.0 * b(1) ]
          //             [ 2.0 * b(1) ]
          //
          ret.resize( _nY[_who] );
          ret[0] = 2.0 * _b[0];
          ret[1] = 2.0 * _b[0];
      }
      virtual bool doDataMean_indPar( valarray<double>& ret ) const
      {
          //
          // f_b(alp, b) = [ 2.0   0.0 ]
          //               [ 2.0   0.0 ]
          //
          ret.resize( _nY[_who], _nB);
          ret[0] = 2.0;
          ret[1] = 2.0;
          ret[2] = 0.0;
          ret[3] = 0.0;
      }
      virtual bool doDataMean_popPar( valarray<double>& ret ) const
      {
          //
          // f_alp(alp, b) = [ 0.0   0.0 ]
          //                 [ 0.0   0.0 ]
          //
          ret.resize( _nY[_who], _nAlp);
          for( int i=0; i<_nY[_who]*_nAlp; i++ )
            ret[i] = 0.0;
          return false;
      }
      virtual void doDataVariance( valarray<double>& ret ) const
      {
          //
          // Ri(alp, b) = [ b(2)  0.0  ]
          //              [ 0.0   b(2) ]
          //
          ret.resize( _nY[_who], _y[_who]);
          ret[0] = _b[1];
          ret[1] = 0.0;
          ret[2] = 0.0;
          ret[3] = _b[1];
      }
      virtual bool doDataVariance_indPar( valarray<double>& ret ) const
      {
          //
          // Ri_b(alp,b) = [ 0.0    1.0 ]
          //               [ 0.0    0.0 ]
          //               [ 0.0    0.0 ]
          //               [ 0.0    1.0 ]
          //          
          ret.resize(_nY[_who]*_nY[_who], _nB);
          for( int i=0; i<_nY[_who]*_nY[_who]*_nB; i++ )
            ret[i] = 0.0;
          
          ret[4] = 1.0;
          ret[7] = 1.0;
          return true;
      }
      virtual bool doDataVariance_popPar( valarray<double>& ret ) const
      {
          //
          // Ri_alp(alp,b) = [ 0.0    0.0 ]
          //                 [ 0.0    0.0 ]
          //                 [ 0.0    0.0 ]
          //                 [ 0.0    0.0 ]
          //          
          ret.resize(_nY[_who]*_nY[_who], _nAlp);
          for( int i=0; i<_nY[_who]*_nY[_who]*_nAlp; i++ )
            ret[i] = 0.0;
          
          return false;
      }
      virtual void doDataVarianceInv( valarray<double>& ret ) const
      {
          //
          // Ri(alp, b)^(-1) = [ 1.0 / b(2)  0.0        ]
          //                   [ 0.0         1.0 / b(2) ]
          //
          ret.resize(_nY[_who], _nY[_who]);
          ret[0] = 1.0 / _b[1];
          ret[1] = 0.0;
          ret[2] = 0.0;
          ret[3] = 1.0 / _b[1];
      }
      virtual bool doDataVarianceInv_indPar( valarray<double>& ret ) const
      {
          //
          // Ri^(-1)_b(alp,b) = [ 0.0    -1.0 / b(2)^2 ]
          //                    [ 0.0     0.0          ]
          //                    [ 0.0     0.0          ]
          //                    [ 0.0    -1.0 / b(2)^2 ]
          //          
          ret.resize(_nY[_who]*_nY[_who], _nB);
          for( int i=0; i<_nY[_who]*_nY[_who]*_nB; i++ )
            ret[i] = 0.0;
          
          ret[4] = -1.0 / ( _b[1] * _b[1] );
          ret[7] = -1.0 / ( _b[1] * _b[1] );
          return true;
      }
      virtual bool doDataVarianceInv_popPar( valarray<double>& ret ) const
      {
          //
          // Ri^(-1)_alp(alp,b) = [ 0.0    0.0 ]
          //                      [ 0.0    0.0 ]
          //                      [ 0.0    0.0 ]
          //                      [ 0.0    0.0 ]
          //          
          ret.resize(_nY[_who]*_nY[_who], _nAlp);
          for( int i=0; i<_nY[_who]*_nY[_who]*_nAlp; i++ )
            ret[i] = 0.0;
          
          return false;
      }
      virtual void doIndParVariance( valarray<double>& ret ) const
      {
          //
          // D(alp) = [ 1.0  0.0 ]
          //          [ 0.0  1.0 ]
          //
          ret.resize(_nB, _nB);
          ret[0] = 1.0;
          ret[1] = 0.0;
          ret[2] = 0.0;
          ret[3] = 1.0;
      }
      virtual void doIndParVarianceInv( valarray<double>& ret ) const
      {
          //
          // D(alp)^(-1) = [ 1.0  0.0 ]
          //               [ 0.0  1.0 ]
          //
          ret.resize(_nB, _nB);
          ret[0] = 1.0;
          ret[1] = 0.0;
          ret[2] = 0.0;
          ret[3] = 1.0;
          return true;
      }
      virtual bool doIndParVariance_popPar( valarray<double>& ret ) const
      {
          //
          // D_alp(alp) = [ 0.0    0.0 ]
          //              [ 0.0    0.0 ]
          //              [ 0.0    0.0 ]
          //              [ 0.0    0.0 ]
          //          
          ret.resize(_nB*_nB, _nAlp);
          for( int i=0; i<_nB*_nB*_nAlp; i++ )
            ret[i] = 0.0;
          
          return false;
      }
      virtual bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
      {
          //
          // D^(-1)_alp(alp) = [ 0.0    0.0 ]
          //                   [ 0.0    0.0 ]
          //                   [ 0.0    0.0 ]
          //                   [ 0.0    0.0 ]
          //          
          ret.resize(_nB*_nB, _nAlp);
          for( int i=0; i<_nB*_nB*_nAlp; i++ )
            ret[i] = 0.0;
          
          return false;
      }
  };

  void PopModelFunctionTest()
  {
      const int nIndPar = 2;
      const int nPopPar = 2;
      const int nY1     = 2;
      const int nIndividuals = 1;

      //
      // Initialize a vector containing the numbers of measurements per individual.
      //
      //   nY = [ nY1 ]
      //
      valarray<int> nY(nIndividuals);
      nY[0] = nY1;

      //
      // Instantiate a PopModelFunctionTest object
      //
      UserPopModel model(nPopPar, nIndpar, nY);

      //
      // popPar = [ 1.0 ]
      //          [ 1.0 ]
      //
      valarray<double> popPar( nPopPar );
      popPar[0] = 1.0;
      popPar[1] = 1.0;

      //
      // indPar = [ 1.0 ]
      //          [ 1.0 ]
      //
      valarray<double> indPar( nIndPar );
      indPar[0] = 1.0;
      indPar[1] = 1.0;

      //
      // Instantiate a function object for SpkModel<double>::dataMean_indPar( valarray& )
      //
      ModelDerivativeValarray f_bOb( SpkModel<double>::dataMean_indPar, &model );

      //
      // Let the function object computes and return the function value.
      //
      valarray<double> f_bObOut = f_bOb( popPar, indPar );

      //
      // Directly evaluate the function.
      //
      valarray<double> f_bOut;
      model.dateMean_indPar( f_bOut );

      cout << "Function object returned: { ";
      for( int i=0; i<f_bObOut.size(); i++ )
      {
          cout << f_bObOut[i] << " ";
      }
      cout << "}" << endl;

      cout << "SpkModel<double>::dataMean_indPar() returned: { ";
      for( int i=0; i<f_bOut.size(); i++ )
      {
          cout << f_bOut[i] << " ";
      }
      cout << "}" << endl;
$$

then it will display the following when it is run:
$codep
      Function object returned: { 2.0 2.0 0.0 0.0 }
      SpkModel<double>::dataMean_indPar() returned: { 2.0 2.0 0.0 0.0 }
$$
$end
*/
ModelDerivative::ModelDerivative(const model_proto f, SpkModel<double>* m)
    :    fun(f), model(m)
{
}
ModelDerivative::~ModelDerivative() throw() 
{
}
ModelDerivative::ModelDerivative(const ModelDerivative& right)
    :    fun(right.fun), model(right.model)
{
}
const DoubleMatrix ModelDerivative::operator()(const DoubleMatrix& X1, const DoubleMatrix& X2) const
{
  const valarray<double> x1 = X1.toValarray();
  const valarray<double> x2 = X2.toValarray();

  model->setPopPar(x1);
  model->setIndPar(x2);
  (model->*fun)(ret);
  return ret;
}

ModelDerivativeValarray::ModelDerivativeValarray(const model_proto f, SpkModel<double>* m)
    :    fun(f), model(m)
{
}
ModelDerivativeValarray::~ModelDerivativeValarray() throw() 
{
}
ModelDerivativeValarray::ModelDerivativeValarray(const ModelDerivativeValarray& right)
    :    fun(right.fun), model(right.model)
{
}
const valarray<double> ModelDerivativeValarray::operator()(const valarray<double>& x1, const valarray<double>& x2) const
{
    model->setPopPar(x1);
    model->setIndPar(x2);
    (model->*fun)(ret);
    return ret;
}


