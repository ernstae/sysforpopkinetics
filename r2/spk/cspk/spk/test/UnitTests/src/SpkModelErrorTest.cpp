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
 * File: SpkModelErrorTest.cpp
 *
 *
 * Exception throwing test cases for SpkModel
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#include <iostream>
#include <fstream>
#include <string>
#include <exception>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "SpkModelErrorTest.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/SpkException.h"
#include "../../../spk/SpkValarray.h"

using SPK_VA::valarray;
using namespace CppUnit;

void allThrowModelsTests();
void throwModelTest(SpkModel<double>* model);
void testModelCase( SpkModel<double> * model, spkmodelerrortest::model_proto f, SpkError::ErrorCode code );
void testModelCase( SpkModel<double> * model, spkmodelerrortest::deriv_proto f, SpkError::ErrorCode code );

/************************************************************
 *
 * A Model that throw integer as an error object
 *
 ************************************************************/
class SpkModelPartiallyImplementedModel : public SpkModel<double>
{
private:
  int _i;
  valarray<double> _alp;
  valarray<double> _b;
public:
  SpkModelPartiallyImplementedModel(){}
  ~SpkModelPartiallyImplementedModel(){}
};

class throwIntModel : public SpkModel<double>
{
private:
  int _i;
  valarray<double> _alp;
  valarray<double> _b;
  int _error;

public:
  throwIntModel(){}
  ~throwIntModel(){}

private:
  void doSelectIndividual(int i)
  {
    _i = i;
    throw _error;
  }
  void doSetPopPar( const valarray<double> & alp )
  {
    _alp = alp;
    throw _error;
  }
  void doSetIndPar( const valarray<double> & b )
  {
    _b   = b;
    throw _error;
  }
  void doDataMean( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataMean_indPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataMean_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }

  void doDataVariance( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVariance_indPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVariance_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }

  void doDataVarianceInv( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVarianceInv_indPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVarianceInv_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }

  void doIndParVariance( valarray<double>& ret ) const
  {
    throw _error;
  }
  void doIndParVarianceInv( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doIndParVariance_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }
};
/************************************************************
 *
 * A Model that throw std::exception as an error object
 *
 ************************************************************/
class throwStdExceptionModel : public SpkModel<double>
{
private:
  int _i;
  valarray<double> _alp;
  valarray<double> _b;
  std::exception _error;

public:
  throwStdExceptionModel() : _error( std::bad_exception() ) {}
  ~throwStdExceptionModel(){}

private:
  void doSelectIndividual(int i)
  {
    _i = i+1;
    throw _error;
  }
  void doSetPopPar( const valarray<double> & alp )
  {
    _alp = alp;
    throw _error;
  }
  void doSetIndPar( const valarray<double> & b )
  {
    _b   = b;
    throw _error;
  }


  // new set
  void doDataMean( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataMean_indPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataMean_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }

  void doDataVariance( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVariance_indPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVariance_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }

  void doDataVarianceInv( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVarianceInv_indPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVarianceInv_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }

  void doIndParVariance( valarray<double>& ret ) const
  {
    throw _error;
  }
  void doIndParVarianceInv( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doIndParVariance_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }
};
/************************************************************
 *
 * A Model that throw exceptions
 *
 ************************************************************/
class throwSpkExceptionModel : public SpkModel<double>
{
private:
  int _i;
  valarray<double> _alp;
  valarray<double> _b;
  SpkException _error;

public:
  throwSpkExceptionModel() : _error(SpkError::SPK_UNKNOWN_ERR, "SpkException", __LINE__, __FILE__) {}
  ~throwSpkExceptionModel(){}

private:
  void doSelectIndividual(int i)
  {
    _i = i;
    throw _error;
  }
  void doSetPopPar( const valarray<double> & alp )
  {
    _alp = alp;
    throw _error;
  }
  void doSetIndPar( const valarray<double> & b )
  {
    _b   = b;
    throw _error;
  }


  // new set
  void doDataMean( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataMean_indPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataMean_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }

  void doDataVariance( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVariance_indPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVariance_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }

  void doDataVarianceInv( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVarianceInv_indPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doDataVarianceInv_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }

  void doIndParVariance( valarray<double>& ret ) const
  {
    throw _error;
  }
  void doIndParVarianceInv( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doIndParVariance_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }
  bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
  {
    throw _error;
  }
};
/************************************************************
 *
 * Template function that takes function pointers-to 
 * selectIndividual, setPopPar and setIndPar.
 * They should throw only SpkException.
 *
 ************************************************************/
template <typename T, typename P>
void testSetXXXCase( SpkModel<double> * model, T f, SpkError::ErrorCode code )
{
  using namespace std;

  const int n = 3;
  const P parameter(1);
  char stream[128];

  try{
    (model->*f)(parameter);
  }
  catch(const SpkException& e)
  {
    if( e.find(code) < 0)
    {
      sprintf(stream, "testSetXXXCase failed.  %d, %s\n", __LINE__,  __FILE__ );
      throw stream;
    }
    else
    {
      assert(true);
      return;
    }
  }
  catch(...)
  {
    sprintf( stream, "testSetXXXCase failed:  %d, %s\n", __LINE__, __FILE__ );

    throw stream;
  }
  sprintf( stream, "testSetXXXCase failed:  %d, %s\n", __LINE__, __FILE__ );

  throw stream;
}  

/************************************************************
 *
 * A function that takes function pointers-to 
 * model functions (ex. dataMean, dataVariance, indParVariance)
 * They should throw only SpkException.
 *
 ************************************************************/

void testModelCase( SpkModel<double> * model, spkmodelerrortest::model_proto f, SpkError::ErrorCode code )
{
  using namespace std;

  const int n = 3;
  char stream[128];

  try{
    valarray<double> ret;
    (model->*f)(ret);
  }
  catch(const SpkException& e)
  {
    if( e.find(code) < 0)
    {
      sprintf(stream, "testSetXXXCase failed.  %d, %s\n", __LINE__,  __FILE__ );
      throw stream;
    }
    else
    {
      assert(true);
      return;
    }
  }
  catch(...)
  {
    sprintf(stream, "testSetXXXCase failed.  %d, %s\n", __LINE__,  __FILE__ );
    throw stream;
  }
  sprintf(stream, "testSetXXXCase failed.  %d, %s\n", __LINE__,  __FILE__ );
  throw stream;
}

void testModelCase( SpkModel<double> * model, spkmodelerrortest::deriv_proto f, SpkError::ErrorCode code )
{
  using namespace std;

  const int n = 3;
  char stream[128];

  try{
    valarray<double> ret;
    (model->*f)(ret);
  }
  catch(const SpkException& e)
  {
    if( e.find(code) < 0)
    {
      sprintf(stream, "testSetXXXCase failed.  %d, %s\n", __LINE__,  __FILE__ );
      throw stream;
    }
    else
    {
      assert(true);
      return;
    }
  }
  catch(...)
  {
    sprintf(stream, "testSetXXXCase failed.  %d, %s\n", __LINE__,  __FILE__ );
    throw stream;
  }
  sprintf(stream, "testSetXXXCase failed.  %d, %s\n", __LINE__,  __FILE__ );
  throw stream;
}
void SpkModelErrorTest::setUp()
{
    // initializations
}
void SpkModelErrorTest::tearDown()
{
    // clean up
}

Test* SpkModelErrorTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "SpkModelErrorTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<SpkModelErrorTest>(
			 "throwIntModelCase", 
                         &SpkModelErrorTest::throwIntModelCase));
    suiteOfTests->addTest(new TestCaller<SpkModelErrorTest>(
                         "throwStdExceptionModelCase",
                         &SpkModelErrorTest::throwStdExceptionModelCase));
    suiteOfTests->addTest(new TestCaller<SpkModelErrorTest>(
			 "throwSpkExceptionModelCase", 
                         &SpkModelErrorTest::throwSpkExceptionModelCase));

    return suiteOfTests;
}

/************************************************************
 *
 * Run test cases for those models which throw
 * different error objects.
 *
 ************************************************************/
void SpkModelErrorTest::throwIntModelCase()
{
  throwIntModel model1;
  throwModelTest(&model1);
}
void SpkModelErrorTest::throwStdExceptionModelCase()
{
  throwStdExceptionModel model2;
  throwModelTest(&model2);
}
void SpkModelErrorTest::throwSpkExceptionModelCase()
{
  throwSpkExceptionModel model3;
  throwModelTest(&model3);

}
/************************************************************
 *
 * Exhaustive call to all SpkModel member functions.
 *
 ************************************************************/

void throwModelTest(SpkModel<double> *model)
{
    using namespace spkmodelerrortest;

    testSetXXXCase<selectIndividual_proto, int>
      (model, &throwIntModel::selectIndividual,  
      SpkError::SPK_MODEL_SET_INDEX_ERR);  
    
    testSetXXXCase<setXXXPar_proto, valarray<double> >
      (model, &throwIntModel::setPopPar, 
      SpkError::SPK_MODEL_SET_POP_ERR);  
    
    testSetXXXCase<setXXXPar_proto, valarray<double> >
      (model, &throwIntModel::setIndPar, 
      SpkError::SPK_MODEL_SET_IND_ERR);  
    

    testModelCase
      (model, &throwIntModel::dataMean, 
      SpkError::SPK_MODEL_DATA_MEAN_ERR);  
    
    testModelCase
      (model, &throwIntModel::dataMean_popPar, 
      SpkError::SPK_MODEL_DATA_MEAN_POP_ERR);  
    
    testModelCase
      (model, &throwIntModel::dataMean_indPar, 
      SpkError::SPK_MODEL_DATA_MEAN_IND_ERR);  
    

    testModelCase
      (model, &throwIntModel::dataVariance, 
      SpkError::SPK_MODEL_DATA_VARIANCE_ERR);  
    
    testModelCase
      (model, &throwIntModel::dataVariance_popPar,
      SpkError::SPK_MODEL_DATA_VARIANCE_POP_ERR);  
    
    testModelCase
      (model, &throwIntModel::dataVariance_indPar, 
      SpkError::SPK_MODEL_DATA_VARIANCE_IND_ERR);  
    

    testModelCase
      (model, &throwIntModel::dataVarianceInv, 
      SpkError::SPK_MODEL_INV_DATA_VARIANCE_ERR);  
    
    testModelCase
      (model, &throwIntModel::dataVarianceInv_popPar, 
      SpkError::SPK_MODEL_INV_DATA_VARIANCE_POP_ERR);  
    
    testModelCase
      (model, &throwIntModel::dataVarianceInv_indPar, 
      SpkError::SPK_MODEL_INV_DATA_VARIANCE_IND_ERR);  
    

    testModelCase
      (model, &throwIntModel::indParVariance, 
      SpkError::SPK_MODEL_IND_VARIANCE_ERR);  
    
    testModelCase
      (model, &throwIntModel::indParVariance_popPar, 
      SpkError::SPK_MODEL_IND_VARIANCE_POP_ERR);  
    

    testModelCase
      (model, &throwIntModel::indParVarianceInv, 
      SpkError::SPK_MODEL_INV_IND_VARIANCE_ERR);  
    
    testModelCase
      (model, &throwIntModel::indParVarianceInv_popPar, 
      SpkError::SPK_MODEL_INV_IND_VARIANCE_POP_ERR);  
    
}
