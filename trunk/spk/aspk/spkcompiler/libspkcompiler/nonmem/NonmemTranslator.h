#ifndef NONMEM_TRANSLATOR_H
#define NONMEM_TRANSLATOR_H

/**
 * @file NonmemTranslator.h
 *
 * In this header file, tools and data structures specific to NONMEM
 * are declared: NonmemTranslator, NonmemDataColumn, NonmemDataRecords,
 * and NonmemParameters classes. 
 *
 * @defgroup nonmem NONMEM
 */
#include <vector>
#include <valarray>
#include <xercesc/dom/DOM.hpp>

#include "../SpkParameters.h"
#include "../SpkMLToCpp.h"
#include "../client.h"
#include "../ExpTreeGenerator.h"

/**
 * The representation of a measurement (a series of data values)
 * for a subject.
 * @ingroup nonmem
 */
class NonmemDataColumn
{
  public:
  /**
   * The label for the @ref measurement.
   */	 
  char* label;

  /**
   * An alias/synonym for the @ref label.
   */
  char* synonym;

  /**
   * A measurement (a series of data values) for a subject.
   */
  std::valarray<double> measurement;

  /**
   * Initializes the object and set @ref label and @ref synonym to NULL.
   */
  NonmemDataColumn() : label(0), synonym(0){}

  /**
   * Initializes the object and set @ref label and @ref synonym to NULL
   * and reserves the length of @ref measurement vector to @b nMeasurements.
   */
  NonmemDataColumn( int nMeasurements ) 
	  : measurement(nMeasurements), label(0), synonym(0){}
};

/**
 * The representation of a set of measurements associated
 * with a subject.
 *
 * @ingroup nonmem
 */
class NonmemDataRecords
{
 public:
  /**
   * An alpha-numerical value uniquely identifying the subject.
   */
  char* owner;

  /**
   * A set of measurements for the subject identified by @ref owner.
   */
  std::vector<NonmemDataColumn> records;

  /**
   * Initializes the object and set the @ref owner pointer to NULL.
   */
  NonmemDataRecords() : owner(0){}

  /**
   * Initializes the object and sets the @ref owner pointer to NULL and
   * reserves the length of the @ref records vector to @b nRecords.
   */
  NonmemDataRecords( int nRecords ) : records( nRecords ) {}
};

/**
 * @struct NonmemParameters
 * This data structure aggregates the information necessarily
 * to have NONMEM estimate the population parameter,
 * expressed in the NONMEM terms.
 * 
 * @defgroup NonmemParas Nonmem Parameters
 * @ingroup nonmem
 */
struct NonmemParameters
{
  /**
   * All measurements for all subjects in a population.
   * @ingroup NonmemParas
   */
  std::vector<NonmemDataRecords> data;

  /**
   * @defgroup theta theta
   * @ingroup NonmemParas
   */
  /**
   * A flag indicating as to whether @ref theta is fixed during
   * the optimization.
   * @ingroup theta
   */
  std::valarray<bool>   thetaFixed;
  
  /**
   * A vector holding the initial estimate of @ref theta.
   * @ingroup theta
   */
  std::valarray<double> thetaIn;

  /**
   * A vector holding the lower boundary values for @ref theta.
   * This value is ignored if @ref thetaFixed is set to true.
   * @ingroup theta
   */
  std::valarray<double> thetaLow;

  /**
   * A vector holding the upper boundary values for @ref theta.
   * This value is ignored if @ref thetaFixed is set to true.
   * @ingroup theta
   */
  std::valarray<double> thetaUp;

  /**
   * @defgroup Omega Omega
   * @ingroup NonmemParas
   * */
  /**
   * A flag indicating as to whether @ref Omega is fixed during
   * the optimization.
   * @ingroup Omega
   */
  std::valarray<bool>   omegaFixed;

  /**
   * A vector holding the initial estimate of @ref Omega.
   * @ingroup Omega
   */
  std::valarray<double> omegaIn;

  /**
   * @defgroup Sigma Sigma
   * @ingroup NonmemParas
   */
  /**
   * A flag indicating as to whether @ref Sigma is fixed during
   * the optimization.
   * @ingroup Sigma
   */
  std::valarray<bool>   sigmaFixed;

  /**
   * A vector holding the initial estimate of @ref Sigma.
   * @ingroup Sigma
   */
  std::valarray<double> sigmaIn;

  /**
   * @defgroup eta eta
   * @ingroup NonmemParas
   */
  /**
   * A flag indicating as to whether @ref eta is fixed during
   * the optimization.
   * @ingroup eta
   */
  std::valarray<bool>   etaFixed;

  /**
   * A vector holding the initial estimate of @ref eta.
   * @ingroup eta
   */
  std::valarray<double> etaIn;
};

/**
 * An instance of ClientTranslator, specialized for interpreting
 * the input generated by NONMEM.
 * @ingroup nonmem
 */
class NonmemTranslator : public ClientTranslator
{
 public:
  /**
   * The only legal constructor.  The DOM parse tree generated
   * from the input SpkML document, pointed by @b doc,
   * is traversed and translated into C++ source.
   *
   * @param doc points to the rooot of DOM parse tree.
   * 
   */
  NonmemTranslator( xercesc::DOMDocument * doc );
  ~NonmemTranslator();

  
  /**
   * Obtain a pointer to the NonmemParameters data structure object.
   *
   * @return a pointer to the NonmemParameters data structure object.
   * The items in the structure are populated during the process of
   * translation (by translate()).
   */
  virtual const void * getClientParameters() const;
  
  virtual const struct SpkParameters * getSpkParameters() const;
  virtual void translate ( xercesc::DOMDocument * tree );
  virtual const char * getDriverFilename() const;
  virtual const std::vector< const char * > getModelFilenameList() const;
  
 protected:

  NonmemTranslator();
  NonmemTranslator( const NonmemTranslator& right );
  const NonmemTranslator& operator=( const NonmemTranslator& right );

 private:

  enum CannedModel { NONE, 
		   ADVAN1, ADVAN2, ADVAN3, ADVAN4, ADVAN5, 
		   ADVAN6, ADVAN7, ADVAN8, ADVAN9, ADVAN10, 
		   ADVAN11, ADVAN12 };
  enum CannedModel canned_model;
  bool isCannedModelUsed;

  ExpTreeGenerator expTreeUtils;

  const xercesc::DOMDocument * tree;

  struct SpkParameters spk;
  struct NonmemParameters nonmem;

  std::vector<NonmemDataRecords> data_for_all_subjects;

  virtual void assemble ( xercesc::DOMDocument * tree );
  virtual void emit     ( xercesc::DOMDocument * tree );

  void initSymbolTable( SymbolTable& );
  void interpretContent();
  void interpretDriver();
  void interpretModel();
  void interpretData();
  void emitDriver();
  void emitModel();
};
#endif

