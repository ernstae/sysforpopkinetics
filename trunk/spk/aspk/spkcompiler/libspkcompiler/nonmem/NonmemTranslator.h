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
#include <map>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLChar.hpp>

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
  enum NonmemModel { NONE,   
                     ADVAN1,  ADVAN2,  ADVAN3,  ADVAN4,  ADVAN5, 
		     ADVAN6,  ADVAN7,  ADVAN8,  ADVAN9,  ADVAN10, 
		     ADVAN11, ADVAN12 
                   };
  static enum NonmemModel toNonmemModelEnum( const char* );
  static const char* const toNonmemModelString( enum NonmemModel e );

  static const char * const STR_NONE;
  static const char * const STR_ADVAN1;
  static const char * const STR_ADVAN2;
  static const char * const STR_ADVAN3;
  static const char * const STR_ADVAN4;
  static const char * const STR_ADVAN5;
  static const char * const STR_ADVAN6;
  static const char * const STR_ADVAN7;
  static const char * const STR_ADVAN8;
  static const char * const STR_ADVAN9;
  static const char * const STR_ADVAN10;
  static const char * const STR_ADVAN11;
  static const char * const STR_ADVAN12;

  enum NonmemParameterization { DEFAULT,
                     TRANS1, TRANS2, TRANS3, TRANS4, TRANS5 };
  static enum NonmemParameterization toNonmemParameterizationEnum( 
                              const char* );
  static const char* const toNonmemParameterizationString( 
			      enum NonmemParameterization e );

  static const char * const STR_DEFAULT;
  static const char * const STR_TRANS1;
  static const char * const STR_TRANS2;
  static const char * const STR_TRANS3;
  static const char * const STR_TRANS4;
  static const char * const STR_TRANS5;

  // LABEL data type:
  // The reason for the key data type being "string"
  // instead of "char*" is that, if I use "char*" type, 
  // the memory address is rather used as a key, causing multiple entries
  // getting registered in the map for the same string/name.
  typedef std::string LABEL;

  // ALIAS data type:
  // The reason for the key data type being "string"
  // instead of "char*" is to be consistent with LABEL data type.
  typedef std::string ALIAS;

  // MEASUREMENT data type:
  // Let's just make them all expressed in double-precision.
  typedef std::valarray<double> MEASUREMENT;

  NonmemTranslator();
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

  NonmemTranslator( const NonmemTranslator& right );
  const NonmemTranslator& operator=( const NonmemTranslator& right );

 private:

  enum NonmemModel nonmemModel;
  enum NonmemParameterization nonmemParameterization;

  ExpTreeGenerator expTreeUtils;

  struct SpkParameters ourSpk;
  struct NonmemParameters ourNonmem;

  std::vector<NonmemDataRecords> data_for_all_subjects;

  void initSymbolTable( SymbolTable& );
  void readModel(   xercesc::DOMDocument* tree, 
		    int nIndividuals,
		    SymbolTable * table);

  void emitDriver();
  void emitModel();
  std::vector<std::string> emitData( 		
		    int nIndividuals,
		    SymbolTable* table,
		    const std::map<LABEL, ALIAS> & label_alias_mapping,
		    const std::map<LABEL, MEASUREMENT> data_for[],
		    const std::string order_id_pair[]
		 );
};

#endif

