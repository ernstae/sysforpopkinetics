#ifndef NONMEM_TRANSLATOR_H
#define NONMEM_TRANSLATOR_H

/**
 * @file NonmemTranslator.h
 * Delares NonmemTranslator class and DataColumn, DataRecords classes.
 */
#include <vector>
#include <valarray>
#include <xercesc/dom/DOM.hpp>

#include "../SpkParameters.h"
#include "../SpkMLToCpp.h"
#include "../client.h"
#include "../ExpTreeGenerator.h"

class NonmemDataColumn
{
  public:
  char* label;
  char* synonym;
  std::valarray<double> values;

  NonmemDataColumn() : label(0), synonym(0){}
  NonmemDataColumn( int nMeasurements ) :values(nMeasurements), label(0), synonym(0){}
};

class NonmemDataRecords
{
 public:
  char* owner;
  std::vector<NonmemDataColumn> records;

  NonmemDataRecords() : owner(0){}
  NonmemDataRecords( int nRecords ) : records( nRecords ) {}
};
struct NonmemParameters
{
  /**
   * all data
   */
  std::vector<NonmemDataRecords> data;

  /**
   * theta
   */
  std::valarray<bool>   thetaFixed;
  std::valarray<double> thetaIn;
  std::valarray<double> thetaLow;
  std::valarray<double> thetaUp;
  /**
   * Omega
   */
  std::valarray<bool>   omegaFixed;
  std::valarray<double> omegaIn;

  /**
   * Sigma
   */
  std::valarray<bool>   sigmaFixed;
  std::valarray<double> sigmaIn;

  /**
   * eta
   */
  std::valarray<bool>   etaFixed;
  std::valarray<double> etaIn;
};

class NonmemTranslator : public ClientTranslator
{
 public:
  NonmemTranslator( xercesc::DOMDocument * doc );
  ~NonmemTranslator();

  virtual const struct SpkParameters * getSpkParameters() const;
  virtual const void * getClientParameters() const;
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

