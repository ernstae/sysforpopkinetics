#ifndef NONMEM_SPKMLTOCPP_H
#define NONMEM_SPKMLTOCPP_H

#include <vector>
#include <valarray>
#include <xercesc/dom/DOM.hpp>

#include "../libcommon/SpkMLToCpp.h"
#include "../libcommon/client.h"
#include "../libcommon/ExpTreeGenerator.h"

class DataColumn
{
  public:
  char* label;
  char* synonym;
  std::valarray<double> values;

  DataColumn() : label(0), synonym(0){}
  DataColumn( int nMeasurements ) :values(nMeasurements), label(0), synonym(0){}
};

class DataRecords
{
 public:
  char* owner;
  std::vector<DataColumn> records;

  DataRecords() : owner(0){}
  DataRecords( int nRecords ) : records( nRecords ) {}
};
struct NonmemParameters
{
  /**
   * all data
   */
  std::vector<DataRecords> data;

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
/*
class NonmemSpkMLToCpp : public SpkMLToCpp
{
 public:
  NonmemSpkMLToCpp( xercesc::DOMDocument * doc );
  ~NonmemSpkMLToCpp();

  virtual const struct FitParameters * getSpkParameters() const;
  virtual const void* getClientParameters() const;
  enum client::type getClient() const;
  
 protected:

  virtual void assemble( xercesc::DOMDocument* tree );
  virtual void emit( xercesc::DOMDocument * tree );

  NonmemSpkMLToCpp();
  NonmemSpkMLToCpp( const NonmemSpkMLToCpp& right );
  const NonmemSpkMLToCpp& operator=( const NonmemSpkMLToCpp& right );

 private:

  enum CannedModel { NONE, 
		   ADVAN1, ADVAN2, ADVAN3, ADVAN4, ADVAN5, 
		   ADVAN6, ADVAN7, ADVAN8, ADVAN9, ADVAN10, 
		   ADVAN11, ADVAN12 };
  enum CannedModel canned_model;
  bool isCannedModelUsed;

  ExpTreeGenerator expTreeUtils;
  NonmemSpkMLToCpp * specific_translator;
  const xercesc::DOMDocument * tree;

  struct FitParameters spk;
  struct NonmemParameters nonmem;


  std::vector<DataRecords> data_for_all_subjects;

  void initSymbolTable( SymbolTable& );


  void interpretContent();
  void interpretDriver();
  void interpretModel();
  void interpretData();
  void emitDriver();
  void emitModel();
};
*/
class NonmemTranslator : public ClientTranslator
{
 public:
  NonmemTranslator( xercesc::DOMDocument * doc );
  ~NonmemTranslator();

  virtual const struct FitParameters * getSpkParameters() const;
  virtual const void * getClientParameters() const;
  virtual void assemble ( xercesc::DOMDocument * tree );
  virtual void emit     ( xercesc::DOMDocument * tree );
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

  struct FitParameters spk;
  struct NonmemParameters nonmem;

  std::vector<DataRecords> data_for_all_subjects;

  void initSymbolTable( SymbolTable& );

  void interpretContent();
  void interpretDriver();
  void interpretModel();
  void interpretData();
  void emitDriver();
  void emitModel();
};
#endif

