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

namespace nonmem{
  enum MODEL { NONE,   
               ADVAN1,  ADVAN2,  ADVAN3,  ADVAN4,  ADVAN5, 
	       ADVAN6,  ADVAN7,  ADVAN8,  ADVAN9,  ADVAN10, 
	       ADVAN11, ADVAN12 };
  enum TRANS { DEFAULT,
	       TRANS1, TRANS2, TRANS3, TRANS4, TRANS5 };

  enum PK_PARA{Pn,
               K, KA, K12, K21, K23, K32, KM,
	       CL, Q, 
	       V, VSS, V1, V2, V3, VM,
               AOB, ALPHA, BETA, 
	       Sn, SC, S0, S1, S2, S3, S4,
               Fn, F0, FO, F1, F2, F3, 
	       Rn, R1, R2, R3,
	       Dn, D1, D2, D3,
	       ALAGn, ALAG1, ALAG2, ALAG3,
	       XSCALE };

  enum MODEL toEnumMODEL ( const char* );
  const char* const toStringMODEL( enum MODEL e );
  enum TRANS toEnumTRANS ( const char* );
  const char* const toStringTRANS( enum TRANS e );
  enum PK_PARA toEnumPK_PARA( const char* );
  const char* const toStringPK_PARA( enum PK_PARA e );

  const char * const STR_NONE    = "none";
  const char * const STR_ADVAN1  = "advan1";
  const char * const STR_ADVAN2  = "advan2";
  const char * const STR_ADVAN3  = "advan3";
  const char * const STR_ADVAN4  = "advan4";
  const char * const STR_ADVAN5  = "advan5";
  const char * const STR_ADVAN6  = "advan6";
  const char * const STR_ADVAN7  = "advan7";
  const char * const STR_ADVAN8  = "advan8";
  const char * const STR_ADVAN9  = "advan9";
  const char * const STR_ADVAN10 = "advan10";
  const char * const STR_ADVAN11 = "advan11";
  const char * const STR_ADVAN12 = "advan12";

  const char * const STR_DEFAULT = "default";
  const char * const STR_TRANS1  = "trans1";
  const char * const STR_TRANS2  = "trans2";
  const char * const STR_TRANS3  = "trans3";
  const char * const STR_TRANS4  = "trans4";
  const char * const STR_TRANS5  = "trans5";

  const char * const STR_PK_Pn   = "p";
  const char * const STR_PK_K    = "k";
  const char * const STR_PK_KA   = "ka";
  const char * const STR_PK_K12  = "k12";
  const char * const STR_PK_K21  = "k21";
  const char * const STR_PK_K23  = "k23";
  const char * const STR_PK_K32  = "k32";
  const char * const STR_PK_KM   = "km";
  const char * const STR_PK_CL   = "cl";
  const char * const STR_PK_Q    = "q";
  const char * const STR_PK_V    = "v";
  const char * const STR_PK_VSS  = "vss";
  const char * const STR_PK_V1   = "v1";
  const char * const STR_PK_V2   = "v2";
  const char * const STR_PK_V3   = "v3";
  const char * const STR_PK_VM   = "vm";
  const char * const STR_PK_AOB  = "aob";
  const char * const STR_PK_ALPHA= "alpha";
  const char * const STR_PK_BETA = "beta";
  const char * const STR_PK_SC   = "sc";
  const char * const STR_PK_Sn   = "s";
  const char * const STR_PK_S0   = "s0";
  const char * const STR_PK_S1   = "s1";
  const char * const STR_PK_S2   = "s2";
  const char * const STR_PK_S3   = "s3";
  const char * const STR_PK_S4   = "s4";
  const char * const STR_PK_Fn   = "f";
  const char * const STR_PK_F0   = "f0";  // f-zero
  const char * const STR_PK_FO   = "fo";  // f-oh
  const char * const STR_PK_F1   = "f1";
  const char * const STR_PK_F2   = "f2";
  const char * const STR_PK_F3   = "f3";
  const char * const STR_PK_Rn   = "r";
  const char * const STR_PK_R1   = "r1";
  const char * const STR_PK_R2   = "r2";
  const char * const STR_PK_R3   = "r3";
  const char * const STR_PK_Dn   = "d";
  const char * const STR_PK_D1   = "d1";
  const char * const STR_PK_D2   = "d2";
  const char * const STR_PK_D3   = "d3";
  const char * const STR_PK_ALAGn= "alag";
  const char * const STR_PK_ALAG1= "alag1";
  const char * const STR_PK_ALAG2= "alag2";
  const char * const STR_PK_ALAG3= "alag3";
  const char * const STR_PK_XSCALE="xscale";

  bool isNonmemLabel( const std::string &var );

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
  virtual const std::vector< std::string > getFilenameList() const;
  
 protected:

  NonmemTranslator( const NonmemTranslator& right );
  const NonmemTranslator& operator=( const NonmemTranslator& right );

 private:

  enum nonmem::MODEL nonmemModel;
  enum nonmem::TRANS nonmemTrans;

  //ExpTreeGenerator expTreeUtils;

  struct SpkParameters    ourSpk;
  struct NonmemParameters ourNonmem;

  std::vector<std::string> ourGeneratedFileNames;

  std::vector<std::string> emit( 		
		    int nIndividuals,
		    const SymbolTable* table,
		    const std::map<nonmem::LABEL, nonmem::ALIAS> & label_alias_mapping,
		    const std::map<nonmem::LABEL, nonmem::MEASUREMENT> data_for[],
		    const std::string order_id_pair[]
		 );
};

#endif

