#ifndef NONMEM_NAMESPACE_H
#define NONMEM_NAMESPACE_H

/**
 * @file nonmem.h
 *
 * Define "nonmem" namespace and NonmemParameters data structure.
 * Also include all headers specific to NONMEM.
 *
 * @defgroup nonmem NONMEM
 */
/**
 * @example nonmemNamespaceTest.cpp
 */
#include <spk/SpkValarray.h>

namespace nonmem{
  enum MODEL { NONE,   
               ADVAN1,  ADVAN2,  ADVAN3,  ADVAN4,  ADVAN5, 
	       ADVAN6,  ADVAN7,  ADVAN8,  ADVAN9,  ADVAN10 };
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

  enum LABEL{  ID, L1, L2, DV, MDV, TIME, 
               DATA, DAT1, DAT2, DAT3,
               DROP, SKIP, EVID, AMT,
               RATE, SS, II, ADD1, CMT, PCMT,
               CALL, CONT };

  enum MODEL toEnumMODEL ( const char* );
  const char* const toStringMODEL( enum MODEL e );
  enum TRANS toEnumTRANS ( const char* );
  const char* const toStringTRANS( enum TRANS e );
  enum PK_PARA toEnumPK_PARA( const char* );
  const char* const toStringPK_PARA( enum PK_PARA e );
  enum LABEL toEnumLABEL( const char* );
  const char* const toStringLABEL( enum LABEL l );
  
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

  const char * const STR_Pn      = "p";
  const char * const STR_K       = "k";
  const char * const STR_KA      = "ka";
  const char * const STR_K12     = "k12";
  const char * const STR_K21     = "k21";
  const char * const STR_K23     = "k23";
  const char * const STR_K32     = "k32";
  const char * const STR_KM      = "km";
  const char * const STR_CL      = "cl";
  const char * const STR_Q       = "q";
  const char * const STR_V       = "v";
  const char * const STR_VSS     = "vss";
  const char * const STR_V1      = "v1";
  const char * const STR_V2      = "v2";
  const char * const STR_V3      = "v3";
  const char * const STR_VM      = "vm";
  const char * const STR_AOB     = "aob";
  const char * const STR_ALPHA   = "alpha";
  const char * const STR_BETA    = "beta";
  const char * const STR_SC      = "sc";
  const char * const STR_Sn      = "s";
  const char * const STR_S0      = "s0";
  const char * const STR_S1      = "s1";
  const char * const STR_S2      = "s2";
  const char * const STR_S3      = "s3";
  const char * const STR_S4      = "s4";
  const char * const STR_Fn      = "f";
  const char * const STR_F0      = "f0";  // f-zero
  const char * const STR_FO      = "fo";  // f-oh
  const char * const STR_F1      = "f1";
  const char * const STR_F2      = "f2";
  const char * const STR_F3      = "f3";
  const char * const STR_Rn      = "r";
  const char * const STR_R1      = "r1";
  const char * const STR_R2      = "r2";
  const char * const STR_R3      = "r3";
  const char * const STR_Dn      = "d";
  const char * const STR_D1      = "d1";
  const char * const STR_D2      = "d2";
  const char * const STR_D3      = "d3";
  const char * const STR_ALAGn   = "alag";
  const char * const STR_ALAG1   = "alag1";
  const char * const STR_ALAG2   = "alag2";
  const char * const STR_ALAG3   = "alag3";
  const char * const STR_XSCALE  = "xscale";

  const char * const STR_ID      = "id";
  const char * const STR_L1      = "l1";
  const char * const STR_L2      = "l2";
  const char * const STR_DV      = "dv";
  const char * const STR_MDV     = "mdv";
  const char * const STR_DATA    = "data";
  const char * const STR_DAT1    = "dat1";
  const char * const STR_DAT2    = "dat2";
  const char * const STR_DAT3    = "dat3";
  const char * const STR_DROP    = "drop";
  const char * const STR_SKIP    = "skip";
  const char * const STR_EVID    = "evid";
  const char * const STR_AMT     = "amt";
  const char * const STR_RATE    = "rate";
  const char * const STR_TIME    = "time";
  const char * const STR_SS      = "ss";
  const char * const STR_II      = "ii";
  const char * const STR_ADD1    = "add1";
  const char * const STR_CMT     = "cmt";
  const char * const STR_PCMT    = "pcmt";
  const char * const STR_CALL    = "call";
  const char * const STR_CONT    = "cont"; 

  bool isReserved( const char * var );

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
  SPK_VA::valarray<bool>   thetaFixed;
  
  /**
   * A vector holding the initial estimate of @ref theta.
   * @ingroup theta
   */
  SPK_VA::valarray<double> thetaIn;

  /**
   * A vector holding the lower boundary values for @ref theta.
   * This value is ignored if @ref thetaFixed is set to true.
   * @ingroup theta
   */
  SPK_VA::valarray<double> thetaLow;

  /**
   * A vector holding the upper boundary values for @ref theta.
   * This value is ignored if @ref thetaFixed is set to true.
   * @ingroup theta
   */
  SPK_VA::valarray<double> thetaUp;

  /**
   * @defgroup Omega Omega
   * @ingroup NonmemParas
   * */
  /**
   * A flag indicating as to whether @ref Omega is fixed during
   * the optimization.
   * @ingroup Omega
   */
  SPK_VA::valarray<bool>   omegaFixed;

  /**
   * A vector holding the initial estimate of @ref Omega.
   * @ingroup Omega
   */
  SPK_VA::valarray<double> omegaIn;

  /**
   * @defgroup Sigma Sigma
   * @ingroup NonmemParas
   */
  /**
   * A flag indicating as to whether @ref Sigma is fixed during
   * the optimization.
   * @ingroup Sigma
   */
  SPK_VA::valarray<bool>   sigmaFixed;

  /**
   * A vector holding the initial estimate of @ref Sigma.
   * @ingroup Sigma
   */
  SPK_VA::valarray<double> sigmaIn;

  /**
   * @defgroup eta eta
   * @ingroup NonmemParas
   */
  /**
   * A flag indicating as to whether @ref eta is fixed during
   * the optimization.
   * @ingroup eta
   */
  SPK_VA::valarray<bool>   etaFixed;

  /**
   * A vector holding the initial estimate of @ref eta.
   * @ingroup eta
   */
  SPK_VA::valarray<double> etaIn;
};

#endif

