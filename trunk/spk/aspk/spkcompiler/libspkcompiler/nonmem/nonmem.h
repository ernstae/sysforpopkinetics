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
#include <spk/SpkValarray.h>

/**
 * Encapsulate Constant values and utitilies specific to NONMEM.
 */
namespace nonmem{
  
  /**
   * NONMEM ADVAN routine names or none.
   */
  enum MODEL { NONE,   /**< no ADVAN */
               ADVAN1, /**< ADVAN1  (consult "ADVAN1" in NONMEM Users Guide VIII) */
               ADVAN2, /**< ADVAN2  (consult "ADVAN2" in NONMEM Users Guide VIII) */  
               ADVAN3, /**< ADVAN3  (consult "ADVAN3" in NONMEM Users Guide VIII) */
               ADVAN4, /**< ADVAN4  (consult "ADVAN4" in NONMEM Users Guide VIII) */
               ADVAN5, /**< ADVAN5  (consult "ADVAN5" in NONMEM Users Guide VIII) */
	       ADVAN6, /**< ADVAN6  (consult "ADVAN6" in NONMEM Users Guide VIII) */
               ADVAN7, /**< ADVAN7  (consult "ADVAN7" in NONMEM Users Guide VIII) */
               ADVAN8, /**< ADVAN8  (consult "ADVAN8" in NONMEM Users Guide VIII) */
               ADVAN9, /**< ADVAN9  (consult "ADVAN9" in NONMEM Users Guide VIII) */
               ADVAN10 /**< ADVAN10 (consult "ADVAN10" in NONMEM Users Guide VIII) */
              };

  /**
   * NONMEM TRANS routine names or none.
   */
  enum TRANS { DEFAULT, /**< default or none */
	       TRANS1,  /**< TRANS1 (consult "TRANS1" in NONMEM Usres Guide VIII) */
               TRANS2,  /**< TRANS2 (consult "TRANS2" in NONMEM Usres Guide VIII) */
               TRANS3,  /**< TRANS3 (consult "TRANS3" in NONMEM Usres Guide VIII) */
               TRANS4,  /**< TRANS4 (consult "TRANS4" in NONMEM Usres Guide VIII) */
               TRANS5   /**< TRANS5 (consult "TRANS5" in NONMEM Usres Guide VIII) */
             };

  /**
   * NONMEM PK parameters associated with combinations of ADVAN and TRANS.
   * Consult ADVANs and TRANSs documentations in NONMEM Users Guide VIII
   * for details.
   * 
   */
  enum PK_PARA{Pn,
               K, 
               KA,
	       K12,
	       K21, 
	       K23, 
	       K32, 
	       KM,
	       CL, 
	       Q, 
	       V, 
	       VSS, 
	       V1, 
	       V2, 
	       V3, 
	       VM,
               AOB, 
	       ALPHA, 
	       BETA, 
	       Sn, 
	       SC, 
	       S0, 
	       S1, 
	       S2, 
	       S3, 
	       S4,
               Fn, 
	       F0, 
	       FO, 
	       F1, 
	       F2, 
	       F3, 
	       Rn, 
	       R1, 
	       R2, 
	       R3,
	       Dn, 
	       D1, 
	       D2, 
	       D3,
	       ALAGn, 
	       ALAG1, 
	       ALAG2, 
	       ALAG3,
	       XSCALE };

  /**
   * NONMEM predefined data labels.
   */
  enum LABEL{  ID,   /**< ID   (consult "ID .ID. AND L1 DATA ITEM" in NONMEM Users Guide VIII) */
	       L1,   /**< L1   (consult "ID .ID. AND L1 DATA ITEM" in NONMEM Users Guide VIII) */
	       L2,   /**< L2   (consult "L2 DATA ITEM" in NONMEM Users Guide VIII) */
	       DV,   /**< DV   (consult "DV AND MDV DATA ITEMS" in NONMEM Users Guide VIII) */
	       MDV,  /**< MDV  (consult "DV AND MDV DATA ITEMS" in NONMEM Users Guide VIII) */
	       TIME, /**< TIME (consult "TIME DATA ITEMS" in NONMEM Users Guide VIII) */
               DATE, /**< DATE (consult "DATE DATA ITEMS" in NONMEM Users Guide VIII) */
	       DAT1, /**< DATE (consult "DATE DATA ITEMS" in NONMEM Users Guide VIII) */
	       DAT2, /**< DATE (consult "DATE DATA ITEMS" in NONMEM Users Guide VIII) */
	       DAT3, /**< DATE (consult "DATE DATA ITEMS" in NONMEM Users Guide VIII) */
               DROP, /**< DROP (consult "$INPUT" in NONMEM Users Guide VIII) */
	       SKIP, /**< DROP (consult "$INPUT" in NONMEM Users Guide VIII) */
	       EVID, /**< EVID (consult "EVID DATA ITEM" in NONMEM Users Guide VIII) */
	       AMT,  /**< AMT  (consult "AMT DATA ITEM" in NONMEM Users Guide VIII) */
               RATE, /**< RATE (consult "RATE DATA ITEM" in NONMEM Users Guide VIII) */
	       SS,   /**< SS   (consult "SS DATA ITEM" in NONMEM Users Guide VIII) */
	       II,   /**< II   (consult "II DATA ITEM" in NONMEM Users Guide VIII) */
	       ADDL, /**< ADDL (consult "ADDL DATA ITEM" in NONMEM Users Guide VIII) */
	       CMT,  /**< CMT  (consult "CMT PCMT DATA ITEM" in NONMEM Users Guide VIII) */
	       PCMT, /**< PCMT (consult "CMT PCMT DATA ITEM" in NONMEM Users Guide VIII) */
               CALL, /**< CALL (consult "CALL DATA ITEM" in NONMEM Users Guide VIII) */
	       CONT  /**< CONT (consult "CONT DATA ITEM" in NONMEM Users Guide VIII) */
            };

  // EVIDs
  /**
   * Same as EVID=0 (consult "EVID DATA ITEM" in NONMEM Users Guide VIII).
   */
  const double EVID_OBSERVATION     = static_cast<double>(0);
  /**
   * Same as EVID=1 (consult "EVID DATA ITEM" in NONMEM Users Guide VIII).
   */
  const double EVID_DOSE            = static_cast<double>(1);
  /**
   * Same as EVID=2 (consult "EVID DATA ITEM" in NONMEM Users Guide VIII).
   */
  const double EVID_OTHER           = static_cast<double>(2);
  /**
   * Same as EVID=3 (consult "EVID DATA ITEM" in NONMEM Users Guide VIII).
   */
  const double EVID_RESET           = static_cast<double>(3);
  /**
   * Same as EVID=4 (consult "EVID DATA ITEM" in NONMEM Users Guide VIII).
   */
  const double EVID_RESET_THEN_DOSE = static_cast<double>(4);

  // MDVs
  /**
   * Same as MDV=0 (consult "DV AND MDV DATA ITEM" in NONMEM Users Guide VIII)
   */
  const double MDV_NOT_MISSING      = static_cast<double>(0);
  /**
   * Same as MDV=1 (consult "DV AND MDV DATA ITEM" in NONMEM Users Guide VIII)
   */
  const double MDV_MISSING          = static_cast<double>(1);

  /**
   * Returns the MODEL enumulator corresponding to the given string.
   */
  enum MODEL toEnumMODEL ( const char* s );
  /**
   * Returns the string corresponding to the MODEL enumulator.
   */
  const char* const toStringMODEL( enum MODEL e );
  /**
   * Returns the TRANS enumulator corresponding to the given string.
   */
  enum TRANS toEnumTRANS ( const char* s );
  /**
   * Returns the string corresponding to the TRANS enumulator.
   */
  const char* const toStringTRANS( enum TRANS e );
  /**
   * Returns the PK_PARA enumulator corresponding to the given string.
   */
  enum PK_PARA toEnumPK_PARA( const char* s );
  /**
   * Returns the string corresponding to the PK_PARA enumulator.
   */
  const char* const toStringPK_PARA( enum PK_PARA e );
  /**
   * Returns the data LABEL enumulator corresponding to the string.
   */
  enum LABEL toEnumLABEL( const char* s );
  /**
   * Returns the string corresponding to the data LABEL enumulator.
   */
  const char* const toStringLABEL( enum LABEL l );
  

  const char * const STR_NONE    = "none";   /**< Corresponds to ADVAN=NONE **/
  const char * const STR_ADVAN1  = "advan1"; /**< Corresponds to ADVAN=ADVAN1 **/
  const char * const STR_ADVAN2  = "advan2"; /**< Corresponds to ADVAN=ADVAN2 **/
  const char * const STR_ADVAN3  = "advan3"; /**< Corresponds to ADVAN=ADVAN3 **/
  const char * const STR_ADVAN4  = "advan4"; /**< Corresponds to ADVAN=ADVAN4 **/
  const char * const STR_ADVAN5  = "advan5"; /**< Corresponds to ADVAN=ADVAN5 **/
  const char * const STR_ADVAN6  = "advan6"; /**< Corresponds to ADVAN=ADVAN6 **/
  const char * const STR_ADVAN7  = "advan7"; /**< Corresponds to ADVAN=ADVAN7 **/
  const char * const STR_ADVAN8  = "advan8"; /**< Corresponds to ADVAN=ADVAN8 **/
  const char * const STR_ADVAN9  = "advan9"; /**< Corresponds to ADVAN=ADVAN9 **/
  const char * const STR_ADVAN10 = "advan10";/**< Corresponds to ADVAN=ADVAN10 **/
  const char * const STR_ADVAN11 = "advan11";/**< Corresponds to ADVAN=ADVAN11 **/
  const char * const STR_ADVAN12 = "advan12";/**< Corresponds to ADVAN=ADVAN12 **/

  const char * const STR_DEFAULT = "default";/**< Corresponds to TRANS=DEFAULT **/
  const char * const STR_TRANS1  = "trans1"; /**< Corresponds to TRANS=TRANS1 **/
  const char * const STR_TRANS2  = "trans2"; /**< Corresponds to TRANS=TRANS2 **/
  const char * const STR_TRANS3  = "trans3"; /**< Corresponds to TRANS=TRANS3 **/
  const char * const STR_TRANS4  = "trans4"; /**< Corresponds to TRANS=TRANS4 **/
  const char * const STR_TRANS5  = "trans5"; /**< Corresponds to TRANS=TRANS5 **/

  const char * const STR_Pn      = "p";      /**< Corresponds to PK_PARA=Pn **/
  const char * const STR_K       = "k";      /**< Corresponds to PK_PARA=K **/
  const char * const STR_KA      = "ka";     /**< Corresponds to PK_PARA=KA **/
  const char * const STR_K12     = "k12";    /**< Corresponds to PK_PARA=K12 **/
  const char * const STR_K21     = "k21";    /**< Corresponds to PK_PARA=K21 **/
  const char * const STR_K23     = "k23";    /**< Corresponds to PK_PARA=K23 **/
  const char * const STR_K32     = "k32";    /**< Corresponds to PK_PARA=K32 **/
  const char * const STR_KM      = "km";     /**< Corresponds to PK_PARA=KM **/
  const char * const STR_CL      = "cl";     /**< Corresponds to PK_PARA=CL **/
  const char * const STR_Q       = "q";      /**< Corresponds to PK_PARA=Q **/
  const char * const STR_V       = "v";      /**< Corresponds to PK_PARA=V **/
  const char * const STR_VSS     = "vss";    /**< Corresponds to PK_PARA=VSS **/
  const char * const STR_V1      = "v1";     /**< Corresponds to PK_PARA=V1 **/
  const char * const STR_V2      = "v2";     /**< Corresponds to PK_PARA=V2 **/
  const char * const STR_V3      = "v3";     /**< Corresponds to PK_PARA=V3 **/
  const char * const STR_VM      = "vm";     /**< Corresponds to PK_PARA=VM **/
  const char * const STR_AOB     = "aob";    /**< Corresponds to PK_PARA=AOB **/
  const char * const STR_ALPHA   = "alpha";  /**< Corresponds to PK_PARA=ALPHA **/
  const char * const STR_BETA    = "beta";   /**< Corresponds to PK_PARA=BETA **/
  const char * const STR_SC      = "sc";     /**< Corresponds to PK_PARA=SC **/
  const char * const STR_Sn      = "s";      /**< Corresponds to PK_PARA=S **/
  const char * const STR_S0      = "s0";     /**< Corresponds to PK_PARA=S0 **/
  const char * const STR_S1      = "s1";     /**< Corresponds to PK_PARA=S1 **/
  const char * const STR_S2      = "s2";     /**< Corresponds to PK_PARA=S2 **/
  const char * const STR_S3      = "s3";     /**< Corresponds to PK_PARA=S3 **/
  const char * const STR_S4      = "s4";     /**< Corresponds to PK_PARA=S4 **/
  const char * const STR_Fn      = "f";      /**< Corresponds to PK_PARA=F **/
  const char * const STR_F0      = "f0";     /**< Corresponds to PK_PARA=F0 (F-zero) **/
  const char * const STR_FO      = "fo";     /**< Corresponds to PK_PARA=FO (F-oh) **/
  const char * const STR_F1      = "f1";     /**< Corresponds to PK_PARA=F1 **/
  const char * const STR_F2      = "f2";     /**< Corresponds to PK_PARA=F2 **/
  const char * const STR_F3      = "f3";     /**< Corresponds to PK_PARA=F3 **/
  const char * const STR_Rn      = "r";      /**< Corresponds to PK_PARA=R **/
  const char * const STR_R1      = "r1";     /**< Corresponds to PK_PARA=R1 **/
  const char * const STR_R2      = "r2";     /**< Corresponds to PK_PARA=R2 **/
  const char * const STR_R3      = "r3";     /**< Corresponds to PK_PARA=R3 **/
  const char * const STR_Dn      = "d";      /**< Corresponds to PK_PARA=D **/
  const char * const STR_D1      = "d1";     /**< Corresponds to PK_PARA=D1 **/
  const char * const STR_D2      = "d2";     /**< Corresponds to PK_PARA=D2 **/
  const char * const STR_D3      = "d3";     /**< Corresponds to PK_PARA=D3 **/
  const char * const STR_ALAGn   = "alag";   /**< Corresponds to PK_PARA=ALAG **/
  const char * const STR_ALAG1   = "alag1";  /**< Corresponds to PK_PARA=ALAG1 **/
  const char * const STR_ALAG2   = "alag2";  /**< Corresponds to PK_PARA=ALAG2 **/
  const char * const STR_ALAG3   = "alag3";  /**< Corresponds to PK_PARA=ALAG3 **/
  const char * const STR_XSCALE  = "xscale"; /**< Corresponds to PK_PARA=XSCALE **/

  const char * const STR_ID      = "id";     /**< Corresponds to LABEL=ID **/
  const char * const STR_L1      = "l1";     /**< Corresponds to LABEL=L1 **/
  const char * const STR_L2      = "l2";     /**< Corresponds to LABEL=L2 **/
  const char * const STR_DV      = "dv";     /**< Corresponds to LABEL=DV **/
  const char * const STR_MDV     = "mdv";    /**< Corresponds to LABEL=MDV **/
  const char * const STR_DATE    = "date";   /**< Corresponds to LABEL=DATE **/
  const char * const STR_DAT1    = "dat1";   /**< Corresponds to LABEL=DAT1 **/
  const char * const STR_DAT2    = "dat2";   /**< Corresponds to LABEL=DAT2 **/
  const char * const STR_DAT3    = "dat3";   /**< Corresponds to LABEL=DAT3 **/
  const char * const STR_DROP    = "drop";   /**< Corresponds to LABEL=DROP **/
  const char * const STR_SKIP    = "skip";   /**< Corresponds to LABEL=SKIP **/
  const char * const STR_EVID    = "evid";   /**< Corresponds to LABEL=EVID **/
  const char * const STR_AMT     = "amt";    /**< Corresponds to LABEL=AMT **/
  const char * const STR_RATE    = "rate";   /**< Corresponds to LABEL=RATE **/
  const char * const STR_TIME    = "time";   /**< Corresponds to LABEL=TIME **/
  const char * const STR_SS      = "ss";     /**< Corresponds to LABEL=SS **/
  const char * const STR_II      = "ii";     /**< Corresponds to LABEL=II **/
  const char * const STR_ADDL    = "addl";   /**< Corresponds to LABEL=ADDL **/
  const char * const STR_CMT     = "cmt";    /**< Corresponds to LABEL=CMT **/
  const char * const STR_PCMT    = "pcmt";   /**< Corresponds to LABEL=PCMT **/
  const char * const STR_CALL    = "call";   /**< Corresponds to LABEL=CALL **/
  const char * const STR_CONT    = "cont";   /**< Corresponds to LABEL=CONT **/ 

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

