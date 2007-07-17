/**
 * @file nonmem.h
 * Define "nonmem" namespace. 
 */
#ifndef NONMEMKEYWORD_DATASTRUCT
#define NONMEMKEYWORD_DATASTRUCT
#endif
#include <string>

/**
 * NONMEM reserved words.
 *
 * These are symbols that have special meanings to NONMEM and
 * therefore to SPK.  They are all in capital letters, or
 * the system will fail to recognize.
 *
 * For descriptions of words, consult NONMEM VIII manual guide.
 */
namespace nonmem{
  const std::string A        = "A";            /**< A */
  const std::string ALAG     = "ALAG";         /**< ALAG */
  const std::string AMT      = "AMT";          /**< AMT */
  const std::string CETARES  = "CETARES";      /**< CETARES: Conditional ETA residual */
  const std::string CMT      = "CMT";          /**< CMT */
  const std::string CPRED    = "CPRED";        /**< CPRED: Conditional PRED */
  const std::string CRES     = "CRES";         /**< CRES: Conditional residual */
  const std::string CWRES    = "CWRES";        /**< CWRES: Conditional weighted residual */
  const std::string CWETARES = "CWETARES";     /**< CWETARES: Conditional weighted ETA residual */
  const std::string D        = "D";            /**< D */
  const std::string DADT     = "DADT";         /**< DADT */
  const std::string DROP     = "DROP";         /**< DROP */
  const std::string DV       = "DV";           /**< DV */
  const std::string EPS      = "EPS";          /**< EPS */
  const std::string ETA      = "ETA";          /**< ETA */
  const std::string ETARES   = "ETARES";       /**< ETARES: ETA residual */
  const std::string EVID     = "EVID";         /**< EVID */
  const std::string F        = "F";            /**< F */
  const std::string FO       = "FO";           /**< FO (ef oh) */
  const std::string F0       = "F0";           /**< F0 (ef zero) */
  const std::string ID       = "ID";           /**< ID */
  const std::string IETARES  = "IETARES";      /**< IETARES: Individualized ETA residual */
  const std::string IRES     = "IRES";         /**< IRES: Individualized residual  */
  const std::string IPRED    = "IPRED";        /**< IPRED: Individualized PRED */
  const std::string IWRES    = "IWRES";        /**< IWRES: Individualized weighted residual  */
  const std::string IWETARES = "IWETARES";     /**< IWETARES: Individualized weighted ETA residual */
  const std::string MDV      = "MDV";          /**< MDV */
  const std::string OMEGA    = "OMEGA";        /**< OMEGA */
  const std::string ORGDV    = "ORGDV";        /**< ORGDV: Original data set */
  const std::string P        = "P";            /**< P */
  const std::string PCMT     = "PCMT";         /**< PCMT */
  const std::string PETARES  = "PETARES";      /**< PETARES: Population ETA residual */
  const std::string PPRED    = "PPRED";        /**< PPRED: Population PRED */
  const std::string PRED     = "PRED";         /**< PRED */
  const std::string PRES     = "PRES";         /**< PRES: Population residual */
  const std::string PWRES    = "PWRES";        /**< PWRES: Population weighted residual */
  const std::string PWETARES = "PWETARES";     /**< PWETARES: Population weigthed ETA residual */
  const std::string R        = "R";            /**< R */
  const std::string RATE     = "RATE";         /**< RATE */
  const std::string RES      = "RES";          /**< RES */
  const std::string S        = "S";            /**< S */
  const std::string S0       = "S0";           /**< S0 (es zero) */
  const std::string SKIP     = "SKIP";         /**< SKIP */
  const std::string SIGMA    = "SIGMA";        /**< SIGMA */
  const std::string T        = "T";            /**< T */
  const std::string THETA    = "THETA";        /**< THETA */
  const std::string TIME     = "TIME";         /**< TIME */
  const std::string TSCALE   = "TSCALE";       /**< TSCALE */
  const std::string WETARES  = "WETARES";      /**< WETARES: Weighted ETA residual */
  const std::string WRES     = "WRES";         /**< WRES: Weighted residual */
  const std::string Y        = "Y";            /**< Y */
};
