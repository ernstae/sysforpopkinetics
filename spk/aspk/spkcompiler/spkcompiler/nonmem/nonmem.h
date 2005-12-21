#ifndef NONMEMKEYWORD_DATASTRUCT
#define NONMEMKEYWORD_DATASTRUCT
#endif
#include <string>

/**
 * NONMEM reserved words (all in capital letters).
 *
 * These are symbols that have special meanings to NONMEM and
 * therefore to SPK.  They are all in capital letters, or
 * the system will fail to recognize.
 */
namespace nonmem{
  const std::string THETA    = "THETA";
  const std::string ETA      = "ETA";
  const std::string EPS      = "EPS";
  const std::string OMEGA    = "OMEGA";
  const std::string SIGMA    = "SIGMA";
  const std::string PRED     = "PRED";
  const std::string RES      = "RES";
  const std::string WRES     = "WRES";
  const std::string ETARES   = "ETARES";
  const std::string WETARES  = "WETARES";
  const std::string IPRED    = "IPRED";
  const std::string IRES     = "IRES";
  const std::string IWRES    = "IWRES";
  const std::string IETARES  = "IETARES";
  const std::string IWETARES = "IWETARES";
  const std::string PPRED    = "PPRED";
  const std::string PRES     = "PRES";
  const std::string PWRES    = "PWRES";
  const std::string PETARES  = "PETARES";
  const std::string PWETARES = "PWETARES";
  const std::string CPRED    = "CPRED";
  const std::string CRES     = "CRES";
  const std::string CWRES    = "CWRES";
  const std::string CETARES  = "CETARES";
  const std::string CWETARES = "CWETARES";
  const std::string DV       = "DV";
  const std::string ORGDV    = "ORGDV";
  const std::string MDV      = "MDV";
  const std::string EVID     = "EVID";
  const std::string ID       = "ID";
  const std::string F        = "F";
  const std::string Y        = "Y";
  const std::string T        = "T";
  const std::string P        = "P";
  const std::string A        = "A";
  const std::string DADT     = "DADT";
  const std::string CMT      = "CMT";
  const std::string PCMT     = "PCMT";
  const std::string AMT      = "AMT";
  const std::string R        = "R";
  const std::string D        = "D";
  const std::string S        = "S";
  const std::string ALAG     = "ALAG";
  const std::string FO       = "FO";
  const std::string F0       = "F0";
  const std::string S0       = "S0";
  const std::string RATE     = "RATE";
  const std::string TIME     = "TIME";
  const std::string TSCALE   = "TSCALE";
  const std::string DROP     = "DROP";
  const std::string SKIP     = "SKIP";
};
