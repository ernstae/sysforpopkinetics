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
 *
 * For developers: I wanted to declare these symbols in a namespace
 * but failed to make it work.  The reason why these symbols are not just
 * declared in the global namespace is that because some of them are
 * very generic or very short (ex. "R").
 */
struct NonmemKeyword{
  static const std::string THETA;
  static const std::string ETA;
  static const std::string EPS;
  static const std::string OMEGA;
  static const std::string SIGMA;
  static const std::string PRED;
  static const std::string RES;
  static const std::string WRES;
  static const std::string ETARES;
  static const std::string WETARES;
  static const std::string IPRED;
  static const std::string IRES;
  static const std::string IWRES;
  static const std::string IETARES;
  static const std::string IWETARES;
  static const std::string PPRED;
  static const std::string PRES;
  static const std::string PWRES;
  static const std::string PETARES;
  static const std::string PWETARES;
  static const std::string CPRED;
  static const std::string CRES;
  static const std::string CWRES;
  static const std::string CETARES;
  static const std::string CWETARES;
  static const std::string DV;
  static const std::string ORGDV;
  static const std::string MDV;
  static const std::string EVID;
  static const std::string ID;
  static const std::string F;
  static const std::string Y;

  static const std::string T;
  static const std::string P;
  static const std::string A;
  static const std::string DADT;
  static const std::string CMT;
  static const std::string PCMT;
  static const std::string AMT;
  static const std::string R;
  static const std::string D;
  static const std::string S;
  static const std::string ALAG;
  static const std::string FO;
  static const std::string F0;
  static const std::string S0;
  static const std::string RATE;
  static const std::string TIME;
  static const std::string TSCALE;
  static const std::string DROP;
  static const std::string SKIP;
  };
