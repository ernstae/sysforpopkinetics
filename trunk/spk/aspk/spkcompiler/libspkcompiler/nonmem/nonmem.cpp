#include "nonmem.h"
#include <cstdlib>


////////////////////////////////////////////////////////////////////////////////////
//
// LOCAL FUNCTIONS
//
////////////////////////////////////////////////////////////////////////////////////

//=======================================================
// isNonmemLabel( const string var )
//
// Return true if "var" is one of the NONMEM reserved
// words.
//=======================================================
 bool nonmem::isReserved( const char * str )
{
  if( strcmp( str, nonmem::STR_ID   )    == 0 |
      strcmp( str, nonmem::STR_L1   )    == 0 |
      strcmp( str, nonmem::STR_L2   )    == 0 |
      strcmp( str, nonmem::STR_DV   )    == 0 |
      strcmp( str, nonmem::STR_MDV  )    == 0 |
      strcmp( str, nonmem::STR_TIME )    == 0 |
      strcmp( str, nonmem::STR_DATA )    == 0 |
      strcmp( str, nonmem::STR_DAT1 )    == 0 |
      strcmp( str, nonmem::STR_DAT2 )    == 0 |
      strcmp( str, nonmem::STR_DAT3 )    == 0 |
      strcmp( str, nonmem::STR_DROP )    == 0 |
      strcmp( str, nonmem::STR_SKIP )    == 0 |
      strcmp( str, nonmem::STR_EVID )    == 0 |
      strcmp( str, nonmem::STR_AMT  )    == 0 |
      strcmp( str, nonmem::STR_RATE )    == 0 |
      strcmp( str, nonmem::STR_SS   )    == 0 |
      strcmp( str, nonmem::STR_II   )    == 0 |
      strcmp( str, nonmem::STR_ADD1 )    == 0 |
      strcmp( str, nonmem::STR_CMT  )    == 0 |
      strcmp( str, nonmem::STR_PCMT )    == 0 |
      strcmp( str, nonmem::STR_CALL )    == 0 |
      strcmp( str, nonmem::STR_CONT )    == 0 |

      strcmp( str, nonmem::STR_ADVAN1 )  == 0 |
      strcmp( str, nonmem::STR_ADVAN2 )  == 0 |
      strcmp( str, nonmem::STR_ADVAN3 )  == 0 |
      strcmp( str, nonmem::STR_ADVAN4 )  == 0 |
      strcmp( str, nonmem::STR_ADVAN5 )  == 0 |
      strcmp( str, nonmem::STR_ADVAN6 )  == 0 |
      strcmp( str, nonmem::STR_ADVAN7 )  == 0 |
      strcmp( str, nonmem::STR_ADVAN8 )  == 0 |
      strcmp( str, nonmem::STR_ADVAN9 )  == 0 |
      strcmp( str, nonmem::STR_ADVAN10 ) == 0 |

      strcmp( str, nonmem::STR_TRANS1 )  == 0 |
      strcmp( str, nonmem::STR_TRANS2 )  == 0 |
      strcmp( str, nonmem::STR_TRANS3 )  == 0 |
      strcmp( str, nonmem::STR_TRANS4 )  == 0 |
      strcmp( str, nonmem::STR_TRANS5 )  == 0 |

      strcmp( str, nonmem::STR_Pn )      == 0 |
      strcmp( str, nonmem::STR_K )       == 0 |
      strcmp( str, nonmem::STR_KA )      == 0 |
      strcmp( str, nonmem::STR_K12 )     == 0 |
      strcmp( str, nonmem::STR_K21 )     == 0 |
      strcmp( str, nonmem::STR_K23 )     == 0 |
      strcmp( str, nonmem::STR_K32 )     == 0 |
      strcmp( str, nonmem::STR_KM )      == 0 |
      strcmp( str, nonmem::STR_CL )      == 0 |
      strcmp( str, nonmem::STR_Q )       == 0 |
      strcmp( str, nonmem::STR_V )       == 0 |
      strcmp( str, nonmem::STR_VSS )     == 0 |
      strcmp( str, nonmem::STR_V1 )      == 0 |
      strcmp( str, nonmem::STR_V2 )      == 0 |
      strcmp( str, nonmem::STR_V3 )      == 0 |
      strcmp( str, nonmem::STR_VM )      == 0 |
      strcmp( str, nonmem::STR_AOB )     == 0 |
      strcmp( str, nonmem::STR_ALPHA )   == 0 |
      strcmp( str, nonmem::STR_BETA )    == 0 |
      strcmp( str, nonmem::STR_Sn )      == 0 |
      strcmp( str, nonmem::STR_SC )      == 0 |
      strcmp( str, nonmem::STR_S0 )      == 0 |
      strcmp( str, nonmem::STR_S1 )      == 0 |
      strcmp( str, nonmem::STR_S2 )      == 0 |
      strcmp( str, nonmem::STR_S3 )      == 0 |
      strcmp( str, nonmem::STR_S4 )      == 0 |
      strcmp( str, nonmem::STR_Fn )      == 0 |
      strcmp( str, nonmem::STR_F0 )      == 0 |
      strcmp( str, nonmem::STR_FO )      == 0 |
      strcmp( str, nonmem::STR_F1 )      == 0 |
      strcmp( str, nonmem::STR_F2 )      == 0 |
      strcmp( str, nonmem::STR_F3 )      == 0 |
      strcmp( str, nonmem::STR_Rn )      == 0 |
      strcmp( str, nonmem::STR_R1 )      == 0 |
      strcmp( str, nonmem::STR_R2 )      == 0 |
      strcmp( str, nonmem::STR_R3 )      == 0 |
      strcmp( str, nonmem::STR_Dn )      == 0 |
      strcmp( str, nonmem::STR_D1 )      == 0 |
      strcmp( str, nonmem::STR_D2 )      == 0 |
      strcmp( str, nonmem::STR_D3 )      == 0 |
      strcmp( str, nonmem::STR_ALAGn )   == 0 |
      strcmp( str, nonmem::STR_ALAG1 )   == 0 |
      strcmp( str, nonmem::STR_ALAG2 )   == 0 |
      strcmp( str, nonmem::STR_ALAG3 )   == 0 |
      strcmp( str, nonmem::STR_XSCALE )  == 0 )
    {
      return true;
    }
  else
    return false;
}
enum nonmem::MODEL
nonmem::toEnumMODEL( const char* str )
{
  if( strcmp( str, nonmem::STR_NONE    ) == 0 )
    return nonmem::NONE;
  else if( strcmp( str, nonmem::STR_ADVAN1  ) == 0 )
    return nonmem::ADVAN1;
  else if( strcmp( str, nonmem::STR_ADVAN2  ) == 0 )
    return nonmem::ADVAN2;
  else if( strcmp( str, nonmem::STR_ADVAN3  ) == 0 )
    return nonmem::ADVAN3;
  else if( strcmp( str, nonmem::STR_ADVAN4  ) == 0 )
    return nonmem::ADVAN4;
  else if( strcmp( str, nonmem::STR_ADVAN5  ) == 0 )
    return nonmem::ADVAN5;
  else if( strcmp( str, nonmem::STR_ADVAN6  ) == 0 )
    return nonmem::ADVAN6;
  else if( strcmp( str, nonmem::STR_ADVAN7  ) == 0 )
    return nonmem::ADVAN7;
  else if( strcmp( str, nonmem::STR_ADVAN8  ) == 0 )
    return nonmem::ADVAN8;
  else if( strcmp( str, nonmem::STR_ADVAN9  ) == 0 )
    return nonmem::ADVAN9;
  else if( strcmp( str, nonmem::STR_ADVAN10 ) == 0 )
    return nonmem::ADVAN10;
  else
    {
      fprintf( stderr, "%s is not a valid ADVAN!!!\n", str );
      exit(-1);
    }
}
enum nonmem::TRANS
nonmem::toEnumTRANS( const char* str )
{
  if( strcmp( str, nonmem::STR_DEFAULT) == 0 )
    return nonmem::DEFAULT;
  else if( strcmp( str, nonmem::STR_TRANS1 ) == 0 )
    return nonmem::TRANS1;
  else if( strcmp( str, nonmem::STR_TRANS2 ) == 0 )
    return nonmem::TRANS2;
  else if( strcmp( str, nonmem::STR_TRANS3 ) == 0 )
    return nonmem::TRANS3;
  else if( strcmp( str, nonmem::STR_TRANS4 ) == 0 )
    return nonmem::TRANS4;
  else if( strcmp( str, nonmem::STR_TRANS5 ) == 0 )
    return nonmem::TRANS5;
  else
    {
      fprintf( stderr, "%s is not a valid TRANS!!!\n", str );
      exit(-1);
    }
}

const char* const 
nonmem::toStringMODEL( 
   enum nonmem::MODEL e )
{
  if( e == nonmem::NONE )
    return nonmem::STR_NONE;
  else if( e == nonmem::ADVAN1 )
    return nonmem::STR_ADVAN1;
  else if( e == nonmem::ADVAN2 )
    return nonmem::STR_ADVAN2;
  else if( e == nonmem::ADVAN3 )
    return nonmem::STR_ADVAN3;
  else if( e == nonmem::ADVAN4 )
    return nonmem::STR_ADVAN4;
  else if( e == nonmem::ADVAN5 )
    return nonmem::STR_ADVAN5;
  else if( e == nonmem::ADVAN6 )
    return nonmem::STR_ADVAN6;
  else if( e == nonmem::ADVAN7 )
    return nonmem::STR_ADVAN7;
  else if( e == nonmem::ADVAN8 )
    return nonmem::STR_ADVAN8;
  else if( e == nonmem::ADVAN9 )
    return nonmem::STR_ADVAN9;
  else if( e == nonmem::ADVAN10 )
    return nonmem::STR_ADVAN10;
  else
    {
      fprintf( stderr, "%d is not a valid MODEL!!!\n", static_cast<int>( e ) );
      exit(-1);
    }
}

const char* const 
nonmem::toStringTRANS( 
   enum nonmem::TRANS e )
{
  if( e == nonmem::DEFAULT )
    return nonmem::STR_DEFAULT;
  else if( e == nonmem::TRANS1 )
    return nonmem::STR_TRANS1;
  else if( e == nonmem::TRANS2 )
    return nonmem::STR_TRANS2;
  else if( e == nonmem::TRANS3 )
    return nonmem::STR_TRANS3;
  else if( e == nonmem::TRANS4 )
    return nonmem::STR_TRANS4;
  else if( e == nonmem::TRANS5 )
    return nonmem::STR_TRANS5;
  else
    {
      fprintf( stderr, "%d is not a valid TRANS!!!\n", static_cast<int>( e ) );
      exit(-1);
    }
}
enum nonmem::PK_PARA nonmem::toEnumPK_PARA( const char* s )
{
  if( strcmp( s, STR_Pn ) == 0 )
    return nonmem::Pn;
  else if( strcmp( s, STR_K ) == 0 )
    return nonmem::K;
  else if( strcmp( s, STR_KA ) == 0 )
    return nonmem::KA;
  else if( strcmp( s, STR_K12 ) == 0 )
    return nonmem::K12;
  else if( strcmp( s, STR_K21 ) == 0 )
    return nonmem::K21;
  else if( strcmp( s, STR_K23 ) == 0 )
    return nonmem::K23;
  else if( strcmp( s, STR_K32 ) == 0 )
    return nonmem::K32;
  else if( strcmp( s, STR_KM ) == 0 )
    return nonmem::KM;
  else if( strcmp( s, STR_CL ) == 0 )
    return nonmem::CL;
  else if( strcmp( s, STR_Q ) == 0 )
    return nonmem::Q;
  else if( strcmp( s, STR_V ) == 0 )
    return nonmem::V;
  else if( strcmp( s, STR_VSS ) == 0 )
    return nonmem::VSS;
  else if( strcmp( s, STR_V1 ) == 0 )
    return nonmem::V1;
  else if( strcmp( s, STR_V2 ) == 0 )
    return nonmem::V2;
  else if( strcmp( s, STR_V3 ) == 0 )
    return nonmem::V3;
  else if( strcmp( s, STR_VM ) == 0 )
    return nonmem::VM;
  else if( strcmp( s, STR_AOB ) == 0 )
    return nonmem::AOB;
  else if( strcmp( s, STR_ALPHA ) == 0 )
    return nonmem::ALPHA;
  else if( strcmp( s, STR_BETA ) == 0 )
    return nonmem::BETA;
  else if( strcmp( s, STR_Sn ) == 0 )
    return nonmem::Sn;
  else if( strcmp( s, STR_SC ) == 0 )
    return nonmem::SC;
  else if( strcmp( s, STR_S0 ) == 0 )
    return nonmem::S0;
  else if( strcmp( s, STR_S1 ) == 0 )
    return nonmem::S1;
  else if( strcmp( s, STR_S2 ) == 0 )
    return nonmem::S2;
  else if( strcmp( s, STR_S3 ) == 0 )
    return nonmem::S3;
  else if( strcmp( s, STR_S4 ) == 0 )
    return nonmem:: S4;
  else if( strcmp( s, STR_Fn ) == 0 )
    return nonmem::Fn;
  else if( strcmp( s, STR_F0 ) == 0 )
    return nonmem::F0; // f-zero
  else if( strcmp( s, STR_FO ) == 0 )
    return nonmem::FO; // f-oh
  else if( strcmp( s, STR_F1 ) == 0 )
    return nonmem::F1;
  else if( strcmp( s, STR_F2 ) == 0 )
    return nonmem::F2;
  else if( strcmp( s, STR_F3 ) == 0 )
    return nonmem::F3;
  else if( strcmp( s, STR_Rn ) == 0 )
    return nonmem::Rn;
  else if( strcmp( s, STR_R1 ) == 0 )
    return nonmem::R1;
  else if( strcmp( s, STR_R2 ) == 0 )
    return nonmem::R2;
  else if( strcmp( s, STR_R3 ) == 0 )
    return nonmem::R3;
  else if( strcmp( s, STR_Dn ) == 0 )
    return nonmem::Dn;
  else if( strcmp( s, STR_D1 ) == 0 )
    return nonmem::D1;
  else if( strcmp( s, STR_D2 ) == 0 )
    return nonmem::D2;
  else if( strcmp( s, STR_D3 ) == 0 )
    return nonmem::D3;
  else if( strcmp( s, STR_ALAGn ) == 0 )
    return nonmem::ALAGn;
  else if( strcmp( s, STR_ALAG1 ) == 0 )
    return nonmem::ALAG1;
  else if( strcmp( s, STR_ALAG2 ) == 0 )
    return nonmem::ALAG2;
  else if( strcmp( s, STR_ALAG3 ) == 0 )
    return nonmem::ALAG3;
  else if( strcmp( s, STR_XSCALE ) == 0 )
    return nonmem::XSCALE;
  else
    {
      fprintf( stderr, "%s is not a valid PK parameterization!!!\n", s );
      exit(-1);
    }
}
const char* const nonmem::toStringPK_PARA( enum nonmem::PK_PARA e )
{
  if( e == nonmem::Pn )
    return STR_Pn;
  else if( e == nonmem::K )
    return STR_K;
  else if( e == nonmem::KA )
    return STR_KA;
  else if( e == nonmem::K12 )
    return STR_K12;
  else if( e == nonmem::K21 )
    return STR_K21;
  else if( e == nonmem::K23 )
    return STR_K23;
  else if( e == nonmem::K32 )
    return STR_K32;
  else if( e == nonmem::KM )
    return STR_KM;
  else if( e == nonmem::CL )
    return STR_CL;
  else if( e == nonmem::Q )
    return STR_Q;
  else if( e == nonmem::V )
    return STR_V;
  else if( e == nonmem::VSS )
    return STR_VSS;
  else if( e == nonmem::V1 )
    return STR_V1;
  else if( e == nonmem::V2 )
    return STR_V2;
  else if( e == nonmem::V3 )
    return STR_V3;
  else if( e == nonmem::VM )
    return STR_VM;
  else if( e == nonmem::AOB )
    return STR_AOB;
  else if( e == nonmem::ALPHA )
    return STR_ALPHA;
  else if( e == nonmem::BETA )
    return STR_BETA;
  else if( e == nonmem::Sn )
    return STR_Sn;
  else if( e == nonmem::SC )
    return STR_SC;
  else if( e == nonmem::S0 )
    return STR_S0;
  else if( e == nonmem::S1 )
    return STR_S1;
  else if( e == nonmem::S2 )
    return STR_S2;
  else if( e == nonmem::S3 )
    return STR_S3;
  else if( e == nonmem::S4 )
    return STR_S4;
  else if( e == nonmem::Fn )
    return STR_Fn;
  else if( e == nonmem::F0 ) 
    return STR_F0;
  else if( e == nonmem::FO )
    return STR_FO;
  else if( e == nonmem::F1 )
    return STR_F1;
  else if( e == nonmem::F2 )
    return STR_F2;
  else if( e == nonmem:: F3 )
    return STR_F3;
  else if( e == nonmem::Rn )
    return STR_Rn;
  else if( e == nonmem::R1 )
    return STR_R1;
  else if( e == nonmem::R2 )
    return STR_R2;
  else if( e == nonmem::R3 )
    return STR_R3;
  else if( e == nonmem::Dn )
    return STR_Dn;
  else if( e == nonmem::D1 )
    return STR_D1;
  else if( e == nonmem::D2 )
    return STR_D2;
  else if( e == nonmem::D3 )
    return STR_D3;
  else if( e == nonmem::ALAGn )
    return STR_ALAGn;
  else if( e == nonmem::ALAG1 )
    return STR_ALAG2;
  else if( e == nonmem::ALAG2 )
    return STR_ALAG2;
  else if( e == nonmem::ALAG3 )
    return STR_ALAG3;
  else if( e == nonmem::XSCALE )
    return STR_XSCALE;
  else
    {
      fprintf( stderr, "%d is not a valid PK_PARA!!!\n", static_cast<int>( e ) );
      exit(-1);
    }
}

enum nonmem::LABEL nonmem::toEnumLABEL( const char * str )
{
  if( strcmp( str, nonmem::STR_ID   ) == 0 )
    return nonmem::ID;
  else if( strcmp( str, nonmem::STR_L1   ) == 0 )
    return nonmem::L1;
  else if( strcmp( str, nonmem::STR_L2   ) == 0 )
    return nonmem::L2;
  else if( strcmp( str, nonmem::STR_DV   ) == 0 )
    return nonmem::DV;
  else if( strcmp( str, nonmem::STR_MDV  ) == 0 )
    return nonmem::MDV;
  else if( strcmp( str, nonmem::STR_TIME ) == 0 )
    return nonmem::TIME;
  else if( strcmp( str, nonmem::STR_DATA ) == 0 )
    return nonmem::DATA;
  else if( strcmp( str, nonmem::STR_DAT1 ) == 0 )
    return nonmem::DAT1;
  else if( strcmp( str, nonmem::STR_DAT2 ) == 0 )
    return nonmem::DAT2;
  else if( strcmp( str, nonmem::STR_DAT3 ) == 0 )
    return nonmem::DAT3;
  else if( strcmp( str, nonmem::STR_DROP ) == 0 )
    return nonmem::DROP;
  else if( strcmp( str, nonmem::STR_SKIP ) == 0 )
    return nonmem::SKIP;
  else if( strcmp( str, nonmem::STR_EVID ) == 0 )
    return nonmem::EVID;
  else if( strcmp( str, nonmem::STR_AMT  ) == 0 )
    return nonmem::AMT;
  else if( strcmp( str, nonmem::STR_RATE ) == 0 )
    return nonmem::RATE;
  else if( strcmp( str, nonmem::STR_SS   ) == 0 )
    return nonmem::SS;
  else if( strcmp( str, nonmem::STR_II   ) == 0 )
    return nonmem::II;
  else if( strcmp( str, nonmem::STR_ADD1 ) == 0 )
    return nonmem::ADD1;
  else if( strcmp( str, nonmem::STR_CMT  ) == 0 )
    return nonmem::CMT;
  else if( strcmp( str, nonmem::STR_PCMT ) == 0 )
    return nonmem::PCMT;
  else if( strcmp( str, nonmem::STR_CALL ) == 0 )
    return nonmem::CALL;
  else if( strcmp( str, nonmem::STR_CONT ) == 0 )
    return nonmem::CONT;
  else
    {
      fprintf( stderr, "%s is not a valid nonmem reserved label!!!\n", str );
      exit(-1);
    }
}
const char * const nonmem::toStringLABEL( enum nonmem::LABEL e )
{
  if( e == nonmem::ID )
    return nonmem::STR_ID;
  else if( e == nonmem::L1 )
    return STR_L1;
  else if( e == nonmem::L2 )
    return nonmem::STR_L2;
  else if( e == nonmem::DV )
    return nonmem::STR_DV;
  else if( e == nonmem::MDV )
    return nonmem::STR_MDV;
  else if( e == nonmem::TIME )
    return nonmem::STR_TIME;
  else if( e == nonmem::DATA )
    return nonmem::STR_DATA;
  else if( e == nonmem::DAT1 )
    return nonmem::STR_DAT1;
  else if( e == nonmem::DAT2 )
    return nonmem::STR_DAT2;
  else if( e == nonmem::DAT3 )
    return nonmem::STR_DAT3;
  else if( e == nonmem::DROP )
    return nonmem::STR_DROP;
  else if( e == nonmem::SKIP )
    return nonmem::STR_SKIP;
  else if( e == nonmem::EVID )
    return nonmem::STR_EVID;
  else if( e == nonmem::AMT )
    return nonmem::STR_AMT;
  else if( e == nonmem::RATE )
    return nonmem::STR_RATE;
  else if( e == nonmem::SS )
    return nonmem::STR_SS;
  else if( e == nonmem::II )
    return nonmem::STR_II;
  else if( e == nonmem::ADD1 )
    return nonmem::STR_ADD1;
  else if( e == nonmem::CMT )
    return nonmem::STR_CMT;
  else if( e == nonmem::PCMT )
    return nonmem::STR_PCMT;
  else if( e == nonmem::CALL )
    return nonmem::STR_CALL;
  else if( e == nonmem::CONT )  
    return nonmem::STR_CONT;
  else
    {
      fprintf( stderr, "%d is not a valid LABEL!!!\n", static_cast<int>( e ) );
      exit(-1);
    }
}
