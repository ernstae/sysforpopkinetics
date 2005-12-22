/**
 * @file NM_generateOdePred.cpp
 * Define NonmemTranslator::generateOdePred().
 */
#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

using namespace std;
using namespace xercesc;
//=========================================================================================
// generateOdePred - geneate the source code for OdePred class
//
// The followings have special meanings to the ODE models (ADVAN6, 8, 9):
//
// <from data set>
//
//    DV
//    MDV
//    EVID
//    CMT
//    PCMT
//    AMT
//    RATE
//    TIME
//
// <user's responsiblity to compute>
//
//    Y
//    DADT(1) .. .DADT(n)
//
// <user's option to compute>
//
//    R1 ... Rn  // n is #of compartments + the output compartment
//    D1 ... Dn
//    ALAG1 ... ALAGn
//    S1 ... Sn
//    F1 ... Fm  // m = n-1
//    P(1) ... P(x)
//    A(1) ... A(n)
//    T
//    TSCALE
//    F
//    FO  (ef-oh)
//    F0  (ef-zero)
//    S0  (es-zero)

//
//=========================================================================================
void NonmemTranslator::generateOdePred( const char* fPkEqn_cpp, 
					const char* fDiffEqn_cpp, 
					const char* fErrorEqn_cpp ) const
{
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preliminaries
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // "labels" is an array of data labels.  The order in which the labels are stored
  // must not be disturbed.
  //
  const vector<string> * const labels = table->getLabels();
  const int nLabels = labels->size();
  vector<string>::const_iterator pLabel;

  // 
  // A SymbolTable object uses a std::map object as the actual
  // table.  The usual way of retrieving the entries in the internal
  // table is though member functions provided by SymbolTable class.
  // Another way, which is used here, is to access the internal
  // table directly, which is faster and convenient.
  //
  const map<const string, Symbol> * const t = table->getTable();
  map<const string, Symbol>::const_iterator pT;

  char ch;

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Write into OdePred.h
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // Declare and define Pred template class.
  // For name binding reason, the declaration and the definition
  // are both stored in a single file: Pred.h
  // 
  ofstream oOdePred_h( fOdePred_h );
  if( !oOdePred_h.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to create %s file.", fOdePred_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  //---------------------------------------------------------------------------------------
  //
  // Print out some description for this file.
  //
  //---------------------------------------------------------------------------------------
  oOdePred_h << "// " << myDescription << endl;

  //---------------------------------------------------------------------------------------
  // 
  // Header include statements.
  //
  //---------------------------------------------------------------------------------------
  oOdePred_h << "#ifndef ODEPRED_H" << endl;
  oOdePred_h << "#define ODEPRED_H" << endl;
  oOdePred_h << endl;

  oOdePred_h << "#include <vector>" << endl;
  oOdePred_h << "#include <string>" << endl;
  oOdePred_h << "#include <spkpred/OdePredBase.h>" << endl;
  oOdePred_h << "#include <CppAD/CppAD.h>" << endl;
  oOdePred_h << "#include <spkpred/OdeBreak.h>" << endl;
  oOdePred_h << "#include <spk/linearInterpolate.h>" << endl;
  oOdePred_h << "#include \"DataSet.h\"" << endl;
  oOdePred_h << endl;
  
  //---------------------------------------------------------------------------------------
  //
  // Declaration of OdePred class
  //
  // The template argument name (ie. "ValueType" in this case)
  // must guarantee that interfere a user's variable.
  //
  // Add a prefix, spk_ to any variable created by the system
  // to avoid conflict.
  //
  //---------------------------------------------------------------------------------------
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "class OdePred : public OdePredBase<spk_ValueType>" << endl;
  oOdePred_h << "{" << endl;
      
  //----------------------------------------
  // Public member declarations.
  //----------------------------------------
  oOdePred_h << "public:" << endl;

  // -----------
  // Constructor
  // -----------
  // This constructor takes a pointer to the DataSet (the set of
  // all individuals' data).
  //
  oOdePred_h << "   OdePred( const DataSet<spk_ValueType>* dataIn,"    << endl;
  oOdePred_h << "            int nPopSizeIn,"                          << endl;
  oOdePred_h << "            bool isPkFunctionOfTIn,"                  << endl;
  oOdePred_h << "            int nCompartmentsWithOutputIn,"           << endl;
  oOdePred_h << "            int nParametersIn,"                       << endl;
  oOdePred_h << "            int defaultDoseCompIn,"                   << endl;
  oOdePred_h << "            int defaultObservationCompIn,"            << endl;
  oOdePred_h << "            const std::valarray<bool>& initialOffIn," << endl;
  oOdePred_h << "            const std::valarray<bool>& noOffIn,"      << endl;
  oOdePred_h << "            const std::valarray<bool>& noDoseIn,"     << endl;
  oOdePred_h << "            double tolRelIn"                          << endl;
  oOdePred_h << "       );" << endl;


  // ----------
  // Destructor
  // ----------
  oOdePred_h << "   ~OdePred();" << endl;

  // -----------------------
  // Public member functions
  // -----------------------
  oOdePred_h << "   int getNObservs( int ) const;" << endl;
  oOdePred_h << "   int getNRecords( int ) const;" << endl;
  oOdePred_h << "   const spk_ValueType lininterp( const std::string & depVar );" << endl;
  oOdePred_h << "   virtual void readDataRecord( int i, int j );" << endl;

  oOdePred_h << "   virtual void initUserEnv( int spk_thetaOffset, int spk_thetaLen,"     << endl;
  oOdePred_h << "                      int spk_etaOffset,   int spk_etaLen,"              << endl;
  oOdePred_h << "                      int spk_epsOffset,   int spk_epsLen,"              << endl;
  oOdePred_h << "                      int spk_fOffset,     int spk_fLen,"                << endl;
  oOdePred_h << "                      int spk_yOffset,     int spk_yLen,"                << endl;
  oOdePred_h << "                      int spk_i,"                                        << endl;
  oOdePred_h << "                      int spk_j,"                                        << endl;
  oOdePred_h << "                      const std::vector<spk_ValueType>& spk_indepVar,"   << endl;
  oOdePred_h << "                      std::vector<spk_ValueType>& spk_depVar );"         << endl;
  oOdePred_h << endl;

  oOdePred_h << "   virtual void saveUserEnv( int spk_thetaOffset, int spk_thetaLen,"     << endl;
  oOdePred_h << "                      int spk_etaOffset,   int spk_etaLen,"              << endl;
  oOdePred_h << "                      int spk_epsOffset,   int spk_epsLen,"              << endl;
  oOdePred_h << "                      int spk_fOffset,     int spk_fLen,"                << endl;
  oOdePred_h << "                      int spk_yOffset,     int spk_yLen,"                << endl;
  oOdePred_h << "                      int spk_i,"                                        << endl;
  oOdePred_h << "                      int spk_j,"                                        << endl;
  oOdePred_h << "                      const std::vector<spk_ValueType>& spk_indepVar,"   << endl;
  oOdePred_h << "                      const std::vector<spk_ValueType>& spk_depVar );"   << endl;
  oOdePred_h << endl;

  oOdePred_h << "   virtual void evalError( " << endl;
  oOdePred_h << "                      int spk_thetaOffset, int spk_thetaLen,"            << endl;
  oOdePred_h << "                      int spk_etaOffset,   int spk_etaLen,"              << endl;
  oOdePred_h << "                      int spk_epsOffset,   int spk_epsLen,"              << endl;
  oOdePred_h << "                      int spk_i,"                                        << endl;
  oOdePred_h << "                      int spk_j,"                                        << endl;
  oOdePred_h << "                      const std::vector<spk_ValueType>& spk_indepVar);"  << endl;
  oOdePred_h << "   void evalError();" << endl;
  oOdePred_h << endl;

  oOdePred_h << "   virtual void evalPk( " << endl;
  oOdePred_h << "                      int thetaOffset, int thetaLen,"                    << endl;
  oOdePred_h << "                      int etaOffset,   int etaLen,"                      << endl;
  oOdePred_h << "                      int spk_i,"                                        << endl;
  oOdePred_h << "                      int spk_j,"                                        << endl;
  oOdePred_h << "                      const std::vector<spk_ValueType>& spk_indepVar );" << endl;
  oOdePred_h << "   void evalPk( const spk_ValueType & t );" << endl;
  oOdePred_h << endl;

  oOdePred_h << "   virtual void evalDes( " << endl;
  oOdePred_h << "                      int thetaOffset, int thetaLen,"                    << endl;
  oOdePred_h << "                      int spk_i,"                                        << endl;
  oOdePred_h << "                      int spk_j,"                                        << endl;
  oOdePred_h << "                      const std::vector<spk_ValueType>& spk_indepVar );" << endl;
  oOdePred_h << "   void evalDes(      const spk_ValueType & t," << endl;
  oOdePred_h << "                      typename std::vector<spk_ValueType>::const_iterator a );"  << endl;
  oOdePred_h << endl;

  //----------------------------------------
  // Protected member declarations.
  //----------------------------------------
  oOdePred_h << "protected:" << endl;
  oOdePred_h << "   OdePred();" << endl;
  oOdePred_h << "   OdePred( const OdePred& );" << endl;
  oOdePred_h << "   OdePred & operator=( const OdePred& );" << endl;

  //----------------------------------------
  // Private member declarations.
  //----------------------------------------
  oOdePred_h << "private:" << endl;
  oOdePred_h << "   const int                     spk_nIndividuals;"             << endl;
  oOdePred_h << "   const DataSet<spk_ValueType> *spk_perm;" << endl;
  oOdePred_h << "   DataSet<spk_ValueType>        spk_temp;"        << endl;
  oOdePred_h << "   bool                          spk_isIterCompleted;"  << endl;
  oOdePred_h << "   const int                     spk_nCompartments; /* with Output*/" << endl;
  oOdePred_h << "   const int                     spk_nParameters;"              << endl;
  oOdePred_h << "   int                           spk_curWho;" << endl;
  oOdePred_h << "   int                           spk_curWhichRecord;" << endl;
  oOdePred_h << endl;

  //
  // Declare variables.
  //
  // REVISIT Sachiko 08/05/2005
  // Make Symbol objects carry the data type: int/string/float.
  //
  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      const string varName            = pT->second.name;
      enum Symbol::Ownership  owner   = pT->second.owner;
      enum Symbol::ObjectType objType = pT->second.object_type;
      enum Symbol::Access     access  = pT->second.access;
      
      if( owner == Symbol::DATASET )
        {
	  if( varName == nonmem::ID )
	    {
	      oOdePred_h << "   std::string " << varName << ";" << endl;
	      if( pT->second.synonym != "" )
		oOdePred_h << "   std::string " << pT->second.synonym << ";" << endl;
	    }
	  else if( varName == nonmem::EVID || varName == nonmem::CMT || varName == nonmem::PCMT )
	    {
	      oOdePred_h << "   int " << varName << ";" << endl;
	      if( pT->second.synonym != "" )
		oOdePred_h << "   int " << pT->second.synonym << ";" << endl;
	    }
	  else
	    {
	      oOdePred_h << "   spk_ValueType " << varName << ";" << endl;
	      if( pT->second.synonym != "" )
		oOdePred_h << "   spk_ValueType " << pT->second.synonym << ";" << endl;
	    }
        }
      else if( owner == Symbol::SYSTEM )
        {
	  if( objType == Symbol::MATRIX )
	    {
	      continue;
	    }
	  else if( objType == Symbol::VECTOR )
	    {
	      if( varName == nonmem::DADT || varName == nonmem::P 
		  || varName == nonmem::A
		  || varName == nonmem::THETA || varName == nonmem::ETA || varName == nonmem::EPS )
		{
		  if( access == Symbol::READWRITE ) // A
		    oOdePred_h << "   typename std::vector<spk_ValueType>::iterator " << varName << ";" << endl;
		  else // DADT, P, THETA, ETA, EPS
		    oOdePred_h << "   typename std::vector<spk_ValueType>::const_iterator " << varName << ";" << endl;
		}
	      else
		continue;
	    }
	  else // Scalars
	    {
	      if( access == Symbol::READONLY )
		oOdePred_h << "   spk_ValueType " << varName << ";" << endl;
	      else if( access == Symbol::READWRITE )
		oOdePred_h << "   spk_ValueType & " << varName << ";" << endl;
	      else
		continue;
	    }
        }
      else // objType == Symbol::USER
        {
	  if( objType == Symbol::MATRIX )
	    {
	      // can't be! error!
	    }
	  else if( objType == Symbol::VECTOR )
	    {
	      if( access == Symbol::READONLY )
		oOdePred_h << "   typename std::vector<spk_ValueType>::const_iterator " << varName << ";" << endl;
	      else if( access == Symbol::READWRITE )
		oOdePred_h << "   typename std::vector<spk_ValueType>::iterator " << varName << ";" << endl;
	      else
		continue;
	    }
	  else // Scalars
	    {
	      oOdePred_h << "   spk_ValueType " << varName << ";" << endl;
	    }
        }
    }

  oOdePred_h << "};" << endl;
  oOdePred_h << endl;

  //---------------------------------------------------------------------------------------
  //
  // Definition of OdePred class
  //
  //---------------------------------------------------------------------------------------

  // -----------
  // Constructor
  // -----------
  // Constructors must call its super class's constructor.
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "OdePred<spk_ValueType>::OdePred( const DataSet<spk_ValueType>* dataIn,"    << endl;
  oOdePred_h << "                                 int nPopSizeIn,"                          << endl;
  oOdePred_h << "                                 bool isPkFunctionOfTIn,"                  << endl;
  oOdePred_h << "                                 int nCompartmentsWithOutputIn,"           << endl;
  oOdePred_h << "                                 int nParametersIn,"                       << endl;
  oOdePred_h << "                                 int defaultDoseCompIn,"                   << endl;
  oOdePred_h << "                                 int defaultObservationCompIn,"            << endl;
  oOdePred_h << "                                 const std::valarray<bool>& initialOffIn," << endl;
  oOdePred_h << "                                 const std::valarray<bool>& noOffIn,"      << endl;
  oOdePred_h << "                                 const std::valarray<bool>& noDoseIn,"     << endl;
  oOdePred_h << "                                 double tolRelIn"                          << endl;
  oOdePred_h << "                               )" << endl;
  oOdePred_h << ": " << endl;
  oOdePred_h << "  OdePredBase<spk_ValueType>( isPkFunctionOfTIn,"           << endl;
  oOdePred_h << "                              nCompartmentsWithOutputIn,"   << endl;
  oOdePred_h << "                              defaultDoseCompIn,"           << endl;
  oOdePred_h << "                              defaultObservationCompIn,"    << endl;
  oOdePred_h << "                              initialOffIn,"                << endl;
  oOdePred_h << "                              noOffIn,"                     << endl;
  oOdePred_h << "                              noDoseIn,"                    << endl; 
  oOdePred_h << "                              tolRelIn"                     << endl;
  oOdePred_h << "                            )," << endl;
  oOdePred_h << "  F                  ( getF() )," << endl;
  oOdePred_h << "  Y                  ( getY() )," << endl;
  oOdePred_h << "  F0                 ( getCompBioavailFrac(" << myCompModel->getNCompartments()-1 << ") )," << endl;
  oOdePred_h << "  FO                 ( getFO() )," << endl;
  oOdePred_h << "  S0                 ( getCompScaleParam(" << myCompModel->getNCompartments()-1 << ") )," << endl;
  oOdePred_h << "  T                  ( getT() )," << endl;
  oOdePred_h << "  TSCALE             ( getTSCALE() )," << endl;

  for( int i=0; i<myCompModel->getNCompartments(); i++ )
    {
      char Ri[64];
      char Di[64];
      char ALAGi[64];
      char Si[64];
      char Fi[64];
      snprintf( Ri,    64, "R%d", i+1 );
      snprintf( Di,    64, "D%d", i+1 );
      snprintf( ALAGi, 64, "ALAG%d", i+1 );
      snprintf( Si,    64, "S%d", i+1 );
      snprintf( Fi,    64, "F%d", i+1 );
      oOdePred_h << "  " << Ri << "                 ( getCompInfusRate("     << i << ") )," << endl;
      oOdePred_h << "  " << Di << "                 ( getCompInfusDurat("    << i << ") )," << endl;
      oOdePred_h << "  " << ALAGi << "              ( getCompAbsorpLagTime(" << i << ") )," << endl;
      oOdePred_h << "  " << Si << "                 ( getCompScaleParam("    << i << ") )," << endl;
      oOdePred_h << "  " << Fi << "                 ( getCompBioavailFrac(" << i << ") )," << endl;
    }
  oOdePred_h << "  spk_perm           ( dataIn ),"                    << endl;
  oOdePred_h << "  spk_nIndividuals   ( nPopSizeIn ),"                << endl;
  oOdePred_h << "  spk_isIterCompleted( true ),"                      << endl;
  oOdePred_h << "  spk_nCompartments  ( nCompartmentsWithOutputIn )," << endl;
  oOdePred_h << "  spk_nParameters    ( nParametersIn ),"             << endl;
  oOdePred_h << "  spk_curWho         ( 0 ),"                         << endl;
  oOdePred_h << "  spk_curWhichRecord ( 0 )"                          << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "}" << endl;
  oOdePred_h << endl;

  // ------------
  // Destructor
  // ------------
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "OdePred<spk_ValueType>::~OdePred()" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "}" << endl;
  oOdePred_h << endl;

  // -------------
  // getNObservs()
  // -------------
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "int OdePred<spk_ValueType>::getNObservs( int spk_i ) const" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "  return spk_perm->getNObservs( spk_i );" << endl;
  oOdePred_h << "}" << endl;
  oOdePred_h << endl;

  // -------------
  // getNRecords()
  // -------------
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "int OdePred<spk_ValueType>::getNRecords( int spk_i ) const" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "  return spk_perm->getNRecords( spk_i );" << endl;
  oOdePred_h << "}" << endl;
  oOdePred_h << endl;

  // -------------
  // lininterp()
  // -------------
  oOdePred_h << "// This uses TIME and DV values" << endl;
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "const spk_ValueType OdePred<spk_ValueType>::lininterp( const std::string& depVarName )" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "   const int nObservs  = getNObservs( spk_curWho );" << endl;
  oOdePred_h << "   const int nRecords  = getNRecords( spk_curWho );" << endl;
  oOdePred_h << "   std::vector<spk_ValueType> indVar( ( depVarName == \"" << nonmem::DV << "\"? nObservs:nRecords) );" << endl;
  oOdePred_h << "   std::vector<spk_ValueType> depVar( ( depVarName == \"" << nonmem::DV << "\"? nObservs:nRecords) );" << endl;
  oOdePred_h << "   std::vector<spk_ValueType> depVarRecords( nRecords );" << endl;
  pLabel = labels->begin();
  while( pLabel != labels->end() )
    {
      oOdePred_h << "   if( depVarName == \"" << *pLabel << "\" )" << endl;
      if( *pLabel == nonmem::ID )
	{
	  oOdePred_h << "      throw SpkException( SpkError::SPK_USER_INPUT_ERR, " << endl;
          oOdePred_h << "                         \"ID cannot be interpolated!\", " << endl;
          oOdePred_h << "                           __LINE__, __FILE__ );" << endl;
	}
      else if( *pLabel == nonmem::MDV )
	{
	  oOdePred_h << "      throw SpkException( SpkError::SPK_USER_INPUT_ERR, " << endl;
          oOdePred_h << "                         \"MDV cannot be interpolated!\", " << endl;
          oOdePred_h << "                           __LINE__, __FILE__ );" << endl;
	}
      else if( *pLabel == nonmem::EVID )
	{
	  oOdePred_h << "      throw SpkException( SpkError::SPK_USER_INPUT_ERR, " << endl;
          oOdePred_h << "                         \"EVID cannot be interpolated!\", " << endl;
          oOdePred_h << "                           __LINE__, __FILE__ );" << endl;
	}
      else if( *pLabel == nonmem::CMT )
	{
	  oOdePred_h << "      throw SpkException( SpkError::SPK_USER_INPUT_ERR, " << endl;
          oOdePred_h << "                         \"CMT cannot be interpolated!\", " << endl;
          oOdePred_h << "                           __LINE__, __FILE__ );" << endl;
	}
      else if( *pLabel == nonmem::PCMT )
	{
	  oOdePred_h << "      throw SpkException( SpkError::SPK_USER_INPUT_ERR, " << endl;
          oOdePred_h << "                         \"PCMT cannot be interpolated!\", " << endl;
          oOdePred_h << "                           __LINE__, __FILE__ );" << endl;
	}
      else
	{
	  oOdePred_h << "         depVarRecords = spk_perm->data[spk_curWho]->" << *pLabel << ";" << endl;
	}
      
      pLabel++;
    }
  oOdePred_h << "   if( depVarName == \"" << nonmem::DV << "\" )" << endl;
  oOdePred_h << "   {" << endl;
  oOdePred_h << "      for( int j=0, k=0; j<nRecords; j++ )" << endl;
  oOdePred_h << "      {" << endl;
  oOdePred_h << "         if( spk_perm->data[spk_curWho]->" << nonmem::MDV << "[j] == 0 )" << endl;
  oOdePred_h << "         {" << endl;
  oOdePred_h << "            indVar[k] = spk_perm->data[spk_curWho]->" << nonmem::TIME << "[j];" << endl;
  oOdePred_h << "            if( TIME < indVar[k] )" << endl;
  oOdePred_h << "            {" << endl;
  oOdePred_h << "               char m[ SpkError::maxMessageLen() ];" << endl;
  oOdePred_h << "               snprintf( m, SpkError::maxMessageLen(), " << endl;
  oOdePred_h << "                        \"The evaluation point specified for linear interpolation is less than the %d-th data point time for the %d-th individual.\", spk_curWho );" << endl;
  oOdePred_h << "            }" << endl;
  oOdePred_h << "            depVar[k] = depVarRecords[j];" << endl;
  oOdePred_h << "            k++;" << endl;
  oOdePred_h << "         }" << endl;
  oOdePred_h << "      }" << endl;
  oOdePred_h << "   }" << endl;
  oOdePred_h << "   else" << endl;
  oOdePred_h << "   {" << endl;
  oOdePred_h << "      indVar = spk_perm->data[spk_curWho]->" << nonmem::TIME << ";" << endl;
  oOdePred_h << "      depVar = depVarRecords;" << endl;
  oOdePred_h << "   }" << endl;
  oOdePred_h << "   return linearInterpolate( T, " << endl;
  oOdePred_h << "                             indVar," << endl;
  oOdePred_h << "                             depVar );" << endl;
  oOdePred_h << "}" << endl;

  // ----------------
  // readDataRecord()
  // ----------------
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "void OdePred<spk_ValueType>::readDataRecord( int spk_i, int spk_j )" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "  spk_curWho = spk_i;" << endl;
  oOdePred_h << "  spk_curWhichRecord = spk_j;" << endl;
  oOdePred_h << endl;

  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      const string varName            = pT->second.name;
      const string synonym            = pT->second.synonym;
      enum Symbol::Ownership  owner   = pT->second.owner;
      enum Symbol::ObjectType objType = pT->second.object_type;
      enum Symbol::Access     access  = pT->second.access;
      
      if( owner == Symbol::DATASET )
        {
	  oOdePred_h << "  " << varName << "= spk_perm->data[spk_i]->" << varName << "[spk_j];" << endl;
	  if( pT->second.synonym != "" )
	    {
	      oOdePred_h << "  " << pT->second.synonym << " = spk_perm->data[spk_i]->";
	      oOdePred_h << pT->second.synonym << "[spk_j];" << endl;
	    }
        }
    }
  oOdePred_h << "  setDV  ( DV );"   << endl;
  oOdePred_h << "  setTIME( TIME );" << endl;
  oOdePred_h << "  setAMT ( AMT );"  << endl;
  oOdePred_h << "  setMDV ( MDV );" << endl;
  oOdePred_h << "  setEVID( EVID );" << endl;

  if( myIsMissingCmt )
    {
      oOdePred_h << "  setCMT ( 0 ); // implying the default compartment" << endl;
    }
  else
    {
      oOdePred_h << "  setCMT ( spk_perm->data[spk_i]->CMT[spk_j] );" << endl;
    }
  if( myIsMissingPcmt )
    {
      oOdePred_h << "  setPCMT( 0 ); // implying the default compartment" << endl;
    }
  else
    {
      oOdePred_h << "  setPCMT( spk_perm->data[spk_i]->PCMT[spk_j] );" << endl;
    }
  if( myIsMissingRate )
    {
      oOdePred_h << "  setRATE( 0.0 ); // implying an instanteneous bolus dose" << endl;
    }
  else
    {
      oOdePred_h << "  setRATE( spk_perm->data[spk_i]->RATE[spk_j] );" << endl;
    }

  oOdePred_h << "}" << endl;
  oOdePred_h << endl;

  // ------------
  // initUsrEnv()
  // ------------
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "void OdePred<spk_ValueType>::initUserEnv( int spk_thetaOffset, int spk_thetaLen," << endl;
  oOdePred_h << "                              int spk_etaOffset,   int spk_etaLen,"               << endl;
  oOdePred_h << "                              int spk_epsOffset,   int spk_epsLen,"               << endl;
  oOdePred_h << "                              int spk_fOffset,     int spk_fLen,"                 << endl;
  oOdePred_h << "                              int spk_yOffset,     int spk_yLen,"                 << endl;
  oOdePred_h << "                              int spk_i,"                                         << endl;
  oOdePred_h << "                              int spk_j,"                                         << endl;
  oOdePred_h << "                              const std::vector<spk_ValueType>& spk_indepVar,"    << endl;
  oOdePred_h << "                              std::vector<spk_ValueType>& spk_depVar )"           << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "   spk_curWho = spk_i;" << endl;
  oOdePred_h << "   spk_curWhichRecord = spk_j;" << endl;
  oOdePred_h << "   assert( spk_thetaLen == " << myThetaLen << " );" << endl;
  oOdePred_h << "   assert( spk_etaLen   == " << myEtaLen << " );"   << endl;
  oOdePred_h << "   assert( spk_epsLen   == " << myEpsLen << " );"   << endl;
  oOdePred_h << endl;

  oOdePred_h << "   // Get non-const references" << endl;
  oOdePred_h << "   F      = getF();" << endl;
  oOdePred_h << "   Y      = getY();" << endl;
  oOdePred_h << "   F0     = getCompBioavailFrac(" << myCompModel->getNCompartments()-1 << ");" << endl;
  oOdePred_h << "   FO     = getFO();" << endl;
  oOdePred_h << "   S0     = getCompScaleParam  (" << myCompModel->getNCompartments()-1 << ");" << endl;
  oOdePred_h << "   T      = getT();" << endl;
  oOdePred_h << "   TSCALE = getTSCALE();" << endl;

  for( int i=0; i<myCompModel->getNCompartments(); i++ )
    {
      char Ri[64];
      char Di[64];
      char ALAGi[64];
      char Si[64];
      char Fi[64];
      snprintf( Ri,    64, "R%d", i+1 );
      snprintf( Di,    64, "D%d", i+1 );
      snprintf( ALAGi, 64, "ALAG%d", i+1 );
      snprintf( Si,    64, "S%d", i+1 );
      snprintf( Fi,    64, "F%d", i+1 );
      oOdePred_h << "   " << Ri << "     = getCompInfusRate    (" << i << ");" << endl;
      oOdePred_h << "   " << Di << "     = getCompInfusDurat   (" << i << ");" << endl;
      oOdePred_h << "   " << ALAGi << "  = getCompAbsorpLagTime(" << i << ");" << endl;
      oOdePred_h << "   " << Si << "     = getCompScaleParam   (" << i << ");" << endl;
      oOdePred_h << "   " << Fi << "     = getCompBioavailFrac (" << i << ");" << endl;
    }
  oOdePred_h << endl;
  oOdePred_h << "   // Get const iterators" << endl;
  oOdePred_h << "   A     = getCompAmountIterator();" << endl;
  oOdePred_h << endl;
  oOdePred_h << "   // Get non-const iterators" << endl;
  oOdePred_h << "   DADT  = getCompAmount_tIterator();" << endl;
  oOdePred_h << endl;
  oOdePred_h << "   // Get a constant value" << endl;
  oOdePred_h << "   T     = getT();" << endl;
  oOdePred_h << endl;

  //
  // THETA: This value is given by the system through the eval() interface.
  //
  //  oOdePred_h << "   typename std::vector<spk_ValueType>::const_iterator " << nonmem::THETA;
  oOdePred_h << "   " << nonmem::THETA << " = spk_indepVar.begin() + spk_thetaOffset;" << endl;
  //
  // NONMEM makes aliases to each element of the vector.
  // THETA(1) can be referred to as THETA1, THETA(2) as THETA2 and so on.
  // 
  for( int i=0; i<myThetaLen; i++ )
    {
      oOdePred_h << "   typename std::vector<spk_ValueType>::const_iterator " << nonmem::THETA << i+1 << endl;
      oOdePred_h << "   = spk_indepVar.begin() + spk_thetaOffset + " << i << ";" << endl;
    }

  //
  // ETA: This value is given by the system through the eval() interface.
  // 
  //oOdePred_h << "   typename std::vector<spk_ValueType>::const_iterator " << nonmem::ETA;
  oOdePred_h << "   " << nonmem::ETA << " = spk_indepVar.begin() + spk_etaOffset;" << endl;
  //
  // NONMEM makes aliases to each element of the vector.
  // ETA(1) can be referred to as ETA1, ETA(2) as ETA2 and so on.
  // 
  for( int i=0; i<myEtaLen; i++ )
    {
      oOdePred_h << "   typename std::vector<spk_ValueType>::const_iterator " << nonmem::ETA << i+1 << endl;
      oOdePred_h << "   = spk_indepVar.begin() + spk_etaOffset + " << i << ";" << endl;
    }

  if( getTarget() == POP )
    {
      //
      // EPS: This value is given by the system through the eval() interface.
      // 
      //oOdePred_h << "   typename std::vector<spk_ValueType>::const_iterator " << nonmem::EPS;
      oOdePred_h << "   " << nonmem::EPS << " = spk_indepVar.begin() + spk_epsOffset;" << endl;
      //
      // NONMEM makes aliases to each element of the vector.
      // EPS(1) can be referred to as EPS1, EPS(2) as EPS2 and so on.
      // 
      for( int i=0; i<myEpsLen; i++ )
	{
	  oOdePred_h << "   typename std::vector<spk_ValueType>::const_iterator " << nonmem::EPS << i+1 << endl;
	  oOdePred_h << " = spk_indepVar.begin() + spk_epsOffset + " << i << ";" << endl; 
	}
    }
  oOdePred_h << endl;
  oOdePred_h << "}" << endl;
  oOdePred_h << endl;

  // ------------
  // saveUsrEnv()
  // ------------
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "void OdePred<spk_ValueType>::saveUserEnv( int spk_thetaOffset, int spk_thetaLen," << endl;
  oOdePred_h << "                              int spk_etaOffset,   int spk_etaLen,"               << endl;
  oOdePred_h << "                              int spk_epsOffset,   int spk_epsLen,"               << endl;
  oOdePred_h << "                              int spk_fOffset,     int spk_fLen,"                 << endl;
  oOdePred_h << "                              int spk_yOffset,     int spk_yLen,"                 << endl;
  oOdePred_h << "                              int spk_i,"                                         << endl;
  oOdePred_h << "                              int spk_j,"                                         << endl;
  oOdePred_h << "                              const std::vector<spk_ValueType>& spk_indepVar,"    << endl;
  oOdePred_h << "                              const std::vector<spk_ValueType>& spk_depVar )"     << endl;
  oOdePred_h << "{" << endl;

  oOdePred_h << "   assert( spk_curWho == spk_i );" << endl;
  oOdePred_h << "   assert( spk_curWhichRecord == spk_j );" << endl;

  // Store the current values in temporary storage and also copy into the supper classes.
  oOdePred_h << "   " << nonmem::PRED << " = " << nonmem::F << ";" << endl;
  oOdePred_h << "   " << nonmem::RES  << " = (" << nonmem::MDV << "==0?" << nonmem::DV << "-" << nonmem::PRED << " : 0 );" << endl;

  // Keep computed values in a temporary location.
  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      // THETA, ETA, EPS are given OdePred::eval() as vectors by the caller.
      // So, we have to treat these guys a bit different from the user variables
      // which are scalar values.
      const string label                 = pT->second.name;
      enum Symbol::Ownership owner       = pT->second.owner;
      enum Symbol::ObjectType objectType = pT->second.object_type;
      enum Symbol::Access access         = pT->second.access;

      // Data items are constant, so we don't need to keep them.
      if( owner == Symbol::DATASET )
	continue;

      else if( owner == Symbol::SYSTEM )
        {
          if( objectType == Symbol::VECTOR )
            {
              if( label == nonmem::THETA )
	        {
	          oOdePred_h << "   copy( " << label << ", " << label << "+spk_thetaLen, ";
	          oOdePred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
                }
              else if( label == nonmem::ETA )
	        {
	          oOdePred_h << "   copy( " << label << ", " << label << "+spk_etaLen, ";
	          oOdePred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	        }
              else if( label == nonmem::EPS )
	        {
	          oOdePred_h << "   copy( " << label << ", " << label << "+spk_epsLen, ";
	          oOdePred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	        }
              else if( label == nonmem::P )
	        {
	          oOdePred_h << "   copy( " << label << ", " << label << "+spk_nParameters, ";
	          oOdePred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
  	        }
              else if( label == nonmem::A || label == nonmem::DADT )
	        {
	          oOdePred_h << "   copy( " << label << ", " << label << "+spk_nCompartments, ";
	          oOdePred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	        }
	      else
		{}
	    }
          else if( objectType == Symbol::MATRIX )
            {
              continue;
	    }
          else // Scalar
	    {
	      if( label == nonmem::PRED || label == nonmem::RES || access == Symbol::READWRITE )
		{
		  oOdePred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ]";
		  oOdePred_h << " = " << label << ";" << endl;
		}
	    }
	}
      else // User
	{
	  if( objectType == Symbol::SCALAR && access == Symbol::READWRITE )
	    {  
	      oOdePred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ]";
	      oOdePred_h << " = " << label << ";" << endl;
	    }
	}
    }   
  oOdePred_h << endl;

  // Saving/moving computed values to ensure a complete set of values
  // is available even when a failure occurs.
  //
  oOdePred_h << "   if( spk_i == " << getPopSize() << "-1 && spk_j == spk_perm->data[spk_i]->";
  oOdePred_h << nonmem::ID << ".size()-1 )" << endl;
  oOdePred_h << "   {" << endl;
  oOdePred_h << "     // This means, SPK advanced in iteration." << endl;
  oOdePred_h << "     // Move spk_temporary storage to spk_permanent storage." << endl;
  oOdePred_h << "     spk_isIterCompleted = true;" << endl;
  oOdePred_h << "     for( int i=0; i < spk_nIndividuals; i++ )" << endl;
  oOdePred_h << "     {" << endl;
  // User defined variables spk_temp(current) => spk_permanent
  // The user defined scalar variables
  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      const string label                 = pT->second.name;
      enum Symbol::Ownership  owner      = pT->second.owner;
      enum Symbol::ObjectType objectType = pT->second.object_type;
      enum Symbol::Access access         = pT->second.access;

      if( owner == Symbol::DATASET )
        continue;
      else if( owner == Symbol::SYSTEM )
        {
	  if( objectType == Symbol::MATRIX )
	    continue;
	  else if( objectType == Symbol::VECTOR )
	    {
	      if( label == nonmem::THETA 
		  || label == nonmem::ETA 
		  || label == nonmem::EPS 
		  || label == nonmem::DADT 
		  || label == nonmem::P 
		  || label == nonmem::A )
		{
		  oOdePred_h << "       spk_perm->data[ i ]->" << label;
		  oOdePred_h << " = spk_temp.data[ i ]->";
		  oOdePred_h << label << ";" << endl;
		}
	    }
          else if( objectType == Symbol::MATRIX )
            {
              continue;
	    }
          else // Scalar
	    {
	      if( label == nonmem::PRED || label == nonmem::RES || access == Symbol::READWRITE )
		{
		  oOdePred_h << "         spk_perm->data[ i ]->" << label;
		  oOdePred_h << " = spk_temp.data[ i ]->" << label << ";" << endl;
		}
	    }
        }
      else // User
        { 
	  if( objectType == Symbol::SCALAR && access == Symbol::READWRITE )
	    {
	      oOdePred_h << "       spk_perm->data[ i ]->" << label;
	      oOdePred_h << " = spk_temp.data[ i ]->";
	      oOdePred_h << label << ";" << endl;
	    }
	}
    }      
  oOdePred_h << "     }" << endl;
  oOdePred_h << "   }" << endl;
  oOdePred_h << "   else" << endl;
  oOdePred_h << "   {" << endl;
  oOdePred_h << "     spk_isIterCompleted = false;" << endl;
  oOdePred_h << "   }" << endl;
  oOdePred_h << endl;
  //
  ///////////////////////////////////////////////////////////////////////////////////
  
  oOdePred_h << "}" << endl;

  // --------
  // evalPk()
  // --------
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "void " << endl;
  oOdePred_h << "OdePred<spk_ValueType>::evalPk( int spk_thetaOffset, int spk_thetaLen,"           << endl;
  oOdePred_h << "                                int spk_etaOffset,   int spk_etaLen,"             << endl;
  oOdePred_h << "                                int spk_i,"                                       << endl;
  oOdePred_h << "                                int spk_j,"                                       << endl;
  oOdePred_h << "                                const std::vector<spk_ValueType>& spk_indepVar )" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "   evalPk( getT() );" << endl;
  oOdePred_h << "}" << endl;

  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "void " << endl;
  oOdePred_h << "OdePred<spk_ValueType>::evalPk( const spk_ValueType & t )" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "   T = t;" << endl;
  oOdePred_h << endl;
  oOdePred_h << "   //=========================================" << endl;
  oOdePred_h << "   // Begin User Code for $PK                 " << endl;
  oOdePred_h << "   //-----------------------------------------" << endl;
  ifstream iPkEqn( fPkEqn_cpp );
  if( !iPkEqn.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, SpkCompilerError::maxMessageLen(),
		"Failed to open %s file.", fPkEqn_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  while( iPkEqn.get(ch) )
    oOdePred_h.put(ch);
  iPkEqn.close();
  remove( fPkEqn_cpp );
  oOdePred_h << "   //-----------------------------------------" << endl;
  oOdePred_h << "   // End User Code for $PK                   " << endl;
  oOdePred_h << "   //=========================================" << endl;
      
  oOdePred_h << "}" << endl;
  oOdePred_h << endl;
  
  // ---------
  // evalDes()
  // ---------
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "void " << endl;
  oOdePred_h << "OdePred<spk_ValueType>::evalDes  ( int spk_thetaOffset, int spk_thetaLen,"           << endl;
  oOdePred_h << "                                   int spk_i, int spk_j,"                            << endl;
  oOdePred_h << "                                   const std::vector<spk_ValueType>& spk_indepVar )" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "   evalDes( getT(), getCompAmountIterator() );" << endl;
  oOdePred_h << "}" << endl;
  oOdePred_h << endl;

  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "void " << endl;
  oOdePred_h << "OdePred<spk_ValueType>::evalDes( const spk_ValueType & t," << endl;
  oOdePred_h << "                                typename std::vector<spk_ValueType>::const_iterator a )" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "   T = t;" << endl;
  oOdePred_h << "   A = a;" << endl;
  oOdePred_h << endl;
  oOdePred_h << "   //=========================================" << endl;
  oOdePred_h << "   // Begin User Code for $DES                " << endl;
  oOdePred_h << "   //-----------------------------------------" << endl;
  ifstream iDiffEqn( fDiffEqn_cpp );
  if( !iDiffEqn.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to open %s file.", fDiffEqn_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  while( iDiffEqn.get(ch) )
    oOdePred_h.put(ch);
  iDiffEqn.close();
  remove( fDiffEqn_cpp );
  oOdePred_h << "   //-----------------------------------------" << endl;
  oOdePred_h << "   // End User Code for $DES                  " << endl;
  oOdePred_h << "   //=========================================" << endl;
      
  oOdePred_h << endl;
  oOdePred_h << "}" << endl;
  oOdePred_h << endl;

  // -----------
  // evalError()
  // -----------
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "void " << endl;
  oOdePred_h << "OdePred<spk_ValueType>::evalError( int spk_thetaOffset, int spk_thetaLen,"           << endl;
  oOdePred_h << "                                   int spk_etaOffset,   int spk_etaLen,"             << endl;
  oOdePred_h << "                                   int spk_epsOffset,   int spk_epsLen,"             << endl;
  oOdePred_h << "                                   int spk_i,"                                       << endl;
  oOdePred_h << "                                   int spk_j,"                                       << endl;
  oOdePred_h << "                                   const std::vector<spk_ValueType>& spk_indepVar )" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "   assert( spk_curWho == spk_i );" << endl;
  oOdePred_h << "   assert( spk_curWhichRecord == spk_j );" << endl;
  oOdePred_h << "   assert( spk_thetaLen == " << myThetaLen << " );" << endl;
  oOdePred_h << "   assert( spk_etaLen   == " << myEtaLen << " );"   << endl;
  oOdePred_h << "   assert( spk_epsLen   == " << myEpsLen << " );"   << endl;
  oOdePred_h << endl;      
  oOdePred_h << "   return evalError();" << endl;
  oOdePred_h << "}" << endl;
  oOdePred_h << endl;
  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "void" << endl;
  oOdePred_h << "OdePred<spk_ValueType>::evalError()" << endl;
  oOdePred_h << "{" << endl;

  oOdePred_h << "   //=========================================" << endl;
  oOdePred_h << "   // Begin User Code for $ERROR              " << endl;
  oOdePred_h << "   //-----------------------------------------" << endl;
  ifstream iErrorEqn( fErrorEqn_cpp );
  if( !iErrorEqn.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to open %s file.", fErrorEqn_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  while( iErrorEqn.get(ch) )
    oOdePred_h.put(ch);
  iErrorEqn.close();
  remove( fErrorEqn_cpp );
  oOdePred_h << "   //-----------------------------------------" << endl;
  oOdePred_h << "   // End User Code for $ERROR                " << endl;
  oOdePred_h << "   //=========================================" << endl;   
  oOdePred_h << "}" << endl;

  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "OdePred<spk_ValueType>::OdePred()" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "}" << endl;

  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "OdePred<spk_ValueType>::OdePred( const OdePred<spk_ValueType>& )" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "}" << endl;

  oOdePred_h << "template <class spk_ValueType>" << endl;
  oOdePred_h << "OdePred<spk_ValueType> & OdePred<spk_ValueType>::operator=( const OdePred<spk_ValueType>& )" << endl;
  oOdePred_h << "{" << endl;
  oOdePred_h << "}" << endl;

  oOdePred_h << "#endif" << endl;
  oOdePred_h.close();
}
