/**
 * @file NM_generatePred.cpp 
 * Define NonmemTranslator::generatePred().
 */
#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

using namespace std;
using namespace xercesc;
//=========================================================================================
//
// Generate Pred.h, a file declaring and defining Pred template class.
//
// Pre-conditions  - The symbol table contains the NONMEM keywords and user defined variable
//                   names needed by the user-provided model.
//
//                 - The symbol table contains entries for the data labels and aliases.
//                   The data labels have to be retrievable by calling 
//                   SymbolTable::getlabels().  
//          
//                 - The vector returned by SymbolTable::getLables() must contain
//                   the data labels in the order in which they define the data items 
//                   (ie. columns) in the data set.
//
//                 - The current working directory is writable.
//
// Post-conditions - Pred.h is saved in the current working directory.
//
//=========================================================================================
void NonmemTranslator::generatePred( const char* fPredEqn_cpp ) const
{
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preliminaries
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Write into Pred.h
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // Declare and define Pred template class.
  // For name binding reason, the declaration and the definition
  // are both stored in a single file: Pred.h
  // 
  ofstream oPred_h( fPred_h );
  if( !oPred_h.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to create %s file.", fPred_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  //---------------------------------------------------------------------------------------
  //
  // Print out some description for this file.
  //
  //---------------------------------------------------------------------------------------
  oPred_h << "// " << myDescription << endl;

  //---------------------------------------------------------------------------------------
  // 
  // Header include statements.
  //
  //---------------------------------------------------------------------------------------
  oPred_h << "#ifndef PRED_H" << endl;
  oPred_h << "#define PRED_H" << endl;
  oPred_h << endl;

  oPred_h << "#include <vector>" << endl;
  oPred_h << "#include <string>" << endl;
  oPred_h << "#include <spkpred/PredBase.h>" << endl;
  oPred_h << "#include <CppAD/CppAD.h>" << endl;
  oPred_h << "#include \"DataSet.h\"" << endl;
  oPred_h << endl;
  
  
  //---------------------------------------------------------------------------------------
  //
  // Declaration of Pred class
  //
  // The template argument (ie. "ValueType" in this case)
  // must be something guaranteed that the user
  // do not use for one of their user-defined variables.
  //
  // The specification for SpkSourceML where the 
  // definition of PRED or other models appears
  // restricts user use of variable names beginning
  // with "spk_", let's take advantage of it here.
  //
  //---------------------------------------------------------------------------------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "class Pred : public PredBase<spk_ValueType>" << endl;
  oPred_h << "{" << endl;
      
  //----------------------------------------
  // Public member declarations.
  //----------------------------------------
  oPred_h << "public:" << endl;

  // -----------
  // Constructor
  // -----------
  // This constructor takes a pointer to the DataSet (the set of
  // all individuals' data).
  //
  oPred_h << "   Pred( const DataSet<spk_ValueType>* dataIn );" << endl;

  // ----------
  // Destructor
  // ----------
  oPred_h << "   ~Pred();" << endl;

  // -----------------------
  // Public member functions
  // -----------------------
  oPred_h << "   int getNObservs( int ) const;" << endl;
  oPred_h << "   int getNRecords( int ) const;" << endl;
  oPred_h << "   int getMeasurementIndex( int ) const;" << endl;
  oPred_h << "   int getMeasurementIndex( int, int ) const;" << endl;
  oPred_h << "   int getRecordIndex( int ) const;" << endl;
  oPred_h << "   int getRecordIndex( int, int ) const;" << endl;
  oPred_h << "   bool eval( int  spk_thetaOffset, int spk_thetaLen," << endl;
  oPred_h << "              int  spk_etaOffset,   int spk_etaLen," << endl;
  oPred_h << "              int  spk_epsOffset,   int spk_epsLen," << endl;
  oPred_h << "              int  spk_fOffset,     int spk_fLen,"   << endl;
  oPred_h << "              int  spk_yOffset,     int spk_yLen,"   << endl;
  oPred_h << "              int  spk_i," << endl;
  oPred_h << "              int  spk_j," << endl;
  oPred_h << "              int &spk_m," << endl;
  oPred_h << "              const std::vector<spk_ValueType>& spk_indepVar," << endl;
  oPred_h << "              std::vector<spk_ValueType>& spk_depVar );" << endl;
  oPred_h << endl;
  oPred_h << "   bool eval( int  spk_thetaOffset, int spk_thetaLen," << endl;
  oPred_h << "              int  spk_etaOffset,   int spk_etaLen," << endl;
  oPred_h << "              int  spk_epsOffset,   int spk_epsLen," << endl;
  oPred_h << "              int  spk_fOffset,     int spk_fLen," << endl;
  oPred_h << "              int  spk_yOffset,     int spk_yLen," << endl;
  oPred_h << "              int  spk_i," << endl;
  oPred_h << "              int  spk_j," << endl;
  oPred_h << "              const std::vector<spk_ValueType>& spk_indepVar," << endl;
  oPred_h << "              std::vector<spk_ValueType>& spk_depVar );" << endl;
  oPred_h << endl;

  //----------------------------------------
  // Protected member declarations.
  //----------------------------------------
  oPred_h << "protected:" << endl;
  oPred_h << "   Pred();" << endl;
  oPred_h << "   Pred( const Pred& );" << endl;
  oPred_h << "   Pred & operator=( const Pred& );" << endl;

  //----------------------------------------
  // Private member declarations.
  //----------------------------------------
  oPred_h << "private:" << endl;
  oPred_h << "   const int                     spk_nIndividuals;" << endl;
  oPred_h << "   const DataSet<spk_ValueType> *spk_perm;" << endl;
  oPred_h << "   DataSet<spk_ValueType>        spk_temp;" << endl;
  oPred_h << "   bool                          spk_isIterCompleted;" << endl;
  oPred_h << endl;

  // Taking care of the data items (from the data file).
  // Only the "ID" data item values are of type string,
  // otherwise all numeric, spk_ValueType.
  pLabel = labels->begin();
  for( int i=0; i<nLabels, pLabel != labels->end(); i++, pLabel++ )
    {
      bool isID = ( *pLabel == nonmem::ID );

      const Symbol* s = table->find( *pLabel );
      oPred_h << ( isID? "std::string" : "spk_ValueType" );
      oPred_h << " " << s->name << ";" << endl;
      if( !s->synonym.empty() )
	{
	  oPred_h << ( isID? "std::string" : "spk_ValueType" );
	  oPred_h << " " << s->synonym << ";" << endl;
	}
    }

  // Taking care of the user defined scalar variables.
  // The entries in the symbol table include everything,
  // the NONMEM required items such as THETA and EPS
  // and the data item labels as well as the user defined
  // scalar variable names.  The data item variables
  // are taken care in the previous step, so now
  // just pull out the user defined scalar variables.
  // The NONMEM variables are given to
  // Pred::eval() every time the iteration advances.
  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      const string    varName         = pT->second.name;
      enum Symbol::Ownership  owner   = pT->second.owner;
      enum Symbol::ObjectType objType = pT->second.object_type;

      if( objType == Symbol::VECTOR )
	{
	  if( varName == nonmem::THETA || varName == nonmem::ETA || varName == nonmem::EPS )
	    {
	      // These are independent variables for the user's model.  So, want to keep
	      // them around along with the computed values.
	      oPred_h << "typename std::vector<spk_ValueType>::const_iterator " << varName << ";" << endl;
	      continue;
	    }
	  else if( owner == Symbol::USER )
	    {
	      // There should be no vectors except the ones above.
	      char m[ SpkCompilerError::maxMessageLen() ];
	      snprintf( m, 
			SpkCompilerError::maxMessageLen(),
			"%s is a vector.  It should not appear here.", varName.c_str() );
	      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
	    }
	  else
	    {
	      // The other vectors if there's any are irrelevant in this class
	      // because they are computed outside of Pred.
	      continue;
	    }
	}
      else if( objType == Symbol::MATRIX )
	{
	  if( varName == nonmem::OMEGA || varName == nonmem::SIGMA )
	    {
	      // These are irrelevant in this class.
	      continue;
	    }
	  else
	    {
	      // There should be no matrices except the ones above.
	      char m[ SpkCompilerError::maxMessageLen() ];
	      snprintf( m, 
			SpkCompilerError::maxMessageLen(),
			"%s is a matrix.  It should not appear here.", varName.c_str() );
	      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
	    }
	}
      else // SCALAR
	{
	  oPred_h << "spk_ValueType " << varName << ";" << endl;
	}
    }

  oPred_h << "};" << endl;
  oPred_h << endl;

  //---------------------------------------------------------------------------------------
  //
  // Definition of DataSet class
  //
  //---------------------------------------------------------------------------------------

  // -----------
  // Constructor
  // -----------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "Pred<spk_ValueType>::Pred( const DataSet<spk_ValueType>* dataIn )" << endl;
  oPred_h << ": spk_perm( dataIn )," << endl;
  oPred_h << "  spk_nIndividuals( " << getPopSize() << " )," << endl;
  oPred_h << "  spk_isIterCompleted( true )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;
  oPred_h << endl;

  // ----------
  // Destructor
  // ----------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "Pred<spk_ValueType>::~Pred()" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;
  oPred_h << endl;

  // -------------
  // getNObservs()
  // -------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "int Pred<spk_ValueType>::getNObservs( int spk_i ) const" << endl;
  oPred_h << "{" << endl;
  oPred_h << "  return spk_perm->getNObservs( spk_i );" << endl;
  oPred_h << "}" << endl;
  oPred_h << endl;

  // -------------
  // getNRecords()
  // -------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "int Pred<spk_ValueType>::getNRecords( int spk_i ) const" << endl;
  oPred_h << "{" << endl;
  oPred_h << "  return spk_perm->getNRecords( spk_i );" << endl;
  oPred_h << "}" << endl;
  oPred_h << endl;

  // ----------------------
  // getMeasurementIndex(i,j)
  // Returns the index to an observation record that corresponds to the i-th individual's j-th record.
  // ----------------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "int Pred<spk_ValueType>::getMeasurementIndex( int who, int recordIndex ) const" << endl;
  oPred_h << "{" << endl;
  oPred_h << "   return spk_perm->getMeasurementIndex( who, recordIndex );" << endl;
  oPred_h << "}" << endl;

  // ----------------------
  // getMeasurementIndex(j)
  // Returns the index to an observation record that corresponds to j-th record within the whole population data set.
  // ----------------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "int Pred<spk_ValueType>::getMeasurementIndex( int recordIndex ) const" << endl;
  oPred_h << "{" << endl;
  oPred_h << "   return spk_perm->getMeasurementIndex( recordIndex );" << endl;
  oPred_h << "}" << endl;

  // -----------------
  // getRecordIndex(i,j)
  // Returns the index to a record that corresponds to the i-th individual's j-th measurement.
  // -----------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "int Pred<spk_ValueType>::getRecordIndex( int who, int measurementIndex ) const" << endl;
  oPred_h << "{" << endl;
  oPred_h << "   return spk_perm->getRecordIndex( who, measurementIndex );" << endl;
  oPred_h << "}" << endl;


  // -----------------
  // getRecordIndex(j)
  // Returns the index to a record that corresponds to the j-th measurement within the whole population data set.
  // -----------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "int Pred<spk_ValueType>::getRecordIndex( int measurementIndex ) const" << endl;
  oPred_h << "{" << endl;
  oPred_h << "   return spk_perm->getRecordIndex( measurementIndex );" << endl;
  oPred_h << "}" << endl;

  // -------------------------------------
  // eval(): no spk_m, no MDV=1 versions
  // --------------------------------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "bool Pred<spk_ValueType>::eval( int spk_thetaOffset, int spk_thetaLen," << endl;
  oPred_h << "                                int spk_etaOffset,   int spk_etaLen," << endl;
  oPred_h << "                                int spk_epsOffset,   int spk_epsLen," << endl;
  oPred_h << "                                int spk_fOffset,     int spk_fLen,"   << endl;
  oPred_h << "                                int spk_yOffset,     int spk_yLen,"   << endl;
  oPred_h << "                                int spk_i," << endl;
  oPred_h << "                                int spk_j," << endl;
  oPred_h << "                                const std::vector<spk_ValueType>& spk_indepVar," << endl;
  oPred_h << "                                std::vector<spk_ValueType>& spk_depVar )"        << endl;
  oPred_h << "{" << endl;
  oPred_h << "   int spk_m;" << endl;
  oPred_h << "   return eval( spk_thetaOffset, spk_thetaLen," << endl;
  oPred_h << "                spk_etaOffset,   spk_etaLen,"   << endl;
  oPred_h << "                spk_epsOffset,   spk_epsLen,"   << endl;
  oPred_h << "                spk_fOffset,     spk_fLen,"     << endl;
  oPred_h << "                spk_yOffset,     spk_yLen,"     << endl;
  oPred_h << "                spk_i," << endl;
  oPred_h << "                spk_j," << endl;
  oPred_h << "                spk_m," << endl;
  oPred_h << "                spk_indepVar," << endl;
  oPred_h << "                spk_depVar );" << endl;
  oPred_h << "}" << endl;
  oPred_h << endl;

  // ------------------------------------
  // eval(): with spk_m, possible MDV=1
  // ------------------------------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "bool Pred<spk_ValueType>::eval( int  spk_thetaOffset, int spk_thetaLen," << endl;
  oPred_h << "                                int  spk_etaOffset,   int spk_etaLen," << endl;
  oPred_h << "                                int  spk_epsOffset,   int spk_epsLen," << endl;
  oPred_h << "                                int  spk_fOffset,     int spk_fLen,"   << endl;
  oPred_h << "                                int  spk_yOffset,     int spk_yLen,"   << endl;
  oPred_h << "                                int  spk_i," << endl;
  oPred_h << "                                int  spk_j," << endl;
  oPred_h << "                                int &spk_m," << endl;
  oPred_h << "                                const std::vector<spk_ValueType>& spk_indepVar," << endl;
  oPred_h << "                                std::vector<spk_ValueType>& spk_depVar )" << endl;
  oPred_h << "{" << endl;

  oPred_h << "   assert( spk_thetaLen == " << myThetaLen << " );" << endl;
  oPred_h << "   assert( ( spk_etaLen == " << myEtaLen << " && spk_epsLen == " << myEpsLen   << " ) ||" << endl;
  oPred_h << "           ( spk_etaLen == " << myEpsLen << " && spk_epsLen == " << 0          << " ) );   // Allow these for two-stage methods." << endl;
  oPred_h << endl;

  ///////////////////////////////////////////////////////////////////////////////////
  // Assign the current data (i,j) to appropriate variables
  // so that the user's (originally-fortran) code can easily
  // access them.
  // ex.  cp = spk_perm->data[spk_i]->cp
  // ...given that the user's PRED code has a reference to something
  // ...like "aaa = cp * 10.0".
  //
  for( pLabel = labels->begin(); pLabel != labels->end(); pLabel++ )
    {
      const Symbol *s = table->find( *pLabel );
      // label
      oPred_h << "   " << s->name;
      oPred_h << " = spk_perm->data[spk_i]->";
      oPred_h << s->name << "[spk_j];" << endl;
      // synonym
      if( !s->synonym.empty() )
	{
	  oPred_h << "   " << s->synonym;
	  oPred_h << " = spk_perm->data[spk_i]->";
	  oPred_h << s->synonym << "[spk_j];" << endl;
	}
    }
  oPred_h << "   " << nonmem::THETA;
  oPred_h << " = spk_indepVar.begin() + spk_thetaOffset;" << endl;
  for( int i=0; i<myThetaLen; i++ )
    {
      oPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << nonmem::THETA << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_thetaOffset + " << i << ";" << endl;
    }

  oPred_h << "   " << nonmem::ETA;
  oPred_h << " = spk_indepVar.begin() + spk_etaOffset;" << endl;
  for( int i=0; i<myEtaLen; i++ )
    {
      oPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << nonmem::ETA << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_etaOffset + " << i << ";" << endl;
    }

  // EPS is only apparent in the population analysis.
  // The size of EPS vector is the order of SIGMA which is only apparent in
  // the population analysis.  So, if this is the individual level,
  // "myEpsLen" has been set to zero; thus the following loop loops zero times.
  if( getTarget() == POP )
    {
      oPred_h << "   if ( spk_epsLen != 0 )" << endl;
      oPred_h << "   {" << endl;
      oPred_h << "      // Skip this if there are no eps values." << endl;
      oPred_h << "      " << nonmem::EPS;
      oPred_h << " = spk_indepVar.begin() + spk_epsOffset;" << endl;
      for( int i=0; i<myEpsLen; i++ )
	{
	  oPred_h << "      typename std::vector<spk_ValueType>::const_iterator " << nonmem::EPS << i+1;
	  oPred_h << " = spk_indepVar.begin() + spk_epsOffset + " << i << ";" << endl;
	}
      oPred_h << "   }" << endl;
    }

  //
  // Initializing Y and F.
  //
  // REVISIT Sachiko 08/25/2005
  // Do they need to be really initialized to zero here?
  // 
  //  oPred_h << "   spk_ValueType " << nonmem::F << " = 0.0;" << endl;
  //  oPred_h << "   spk_ValueType " << nonmem::Y << " = 0.0;" << endl;
  //
  ///////////////////////////////////////////////////////////////////////////////////
      
  oPred_h << "//=========================================" << endl;
  oPred_h << "// Begin User Code                         " << endl;
  oPred_h << "//-----------------------------------------" << endl;
  char ch;
  ifstream iPredEqn( fPredEqn_cpp );
  if( !iPredEqn.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to open %s file.", fPredEqn_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  while( iPredEqn.get(ch) )
    oPred_h.put(ch);
  iPredEqn.close();
  remove( fPredEqn_cpp );
  oPred_h << "//-----------------------------------------" << endl;
  oPred_h << "// End User Code                           " << endl;
  oPred_h << "//=========================================" << endl;
      
  ///////////////////////////////////////////////////////////////////////////////////
  // Store the current values in temporary storage
  // : the user defined variable values and the NONMEM required variable values.
  oPred_h << "   " << nonmem::PRED << " = " << nonmem::F << ";" << endl;
  oPred_h << "   " << nonmem::RES  << " = (" << nonmem::MDV << "==0? " << nonmem::DV << "-" << nonmem::PRED << " : 0 );" << endl;

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
	          oPred_h << "   copy( " << label << ", " << label << "+spk_thetaLen, ";
	          oPred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
                }
              else if( label == nonmem::ETA )
	        {
	          oPred_h << "   copy( " << label << ", " << label << "+spk_etaLen, ";
	          oPred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	        }
              else if( label == nonmem::EPS )
	        {
	          oPred_h << "   if ( spk_epsLen != 0 )" << endl;
	          oPred_h << "   {" << endl;
	          oPred_h << "      // Skip this if there are no eps values." << endl;
	          oPred_h << "      copy( " << label << ", " << label << "+spk_epsLen, ";
	          oPred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	          oPred_h << "   }" << endl;
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
		  oPred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ]";
		  oPred_h << " = " << label << ";" << endl;
		}
	    }
	}
      else // User
	{
	  if( objectType == Symbol::SCALAR && access == Symbol::READWRITE )
	    {
	      oPred_h << "   spk_temp.data[ spk_i ]->" << label << "[ spk_j ]";
	      oPred_h << " = " << label << ";" << endl;
	    }	 
	}
    }   
  oPred_h << endl;

  // Saving/moving computed values to ensure a complete set of values
  // is available even when a failure occurs.
  //
  oPred_h << "   if( spk_i == " << getPopSize() << "-1 && spk_j == spk_perm->data[spk_i]->";
  oPred_h << nonmem::ID << ".size()-1 )" << endl;
  oPred_h << "   {" << endl;
  oPred_h << "     // This means, SPK advanced in iteration." << endl;
  oPred_h << "     // Move temporary storage to spk_permanent storage." << endl;
  oPred_h << "     spk_isIterCompleted = true;" << endl;
  oPred_h << "     for( int i=0; i < spk_nIndividuals; i++ )" << endl;
  oPred_h << "     {" << endl;

  // Move spk_temp(current) => spk_permanent for all user defined variables and THETA/ETA/EPS vectors.
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
		  || label == nonmem::ETA )
		{
		  oPred_h << "         spk_perm->data[ i ]->" << label;
		  oPred_h << " = spk_temp.data[ i ]->";
		  oPred_h << label << ";" << endl;
		}
	      else if( label == nonmem::EPS )
		{
	          oPred_h << "         if ( spk_epsLen != 0 )" << endl;
	          oPred_h << "         {" << endl;
	          oPred_h << "            // Skip this if there are no eps values." << endl;
		  oPred_h << "            spk_perm->data[ i ]->" << label;
		  oPred_h << " = spk_temp.data[ i ]->";
		  oPred_h << label << ";" << endl;
	          oPred_h << "         }" << endl;
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
		  oPred_h << "         spk_perm->data[ i ]->" << label;
		  oPred_h << " = spk_temp.data[ i ]->" << label << ";" << endl;
		}
	    }
        }
      else // User
        { 
	  if( objectType == Symbol::SCALAR && access == Symbol::READWRITE )
	    {
	      oPred_h << "         spk_perm->data[ i ]->" << label;
	      oPred_h << " = spk_temp.data[ i ]->";
	      oPred_h << label << ";" << endl;
	    }
        }
    }      
  oPred_h << "      }" << endl;
  oPred_h << "   }" << endl;
  oPred_h << "   else" << endl;
  oPred_h << "   {" << endl;
  oPred_h << "      spk_isIterCompleted = false;" << endl;
  oPred_h << "   }" << endl;
  oPred_h << endl;

  ///////////////////////////////////////////////////////////////////////////////////

  oPred_h << "   if( spk_perm->data[ spk_i ]->" << nonmem::MDV << "[ spk_j ] == 0 )" << endl;
  oPred_h << "   {" << endl;
  oPred_h << "      spk_m = getMeasurementIndex( spk_i, spk_j );" << endl;
  oPred_h << "      spk_depVar[ spk_fOffset+spk_m ] = " << nonmem::F << ";" << endl;
  oPred_h << "      spk_depVar[ spk_yOffset+spk_m ] = " << nonmem::Y << ";" << endl;
  oPred_h << "      return true;" << endl;
  oPred_h << "   }" << endl;
  oPred_h << "   else" << endl;
  oPred_h << "      return false;" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "Pred<spk_ValueType>::Pred()" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "Pred<spk_ValueType>::Pred( const Pred<spk_ValueType>& )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "Pred<spk_ValueType> & Pred<spk_ValueType>::operator=( const Pred<spk_ValueType>& )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "#endif" << endl;
  oPred_h.close();
}
