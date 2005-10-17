#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"

using namespace std;
using namespace xercesc;
//=========================================================================================
// 
// Generate IndData.h, a file declaring and defining IndData template class,
// a class that represents a data set for a single individual.
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
// Post-conditions - A file, IndData.h, is saved in the current working directory.
//=========================================================================================
void NonmemTranslator::generateIndData( ) const
{
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preliminaries
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // The only the "ID" data items have type of string
  // All others have double precision type.
  // When generating C++ source code, thus, the ID
  // data items have to be recognized and treated
  // differently.  We keep a pointer to the Symbol
  // object that holds "ID" data items handy for
  // frequent references.
  //
  const Symbol * pID = table->findi( KeyStr.ID );

  //
  // DV has to be declared non-constant if simulated data replaces it.
  //
  const Symbol * pDV = table->findi( KeyStr.DV );
  const Symbol * pORGDV = table->findi( KeyStr.ORGDV );

  //
  // The order in which the label strings appear is significant.
  // So, get a constant pointer to the list so that I cannot 
  // mess it up.
  //
  const vector<string> * labels = table->getLabels();
  vector<string>::const_iterator pLabel;

  //
  // Just a sanity check: there has be one and only one "ID" entry in the label list.
  //
#ifndef NDEBUG
  int cnt=0;
  for( pLabel = labels->begin(); pLabel != labels->end(); pLabel++ )
    {
      if( *pLabel == pID->name )
	++cnt;
    }
  if( cnt != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"There may be one and only one \"%s\" label.", pID->name.c_str() );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
#endif

  // 
  // A SymbolTable object uses a std::map object as the actual
  // table.  The usual way of retrieving the entries in the internal
  // table is though member functions provided by SymbolTable class.
  // Another way, which is used here, is to access the internal
  // table directly, which is faster and convenient.
  //
  const map<const string, Symbol> * const internalTable = table->getTable();
  map<const string, Symbol>::const_iterator pInternalTable;

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Write into IndData.h
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // Declare and define IndData template class.
  // For name binding reason, the declaration and the definition
  // are both stored in a single file: IndData.h
  //
  // Variable names strictly preserve the names defined/typed by the user.
  //
  ofstream oIndData_h( fIndData_h );
  if( !oIndData_h.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to create a file, %s.", fIndData_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  
  //---------------------------------------------------------------------------------------
  //
  // Print out some description for this file.
  //
  //---------------------------------------------------------------------------------------
  oIndData_h << "/* " << myDescription << "*/" << endl;

  //---------------------------------------------------------------------------------------
  // 
  // Header include statements.
  //
  //---------------------------------------------------------------------------------------
  oIndData_h << "#ifndef INDDATA_H" << endl;
  oIndData_h << "#define INDDATA_H" << endl;
  oIndData_h << endl;

  oIndData_h << "#include <vector>" << endl;
  oIndData_h << "#include <map>" << endl;
  oIndData_h << "#include <spk/SpkValarray.h>" << endl;
  oIndData_h << "#include <spk/cholesky.h>" << endl;
  oIndData_h << "#include <spk/multiply.h>" << endl;
  oIndData_h << "#include <CppAD/CppAD.h>" << endl;
  oIndData_h << endl;
  
  //---------------------------------------------------------------------------------------
  //
  // Declaration of IndData class
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
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "class IndData{" << endl;
  
  //----------------------------------------
  // Public member declarations.
  //----------------------------------------
  oIndData_h << "public:" << endl;

  // -----------
  // Constructor
  // -----------
  // A constructor that takes the number of measurements
  // for this particular individual's data set 
  // and the data item values (from the data file).
  // The prototype varies depending on different data sets.
  // The following example shows the contructor prototype
  // for a data set which has three data items: ID, d1 and d2.
  // The data labels d1 and d2 have aliases "d1_alias" and 
  // "d2_alias", respectively.
  // 
  // IndData( int nRecordsIn,
  //          const vector<char*>         &IDIn,
  //          const vector<spk_ValueType> &d1In,  // data item 1
  //          const vector<spk_ValueType> &d2In   // data item 2 )
  // : n(nRecordsIn), d1(d1In), d1_alias(d1In), d2(d2In), d2_alias(d2In)
  // {...}
  // 
  oIndData_h << "   IndData( int nRecordsIn";
  pLabel = labels->begin();
  for( ; pLabel != labels->end(); pLabel++ )
    {
      const string keyVarName = SymbolTable::key( *pLabel );
      bool isID  = ( *pLabel == pID->name );
      bool isInt = (keyVarName==KeyStr.EVID || keyVarName == KeyStr.CMT || keyVarName == KeyStr.PCMT );
      oIndData_h << "," << endl;
	  
      //
      // If the label is of "ID", then, the data type is char*.
      // Otherwise, all others have double precision.
      //
      oIndData_h << '\t' << "   const std::vector<";
      if( isID )
	oIndData_h << "char*";
      else if( isInt )
	oIndData_h << "int";
      else
        oIndData_h << "spk_ValueType";
      oIndData_h << ">";
      oIndData_h << " & " << *pLabel << "In";
    }
  oIndData_h << " );" << endl;
  oIndData_h << endl;

  // 
  // Declare member variables whose names resemble the data labels and their
  // corresponding synonyms if they have.  They are all have double
  // precision except for the ID data item which has char* type.
  //
  for( pInternalTable=internalTable->begin(); pInternalTable != internalTable->end(); pInternalTable++ )
    {
      const string varName         = pInternalTable->second.name;
      const string varAlias        = pInternalTable->second.synonym;
      const string keyVarName      = SymbolTable::key( varName );
      const string keyVarAlias     = SymbolTable::key( varAlias );
      enum Symbol::ObjectType objectType = pInternalTable->second.object_type;
      enum Symbol::Ownership  owner      = pInternalTable->second.owner;
      
      if( keyVarName == KeyStr.EVID || keyVarName == KeyStr.CMT || keyVarName == KeyStr.PCMT )
	{
	  oIndData_h << "   std::vector<int> " << varName << ";" << endl;
	  if( varAlias != "" )
	    oIndData_h << "   std::vector<int> " << varAlias << ";" << endl;
	  continue;
	}
      else if( keyVarName == KeyStr.ID )
	{
	  oIndData_h << "   std::vector<char*> " << varName << ";" << endl;
	  if( varAlias != "" )
	    oIndData_h << "   std::vector<char*> " << varAlias << ";" << endl;
	  continue;
	}

      if( objectType == Symbol::VECTOR )
	{
	  if( owner == Symbol::DATASET )
	    {
	      oIndData_h << "   std::vector<spk_ValueType> " << varName << ";" << endl;
	      if( varAlias != "" )
		oIndData_h << "   std::vector<spk_ValueType> " << varAlias << ";" << endl;
	    }
	  else
	    {
	      oIndData_h << "   std::vector< std::vector<spk_ValueType> > " << varName << ";" << endl;
	      if( varAlias != "" )
		oIndData_h << "   std::vector< std::vector<spk_ValueType> > " << varAlias << ";" << endl;
	    }
	}
      else if( objectType == Symbol::SCALAR )
	{
	  oIndData_h << "   std::vector<spk_ValueType> " << varName << ";" << endl;
	  if( varAlias != "" )
	    oIndData_h << "   std::vector<spk_ValueType> " << varAlias << ";" << endl;
	  
	}
      else // Matrix
	{
	  // OMEGA and SIGMA are the only legal matrices.
	  if( !( keyVarName == KeyStr.OMEGA || keyVarName == KeyStr.SIGMA ) )
	    {
	      char m[ SpkCompilerError::maxMessageLen() ];
	      snprintf( m, 
			SpkCompilerError::maxMessageLen(),
			"Matrix(%s) is not allowed here!", varName.c_str() );
	      throw SpkCompilerException( SpkCompilerError::ASPK_PROGRAMMER_ERR, m, __LINE__, __FILE__ );
	    }
	}
      /*
      
       // Handling data labels.
      if( type == Symbol::DATALABEL )
	{
	  bool isID    = ( varName == pID->name?    true : false );
          bool isDV    = ( varName == pDV->name?    true : false );
	  bool isInt   = ( keyVarName == KeyStr.EVID || keyVarName == KeyStr.CMT || keyVarName == KeyStr.PCMT );

	  // If data simulation is requested, DV values are replaced by simulated
	  // measurements and the original DV values are moved/stored in ORGDV.
	  // Thus, in case of data simulation, DV (and ORGDV) has to be writable.
	  // Otherwise, DV is read-only.
	  oIndData_h << "   std::vector<";
	  if( isID )
	    {
	      oIndData_h << "char *";
	    }
	  else if( isInt )
	    {
	      oIndData_h << "int";
	    }
	  else
	    {
	      oIndData_h << "spk_ValueType";
	    }
	  oIndData_h << "> " << varName << ";" << endl;

	  // If the current symbol has an alias, declare the alias as well.
	  if( varAlias != "" )
	    {
	      oIndData_h << "   std::vector<";
	      if( isID )
		{
		  oIndData_h << "char *";
		}
	      else if( isInt )
		{
		  oIndData_h << "int";
		}
	      else
		{
		  oIndData_h << "spk_ValueType";
		}
	      oIndData_h << "> " << varAlias << ";" << endl;
	    }
	}
      // Handling NONMEM pred variables.
      //
      // The NONMEM pred variables are vectors whose elements
      // would be replaced by computed values.  So they have to be
      // declared writable.
      else if( type == Symbol::PREDEFINED )
	{
	  if( keyVarName == KeyStr.THETA 
	      || keyVarName == KeyStr.ETA 
	      || keyVarName == KeyStr.EPS 
	      || keyVarName == KeyStr.ETARES
	      || keyVarName == KeyStr.WETARES
	      || keyVarName == KeyStr.IETARES
	      || keyVarName == KeyStr.IWETARES
	      || keyVarName == KeyStr.PETARES
	      || keyVarName == KeyStr.PWETARES
	      || keyVarName == KeyStr.CETARES
	      || keyVarName == KeyStr.CWETARES
	      || keyVarName == KeyStr.DADT
	      || keyVarName == KeyStr.P
	      || keyVarName == KeyStr.A )
	    oIndData_h << "   std::vector<std::vector<spk_ValueType> > " << varName << ";" << endl;

	  // The values of Omega and Sigma matrices are
	  // rather expressed as ETA and EPS, respectively.
	  // So, these matrices don't need place-holders.
	  else if( keyVarName == KeyStr.OMEGA 
              || keyVarName == KeyStr.SIGMA )
	    {}
	  else
	    {
	      oIndData_h << "   std::vector<spk_ValueType> " << varName << ";" << endl;
	    }
	}

      // These may appear in the data set or not appear.
      else if( keyVarName == KeyStr.EVID 
	       || keyVarName == KeyStr.CMT
	       || keyVarName == KeyStr.PCMT )
	{
	  oIndData_h << "   std::vector<int> " << varName << ";" << endl;
	}

      // Handling all others (ie. the user defined variables)
      // 
      // User defined variables store values computed every time the user model
      // is evaluated.  Thus, these have to be declared writable.
      else
	{
	  oIndData_h << "   std::vector<spk_ValueType> " << varName << ";" << endl;
	}
      */
    }

  oIndData_h << endl;
  
  // ----------
  // Destructor
  // ----------
  oIndData_h << "   ~IndData();" << endl;

  //----------------------------------------
  // Public member declarations
  //----------------------------------------
  oIndData_h << "   int getNRecords() const;" << endl;
  oIndData_h << "   int getNObservs() const;" << endl;
  oIndData_h << "   const SPK_VA::valarray<double> getMeasurements() const;"          << endl;
  oIndData_h << "   int getRecordIndex( int measurementIndex ) const;"                << endl;
  oIndData_h << "   int getMeasurementIndex( int recordIndex ) const;"                << endl;
  oIndData_h << "   void replaceMeasurements( const SPK_VA::valarray<double>& yyi );" << endl;
  oIndData_h << "   void replacePred   ( const SPK_VA::valarray<double>& predIn );"   << endl;
  oIndData_h << "   void replaceRes    ( const SPK_VA::valarray<double>& ResIn );"    << endl;
  oIndData_h << "   void replaceWRes   ( const SPK_VA::valarray<double>& WresIn );"   << endl;
  oIndData_h << "   void replacePPred  ( const SPK_VA::valarray<double>& pPredIn );"  << endl;
  oIndData_h << "   void replacePRes   ( const SPK_VA::valarray<double>& pResIn );"   << endl;
  oIndData_h << "   void replacePWRes  ( const SPK_VA::valarray<double>& pWResIn );"  << endl;
  oIndData_h << "   void replaceIPred  ( const SPK_VA::valarray<double>& iPredIn );"  << endl;
  oIndData_h << "   void replaceIRes   ( const SPK_VA::valarray<double>& iResIn );"   << endl;
  oIndData_h << "   void replaceIWRes  ( const SPK_VA::valarray<double>& iWresIn );"  << endl;
  oIndData_h << "   void replaceCPred  ( const SPK_VA::valarray<double>& cPredIn );"  << endl;
  oIndData_h << "   void replaceCRes   ( const SPK_VA::valarray<double>& cResIn );"   << endl;
  oIndData_h << "   void replaceCWRes  ( const SPK_VA::valarray<double>& cWresIn );"  << endl;
  if( getTarget() == POP )
    {
      oIndData_h << "   void replaceEta     ( const SPK_VA::valarray<double>& etaIn );"      << endl;
      oIndData_h << "   void replaceEtaRes  ( const SPK_VA::valarray<double>& EtaresIn );"  << endl;
      oIndData_h << "   void replaceWEtaRes ( const SPK_VA::valarray<double>& WetaresIn );" << endl;
      oIndData_h << "   void replaceIEtaRes ( const SPK_VA::valarray<double>& iEtaresIn );"  << endl;
      oIndData_h << "   void replaceIWEtaRes( const SPK_VA::valarray<double>& iWetaresIn );" << endl;
      oIndData_h << "   void replacePEtaRes ( const SPK_VA::valarray<double>& pEtaResIn );"  << endl;
      oIndData_h << "   void replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResIn );" << endl;
      oIndData_h << "   void replaceCEtaRes ( const SPK_VA::valarray<double>& cEtaResIn );"  << endl;
      oIndData_h << "   void replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResIn );" << endl;
    }
  oIndData_h << endl;

  //----------------------------------------
  // Protected member declarations.
  //----------------------------------------
  oIndData_h << "protected:" << endl;
  oIndData_h << "   IndData();" << endl;
  oIndData_h << "   IndData( const IndData& );" << endl;
  oIndData_h << "   IndData& operator=( const IndData& );" << endl;
  oIndData_h << endl;
  oIndData_h << "   int nY; // #of measurements (DVs where MDV=0)." << endl;
  oIndData_h << "   SPK_VA::valarray<double> measurements;" << endl;

  //----------------------------------------
  // Private member declarations.
  //----------------------------------------
  oIndData_h << "private:" << endl;
  oIndData_h << "   const int nRecords; // the number of data records." << endl;
  oIndData_h << "   void assign( double&, const CppAD::AD<double>& ) const;" << endl;
  oIndData_h << "   void assign( double&, double ) const;" << endl;
  oIndData_h << "   /////////////////////////////////////////////////////////" << endl;
  oIndData_h << "   //      original                     y"                    << endl;
  oIndData_h << "   //  -------------------      -------------------"          << endl;
  oIndData_h << "   //   j    i   MDV   DV         j'  j   i   DV"             << endl;
  oIndData_h << "   //  -------------------      -------------------"          << endl;
  oIndData_h << "   //   0    0    0    0.1        0   0   0   0.1"            << endl;
  oIndData_h << "   //   1    0    1               1   2   0   0.2"            << endl;
  oIndData_h << "   //   2    0    0    0.2        2   4   0   0.3"            << endl;
  oIndData_h << "   //   3    0    1"                                          << endl;
  oIndData_h << "   //   4    0    0    0.3"                                   << endl;
  oIndData_h << "   //"                                                        << endl;
  oIndData_h << "   //"                                                        << endl;
  oIndData_h << "   //   jTojPrime            jPrimeToj"                       << endl;
  oIndData_h << "   //  -----------          -----------"                      << endl;
  oIndData_h << "   //    j    j'              j'   j"                         << endl;
  oIndData_h << "   //  -----------          -----------"                      << endl;
  oIndData_h << "   //    0    0               0    0"                         << endl;
  oIndData_h << "   //    1   -1*              1    2"                         << endl;
  oIndData_h << "   //    2    1               2    4"                         << endl;
  oIndData_h << "   //    3   -1*"                                             << endl;
  oIndData_h << "   //    4    2"                                              << endl;
  oIndData_h << "   //"                                                        << endl;
  oIndData_h << "   //  * (-1) points to no j', i.e. MDV=1"                    << endl;
  oIndData_h << "   /////////////////////////////////////////////////////////" << endl;
  oIndData_h << "   std::vector<int> jTojPrime;" << endl;
  oIndData_h << "   std::vector<int> jPrimeToj;" << endl;
  oIndData_h << "};" << endl;


  //---------------------------------------------------------------------------------------
  //
  // Definition of IndData class
  //
  //---------------------------------------------------------------------------------------

  // -----------
  // Constructor
  // -----------
  //
  // Prototype:
  // Pay extra attention to the order of arguments.
  // The declaration portiona was done using the vector of labels
  // returned by SymbolTable::getLabels().  Use the
  // same exact vector to ensure the order consistency.
  //
  string synonym;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "IndData<spk_ValueType>::IndData( int nRecordsIn";
  pLabel = labels->begin();
  for( ; pLabel != labels->end(); pLabel++ )
    {
      const string keyLabel = SymbolTable::key( *pLabel );
      bool isID  = ( *pLabel == pID->name );
      bool isInt = (keyLabel == KeyStr.EVID || keyLabel == KeyStr.CMT || keyLabel == KeyStr.PCMT );
      oIndData_h << "," << endl;

      //
      // If the label string is of "ID", then the data type is char*.
      // Othewise, double.
      //
      oIndData_h << "const std::vector<";
      if( isID )
	oIndData_h << "char*";
      else if( isInt )
	oIndData_h << "int";
      else
	oIndData_h << "spk_ValueType";
      oIndData_h << "> ";
      oIndData_h << "& " << *pLabel << "In";
    }
  oIndData_h << ")" << endl;
  oIndData_h << ": nRecords( nRecordsIn ), // # of records (including MDV=0)" << endl;
  oIndData_h << "  nY( 0 )   // # of measurements" << endl;

  //
  // Constructor initialization:
  // Assign the argument values to the internal valarray variables.
  // Also assign the same values to equivalent (synonym) variables
  // if the variable has a synonym defined.
  // NOTE: Yes, I know if the values were pointed by pointers
  // the primary variable and alias can share the same objects.
  // But, since they are of type of, essentially, double, 
  // copying would not be a big deal; as a matter of fact, 
  // it may be actually faster than accessing via pointers.
  //
  pLabel = labels->begin();
  for( ; pLabel != labels->end(); pLabel++ )
    {
      const string keyLabel = SymbolTable::key( *pLabel );
      oIndData_h << "," << endl;
      oIndData_h << *pLabel;
      oIndData_h << "( " << *pLabel << "In" << " )";

      //
      // If the label has a synonym, apply the same value to the synonym.
      //
      if( ( synonym = table->findi( keyLabel/**pLabel*/ )->synonym ) != "" )
	{
	  oIndData_h << "," << endl;
	  oIndData_h << synonym;
	  oIndData_h << "( " << *pLabel << "In" << " )";
	}
    }

  //
  // Constructor body:
  // Initialize the sizes of the user defined variables that
  // appear in the model definition.
  // We don't know the values yet, so just assign the size,
  // which is the same as the number of data records for a subject.
  //
  // These arrays will be internally (ie. PRED routine) used to store 
  // intermediate values.  The intermediate values are
  // returned to the user for tabular display or plot display.
  // They need corresponding shadow placeholders so that
  // if an iteration fails, the system can return the previously
  // successfully computed values.
  //
  pInternalTable = internalTable->begin();
  for( ; pInternalTable != internalTable->end(); pInternalTable++ )
    {
      const string label    = pInternalTable->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.OMEGA || keyLabel == KeyStr.SIGMA )
	{
	  continue;
	}

      //
      // Initialize sizes of place holders for computed values.
      //
      if( find( labels->begin(), labels->end(), pInternalTable->second.name ) 
	  == labels->end() )
	oIndData_h << "," << endl << label << "( nRecordsIn )";
    }
  oIndData_h << endl;

  oIndData_h << "{" << endl;
  oIndData_h << "   for( int j=0; j<nRecords; j++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      if( " << UserStr.MDV << "[j] == 0 )" << endl;
  oIndData_h << "      {" << endl;
  oIndData_h << "         nY++;" << endl;
  oIndData_h << "      }" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "   measurements.resize( nY );" << endl;

  // Initialize place holders for scalar variables.
  //
  // getScalars()
  //
  oIndData_h << "   //" << endl;
  oIndData_h << "   // Initialize scalar variables" << endl;
  oIndData_h << "   //" << endl;
  pInternalTable = internalTable->begin();
  for( ; pInternalTable != internalTable->end(); pInternalTable++ )
    {
      const string varName    = pInternalTable->second.name;
      const string keyVarName = SymbolTable::key( varName );
      enum Symbol::ObjectType objectType = pInternalTable->second.object_type;
      enum Symbol::Ownership  owner      = pInternalTable->second.owner;
      /*
      if( keyLabel == KeyStr.OMEGA 
	  || keyLabel == KeyStr.SIGMA
	  || keyLabel == KeyStr.THETA
	  || keyLabel == KeyStr.ETA
	  || keyLabel == KeyStr.ETARES
	  || keyLabel == KeyStr.WETARES
	  || keyLabel == KeyStr.IETARES
	  || keyLabel == KeyStr.IWETARES
	  || keyLabel == KeyStr.PETARES
	  || keyLabel == KeyStr.PWETARES
	  || keyLabel == KeyStr.CETARES
	  || keyLabel == KeyStr.CWETARES
	  || keyLabel == KeyStr.EPS
	  || keyLabel == KeyStr.DADT
	  || keyLabel == KeyStr.A
	  || keyLabel == KeyStr.P
	  )
	{
	  continue;
	}

      // fill the place holders with -99999
      if( find( labels->begin(), labels->end(), pInternalTable->second.name ) 
	  == labels->end() )
	oIndData_h << "fill( " << label << ".begin(), " << label << ".end(), -99999 );" << endl;
      */
      if( owner != Symbol::DATASET && objectType != Symbol::VECTOR && objectType != Symbol::MATRIX )
	{
	  oIndData_h << "fill( " << varName << ".begin(), " << varName << ".end(), -99999 );" << endl;
	}
    }
  oIndData_h << endl;


  // Save DV values in ORGDV so that the original data set values are kept in case
  // simulation overwrites DV.
  oIndData_h << "copy( " << UserStr.DV << ".begin(), " << UserStr.DV << ".end(), ";
  oIndData_h << UserStr.ORGDV << ".begin() );" << endl;
  oIndData_h << endl;

  // Resize and initialize (with -999999) vector variables.
  //
  //  table->getVectors();
  //
  oIndData_h << "   //" << endl;
  oIndData_h << "   // Resize and initialize vector variables" << endl;
  oIndData_h << "   //" << endl;
  oIndData_h << "   jTojPrime.resize( nRecords );" << endl;
  oIndData_h << "   jPrimeToj.resize( nY );" << endl;
  oIndData_h << "   for( int j=0, jPrime=0; j<nRecords; j++ )" << endl;
  oIndData_h << "   {" << endl;
  if( myThetaLen > 0 )
    {
      oIndData_h << "      " << UserStr.THETA << "[j].resize( " << myThetaLen << " );" << endl;
      oIndData_h << "      " << "fill( " << UserStr.THETA << "[j].begin(), " << UserStr.THETA << "[j].end(), -99999 );" << endl;
    }
  if( myEtaLen > 0 )
    {
      oIndData_h << "      " << UserStr.ETA     << "[j].resize( " << myEtaLen << " );" << endl;
      oIndData_h << "      " << "fill( " << UserStr.ETA << "[j].begin(), " << UserStr.ETA << "[j].end(), -99999 );" << endl;
      if( getTarget() == POP )
	{
	  oIndData_h << "      " << UserStr.ETARES   << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.WETARES  << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.IETARES  << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.IWETARES << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.PETARES  << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.PWETARES << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.CETARES  << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.CWETARES << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.ETARES   << "[j].begin(), ";
	  oIndData_h << UserStr.ETARES   << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.WETARES  << "[j].begin(), ";
	  oIndData_h << UserStr.WETARES  << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.IETARES  << "[j].begin(), ";
	  oIndData_h << UserStr.IETARES  << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.IWETARES << "[j].begin(), ";
	  oIndData_h << UserStr.IWETARES << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.PETARES  << "[j].begin(), ";
	  oIndData_h << UserStr.PETARES  << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.PWETARES << "[j].begin(), ";
	  oIndData_h << UserStr.PWETARES << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.CETARES  << "[j].begin(), ";
	  oIndData_h << UserStr.CETARES  << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.CWETARES << "[j].begin(), ";
	  oIndData_h << UserStr.CWETARES << "[j].end(), -99999 );" << endl;
	}
    }
  if( myEpsLen > 0 )
    {
      oIndData_h << "      " << UserStr.EPS   << "[j].resize( " << myEpsLen << " );" << endl;
      oIndData_h << "      " << "fill( " << UserStr.EPS   << "[j].begin(), " << UserStr.EPS << "[j].end(), -99999 );" << endl;
    }
  
  if( myModelSpec == ADVAN6 || myModelSpec == ADVAN8 || myModelSpec == ADVAN9 )
    {
      assert( table->findi( KeyStr.DADT ) != Symbol::empty() );
      assert( table->findi( KeyStr.A    ) != Symbol::empty() );
      assert( table->findi( KeyStr.P    ) != Symbol::empty() );
      
      oIndData_h << "      " << UserStr.DADT << "[j].resize( " << myCompModel->getNCompartments()  << " );" << endl;
      oIndData_h << "      " << UserStr.A    << "[j].resize( " << myCompModel->getNCompartments()  << " );" << endl;
      oIndData_h << "      " << UserStr.P    << "[j].resize( " << myCompModel->getNParameters()    << " );" << endl;
      
      oIndData_h << "      " << "fill( " << UserStr.DADT << "[j].begin(), " << UserStr.DADT << "[j].end(), -99999 );" << endl;
      oIndData_h << "      " << "fill( " << UserStr.A    << "[j].begin(), " << UserStr.A    << "[j].end(), -99999 );" << endl;
      oIndData_h << "      " << "fill( " << UserStr.P    << "[j].begin(), " << UserStr.P    << "[j].end(), -99999 );" << endl;
      
    }
  
  oIndData_h << "        if( " << UserStr.MDV << "[j] == 0 )" << endl;
  oIndData_h << "        {" << endl;
  oIndData_h << "           assign( measurements[jPrime], " << UserStr.DV << "[j] );" << endl;
  oIndData_h << "           jPrimeToj[jPrime] = j;" << endl;
  oIndData_h << "           jTojPrime[j] = jPrime;" << endl;
  oIndData_h << "           jPrime++;" << endl;
  oIndData_h << "        }" << endl;
  oIndData_h << "        else" << endl;
  oIndData_h << "        {" << endl;
  oIndData_h << "           jTojPrime[j] = -1;" << endl;
  oIndData_h << "        }" << endl;
  oIndData_h << "   } // end of FOR loop over vector variables" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // ----------
  // Destructor
  // ---------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "IndData<spk_ValueType>::~IndData(){}" << endl;

  // -------------------
  // Default constructor
  // (protected)
  // -------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "IndData<spk_ValueType>::IndData(){}" << endl;

  // ----------------
  // Copy constructor
  // (protected)
  // ---------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "IndData<spk_ValueType>::IndData( const IndData<spk_ValueType>& ){}" << endl;

  // -------------------
  // Assignment operator
  // (protected)
  // -------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "IndData<spk_ValueType>& IndData<spk_ValueType>::operator=( const IndData<spk_ValueType>& ){}" << endl;

  //------------------
  // getNRecords()
  //------------------
  oIndData_h << "// return the number of data records (include MDV=1)" << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "int IndData<spk_ValueType>::getNRecords() const " << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   return nRecords;" << endl;
  oIndData_h << "}" << endl;

  //------------------
  // getNObservs()
  //------------------
  oIndData_h << "// return the number of measurements (only ones with MDV=0)" << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "int IndData<spk_ValueType>::getNObservs() const " << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   return nY;" << endl;
  oIndData_h << "}" << endl;

  //----------------------------------------
  // getMeasurementIndex( int j' )
  //----------------------------------------
  oIndData_h << "// Return an index to y (measurements/DVs) vector, j, such that " << endl;
  oIndData_h << "// the value of j-th element in y corresponds to the DV value of " << endl;
  oIndData_h << "// the j'-th record in the data set .  " << endl;
  oIndData_h << "// If the j'-th record does not have a DV value, the returned value is -1." << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "int IndData<spk_ValueType>::getMeasurementIndex( int recordIndex ) const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   return jTojPrime[recordIndex];" << endl;
  oIndData_h << "}" << endl;

  //----------------------------------------
  // getRecordIndex( int j )
  //----------------------------------------
  oIndData_h << "// Return the index, j', to a record in the dataset to which the value of " << endl;
  oIndData_h << "// the j-th element of y (measurements/DVs) vector belongs." << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "int IndData<spk_ValueType>::getRecordIndex( int measurementIndex ) const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   return jPrimeToj[measurementIndex];" << endl;
  oIndData_h << "}" << endl;

  // -----------------
  // getMeasurements()
  // -----------------
  oIndData_h << "// Return SPK's y" << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "const SPK_VA::valarray<double> IndData<spk_ValueType>::getMeasurements() const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   return measurements;" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // ---------------------
  // replaceMeasurements()
  // ---------------------
  // Replace (the internally kept) y with the given y'.
  //
  oIndData_h << "// Replace y with the given y'." << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceMeasurements( const SPK_VA::valarray<double>& yyi )" << endl;
  oIndData_h << "{" << endl;
  bool hasAlias = ( pDV->synonym != "" );
  oIndData_h << "   for( int i=0, k=0; i<nRecords; i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      if( " << UserStr.MDV << "[i] == 0 )" << endl;
  oIndData_h << "      {" << endl;
  oIndData_h << "         " << UserStr.ORGDV << "[i] = " << UserStr.DV << "[i];" << endl;
  oIndData_h << "         " << UserStr.DV << "[i] = yyi[k];" << endl;
  if( hasAlias )
    oIndData_h << "         " << pDV->synonym << "[i] = yyi[k];" << endl;
  oIndData_h << "         k++;" << endl;
  oIndData_h << "      }" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
 
  // --------------------
  // replacePred()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replacePred( const SPK_VA::valarray<double>& predIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( predIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.PRED << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.PRED << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = predIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceRes( const SPK_VA::valarray<double>& ResIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( ResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.RES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.RES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = ResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceWRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceWRes( const SPK_VA::valarray<double>& WResIn )"    << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( WResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.WRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.WRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = WResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceIPred()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceIPred( const SPK_VA::valarray<double>& iPredIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( iPredIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.IPRED << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.IPRED << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = iPredIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceIRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceIRes( const SPK_VA::valarray<double>& iResIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( iResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.IRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.IRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = iResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceIWRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceIWRes( const SPK_VA::valarray<double>& iWResIn )"    << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( iWResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.IWRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.IWRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = iWResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replacePPred()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replacePPred( const SPK_VA::valarray<double>& pPredIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( pPredIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.PPRED << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.PPRED << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = pPredIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replacePRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replacePRes( const SPK_VA::valarray<double>& pResIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( pResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.PRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.PRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = pResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replacePWRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replacePWRes( const SPK_VA::valarray<double>& pWResIn )"    << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( pWResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.PWRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.PWRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = pWResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceCPred()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceCPred( const SPK_VA::valarray<double>& cPredIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( cPredIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.CPRED << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.CPRED << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = cPredIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceCRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceCRes( const SPK_VA::valarray<double>& cResIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( cResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.CRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.CRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = cResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceCWRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceCWRes( const SPK_VA::valarray<double>& cWResIn )"    << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( cWResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.CWRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.CWRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = cWResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  if( getTarget() == POP )
    {
      // --------------------
      // replaceEta()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceEta( const SPK_VA::valarray<double>& etaIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( etaIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.ETA << "[i][j] = etaIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
      
      // --------------------
      // replaceEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceEtaRes( const SPK_VA::valarray<double>& EtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( EtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.ETARES << "[i][j] = EtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
      
      // --------------------
      // replaceWEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceWEtaRes( const SPK_VA::valarray<double>& WEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( WEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.WETARES << "[i][j] = WEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;

      // --------------------
      // replaceIEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceIEtaRes( const SPK_VA::valarray<double>& iEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( iEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.IETARES << "[i][j] = iEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
      
      // --------------------
      // replaceIWEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceIWEtaRes( const SPK_VA::valarray<double>& iWEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( iWEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.IWETARES << "[i][j] = iWEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
 
      // --------------------
      // replacePEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replacePEtaRes( const SPK_VA::valarray<double>& pEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( pEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.PETARES << "[i][j] = pEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
      
      // --------------------
      // replacePWEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( pWEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.PWETARES << "[i][j] = pWEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;

      // --------------------
      // replaceCEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceCEtaRes( const SPK_VA::valarray<double>& cEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( cEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.CETARES << "[i][j] = cEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
      
      // --------------------
      // replaceCWEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( cWEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.CWETARES << "[i][j] = cWEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
    }

  // -------------------
  // assignToDbl()
  // -------------------
  // This is to make an assignment operation, a = b, transparent for situations where a is double and b is CppAD<double>
  // and a an b are both double.
  //
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::assign( double & d, const CppAD::AD<double>& ad ) const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   d = CppAD::Value( ad );" << endl;
  oIndData_h << "   return;" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::assign( double & left, double right ) const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   left = right;" << endl;
  oIndData_h << "   return;" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  oIndData_h << "#endif" << endl;

  oIndData_h.close();
}
