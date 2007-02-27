/**
 * @file NM_generateDataSet.cpp 
 * Define NonmemTranslator::generateDataSet().
 */
#include <fstream>
#include <string>
#include <cstdlib>
#include "NonmemTranslator.h"
#include "SpkCompilerException.h"
#include <fcntl.h>
#include <malloc.h>
#include <stdlib.h>
#include <inttypes.h>

using namespace std;
using namespace xercesc;

//=========================================================================================
//
// Generate DataSet.h, a file declaring and defining DataSet template class,
// a class that represents an entire population data set.
//
// Pre-conditions  - The symbol table contains the NONMEM keywords and user defined variable
//                   names needed by the user-provided model.
//
//                 - The symbol table contains entries for the data labels and aliases.
//                   The list of data labels have to be retrievable by calling 
//                   SymbolTable::getlabels().  
//          
//                 - The vector returned by SymbolTable::getLables() must contain
//                   the data labels in the order in which they define the data items 
//                   (ie. columns) in the data set.
//
//                 - The current working directory is writable.
//
// post-conditions - DataSet.h will be generated in the current working directory.
//
//=========================================================================================
void NonmemTranslator::generateDataSet( ) const
{
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preliminaries
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  // 
  // A SymbolTable object uses a std::map object as the actual
  // table.  The usual way of retrieving the entries in the internal
  // table is though member functions provided by SymbolTable class.
  // Another way, which is used here, is to access the internal
  // table directly, which is faster and convenient.
  //
  const map<const string, Symbol> * const t = table->getTable();

  //
  // The order in which the label strings appear is significant.
  // So, get a constant pointer to the list so that I cannot 
  // mess it up.
  //
  const vector<string> *labels = table->getLabels();
  vector<string>::const_iterator pLabel;
  const int nLabels = labels->size();

  //
  // Reference to an Symbol object that represents "ID" data label.
  // Keep it for repetitive use.
  //
  const Symbol * const pID = table->find( nonmem::ID );

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Write into DataSet.h
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // Declare and define DataSet template class.
  // For name binding reason, the declaration and the definition
  // are both stored in a single file: DataSet.h
  // 
  // The only legal constructor is the default constructor.
  // The constructor initializes the array of IndData objects,
  // each contains the entire data set for a subject.
  //
  // The order in which the arguments to the IndData 
  // constructor appear is significant.  The order must
  // match with the IndData constructor's interface.
  // It relizes on the order of strings stored in the list 
  // returned by "SymbolTable::getLabels()".
  // Thus, in between the time when the DataSet constructor
  // is defined and the time when the IndData constructor
  // is declared/defined, the SymbolTable object
  // may NOT be modified.
  //
  ofstream oDataSet_h( fDataSet_h );
  if( !oDataSet_h.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Failed to create a file, %s.", fDataSet_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  //---------------------------------------------------------------------------------------
  //
  // Print out some description for this file.
  //
  //---------------------------------------------------------------------------------------
  oDataSet_h << "// " << myDescription << endl;

  //---------------------------------------------------------------------------------------
  // 
  // Header include statements.
  //
  //---------------------------------------------------------------------------------------
  oDataSet_h << "#ifndef DATASET_H" << endl;
  oDataSet_h << "#define DATASET_H" << endl;

  oDataSet_h << "#include <vector>" << endl;
  oDataSet_h << "#include <spk/doubleToScalar.h>" << endl;
  oDataSet_h << "#include <spk/SpkValarray.h>" << endl;
  oDataSet_h << "#include \"IndData.h\"" << endl;
  oDataSet_h << "#include <fcntl.h>" << endl;
  oDataSet_h << "#include <malloc.h>" << endl;
  oDataSet_h << "#include <stdlib.h>" << endl;
  oDataSet_h << endl;

  //---------------------------------------------------------------------------------------
  //
  // Declaration of DataSet class
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
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "class DataSet" << endl;
  oDataSet_h << "{" << endl;
      
  //----------------------------------------
  // Public member declarations.
  //----------------------------------------
  oDataSet_h << "public:" << endl;

  // -------------------
  // Default constructor
  // -------------------
  // The default constructor initializes the entire data set
  // internally.
  //
  oDataSet_h << "   DataSet();" << endl;

  // ----------
  // Destructor
  // ----------
  oDataSet_h << "   ~DataSet();" << endl;
  oDataSet_h << endl;

  // ------------
  // The data set
  // ------------
  oDataSet_h << "   std::vector<IndData<spk_ValueType>*> data;" << endl;

  // -----------------------
  // Public member functions
  // -----------------------
  oDataSet_h << "   void expand( const SPK_VA::valarray<double>& measurements, SPK_VA::valarray<double>& records ) const;" << endl;
  oDataSet_h << "   void expand( int who, const SPK_VA::valarray<double>& measurements_who, SPK_VA::valarray<double>& records_who ) const;" << endl;
  oDataSet_h << "   int getMeasurementIndex( int recordIndex ) const;" << endl;
  oDataSet_h << "   int getMeasurementIndex( int who, int recordIndex ) const;" << endl;
  oDataSet_h << "   int getRecordIndex( int measurementIndex ) const;" << endl;
  oDataSet_h << "   int getRecordIndex( int who, int measurementIndex ) const;" << endl;
  oDataSet_h << "   int getPopSize() const;" << endl;
  oDataSet_h << "   const SPK_VA::valarray<int> getN() const;" << endl;
  oDataSet_h << "   const SPK_VA::valarray<int> getNObservs() const;" << endl;
  oDataSet_h << "   int getNObservs( int i ) const;" << endl;
  oDataSet_h << "   const SPK_VA::valarray<int> getNRecords() const;" << endl;
  oDataSet_h << "   int getNRecords( int i ) const;" << endl;
  oDataSet_h << "   const SPK_VA::valarray<double> getAllMeasurements() const;" << endl;
  oDataSet_h << "   void replaceAllMeasurements( const SPK_VA::valarray<double> & yy );"    << endl;
  oDataSet_h << "   void replaceEta     ( const SPK_VA::valarray<double>& EtaIn );"      << endl;
  oDataSet_h << "   void replacePred    ( const SPK_VA::valarray<double>& PredIn );"        << endl;
  oDataSet_h << "   void replaceRes     ( const SPK_VA::valarray<double>& ResIn );"         << endl;
  oDataSet_h << "   void replaceWRes    ( const SPK_VA::valarray<double>& WResIn );"        << endl;
  oDataSet_h << "   void replaceEtaRes  ( const SPK_VA::valarray<double>& EtaResIn );"      << endl;
  oDataSet_h << "   void replaceWEtaRes ( const SPK_VA::valarray<double>& WEtaResIn );"     << endl;
  oDataSet_h << "   void replaceIPred   ( const SPK_VA::valarray<double>& iPredIn );"       << endl;
  oDataSet_h << "   void replaceIRes    ( const SPK_VA::valarray<double>& iResIn );"        << endl;
  oDataSet_h << "   void replaceIWRes   ( const SPK_VA::valarray<double>& iWResIn );"       << endl;
  oDataSet_h << "   void replaceIEtaRes ( const SPK_VA::valarray<double>& iEtaResIn );"     << endl;
  oDataSet_h << "   void replaceIWEtaRes( const SPK_VA::valarray<double>& iWEtaResIn );"    << endl;
  oDataSet_h << "   void replacePPred   ( const SPK_VA::valarray<double>& pPredIn );"       << endl;
  oDataSet_h << "   void replacePRes    ( const SPK_VA::valarray<double>& pResAllIn );"     << endl;
  oDataSet_h << "   void replacePWRes   ( const SPK_VA::valarray<double>& pWResAllIn );"    << endl;
  oDataSet_h << "   void replacePEtaRes ( const SPK_VA::valarray<double>& pEtaResAllIn );"  << endl;
  oDataSet_h << "   void replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResAllIn );" << endl;
  oDataSet_h << "   void replaceCPred   ( const SPK_VA::valarray<double>& cPredIn );"       << endl;
  oDataSet_h << "   void replaceCRes    ( const SPK_VA::valarray<double>& cResAllIn );"     << endl;
  oDataSet_h << "   void replaceCWRes   ( const SPK_VA::valarray<double>& cWResAllIn );"    << endl;
  oDataSet_h << "   void replaceCEtaRes ( const SPK_VA::valarray<double>& cEtaResAllIn );"  << endl;
  oDataSet_h << "   void replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResAllIn );" << endl;
  oDataSet_h << endl;
  //  oDataSet_h << "   friend std::ostream& operator<< <spk_ValueType>( std::ostream& o, const DataSet<spk_ValueType>& A );" << endl;
  oDataSet_h << endl;
 
  //----------------------------------------
  // Protected member declarations.
  //----------------------------------------
  oDataSet_h << "protected:" << endl;
  oDataSet_h << "   DataSet( const DataSet& );" << endl;
  oDataSet_h << "   DataSet& operator=( const DataSet& );" << endl;
  oDataSet_h << endl;

  //----------------------------------------
  // Converted to public due to access by non-friends 06/22/2006 ernst@u
  //----------------------------------------
  oDataSet_h << "public:" << endl;
  oDataSet_h << "   SPK_VA::valarray<int> NRecords;  // # of records for each individual." << endl;  

  //----------------------------------------
  // Private member declarations.
  //----------------------------------------
  oDataSet_h << "private:" << endl;
  oDataSet_h << "   SPK_VA::valarray<double> measurements; // a long vector containg all measurements" << endl;
  //  oDataSet_h << "   SPK_VA::valarray<int> NRecords;  // # of records for each individual." << endl;
  oDataSet_h << "   SPK_VA::valarray<int> NObservs; // # of measurements for each individual." << endl;
  oDataSet_h << "   const int popSize;" << endl;
  oDataSet_h << endl;
  oDataSet_h << "   /////////////////////////////////////////////////////////" << endl;
  oDataSet_h << "   //      original                     y"                    << endl;
  oDataSet_h << "   //  -------------------      -------------------"          << endl;
  oDataSet_h << "   //   j    i   MDV   DV         j'  j   i   DV"             << endl;
  oDataSet_h << "   //  -------------------      -------------------"          << endl;
  oDataSet_h << "   //   0    0    0    0.1        0   0   0   0.1"            << endl;
  oDataSet_h << "   //   1    0    1               1   2   0   0.2"            << endl;
  oDataSet_h << "   //   2    0    0    0.2        2   4   0   0.3"            << endl;
  oDataSet_h << "   //   3    0    1               3   5   1   0.01"           << endl;
  oDataSet_h << "   //   4    0    0    0.3        4   7   1   0.02"           << endl;
  oDataSet_h << "   //   5    1    0    0.1        5   9   1   0.03"           << endl;
  oDataSet_h << "   //   6    1    1"                                          << endl;
  oDataSet_h << "   //   7    1    0    0.2"                                   << endl;
  oDataSet_h << "   //   8    1    1"                                          << endl;
  oDataSet_h << "   //   9    1    0    0.3"                                   << endl;
  oDataSet_h << "   //"                                                        << endl;
  oDataSet_h << "   //"                                                        << endl;
  oDataSet_h << "   //   jTojPrime            jPrimeToj"                       << endl;
  oDataSet_h << "   //  -----------          -----------"                      << endl;
  oDataSet_h << "   //    j    j'              j'   j"                         << endl;
  oDataSet_h << "   //  -----------          -----------"                      << endl;
  oDataSet_h << "   //    0    0               0    0"                         << endl;
  oDataSet_h << "   //    1   -1*              1    2"                         << endl;
  oDataSet_h << "   //    2    1               2    4"                         << endl;
  oDataSet_h << "   //    3   -1*              3    5"                         << endl;
  oDataSet_h << "   //    4    2               4    7"                         << endl;
  oDataSet_h << "   //    5    3               5    9"                         << endl;
  oDataSet_h << "   //    6   -1*"                                             << endl;
  oDataSet_h << "   //    7    4"                                              << endl;
  oDataSet_h << "   //    8   -1*"                                             << endl;
  oDataSet_h << "   //    9    5"                                              << endl;
  oDataSet_h << "   //"                                                        << endl;
  oDataSet_h << "   //  * (-1) points to no j', i.e. MDV=1"                    << endl;
  oDataSet_h << "   /////////////////////////////////////////////////////////" << endl;
  oDataSet_h << "   std::vector<int> jTojPrime;" << endl;
  oDataSet_h << "   std::vector<int> jPrimeToj;" << endl;

  oDataSet_h << "};" << endl;
  oDataSet_h << endl;

  //---------------------------------------------------------------------------------------
  //
  // Definition of DataSet class
  //
  //---------------------------------------------------------------------------------------
       
  // -------------------
  // Default constructor
  // -------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "DataSet<spk_ValueType>::DataSet()" << endl;

  //
  // Constructor intialization
  //
  oDataSet_h << ": popSize( "  << getPopSize() << " )," << endl;
  oDataSet_h << "  data( "     << getPopSize() << " )," << endl;
  oDataSet_h << "  NRecords( " << getPopSize() << " ), // #of records for each individual" << endl;
  oDataSet_h << "  NObservs( " << getPopSize() << " )  // #of DVs for each individuals" << endl;

  //
  // Constructor body
  //
  oDataSet_h << "{" << endl;
  //-----------------------------------------------------------------------------------------++++
  oDataSet_h << "int NRecords_c[] = { ";
  int nRecords_total = 0;
  for( int who=0; who < getPopSize(); who++)
    {
      if( who > 0 )
		oDataSet_h << ", "; 
      oDataSet_h << pID->initial[who].size();
      nRecords_total += pID->initial[who].size();
    }
  oDataSet_h << " };" << endl;
  oDataSet_h << "NRecords = SPK_VA::valarray<int> (NRecords_c, "<< getPopSize() <<" );" << endl;
  //-----------------------------------------------------------------------------------------++++ 
  //
  // The order in which the labels appear must be consistent
  // with the order in the constructor declaration.
  // By using the iterator in both places, here and up there,
  // they shall match.  However, this should be tested in
  // the corresponding unit tests.
  //
  //
  // Initialize C arrays with data values.
  // The C arrays are passed to the valarray's constructor.
  //

  // open up dataset file
  int n_read, n_written, n_bytes, ok;
  int fd = open("spk_dataset.dat", O_CREAT|O_RDWR, 0700);

  if ( fd == -1 ) {
    fprintf(stderr, "open of spk_dataset.dat failed\n");
    exit(1);
  }

  pLabel = labels->begin();

  // in the output file, we need to define the file descriptor and get 
  // everything set up for reading in the values.
  oDataSet_h << endl << "int fd, n_read = 0, n_bytes;" << endl;
  oDataSet_h << endl << "double buffer_in[" << nRecords_total << "];" << endl;
  oDataSet_h << endl << "// open for reading, perms for read";
  oDataSet_h << endl << "fd = open(\"spk_dataset.dat\", O_RDONLY, 0);";
  oDataSet_h << endl << "if ( fd == -1 ) {" << endl << "fprintf(stderr,\"open of SPK data file failed\\n\");" << endl << "exit(1);" << endl << "}" << endl;


  for( int i=0; pLabel != labels->end(), i<nLabels; i++, pLabel++ )
    {
      const Symbol * s = table->find( *pLabel );
      bool isID  = (*pLabel == pID->name);
      bool isInt = (*pLabel == nonmem::EVID || *pLabel == nonmem::CMT || *pLabel == nonmem::PCMT );
      
      string carray_name = s->name + "_c";

      int temp_buffer_int[nRecords_total];
      double temp_buffer[nRecords_total];
  
      if( isID ) {
	oDataSet_h << "char* const";
	oDataSet_h << " " << carray_name << "[] = { "; 
      }
      else if( isInt ) {
	oDataSet_h << "int";
	oDataSet_h << " " << carray_name << "[" << nRecords_total << "];" << endl;
	//	oDataSet_h << "int_buffer_in[" << nRecords_total << "];" << endl;
	oDataSet_h << "n_bytes = sizeof(int) * " <<  nRecords_total << ";" << endl;
	oDataSet_h << "n_read = read(fd, &" << carray_name << ", n_bytes);" << endl;
	oDataSet_h << "if ( n_read != n_bytes ) {" << endl << "exit(1);" << endl << "}" << endl;
      }
      else {
	//	oDataSet_h << "const spk_ValueType";
	oDataSet_h << "spk_ValueType";
	oDataSet_h << " " << carray_name << "[" << nRecords_total << "];" << endl;
	//	oDataSet_h << "free(buffer_in);" << endl;
	//	oDataSet_h << "double buffer_in[" << nRecords_total << "];" << endl;
	oDataSet_h << "n_bytes = sizeof(double) * " <<  nRecords_total << ";" << endl;
	oDataSet_h << "n_read = read(fd, buffer_in, n_bytes);" << endl;
	oDataSet_h << "if ( n_read != n_bytes ) {" << endl << "exit(1);" << endl << "}" << endl;

	oDataSet_h << "for ( int me=0; me < " << nRecords_total << "; me++ ) {" << endl;
	oDataSet_h << "doubleToScalar( buffer_in[me], " << carray_name << "[me] );" << endl;
	oDataSet_h << "}" << endl << endl;
      }
      

      // Loop over individuals
      int nRecsOut = 0;
      for( int who=0, nRecords=0; who < getPopSize(); who++)
	{
	  nRecords = pID->initial[who].size();
	  // Loop over records per individual
	  for( int j=0; j<nRecords; j++ )
	    {
	      if ( isID ) {
		if( who + j > 0  )
		  oDataSet_h << ", ";
		if( j == 0  )
		  //	oDataSet_h << endl << "      ";
		  oDataSet_h << endl << "    /* " << who+1 << "*/  ";
		oDataSet_h << "\"" << s->initial[who][j] << "\"";
	      }
	      else if ( isInt ) {
		temp_buffer_int[nRecsOut] = atoi(s->initial[who][j].c_str());
	      } 
	      else {
		temp_buffer[nRecsOut] = atof(s->initial[who][j].c_str());
	      }
	      nRecsOut++;
	    }
	}
      if ( isID ) {
	oDataSet_h << " };" << endl;
      }
      else if ( isInt ) {
	// output the temporary variable to the data file
	n_bytes = sizeof(int) * nRecords_total;
	n_written = write(fd, temp_buffer_int, n_bytes);

      if ( n_written != n_bytes )
	{
	  printf("write of something failed in: %s\n", &carray_name);
	  exit(1);
	}

      }
      else {
	n_bytes = sizeof(double) * nRecords_total;
	n_written = write(fd, temp_buffer, n_bytes);

      if ( n_written != n_bytes )
	{
	  printf("write of something failed in: %s\n", &carray_name);
	  exit(1);
	}

      }
      
      oDataSet_h << endl << "/**************** END OF " << carray_name << " *********************/" << endl << endl;

      // deleting causes segfault.
      //delete(temp_buffer);
      //delete(temp_buffer_int);
    }
  
  oDataSet_h << endl;
  oDataSet_h << "close(fd);" << endl;
  
  // closes the dataset file
  close(fd);
  
  
  // Create IndData object for each individual.  
  // The order in which the arguments
  // are passed to the IndData constructor must be strictly
  // compliant to the order in which the label strings are stored
  // in the list returned by SymbolTable::getLabels().
  //
  oDataSet_h << "for (int i=0, start=0, stop=0, nRec=0; i < " << getPopSize() << "; i++) {" << endl;
  oDataSet_h << "  nRec = NRecords[i];" << endl;
  oDataSet_h << "  stop = start + nRec;" << endl;
  oDataSet_h << "  data[i] = new IndData<spk_ValueType> (nRec";

  pLabel = labels->begin();
  for( int i=0; pLabel != labels->end(), i<nLabels; i++, pLabel++ )
    {
      oDataSet_h << "," << endl;
      oDataSet_h << "              std::vector<";
      const Symbol * s = table->find( *pLabel );
      bool isID  = (*pLabel == pID->name);
      bool isInt = (*pLabel == nonmem::EVID || *pLabel == nonmem::CMT || *pLabel == nonmem::PCMT );
      
      if( isID )
	oDataSet_h << "char*>        ( ";
      else if( isInt )
	oDataSet_h << "int>          ( ";
      else
	oDataSet_h << "spk_ValueType>( ";

      oDataSet_h << s->name << "_c+start ," <<  s->name << "_c+stop )";
    }
  oDataSet_h << " );" << endl;

  oDataSet_h << "  start = stop;" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;  

  // ==============================NO CHANGE BELOW ==============================================
  // 
  // Extracts measurements (ie. SPK's y) from the entire data set and keep it in "measurements".
  //
  oDataSet_h << "   int nRecords = 0;" << endl;
  oDataSet_h << "   for( int i=0; i<popSize; i++ )" << endl;
  oDataSet_h << "      nRecords += data[i]->getNRecords();" << endl;
  oDataSet_h << "   " << endl;
  oDataSet_h << "   int nY = 0;  // # of DVs" << endl;
  oDataSet_h << "   for( int i=0; i<popSize; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      NObservs[i] = data[i]->getMeasurements().size();" << endl;
  oDataSet_h << "      nY += NObservs[i];" << endl;  
  oDataSet_h << "   }" << endl;
  oDataSet_h << "   measurements.resize( nY ); " << endl;
  oDataSet_h << "   jPrimeToj.resize( nY );" << endl;
  oDataSet_h << "   jTojPrime.resize( nRecords );" << endl;
  oDataSet_h << "   for( int i=0, m=0, j=0, jPrime=0; i<popSize; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      int nYi = data[i]->getMeasurements().size();" << endl;
  oDataSet_h << "      measurements[ SPK_VA::slice( m, nYi, 1 ) ] = data[i]->getMeasurements();" << endl;
  oDataSet_h << "      m+=nYi;" << endl;
  oDataSet_h << "      int n = data[i]->getNRecords();" << endl;
  oDataSet_h << "      for( int k=0; k<n; k++, j++ )" << endl;
  oDataSet_h << "      {" << endl;
  oDataSet_h << "         if( data[i]->" << nonmem::MDV << "[k] == 0 )" << endl;
  oDataSet_h << "         {" << endl;
  oDataSet_h << "            jPrimeToj[jPrime] = j;" << endl;
  oDataSet_h << "            jTojPrime[j] = jPrime;" << endl;
  oDataSet_h << "            jPrime++;" << endl;
  oDataSet_h << "         }" << endl;
  oDataSet_h << "         else" << endl;
  oDataSet_h << "            jTojPrime[j] = -1;" << endl;
  oDataSet_h << "      }" << endl;
  oDataSet_h << "   }" << endl;

  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ----------
  // Destructor
  // ----------
  // Free the memory allocated for the data set.
  //
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "DataSet<spk_ValueType>::~DataSet()" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int nPop = data.size();" << endl;
  oDataSet_h << "   for( int i=0; i<nPop; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      delete data[i];" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ----------------
  // Copy constructor
  // (protected)
  // ----------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "DataSet<spk_ValueType>::DataSet( const DataSet<spk_ValueType>& )" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // -------------------
  // Assignment operator
  // (protected)
  // -------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "DataSet<spk_ValueType>& DataSet<spk_ValueType>::operator=( const DataSet<spk_ValueType>& )" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------------
  // getMeasurementIndex( int j )
  // This returns the index to an observation record that corresponds to the record index within the whole population.
  // ------------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "int DataSet<spk_ValueType>::getMeasurementIndex( int recordIndex ) const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return jTojPrime[ recordIndex ];" << endl;
  oDataSet_h << "}" << endl;

  // ------------------------------
  // getMeasurementIndex( int i, int j )
  // This returns the index to an observation record that corresponds to the i-th individual's record index.
  // ------------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "int DataSet<spk_ValueType>::getMeasurementIndex( int who, int recordIndex ) const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return data[who]->getMeasurementIndex(recordIndex);" << endl;
  oDataSet_h << "}" << endl;

  // ------------------------------
  // getRecordIndex( int i, int jPrime )
  // This returns the index to a record that corresponds to the measurement index within the whole population.
  // ------------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "int DataSet<spk_ValueType>::getRecordIndex( int measurementIndex ) const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return jPrimeToj[ measurementIndex ];" << endl;
  oDataSet_h << "}" << endl;

  // ------------------------------
  // getRecordIndex( int i, int jPrime )
  // This returns the index to a record that corresponds to the i-th individual's measurement index.
  // ------------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "int DataSet<spk_ValueType>::getRecordIndex( int who, int measurementIndex ) const" << endl;
  oDataSet_h << "{" << endl;
  /*
  oDataSet_h << "   return jPrimeToj[ measurementIndex ];" << endl;
  */
  oDataSet_h << "   return data[who]->getRecordIndex(measurementIndex);" << endl;
  oDataSet_h << "}" << endl;

  // --------------------
  // getAllMeasurements()
  // --------------------
  // Returns SPK's y.
  //
  oDataSet_h << "// Returns SPK's y." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "const SPK_VA::valarray<double> DataSet<spk_ValueType>::getAllMeasurements() const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return measurements;" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;


  //----------------------------------------------------------
  // expand( const valarray& measurements, valarray& records )
  // ----------------------------------------------------------
  oDataSet_h << "// Sets the record values that have measurement values for all of the individuals." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::expand( " << endl;
  oDataSet_h << "       const SPK_VA::valarray<double> & measurements," << endl;  
  oDataSet_h << "       SPK_VA::valarray<double> & records ) const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = NObservs.sum();" << endl;
  oDataSet_h << "   int m = 0;" << endl;
  oDataSet_h << "   for( int i=0; i<popSize; i++ )" << endl;
  oDataSet_h << "      m += getNRecords(i);" << endl;
  oDataSet_h << "   records.resize( m );" << endl;
  oDataSet_h << "   records = undefinedValue<double>();" << endl;
  oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      records[ getRecordIndex( i ) ] = measurements[i];" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
                                                                                
  //----------------------------------------------------------
  // expand( int who, const valarray& measurements_who, valarray& records_who )
  // ----------------------------------------------------------
  oDataSet_h << "// Sets the record values that have measurement values for the who-th individual." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::expand( " << endl;
  oDataSet_h << "       int who," << endl;
  oDataSet_h << "       const SPK_VA::valarray<double>& measurements_who," << endl;
  oDataSet_h << "       SPK_VA::valarray<double>& records_who ) const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = measurements_who.size();" << endl;
  oDataSet_h << "   assert( n == NObservs[who] );" << endl;
  oDataSet_h << "   int m = getNRecords(who);" << endl;
  oDataSet_h << "   assert( m >= n );" << endl;
  oDataSet_h << "   records_who.resize( m );" << endl;
  oDataSet_h << "   records_who = undefinedValue<double>();" << endl;
  oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      records_who[ getRecordIndex( who, i ) ] = measurements_who[i];" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
                                                                                
  //------------
  // getPopSize()
  // ------------
  // Return the size of population.
  //
  oDataSet_h << "// Returns the population size." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "int DataSet<spk_ValueType>::getPopSize() const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return popSize;" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------
  // getN()
  // ------
  // Return SPK's N (ie. N[i] is the number of measurements of the i-th individual.
  //
  oDataSet_h << "// Return SPK's N (ie. N[i] is the number of measurements of the i-th individual." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "const SPK_VA::valarray<int> DataSet<spk_ValueType>::getN() const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return NObservs;" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // -------------
  // getNObservs()
  // Return the number of observation records for the entire population.
  // -------------
  oDataSet_h << "// Return the number of measurements (DVs) for the entire population." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "const SPK_VA::valarray<int> DataSet<spk_ValueType>::getNObservs() const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return NObservs;" << endl;
  oDataSet_h << "}" << endl;

  // -------------
  // getNObservs(i)
  // Return the i-th individual's number of observation records.
  // -------------
  oDataSet_h << "// Return the number of measurements (DVs) of the i-th individual." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "int DataSet<spk_ValueType>::getNObservs( int i ) const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return data[i]->getNObservs();" << endl;
  oDataSet_h << "}" << endl;

  // -------------
  // getNRecords()
  // Return the number of total records for the entire population.
  // -------------
  oDataSet_h << "// Return the number of data records (including MDV=1) for the entire population." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "const SPK_VA::valarray<int> DataSet<spk_ValueType>::getNRecords() const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return NRecords;" << endl;
  oDataSet_h << "}" << endl;

  // -------------
  // getNRecords(i)
  // Return the i-th individual's number of total records.
  // -------------
  oDataSet_h << "// Return the number of data records (including MDV=1) of the i-th individual." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "int DataSet<spk_ValueType>::getNRecords( int i ) const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return data[i]->getNRecords();" << endl;
  oDataSet_h << "}" << endl;

  // ------------------------
  // replaceAllMeasurements()
  // ------------------------
  // Replace the currently kept y with the given y'.
  //
  oDataSet_h << "// Replace the currently kept y with the given y'." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceAllMeasurements( const SPK_VA::valarray<double> & yy )" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n= data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceMeasurements( yy[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "   measurements = yy;" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;


  // ------------------------
  // replacePred()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replacePred( const SPK_VA::valarray<double>& PredIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replacePred( PredIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceRes( const SPK_VA::valarray<double>& ResIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceRes( ResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceWRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceWRes( const SPK_VA::valarray<double>& WResIn )"    << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceWRes( WResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceIPred()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceIPred( const SPK_VA::valarray<double>& iPredIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceIPred( iPredIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceIRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceIRes( const SPK_VA::valarray<double>& iResIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceIRes( iResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceIWRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceIWRes( const SPK_VA::valarray<double>& iWResIn )"    << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceIWRes( iWResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replacePPred()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replacePPred( const SPK_VA::valarray<double>& pPredIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replacePPred( pPredIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replacePRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replacePRes( const SPK_VA::valarray<double>& pResIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replacePRes( pResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replacePWRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replacePWRes( const SPK_VA::valarray<double>& pWResIn )"    << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replacePWRes( pWResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceCPred()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceCPred( const SPK_VA::valarray<double>& cPredIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceCPred( cPredIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceCRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceCRes( const SPK_VA::valarray<double>& cResIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceCRes( cResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceCWRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceCWRes( const SPK_VA::valarray<double>& cWResIn )"    << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=NRecords[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceCWRes( cWResIn[ SPK_VA::slice(k, NRecords[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  if( getTarget() == POP )
    {
      // ------------------------
      // replaceEta()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceEta( const SPK_VA::valarray<double>& etaIn )"  << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( etaIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceEta( etaIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceEtaRes( const SPK_VA::valarray<double>& EtaResIn )"  << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( EtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceEtaRes( EtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceWEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceWEtaRes( const SPK_VA::valarray<double>& WEtaResIn )" << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( WEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceWEtaRes( WEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceIEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceIEtaRes( const SPK_VA::valarray<double>& iEtaResIn )"  << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( iEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceIEtaRes( iEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceIWEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceIWEtaRes( const SPK_VA::valarray<double>& iWEtaResIn )" << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( iWEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceIWEtaRes( iWEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replacePEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replacePEtaRes( const SPK_VA::valarray<double>& pEtaResIn )"  << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( pEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replacePEtaRes( pEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replacePWEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResIn )" << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( pWEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replacePWEtaRes( pWEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceCEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceCEtaRes( const SPK_VA::valarray<double>& cEtaResIn )"  << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( cEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceCEtaRes( cEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceCWEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResIn )" << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( cWEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceCWEtaRes( cWEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;
    }

  // ---------
  // Extractor
  // ---------
  oDataSet_h << "// Extracts the contents of this class object in the SpkResultML::presentation_data form." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "std::ostream& operator<<( std::ostream& o, const DataSet<spk_ValueType>& A )" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   using std::endl;" << endl;

  // Compute the number of items that are supposed to be printed out in <presentation_data>.
  // This includes User defined scalar variables, NONMEM (scalar/vector/matrix)
  // and RES/WRES.
  // 
  // THETA/ETA/OMEGA/(EPS)/(SIGMA) are vectors or matrices.
  // <presentation_data> prints out elements of these objects one by one.
  // That is, if THETA had a length of 2, it prints out THETA(1) and THETA(2) individually.
  // The names such as "THETA" is already in the symbol table, so when we get the size of
  // SymbolTable object, it returns the number that already contains the count for "THETA".
  // So, we increment the country by (2-1)=1. 
  const int nItems = t->size();
  int nColumns = nItems
    + myThetaLen-1
    + myEtaLen-1 
    + (getTarget()==POP? (myEpsLen - 1) : 0 ) // for EPS
    + (getTarget()==POP? (myEtaLen - 1) : 0 ) // for ETARES
    + (getTarget()==POP? (myEtaLen - 1) : 0 ) // for WETARES
    + (getTarget()==POP? (myEtaLen - 1) : 0 ) // for IETARES
    + (getTarget()==POP? (myEtaLen - 1) : 0 ) // for IWETARES
    + (getTarget()==POP? (myEtaLen - 1) : 0 ) // for PETARES
    + (getTarget()==POP? (myEtaLen - 1) : 0 ) // for PWETARES
    + (getTarget()==POP? (myEtaLen - 1) : 0 ) // for CETARES
    + (getTarget()==POP? (myEtaLen - 1) : 0 ) // for CWETARES
    - (table->find(nonmem::OMEGA)   == Symbol::empty()? 0 : 1 )
    - (table->find(nonmem::SIGMA)   == Symbol::empty()? 0 : 1 );
 
  if( myCompModel )
    {
      const Symbol* s;
      if( ( s = table->find( nonmem::DADT ) ) != Symbol::empty() )
	{
	  if( s->object_type == Symbol::VECTOR )
	    nColumns += myCompModel->getNCompartments()-1;
	}
      if( ( s = table->find( nonmem::A ) ) != Symbol::empty() )
	{
	  if( s->object_type == Symbol::VECTOR )
	    nColumns += myCompModel->getNCompartments()-1;
	}
      if( ( s = table->find( nonmem::P ) ) != Symbol::empty() )
	{
	  if( s->object_type == Symbol::VECTOR )
	    nColumns += myCompModel->getNParameters()-1;
	}
      if( ( s = table->find( nonmem::T ) ) != Symbol::empty() )
	{
	  nColumns--;
	}
    }

  map<const string, Symbol>::const_iterator pEntry = t->begin();
  const vector<string>::const_iterator pLabelBegin = table->getLabels()->begin();
  const vector<string>::const_iterator pLabelEnd   = table->getLabels()->end();
  vector<string> whatGoesIn;  // will hold those labels in the order that actually go into the data section.
  vector<string>::const_iterator pWhatGoesIn;

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Open <presentation_data>
  //
  oDataSet_h << "   o << \"<\" << \"presentation_data\" << \" rows=\\\"\" << A.NRecords.sum() << \"\\\" \";" << endl;
  oDataSet_h << "   o << \"columns=\\\"" << nColumns << "\\\">\" << endl;" << endl;

  //-----------------------------------------------------------------------------
  // Begin printing out labels.
  //
//  oDataSet_h << "   o << \"<data_labels>\" << endl;" << endl;
  
  // Put ID first in the sequence
  whatGoesIn.push_back( pID->name );
//  oDataSet_h << "   o << \"<label name=\\\"" << pID->name << "\\\"/>\" << endl;" << endl;
  oDataSet_h << "   o << \"ID";
  
  // ...aaand, following ID is, all the left hand side quantities in the model definition.
  // cntColumns is initialized to 1 because the ID column is already printed out.
  int cntColumns = 1;
  for( cntColumns=1,  pEntry = t->begin(); pEntry!=t->end(); pEntry++ )
    {
      if( pEntry->first != nonmem::ID )
	{
	  // Skip Omega and Sigma.
          // These values are not computed by Pred::eval().
	  if(    pEntry->first != nonmem::OMEGA 
		 && pEntry->first != nonmem::SIGMA )
	    {
	      whatGoesIn.push_back( pEntry->second.name );
	      
	      // theta: This is a vector.  So, all element values have to be printed out individually.
	      if( pEntry->first == nonmem::THETA )
		{
		  for( int cntTheta=0; cntTheta<myThetaLen; cntTheta++ )
		    {
                      oDataSet_h << ",";
//		      oDataSet_h << "   o << \"<label name=\\\"";
		      oDataSet_h << pEntry->second.name << "(" << cntTheta+1 << ")";
//		      oDataSet_h << "\\\"/>\" << endl;" << endl;
		      cntColumns++;
		    }
		}
	      // eta, etares, wetares: These are vectors of length myEtaLen.
              // So, all elements values have to be printed out individually.
	      else if( pEntry->first == nonmem::ETA
		       || pEntry->first == nonmem::ETARES 
		       || pEntry->first == nonmem::WETARES
		       || pEntry->first == nonmem::IETARES 
		       || pEntry->first == nonmem::IWETARES
		       || pEntry->first == nonmem::PETARES 
		       || pEntry->first == nonmem::PWETARES
		       || pEntry->first == nonmem::CETARES 
		       || pEntry->first == nonmem::CWETARES )
		{
		  for( int cntEta=0; cntEta<myEtaLen; cntEta++ )
		    {
                      oDataSet_h << ",";
//		      oDataSet_h << "   o << \"<label name=\\\"";
		      oDataSet_h << pEntry->second.name << "(" << cntEta+1 << ")";
//		      oDataSet_h << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      // eps: This is a vector of length myEpsLen.
              // So, all elements values have to be printed out individually.
	      else if( pEntry->first == nonmem::EPS )
		{
		  for( int cntEps=0; cntEps<myEpsLen; cntEps++ )
		    {
                      oDataSet_h << ",";
//		      oDataSet_h << "   o << \"<label name=\\\"";
		      oDataSet_h << pEntry->second.name << "(" << cntEps+1 << ")";
//		      oDataSet_h << "\\\"/>\" << endl;" << endl;
		      cntColumns++;
		    }
		}
	      // DADT or A: This is a vector of length nCompartments
	      else if( (pEntry->first == nonmem::DADT || pEntry->first == nonmem::A )
		       && myCompModel ) 
		{
		  for( int cnt=0; cnt<myCompModel->getNCompartments(); cnt++ )
		    {
                      oDataSet_h << ",";
//		      oDataSet_h << "   o << \"<label name=\\\"";
		      oDataSet_h << pEntry->second.name << "(" << cnt+1 << ")";
//		      oDataSet_h << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      // P: This is a vector of length nParameters
	      else if( pEntry->first == nonmem::P && myCompModel )
		{
		  for( int cnt=0; cnt<myCompModel->getNParameters(); cnt++ )
		    {
                      oDataSet_h << ",";
//		      oDataSet_h << "   o << \"<label name=\\\"";
		      oDataSet_h << pEntry->second.name << "(" << cnt+1 << ")";
//		      oDataSet_h << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}

	      // T should be omitted from print out if it means the ODE's T.
	      else if( myCompModel && (pEntry->first == nonmem::T) ) 
		{
		  // ignore
		}

	      // scalar variables (user-defined variables & NONMEM reserved variables).
	      else
		{
                  oDataSet_h << ",";
//		  oDataSet_h << "   o << \"<label name=\\\"";
		  oDataSet_h << pEntry->second.name;
//		  oDataSet_h << "\\\"/>\" << endl;" << endl;
		  cntColumns++;
		}
	    }
	}
    }

  // Sanity check; is the actual number of labels (ie. columns) match the value given by the user earlier?

  if( cntColumns != nColumns )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"The number of data items (%d) does not match the number of labels (%d).",
		cntColumns, nColumns );
      SpkCompilerException e( SpkCompilerError::ASPK_USER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

//  oDataSet_h << "   o << \"</data_labels>\" << endl;" << endl;
  oDataSet_h << "\" << endl;" << endl;
  //
  // End of labels.
  //-----------------------------------------------------------------------------
  
  //-----------------------------------------------------------------------------
  // Begin printing out computed values.
  // 
  oDataSet_h << "   for( int i=0, position=1; i<A.getPopSize(); i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      for( int j=0; j<A.NRecords[i]; j++, position++ )" << endl;
  oDataSet_h << "      {" << endl;
//  oDataSet_h << "         o << \"<row position=\\\"\" << position << \"\\\">\" << endl;" << endl;

  for( cntColumns=0, pWhatGoesIn = whatGoesIn.begin(); pWhatGoesIn!=whatGoesIn.end(); pWhatGoesIn++ )
    {
      if( *pWhatGoesIn == nonmem::THETA )
	{
	  for( int cntTheta=0; cntTheta<myThetaLen; cntTheta++ )
	    {
              oDataSet_h << " << \",\" << ";
//	      oDataSet_h << "         o << \"<value ref=\\\"";
//	      oDataSet_h << *pWhatGoesIn << "(" << cntTheta+1 << ")" << "\\\"" << ">\" << ";
	      oDataSet_h << "A.data[i]->" << *pWhatGoesIn << "[j][" << cntTheta << "]";
//	      oDataSet_h << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( *pWhatGoesIn == nonmem::ETA
	       || *pWhatGoesIn == nonmem::ETARES
	       || *pWhatGoesIn == nonmem::WETARES
	       || *pWhatGoesIn == nonmem::IETARES
	       || *pWhatGoesIn == nonmem::IWETARES
	       || *pWhatGoesIn == nonmem::PETARES
	       || *pWhatGoesIn == nonmem::PWETARES
	       || *pWhatGoesIn == nonmem::CETARES
	       || *pWhatGoesIn == nonmem::CWETARES )
	{
	  for( int cntEta=0; cntEta<myEtaLen; cntEta++ )
	    {
              oDataSet_h << " << \",\" << ";
//	      oDataSet_h << "         o << \"<value ref=\\\"";
//	      oDataSet_h << *pWhatGoesIn << "(" << cntEta+1 << ")"<< "\\\"" << ">\" << ";
	      oDataSet_h << "A.data[i]->" << *pWhatGoesIn << "[j][" << cntEta << "]";
//	      oDataSet_h << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( *pWhatGoesIn == nonmem::EPS && getTarget() == POP )
	{
	  for( int cntEps=0; cntEps<myEpsLen; cntEps++ )
	    {
              oDataSet_h << " << \",\" << ";
//	      oDataSet_h << "         o << \"<value ref=\\\"";
//	      oDataSet_h << *pWhatGoesIn << "(" << cntEps+1 << ")"<< "\\\"" << ">\" << ";
	      oDataSet_h << "A.data[i]->" << *pWhatGoesIn << "[j][" << cntEps << "]";
//	      oDataSet_h << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( *pWhatGoesIn == nonmem::OMEGA || *pWhatGoesIn == nonmem::SIGMA )
	{
	  // ignore
	}
      else if( ( *pWhatGoesIn == nonmem::DADT || *pWhatGoesIn == nonmem::A ) 
	       && myCompModel )
	{
	  for( int cnt=0; cnt<myCompModel->getNCompartments(); cnt++ )
	    {
              oDataSet_h << " << \",\" << ";
//	      oDataSet_h << "         o << \"<value ref=\\\"";
//	      oDataSet_h << *pWhatGoesIn << "(" << cnt+1 << ")"<< "\\\"" << ">\" << ";
	      oDataSet_h << "A.data[i]->" << *pWhatGoesIn << "[j][" << cnt << "]";
//	      oDataSet_h << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( *pWhatGoesIn == nonmem::P && myCompModel )
	{
	  for( int cnt=0; cnt<myCompModel->getNParameters(); cnt++ )
	    {
              oDataSet_h << " << \",\" << ";
//	      oDataSet_h << "         o << \"<value ref=\\\"";
//	      oDataSet_h << *pWhatGoesIn << "(" << cnt+1 << ")"<< "\\\"" << ">\" << ";
	      oDataSet_h << "A.data[i]->" << *pWhatGoesIn << "[j][" << cnt << "]";
//	      oDataSet_h << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( myCompModel && (*pWhatGoesIn == nonmem::T) )
        {
          // ignore
        }
      else 
	{
          string s(*pWhatGoesIn);
          if(s.compare(0, 2, "ID") == 0) oDataSet_h << "         o << ";
          else oDataSet_h << " << \",\" << ";
//	  oDataSet_h << "         o << \"<value ref=\\\"" << *pWhatGoesIn << "\\\"" << ">\" << ";
	  oDataSet_h << "A.data[i]->" << *pWhatGoesIn << "[j]";
//	  oDataSet_h << " << \"</value>\" << endl;" << endl;
	  cntColumns++;
	}
    }
  if( cntColumns != nColumns )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"The number of data items (%d), does not the number of labels (%d).",
		cntColumns, nColumns );
      SpkCompilerException e( SpkCompilerError::ASPK_USER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
//  oDataSet_h << "         o << \"</row>\" << endl;" << endl;
  oDataSet_h << " << endl;" << endl;
  oDataSet_h << "      }" << endl;
  oDataSet_h << "   }" << endl;
  //
  // End of computed values.
  //-----------------------------------------------------------------------------
  
  oDataSet_h << "   o << \"</\" << \"presentation_data\" << \">\";" << endl;
  //
  // Close <presentation_data>
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  oDataSet_h << "};" << endl;


  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "std::ostream& operator<< ( std::ostream& o, const DataSet<spk_ValueType>& A );" << endl;
  oDataSet_h << "#endif" << endl;
  oDataSet_h.close();
}
