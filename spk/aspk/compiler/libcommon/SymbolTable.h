#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <map>
#include <valarray>
#include <string>
#include "Symbol.h"
#include "client.h"

/**
 * Approximation methods applied during the population parameter estimation process.
 *
 * This enumulator is already defined in Objective.h in Spk.lib.
 * It is repeated here until the library is ported to Linux.
 */
enum Objective {LAPLACE, FOCE, FO};

/**
 * A bundle of SPK (fitPopulation) required values.
 *
 * This structure bundles all these values required to fun fitPopulation().
 * It also maps the symbols used within SPK to describe these values to the symbols
 * used within the client space.
 */
struct FitParameters{
  enum Objective        objective;
  int                   nIndividuals;
  std::valarray<int>    nMeasurementsAll;
  std::valarray<double> measurementsAll;
  std::valarray<double> popParIn, popParLow, popParUp, popParStep;
  double                popEpsilon;
  int                   popMaxItr;
  int                   popTrace;
  bool                  isPopWarmStart;
  bool                  isPopParOut;
  bool                  isPopObjOut, isPopObj_popParOut, isPopObj_popPar_popParOut;  
  std::valarray<double> indParIn, indParLow, indParUp, indParStep;
  double                indEpsilon;
  int                   indMaxItr;
  int                   indTrace;
  bool                  isIndWarmStart;
  bool                  isIndParOut;
  bool                  isIndObjOut, isIndObj_indParOut, isIndObj_indPar_indParOut;  

  bool                  isPopStderrorOut;
  bool                  isPopCorrelationOut;
  bool                  isPopCovarianceOut;
  bool                  isPopCoefficientOut;
  bool                  isPopConfidenceOut;
  
  bool                  isIndStderrorOut;
  bool                  isIndCorrelationOut;
  bool                  isIndCovarianceOut;
  bool                  isIndCoefficientOut;
  bool                  isIndConfidenceOut;
  
  std::map< std::string, std::valarray<double> > aliases;
};

/**
 * @file SymbolTable.h
 *
 * Declares SymbolTable class
 */
/**
 * SymbolTable maintains a symbol table that is the central place where information 
 * about variables/functions appeared in source files are gathered and maintained and
 * provide methods for the caller to manipulate the table.
 *
 * The table is implemented using std::map.  It is a set of (unique) key-entry pairs.
 * The keys are null termimated arrays of characters.  Each key reflects a name discovered 
 * and associated with a Symbol object which contains information such as data type and structure 
 * related to the name.
 */
class SymbolTable{
  
 public:
  /**
   * Default constructor
   * @param whoIn is an client enum indicating the type of client.
   */
  SymbolTable( client::type whoIn );

  /**
   * Destructor
   */
  ~SymbolTable();

  /**
   * A data structure composed of SPK required variables.
   */
  struct FitParameters *spkSymbols;

  /**
   * Searches an entry matches to the name.
   *
   * @param name is the user-defined symbol to be sought.
   *
   * @return a pointer to the Symbol object when a match is found in the table.
   * The pointer value itself may not be modified but the values pointed by it can be modified.
   * If no match is found, it returns NULL.
   */
  Symbol * const find( const std::string& name );

  /**
   * Inserts a Symbol object to the table.
   *
   * @param symbol is a Symbol object associated with a user-defined name (symbol).  
   * If an  entry with the same symbol name
   * already exists, the new Symbol object replaces the old.
   * 
   * @return a pointer to the Symbol object if the symbol were new to the table.  
   * NULL if the symbol was found.
   * The pointer value itself may not be modified while the values associated 
   * with the Symbol object may be modified.
   * 
   */
  Symbol * const insert( Symbol & symbol );

  /**
   * Inserts a user-defined symbol to the table.
   *
   * @param uName is a user-defined name (symbol) to be registered.
   * It creates an Symbol object with the name and is entered in the table.
   * If an entry with the same symbol name already exists, the new symbol replaces the old.
   * 
   * @return a pointer to a newly created Symbol object if the symbol were new to the table.  
   * NULL if the symbol was found.
   * The pointer value itself may not be modified while the values associated with 
   * the Symbol object may be modified.
   */
  Symbol * const insert( const std::string& uName );

  /**
   * Determine whether there is any user defined symbol registered at all.
   *
   * @return true if there is at least one user defined symbol in the table.
   */
  inline bool empty() const { if( userSymbols.size() < 1 ) return true; else return false; }

  /**
   * Determine the number of user defined symbols.
   *
   * @return the number of user defined symbols..
   */
  inline int size() const{ return userSymbols.size(); }

  /**
   * Dumps the contents of the table to the standard output.
   */
  void dump() const;

 protected:
  SymbolTable(){};
  SymbolTable( const SymbolTable& ){};
  SymbolTable & operator=( const SymbolTable& ){};


 private:
  client::type who;

  typedef std::map < std::string, Symbol > UserSymbols;
  UserSymbols userSymbols;

};

#endif
