/**
 * @file CompModelInfo.h
 * Decleare the CompModelInfo class.
 */
#ifndef COMPMODELINFO_H
#define COMPMODELINFO_H

#include "CompartmentInfo.h"
#include <vector>

/**
 * Representation of a compartmental model.
 */
class CompModelInfo{

  public:
  /**
   * Destructor
   */
  ~CompModelInfo();

  /**
   * Copy constructor.
   */
  CompModelInfo( const CompModelInfo& );

  /**
   * Assignment operator.
   */
  CompModelInfo& operator=( const CompModelInfo& );

  /**
   * Constructor.  Initialize the model with the given attribute values and dummy compartments.
   *
   * @param nCompartmentsIn The number of compartments in this system.
   * @param nParametersIn   The number of basic PK parameters.
   * @param nEquilibrimsIn  The number of quilibrim compartments.
   * @param relTolIn        The relative tolerance (10^-(TOL+1)) where TOL is the #of sigs user provide.
   */
  CompModelInfo( int nCompartmentsIn, 
		 int nParametersIn, 
		 int nEquilibrimsIn,
		 double relTolIn );

  /**
   * Get the number of compartments in this system including the output comp.
   */
  int getNCompartments() const;

  /**
   * Get the number of basic PK parameters associated with this system.
   */
  int getNParameters() const;

  /**
   * Get the number of equilibrim compartments in this system.
   */
  int getNEquilibrims() const;
 
  /**
   * True if the PK block is a function of T (continuous time).
   */
  bool isPkFunctionOfT() const;

  /**
   * Set as to whether the PK block is a function of T or not.
   */
  void setPkFunctionOfT( bool );
  
  /**
   * Returns the number of compartment that is set to receive dose.
   * The first compartment (=1) is set by default.
   */
  int getDefaultDose() const;

  /**
   * Set the relative tolerance.
   */
  void setRelTol( double );

  /**
   * Returns the relative tolerance.
   */
  double getRelTol() const;

  /**
   * Returns the number of compartment that is set to be the
   * observation compartment.  The first compartment (=1)
   * is set by default.
   */
  int getDefaultObservation() const;

  /**
   * Returns a vector of boolean values where the i-th element
   * indicates as to whether
   * the corresponding compartment is to be turned off initially.
   * True indicates turn-off.
   */
  void getInitialOff( std::vector<bool>& initial_off ) const;

  /**
   * Returns a vector of boolean values where the i-th element
   * indicates as to whether the corresponding compartment
   * can be turn off or on.  True indicates no turn off.
   */
  void getNoOff( std::vector<bool>& no_off ) const;

  /**
   * Returns a vector of boolean values, where the i-th element
   * indicates as to whether the corresponding compartment
   * can receive dose or not.  True indicates no dose.
   */
  void getNoDose( std::vector<bool>& no_dose ) const;

  /**
   * Get the constant reference to the i-th CompartmentInfo object.
   * 
   * @param i The number/order of the object (>=0).
   */
  const CompartmentInfo& operator[]( int i ) const;

  /**
   * Get the constant reference to the i-th CompartmentInfo object.
   * @param i The number/order of the object (>=0).
   */
  const CompartmentInfo& getCompartment( int i ) const;

  /**
   * Get the non-const reference to the i-th CompartmentInfo object.
   * @param i The number/order of the object.
   */
  CompartmentInfo& operator[]( int i );

  /**
   * Get the non-const reference to the i-th CompartmentInfo object.
   * @param i The number/order of the object.
   */
  CompartmentInfo& getCompartment( int i );

  protected:
  /**
   * (Prohibited) Default constructor.
   */
  CompModelInfo();

  private:

  /** The number of compartments */
  int nCompartments;

  /** The number of PK parameters 
   * @note for developers: We don't really understand what this number stands for at this point.
   *                       The value is not currently used.
   */
  int nParameters;

  /**
   * The number of equilibrim compartments.
   */
  int nEquilibrims;

  /**
   * The relative tolerance.
   */
  double relTol;
  
  /**
   * A flag indicating as to whether the PK model is a function of T.
   * true means it is a function of T.
   */
  bool is_pkFunctionOfT;

  /**
   * The internal list of compartment objects.
   */
  std::vector<CompartmentInfo> compartments;
};

#endif
