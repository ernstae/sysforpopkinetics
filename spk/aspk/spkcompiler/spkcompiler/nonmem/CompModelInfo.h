#ifndef COMPMODELINFO_H
#define COMPMODELINFO_H

#include "CompartmentInfo.h"
#include <vector>

/**
 * A representation of a compartmental model.
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
   * @param nEquilibrimIn   The number of quilibrim compartments.
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
   * Get the i-th Compartment object.
   * @param i The number/order of the object.
   */
  const CompartmentInfo& operator[]( int i ) const;
  const CompartmentInfo& getCompartment( int i ) const;
  /**
   * Get the i-th CompartmentInfo object.
   * @param i The number/order of the object.
   */
  CompartmentInfo& operator[]( int i );
  CompartmentInfo& getCompartment( int i );

  /**
   *
   */

  protected:
  CompModelInfo();

  private:
  int nCompartments;
  int nParameters;
  int nEquilibrims;
  double relTol;
  bool is_pkFunctionOfT;
  std::vector<CompartmentInfo> compartments;
};

#endif
