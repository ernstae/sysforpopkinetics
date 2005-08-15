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
   * The model is assumed to contain no equilibrim compartment.
   *
   * @param nCompartmentsIn The number of compartments in this system.
   * @param nParametersIn   The number of basic PK parameters.
   */
  CompModelInfo( int nCompartmentsIn, int nParametersIn );
  /**
   * Constructor.  Initialize the model with the given attribute values.
   * The model is assumed to contain no equilibrim compartment.
   *
   * @param nCompartmentsIn The number of compartments in this system.
   * @param nParametersIn   The number of basic PK parameters.
   * @param compartmentsIn  Compartments making up the system.
   */
  CompModelInfo( int nCompartmentsIn, int nParametersIn, const std::vector<CompartmentInfo>& compartmentsIn );

  /**
   * Constructor.  Initialize the model with the given attribute values and dummy compartments.
   *
   * @param nCompartmentsIn The number of compartments in this system.
   * @param nParametersIn   The number of basic PK parameters.
   * @param nEquilibrimIn   The number of quilibrim compartments.
   */
  CompModelInfo( int nCompartmentsIn, int nParametersIn, int nEquilibrimsIn );

  /**
   * Constructor.  Initialize the model with the given attribute values.
   *
   * @param nCompartmentsIn The number of compartments in this system.
   * @param nParametersIn   The number of basic PK parameters.
   * @param nEquilibrimIn   The number of quilibrim compartments.
   * @param compartmentsIn  Compartments making up the system.
   */
  CompModelInfo( int nCompartmentsIn, int nParametersIn, int nEquilibrimsIn, const std::vector<CompartmentInfo>& compartmentsIn );

  /**
   * Get the number of compartments in this system.
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
  bool is_pkFunctionOfT() const;

  /**
   * Set as to whether the PK block is a function of T or not.
   */
  void setPkFunctionOfT( bool );
  
  /**
   * Returns the number of compartment that is set to receive dose.
   * The first compartment (=0) is set by default.
   */
  int getDefaultDose() const;

  /**
   * Returns the number of compartment that is set to be the
   * observation compartment.  The first compartment (=0)
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
  /**
   * Get the i-th CompartmentInfo object.
   * @param i The number/order of the object.
   */
  CompartmentInfo& operator[]( int i );
  /**
   *
   */

  protected:
  CompModelInfo();

  private:
  int nCompartments;
  int nParameters;
  int nEquilibrims;
  bool isPkFunctionOfT;
  std::vector<CompartmentInfo> compartments;
};

#endif
