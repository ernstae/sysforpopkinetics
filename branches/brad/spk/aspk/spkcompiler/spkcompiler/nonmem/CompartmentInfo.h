/**
 * @file CompartmentInfo.h
 * Declare the CompartmentInfo class.
 */
#ifndef COMPARTMENTINFO_H
#define COMPARTMENTINFO_H
#include <string>
/**
 * A representation of a compartment.
 * 
 * A compartment has the following boolean attributes:
 *                                                                                                                                 
 * INITIALOFF     // If true, this compartment is initially off.
 * NOOFF          // If true, this compartment may not be turned on or off.
 * NODOSE         // If true, this compartment may not receive a dose.
 * EQUILIBRIM     // If true, this compartment is an equilibrim compartment (ADVAN9 only: implies NODOSE).
 * EXCLUDE        // If true, this compartment's amount is excluded from the amount of the output compartment (ADVAN9 only).
 * DEFOBSERVATION // If true, this compartment is the default observation compartment.
 * DEFDOSE        // if true, this compartment is the default dose compartment.
 */
class CompartmentInfo{
  public:
  /**
   * Destructor
   */
  ~CompartmentInfo();

  /**
   * Default constructor.  Initialize the compartment with the default attribute values.
   *
   * NAME           = "dummy"
   * INITIALOFF     = false // this compartment is initially on.
   * NOOFF          = false // this compartment may be turned on or off.
   * NODOSE         = false // this compartment may receive a dose.
   * EQUILIBRIM     = false // this compartment is not an equilibrim compartment.
   * EXCLUDE        = false // this compartment's amount contributes to the output compartment.
   * DEFOBSERVATION = false // this compartment is not the default observation compartment.
   * DEFDOSE        = false // this compartment is not the default dose compartment.
   */
  CompartmentInfo();

  /**
   * Constructor.  Initialize the compartment witht the default attribute values:
   *
   * INITIALOFF     = false // this compartment is initially on.
   * NOOFF          = false // this compartment may be turned on or off.
   * NODOSE         = false // this compartment may receive a dose.
   * EQUILIBRIM     = false // this compartment is not an equilibrim compartment.
   * EXCLUDE        = false // this compartment's amount contributes to the output compartment.
   * DEFOBSERVATION = false // this compartment is not the default observation compartment.
   * DEFDOSE        = false // this compartment is not the default dose compartment.
   * 
   * @param nameIn The name for this compartment.
   *
   */
  CompartmentInfo( const std::string& nameIn );

  /**
   * Constructor.  Initialize the compartment with the given attribute values:
   *
   * INITIALOFF     = is_initial_offIn
   * NOOFF          = is_no_offIn
   * NODOSE         = is_doseIn
   * EQUILIBRIM     = is_equilibrim
   * EXCLUDE        = is_excluded
   * DEFOBSERVATION = is_default_observation
   * DEFDOSE        = is_default_dose
   * 
   * @param nameIn The name for this compartment.
   * @param is_initial_offIn         Sets INITIALOFF.      If true, this compartment is initially off.
   * @param is_no_offIn              Sets NOOFF.           If true, this compartment may not be turned on and off.
   * @param is_no_doseIn             Sets NODOSE.          If true, this compartment may not receive a dose.
   * @param is_equilibrimIn          Sets EQUILIBRIM.      If true, this compartment is an equilibrim compartment 
   *                                 (ADVAN9 only: implies NODOSE).
   * @param is_excludedIn            Sets EXCLUDE          If true, this compartment's amount is excluded 
   *                                 from the output compartment (ADVAN9 only).
   * @param is_default_observationIn Sets DEFOBSERVATION.  If true, this compartment is the default observation compartment.
   * @param is_default_doseIn        Sets DEFDOSE.         If true, this compartment is the default dose compartment.
   */
  CompartmentInfo( const std::string& nameIn,
               bool is_initial_offIn,
               bool is_no_offIn,
               bool is_no_doseIn,
               bool is_equilibrimIn,
               bool is_excludedIn,
               bool is_default_observationIn,
               bool is_default_doseIn );

  /**
   * Copy constructor.
   */
  CompartmentInfo( const CompartmentInfo& );

  /**
   * Assignment operator.
   */
  CompartmentInfo& operator=( const CompartmentInfo& );

  /**
   * Get the name of this compartment.
   */
  const std::string getName() const;

  /**
   * Determine if this compartment is set initially off.
   * @return true if it is set initially off.
   */
  bool is_initial_off() const;

  /**
   * Determine if this compartment may not be turned on or off.
   * @return true if it may not be turned on or off.
   */
  bool is_no_off() const;

  /**
   * Determine if this compartment may not receive a dose.
   * @return true if it may not receive a dose.
   */
  bool is_no_dose() const;

  /**
   * Determine if this compartment is an equilibrim compartment.
   * @return true if it is an equilibrim compartment.
   */
  bool is_equilibrim() const;

  /**
   * Determine if this compartment's amount is excluded from the output compartment amount.
   * @return true if its amount is to be excluded.
   */
  bool is_exclude() const;

  /**
   * Determine if this compartment is the default observation compartment.
   * @return true if it is the default observation compartment.
   */
  bool is_default_observation() const;

  /**
   * Determine if this compartment is the default dose compartment.
   * @return true if it is the deafult dose compartment.
   */
  bool is_default_dose() const;
  /**
   * Set the name for this compartment.
   * @param nameIn A new compartment name.
   */
  void setName                ( const std::string& nameIn );
  /**
   * Turn this compartment initially turned off or on.
   * @param initial_offIn true turns this compartment initially off.
   */
  void set_initial_off        ( bool initial_offIn );

  /**
   * Prohibit or allow this compartment to be turned on or off.
   * @param no_offIn true prohibits this compartment from being turned on or off.
   */
  void set_no_off             ( bool no_offIn );

  /**
   * Prohibit or allow this compartment to receive a dose.
   * @param no_doseIn true prohibits this compartment from receiveing a dose.
   */
  void set_no_dose            ( bool no_doseIn );

  /**
   * Set/reset this compartment to be an equilibrim compartment.
   * @param equilibrimIn true makes this compartment an equilibrim compartment.
   */
  void set_equilibrim         ( bool equilibrimIn );

  /**
   * Set/reset this compartment's amount to be excluded from the output compartment.
   * @param excludedIn true makes this compartment's amount excluded.
   */
  void set_exclude            ( bool excludedIn );
  /**
   * Set/reset this compartment to be the default observation compartment.
   * @param default_observationIn true makes this compartment the default observation compartment.
   */
  void set_default_observation( bool default_observationIn );

  /**
   * Set/reset this compartment to be the default dose compartment.
   * @param default_doseIn true makes this compartment the default dose compartment.
   */
  void set_default_dose       ( bool default_doseIn );

  protected:

  private:

  /** The label for the compartment.  */
  std::string name;

  /** When this flag is true, this compartment is marked so that it is set initially OFF. */
  bool initial_off;

  /** When this flag is true, this compartment is marked so that it is never turned off. */
  bool no_off;

  /** When this flag is true, this compartment is marked so that it never receive a dose. */
  bool no_dose;

  /** When this flag is true, this compartment is considered at quilibrim.  Valid only for ADVAN 9.*/
  bool equilibrim;  // ADVAN 9 only

  /** When this flag is true, this compartment is considered excluded from the system.  Valid only for ADVAN 9. */
  bool exclude;     // ADVAN 9 only
  /** When this flag is true, observations are taken from this compartment by default. */
  bool default_observation;

  /** When this flag is true, doses are given to this compartment by default. */
  bool default_dose;
};


#endif
