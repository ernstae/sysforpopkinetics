/**
 * @file PredBase.h
 * 
 * Declares PredBase class.
 */
#ifndef PREDBASE_H
#define PREDBASE_H

#include <vector>


/**
 * Abstract class prividing only the interfaces needed to evaluate
 * the user's PRED definition. 
 */
template <class Value>
class PredBase
{
public:
  /**
   * <code>eval</code> evaluates the model at the given independent variables
   * and predicts the i-th individual's j-th measurement.
   *
   * @param spk_thetaOffset The index to the head of THETA vector within spk_indepVar.
   * @param spk_thetaLen    The length of THETA vector.
   *                        The vector elements are assumed to be placed
   *                        from spk_indepVar[spk_thetaOffset] 
   *                        to spk_indepVar[spk_theetaOffset + spk_theetaLen].
   * @param spk_etaOffset   The index to the head of ETA vector within spk_indepVar.
   * @param spk_etaLen      The length of ETA vector.  
   *                        The vector elements are assumed to be placed
   *                        from spk_indepVar[spk_etaOffset] 
   *                        to spk_indepVar[spk_etaOffset + spk_etaLen].
   * @param spk_epsOffset   The index to the head of EPS vector within spk_indepVar.
   * @param spk_epsLen      The length of EPS vector.
   *                        The vector elements are assumed to be placed
   *                        from spk_indepVar[spk_thetaOffset] 
   *                        to spk_indepVar[spk_theetaOffset + spk_theetaLen].
   * @param spk_fOffset     The index to the memory location within spk_depVar in which
   *                        the computed <code>F</code> (ie. prediction) 
   *                        for the j-th measurement of the i-th individual
   *                        shall be placed.
   * @param spk_fLen        The total length of <code>F</code> vector.
   * @param spk_yOffset     The index to the memory location within spk_depVar in which
   *                        the computed <code>Y</code> (ie. data variance) 
   *                        for the j-th measurement of the i-th individual
   *                        shall be placed.
   * @param spk_yLen        The total length of <code>Y</code> vector.
   *
   * @param spk_i           The index to the individual of interest 
   *                        within the population (0 indicates the first individual).
   * @param spk_j           The index to the sampling point of interest.
   * @param spk_indepVar    The vector containing independent variables: THETA, ETA and EPS.
   * @param spk_depVar      (output) The vector whose spk_yOffset-th and spk_yOffset-th elements
   *                        are replaced with newly computed values.
   *
   * @return true           if the i-th individual's j-th data record was NOT marked MDV (ie. Missing Data Variable).
   * @return false          if the i-th individual's j-th data record WAS marked MDV.
   */
  virtual bool eval( int spk_thetaOffset, int spk_thetaLen,
		     int spk_etaOffset,   int spk_etaLen,
		     int spk_epsOffset,   int spk_epsLen,
		     int spk_fOffset,     int spk_fLen,
		     int spk_yOffset,     int spk_yLen,
		     int spk_i,
		     int spk_j,
		     const std::vector<Value>& spk_indepVar,
		     std::vector<Value>& spk_depVar ) = 0;
 protected:
  ~PredBase(){}
  PredBase(){}
  PredBase( const PredBase& ){}
  PredBase & operator=( const PredBase& ){}
};
#endif
