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
   * @param thetaOffset The index to the head of THETA vector within indepVar.
   * @param thetaLen    The length of THETA vector.
   *                        The vector elements are assumed to be placed
   *                        from indepVar[thetaOffset] 
   *                        to indepVar[thetaOffset + thetaLen].
   * @param etaOffset   The index to the head of ETA vector within indepVar.
   * @param etaLen      The length of ETA vector.  
   *                        The vector elements are assumed to be placed
   *                        from indepVar[etaOffset] 
   *                        to indepVar[etaOffset + etaLen].
   * @param epsOffset       The index to the head of EPS vector within indepVar.
   * @param epsLen          The length of EPS vector.
   *                        The vector elements are assumed to be placed
   *                        from indepVar[thetaOffset] 
   *                        to indepVar[thetaOffset + thetaLen].
   * @param fOffset         The index to the memory location within depVar in which
   *                        the computed <code>F</code> (ie. prediction) 
   *                        for the j-th measurement of the i-th individual
   *                        shall be placed.
   * @param fLen            The total length of <code>F</code> vector.
   * @param yOffset         The index to the memory location within depVar in which
   *                        the computed <code>Y</code> (ie. data variance) 
   *                        for the j-th measurement of the i-th individual
   *                        shall be placed.
   * @param yLen            The total length of <code>Y</code> vector.
   *
   * @param i               The index to the individual of interest 
   *                        within the population (0 indicates the first individual).
   * @param j               The index to the sampling point of interest.
   * @param indepVar        The vector containing independent variables: THETA, ETA and EPS.
   * @param depVar         (output) The vector whose yOffset-th and yOffset-th elements
   *                        are replaced with newly computed values.
   *
   * @return true           if the i-th individual's j-th data record was NOT marked MDV (ie. Missing Data Variable).
   * @return false          if the i-th individual's j-th data record WAS marked MDV.
   */
  virtual bool eval( int thetaOffset, int thetaLen,
		     int etaOffset,   int etaLen,
		     int epsOffset,   int epsLen,
		     int fOffset,     int fLen,
		     int yOffset,     int yLen,
		     int i,
		     int j,
		     const std::vector<Value>& indepVar,
		     std::vector<Value>& depVar ) = 0;

  /**
   * getNObservs( int i ) returns the number of observation records
   * for the i-th individual.  For the individual analysis,
   * i should be always 0.
   *
   * @param i The index to the individual of interest.
   *          0 indicates the first individual in the population.
   *          For the individual analysis, this value is ignored.
   * 
   * @return The number of observation records for the i-th individual.
   *         If the this function is called in the context of individual
   *         (only) analysis, then the number of the only individual's
   *         obervation records is returned.
   */
  virtual int getNObservs( int i ) const = 0;

public:
  virtual ~PredBase(){}

protected:
  PredBase(){}
  PredBase( const PredBase& ){}
  PredBase & operator=( const PredBase& ){}
};
#endif
