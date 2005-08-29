/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
/*************************************************************************
 *//**
 * @file PredBase.h
 * 
 * 
 * Declares PredBase class.
 *//*
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef PREDBASE_H
#define PREDBASE_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <vector>


/*************************************************************************
 *
 * Class: PredBase
 *
 *//**
 * This abstract base class provides the interfaces needed to evaluate
 * Pred block based models.
 *//*
 *************************************************************************/

template <class Value>
class PredBase
{
public:
  /**
   * <code>eval</code> evaluates the Pred block expressions for the
   * i-th individual's j-th data record at the given values for the
   * independent variables.
   *
   * If the Dependent Variable (DV) data item for this data record is
   * not missing, i.e., its Missing Dependent Variable (MDV) data item
   * is equal to zero, then the calculated value for <code>F</code>
   * and the calculated value for <code>Y</code> will be set in the
   * vector of dependent variables.
   *
   * The variable <code>F</code> is the model for the expected value
   * for the DV data item, and the variable <code>Y</code> is the full
   * statistical model for the DV that includes the noise in the data.
   *
   * This function should set the value for DV, and it should also set
   * the values for all of the other data items that appear in the
   * $INPUT record.
   *
   * @param thetaOffset     The index to the head of THETA vector within indepVar.
   * @param thetaLen        The length of THETA vector.
   *                        The vector elements are assumed to be placed
   *                        from indepVar[thetaOffset] 
   *                        to indepVar[thetaOffset + thetaLen].
   * @param etaOffset       The index to the head of ETA vector within indepVar.
   * @param etaLen          The length of ETA vector.  
   *                        The vector elements are assumed to be placed
   *                        from indepVar[etaOffset] 
   *                        to indepVar[etaOffset + etaLen].
   * @param epsOffset       The index to the head of EPS vector within indepVar.
   * @param epsLen          The length of EPS vector.
   *                        The vector elements are assumed to be placed
   *                        from indepVar[thetaOffset] 
   *                        to indepVar[thetaOffset + thetaLen].
   * @param fOffset         The index to the head of the vector of <code>F</code>  
   *                        values within depVar.
   * @param fLen            The total length of <code>F</code> vector.
   * @param yOffset         The index to the head of the vector of <code>Y</code> 
   *                        values within depVar.
   * @param yLen            The total length of <code>Y</code> vector.
   * @param i               The index to the individual of interest 
   *                        within the population (0 indicates the first
   *                        individual.)
   * @param j               The index to the data record of interest 
   *                        for the i-th individual (0 indicates their 
   *                        first data record.)
   * @param indepVar        The vector containing independent variables:
   *                        THETA, ETA and EPS.
   * @param depVar          (output) The vector whose (fOffset + m)-th and 
   *                        (yOffset + m)-th elements will be replaced with 
   *                        the calculated values for <code>F<code> and 
   *                        <code>Y<code>, respectively, if the DV data
   *                        item for this data record is not missing.  The
   *                        number m is the position of this DV value in
   *                        the list of DV values for this individual that
   *                        are not missing.  Note that m is a number that
   *                        must be determined by the implementation of
   *                        this function, i.e., it is not an input to this 
   *                        function.
   *
   * @return true           if the DV value for the i-th individual's j-th 
   *                        data record was not missing.
   * @return false          if the DV value for the i-th individual's j-th 
   *                        data record was missing.
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
   * getNRecords( int i ) returns the number of data records
   * for the i-th individual.  For the individual analysis,
   * i should be always 0.
   *
   * Previously, the PredBase class assumed that all data records
   * were observation records, and it did not have a getNRecords()
   * function.
   *
   * In order to be backward compatible, the implementation for this
   * function provided in this abstract base class returns the number
   * of observation records as the number of data records.
   *
   * Since there are usually more data records than observation
   * records, this function's implementation will usually be wrong.
   * If that is the case, then its implementation should be replaced
   * in the concrete subclass by an implementation that returns the
   * right number of data records.
   *
   * @param i The index to the individual of interest.
   *          0 indicates the first individual in the population.
   *          For the individual analysis, this value is ignored.
   * 
   * @return The number of data records for the i-th individual.
   *         If this function is called in the context of individual
   *         (only) analysis, then the number of the only individual's
   *         data records is returned.
   */
  virtual int getNRecords( int i ) const
  {
    // For the purposes of backwards compatability , return the number
    // of observation records as the number of data records.  See the
    // description for this function for more details.
    return getNObservs( i );
  }

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
   *         If this function is called in the context of individual
   *         (only) analysis, then the number of the only individual's
   *         observation records is returned.
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
