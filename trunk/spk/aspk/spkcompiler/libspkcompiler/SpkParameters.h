#ifndef SPKPARAMETERS_H
#define SPKPARAMETERS_H

#include <map>
#include <valarray>
#include <string>
#include <spk/Objective.h>
#include <spk/popStatistics.h>
#include "Symbol.h"
#include "client.h"

/**
 * @file SpkParameters.h
 * Declares SpkParameters data structure.
 *
 */
/**
 * @struct SpkParameters
 * This data structure aggregates the information necessarily to have
 * SPK estimate the population parameters, expressed in the SPK terms. 
 */
struct SpkParameters{
  enum Analysis { POPULATION, INDIVIDUAL };

  enum Analysis         analysis;
  bool                  isEstimation;
  bool                  isSimulation;
  int                   seed;
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

  enum PopCovForm       popCovarianceForm;
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
  
  //  std::map< std::string, std::valarray<double> > aliases;
};

#endif
