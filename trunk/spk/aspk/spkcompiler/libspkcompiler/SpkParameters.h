#ifndef SPKPARAMETERS_H
#define SPKPARAMETERS_H

#include <map>
#include <string>
#include <spk/Objective.h>
#include <spk/popStatistics.h>
#include <spk/SpkValarray.h>

/**
 * @file SpkParameters.h
 * Declares SpkParameters data structure.
 */
/**
 * @struct SpkParameters
 * This data structure aggregates the information necessarily 
 * for parameter estimation, simulation and statistical analysis,
 * expressed in the SPK terms. 
 */
struct SpkParameters{

  /**
   * Analysis type.
   */
  enum Analysis { POPULATION /** for population analysis */, 
                  INDIVIDUAL /** for individual analysis */
                };

  /**
   * Indicator for which the information found in the instance of
   * this data structure is gathered for, either population or individual analyis.
   */
  enum Analysis         analysis;

  /**
   * Indicator for which the instance of 
   * this data structure contains the information necessary to
   * execute parameter estimation.
   */
  bool                  isEstimation;

  /**
   * Indicator for which the instance of this data structure
   * contains the information necessary to 
   * execute data simulation.
   */
  bool                  isSimulation;

  /**
   * Integer value used as a seed to generate random numbers during simulation.
   * @note Required when @a isSimulation is true.
   */
  int                   seed;

  /**
   * Objective function for population parameter estimation.
   * @note Required when @a analysis is POPULATION and @a isEstimation is true.
   *
   * @code
   * enum Objective = {MODIFIED_LAPLACE, FIRST_ORDER, EXPECTED_HESSIAN}
   * @endcode
   */
  enum Objective        objective;

  /**
   * The number of subjects in the population.
   * When @a analysis is INDIVIDUAL, this should be one.
   * @note Required in any case.
   */
  int                   nIndividuals;

  /**
   * The number of measurments (y) per subject.
   * @note Required in any case.
   */
  SPK_VA::valarray<int>    nMeasurements;

  /**
   * Measurements (y) for all subjects.  The length of the array
   * shall be the product of @a nMeasurementsAll and @a nIndividuals.
   * @note Required in any case.
   */
  SPK_VA::valarray<double> measurementsAll;

  /**
   * Initial values for the population parameter (alp).
   * @note Required when @a analysis is POPULATION and @a isEstimation 
   * or @a isEstimation is true.
   */
  SPK_VA::valarray<double> popParIn;

  /**
   * Lower boundary values for the population parameter (alp).
   * @note Required when @a analysis is POPULATION and @a isEstimation is true.
   */
  SPK_VA::valarray<double> popParLow;

  /**
   * Upper boundary values for the population parameter (alp).
   * @note Required when @a analysis is POPULATION and @a isEstimation is true.
   */
  SPK_VA::valarray<double> popParUp;

  /**
   * Step values used during differentiation with respect to
   * the population parameter (alp).
   * @note Required when @a analysis is POPULATION and @a isEstimation is true.
   */
  SPK_VA::valarray<double> popParStep;

  /**
   * Epsilon value used as a convergence criteria during 
   * the population level optimization.
   * @note Required when @a analysis is POPULATION and @a isEstimation is true.
   */
  double                popEpsilon;

  /**
   * Maxmum number of iterations before the population level optimizer
   * gives up.
   * @note Required when @a analysis is POPULATION and @a isEstimation is true.
   */
  int                   popMaxItr;

  /**
   * Integer value indicating the level of tracing during 
   * the population level optimization.
   * @note Required when @a analysis is POPULATION and @a isEstimation is true.
   */
  int                   popTrace;

  /**
   * Flag that forces the population level optimizer to continue
   * when @ popMaxItr is reached.
   * @note Required when @a analysis is POPULATION and @a isEstimation is true.
   */
  bool                  isPopWarmStart;

  /**
   * Flag indicating as to whether the SPK population level entry point
   * should return the final estimate
   * for the population parameter (alp).
   * @note Required when @a analysis is POPULATION and @a isEstimation is true.
   */
  bool                  isPopParOut;

  /**
   * Flag indicating as to whether the SPK population level entry point
   * should return the value of population objective function.
   * @note Required when @a analysis is POPULATION and @a isEstimation is true. 
   */
  bool                  isPopObjOut;

  /**
   * Flag indicating as to whether the SPK population level entry point
   * should return the first derivative of the value of 
   * population objective function with respect to the population parameter.
   * @note Required when @a analysis is POPULATION and @a isEstimation is true. 
   */
  bool                  isPopObj_popParOut;

  /**
   * Flag indicating as to whether the SPK population level entry point
   * should return the second derivative of the value of population
   * objective function with respect to doubly the population parameter.
   * @note Required when @a analysis is POPULATION and @a isEstimation is true. 
   */
  bool                  isPopObj_popPar_popParOut;  

  /**
   * Initial values for the individual parameter (b).
   * @note Required when @a isEstimation is true.
   */
  SPK_VA::valarray<double> indParIn;

  /**
   * Lower boundary values for the individual parameter (b).
   * @note Required in any case.
   */
  SPK_VA::valarray<double> indParLow;

  /**
   * Upper boundary values for the initial parameter (b).
   * @note Required in any case. 
   */
  SPK_VA::valarray<double> indParUp;

  /**
   * Step value used during differentiation with respect to the individual
   * parameter (b).
   * @note Required when @a isEstimation is true.
   */
  SPK_VA::valarray<double> indParStep;

  /**
   * Epsilon vaue used as a convergence criteria during 
   * the individual level optimization.
   * @note Required when @a isEstimation is true.
   */
  double                indEpsilon;

  /**
   * Maxmum number of iterations before the individual level optimizer
   * gives up.
   * @note Required when @a isEstimation is true.
   */
  int                   indMaxItr;

  /**
   * Integer value indicating the level of tracing during 
   * the individual level optimization.
   * @note Required when @a isEstimation is true.
   */
  int                   indTrace;

  /**
   * Flag that forces the individual level optimizer to continue
   * when @ popMaxItr is reached.
   * @note Required when @a isEstimation is true.
   */
  bool                  isIndWarmStart;
  /**
   * Flag indicating as to whether the SPK population/individual 
   * level entry point
   * should return the final estimate
   * for the individual parameter (b).
   * @note Required when @a isEstimation is true.
   */
  bool                  isIndParOut;

  /**
   * Flag indicating as to whether the SPK individual level entry point
   * should return the value of individual objective function (map baysian).
   * @note Required when @a analysis is INDIVIDUAL and @a isEstimation is true. 
   */
  bool                  isIndObjOut;

  /**
   * Flag indicating as to whether the SPK indivdiual level entry point
   * should return the first derivative of the value of 
   * individual objective function with respect to the individual parameter.
   * @note Required when @a analysis is INDIVIDUAL and @a isEstimation is true. 
   */
  bool                  isIndObj_indParOut;

  /**
   * Flag indicating as to whether the SPK individual level entry point
   * should return the second derivative of the value of individual
   * objective function with respect to doubly the individual parameter.
   * @note Required when @a analysis is INDIVIDUAL and @a isEstimation is true. 
   */
  bool                  isIndObj_indPar_indParOut;  

  /**
   * The kind of matrix used as a basis for analysing the final
   * estimation for the population parameter.
   * @code
   * enum PopCovForm = {RSR, R, S}
   * @endcode
   * @note Required when any of @a isPopStderrorOut, @a isPopCorrelatonOut,
   * @a isPopCovarianceOut, @a isPopCoefficientOut or @a isPopConfidenceOut
   * is true.
   */
  enum PopCovForm       popCovarianceForm;

  /**
   * Flag that requests the routine responsible for computing the
   * standard error for the population parameter to return the value.
   * 
   * @note @a isEstimation must be true and @a analysis must be POPULATION.
   */
  bool                  isPopStderrorOut;

  /**
   * Flag that requests the routine responsible for computing the
   * correlation matarix for the population parameter to return the value.
   *
   * @note @a isEstimation must be true and @a analysis must be POPULATION.
   */
  bool                  isPopCorrelationOut;

  /**
   * Flag that requests the routine responsible for computing the
   * covariance matarix for the population parameter to return the value.
   * 
   * @note @a isEstimation must be true and @a analysis must be POPULATION.
   */
  bool                  isPopCovarianceOut;

  /**
   * Flag that requests the routine responsible for computing the
   * coefficient for the population parameter to return the value.
   * 
   * @note @a isEstimation must be true and @a analysis must be POPULATION.
   */
  bool                  isPopCoefficientOut;

  /**
   * Flag that requests the routine responsible for computing the
   * 95% confidence interval for the population parameter to return the value.
   * 
   * @note @a isEstimation must be true and @a analysis must be POPULATION.
   */
  bool                  isPopConfidenceOut;
  
  /**
   * Flag that requests the routine responsible for computing the
   * standard error for the population parameter to return the value.
   * 
   * @note @a isEstimation must be true.
   */
  bool                  isIndStderrorOut;

  /**
   * Flag that requests the routine responsible for computing the
   * correlation matarix for the individual parameter to return the value.
   * 
   * @note @a isEstimation must be true.
   */
  bool                  isIndCorrelationOut;

  /**
   * Flag that requests the routine responsible for computing the
   * covariance matarix for the individual parameter to return the value.
   * 
   * @note @a isEstimation must be true.
   */
  bool                  isIndCovarianceOut;

  /**
   * Flag that requests the routine responsible for computing the
   * coefficient for the individual parameter to return the value.
   * 
   * @note @a isEstimation must be true.
   */
  bool                  isIndCoefficientOut;
  /**
   * Flag that requests the routine responsible for computing the
   * 95% confidence interval for the individual parameter to return the value.
   * 
   * @note @a isEstimation must be true.
   */
  bool                  isIndConfidenceOut;
  
};

#endif
