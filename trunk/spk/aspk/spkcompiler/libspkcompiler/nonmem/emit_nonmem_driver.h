#ifndef EMIT_NONMEM_DRIVER_H
#define EMIT_NONMEM_DRIVER_H

#include <cstdlib>
#include "../SpkParameters.h"
#include "NonmemTranslator.h"

/**
 * @file emit_nonmem_driver.h
 * Declares emit_nonmem_driver() function.
 *
 * @ingroup nonmem
 */
/**
 * @example emit_nonmem_driverTest.cpp
 */
/**
 * emit_nonmem_driver() function generates C++ source code 
 * for  a main program which drives a sequence of 
 * popSimulation(), fitPopulation() and popStatistics()
 * or indSimulation(), fitIndividual() and indStatistics().
 *
 * The generated file pointed by @a out
 * will contain the main program which duplicates
 * the declarations and definitions of those elements 
 * in @ref SpkParameters "spkInfo"
 * which are relevant to requested
 * computational processes and the statements that
 * executes these processes.   
 * For example, the generated main program will contain
 * a statement;
 * @code
 *    int seed = 1;
 * @endcode
 * if @ref SpkParameters::isSimulation "spkInfo.isSimulation"
 * were true and @ref SpkParameters::seed "spkInfo.seed" were set to 1.
 *
 * When @ref SpkParameters::analysis "spkInfo.analysis" == POPULATION, 
 * - @ref SpkParameters::isEstimation "spkInfo.isEstimation" == true
 * requests the execution of fitPopulation(), 
 * - @ref SpkParameters::isSimulation "spkInfo.isSimulation" == true,  
 * requests the executionof popSimulation() and 
 * - any of 
 *   - @ref SpkParameters::isPopStderrorOut "spkInfo.isPopStderror", 
 *   - @ref SpkParameters::isPopCovarianceOut "spkInfo.isPopCovariance", 
 *   - @ref SpkParameters::isPopCorrelationOut "spkInfo.isPopCorrelation",
 *   - @ref SpkParameters::isPopCoefficientOut "spkInfo.isPopCoefficient" or
 *   - @ref SpkParameters::isPopConfidenceOut "spkInfo.isPopConfidence"
 * == true requests the execution of popStatistics() routine.
 *
 * When @ref SpkParameters::analysis "spkInfo.analysis" == INDIVIDUAL, 
 * - @ref SpkParameters::isEstimation "spkInfo.isEstimation" == true
 * requests the execution of fitIndividual(), 
 * - @ref SpkParameters::isSimulation "spkInfo.isSimulation" == true,
 * requests the executionof indSimulation() and 
 * - any of
 *   - @ref SpkParameters::isIndStderrorOut "spkInfo.isIndStderror", 
 *   - @ref SpkParameters::isIndCovarianceOut "spkInfo.isIndCovariance", 
 *   - @ref SpkParameters::isIndCorrelationOut "spkInfo.isIndCorrelation",
 *   - @ref SpkParameters::isIndCoefficientOut "spkInfo.isIndCoefficient" or
 *   - @ref SpkParameters::isIndConfidenceOut "spkInfo.isIndConfidence" 
 * == true requests the execution of indStatistics() routine.
 *
 * If @ref SpkParameters::analysis "spkInfo.analysis" == INDIVIDUAL 
 * and @ref SpkParameters::isSimulation "spkInfo.isSimulation" == true,
 * the following elements in @ref SpkParameters "spkInfo" are assumed 
 * to have valid values and will be declared and defined with the 
 * same data type and identifiers in the generated main program:
 *
 * - @ref SpkParameters::seed "spkInfo.seed"
 * - @ref SpkParameters::indParLow "spkInfo.indParLow"
 * - @ref SpkParameters::indParUp  "spkInfo.indParUp"
 * - @ref SpkParameters::nMeasurements  "spkInfo.nMeasurements"
 * - @ref SpkParameters::seed "spkInfo.seed"
 * 
 * If @ref SpkParameters::analysis "spkInfo.analysis" == INDIVIDUAL
 * and @ref SpkParameters::isEstimation "spkInfo.isEstimation" is true,
 * the following elements in #ref SpkParameters "spkInfo" in addition to the
 * ones required under "@ref SpkParameters::analysis "spkInfo.analysis" 
 * == INDIVIDUAL and #ref SpkParameters::isEstimation "spkInfo.isEstimation"
 * is true" are assumed to have valid values  and will be declared and defined with the
 * same data type and identifiers in the generated main program:
 *
 * - @ref SpkParameters::nMeasurements "spkInfo.nMeasurements"
 * - @ref SpkParameters::measurementsAll "spkInfo.measurementsAll"
 *   (however, when $ref SpkParameters::isSimulation "spkInfo.isSimulation"
 *    were true, the contents of this array will be replaced by the simulated data)
 * - @ref SpkParameters::indEpsilon "spkInfo.indEpsilon"
 * - @ref SpkParameters::indMaxItr "spkInfo.indMaxItr"
 * - @ref SpkParameters::indTrace "spkInfo.indTrace"
 * - @ref SpkParameters::isIndWarmStart "spkInfo.isIndWarmStart"
 * - @ref SpkParameters::isIndParOut "spkInfo.isIndParOut"
 * - @ref SpkParameters::isIndObjOut "spkInfo.isIndObjOut"
 * - @ref SpkParameters::isIndObj_indParOut "spkInfo.isIndObj_indParOut"
 * - @ref SpkParameters::isIndObj_indPar_indParOut "spkInfo.isIndObj_indPar_indParOut"
 *
 * If @ref SpkParameters::analysis "spkInfo.analysis" == INDIVIDUAL,
 * and any of the following were true, 
 * - @ref SpkParameters::isIndStderrorOut "spkInfo.isIndStderrorOut"
 * - @ref SpkParameters::isIndCorrelationOut "spkInfo.isIndCorrelationOut"
 * - @ref SpkParameters::isIndCovarianceOut "spkInfo.isIndCovarianceOut"
 * - @ref SpkParameters::isIndCoefficientOut "spkInfo.isIndCoefficientOut"
 * - @ref SpkParameters::isIndConfidenceOut "spkInfo.isIndConfidenceOut"
 *
 * the followings are assumed:
 * - @ref SpkParameters::isEstimation "spkInfo.isEstimation" is true.
 *
 * and will be declared and defined with the
 * same data type and identifiers in the generated main program.
 *
 * If @ref SpkParameters::analysis "spkInfo.analysis" == POPULATION 
 * and @ref SpkParameters::isSimulation "spkInfo.isSimulation" == true,
 * the following elements in @ref SpkParameters "spkInfo"
 * in addition to the
 * ones required under "@ref SpkParameters::analysis "spkInfo.analysis" 
 * == INDIVIDUAL and #ref SpkParameters::isEstimation "spkInfo.isEstimation"
 * is true" are assumed 
 * to have valid values and will be declared and defined with the 
 * same data type and identifiers in the generated main program:
 *
 * - @ref SpkParameters::popParIn "spkInfo.popParIn"
 * 
 * If @ref SpkParameters::analysis "spkInfo.analysis" == POPULATION
 * and @ref SpkParameters::isEstimation "spkInfo.isEstimation" is true,
 * the following elements in #ref SpkParameters "spkInfo" 
 * in addition to the
 * ones required under "@ref SpkParameters::analysis "spkInfo.analysis" 
 * == INDIVIDUAL and #ref SpkParameters::isEstimation "spkInfo.isEstimation"
 * is true" are assumed
 * to have valid values  and will be declared and defined with the
 * same data type and identifiers in the generated main program:
 *
 * - @ref SpkParameters::objective "spkInfo.objective"
 * - @ref SpkParameters::popEpsilon "spkInfo.popEpsilon"
 * - @ref SpkParameters::popMaxItr "spkInfo.popMaxItr"
 * - @ref SpkParameters::popTrace "spkInfo.popTrace"
 * - @ref SpkParameters::isPopWarmStart "spkInfo.isPopWarmStart"
 * - @ref SpkParameters::isPopParOut "spkInfo.isPopParOut"
 * - @ref SpkParameters::isPopObjOut "spkInfo.isPopObjOut"
 * - @ref SpkParameters::isPopObj_popParOut "spkInfo.isPopObj_popParOut"
 * - @ref SpkParameters::isPopObj_popPar_popParOut "spkInfo.isPopObj_popPar_popParOut"
 *
 * If @ref SpkParameters::analysis "spkInfo.analysis" == POPULATION,
 * and any of the following were true, 
 * - @ref SpkParameters::isPopStderrorOut "spkInfo.isPopStderrorOut"
 * - @ref SpkParameters::isPopCorrelationOut "spkInfo.isPopCorrelationOut"
 * - @ref SpkParameters::isPopCovarianceOut "spkInfo.isPopCovarianceOut"
 * - @ref SpkParameters::isPopCoefficientOut "spkInfo.isPopCoefficientOut"
 * - @ref SpkParameters::isPopConfidenceOut "spkInfo.isPopConfidenceOut"
 *
 * the followings are assumed:
 * - @ref SpkParameters::isEstimation "spkInfo.isEstimation" is true and
 * - @ref SpkParameters::popCovarianceForm "spkInfo.popCovarianceForm" is set to a valid value 
 *
 * and will be declared and defined with the
 * same data type and identifiers in the generated main program.
 * 
 * 
 * @return 0 if the program terminate normally and non-zero
 * if terminated abnormally.
 *
 * @param out points to a FILE handler to an open writable
 * file (with path) to which generated C++ code is emitted to.
 * The function does NOT close the file upon the 
 * completion.
 * 
 * @param nIndividuals indicates the number of individuals
 * in the population.  It would be one if it is for
 * the individual parameter analysis.
 *
 * @param modelClass_name is the name of subclass drived
 * from SpkModel class.  The name is used to generate
 * the line of code where its object is instantiated.
 *
 * @param modelObject_init_block is a character array containing
 * the sequence of events that take place before calling
 * the model's constructor and terminated by a statement
 * instanciating the model class --- i.e. the block of 
 * C++ code initializing the parameters
 * passed to the model constructor followed by a line of
 * code constructing the model.  The model object name
 * must be "model".  For example, if the SpkModel subclass
 * is named "MyModel" and its primary constructor takes
 * three integer values as arguments, modelObject_init_block[]
 * should look like the following, except that the variables
 * used to describe these integers can be altered as you wish:
 * @code
 *    int a = 1;
 *    int b = 2;
 *    int c = 3;
 *    MyModel model( a, b, c );
 * @endcode
 * or
 * @code
 *    MyModel model( 1, 2, 3 );
 * @endcode
 * Note that all elements in SpkParameters will be declared as
 * the same types and with the same names in the generated
 * main() source code.  So, you can assume and refer to these
 * entities in the constructor call.
 * 
 * @param spkInfo contains flags to indicate which
 * routines (a data simulation routine, a parameter estimation
 * routine, a statistical analysis routine) to be executed 
 * and values necessary to initialize
 * parameters passed to these routines.
 *
 * @param nonmemInfo contains values expressed in the NONMEM
 * terminology.  The function does not currently depend
 * on these values.
 */
void emit_nonmem_driver( 
		 FILE * out, 
		 int nIndividuals, 
		 const char modelClass_name[],
		 const char modelObject_init_block[],
		 struct SpkParameters & spkInfo,
		 struct NonmemParameters & nonmemInfo 
		 );

#endif
