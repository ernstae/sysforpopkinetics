#ifndef READ_NONMEM_DRIVER_H
#define READ_NONMEM_DRIVER_H

#include <xercesc/dom/DOM.hpp>

#include "../SpkParameters.h"
#include "NonmemTranslator.h"

/**
 * @file read_nonmem_driver.h
 * Declare read_nonmem_driver() function.
 */
/**
 * @example read_nonmem_driverTest.cpp
 */
/**
 * Process <driver> section of SpkInML document.
 *
 * DTD for <driver>:
 * @code
 * <!ELEMENT driver (theta, sigma+, omega+, eta, 
 *                   pop_opt, ind_opt,
 *                   pop_stat, ind_stat,
 *                   simulation?)>
 * <!ELEMENT theta  (in, low, up)>
 * <!ELEMENT sigma  (in)>
 * <!ELEMENT omega  (in)>
 * <!ELEMENT value  ($PCDATA)>
 * <!ELEMENT in     (value)+>
 * <!ELEMENT low    (value)+>
 * <!ELEMENT up     (value)+>
 * <!ELEMENT pop_opt EMPTY>
 *
 * <!ATTLIST theta    length              CDATA            #REQUIRED>
 * <!ATTLIST sigma    dimension           CDATA            #REQUIRED>
 * <!ATTLIST simga    same_as_previous   (yes|no)          #REQUIRED>
 * <!ATTLIST sigma    struct             (block|diagonal)  #REQUIRED>
 * <!ATTLIST omega    dimension           CDATA            #REQUIRED>
 * <!ATTLIST omega    same_as_previous   (yes|no)          #REQUIRED>
 * <!ATTLIST omega    struct             (block|diagonal)  #REQUIRED>
 * <!ATTLIST eta      length              CDATA            #REQUIRED>
 * <!ATTLIST value    fixed              (yes|no)          #REQUIRED>
 * <!ATTLIST pop_opt  approximation      (fo|foce|laplace) #REQUIRED>
 * <!ATTLIST pop_opt  pop_size            CDATA            #REQUIRED>
 * <!ATTLIST pop_opt  epsilon             CDATA            #REQUIRED>
 * <!ATTLIST pop_opt  mitr                CDATA            #REQUIRED>
 * <!ATTLIST pop_opt  trace               CDATA            #REQUIRED>
 * <!ATTLIST pop_opt  is_restart         (yes|no)          #REQUIRED>
 * <!ATTLIST pop_opt  is_par_out         (yes|no)          #REQUIRED>
 * <!ATTLIST pop_opt  is_obj_out         (yes|no)          #REQUIRED>
 * <!ATTLIST pop_opt  is_deriv1_out      (yes|no)          #REQUIRED>
 * <!ATTLIST pop_opt  is_deriv2_out      (yes|no)          #REQUIRED>
 * <!ATTLIST ind_opt  epsilon             CDATA            #REQUIRED>
 * <!ATTLIST ind_opt  mitr                CDATA            #REQUIRED>
 * <!ATTLIST ind_opt  is_restart         (yes|no)          #REQUIRED>
 * <!ATTLIST ind_opt  is_par_out         (yes|no)          #REQUIRED>
 * <!ATTLIST ind_opt  is_obj_out         (yes|no)          #REQUIRED>
 * <!ATTLIST ind_opt  is_deriv1_out      (yes|no)          #REQUIRED>
 * <!ATTLIST ind_opt  is_deriv2_out      (yes|no)          #REQUIRED>
 * <!ATTLIST pop_stat covariance_form    (rsr|r|s)         #REQUIRED>
 * <!ATTLIST pop_stat is_stderror_out    (yes|no)          #REQUIRED>
 * <!ATTLIST pop_stat is_correlation_out (yes|no)          #REQUIRED>
 * <!ATTLIST pop_stat is_covariance_out  (yes|no)          #REQUIRED>
 * <!ATTLIST pop_stat is_coeffient_out   (yes|no)          #REQUIRED>
 * <!ATTLIST pop_stat is_confidence_out  (yes|no)          #REQUIRED>
 * <!ATTLIST ind_stat is_stderror_out    (yes|no)          #REQUIRED>
 * <!ATTLIST ind_stat is_correlation_out (yes|no)          #REQUIRED>
 * <!ATTLIST ind_stat is_covariance_out  (yes|no)          #REQUIRED>
 * <!ATTLIST ind_stat is_coefficient_out (yes|no)          #REQUIRED>
 * <!ATTLIST ind_stat is_confidence_out  (yes|no)          #REQUIRED>
 * <!ATTLIST simulation seed              CDATA            #REQUIRED>
 * @endcode
 *
 * @return The value of @a pop_size attirubute associated with
 * @a pop_opt element.
 *
 * @param driverTree A pointer to the DOMElement that is the root of
 * <driver> subtree.
 *
 *
 * @param spkInfoOut Most elements of @ref SpkParameters "spkInfoOut"
 * will be altered to match the information found in the subtree as followings:
 *
 * - @ref SpkParameters::objective "objective" will be set to the value of
 * @a approximation attribute of @a pop_opt element.
 *
 * - @ref SpkParameters::nIndividuals "nIndividuals" will be set to the
 * value of @a pop_size of @a pop_opt element.
 *
 * - @ref SpkParameters::popParIn "popParIn" will be a sequence of
 * @a in values of @a theta, @a omega and @a sigma, in the order.
 * The length of @ref SpkParameters::popParIn "popParIn" is the sum 
 * of the value of @a length attribute of @a theta and the values 
 * of @a dimension attribute of @a omega and @a sigma.
 *
 * - @ref SpkParameters::popParLow "popParLow" will be a sequence of
 * the followings:
 *   -# the values of @a low under @a theta element
 *   -# TBD (REVISIT - SACHIKO 09/26/03) omega
 *   -# TBD (REVISIT - SACHIKO 09/26/03) simga
 *
 * - @ref SpkParameters::popParUp "popParUp" will be a sequence of
 * the followings:
 *   -# the values of @a Up under @a theta element
 *   -# TBD (REVISIT - SACHIKO 09/26/03) omega
 *   -# TBD (REVISIT - SACHIKO 09/26/03) simga
 *
 * - @ref SpkParameters::popParStep "popParStep" will be a sequence of
 * the followings:
 *   -# TBD (REVISIT - SACHIKO 09/26/03) theta
 *   -# TBD (REVISIT - SACHIKO 09/26/03) omega
 *   -# TBD (REVISIT - SACHIKO 09/26/03) simga
 *
 * - @ref SpkParameters::popEpsilon "popEpsilon" will be set
 * to the value of @a epsilon attribute of @a pop_opt element.
 *
 * - @ref SpkParameters::popMaxItr "popMaxItr" will be set to
 * the value of "mitr" attribute of @a pop_opt element.
 *
 * - @ref SpkParameters::popTrace "popTrace" will be set to 
 * the value of "trace" attribute of @a pop_opt element.
 *
 * - @ref SpkParameters::isPopWarmStart "isPopWarmStart" will be
 * set to the value of @a is_restart attribute of @a pop_opt element.
 *
 * - @ref SpkParameters::isPopParOut "isPopParOut" will be set
 * to @a true if the value of @a is_par_out attribute of @a pop_opt
 * element is @a yes; otherwise @a false.
 *
 * - @ref SpkParameters::isPopObjOut "isPopObjOut" will be set
 * to @a true if the value of @a is_obj_out attribute of @a pop_opt
 * element is @a yes; otherwise @a false.
 *
 * - @ref SpkParameters::isPopObj_popParOut "isPopObj_popParOut" 
 * will be set to @a true if the value of @a is_deriv1_out attribute
 * of @a pop_opt element is @a yes; otherwise @a false.
 *
 * - @ref SpkParameters::popCovarianceForm "popCovarianceForm"
 * is set to an correesponding enum value of the value of 
 * @a covariance_form attribute of @a pop_stat element;
 *   - @a RSR for "rsr"
 *   - @a R   for "r"
 *   - @a S   for "s"
 *
 * - @ref SpkParameters::isPopStderrorOut "isPopStderrorOut" is set
 * to @a true if the value of @a is_stderror_out attribute of
 * @a pop_stat is "yes"; otherwise @a false.
 *
 * - @ref SpkParameters::isPopCovarianceOut "isPopCovarianceOut" is set
 * to @a true if the value of @a is_covariance_out attribute of
 * @a pop_stat is "yes"; otherwise @a false.
 *
 * - @ref SpkParameters::isPopCorrelationOut "isPopCorreationOut" is set
 * to @a true if the value of @a is_correlation_out attribute of
 * @a pop_stat is "yes"; otherwise @a false.
 *
 * - @ref SpkParameters::isPopCoefficientOut "isPopCoefficientOut" is set
 * to @a true if the value of @a is_coefficient_out attribute of
 * @a pop_stat is "yes"; otherwise @a false.
 *
 * - @ref SpkParameters::isPopConfidenceOut "isPopConfidenceOut" is set
 * to @a true if the value of @a is_confidence_out attribute of
 * @a pop_stat is "yes"; otherwise @a false.
 *
 * - @ref SpkParameters::indParIn "indParIn" will be set to the values
 * of @a in under @a eta element.
 * The length of @ref SpkParameters::indParIn "popParIn" is 
 * determined by the value of @a length attribute of @a eta.
 *
 * - @ref SpkParameters::indParLow "indParLow" will be set to 
 * the values calculated by the following rule:
 *   -# TBD (REVISIT - SACHIKO 09/26/03) eta
 *
 * - @ref SpkParameters::indParUp "indParUp" will be set to 
 * the values calculated by the following rule:
 *   -# TBD (REVISIT - SACHIKO 09/26/03) eta
 *
 * - @ref SpkParameters::indParStep "indParStep" will be set to 
 * the values calculated by the following rule:
 *   -# TBD (REVISIT - SACHIKO 09/26/03) eta
 *
 * - @ref SpkParameters::indEpsilon "indEpsilon" will be set
 * to the value of @a epsilon attribute of @a ind_opt element.
 *
 * - @ref SpkParameters::indMaxItr "indMaxItr" will be set to
 * the value of "mitr" attribute of @a ind_opt element.
 *
 * - @ref SpkParameters::indTrace "indTrace" will be set to 
 * the value of "trace" attribute of @a ind_opt element.
 *
 * - @ref SpkParameters::isIndWarmStart "isIndWarmStart" will be
 * set to the value of @a is_restart attribute of @a ind_opt element.
 *
 * - @ref SpkParameters::isIndParOut "isIndParOut" will be set
 * to @a true if the value of @a is_par_out attribute of @a ind_opt
 * element is @a yes; otherwise @a false.
 *
 * - @ref SpkParameters::isIndObjOut "isIndObjOut" will be set
 * to @a true if the value of @a is_obj_out attribute of @a ind_opt
 * element is @a yes; otherwise @a false.
 *
 * - @ref SpkParameters::isIndObj_indParOut "isIndObj_indParOut" 
 * will be set to @a true if the value of @a is_deriv1_out attribute
 * of @a ind_opt element is @a yes; otherwise @a false.
 *
 * - @ref SpkParameters::isIndStderrorOut "isIndStderrorOut" is set
 * to @a true if the value of @a is_stderror_out attribute of
 * @a ind_stat is "yes"; otherwise @a false.
 *
 * - @ref SpkParameters::isIndCovarianceOut "isIndCovarianceOut" is set
 * to @a true if the value of @a is_covariance_out attribute of
 * @a ind_stat is "yes"; otherwise @a false.
 *
 * - @ref SpkParameters::isIndCorrelationOut "isIndCorreationOut" is set
 * to @a true if the value of @a is_correlation_out attribute of
 * @a ind_stat is "yes"; otherwise @a false.
 *
 * - @ref SpkParameters::isIndCoefficientOut "isIndCoefficientOut" is set
 * to @a true if the value of @a is_coefficient_out attribute of
 * @a ind_stat is "yes"; otherwise @a false.
 *
 * - @ref SpkParameters::isIndConfidenceOut "isIndConfidenceOut" is set
 * to @a true if the value of @a is_confidence_out attribute of
 * @a ind_stat is "yes"; otherwise @a false.
 *
 * - @ref SpkParameters::seed "seed" is set to the value of
 * @a seed attribute of @a simulation element.
 *
 * @param nonmemInfoOut Most elements of @ref SpkParameters "nonmemInfoOut"
 * will be altered to match the information found in the subtree as followings:
 *
 * - @ref NonmemParameters::thetaFixed "thetaFixed" is set to a sequence
 * of boolean values in which i-th element is true if the value of i-th 
 * @a in value under @a theta element has the attribute @a fixed set to
 * "yes"; otherwise @a false.
 *
 * - @ref NonmemParameters::thetaIn "thetaIn" is set to the 
 * @a in values of @a theta element in the order of appearance.  
 * The length of @ref NonmemParameters::thetaIn "thetaIn" is
 * determined by the value of @a length attribute of @a theta.
 *
 * - @ref NonmemParameters::thetaLow "thetaLow" is set to the 
 * of @a low values of @a theta element in the order of appearance.
 * The length of @ref NonmemParameters::thetaIn "thetaIn" is
 * determined by the value of @a length attribute of @a theta.
 *
 * - @ref NonmemParameters::thetaLow "thetaUp" is set to the 
 * @a up values of @a theta element in the order of appearance.
 * The length of @ref NonmemParameters::thetaIn "thetaIn" is
 * determined by the value of @a length attribute of @a theta.
 *
 * - @ref NonmemParameters::omegaFixed "omegaFixed" is set to a sequence
 * of boolean values in which i-th element is true if the value of i-th 
 * @a in value under @a omega element has the attribute @a fixed set to
 * "yes"; otherwise @a false.  
 * The size of @ref NonmemParameters::omegaFixed "omegaFixed" vector
 * is determined by the structure of Omega matrix.  
 * If @a struct attribute of @a omega is @a block, it is equal to
 * the factorial of the value of @a dimension attribute.
 * If @a struct is @a diagonal, the size is equal to the value of
 * @a dimension.
 *
 * - @ref NonmemParameters::omegaIn "omegaIn" is set to the
 * @a in values of @a omega element in the order of appearance.
 * The size of @ref NonmemParameters::omegaFixed "omegaFixed" vector
 * is determined by the structure of Omega matrix.  
 * If @a struct attribute of @a omega is @a block, it is equal to
 * the factorial of the value of @a dimension attribute.
 * If @a struct is @a diagonal, the size is equal to the value of
 * @a dimension.
 *
 * - @ref NonmemParameters::sigmaFixed "sigmaFixed" is set to a sequence
 * of boolean values in which i-th element is true if the value of i-th 
 * @a in value under @a sigma element has the attribute @a fixed set to
 * "yes"; otherwise @a false.  
 * The size of @ref NonmemParameters::sigmaFixed "sigmaFixed" vector
 * is determined by the structure of Sigma matrix.  
 * If @a struct attribute of @a sigma is @a block, it is equal to
 * the factorial of the value of @a dimension attribute.
 * If @a struct is @a diagonal, the size is equal to the value of
 * @a dimension.
 *
 * - @ref NonmemParameters::sigmaIn "sigmaIn" is set to the
 * @a in values of @a sigma element in the order of appearance.
 * The size of @ref NonmemParameters::sigmaFixed "sigmaFixed" vector
 * is determined by the structure of Sigma matrix.  
 * If @a struct attribute of @a sigma is @a block, it is equal to
 * the factorial of the value of @a dimension attribute.
 * If @a struct is @a diagonal, the size is equal to the value of
 * @a dimension.
 *
 * - @ref NonmemParameters::etaFixed "etaFixed" is set to a sequence
 * of boolean values in which i-th element is true if the value of i-th 
 * @a in value under @a eta element has the attribute @a fixed set to
 * "yes"; otherwise @a false.
 *
 * - @ref NonmemParameters::etaIn "etaIn" is set to the 
 * @a in values of @a eta element in the order of appearance.  
 * The length of @ref NonmemParameters::etaIn "etaIn" is
 * determined by the value of @a length attribute of @a eta element.
 */
int read_nonmem_driver( 
   xercesc::DOMElement* driverTree, 
   SpkParameters & spkInfoOut, 
   NonmemParameters& nonmemInfoOut );

#endif
