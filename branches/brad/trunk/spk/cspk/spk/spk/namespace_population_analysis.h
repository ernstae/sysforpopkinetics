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
/*
$begin namespace_population_analysis$$
$spell 
   namespace
   laplace
   hessian
   const
   namespace_population_analysis
$$

$section Population Analysis Namespace$$

$index population_analysis$$
$index namespace, population analysis$$

$table
$bold Header:$$       $cend
Namespace_population_analysis.h $rend
$bold namespace:$$    $cend
namespace_namespace_population $rend
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This namespace defines a set of identifiers specific to the population analysis.

$head Defined Values$$
$syntax/
const int WITH_RESPECT_TO_ALP
/$$
is a constant integer that is used to indicate that the variable to a derivative to be taken is the population parameter.
$syntax/

const int WITH_RESPECT_TO_B
/$$
is a constant integer that is used to indicate that the variable to a derivative to be taken is the individual parameter.
$syntax/

const int LAPLACE
/$$
is a constant integer that is used to select the modified Laplace objective.
$syntax/

const int HESSIAN
/$$     
is a constant integer that is used to select the Expected Hessian objective
$syntax/

const int FO
/$$
is a constant integer that is used to select the First Order objective.       
$syntax/

const int NAIVE_FO
/$$
is a constant integer that is used to select the naive (straight translation of ) First Order objective. 
$syntax/

const int JOINT
/$$
is a constant integer that is used to select the Joint Likelihood objective.

$end
*/


#ifndef POPULATION_ANALYSIS_NAMESPACE
#define POPULATION_ANALYSIS_NAMESPACE

#include <string>

namespace population_analysis{
    //const std::string WITH_RESPECT_TO_ALP = "alp";
    //const std::string WITH_RESPECT_TO_B   = "b"; 

    const std::string STR_LAPLACE = "Modified Laplace";
    const std::string STR_HESSIAN = "Expected Hessian";
    const std::string STR_FO      = "First Order";
    const std::string STR_NAIVE_FO= "Naive First Order";
    const std::string STR_JOINT   = "Joint Likelihood";

    const int LAPLACE = 1;
    const int HESSIAN = 2;
    const int FO      = 3;
    const int NAIVE_FO= 4;
    const int JOINT   = 5;

    const int WITH_RESPECT_TO_ALP = 0;  // with respect to alp
    const int WITH_RESPECT_TO_B   = 1;  // with respect to b
}
#endif
