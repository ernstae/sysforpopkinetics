/**********************************************************************
From:   Resource Facility for Population Kinetics                    
        Department of Bioengineering Box 352255                      
        University of Washington                                     
        Seattle, WA 98195-2255                                       

This file is part of the System for Population Kinetics (SPK), which
was developed with support from NIH grants RR-12609 and P41-
EB001975. Please cite these grants in any publication for which this
software is used and send a notification to the address given above.

SPK is Copyright (C) 1998-2003, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution.
**********************************************************************/
package uw.rfpk.mda.nonmem;

import java.util.ArrayList;
import java.util.Properties;

/**
 * This class defines an object that contains output data.
 * @author  Jiaji Du
 */
public class Output {
            
    /** Creates a new instance of Output. */
    public Output() {
    }

    /** The job identification number. */
    public String jobId = null;    
    /** The job abstract. */
    public String jobAbstract = null;
     /** The job submission time. */
    public String submissionTime = null;
    /** The job completion time. */
    public String completionTime = null;    
    /** The computing times (estimation, statistics). */
    public String[] computingTimes = null;
    /** The computation mode (single, parallel). */
    public String computationMode = null;
    /** The sub-problem number. */
    public String subProblem = null;
    /** The simulation seed. */
    public String simulationSeed = null;
    /** The number of evaluations. */
    public String nEvaluation = null;
    /** The analysis type: population or individual. */
    public String analysis = null;
    /** The method code for the analysis. */
    public String methodCode = null;
    /** The model name. */
    public String modelName = null;
    /** The model version. */
    public String modelVersion = null;
    /** The model abstract. */
    public String modelAbstract = null;
    /** The model version log. */
    public String modelVersionLog = null;
    /** The data name. */
    public String dataName = null;
    /** The data version. */
    public String dataVersion = null;
    /** The data abstract. */
    public String dataAbstract = null;
    /** The data version log. */
    public String dataVersionLog = null;
    /** The data lables map. */    
    public Properties dataLabelMap = null;
    /** The error message. (message, file_name, line_number)s */
    public String[][] error = null;
    /** The warning messages. (message, file_name, line_number)s */
    public String[][] warning = null; 
    /** The optimization trace output. */
    public String trace = null;    
    /** The objective value. */
    public String objective = null;
    /** The alpha_center. (alpha_center)s, (alpha_step)s */
    public String[][] alpha = null;
    /** The likelihood. (left, center, right)s */
    public String[][] likelihood = null;    
    /** The likelihood standard error. (left, center, right)s */
    public String[][] likelihood_std = null;    
    /** The THETA vector. */
    public String[] theta = null;
    /** The OMEGA matrix. */
    public String[][][] omega = null;
    /** The OMEGA structure. */
    public String[] omegaStruct = null;
    /** The SIGMA matrix. */
    public String[][][] sigma = null;
    /** The SIGMA structure. */
    public String[] sigmaStruct = null;    
    /** The statistics label list. */
    public String[] statLabels = null;
    /** The standard error of THETA. */
    public String[] stdErrTheta = null;
    /** The standard error of OMEGA. */
    public String[][][] stdErrOmega = null;
    /** The standard error of SIGMA. */
    public String[][][] stdErrSigma = null;
    /** The standard error vector. */
    public String[] stdErrVector = null;
    /** The covariance matrix. */
    public String[][] covariance = null;
    /** The correlation matrix. */
    public String[][] correlation = null;
    /** The invert covariance matrix. */
    public String[][] invCovariance = null;
    /** The coefficient of variation. */
    public String[] coefVariation = null;
    /** The 95% confidence interval (left, right). */
    public String[][] confInterval = null;
    /** The individual IDs. */
    public String[] indIDs = null;
    /** The data matrix: nCol(item) by nRow(sum(measurements of each individual)). */
    public double[][] dataAll = null;
    /** The data item label list. */
    public ArrayList<String> dataItems = null;      
    /** The table specification: ((file, header, process, nSorting), (items in appearance orders))s */
    public String[][][] table = null; 
    /** The scatterplot specification: ((from, to, X0, Y0, unit, process), (list1), (list2), (list3))s */
    public String[][][] scatterplot = null;
    /** The flag meaning successful */
    public boolean ok = true;
    /** Identifiability status */
    public String identifyStatus = null;
    /** Identifiability number of solutions */
    public String identifySolutions = null;
    /** The generation method for nonparametric model */
    public String nonparamMethod = null;
    /** The seed for nonparametric model */
    public String nonparamSeed = null;
    /** The number of points for random uniform method of nonparametric model */
    public String nonparamNumOfPoints = null;
    /** The number of points per dimension for grid method of nonparametric model */
    public String nonparamPointsPerDim = null;
    /** Nonparametric initial theta*/
    public String[][] nonparamInTheta = null;
    /** Nonparametric initial omega*/
    public String[][][] nonparamInOmega = null;
    /** Nonparametric theta output*/
    public String[][] nonparamOutTheta = null;
    /** Nonparametric omega output*/
    public String[][][] nonparamOutOmega = null;
    /** Nonparametric weight */
    public String[] nonparamWeight = null;
    /** Nonparametric probability density of measurements */
    public String[][] nonparamDensity = null;
    /** Nonparametric posterior mean of theta */
    public String[][] nonparamMeanTheta = null;
    /** Nonparametric posterior mean of theta */
    public String[][][] nonparamMeanOmega = null;
    /** Nonparametric initial number of points */
    public String nonparamInPoints = null;
    /** Nonparametric output number of points */
    public String nonparamOutPoints = null;
}
