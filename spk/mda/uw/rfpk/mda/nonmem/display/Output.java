/*
 * Result.java
 *
 * Created on November 21, 2003, 9:25 AM
 */

package uw.rfpk.mda.nonmem.display;

import java.util.ArrayList;
import java.util.Properties;

/**
 * This class defines an object that contains output data
 * @author  jiaji Du
 */
public class Output {
    
    /** Creates a new instance of Result */
    public Output() {
    }

    /** job abstract */
    public String jobAbstract = null;
     /** job submission time */
    public String submissionTime = null;
    /** job completion time */
    public String completionTime = null;    
    /** computing times(estimation, statistics) */
    public String[] computingTimes = null;
    /** model name */
    public String modelName = null;
    /** model version */
    public String modelVersion = null;
    /** model abstract */
    public String modelAbstract = null;    
    /** data name */
    public String dataName = null;
    /** data version */
    public String dataVersion = null;
    /** data abstract */
    public String dataAbstract = null;    
    /** data lables map */    
    public Properties dataLabelMap = null;
    /** error message */
    public String error = null;
    /** objective value */
    public String objective = null;
    /** THETA vector */
    public String[] theta = null;
    /** OMEGA matrix */
    public String[][] omega = null;
    /** SIGMA matrix */
    public String[][] sigma = null;
    /** statistics label list */
    public String[] statLabels = null;
    /** standard error of THETA */
    public String[] stdErrTheta = null;
    /** standard error of OMEGA */
    public String[][] stdErrOmega = null;
    /** standard error of SIGMA */
    public String[][] stdErrSigma = null;
    /** covariance matrix */
    public String[][] covariance = null;
    /** correlation matrix */
    public String[][] correlation = null;
    /** invert covariance matrix */
    public String[][] invCovariance = null;
    /** coefficient of variation */
    public String[] coefVariation = null;
    /** 95% confidence interval (left, right) */
    public String[][] confInterval = null;
    /** nCol(item) by nRow(sum(measurements of each individual)) */
    public double[][] dataAll = null;
    /** data item label list */
    public ArrayList dataItems = null;      
    /** ((file, header, process, nSorting), (items in appearance orders))s */
    public String[][][] table = null; 
    /** ((from, to, X0, Y0, unit, process), (list1), (list2), (list3))s */
    public String[][][] scatterplot = null;
}
