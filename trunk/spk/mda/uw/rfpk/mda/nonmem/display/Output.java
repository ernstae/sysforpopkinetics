/*
 * Result.java
 *
 * Created on November 21, 2003, 9:25 AM
 */

package uw.rfpk.mda.nonmem.display;

import java.util.ArrayList;
import java.util.Properties;

/**
 *
 * @author  jiaji Du
 */
public class Output {
    
    /** Creates a new instance of Result */
    public Output() {
    }

    public String title = null;             // problem title
    public Properties dataLabelMap = null; // data lables map
    public String error = null;             // error message
    public String objective = null;         // objective value
    public String[] theta = null;           // THETA parameters
    public String[][] omega = null;         // OMEGA parameters
    public String[][] sigma = null;         // SIGMA parameters  
    public String[] statLabels = null;      // statistics label list
    public String[] stdErrTheta = null;     // standard error vector
    public String[][] stdErrOmega = null;   // standard error vector
    public String[][] stdErrSigma = null;   // standard error vector    
    public String[][] covariance = null;    // covariance matrix
    public String[][] correlation = null;   // correlation matrix
    public String[][] invCovariance = null; // invert covariance matrix
    public String[][] dataAll = null;       // nCol(item) by nRow(sum(measurements of each individual)) // spk output data
    public ArrayList dataItems = null;      // list of data item labels // able to determine the index of an element
    public String[][][] table = null;       // ((file, header, process, nSorting), (items in appearance orders))s   
    public String[][][] scatterplot = null; // ((from, to, unit, X0, Y0, process), (list1), (list2), (list3))s
}
