/*
 * Control.java
 *
 * Created on September 30, 2003, 11:42 AM
 */

package uw.rfpk.mda.nonmem.wizard;

/**
 *
 * @author  jiaji
 */
public class Control {
    
    /** Creates a new instance of Control */
    public Control() {
    }
    
    public String analysis = null;        // population or individual
    public String problem = null;         // Problem heading
    public String data = null;            // Data file path and name
    public String[] input = null;         // data item names
    public String[] subroutines = null;   // ADVAN, TOL, TRANS
    public String[][] model = null;       // (#Comp, `#EqComp), (name, attr1, attr2, ...)s
    public String aes = null;             // AES program
    public String aesinitial = null;      // AESINITIAL program
    public String pred = null;            // PRED program
    public String pk = null;              // PK program
    public String error = null;           // ERROR program
    public String des = null;             // DES program
    public String[][] theta = null;       // (Low, Ini, Up)s
    public String[][] omega = null;       // (form, dim, elem1, elem2, ...)s 
    public String[][] sigma = null;       // (form, dim, elem1, elem2, ...)s
    public String[] estimation = null;    // method, sigdigits, maxevals, print, restart, ind_out, centering
    public String covariance = null;      // formulation
    public String simulation = null;      // seed number
    public String[][][] tableEst = null;  // ((file, header), (list1), (appearance orders), (sorting orders))s   
    public String[][][] splotEst = null;  // ((from, to, unit, X0, Y0), (list1), (list2), (list3))s
    public String[][][] tableSim = null;  // ((file, header), (list1), (appearance orders), (sorting orders))s
    public String[][][] splotSim = null;  // ((from, to, unit, X0, Y0), (list1), (list2), (list3))s    
}
