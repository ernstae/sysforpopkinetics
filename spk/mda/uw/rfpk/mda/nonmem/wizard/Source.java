/*
 * Control.java
 *
 * Created on September 30, 2003, 11:42 AM
 */

package uw.rfpk.mda.nonmem.wizard;

/**
 * This class defines an object that contains input data
 * @author  jiaji Du
 */
public class Source {
    
    /** Creates a new instance of Source */
    public Source() {
    }
    
    /** population or individual */
    public String analysis = null;        
    /** if estimation using simulated data */
    public boolean isUsingSimulatedData;  
    /** problem heading */
    public String problem = null;         
    /** data file path and name */
    public String data = null; 
    /** data item names */
    public String[] input = null;
    /** ADVAN, TOL, TRANS */
    public String[] subroutines = null; 
    /** (NCompartments, NEqCompartments, NParameters), (name, attr1, attr2, ...)s */
    public String[][] model = null;       
    /** $AES program */
    public String aes = null; 
    /** $AESINITIAL program */
    public String aesinitial = null;      
    /** $PRED program */
    public String pred = null;            
    /** $PK program */
    public String pk = null;
    /** $ERROR program */
    public String error = null;
    /** $DES program */
    public String des = null;
    /** (Low, Ini, Up)s */
    public String[][] theta = null;
    /** (form, dim, elem1, elem2, ...)s */
    public String[][] omega = null;
    /** (form, dim, elem1, elem2, ...)s */
    public String[][] sigma = null; 
    /** method, sigdigits, maxevals, print, restart, ind_out, centering */
    public String[] estimation = null; 
    /** formulation */
    public String covariance = null;
    /** seed number */
    public String simulation = null;
    /** ((file, header), (list1), (appearance orders), (sorting orders))s */
    public String[][][] tableEst = null;
    /** ((from, to, unit, X0, Y0), (list1), (list2), (list3))s */
    public String[][][] splotEst = null;  
    /** ((file, header), (list1), (appearance orders), (sorting orders))s */
    public String[][][] tableSim = null; 
    /** ((from, to, unit, X0, Y0), (list1), (list2), (list3))s */
    public String[][][] splotSim = null;
}
