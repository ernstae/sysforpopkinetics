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
package uw.rfpk.mda.nonmem.wizard;

/**
 * This class defines an object that contains input data.
 * @author  Jiaji Du
 */
public class Source {
    
    /** Creates a new instance of Source. */
    public Source() {
    }
    
    /** The analysis type: population or individual. */
    public String analysis = null;        
    /** The flag for continuing when maximum number of iterations is reached. */
    public boolean isRestart;    
    /** The problem heading. */
    public String problem = null;         
    /** The data file path and name. */
    public String data = null; 
    /** The data item names. */
    public String[] input = null;
    /** The optionss for Subroutines: ADVAN, TOL, TRANS. */
    public String[] subroutines = null; 
    /** The options for Model: (NCompartments, NEqCompartments, NParameters), (name, attr1, attr2, ...)s. */
    public String[][] model = null;       
    /** The $AES program. */
    public String aes = null; 
    /** The $AESINITIAL program. */
    public String aesinitial = null;      
    /** The $PRED program. */
    public String pred = null;            
    /** The $PK program. */
    public String pk = null;
    /** The $ERROR program. */
    public String error = null;
    /** The $DES program. */
    public String des = null;
    /** The Theta values: (Low, In, Up, fixed)s. */
    public String[][] theta = null;
    /** The Omega values: (form, dim, same, elem1, elem2, ...)s. */
    public String[][] omega = null;
    /** The Sigma values: (form, dim, same, elem1, elem2, ...)s. */
    public String[][] sigma = null; 
    /** The options for Estimation: method, sigdigits, maxevals, print, noabort, ind_out, centering, interaction. */
    public String[] estimation = null; 
    /** The formulation for statistics. */
    public String covariance = null;
    /** The options for Simulation: seed number, subproblems */
    public String[] simulation = null;
    /** The specification of Table for Estimation: ((file, header), (list1), (appearance orders), (sorting orders))s. */
    public String[][][] tableEst = null;
    /** The specification of Scatterplot for Estimation: ((from, to, unit, X0, Y0), (list1), (list2), (list3))s. */
    public String[][][] splotEst = null;  
    /** The specification of Table for Simulation: ((file, header), (list1), (appearance orders), (sorting orders))s. */
    public String[][][] tableSim = null; 
    /** The specification of Scatterplot for Simulation: ((from, to, unit, Y0, X0), (list1), (list2), (list3))s. */
    public String[][][] splotSim = null;
}
