/*
 * MDAObject.java
 *
 * Created on September 30, 2003, 11:38 AM
 */

package uw.rfpk.mda.nonmem.wizard;

import java.util.Properties;
import java.util.Vector;

/**
 *
 * @author  jiaji
 */
public class MDAObject {
    
    /** Creates a new instance of MDAObject */
    public MDAObject() {
    }
    
    /** Get NONMEM control file records */
    public Properties getRecords() { return records; }  
    
    /** Get Parsed control for SPK input file */
    public Control getControl() { return control; }
    
    /** Get Parsed data for SPK input file */
    public Vector getData() { return data; }
    
    /** Get Parsed data for SPK input file */
    public void setData(Vector v) { data = v; }
    
    // NONMEM control file records
    private Properties records = new Properties();
    
    // Parsed control for SPK input file
    private Control control = new Control(); 
    
    // Parsed data for SPK input file
    private Vector data = null;
}
