/*
 * MDAObject.java
 *
 * Created on September 30, 2003, 11:38 AM
 */

package uw.rfpk.mda.nonmem.wizard;

import java.util.Properties;
import java.util.Vector;

/** This class defines a object containing NONMEM records, source for SPK input 
 * in XML format and parsed data for SPK dataset in XML format.
 *
 * @author  Jiaji Du
 */
public class MDAObject {
    
    /** Creates a new instance of MDAObject. */
    public MDAObject() {
    }
    
    /** Get NONMEM control file records.
     * @return A Properties object containing the NONMEM records.
     */
    public Properties getRecords() { return records; }  
    
    /** Get Parsed control for SPK input file
     * @return A Source object containing the information to generate SPK input.
     */
    public Source getSource() { return source; }
    
    /** Get Parsed data for SPK input file
     * @return An Vector object containing the parsed data to generate SPK data.
     */
    public Vector getData() { return data; }
    
    /** Get Parsed data for SPK input file
     * @param v A vector containing the parsed data to generate SPK data.
     */
    public void setData(Vector v) { data = v; }
    
    // NONMEM control file records
    private Properties records = new Properties();
    
    // Parsed control for SPK input file
    private Source source = new Source(); 
    
    // Parsed data for SPK input file
    private Vector data = null;
}
