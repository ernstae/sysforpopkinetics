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
     * @return a Properties object containing the NONMEM records.
     */
    public Properties getRecords() { return records; }  
    
    /** Get Parsed control for SPK input file.
     * @return a Source object containing the information to generate SPK input.
     */
    public Source getSource() { return source; }
    
    /** Get Parsed data for SPK input file.
     * @return an Vector object containing the parsed data to generate SPK data.
     */
    public Vector<Vector<String[]>> getData() { return data; }
    
    /** Set Parsed data for SPK input file.
     * @param v an vector containing the parsed data to generate SPK data.
     */
    public void setData(Vector<Vector<String[]>> v) { data = v; }
    
    /** Get data labels.
     * @return a String array containing the data labels.
     */
    public String[] getDataLabels() { return dataLabels; }
    
    /** Set data labels.
     * @param s a String array containing the data labels.
     */
    public void setDataLabels(String[] s) { dataLabels = s; }
    
    /** Get graphical model.
     * @return a String containing the graphical model.
     */
    public String getGraphicalModel() { return graphicalModel; }
    
    /** Set graphical model.
     * @param s a String containing the graphical model.
     */
    public void setGraphicalModel(String s) { graphicalModel = s; }
    
    // NONMEM control file records
    private Properties records = new Properties();
    
    // Parsed control for SPK input file
    private Source source = new Source(); 
    
    // Parsed data for SPK input file
    private Vector<Vector<String[]>> data = null;
    
    // Data labels
    private String[] dataLabels = null;
    
    // Graphical model
    private String graphicalModel = null;
}
