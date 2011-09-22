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
package uw.rfpk.mda.nonmem.compartment;

import java.util.ArrayList;

/** This class defines parameter.
 *
 * @author jiaji
 */
public class Parameter {
    
    /** Creates a new instance of Parameter.
     * @param name parameter name.
     * @param value parameter value.
     */
    public Parameter(String name, String value) {
        this.name = name;
        this.value = value;
    }
    
    protected String name;
    protected String value;
    protected ArrayList<Variable> dependVariables = new ArrayList<Variable>();
}
