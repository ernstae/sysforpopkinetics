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
package uw.rfpk.beans;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.File;
/**
 * Session object to delete jnlp file for the session.
 * @author Jiaji Du
 */
public class SessionObject implements HttpSessionBindingListener
{
    /** Constructor with no argument.
     */    
    public SessionObject(){}
    
    /** Initializes file.
     * @param file a File object.
     */
    public void setSessionObject(File file)
    {
        this.file = file;
    }

    /** Does nothing.
     * @param event the HttpSessionBindingEvent object.
     */
    public void valueBound(HttpSessionBindingEvent event)
    {
    }
    
    /** Deletes the file.
     * @param event the HttpSessionBindingEvent object.
     */
    public void valueUnbound(HttpSessionBindingEvent event)
    {
        // Delete the jnlp file for the session
        if(file != null)
            file.delete();
    }

    private File file = null;
}
