package uw.rfpk.beans;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.File;
/**
 * Session object deleting jnlp file for the session
 * @author Jiaji Du
 * @version 1.0
 */
public class SessionObject implements HttpSessionBindingListener
{
    /** Constructor with no argument
     */    
    public SessionObject(){}
    
    /** Initializes file.
     * @param file A File object.
     */
    public void setSessionObject(File file)
    {
        this.file = file;
    }

    /** Does nothing.
     * @param event The HttpSessionBindingEvent object.
     */
    public void valueBound(HttpSessionBindingEvent event)
    {
    }
    
    /** Deletes the file.
     * @param event The HttpSessionBindingEvent object.
     */
    public void valueUnbound(HttpSessionBindingEvent event)
    {
        // Delete the jnlp file for the session
        if(file != null)
            file.delete();
    }

    private File file = null;
}
