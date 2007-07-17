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
    public SessionObject(){}

    public void setSessionObject(String secret)
    {
        this.secret = secret;
    }

    public void valueBound(HttpSessionBindingEvent event)
    {
    }

    public void valueUnbound(HttpSessionBindingEvent event)
    {
        // Get the jnlp file path
        String path = "/home/jiaji/jakarta-tomcat-4.1.24/webapps/spk/jnlp/"+secret+".jnlp";

        // Create a new file object
        File file = new File(path);

        // Delete the jnlp file for the session
        file.delete();
    }

    private String secret = null;
}
