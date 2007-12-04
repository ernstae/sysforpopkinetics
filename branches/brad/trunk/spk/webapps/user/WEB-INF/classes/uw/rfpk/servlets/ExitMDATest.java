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
package uw.rfpk.servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
import java.nio.*;

/** This servlet invalidates the session.  It receives a String object that is
 * the secret code to identify the client and returns an empty String object.
 *
 * @author Jiaji Du
 */
public class ExitMDATest extends HttpServlet
{
    /**
     * Dispatches client requests to the protected service method.
     * 
     * @param req  the HttpServletRequest object that contains the request the client made of the servlet.
     * @param resp  the HttpServletResponse object that contains the response the servlet returns to the client.
     * @exception ServletException a general exception a servlet can throw when it encounters difficulty.
     * @exception IOException an I/O exception of some sort.
     */
    public void service(HttpServletRequest req, HttpServletResponse resp)
	throws ServletException, IOException
    {
        // Prepare output message
        String messageOut = "";
        
        // Get the input stream for reading data from the client
        ObjectInputStream in = new ObjectInputStream(req.getInputStream());  
      
        // Set the content type we are sending
        resp.setContentType("application/octet-stream");

        // Data will always be written to a byte array buffer so
        // that we can tell the server the length of the data
        ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
      
        // Create the output stream to be used to write the data to our buffer
        ObjectOutputStream out = new ObjectOutputStream(byteOut);        
        try
        {
            // Read the data from the client 
            String secret = (String)in.readObject();            
            if(secret.equals((String)req.getSession().getAttribute("SECRET")))             
            {                 
                req.getSession().invalidate();
            }
            else
            {
                // Write the outgoing messages
                messageOut = "Authentication error.";              
            } 
        }
        catch(ClassNotFoundException e)
        {
            messageOut += "\n" + e.getMessage();
        }
       
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject("");
        
        // Flush the contents of the output stream to the byte array
        out.flush();
        
        // Get the buffer that is holding our response
        byte[] buf = byteOut.toByteArray();
        
        // Notify the client how much data is being sent
        resp.setContentLength(buf.length);
        
        // Send the buffer to the client
        ServletOutputStream servletOut = resp.getOutputStream();
        
        // Wrap up
        servletOut.write(buf);
        servletOut.close();
    }
}

