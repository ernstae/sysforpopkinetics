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

/** This servlet sends back the version's text of model or dataset that was selected by 
 * the user from version list by the immediately previous call to the servlet GetVersions.
 * The servlet receives a String array containing two String objects from the client.
 * The first String object is the secret code to identify the client.  The second String  
 * object is the version number.  The servlet retrieves the archive from the session object
 * and then calls JRCS API methods, getRevision and arrayToString, to get the archive text.
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there is an error or an empty String if there is not any error.  The second 
 * object is the returning archive text as a String object.
 *
 * @author Jiaji Du
 */
public class GetJnlp extends HttpServlet
{
    /**
     * Dispatches client requests to the protected service method.
     * 
     * @param req  the HttpServletRequest object that contains the request the client made of the servlet.
     * @param resp  the HttpServletResponse object that contains the response the servlet returns to the client.
     * @exception ServletException a general exception a servlet can throw when it encounters difficulty.
     * @exception IOException an I/O exception of some sort.
     */
    public void doPost(HttpServletRequest req, HttpServletResponse resp)
	throws ServletException,IOException
    {       
        String login = "https://" + req.getParameter("host") + ":" + 
                       req.getParameter("port") + "/user/index.jsp";
        
        if(req.getSession(false) == null)
        {           
            // Redirect to the login page            
            resp.sendRedirect(login);
            return;
        }
       
        String secret = req.getParameter("secret");
        String jnlp = req.getParameter("jnlp_dir") + "/" + secret + ".jnlp";

        if(secret.equals((String)req.getSession().getAttribute("SECRET")))             
        {                                    
 	    // Read from the jnlp file.
            BufferedReader in = new BufferedReader(new FileReader(jnlp));
            StringBuffer buffer = new StringBuffer();
            boolean done = false;
            while(!done)
            {
                // Read a line
                String line = in.readLine();                            
                if(line == null) 
                    done = true;
                else
                    buffer.append(line).append("\n");
	    }
            in.close();
             
            // Set the header parameters
            resp.setContentType("application/x-java-jnlp-file; charset=utf-8");
            resp.setContentLength(buffer.length());
            resp.setHeader("Content-Disposition", "filename=" + secret + ".jnlp");
            
            // Write the file content to the output
            PrintWriter out = resp.getWriter();
            out.print(buffer.toString());
            out.flush();
            out.close();              
        }
        else
        {           
            // Redirect to the login page
            resp.sendRedirect(login);                              
        }                
    }
}

