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

/** This servlet receives five parameters: host, port, secret, jnlp_dir and type.
 *  The host and port are the web server parameters, the secret is the secret code 
 *  to identify the user, the jnlp_dir is the path name of the jnlp file, the type
 *  is the type of Model Design Agent the user selected to download.  First, the
 *  servlet uses the host and the port to form a url to the login page. Then, the 
 *  sevlet checks if the session is open.  If the session is not open, the servlet
 *  redirects to the login page.  Then the servlet uses the secret code to check 
 *  the user.  If the secret code is correct, it opens the jnlp file and modify it 
 *  according to the parameter "type", and then save the file back and sends out
 *  the modified jnlp_file to the client.  Otherwise it redirects to the login page.
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
            // Redirect to the login page.        
            resp.sendRedirect(login);
            return;
        }
       
        String secret = req.getParameter("secret");
        String filePath = req.getParameter("jnlp_dir") + "/" + secret + ".jnlp";

        if(secret.equals((String)req.getSession().getAttribute("SECRET")))             
        {                                    
 	    // Read from the jnlp file.
            BufferedReader in = new BufferedReader(new FileReader(filePath));
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
            String jnlp = buffer.toString();
            
            // Modify the jnlp file according to the MDA type.
            if(req.getParameter("type").equals("saamii"))
                jnlp = jnlp.replaceFirst("nonmem", "saamii").replaceFirst("MDAn.jar", "MDAs.jar");
            if(req.getParameter("type").equals("nonmem"))
                jnlp = jnlp.replaceFirst("saamii", "nonmem").replaceFirst("MDAs.jar", "MDAn.jar");
            
            // Save the jnlp file.
            BufferedWriter writer = new BufferedWriter(new FileWriter(filePath));            
            writer.write(jnlp);
            writer.flush();
            writer.close();
            
            // Set the header parameters
            resp.setContentType("application/x-java-jnlp-file; charset=utf-8");
            resp.setContentLength(jnlp.length());
            resp.setHeader("Content-Disposition", "filename=" + secret + ".jnlp");
            
            // Write the file content to the output.
            PrintWriter out = resp.getWriter();
            out.print(jnlp);
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

