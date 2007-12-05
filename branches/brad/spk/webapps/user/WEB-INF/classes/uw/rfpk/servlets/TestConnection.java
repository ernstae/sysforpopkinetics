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
import java.sql.*;
import rfpk.spk.spkdb.*;
import java.util.HashMap;

/** This servlet is to test the connection between the Model Design Agent and the associated
 * web server and get the content of the method table from the database for MDA.  
 * It receives a String object that is the secret code used to check authentication.
 * The servlet calls database API method getMethodTable to get the content of the table
 * and put the data into a java.util.HashMap object.  The servlet sends back two objects.
 * The first object is a String object containing error message if there is an error or 
 * an empty String if there is not any error.  The second object is the java.util.HashMap 
 * object containing the method table content.  
 *
 * @author Jiaji Du
 */
public class TestConnection extends HttpServlet
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
        // Database connection
        Connection con = null;
        Statement methodStmt = null;
        
        // Prepare output message
        String messageOut = "";
        HashMap methodTable = null;
       
        // Get the input stream for reading data from the client
        ObjectInputStream in = new ObjectInputStream(req.getInputStream());  
      
        // Set the content type we are sending
        resp.setContentType("application/octet-stream");

        // Data will always be written to a byte array buffer so
        // that we can tell the server the length of the data
        ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
      
        // Create the output stream to be used to write the data
        // to our buffer
        ObjectOutputStream out = new ObjectOutputStream(byteOut);
        
        try
        {
            // Read the data from the client 
            String secret = (String)in.readObject();
            if(secret.equals((String)req.getSession().getAttribute("SECRET")))               
            {   
                // Connect to the database
                ServletContext context = getServletContext();
                con = Spkdb.connect(context.getInitParameter("database_name"),
                                    context.getInitParameter("database_host"),
                                    context.getInitParameter("database_username"),
                                    context.getInitParameter("database_password"));

                // Get method table
                methodTable = new HashMap(7);              
                ResultSet methodRS = Spkdb.getMethodTable(con);
                methodStmt = methodRS.getStatement();
    
                while(methodRS.next())
                {
                    String[] row = new String[3];                    
                    row[0] = methodRS.getString("method_name");
                    row[1] = methodRS.getString("class_code");
                    row[2] = String.valueOf(methodRS.getInt("test_only"));
                    methodTable.put(methodRS.getString("method_code"), row);                   
                }
            }
            else
            {
                // Write the outgoing messages
                messageOut = "Authentication error.";              
            }            
        }
        catch(SQLException e)
        {
            messageOut = e.getMessage();
        }
        catch(SpkdbException e)
        {
            messageOut = e.getMessage();
        }
        catch(ClassNotFoundException e)
        {
            messageOut = e.getMessage();
        }
        finally
        {
            try
            {
                if(methodStmt != null) methodStmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){messageOut += "\n" + e.getMessage();}
        }
                            
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(methodTable);

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

