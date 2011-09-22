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
import java.net.Socket;
import rfpk.spk.spkdb.*;
import uw.rfpk.beans.UserInfo;

/** This servlet sets the job share_with field of the job specified by the client.
 * The servlet receives a String array containing three String objects from the client.
 * The first String object is the secret code to identify the client.  The second String  
 * object is the job_id.  The third String is the username of the user to share this job with.  The servlet uses database
 * API method setJobShareWith to update the share_with field of the given job in the database. 
 * The servlet sends back two String objects.  The first String object contains the error 
 * message if there is an error or an empty String if there is not any error.  The secod
 * String object is a text "true" if this operation is successful, "false" otherwise.
 *
 * @author Jiaji Du
 */
public class SetJobShareWith extends HttpServlet
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
        // Get the user ID of the session
        UserInfo user = (UserInfo)req.getSession().getAttribute("validUser");
        long userId = Long.parseLong(user.getUserId());  
        
        // Database connection
        Connection con = null;
        Statement userStmt = null;

        // Prepare output message
        String messageOut = "";
        String success = "false";
 
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
            String[] messageIn = (String[])in.readObject();
            String secret = messageIn[0];
            if(secret.equals((String)req.getSession().getAttribute("SECRET")))             
            {                        
                long jobId = Long.parseLong(messageIn[1]);
                String shareWithName = messageIn[2];

                // Connect to the database
                ServletContext context = getServletContext();
                con = Spkdb.connect(context.getInitParameter("database_name"),
                                    context.getInitParameter("database_host"),
                                    context.getInitParameter("database_username"),
                                    context.getInitParameter("database_password"));

                // Find user_id of the user to share the job with
                long shareWithId = 0;
                if(!shareWithName.equals(""))
                {
                    ResultSet userRS = Spkdb.getUser(con, shareWithName);
                    userStmt = userRS.getStatement();
                    if(userRS.next())
                    {
                        shareWithId = userRS.getLong("user_id");
                    }
                    else
                    {
                        // Write the outgoing messages
                        messageOut = "Username was not valid.";
                    }
                }
                else
                {
                    // Write the outgoing messages
                    messageOut = "Username was not found.";
                }
                
                // Set job share_with
                if(shareWithId != 0 && Spkdb.setJobShareWith(con, userId, jobId, shareWithId))
                    success = "true";
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
                if(userStmt != null) userStmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){messageOut += "\n" + e.getMessage();}
        }
        
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(success);
        
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

