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
import java.util.Vector;
import rfpk.spk.spkdb.*;
//import org.apache.commons.jrcs.rcs.*;
//import org.apache.commons.jrcs.util.ToString;
//import org.apache.commons.jrcs.diff.*;
import uw.rfpk.beans.UserInfo;
import uw.rfpk.rcs.Archive;

/** This servlet sends bsck the revision number of the model or datsset requested by 
 * the client.
 * The servlet receives a String array containing three String objects from the client.
 * The first String object is the secret code to identify the client.  The second String  
 * object is the id of the model or the dataset.  The third String object is archive type
 * that is either model or data.  The servlet first checks if this model or dataset
 * belongs to the user using ,database API method, getUser, to get the user_id and using 
 * database API method, getModel or getDataset, to get user_id, then comparing them.  If 
 * they are the same, the servlet uses the the archive from the previous database API 
 * method call and the RCS API method getNumRevision to get the last revision version number.
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there is an error or an empty String if there is not any error.  The second 
 * object is String containing the version number.
 *
 * @author Jiaji Du
 */
public class GetLastVersion extends HttpServlet
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
        // Get UserInfo of the session
        UserInfo user = (UserInfo)req.getSession().getAttribute("validUser");
        long userId = Long.parseLong(user.getUserId());
        
        // Database connection
        Connection con = null;
        Statement archiveStmt = null;
        
        // Prepare output message
        String messageOut = "";
        String versionLast = null;
        
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
 	        long id = Long.parseLong(messageIn[1]);
                String type = messageIn[2];
                
                // Connect to the database
                ServletContext context = getServletContext();
                con = Spkdb.connect(context.getInitParameter("database_name"),
                                    context.getInitParameter("database_host"),
                                    context.getInitParameter("database_username"),
                                    context.getInitParameter("database_password"));                

                // Get model or data archive
                ResultSet archiveRS = null;
                if(type.equals("model"))
                    archiveRS = Spkdb.getModel(con, id); 
                if(type.equals("data"))
                    archiveRS = Spkdb.getDataset(con, id);
                archiveStmt = archiveRS.getStatement();
                archiveRS.next();
                
                // Check if the archive belongs to the user
                if((archiveRS.getLong("user_id") == userId))
                {                
      	            Blob blobArchive = archiveRS.getBlob("archive");
	            long length = blobArchive.length(); 
	            String archive = new String(blobArchive.getBytes(1L, (int)length));                  
                            
                    // Get the last revision version number
//                    Archive arch = new Archive("", new ByteArrayInputStream(archive.getBytes())); 
//                    versionLast = String.valueOf(arch.getRevisionVersion().last());
                    versionLast = String.valueOf(Archive.getNumRevision(archive)); 
                }
                else
                {
                    // Write the outgoing messages
                    messageOut = "Authorization error.";                
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
                if(archiveStmt != null) archiveStmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){messageOut += "\n" + e.getMessage();}
        }
        
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(versionLast);

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

