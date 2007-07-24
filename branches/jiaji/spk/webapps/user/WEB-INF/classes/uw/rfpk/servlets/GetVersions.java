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
import java.text.SimpleDateFormat;
//import org.apache.commons.jrcs.rcs.*;
//import org.apache.commons.jrcs.util.ToString;
//import org.apache.commons.jrcs.diff.*;
import uw.rfpk.beans.UserInfo;
import uw.rfpk.rcs.Archive;

/** This servlet sends bsck the version list of the model or datsset that was selected by 
 * the user from model list by the immediately previous call to the servlet UserModels
 * or from dataset list by the immediately previous call to the servlet UserDatasets.
 * The servlet receives a String array containing three String objects from the client.
 * The first String object is the secret code to identify the client.  The second String  
 * object is the id of the model or the dataset.  The third String object is archive type
 * that is either model or data.  The servlet first checks if the archive belongs to the 
 * user in the group or to the library, then uses the archive from the previous database 
 * API method call and the RCS API methods to get the status of the versions including 
 * version number, author name, revision time and log of the versions.  The servlet puts 
 * these data into a String[][] object. 
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there is an error or an empty String if there is not any error.  The second 
 * object is the returning data String[][] object.
 *
 * @author Jiaji Du
 */
public class GetVersions extends HttpServlet
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
        long groupId = Long.parseLong(user.getTeamId());
        
        // Database connection
        Connection con = null;
        Statement userStmt = null;
        Statement archiveStmt = null;

        // Prepare output message
        String messageOut = "";
        String[][] versionList = null;
        
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
            String type = messageIn[2];
            
            if(secret.equals((String)req.getSession().getAttribute("SECRET")))             
            {                        
 	        long id = Long.parseLong(messageIn[1]);
                
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
                
                // Get archive's owner
                long userId = archiveRS.getLong("user_id");
                ResultSet userRS = Spkdb.getUserById(con, userId);
                userStmt = userRS.getStatement();
                userRS.next();
                
                // Check if the archive belongs to the user in the group or to the library
                if((groupId != 0 && userRS.getLong("team_id") == groupId) || 
                   (groupId == 0 && Long.parseLong(user.getUserId()) == userId) || 
                   userRS.getString("username").equals("librarian"))
                {                
      	            Blob blobArchive = archiveRS.getBlob("archive");
	            long length = blobArchive.length(); 
	            String archive = new String(blobArchive.getBytes(1L, (int)length));
                    
                    versionList = Archive.getVersionList(archive);
                    req.getSession().setAttribute("ARCHIVE", archive);
            
                    // Generate version list for the model or the dataset
//                    Archive arch = new Archive("", new ByteArrayInputStream(archive.getBytes())); 
//                    int number = arch.getRevisionVersion().last();
//                    versionList = new String[number][4];
//                    for(int i = 0; i < number; i++)
//                    {
//                        int n = number - i;
//                        Node node = arch.findNode(new Version("1." + n));
//                        versionList[i][0] = String.valueOf(n);
//                        versionList[i][1] = node.getAuthor().toString();
//                        versionList[i][2] = node.getDate().toString();
//                        versionList[i][3] = arch.getLog("1." + n);
//                    }
//                    req.getSession().setAttribute("ARCHIVE", arch);
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
                if(userStmt != null) userStmt.close();
                if(archiveStmt != null) archiveStmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){messageOut += "\n" + e.getMessage();}
        }
        
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(versionList);

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

