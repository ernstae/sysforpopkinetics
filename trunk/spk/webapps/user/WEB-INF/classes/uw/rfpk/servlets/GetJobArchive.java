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
import java.util.Vector;
import java.util.Properties;
import java.sql.*;
import rfpk.spk.spkdb.*;
import org.apache.commons.jrcs.rcs.*;
import org.apache.commons.jrcs.util.ToString;
import org.apache.commons.jrcs.diff.*;
import uw.rfpk.beans.UserInfo;

/** This servlet receives a String array containing four String objects from the client.
 * The first String object is the secret code to identify the client.  The second String 
 * is the job_id.  The third String is either "model" or "data".  The fourth String object 
 * is a flag that specified if this call is from a library patron.  The servlet first checks 
 * if the job_id belongs to the user using database API method, getUser, to get the user_id
 * and using database API method, getJob, to get user_id, then comparing them.  If they
 * are the same, the servlet calls database API method, getJob, to get either model_id, 
 * model_version, or dataset_id, dataset_version according to the third String. Then,
 * the model_id is paased in the database API method getModel to get model archive  and 
 * model name, or the dataset_id is passed in the database API method getDataset to get 
 * dataset archive and dataset name.  The servlet calls JRCS API methods, getRevision and 
 * arrayToString, to get the archive text of the version that has been returned from the 
 * database API method getJob call.  The servlet puts the archive text, name and version
 * into a java,util.Properties object.  The servlet sends back two objects.  The first 
 * object is a String containing the error message if there is an error or an empty String 
 * if there is not any error.  The second object is the Properties object containing the 
 * returned data.
 *
 * @author Jiaji Du
 */
public class GetJobArchive extends HttpServlet
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
        // Get the user name of the session
        UserInfo user = (UserInfo)req.getSession().getAttribute("validUser");
        String username = user.getUserName();
        
        // Prepare output message
        String messageOut = "";
        Properties archive = new Properties(); 

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
                if(messageIn[3].equals("true"))
                    username = "librarian";
                
                // Connect to the database
                ServletContext context = getServletContext();
                Connection con = Spkdb.connect(context.getInitParameter("database_name"),
                                               context.getInitParameter("database_host"),
                                               context.getInitParameter("database_username"),
                                               context.getInitParameter("database_password"));
                
                // Get user id
                ResultSet userRS = Spkdb.getUser(con, username);
                userRS.next();
                long userId = userRS.getLong("user_id");
                 
                // Get job for the job_id
                ResultSet jobRS = Spkdb.getJob(con, jobId); 
                jobRS.next();
                 
                // Check if the job belongs to the user
                if(jobRS.getLong("user_id") == userId)
                {
                    // Get model or dataset for the job
                    ResultSet rs = null;
                    String version = null;
                    if(messageIn[2].equals("model"))
                    {
                        rs = Spkdb.getModel(con, jobRS.getLong("model_id"));
                        version = jobRS.getString("model_version");
                    }
                    else if(messageIn[2].equals("data"))
                    {
                        rs = Spkdb.getDataset(con, jobRS.getLong("dataset_id"));
                        version = jobRS.getString("dataset_version");
                    }
                    else
                        messageOut = "The type was not found."; 
 
                    // Disconnect to the database
                    Spkdb.disconnect(con);
                
                    // Get the requested version of model or dataset
                    rs.next();
                    String ar = rs.getString("archive"); 
                    Archive arch = new Archive("", new ByteArrayInputStream(ar.getBytes()));                
                    Object[] revision = arch.getRevision(version); 
                    String text = ToString.arrayToString(revision, "\n"); 
                    if(text.equals(""))
                        messageOut = "The archive is empty."; 
                 
                    // Put data into the properties object
                    archive.setProperty("name", rs.getString("name")); 
                    archive.setProperty("version", version.substring(2));
                    archive.setProperty("text", text);
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
        catch(ParseException e)
        {
            messageOut = e.getMessage();
        }        
        catch(InvalidFileFormatException e)
        {
            messageOut = e.getMessage();
        }
        catch(PatchFailedException e)
        {
            messageOut = e.getMessage();
        }
        
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(archive);
        
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

