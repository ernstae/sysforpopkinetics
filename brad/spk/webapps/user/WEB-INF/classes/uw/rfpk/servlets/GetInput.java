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
//import org.apache.commons.jrcs.rcs.*;
//import org.apache.commons.jrcs.util.ToString;
//import org.apache.commons.jrcs.diff.*;
import uw.rfpk.beans.UserInfo;
import uw.rfpk.rcs.Archive;

/** This servlet seads back the three components: source, data, model of the SPK input file. 
 * The servlet receives a String array containing two String objects from the client.
 * The first String object is the secret code to identify the client.  The second String 
 * is the job_id. The servlet first checks if the job_id belongs to the user, then gets the xml_source,
 * model_id, model_version, dataset_id and dataset_version from the getJob call resultset. 
 * Then, the model_id is paased in the database API method getModel to get model archive 
 * and the dataset_id is passed in the database API method getDataset to get dataset archive.  
 * The servlet calls RCS API method getRevision to get the archive 
 * text of the version that has been returned from the database API method getJob call for  
 * both the model and the dataset.  The servlet puts the xml_source, model, model version log, dataset
 * and dataset version log into a java,util.Properties object.  The servlet sends back two objects.  
 * The first object is a String containing the error message if there is an error or an empty String 
 * if there is not any error.  The second object is the Properties object containing the 
 * returned data.
 *
 * @author Jiaji Du
 * @version 1.0
 */
public class GetInput extends HttpServlet
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
        Statement jobStmt = null;
        Statement modelStmt = null;
        Statement datasetStmt = null;
        
        // Prepare output message
        String messageOut = "";
        Properties spkInput = new Properties(); 

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
                // Connect to the database
                ServletContext context = getServletContext();
                con = Spkdb.connect(context.getInitParameter("database_name"),
                                    context.getInitParameter("database_host"),
                                    context.getInitParameter("database_username"),
                                    context.getInitParameter("database_password"));
                          
                // Get job for the job_id
                ResultSet jobRS = Spkdb.getJob(con, Long.parseLong(messageIn[1]));
                jobStmt = jobRS.getStatement();
                jobRS.next();
                
                // Get job's owner
                long userId = jobRS.getLong("user_id");
                ResultSet userRS = Spkdb.getUserById(con, userId);
                userStmt = userRS.getStatement();
                userRS.next();

                // Check if the job belongs to the user in the group, belongs to the library
                // or is shared wit this user
                if((groupId != 0 && userRS.getLong("team_id") == groupId) || 
                   (groupId == 0 && Long.parseLong(user.getUserId()) == userId) || 
                   userRS.getString("username").equals("librarian") ||
                   Long.parseLong(user.getUserId()) == jobRS.getLong("share_with"))               
                {
                    // Get source
                    Blob blob = jobRS.getBlob("xml_source");
	            long length = blob.length(); 
	            String source = new String(blob.getBytes(1L, (int)length));
                  
                    // Get model
                    ResultSet modelRS = Spkdb.getModel(con, jobRS.getLong("model_id"));
                    modelStmt = modelRS.getStatement();
                    String version = jobRS.getString("model_version");
                    modelRS.next();
                    blob = modelRS.getBlob("archive");
                    length = blob.length();
                    String ar = new String(blob.getBytes(1L, (int)length));
//                    Archive arch = new Archive("", new ByteArrayInputStream(ar.getBytes()));                
//                    Object[] revision = arch.getRevision(version); 
//                    String model = ToString.arrayToString(revision, "\n");
                    String perlDir = getServletContext().getInitParameter("perlDir");                    
                    String model = Archive.getRevision(ar, perlDir, "/tmp/", secret, version);
                    String modelLog = Archive.getVersionList(ar)[Integer.parseInt(version.substring(2)) - 1][3];

                    // Get dataset
                    ResultSet datasetRS = Spkdb.getDataset(con, jobRS.getLong("dataset_id"));
                    datasetStmt = datasetRS.getStatement();
                    version = jobRS.getString("dataset_version");
                    datasetRS.next();
                    blob = datasetRS.getBlob("archive");
                    length = blob.length();
                    ar = new String(blob.getBytes(1L, (int)length));
//                    arch = new Archive("", new ByteArrayInputStream(ar.getBytes()));
//                    revision = arch.getRevision(version);
//                    String dataset = ToString.arrayToString(revision, "\n");
                    String dataset = Archive.getRevision(ar, perlDir, "/tmp/", secret, version);
                    String datasetLog = Archive.getVersionList(ar)[Integer.parseInt(version.substring(2)) - 1][3];

                    // Put data into the properties object
                    spkInput.setProperty("source", source); 
                    spkInput.setProperty("model", model); 
                    spkInput.setProperty("dataset", dataset);
                    spkInput.setProperty("modelLog", modelLog); 
                    spkInput.setProperty("datasetLog", datasetLog);
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
        catch(InterruptedException e)
        {
            messageOut = e.getMessage();
        }        
        finally
        {
            try
            {
                if(userStmt != null) userStmt.close();
                if(jobStmt != null) jobStmt.close();
                if(modelStmt != null) modelStmt.close();
                if(datasetStmt != null) datasetStmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){messageOut += "\n" + e.getMessage();}
        }
        
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(spkInput);

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

