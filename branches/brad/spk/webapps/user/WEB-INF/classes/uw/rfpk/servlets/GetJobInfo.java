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
import java.util.Vector;
import java.util.Properties;
import java.nio.*;
import java.sql.*;
import rfpk.spk.spkdb.*;
import uw.rfpk.beans.UserInfo;

/** This servlet sends back job information including model, dataset, method and parent.
 * The servlet receives a String array containing thwo String objects from the client.
 * The first String object is the secret code to identify the client.  The second String 
 * is the job_id. The servlet first checks if the job belongs to the user in the group or 
 * to the library, then calls database 
 * API method, getJob, to get abstract, state_code, end_code, model_id, model_version, dataset_id, dataset_version,
 * method_code and parent. Then, the model_id is passed in the database API method getModel
 * to get model_name and model_abstract; and the dataset_id is passed in the database API method getDataset to 
 * get dataset_name and dataset_abstract.  The servlet puts job_abstract, owner, model_id, model_name, model_version,
 * model_abstract, dataset_id, dataset_name, dataset_version, dataset_abstract, state_code, end_code, share_with,
 * parent(id) and method_code into a java.util.Properties object.  The servlet sends back 
 * two objects.  The first object is a String containing the error message if there 
 * is an error or an empty String if there is not any error.  The second object is the 
 * Properties object containing the returned data.
 *
 * @author Jiaji Du
 */
public class GetJobInfo extends HttpServlet
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
        Properties jobInfo = new Properties();

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
                String username = userRS.getString("username");
                
                // Check if the job belongs to the user in the group, belongs to the library
                // or is shared with this user
                if((groupId != 0 && userRS.getLong("team_id") == groupId) || 
                   (groupId == 0 && Long.parseLong(user.getUserId()) == userId) || 
                   username.equals("librarian") ||
                   Long.parseLong(user.getUserId()) == jobRS.getLong("share_with"))
                {            
                    // Get job information
                    String jobAbstract = jobRS.getString("abstract");
                    long parent = jobRS.getLong("parent");
                    String methodCode = jobRS.getString("method_code");
                    String endCode = "";
                    String stateCode = jobRS.getString("state_code");
                    if(stateCode.equals("end"))
                        endCode = jobRS.getString("end_code");
                    long modelId = jobRS.getLong("model_id");
                    long datasetId = jobRS.getLong("dataset_id"); 
                    String modelVersion = jobRS.getString("model_version");
                    String datasetVersion = jobRS.getString("dataset_version");
                    ResultSet modelRS = Spkdb.getModel(con, modelId);
                    modelStmt = modelRS.getStatement();
                    modelRS.next();
                    String modelName = modelRS.getString("name");
                    String modelAbstract = modelRS.getString("abstract");
                    ResultSet datasetRS = Spkdb.getDataset(con, datasetId);
                    datasetStmt = datasetRS.getStatement();
                    datasetRS.next();
                    String datasetName = datasetRS.getString("name");
                    String datasetAbstract = datasetRS.getString("abstract");
                    long shareWithId = jobRS.getLong("share_with");
                    String shareWithName = "";
                    if(shareWithId > 0)
                    {
                        userRS = Spkdb.getUserById(con, shareWithId);
                        userRS.next();
                        shareWithName = userRS.getString("username");
                    }
                    int parallel = jobRS.getInt("parallel");
             
                    // Put returning objects into the Properties
                    jobInfo.setProperty("jobAbstract", jobAbstract);
                    jobInfo.setProperty("modelId", String.valueOf(modelId));
                    jobInfo.setProperty("modelName", modelName);
                    jobInfo.setProperty("modelAbstract", modelAbstract);
                    jobInfo.setProperty("modelVersion", modelVersion.substring(2));
                    jobInfo.setProperty("datasetId", String.valueOf(datasetId));                    
                    jobInfo.setProperty("datasetName", datasetName);
                    jobInfo.setProperty("datasetAbstract", datasetAbstract);
                    jobInfo.setProperty("datasetVersion", datasetVersion.substring(2));
                    jobInfo.setProperty("parent", String.valueOf(parent));
                    jobInfo.setProperty("shareWithName", String.valueOf(shareWithName));
                    jobInfo.setProperty("methodCode", methodCode);
                    jobInfo.setProperty("stateCode", stateCode);
                    jobInfo.setProperty("endCode", endCode);
                    jobInfo.setProperty("username", username);
                    jobInfo.setProperty("parallel", String.valueOf(parallel));
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
            out.writeObject(jobInfo);
        
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

