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
import java.util.Vector;
import java.util.Properties;
import java.text.SimpleDateFormat;
import uw.rfpk.beans.UserInfo;

/** This servlet sends back information about a list of jobs belonging to the user.
 * The servlet receives a String array containing four String objects from the client.
 * The first String object is the secret code to identify the client.  The second String  
 * object is the maximum number of jobs to provide status for.  The third String object is
 * the least job_id previously returned.  The fourth String object is a flag that specified 
 * if this call is from a library patron.  The servlet calls database API method, userJobs,
 * to get job status that includes id, start_time, state_code, end-code, model_id, 
 * model_version, dataset_id, dataset_version and abstract of the jobs.  The servlet uses 
 * the model_id to get model_name using database API method getModel and uses the dataset_id
 * to get dataset_name using database API method getDataset.  The servlet converts the 
 * state_code and the end_code into the corresponding names and uses the names to form a 
 * status_code for each job.  If the state_code is not "End", the status_code is the name of
 * the state_code, otherwise the status_code is the name of the end_code.  The servlet forms
 * model_info string by "model_name.model_version" and forms dataset_info string by 
 " dataset_name.dataset_version.  The servletThe servlet puts the job_id, start_time(formated), 
 * status_code, model_info, dataset_info and arbstract of the jobs into a String[][] object.
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there is an error or an empty String if there is not any error.  The second 
 * object is the returning data String[][] object.
 *
 * @author Jiaji Du
 */
public class UserJobs extends HttpServlet
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
        String[][] userJobs = null;
 
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
     
        // Prepare for the return
        Vector jobList = new Vector();
 
        try
        {
            // Read the data from the client 
            String[] messageIn = (String[])in.readObject();
            String secret = messageIn[0];
            if(secret.equals((String)req.getSession().getAttribute("SECRET")))             
            {                        
 	        int maxNum = Integer.parseInt(messageIn[1]); 
                long leftOff = Long.parseLong(messageIn[2]);
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
 
                // Get user jobs
                ResultSet userJobsRS = Spkdb.userJobs(con, userId, maxNum, leftOff);  
                                
                // Set state_code - name conversion
                ResultSet stateRS = Spkdb.getStateTable(con);
                Properties state = new Properties();                
                while(stateRS.next())
                    state.setProperty(stateRS.getString(1), stateRS.getString(2));

                // Set end_code - name conversion
                ResultSet endRS = Spkdb.getEndTable(con);                
                Properties end = new Properties();
                while(endRS.next())
                    end.setProperty(endRS.getString(1), endRS.getString(2));

                // Fill in the List
                SimpleDateFormat formater = new SimpleDateFormat("EEE yyyy-MM-dd HH:mm:ss z");
                while(userJobsRS.next())
                {                  
                    String[] job = new String[6];   
                    job[0] = String.valueOf(userJobsRS.getLong("job_id"));
                    job[1] = formater.format(new Date(userJobsRS.getLong("start_time") * 1000));                   
                    job[2] = state.getProperty(userJobsRS.getString("state_code"));
                    if(job[2].equals("End"))
                    {                   
                        String endCode = userJobsRS.getString("end_code");
                        if(endCode != null)
                            job[2] = end.getProperty(endCode); 
                        else
                            job[2] = "";                        
                    }                    
                    ResultSet modelRS = Spkdb.getModel(con, userJobsRS.getLong("model_id"));
                    modelRS.next();
                    job[3] = modelRS.getString("name") + "." + userJobsRS.getString("model_version").substring(2);
                    ResultSet datasetRS = Spkdb.getDataset(con, userJobsRS.getLong("dataset_id"));
                    datasetRS.next();
                    job[4] = datasetRS.getString("name") + "." + userJobsRS.getString("dataset_version").substring(2);
                    job[5] = userJobsRS.getString("abstract");                    
                    jobList.add(job);
                }
                int nJob = jobList.size();
                if(nJob > 0)
                {
                    userJobs = new String[nJob][];    
                    for(int i = 0; i < nJob; i++)
                        userJobs[i] = (String[])jobList.get(i);                     
                }
                else
                    messageOut = "No job was found in the database";
                
                // Disconnect to the database
                Spkdb.disconnect(con);
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
         
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(userJobs);

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

