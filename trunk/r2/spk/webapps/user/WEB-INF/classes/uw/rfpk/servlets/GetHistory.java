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
import uw.rfpk.beans.UserInfo;
import java.text.SimpleDateFormat;

/** This servlet sends back information of events occured in the job's processing histry. 
 * The servlet receives a String array containing two String objects from the client.
 * The first String object is the secret code to identify the client.  The second String 
 * is the job_id.  The servlet first checks if the job_id belongs to the user, then gets 
 * the event_time, state_code and host of all the events from the database API jobHistory 
 * call resultset.  The servlet converts the state_code to its name by calling database 
 * API method getStateTable, then sends back two objects.  The first 
 * object is a String containing the error message if there is an error or an empty String 
 * if there is not any error.  The second object is a String[][] object containing the 
 * job history data.
 *
 * @author Jiaji Du
 */
public class GetHistory extends HttpServlet
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
        Statement historyStmt = null;
        Statement stateStmt = null;
        
        // Prepare output message
        String messageOut = "";
        String[][] jobHistory = null; 

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
        Vector eventList = new Vector();
        try
        {
            // Read the data from the client 
            String[] messageIn = (String[])in.readObject();
            String secret = messageIn[0]; 
            if(secret.equals((String)req.getSession().getAttribute("SECRET")))               
            {
                long jobId = Long.parseLong(messageIn[1]);
                
                // Connect to the database
                ServletContext context = getServletContext();
                con = Spkdb.connect(context.getInitParameter("database_name"),
                                    context.getInitParameter("database_host"),
                                    context.getInitParameter("database_username"),
                                    context.getInitParameter("database_password"));
                 
                // Get job for the job_id
                ResultSet jobRS = Spkdb.getJob(con, jobId);
                jobStmt = jobRS.getStatement();
                jobRS.next();
                
                // Get job's owner
                long userId = jobRS.getLong("user_id");
                ResultSet userRS = Spkdb.getUserById(con, userId);
                userStmt = userRS.getStatement();
                userRS.next();

                // Check if the job belongs to the user in the group, belongs to the library
                // or is shared with this user
                if((groupId != 0 && userRS.getLong("team_id") == groupId) || 
                   (groupId == 0 && Long.parseLong(user.getUserId()) == userId) || 
                   userRS.getString("username").equals("librarian") ||
                   Long.parseLong(user.getUserId()) == jobRS.getLong("share_with"))
                {
                    // get job history
                    ResultSet historyRS = Spkdb.jobHistory(con, jobId);
                    historyStmt = historyRS.getStatement();
                    
                    // Set state_code conversion
                    ResultSet stateRS = Spkdb.getStateTable(con);
                    stateStmt = stateRS.getStatement();
                    Properties state = new Properties();                
                    while(stateRS.next())
                        state.setProperty(stateRS.getString(1), stateRS.getString(2));                    
                    
                    // Fill in the List
                    SimpleDateFormat formater = new SimpleDateFormat("EEE, MMM, d yyyy 'at' HH:mm:ss z");                    
                    while(historyRS.next())
                    {
                        String[] history = new String[3]; 
                        history[0] = formater.format(new Date(historyRS.getLong("event_time") * 1000));
                        history[1] = state.getProperty(historyRS.getString("state_code"));
                        history[2] = historyRS.getString("host");
                        eventList.add(history);
                    }
                    int nEvent = eventList.size();
                    if(nEvent > 0)
                    {
                        jobHistory = new String[nEvent][];    
                        for(int i = 0; i < nEvent; i++)
                            jobHistory[i] = (String[])eventList.get(i);                     
                    }
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
                if(historyStmt != null) historyStmt.close();
                if(stateStmt != null) stateStmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){messageOut += "\n" + e.getMessage();}
        }
        
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(jobHistory);
        
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

