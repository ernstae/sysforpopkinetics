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

/** This servlet receives a String array containing three String objects from the client.
 * The first String object is the secret code to identify the client.  The second String  
 * object is the maximum number of jobs to provide status for.  The third String object is
 * the least job_id previously returned.  The servlet calls database API method, userJobs,
 * to get job status that includes id, start_time, state_code, end-cod and abstract
 * of the jobs.  The servlet puts these data into a String[][] object and saves the user's
 * job id list in the Session object, JOBIDS.
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there is an error or an empty String if there is not any error.  The second 
 * object is the returning data String[][] object.
 *
 * @author Jiaji Du
 * @version 1.0
 */
public class UserJobs extends HttpServlet
{
    /**
     * Dispatches client requests to the protected service method.
     * 
     * @param req  the HttpServletRequest object that contains the request the client made of the servlet
     * @param resp  the HttpServletResponse object that contains the response the servlet returns to the client
     * @exception ServletException a general exception a servlet can throw when it encounters difficulty
     * @exception IOException an I/O exception of some sort
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
 
        // Set state_code conversion
        Properties state = new Properties();
        String[] code = {"q2c", "cmp", "q2r", "run", "end"};
        String[] name = {"Queued to compile", "Compiling", "Queued to run", "Running", "End"};
        for(int j = 0; j < 5; j++)
            state.setProperty(code[j], name[j]); 
         
        // Set end_code conversion
        Properties end = new Properties();
        end.setProperty("cerr", "Error found");
        end.setProperty("srun", "Job finished");
         
        try
        {
            // Read the data from the client 
            String[] messageIn = (String[])in.readObject();
            String secret = messageIn[0];
            if(secret.equals((String)req.getSession().getAttribute("SECRET")))             
            {                        
 	        int maxNum = Integer.parseInt(messageIn[1]); 
                long leftOff = Long.parseLong(messageIn[2]);
                
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
             
                // Disconnect to the database
                Spkdb.disconnect(con);
 
                // Fill in the List
                SimpleDateFormat formater = new SimpleDateFormat("EEE, MMM, d yyyy 'at' HH:mm:ss z");
                while(userJobsRS.next())
                {                  
                    String[] job = new String[5];   
                    job[0] = String.valueOf(userJobsRS.getLong("job_id"));
                    job[1] = formater.format(new Date(userJobsRS.getLong("start_time") * 1000));  
                    job[2] = state.getProperty(userJobsRS.getString("state_code"));
                    String endCode = userJobsRS.getString("end_code");
                    if(endCode != null)
                        job[3] = end.getProperty(endCode); 
                    else
                        job[3] = "";
                    job[4] = userJobsRS.getString("abstract");
                    jobList.add(job);
                }
                int nJob = jobList.size();
                if(nJob > 0)
                {
                    userJobs = new String[nJob][4];    
                    Vector jobIds = (Vector)req.getSession().getAttribute("JOBIDS");
                    if(jobIds == null)
                        jobIds = new Vector(); 
                    for(int i = 0; i < nJob; i++)
                    {
                        userJobs[i] = (String[])jobList.get(i); 
                        jobIds.add(userJobs[i][0]);
                    }
                    req.getSession().setAttribute("JOBIDS", jobIds);                    
                }
                else
                    messageOut = "No job was found in the database";
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

