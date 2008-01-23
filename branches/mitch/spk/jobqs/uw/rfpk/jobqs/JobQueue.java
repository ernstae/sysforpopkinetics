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
package uw.rfpk.jobqs;

import java.io.*;
import java.sql.*;
import java.net.*;
import java.util.Date;
import java.util.Vector;
import java.util.Properties;
import java.util.Enumeration;
import java.text.SimpleDateFormat;

/** This class defines a server to handle job queues.  The server keeps four job-queues: 
 *  Compiler queue, Run queue, Aborting compiler queue and Aborting run queue.  The server 
 *  also keeps a hash map called JobList with job ID as key and job state_code as value.
 *  <p>
 *  <b>Initialization</b><br>
 *  The main method takes a String array of 6 elements.  The first 4 of them are for the 
 *  database connection.  The fifth one is the server's port and the last one is the job ID 
 *  of the job to start initialize the server with the database.
 *  <P>
 *  For any job having state_code "q2c", add the job ID to the Compiler queue, and add an
 *  element to the JobList with key of the job ID and value of "q2c".
 *  <P>
 *  For any job having state_code "cmp", add the job ID to the Compiler queue, add an
 *  element to the JobList with key of the job ID and value of "q2c", and change the job's 
 *  state_code to "q2c" in the database.
 *  <P>
 *  For any job having state_code "q2r", add the job ID to the Compiler queue, and add an
 *  element to the JobList with key of the job ID and value of "q2r"
 *  <P>
 *  For any job having state_code "run", add the job ID to the Compiler queue, add an 
 *  element to the JobList with key of the job ID and value of "q2r", and change the job's 
 *  state_code to "q2r" in the database.
 *  <P>
 *  For any job having state_code "q2ac", add the job ID to the Aborting compiler queue,
 *  and add an element to the JobList with key of the job ID and value of "q2ac".
 *  <P>
 *  For any job having state_code "acmp", add the job ID to the Aborting Compiler queue,
 *  and add an element to the JobList with key of the job ID and value of "q2ac", and 
 *  change the job's state_code to "q2ac" in the database.
 *  <P>
 *  For any job having state_code "q2ar", add the job ID to the Aborting run queue
 *  and add an element to the JobList with key of the job ID and value of "q2ar".
 *  <P>
 *  For any job having state_code "arun", add the job ID to the Aborting run queue, and 
 *  addan element to the JobList with key of the job ID and value of "q2ar" and change 
 *  the job's state_code to "q2ar" in the database.<br>
 *  <p>
 *  <b>Request Handling</b><br>
 *  The italic text represent an variable.<br>
 *  Request:   "add-q2c-"<i>job_ID</i><br>
 *  Handling:  Add the job ID to the Compiler queue, and add an element to the JobList 
 *             with key of the job ID and value of "q2c".  The job's state code in the 
 *             database has been set to "q2c" by the Web server.<br>
 *  Response:  "done"
 *  <p>
 *  Request:   "add-q2r-"<i>job_ID</i><br>
 *  Handling:  Add the job ID to the Run queue, and add an element to the JobList with 
 *             key of the job ID and value of "q2r".  The job's state code in the 
 *             database has been set to "q2r" by the Web server.<br>
 *  Response:  "done"
 *  <p>
 *  Request:   "get-q2c"<br>
 *  Handling:  If the Compiler queue contains any job, remove the first job ID from the 
 *             queue, and set the job's state code to "cmp" in the JobList.  The job's
 *             state code in the database will be set to "cmp" by the Compiler daemon.<br>
 *  Response:  <i>job_ID</i> if the job is available, "none" if otherwise.
 *  <p>
 *  Request:   "get-q2r"<br>
 *  Handling:  If the Run queue contains any job, remove the first job ID from the 
 *             queue, and set the job's state code to "run" in the JobList. The job ID may 
 *             be started with a space character.  The space character is removed.  The job's
 *             state code in the database will be set to "run" by the Run-time daemon.<br>
 *  Response:  <i>job_ID</i> if the job is available, "none" if otherwise.
 *  <p>
 *  Request:   "get-q2ac"<br>
 *  Handling:  If the Aborting compiler queue contains any job, remove the first job ID from the 
 *             queue, and set the job's state code to "acmp" in the JobList.  The job's
 *             state code in the database will be set to "acmp" by the Compiler daemon.<br>
 *  Response:  <i>job_ID</i> if the job is available, "none" if otherwise.
 *  <p>
 *  Request:   "get-q2ar"<br>
 *  Handling:  If the Aborting run queue contains any job, remove the first job ID from the 
 *             queue, and set the job's state code to "arun" in the JobList.  The job's
 *             state code in the database will be set to "arun" by the Run-time daemon.<br>
 *  Response:  <i>job_ID</i> if the job is available, "none" if otherwise.
 *  <p>
 *  Request:   "set-abrt-"<i>job_ID</i><br>
 *  Handling:  If the job's state code in the JobList is "q2c", remove it from the Compiler queue 
 *             and the JobList, and set the job's state code to "end" and the job's
 *             end code to "abrt" in the database.
 *             <p>
 *             If the job's state code in the JobList is "cmp", add the job ID to the Aborting  
 *             compiler queue, set the job's state code to "q2ac" in the JobList, and in the 
 *             database.
 *             <p>
 *             If the job's state code in the JobList is "q2r", remove it from the Run queue 
 *             and the JobList, and set the job's state code to "end" and the job's
 *             end code to "abrt" in the database.
 *             <p>
 *             If the job's state code in the JobList is "run", add the job ID to the Aborting  
 *             run queue, set the job's state code to "q2ar" in the JobList, and in the 
 *             database.
 *             <br>
 *  Response:  "done" if the job's state code is either "q2c", "cmp", "q2r" or "run", "none" if
 *             otherwise
 *  <p>
 *  Request:   "set-end"<i>job_ID</i><br>
 *  Handling:  Remove the job from all the queues and the JobList if the job's job ID is 
 *             contained in any of these containers.  In the database the job's state code will be
 *             set to "end" by either the Compiler daemon or the Run-time daemon.<br>
 *  Response:  "done" 
 *  <p>
 *  Request:   "init-cmpd"<br>
 *  Handling:  If the jobList contains any job with state code either "q2c" or "q2ac", set these
 *             jobs' end code to "null" in the database.
 *             <p>
 *             If the jobList contains any job with state code "cmp", add these job's job ID to
 *             the Compiler queue, set these jobs' state code to "q2c" in the JobList and in the
 *             database, and set these jobs' end code to "null" in the database.
 *             <p>
 *             If the jobList contains any job with state code "acmp", add these job's job ID to
 *             the Aborting compiler queue, set these jobs' state code to "q2ac" in the JobList 
 *             and in the database, and set these jobs' end code to "null" in the database.
 *             <br>
 *  Response:  "done"
 *  <p>
 *  Request:   "init-rund"<br>
 *  Handling:  If the jobList contains any job with state code either "q2cr" or "q2ar", set these
 *             jobs' end code to "null" in the database.
 *             <p>
 *             If the jobList contains any job with state code "run", add these job's job ID to
 *             the Run queue with an space character added to the front of the job ID, set these 
 *             jobs' state code to "q2r" in the JobList and in the database, and set these jobs' 
 *             end code to "null" in the database.
 *             <p>
 *             If the jobList contains any job with state code "arun", add these job's job ID to
 *             the Aborting run queue, set these jobs' state code to "q2ar" in the JobList 
 *             and in the database, and set these jobs' end code to "null" in the database.
 *             <br>
 *  Response:  "done"
 *  <p>
 *  Request:   "list-q2c"<br>
 *  Handling:  Do nothing.<br>
 *  Response:  A list of <i>job_ID</i> " - q2c" for all jobs in the Compiler queue, "none" if otherwise.
 *  <p>
 *  Request:   "list-q2r"<br>
 *  Handling:  Do nothing.<br>
 *  Response:  A list of <i>job_ID</i> " - q2r" for all jobs in the Run queue, "none" if otherwise.
 *  <p>
 *  Request:   "list-q2ac"<br>
 *  Handling:  Do nothing.<br>
 *  Response:  A list of <i>job_ID</i> " - q2ac" or all jobs in the Aborting compiler queue, "none" if otherwise.
 *  <p>
 *  Request:   "list-q2ar"<br>
 *  Handling:  Do nothing.<br>
 *  Response:  A list of <i>job_ID</i> " - q2ar" for all jobs in the Aborting run queue, "none" if otherwise.
 *  <p>
 *  Request:   "list-all"<br>
 *  Handling:  Do nothing.<br>
 *  Response:  A list of <i>job_ID - end_code</i> for all jobs in the JobList, "none" if otherwise.
 *  <p>
 *  Request:   "Hi"<br>
 *  Handling:  Do nothing.<br>
 *  Response:  "Hi"
 *  <p>
 *  Request:   "close"<br>
 *  Handling:  Close the connection socket.<br>
 *  Response:  "closing"
 *
 * @author  Jiaji Du
 */
public class JobQueue
{
    /** The main method that runs the application.
     * @param args a String array containing the database host name, the database name, the
     * database username, the database password, the server port and the starting job ID.
     */
    public static void main(String[] args)
    {       
        JobState jobState = new JobState();

        // Get localhost name
        try
        {
            localhostName = InetAddress.getLocalHost().getHostName();
        }
        catch (UnknownHostException e)
        {
            stop("Localhost", e.getMessage());
        }

        // Connect to the database
        String hostName = args[0];
        String dbName = args[1];
        String dbUser = args[2];
        String dbPassword = args[3];
        mode = args[5];
        
        try 
        {
	    Class.forName("com.mysql.jdbc.Driver").newInstance();
	    conn = DriverManager.getConnection("jdbc:mysql://" +
					       hostName + "/" +
                                               dbName +                     //dbName + ":autoReconnect=true" +
					       "?user=" + dbUser + 
					       "&password=" + dbPassword);
        }
        catch(Exception e)
        {
            stop("Database connection", e.getMessage());
        }

        // Initialize queues
        initQueue(jobState);

        // Start monitor
        Thread monitor = new Monitor(jobState);
        monitor.setDaemon(true);
        monitor.start();

        // Start server
        try
        {
            ServerSocket s = new ServerSocket(Integer.parseInt(args[4]));
            while(true)
            {
                Socket incoming = s.accept();
                Thread t = new ThreadedHandler(incoming, jobState);
                t.start();
            }     
        }
        catch(IOException e)
        {
            stop("New server socket and new threads", e.getMessage());
        }
        
        // Disconnect from the database
        try
        {
            if(conn != null) conn.close();
        }
        catch(SQLException e)
        {
            stop("Database disconnection", e.getMessage());
        }
    }
    protected static void stop(String location, String errorMessage)
    {
        // Disconnect from the database
        try
        {
            if(conn != null) conn.close();
        }
        catch(SQLException e)
        {
        }
        // Put a log file in the /tmp directory 
        SimpleDateFormat formatter = new SimpleDateFormat("EEE, MMM, d yyyy 'at' HH:mm:ss z");
        String time = formatter.format(new Date());
        String filename = "/tmp/log_jobq";
        if(mode.equals("test")) filename += "testd";
        else filename += "d";
        File logFile = new File(filename);
        try
        {
            BufferedWriter writer = new BufferedWriter(new FileWriter(logFile));
            writer.write(time + "\n" + location + "\n" + errorMessage);
            writer.flush();
        }
        catch(IOException e) {}
        finally
        {
            File lockFile = new File("/tmp/lock_jobqtestd");
            lockFile.delete();
            System.exit(0);
        }
    }
    private static void initQueue(JobState jobState)
    {
        Statement stmt = null;
	try
        {
            stmt = JobQueue.conn.createStatement();
            String sql = "select job_id from job where state_code != 'end' order by job_id limit 1";
            ResultSet rs = stmt.executeQuery(sql);
            if(rs.next())
            {
                String startingJobId = String.valueOf(rs.getLong("job_id"));
                setJobQueue(stmt, jobState, "cmp", startingJobId);
                setJobQueue(stmt, jobState, "q2c", startingJobId);
                setJobQueue(stmt, jobState, "run", startingJobId);
                setJobQueue(stmt, jobState, "q2r", startingJobId);
                setJobQueue(stmt, jobState, "acmp", startingJobId);
                setJobQueue(stmt, jobState, "q2ac", startingJobId);
                setJobQueue(stmt, jobState, "arun", startingJobId);
                setJobQueue(stmt, jobState, "q2ar", startingJobId);
            }
        }
        catch(SQLException e)
        {
            stop("Server initialization", e.getMessage());
        }
        finally
        {
            try
            {             
                if(stmt != null) stmt.close();
            }
            catch(SQLException e)
            {
                stop("Statement closing", e.getMessage());
            }
        }
    }
    private static void setJobQueue(Statement stmt, JobState jobState, String stateCode, String startingJobId)
        throws SQLException
    {      
        String jobId;
        long eventTime;
        String sql = "select job_id from job where job_id>=" + startingJobId +
                     " and state_code='" + stateCode + "' order by job_id;";
	ResultSet rs = stmt.executeQuery(sql);
        while(rs.next())
        {
            eventTime = (new Date()).getTime()/1000;
            jobId = String.valueOf(rs.getLong("job_id"));
            if(stateCode.equals("q2c"))
            {
                jobState.cmpQueue.add(jobId);
                jobState.jobList.put(jobId, "q2c");
            }
            if(stateCode.equals("cmp"))
            {
                sql = "update job set state_code='q2c',event_time=" +
                       eventTime + " where job_id=" + jobId;
                stmt.executeUpdate(sql);
                jobState.addHistory(jobId, "q2c", eventTime, stmt);
            }
            if(stateCode.equals("q2r"))
            {
                jobState.runQueue.add(jobId);
                jobState.jobList.put(jobId, "q2r");
            }
            if(stateCode.equals("run"))
            {
                sql = "update job set state_code='q2r',event_time=" +
                      eventTime + " where job_id=" + jobId;
                stmt.executeUpdate(sql);
                jobState.addHistory(jobId, "q2r", eventTime, stmt);
            }
            if(stateCode.equals("q2ac"))
            {
                jobState.abortCmpQueue.add(jobId);
                jobState.jobList.put(jobId, "q2ac");
            }
            if(stateCode.equals("acmp"))
            {
                sql = "update job set state_code='q2ac',event_time=" +
                      eventTime + " where job_id=" + jobId;
                stmt.executeUpdate(sql);
                jobState.addHistory(jobId, "q2ac", eventTime, stmt);
            }
            if(stateCode.equals("q2ar"))
            {
                jobState.abortRunQueue.add(jobId);
                jobState.jobList.put(jobId, "q2ar");
            }
            if(stateCode.equals("arun"))
            {
                sql = "update job set state_code='q2ar',event_time=" +
                      eventTime + " where job_id=" + jobId;
                stmt.executeUpdate(sql);
                jobState.addHistory(jobId, "q2ar", eventTime, stmt);
            }
        }
    }

    /** Database connection object */
    protected static Connection conn;
    
    private static String mode;
    
    /** Local host name */
    protected static String localhostName = "unknown";
}


/** This class defines a thread to handle request.
 * @author  jiaji Du
 */
class ThreadedHandler extends Thread
{
    /** Construct a ThreadHandler object.
     * @param jobState JobState object containing job state information.
     * @param i socket for receiving and sending messages.
     */    
    public ThreadedHandler(Socket i, JobState jobState)
    {
        socket = i;
        this.jobState = jobState;
    }
    
    /** This is the run method of the thread. It handles requests from client
     */
    public void run()
    {
        try
        {
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            String[] message;
            while(true)
            {
                message = in.readLine().split("-");
                synchronized(jobState)
                {                   
                    if(message[0].equals("add"))
                    {
                        if(message[1].equals("q2c"))
                        {
                            jobState.cmpQueue.add(message[2]);
                            jobState.jobList.setProperty(message[2], "q2c");
                            out.println("done");
                        }
                        else if(message[1].equals("q2r"))
                        {
                            jobState.runQueue.add(message[2]);
                            jobState.jobList.setProperty(message[2], "q2r");
                            out.println("done");
                        }
                        else
                        {
                            out.println("N/A");
                        }
                    }
                    else if(message[0].equals("get"))
                    {
                        if(message[1].equals("q2c"))
                        {
                            if(jobState.cmpQueue.size() > 0)
                            {
                                String jobId =(String)jobState.cmpQueue.remove(0);
                                jobState.jobList.setProperty(jobId, "cmp");
                                out.println(jobId);
                            }
                            else
                            {
                                out.println("none");
                            }
                        }
                        else if(message[1].equals("q2r"))
                        {
                            if(jobState.runQueue.size() > 0)
                            {
                                String jobId =(String)jobState.runQueue.remove(0);
                                jobState.jobList.setProperty(jobId, "run");
                                if(jobState.restartJobs.remove(jobId)) out.println(" " + jobId);
                                else out.println(jobId);
                            }
                            else
                            {
                                out.println("none");
                            }
                        }
                        else if(message[1].equals("q2ac"))
                        {
                            jobState.cmpd = true;
                            if(jobState.abortCmpQueue.size() > 0)
                            {
                                String jobId = (String)jobState.abortCmpQueue.remove(0);
                                jobState.jobList.setProperty(jobId, "acmp");
                                out.println(jobId);
                            }
                            else
                            {
                                out.println("none");
                            }
                        }
                        else if(message[1].equals("q2ar"))
                        {
                            jobState.rund = true;
                            if(jobState.abortRunQueue.size() > 0)
                            {
                                String jobId = (String)jobState.abortRunQueue.remove(0);
                                jobState.jobList.setProperty(jobId, "arun");
                                out.println(jobId);
                            }
                            else
                            {
                                out.println("none");
                            }
                        }
                        else
                        {
                            out.println("N/A");
                        }
                    }
                    else if(message[0].equals("init"))
                    {
                        if(message[1].equals("cmpd"))
                        {
                            jobState.cmpd = true;
                            Enumeration keys = jobState.jobList.keys();
                            String jobId, stateCode;
                            while(keys.hasMoreElements())
                            {
                                jobId = (String)keys.nextElement();
                                stateCode = jobState.jobList.getProperty(jobId);
                                if(stateCode.equals("q2c") || stateCode.equals("q2ac"))
                                {
                                    jobState.setEndCode(jobId, null);
                                }
                                if(stateCode.equals("cmp"))
                                {
                                    jobState.cmpQueue.insertElementAt(jobId, 0);
                                    jobState.jobList.setProperty(jobId, "q2c");
                                    jobState.setStateCode(jobId, "q2c");
                                }
                                if(stateCode.equals("acmp"))
                                {
                                    jobState.abortCmpQueue.insertElementAt(jobId, 0);
                                    jobState.jobList.setProperty(jobId, "q2ac");
                                    jobState.setStateCode(jobId, "q2ac");
                                }                               
                            }
                            out.println("done");
                        }
                        else if(message[1].equals("rund"))
                        {
                            jobState.rund = true;
                            Enumeration keys = jobState.jobList.keys();
                            String jobId, stateCode;
                            while(keys.hasMoreElements())
                            {
                                jobId = (String)keys.nextElement();
                                stateCode = jobState.jobList.getProperty(jobId);
                                if(stateCode.equals("q2r") || stateCode.equals("q2ar"))
                                {
                                    jobState.setEndCode(jobId, null);
                                }
                                if(stateCode.equals("run"))
                                {
                                    jobState.runQueue.insertElementAt(jobId, 0);                                   
                                    jobState.jobList.setProperty(jobId, "q2r");
                                    jobState.restartJobs.add(jobId);
                                    jobState.setStateCode(jobId, "q2r");
                                }
                                if(stateCode.equals("arun"))
                                {
                                    jobState.abortRunQueue.insertElementAt(jobId, 0);
                                    jobState.jobList.setProperty(jobId, "q2ar");
                                    jobState.setStateCode(jobId, "q2ar");
                                }                                
                            }
                            out.println("done");
                        }
                        else
                        {
                            out.println("N/A");   
                        }
                    }
                    else if(message[0].equals("set"))
                    {
                        String stateCode = jobState.jobList.getProperty(message[2]);
                        String jobId = message[2];
                        if(message[1].equals("abrt"))
                        {
                            if(stateCode != null)
                            {
                                if(stateCode.equals("q2c"))
                                {
                                    jobState.cmpQueue.remove(jobId);
                                    jobState.jobList.remove(jobId);                                   
                                    jobState.abortJob(jobId, "end");
                                    out.println("done");
                                }
                                else if(stateCode.equals("q2r"))
                                {
                                    jobState.runQueue.remove(jobId);
                                    jobState.jobList.remove(jobId);
                                    jobState.restartJobs.remove(jobId);
                                    jobState.abortJob(jobId, "end");
                                    out.println("done");
                                }
                                else if(stateCode.equals("cmp"))
                                {
                                    jobState.abortCmpQueue.add(jobId);
                                    jobState.jobList.setProperty(jobId, "q2ac");
                                    jobState.abortJob(jobId, "q2ac");
                                    out.println("done");
                                }
                                else if(stateCode.equals("run"))
                                {
                                    jobState.abortRunQueue.add(jobId);
                                    jobState.jobList.setProperty(jobId, "q2ar");                                   
                                    jobState.abortJob(jobId, "q2ar");
                                    out.println("done");
                                }
                                else
                                {
                                    out.println("none");
                                }
                            }
                            else
                            {
                                out.println("none");
                            }
                        }
                        else if(message[1].equals("end"))
                        {
                            jobState.jobList.remove(jobId);
                            jobState.restartJobs.remove(jobId);
                            if(stateCode.equals("q2c")) jobState.cmpQueue.remove(jobId);
                            if(stateCode.equals("q2r")) jobState.runQueue.remove(jobId);
                            if(stateCode.equals("q2ac")) jobState.abortCmpQueue.remove(jobId);
                            if(stateCode.equals("q2ar")) jobState.abortRunQueue.remove(jobId);
                            out.println("done");
                        }
                        else
                        {
                            out.println("N/A");
                        }
                    }
                    else if(message[0].equals("list") && message.length == 2)
                    {
                        if(message[1].equals("q2c"))
                        {
                            int size = jobState.cmpQueue.size();
                            if(size == 0)
                            {
                                out.println("none");
                            }
                            else
                            {
                                String list = "";
                                for(int i = 0; i < size; i++)
                                    list += jobState.cmpQueue.get(i) + " - q2c\n";
                                out.println(list.trim());
                            }
                        }
                        else if(message[1].equals("q2r"))
                        {
                            int size = jobState.runQueue.size();
                            if(size == 0)
                            {
                                out.println("none");
                            }
                            else
                            {
                                String list = "";
                                for(int i = 0; i < size; i++)
                                    list += jobState.runQueue.get(i) + " - q2r\n";
                                out.println(list.trim());
                            }
                        }
                        else if(message[1].equals("q2ac"))
                        {
                            int size = jobState.abortCmpQueue.size();
                            if(size == 0)
                            {
                                out.println("none");
                            }
                            else
                            {
                                String list = "";
                                for(int i = 0; i < size; i++)
                                    list += jobState.abortCmpQueue.get(i) + " - q2ac\n";
                                out.println(list.trim());
                            }
                        }
                        else if(message[1].equals("q2ar"))
                        {
                            int size = jobState.abortRunQueue.size();
                            if(size == 0)
                            {
                                out.println("none");
                            }
                            else
                            {
                                String list = "";
                                for(int i = 0; i < size; i++)
                                    list += jobState.abortRunQueue.get(i) + " - q2ar\n";
                                out.println(list.trim());
                            }
                        }
                        else if(message[1].equals("all"))
                        {
                            if(jobState.jobList.isEmpty())
                            {
                                out.println("none");
                            }
                            else
                            {
                                String list = "";
                                Enumeration keys = jobState.jobList.keys();
                                String jobId;
                                while(keys.hasMoreElements())
                                {
                                    jobId = (String)keys.nextElement();
                                    list += jobId + " - " + jobState.jobList.getProperty(jobId) + "\n";
                                }
                                out.println(list.trim());
                            }
                        }
                        else
                        {
                            out.println("N/A");
                        }
                    }
                    else if(message[0].equals("Hi"))
                    {
                        out.println("Hi");
                    }
                    else if(message[0].equals("close"))
                    {
                        out.println("closing");
                        socket.close();
                    }
                    else
                    {
                        out.println("N/A");
                    }
                }
            }
        }
        catch(Exception e)
        {
            if(e.getMessage() != null) JobQueue.stop("Request handling", e.getMessage());
        }
    }
   
    private Socket socket;
    private JobState jobState;
}

/** This class defines a thread to monitor the hosts.
 * @author  jiaji Du
 */
class Monitor extends Thread
{
    /** Construct a Monitor object.
     * @param jobState JobState object containing job state information.
     */    
    public Monitor(JobState jobState)
    {
        this.jobState = jobState;
    }
    
    /** This is the run method of the thread. It monitors the hosts
     */
    public void run()
    {
        int time = 0;
        while(true)
        {
            try
            {
                sleep(10000);
            }
            catch(InterruptedException e)
            {
                JobQueue.stop("Monitor", e.getMessage());
            }
            synchronized(jobState)
            {
                time++;
                if(time == 360)
                {
                    time = 0; 
                    try
                    {
                        jobState.queryDB();
                    }
                    catch(SQLException e)
                    {
                        JobQueue.stop("DBConnect query", e.getMessage());
                    }
                }
                if(!jobState.cmpd)
                {
                    Enumeration keys = jobState.jobList.keys();
                    String jobId, stateCode;
                    while(keys.hasMoreElements())
                    {
                        jobId = (String)keys.nextElement();
                        stateCode = jobState.jobList.getProperty(jobId);
                        if(stateCode.equals("q2c") || stateCode.equals("q2ac") ||
                            stateCode.equals("cmp") || stateCode.equals("acmp"))
                        {
                            try
                            {
                                jobState.setEndCode(jobId, "spku");
                            }
                            catch(SQLException e)
                            {
                                JobQueue.stop("Compiler daemon spku", e.getMessage());
                            }
                        }
                    }
                }
                else
                {
                    jobState.cmpd = false;
                }
                if(!jobState.rund)
                {
                    Enumeration keys = jobState.jobList.keys();
                    String jobId, stateCode;
                    while(keys.hasMoreElements())
                    {
                        jobId = (String)keys.nextElement();
                        stateCode = jobState.jobList.getProperty(jobId);
                        if(stateCode.equals("q2r") || stateCode.equals("q2ar") ||
                           stateCode.equals("run") || stateCode.equals("arun"))
                        {
                            try
                            {
                                jobState.setEndCode(jobId, "spku");
                            }
                            catch(SQLException e)
                            {
                                JobQueue.stop("Run-time daemon spku", e.getMessage());
                            }
                        }
                    }
                }
                else
                {
                    jobState.rund = false;
                }
            }
        }
    }
    private JobState jobState;
}
