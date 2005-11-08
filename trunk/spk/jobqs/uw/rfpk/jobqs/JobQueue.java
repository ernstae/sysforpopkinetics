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

/** This class defines a server to handle job queues.
 * @author  Jiaji Du
 */
public class JobQueue
{
    /** The main method that runs the application.
     * @param args a String array containing the database host name, the database
     * name, the database username, the database password and the starting job ID.
     */
    public static void main(String[] args)
    {
        dbInfo = args;
        JobState jobState = new JobState();
        if(dbInfo[5] != null && isPosLongNumber(dbInfo[5])) startingJobId = dbInfo[5];
        initQueue(jobState);

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
            e.printStackTrace();
        }
    }
    private static boolean isPosLongNumber(String s)
    {
        long l;
        try
        {
            l = Long.parseLong(s);   
        }
        catch(NumberFormatException e)
        {
            return false;   
        }
        if(l <= 0)
            return false;
        return true;
    }
    private static void initQueue(JobState jobState)
    {	
        String hostName = dbInfo[0];
        String dbName = dbInfo[1];
        String dbUser = dbInfo[2];
        String dbPassword = dbInfo[3];
        Connection conn = null;
        Statement stmt = null;
	try 
        {
	    Class.forName("com.mysql.jdbc.Driver").newInstance();
	    conn = DriverManager.getConnection("jdbc:mysql://" +
					       hostName + "/" +
					       dbName + 
					       "?user=" + dbUser + 
					       "&password=" + dbPassword);
            stmt = conn.createStatement();
            setJobQueue(stmt, jobState, "cmp");
            setJobQueue(stmt, jobState, "q2c");
            setJobQueue(stmt, jobState, "run");
            setJobQueue(stmt, jobState, "q2r");
            setJobQueue(stmt, jobState, "acmp");
            setJobQueue(stmt, jobState, "q2ac");
            setJobQueue(stmt, jobState, "arun");
            setJobQueue(stmt, jobState, "q2ar");
        }
        catch(Exception e)
        {         
        }
        finally
        {
            try
            {
                if(stmt != null) stmt.close();
                if(conn != null) conn.close();
            }
            catch(SQLException e) {}
        }
    }
    private static void setJobQueue(Statement stmt, JobState jobState, String stateCode)
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
                sql = "update job set state_code='q2c',event_time="+
                       eventTime + " where job_id=" + jobId;                   
                stmt.executeUpdate(sql);
                addHistory(jobId, "q2c", eventTime, stmt);
                jobState.jobList.put(jobId, "q2c");
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
                addHistory(jobId, "q2r", eventTime, stmt);
                jobState.jobList.put(jobId, "q2r");
            }
            if(stateCode.equals("acmp") || stateCode.equals("q2ac"))
            {
                sql = "update job set state_code='end',end_code='abrt',event_time=" +
                      eventTime + " where job_id=" + jobId;
                stmt.executeUpdate(sql);
                addHistory(jobId, "end", eventTime, stmt);                
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
                addHistory(jobId, "q2ar", eventTime, stmt);
            }            
        }
    }
    /** Add job history to the database.
     * @param jobId job ID of the job.
     * @param stateCode state code of the job.
     * @param eventTime time of this event.
     * @param stmt statement to execute sql command.
     * @throws SQLException SQL exception.
     */
    protected static void addHistory(String jobId, String stateCode, long eventTime,
                                     Statement stmt)
        throws SQLException                           
    {        
        String sql = "insert into history (job_id, state_code, event_time, host) "
	             + "values(" + jobId + ", '" + stateCode + "'," + eventTime
	             + ", 'unknown')";
	stmt.execute(sql);
    }
    
    /** Database information */
    protected static String[] dbInfo;
    
    private static String startingJobId = "1";
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
    
    /** This is the run method of the thread.
     */
    public void run()
    {
        try
        {
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            String message[];
            String jobId;
            String stateCode;
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
                    if(message[1].equals("q2r"))
                    {
                        jobState.runQueue.add(message[2]);
                        jobState.jobList.setProperty(message[2], "q2r");
                        out.println("done");
                    }
                }
                if(message[0].equals("get"))
                {
                    if(message[1].equals("q2c"))
                    {
                        if(jobState.cmpQueue.size() > 0)
                        {
                            jobId =(String)jobState.cmpQueue.remove(0);
                            jobState.jobList.setProperty(jobId, "cmp");
                            out.println(jobId);
                        }
                        else
                            out.println("none");
                    }
                    if(message[1].equals("q2r"))
                    {
                        if(jobState.runQueue.size() > 0)
                        {
                            jobId =(String)jobState.runQueue.remove(0);
                            jobState.jobList.setProperty(jobId, "run");
                            out.println(jobId);
                        }
                        else
                            out.println("none");
                    }
                    if(message[1].equals("q2ac"))
                    {
                        if(jobState.abortCmpQueue.size() > 0)
                        {
                            jobId = (String)jobState.abortCmpQueue.remove(0);
                            jobState.jobList.remove(jobId);
                            out.println(jobId);
                        }
                        else
                            out.println("none");
                    }
                    if(message[1].equals("q2ar"))
                    {
                        if(jobState.abortRunQueue.size() > 0)
                        {
                            jobId = (String)jobState.abortRunQueue.remove(0);
                            jobState.jobList.remove(jobId);
                            out.println(jobId);
                        }
                        else
                            out.println("none");
                    }
                }
                if(message[0].equals("set"))
                {
                    if(message[1].equals("abrt"))
                    {
                        jobId = message[2];
                        long eventTime = (new Date()).getTime()/1000;
                        stateCode = jobState.jobList.getProperty(jobId);
                        jobState.jobList.remove(jobId);
                        if(stateCode != null)
                        {
                            Connection conn = null;
                            Statement stmt = null;
                            try 
                            {
	                        Class.forName("com.mysql.jdbc.Driver").newInstance();
	                        conn = DriverManager.getConnection("jdbc:mysql://" +
						                   JobQueue.dbInfo[0] + "/" +
						                   JobQueue.dbInfo[1] + 
						                   "?user=" + JobQueue.dbInfo[2] + 
						                   "&password=" + JobQueue.dbInfo[3]);
                                stmt = conn.createStatement();
                                if(stateCode.equals("q2c"))
                                {
                                    String sql = "update job set state_code='end',end_code='abrt',event_time=" +
                                                 eventTime + " where job_id=" + jobId;
                                    stmt.executeUpdate(sql);
                                    jobState.cmpQueue.remove(jobId);                                    
                                    JobQueue.addHistory(jobId, "end", eventTime, stmt);
                                    out.println("done");
                                }
                                else if(stateCode.equals("q2r"))
                                {
                                    String sql = "update job set state_code='end',end_code='abrt',event_time=" +
                                                 eventTime + " where job_id=" + jobId;
                                    stmt.executeUpdate(sql);
                                    jobState.runQueue.remove(jobId);
                                    JobQueue.addHistory(jobId, "end", eventTime, stmt);
                                    out.println("done");
                                }
                                else if(stateCode.equals("cmp"))
                                {
                                    String sql ="update job set state_code='q2ac',event_time=" + eventTime +
                                                " where job_id=" + jobId;
                                    stmt.executeUpdate(sql);
                                    jobState.abortCmpQueue.add(jobId);
                                    JobQueue.addHistory(jobId, "q2ac", eventTime, stmt);
                                    out.println("done");
                                }
                                else if(stateCode.equals("run"))
                                {
                                    String sql ="update job set state_code='q2ar',event_time=" + eventTime +
                                                " where job_id=" + jobId;
                                    stmt.executeUpdate(sql);
                                    jobState.abortRunQueue.add(jobId);
                                    JobQueue.addHistory(jobId, "q2ar", eventTime, stmt);
                                    out.println("done");
                                }
                                else
                                    out.println("none");
                            }
                            catch(Exception e)
                            {  
                            }
                            finally
                            {
                                if(stmt != null) stmt.close();
                                if(conn != null) conn.close();
                            }
                        }
                    }
                    if(message[1].equals("end"))
                    {
                        jobState.jobList.remove(message[2]);
                        out.println("done");
                    }
                }
                if(message[0].equals("Hi"))
                    out.println("Hi");
                }
            }
        }
        catch(Exception e)
        {
        }
    }
    
    private Socket socket;
    private JobState jobState;
}