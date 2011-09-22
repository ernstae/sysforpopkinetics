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

import java.sql.*;
import java.util.Date;
import java.util.Vector;
import java.util.HashSet;
import java.util.Properties;

/** This class object contains the states of the currently active jobs.
 *
 * @author  Jiaji Du
 */
public class JobState {

        /** Add job history to the database.
     * @param jobId job ID of the job.
     * @param stateCode state code of the job.
     * @param eventTime time of this event.
     * @param stmt statement to execute sql command.
     * @throws SQLException a SQL exception.
     */
    protected void addHistory(String jobId, String stateCode, long eventTime, Statement stmt)
        throws SQLException
    {
        String sql = "insert into history (job_id, state_code, event_time, host) "
	             + "values(" + jobId + ", '" + stateCode + "'," + eventTime
	             + ", '" + JobQueue.localhostName + "')";
	stmt.execute(sql);
    }
    
    /** Set job's state code to the specified value and job's end code to null into the database.
     * @param jobId job ID of the job.
     * @param stateCode state code of the job.
     * @throws SQLException a SQL exception.
     */ 
    protected void setStateCode(String jobId, String stateCode)
        throws SQLException
    {
        long eventTime = (new Date()).getTime()/1000;
        Statement stmt = JobQueue.conn.createStatement();
        String sql = "update job set state_code='" + stateCode + "',end_code=null,event_time=" +
                     eventTime + " where job_id=" + jobId;
        stmt.executeUpdate(sql);
        addHistory(jobId, stateCode, eventTime, stmt);
        stmt.close();
    }
    
    /** Set job's end code to the specified value into the database.
     * @param jobId job ID of the job.
     * @param endCode end code of the job.
     * @throws SQLException a SQL exception.
     */ 
    protected void setEndCode(String jobId, String endCode)
        throws SQLException
    {
        Statement stmt = JobQueue.conn.createStatement();
        String sql;
        if(endCode != null)
            sql = "update job set end_code='" + endCode + "' where job_id=" + jobId;
        else
            sql = "update job set end_code=null where job_id=" + jobId;
        stmt.executeUpdate(sql);
        stmt.close();
    }
    
    /** Abort job.
     * @param jobId job ID of the job.
     * @param stateCode state code of the job.
     * @throws SQLException a SQL exception.
     */
    protected void abortJob(String jobId, String stateCode)
        throws SQLException
    {
        long eventTime = (new Date()).getTime()/1000;
        Statement stmt = JobQueue.conn.createStatement();
        String sql;
        if(stateCode.equals("end"))
            sql = "update job set state_code='end',end_code='abrt',event_time=" +
                  eventTime + ",cpp_source=null where job_id=" + jobId;
        else
            sql = "update job set state_code='" + stateCode + "',event_time=" 
                  + eventTime + ",cpp_source=null where job_id=" + jobId;
        stmt.executeUpdate(sql);
        addHistory(jobId, stateCode, eventTime, stmt);
        stmt.close();
    }
    
    /** Query database to maintain connection.
     * @throws SQLException a SQL exception.
     */
    protected void queryDB()
        throws SQLException
    {
        Statement stmt = JobQueue.conn.createStatement();
        String sql = "select job_id from job where job_id = 1";
        stmt.executeQuery(sql);
        stmt.close();
    }
    
    /** Compiler queue */
    protected Vector<String> cmpQueue = new Vector<String>();
   
    /** Run queue */
    protected Vector<String> runQueue = new Vector<String>();
    
    /** Aborting compiler queue */
    protected Vector<String> abortCmpQueue = new Vector<String>();
    
    /** Aborting run queue */
    protected Vector<String> abortRunQueue = new Vector<String>();
    
    /** Job state list */
    protected Properties jobList = new Properties();
            
    /** Restart job set */
    protected HashSet<String> restartJobs = new HashSet<String>();
    
    /** Compiler daemon visiting flag */
    protected boolean cmpd = false;
    
    /** Run-time daemon visiting flag */
    protected boolean rund = false;
}
