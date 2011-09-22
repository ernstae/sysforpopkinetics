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
package uw.rfpk.mda;

import java.io.*;
import java.sql.*;
import java.util.Vector;
import java.util.Properties;
import java.text.SimpleDateFormat;
import javax.swing.JOptionPane;

/** This class makes calls to the server for the transactions required by the model
 *  design agent of the SPK.  
 *
 * @author  Jiaji Du
 */
public class Server { 
    /** Constructor to initiate the instant variables of the class.
     * @param args A String array containning server host name, server port number,
     * session ID and secret code.
     */
    public Server(String[] args)
    {
        this.serverHost = args[0];
        this.serverPort = args[1];
        this.sessionId = args[2];
        this.secret = args[3];
    }
    
    /** Get server host name.
     * @return server host name as a String object.
     */
    public String getHost(){return serverHost;}
    
    /** Get server port number.
     * @return server port number as a String object.
     */    
    public String getPort(){return serverPort;}
    
    /** Submit a SPK job to the database server.
     * @param source Spk source XML document.
     * @param jobAbstract short description of the job.
     * @param modelInfo archive information of the model associated with the job.
     * @param dataInfo archive information of the dataset associated with the job.
     * @param jobMethodCode a String containing job method code.
     * @param jobParent a long representing the id of the parent job.
     * @param isWarmStart a boolean "true" for warm start, "false" for otherwise.
     * @param isMailNotice a boolean "true" for sending end-job mail notice, "false" for otherwise.
     * @param nTasks number of sub-tasks in parallel processing or 0 for single processing.
     * @param myName username.
     * @return true if the job submission was successful, false if otherwise.
     */
    public boolean submitJob(String source, String jobAbstract, ArchiveInfo modelInfo, ArchiveInfo dataInfo, 
                             String jobMethodCode, long jobParent, boolean isWarmStart, boolean isMailNotice,
                             int nTasks, String myName)
    {
        String[] messageOut = new String[25];
        messageOut[0] = secret;
        messageOut[1] = source;
        if(dataInfo.isNewArchive || dataInfo.isNewVersion)
            messageOut[2] = dataInfo.text;
        else
            messageOut[2] = "";
        if(modelInfo.isNewArchive || modelInfo.isNewVersion)
            messageOut[3] = modelInfo.text;
        else
            messageOut[3] = "";
        messageOut[4] = jobAbstract;
        messageOut[5] = modelInfo.description;
        messageOut[6] = modelInfo.log;
        messageOut[7] = modelInfo.name;
        messageOut[8] = modelInfo.version;        
        messageOut[9] = String.valueOf(modelInfo.id);
        messageOut[10] = String.valueOf(modelInfo.isNewArchive);
        messageOut[11] = String.valueOf(modelInfo.isNewVersion);
        messageOut[12] = dataInfo.description;
        messageOut[13] = dataInfo.log;
        messageOut[14] = dataInfo.name;
        messageOut[15] = dataInfo.version;        
        messageOut[16] = String.valueOf(dataInfo.id);
        messageOut[17] = String.valueOf(dataInfo.isNewArchive);
        messageOut[18] = String.valueOf(dataInfo.isNewVersion);
        messageOut[19] = jobMethodCode;
        messageOut[20] = String.valueOf(jobParent);
        messageOut[21] = String.valueOf(isWarmStart);
        messageOut[22] = myName;
        messageOut[23] = String.valueOf(isMailNotice);
        messageOut[24] = String.valueOf(nTasks);

        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.SubmitJob",
                                          sessionId);
            String messages = (String)network.talk(messageOut);
            if(messages == null)
                return false;
            JOptionPane.showMessageDialog(null, messages,    
                                          "Database Information",         
                                          JOptionPane.INFORMATION_MESSAGE);
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }
        return true;
    }        

    /** Send a message to the server to abort a job.
     * @param jobId id number of the job.
     * @return true if the job's state_code is set to "end" by this method, otherwise false.
     */
    public boolean abortJob(long jobId)
    {
        String[] messageOut = new String[2];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
        String success = "false";
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.AbortJob",
                                          sessionId);
            success = (String)network.talk(messageOut);
         }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }
        return success != null && success.equals("true");
    }
    
    /** Send a message to the server to set job abstract.
     * @param jobId id number of the job.
     * @param jobAbstract abstract field of the job to set.
     * @return true if the job's abstract has been successfully updated, otherwise false.
     */
    public boolean setJobAbstract(long jobId, String jobAbstract)
    {
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
        messageOut[2] = jobAbstract;
        String success = "false";
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.SetJobAbstract",
                                          sessionId);
            success = (String)network.talk(messageOut);
         }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }
        return success != null && success.equals("true");
    }
    
    /** Send a message to the server to set job share_with.
     * @param jobId id number of the job.
     * @param shareWithName username of the user with whom the job is to share.
     * @return true if the job's share_wide has been successfully set, otherwise false.
     */
    public boolean setJobShareWith(long jobId, String shareWithName)
    {
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
        messageOut[2] = shareWithName;
        String success = "false";
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.SetJobShareWith",
                                          sessionId);
            success = (String)network.talk(messageOut);
         }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }
        return success != null && success.equals("true");
    }
    
    /** Get a sequence of jobs for a given user.
     * @param maxNum maximum number of jobs to provide status for.
     * @param leftOff least jobId previously returned (0 if first call in sequence).
     * @param username job owner's username.
     * @param startID starting jobID.
     * @param startTime starting submission time.
     * @param keyWords key words either in job abstract, in model name on in dataset name.
     * @param modelID finding jobs that use this model.
     * @param datasetID finding jobs that use thos dataset.
     * @return a String[][] object that contains job id, start time(date format), 
     *         state code(long format), end code(long format) and job abstract of the jobs.  
     *         The first index of the array is the job sequence in reversed order.  
     *         The second index designates the fields. 
     *         null if failed.         
     */
    public String[][] getUserJobs(int maxNum, long leftOff, String username,
                                  String startID, String startTime, String keyWords, 
                                  String modelID, String datasetID)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[9];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(maxNum);
        messageOut[2] = String.valueOf(leftOff);
        messageOut[3] = username;
        messageOut[4] = startID;
        messageOut[5] = startTime;
        messageOut[6] = keyWords;
        messageOut[7] = modelID;
        messageOut[8] = datasetID;
        
        // Prepare for the return
        String jobList[][] = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.UserJobs",
                                          sessionId);
            jobList = (String[][])network.talk(messageOut);
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",
                                          "Network Error",
                                          JOptionPane.ERROR_MESSAGE);
        }
        return jobList;    
    }

    /** Get job information including model name, model version dataset name dataset version. 
     * @param jobId id number of the job.    
     * @return a Properties object containing the job information including 
     * model name, model version dataset name dataset version.  null if failed.
     */
    public Properties getJobInfo(long jobId)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[2];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
                
        // Prepare for the return
        Properties jobInfo = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.GetJobInfo",
                                          sessionId);
            jobInfo = (Properties)network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",
                                          JOptionPane.ERROR_MESSAGE);
        }
        return jobInfo;            
    }    

    /** Get either model archive or dataset archive for the job. 
     * @param jobId id number of the job.
     * @param type either "model" or "data" to specify the type.     
     * @return a Properties object containing the archive text, name and version.
     */
    public Properties getJobArchive(long jobId, String type)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
        messageOut[2] = type;
         
        // Prepare for the return
        Properties archive = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.GetJobArchive",
                                          sessionId);
            archive = (Properties)network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",
                                          JOptionPane.ERROR_MESSAGE);
        }
        return archive;            
    }
    
    /** Get SPK input data. 
     * @param jobId id number of the job.    
     * @return a Properties object containing the SPK input data. 
     *         null if failed.
     */
    public Properties getInput(long jobId)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[2];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);        
        
        // Prepare for the return
        Properties spkInput = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.GetInput",
                                          sessionId);
            spkInput = (Properties)network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",
                                          JOptionPane.ERROR_MESSAGE);
        }
        return spkInput;            
    }
        
    /** Get SPK output data. 
     * @param jobId id number of the job.    
     * @return a Properties object containing the SPK output data. 
     *         null if failed.
     */
    public Properties getOutput(long jobId)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[2];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);       
        
        // Prepare for the return
        Properties spkOutput = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.GetOutput",
                                          sessionId);
            spkOutput = (Properties)network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",
                                          JOptionPane.ERROR_MESSAGE);
        }
        return spkOutput;            
    }
    
    /** Get job optimization trace. 
     * @param jobId id number of the job.    
     * @return a String object containing the job optimization trace.
     *         null if failed.
     */
    public String getTrace(long jobId)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[2];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
                
        // Prepare for the return
        String trace = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.GetTrace",
                                          sessionId);
            trace = (String)network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",
                                          JOptionPane.ERROR_MESSAGE);
        }
        return trace;            
    }
    
    /** Get job history. 
     * @param jobId id number of the job.    
     * @return a String[][] object containing the job history including event time, 
     * state code and host. 
     *         null if failed.
     */
    public String[][] getHistory(long jobId)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[2];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
                
        // Prepare for the return
        String[][] jobHistory = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.GetHistory",
                                          sessionId);
            jobHistory = (String[][])network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",
                                          JOptionPane.ERROR_MESSAGE);
        }
        return jobHistory;            
    }    
    
    /** Get a sequence of models for a given user.
     * @return a String[][] object that contains model id, model name,
     *         last revision time(date format), and model abstract of the models.
     *         The first index of the array is the model sequence in reversed order.
     *         The second index designates the fields.
     *         null if failed.
     * @param maxNum maximum number of models to provide status for.
     * @param leftOff least modelId previously returned (0 if first call in sequence).
     * @param username model owner's username.
     */
    public String[][] getUserModels(int maxNum, long leftOff, String username)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[5];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(maxNum);
        messageOut[2] = String.valueOf(leftOff);
        messageOut[3] = username;
        
        // Prepare for the return
        String modelList[][] = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.UserModels",
                                          sessionId);
            modelList = (String[][])network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }        
        return modelList;   
    } 

    /** Get a sequence of datasets for a given user.
     * @return a String[][] object that contains dataset id, dataset name,
     *         last revision time(date format), and dataset abstract of the datasets.
     *         The first index of the array is the dataset sequence in reversed order.
     *         The second index designates the fields.
     *         null if failed.
     * @param maxNum maximum number of datasets to provide status for.
     * @param leftOff least datasetId previously returned (0 if first call in sequence).
     * @param username dataset owner's username.
     */    
    public String[][] getUserDatasets(int maxNum, long leftOff, String username)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[4];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(maxNum);
        messageOut[2] = String.valueOf(leftOff);
        messageOut[3] = username;
        
        // Prepare for the return
        String datasetList[][] = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.UserDatasets",
                                          sessionId);
            datasetList = (String[][])network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }
        return datasetList;   
    } 
    
    /** Get a sequence of versions for a given model or a given dataset.
     * @param id the id number of the model or the dataset.
     * @param type a String specifying model or dataset.     
     * @return a String[][] object that contains version number, author name,  
     *         revision time(date format) and log message of the versions.  
     *         The first index of the array is the version sequence in reversed order.  
     *         The second index designates the fields. 
     *         null if failed.         
     */        
    public String[][] getVersions(long id, String type)    
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(id);
        messageOut[2] = type;
        
        // Prepare for the return
        String[][] versionList = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.GetVersions",
                                          sessionId);
            versionList = (String[][])network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }        
        return versionList;
    }

    /** Get the last revision version of a given model or a given dataset.
     * @param id the id number of the model or the dataset.
     * @param type a String specifying model or dataset.
     * @return a String object that contains last revision version number. 
     *         null if failed.         
     */    
    public String getLastVersion(long id, String type)    
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(id);
        messageOut[2] = type;        
        
        // Prepare for the return
        String versionLast = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.GetLastVersion",
                                          sessionId);
            versionLast = (String)network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }        
        return versionLast;
    }    
    
    /** Get archive and log of a given version.
     * @param version version number.
     * @return a String object containing archive text of the version.
     *         null if failed.
     */
    public String getArchive(String version)    
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[2];
        messageOut[0] = secret;
        messageOut[1] = version;
        
        // Prepare for the return
        String archive = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.GetArchive",
                                          sessionId);
            archive = (String)network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }               
        return archive;   
    }
    
    /** Get archive and log of a given version.
     * @param text1 a String object containing the first text to diff.
     * @param text2 a String object containing the second text to diff.
     * @return a String object containing the revision of the two texts in 
     *         Unix format.  null if failed.
     */
    public String diffFiles(String text1, String text2)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = text1;
        messageOut[2] = text2; 
        
        // Prepare for the return
        String revision = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.DiffFiles",
                                          sessionId);
            revision = (String)network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }       
        return revision;
    }
    
    /** Ends a session of web application at user's choice. */
    public void endSession()
    {
        int end = JOptionPane.showConfirmDialog(null, "Do you want to log out of the MySPK session?",
                                                "Question", JOptionPane.YES_NO_OPTION);
        if(end == 0)
        {
            try
            {
                Network network = new Network("https://" + serverHost + ":" + serverPort +
                                              "/user/servlet/uw.rfpk.servlets.ExitMDATest", sessionId);
                network.talk(secret);
            }
            catch(Exception e)
            {
                JOptionPane.showMessageDialog(null, e, "Exception", JOptionPane.ERROR_MESSAGE);       
            }
        }
    }
    
    /** Get group usernames.
     * @return usernames of the group
     */
    public Vector getGoupUsers()    
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[1];
        messageOut[0] = secret;
        
        // Prepare for the return
        Vector usernames = null;
        
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.GetGroupUsers",
                                          sessionId);
            usernames = (Vector)network.talk(messageOut); 
        }
        catch(Exception e)
	{
            JOptionPane.showMessageDialog(null, "Session expired or other server problem encountered",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }               
        return usernames;   
    }
    
    // Server host
    private String serverHost = null;
    
    // Server port
    private String serverPort = null;
    
    // Session id
    private String sessionId = null;
    
    // Secret code
    private String secret = null; 
}
