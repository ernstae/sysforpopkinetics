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
package uw.rfpk.mda.nonmem;

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
    
    /** Submit a SPK job to the database server.
     * @param source Spk source XML document.
     * @param dataset Spk data XML document.
     * @param modelArchive Spk model archive text.
     * @param jobAbstract short description of the job.
     * @param modelInfo archive information of the model associated with the job.
     * @param dataInfo archive information of the dataset associated with the job.
     * @param jobMethodCode a String containing job method code.
     * @param jobParent a long representing the id of the parent job.
     */
    public void submitJob(String source, String dataset, String modelArchive, 
                          String jobAbstract, ArchiveInfo modelInfo, ArchiveInfo dataInfo,
                          String jobMethodCode, long jobParent)
    {
        String[] messageOut = new String[21];
        messageOut[0] = secret;
        messageOut[1] = source;
        if(dataInfo.isNewArchive || dataInfo.isNewVersion)
            messageOut[2] = dataset;
        else
            messageOut[2] = "";
        if(modelInfo.isNewArchive || modelInfo.isNewVersion)
            messageOut[3] = modelArchive;
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
         
        try
        {
            // Talk to the server
            Network network = new Network("https://" + serverHost + ":" + serverPort + 
                                          "/user/servlet/uw.rfpk.servlets.SubmitJob",
                                          sessionId);
            String messages = (String)network.talk(messageOut);
            if(messages == null)
                return;
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
    }        

    /** Get a sequence of jobs for a given user.
     * @param maxNum maximum number of jobs to provide status for.
     * @param leftOff least jobId previously returned (0 if first call in sequence).
     * @param isLibrary a boolean specifying if it is a library call.
     * @return a String[][] object that contains job id, start time(date format), 
     *         state code(long format), end code(long format) and job abstract of the jobs.  
     *         The first index of the array is the job sequence in reversed order.  
     *         The second index designates the fields. 
     *         null if failed.         
     */
    public String[][] getUserJobs(int maxNum, long leftOff, boolean isLibrary)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[4];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(maxNum);
        messageOut[2] = String.valueOf(leftOff);
        messageOut[3] = String.valueOf(isLibrary);
        
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
     * @param isLibrary a boolean specifying if it is a library call.
     * @return a Properties object containing the job information including 
     * model name, model version dataset name dataset version.  null if failed.
     */
    public Properties getJobInfo(long jobId, boolean isLibrary)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
        messageOut[2] = String.valueOf(isLibrary);
        
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
     * @param isLibrary a boolean specifying if it is a library call.
     * @return a Properties object containing the archive text, name and version.
     */
    public Properties getJobArchive(long jobId, String type, boolean isLibrary)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[4];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
        messageOut[2] = type;
        messageOut[3] = String.valueOf(isLibrary);
        
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
     * @param isLibrary a boolean specifying if it is a library call.
     * @return a Properties object containing the SPK input data. 
     *         null if failed.
     */
    public Properties getInput(long jobId, boolean isLibrary)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
        messageOut[2] = String.valueOf(isLibrary);
        
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
     * @param isLibrary a boolean specifying if it is a library call.
     * @return a Properties object containing the SPK output data. 
     *         null if failed.
     */
    public Properties getOutput(long jobId, boolean isLibrary)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
        messageOut[2] = String.valueOf(isLibrary);        
        
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

    /** Get job history. 
     * @param jobId id number of the job.
     * @param isLibrary a boolean specifying if it is a library call.
     * @return a String[][] object containing the job history including event time, 
     * state code and host. 
     *         null if failed.
     */
    public String[][] getHistory(long jobId, boolean isLibrary)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(jobId);
        messageOut[2] = String.valueOf(isLibrary);
        
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
     * @param maxNum maximum number of models to provide status for.
     * @param leftOff least modelId previously returned (0 if first call in sequence).
     * @param isLibrary a boolean specifying if it is a library call.
     * @return a String[][] object that contains model id, model name,  
     *         last revision time(date format), and model abstract of the models.  
     *         The first index of the array is the model sequence in reversed order.  
     *         The second index designates the fields. 
     *         null if failed.          
     */
    public String[][] getUserModels(int maxNum, long leftOff, boolean isLibrary)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[4];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(maxNum);
        messageOut[2] = String.valueOf(leftOff);
        messageOut[3] = String.valueOf(isLibrary);
        
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
     * @param maxNum maximum number of datasets to provide status for.
     * @param leftOff least datasetId previously returned (0 if first call in sequence).
     * @param isLibrary a boolean specifying if it is a library call.
     * @return a String[][] object that contains dataset id, dataset name,  
     *         last revision time(date format), and dataset abstract of the datasets.  
     *         The first index of the array is the dataset sequence in reversed order.  
     *         The second index designates the fields.  
     *         null if failed.        
     */    
    public String[][] getUserDatasets(int maxNum, long leftOff, boolean isLibrary)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[4];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(maxNum);
        messageOut[2] = String.valueOf(leftOff);
        messageOut[3] = String.valueOf(isLibrary);
        
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
     * @param isLibrary a boolean specifying if it is a library call.
     * @return a String[][] object that contains version number, author name,  
     *         revision time(date format) and log message of the versions.  
     *         The first index of the array is the version sequence in reversed order.  
     *         The second index designates the fields. 
     *         null if failed.         
     */        
    public String[][] getVersions(long id, String type, boolean isLibrary)    
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[4];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(id);
        messageOut[2] = type;
        messageOut[3] = String.valueOf(isLibrary);        
        
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
     
    // Server host
    private String serverHost = null;
    
    // Server port
    private String serverPort = null;
    
    // Session id
    private String sessionId = null;
    
    // Secret code
    private String secret = null; 
}
