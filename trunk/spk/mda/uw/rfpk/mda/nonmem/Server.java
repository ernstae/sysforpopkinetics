/*
 * Server.java
 *
 * Created on February 11, 2004, 4:18 PM
 */

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
 * @author  jiaji Du
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
     * @param jobAbstract Short description of the job.
     * @param modelInfo Archive information of the model associated with the job.
     * @param dataInfo Archive information of the dataset associated with the job.
     */
    public void submitJob(String source, String dataset, String modelArchive, 
                          String jobAbstract, ArchiveInfo modelInfo, ArchiveInfo dataInfo)
    {
        String[] messageOut = new String[19];
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
            JOptionPane.showMessageDialog(null, "Session expired or other server problem",    
                                          "Network Error",         
                                          JOptionPane.ERROR_MESSAGE);
        }   
    }        

    /** Get a sequence of jobs for a given user.
     * @param maxNum Maximum number of jobs to provide status for.
     * @param leftOff Least jobId previously returned (0 if first call in sequence).
     * @return A String[][] object that contains job id, start time(date format), 
     *         state code(long format), end code(long format) and job abstract of the jobs.  
     *         The first index of the array is the job sequence in reversed order.  
     *         The second index designates the fields. 
     *         null if failed.         
     */
    public String[][] getUserJobs(int maxNum, long leftOff)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(maxNum);
        messageOut[2] = String.valueOf(leftOff);
        
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
            JOptionPane.showMessageDialog(null, e,    
                                          "Session expired or other server problem",         
                                          JOptionPane.ERROR_MESSAGE);
        }
        return jobList;    
    }
    
    /** Get SPK output data. 
     * @param jobId Id number of the job.
     * @return A Properties object containing the SPK output data. 
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
            JOptionPane.showMessageDialog(null, e,    
                                          "Session expired or other server problem",         
                                          JOptionPane.ERROR_MESSAGE);
        }
        return spkOutput;            
    }
    
    /** Get a sequence of models for a given user.
     * @param maxNum Maximum number of models to provide status for.
     * @param leftOff Least modelId previously returned (0 if first call in sequence).
     * @param isLibrary A boolean, true for model library, false for the user.
     * @return A String[][] object that contains model id, model name,  
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
            JOptionPane.showMessageDialog(null, e,    
                                          "Session expired or other server problem",         
                                          JOptionPane.ERROR_MESSAGE);
        }        
        return modelList;   
    } 

    /** Get a sequence of datasets for a given user.
     * @param maxNum Maximum number of datasets to provide status for.
     * @param leftOff Least datasetId previously returned (0 if first call in sequence).
     * @return A String[][] object that contains dataset id, dataset name,  
     *         last revision time(date format), and dataset abstract of the datasets.  
     *         The first index of the array is the dataset sequence in reversed order.  
     *         The second index designates the fields.  
     *         null if failed.        
     */    
    public String[][] getUserDatasets(int maxNum, long leftOff)
    {
        // Put secret and arguments in a String array
        String[] messageOut = new String[3];
        messageOut[0] = secret;
        messageOut[1] = String.valueOf(maxNum);
        messageOut[2] = String.valueOf(leftOff);
        
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
            JOptionPane.showMessageDialog(null, e,    
                                          "Session expired or other server problem",         
                                          JOptionPane.ERROR_MESSAGE);
        }
        return datasetList;   
    } 
    
    /** Get a sequence of versions for a given model or a given dataset.
     * @param id The id number of the model or the dataset.
     * @param type Specifying model or dataset.
     * @return A String[][] object that contains version number, author name,  
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
            JOptionPane.showMessageDialog(null, e,    
                                          "Session expired or other server problem",         
                                          JOptionPane.ERROR_MESSAGE);
        }        
        return versionList;
    }
    
    /** Get archive and log of a given version.
     * @param version Version number.
     * @return A String object containing archive text of the version.
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
            JOptionPane.showMessageDialog(null, e,    
                                          "Session expired or other server problem",         
                                          JOptionPane.ERROR_MESSAGE);
        }               
        return archive;   
    }
    
    /** Get archive and log of a given version.
     * @param text1 A String object containing the first text to diff.
     * @param text2 A String object containing the second text to diff.
     * @return A String object containing the revision of the two texts in 
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
            JOptionPane.showMessageDialog(null, e,    
                                          "Session expired or other server problem",         
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
