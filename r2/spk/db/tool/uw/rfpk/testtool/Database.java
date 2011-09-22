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
package uw.rfpk.testtool;

import java.io.*;
import java.sql.*;
import java.util.Vector;
import java.util.Properties;
import rfpk.spk.spkdb.*;
//import org.apache.commons.jrcs.rcs.*;
//import org.apache.commons.jrcs.util.ToString;
//import org.apache.commons.jrcs.diff.*;
import java.text.SimpleDateFormat;
import javax.swing.JOptionPane;
import uw.rfpk.mda.*;
import uw.rfpk.mda.nonmem.*;
import uw.rfpk.rcs.Archive;

/** This class encapsulates the database calls for the transactions required by the model
 *  dasign agent of the SPK.  
 *
 * @author  Jiaji Du
 */
public class Database {
    /** Constructor to initiate the data for the database connection and the user name.
     * @param database Name of the database used.
     * @param hostname Domain name of host on which database resides.
     * @param username User's name of the database user.
     * @param password Password of the database user.
     * @param user User' name of the SPK user.
     */
    public Database(String database, String hostname, String username, String password, String user)
    {
        this.database = database;
        this.hostname = hostname;
        this.username = username;
        this.password = password;
        this.user = user;
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
        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(database, hostname, username, password); 

            // Get user id
            ResultSet userRS = Spkdb.getUser(con, user);
            userRS.next();
            long userId = userRS.getLong("user_id");  
            
            // Initialize id and version for model and dataset
            long modelId = 0;
            long datasetId = 0;
            String modelVersion = "";
            String datasetVersion = ""; 
                                
            // Get model archive information
            if(modelInfo.isNewArchive)
            {
//                Archive arch = new Archive(modelArchive.split("\n"), ""); 
                modelId = Spkdb.newModel(con, 
                                         userId, 
                                         modelInfo.name, 
                                         modelInfo.description, 
//                                         arch.toString("\n"));
                                         modelArchive);
                JOptionPane.showMessageDialog(null, "A new model, " + modelInfo.name +
                                              ", has been added to the database.",  
                                              "model archive information",
                                              JOptionPane.INFORMATION_MESSAGE);
                modelVersion = String.valueOf(1);
            }
            else
            {
                modelId = modelInfo.id;         
                if(modelInfo.isNewVersion)
                {               
                    ResultSet modelRS = Spkdb.getModel(con, 
                                                       modelId);
                    modelRS.next();
                    String strAr = modelRS.getString("archive");
//                    Archive arch = new Archive("", new ByteArrayInputStream(strAr.getBytes()));
//                    arch.addRevision(modelArchive.split("\n"), modelInfo.log);
                    String[] name = new String[1];
                    name[0] = "archive";
                    String[] value = new String[1];
//                    value[0] = arch.toString("\n");
                    value[0] = Archive.addRevision(strAr, modelArchive, perlDir, 
                                                  workingDir, "versionLog", "author", "filename");
                    Spkdb.updateModel(con, 
                                      modelId, 
                                      name, 
                                      value); 
                    JOptionPane.showMessageDialog(null, "The model, " + modelInfo.name +
                                                  ", in the database has been updated.",  
                                                  "model archive information",
                                                  JOptionPane.INFORMATION_MESSAGE);                    
//                    modelVersion = String.valueOf(arch.getRevisionVersion().last());
                    modelVersion = "1." + Archive.getNumRevision(value[0]);
                }
                else
                    modelVersion = modelInfo.version;
            }

            // Get data archive information
            if(dataInfo.isNewArchive)
            {
//                Archive arch = new Archive(dataset.split("\n"), "");                       
                datasetId = Spkdb.newDataset(con, 
                                             userId, 
                                             dataInfo.name, 
                                             dataInfo.description, 
//                                             arch.toString("\n"));
                                             Archive.newArchive(dataset, perlDir, workingDir,
                                                                "versionLog", "author", "filename"));
                JOptionPane.showMessageDialog(null, "A new dataset, " + dataInfo.name +
                                              ", has been added to the database.",  
                                              "Dataset archive information",
                                              JOptionPane.INFORMATION_MESSAGE);                
                datasetVersion = String.valueOf(1);
            }
            else
            {
                datasetId = dataInfo.id;         
                if(dataInfo.isNewVersion)
                {               
                    ResultSet datasetRS = Spkdb.getDataset(con, 
                                                           datasetId);;
                    datasetRS.next();
                    String strAr = datasetRS.getString("archive");
                    //Archive arch = new Archive("", new ByteArrayInputStream(strAr.getBytes()));
                    
                    //arch.addRevision(dataset.split("\n"), dataInfo.log);
                    String[] name = new String[1];
                    name[0] = "archive";
                    String[] value = new String[1];
//                    value[0] = arch.toString("\n"); 
                    value[0] = Archive.addRevision(strAr, dataset, perlDir, workingDir, "versionLog", 
                                                  "author", "filename");
                    Spkdb.updateDataset(con, 
                                        datasetId, 
                                        name, 
                                        value);  
                    JOptionPane.showMessageDialog(null, "The dataset, " + dataInfo.name +
                                                  ", in the database has been updated.",  
                                                  "Dataset archive information",
                                                  JOptionPane.INFORMATION_MESSAGE);                    
//                    datasetVersion = String.valueOf(arch.getRevisionVersion().last());
                    datasetVersion = "1." + Archive.getNumRevision(value[0]);
                }
                else
                    datasetVersion = dataInfo.version;
            }            
                                
            // Add a job
            Spkdb.newJob(con, 
                         userId, 
                         jobAbstract, 
                         datasetId, 
                         datasetVersion, 
                         modelId, 
                         modelVersion, 
                         source,
                         "",
                         0L,
                         false,
                         false,
                         false);
            JOptionPane.showMessageDialog(null, "A new job, " + jobAbstract +
                                          ", has been added to the database.",  
                                          "Job submission information",
                                          JOptionPane.INFORMATION_MESSAGE);  
                 
            // Disconnect to the database
            Spkdb.disconnect(con);
        }
        catch(SQLException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "SQL exception",
                                          JOptionPane.ERROR_MESSAGE);  
        }        
        catch(SpkdbException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Spkdb exception",
                                          JOptionPane.ERROR_MESSAGE);            
        } 
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "IO exception",
                                          JOptionPane.ERROR_MESSAGE);            
        }      
        catch(InterruptedException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Interrupted exception",
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
        // Prepare for the return
        Vector<String[]> jobList = new Vector<String[]>();

        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(database, hostname, username, password);                 

            // Get user id
            ResultSet userRS = Spkdb.getUser(con, user);
            userRS.next();
            long userId = userRS.getLong("user_id");

            // Get user jobs
            ResultSet userJobsRS = Spkdb.userJobs(con, userId, maxNum, leftOff, null,
                                                  null, null, null, null);
            // Set state_code conversion
            ResultSet stateRS = Spkdb.getStateTable(con);
            Properties state = new Properties();                
            while(stateRS.next())
                state.setProperty(stateRS.getString(1), stateRS.getString(2));

            // Set end_code conversion
            ResultSet endRS = Spkdb.getEndTable(con);                
            Properties end = new Properties();
            while(endRS.next())
                end.setProperty(endRS.getString(1), endRS.getString(2));
            
            // Disconnect to the database
            Spkdb.disconnect(con);

            // Fill in the List
            SimpleDateFormat formater = new SimpleDateFormat("EEE, MMM, d yyyy 'at' HH:mm:ss z");
            userJobsRS.last();
            for(int i = 0; i < maxNum; i++)
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
                userJobsRS.previous();
            }
        }
        catch(SQLException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "SQL exception",
                                          JOptionPane.ERROR_MESSAGE); 
            return null;
        }    
        catch(SpkdbException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Spkdb exception",
                                          JOptionPane.ERROR_MESSAGE);  
            return null;
        }  
        int nJob = jobList.size();
        if(nJob == 0)
            return null;
        String[][] userJobs = new String[nJob][];        
        for(int i = 0; i < nJob; i++)
            userJobs[i] = (String[])jobList.get(i);

        return userJobs;          
    }
    
    /** Get SPK output data. 
     * @param jobId Id number of the job.
     * @return A Properties object containing the SPK output data. 
     *         null if failed.
     */
    public Properties getOutput(long jobId)
    {
        // Prepare for the return
        Properties spkOutput = new Properties();
        
        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(database, hostname, username, password);                   
                       
            // Get job for the job_id
            ResultSet jobRS = Spkdb.getJob(con, jobId); 
            jobRS.next();
            
            // Get job information 
            long modelId = jobRS.getLong("model_id");
            long datasetId = jobRS.getLong("dataset_id"); 
            long startTime = jobRS.getLong("start_time");
            long eventTime = jobRS.getLong("event_time");
            String endCode = jobRS.getString("end_code");
            String modelVersion = jobRS.getString("model_version");
            String datasetVersion = jobRS.getString("dataset_version");
            String jobAbstract = jobRS.getString("abstract"); 
            
            // Get Spk report and source
	    Blob blobReport = jobRS.getBlob("report");
            if(blobReport == null)
            {
                JOptionPane.showMessageDialog(null, "The report is not in the database.",  
                                              "Database Information",
                                              JOptionPane.INFORMATION_MESSAGE);
                // Disconnect to the database
                Spkdb.disconnect(con);
                return null;
            }
	    long length = blobReport.length();
	    String report = new String(blobReport.getBytes(1L, (int)length));
	    Blob blobSource = jobRS.getBlob("xml_source");
	    length = blobSource.length(); 
	    String source = new String(blobSource.getBytes(1L, (int)length));
            
            // Get model information
            ResultSet modelRS = Spkdb.getModel(con, modelId); 
            modelRS.next();
            String modelName = modelRS.getString("name");
            String modelAbstract = modelRS.getString("abstract"); 
                        
            // Get dataset information
            ResultSet datasetRS = Spkdb.getDataset(con, modelId); 
            datasetRS.next();
            String datasetName = datasetRS.getString("name");
            String datasetAbstract = datasetRS.getString("abstract");           
           
            // Disconnect to the database
            Spkdb.disconnect(con);
            
            // Put returning objects into the Properties
            spkOutput.setProperty("source", source);
            spkOutput.setProperty("report", report);
            spkOutput.setProperty("startTime", String.valueOf(startTime));
            spkOutput.setProperty("eventTime", String.valueOf(eventTime));
            spkOutput.setProperty("modelName", modelName);
            spkOutput.setProperty("modelVersion", modelVersion);
            spkOutput.setProperty("modelAbstract", modelAbstract);
            spkOutput.setProperty("datasetName", datasetName);
            spkOutput.setProperty("datasetVersion", datasetVersion);
            spkOutput.setProperty("datasetAbstract", datasetAbstract);
            spkOutput.setProperty("jobAbstract", jobAbstract);
            
        }
        catch(SQLException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "SQL exception",
                                          JOptionPane.ERROR_MESSAGE);
            return null;
        }
        catch(SpkdbException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Spkdb exception",
                                          JOptionPane.ERROR_MESSAGE);
            return null;
        }        
        
        return spkOutput;   
    }
    
    /** Get a sequence of models for a given user.
     * @param maxNum Maximum number of models to provide status for.
     * @param leftOff Least modelId previously returned (0 if first call in sequence).
     * @return A String[][] object that contains model id, model name,  
     *         last revision time(date format), and model abstract of the models.  
     *         The first index of the array is the model sequence in reversed order.  
     *         The second index designates the fields. 
     *         null if failed.          
     */
    public String[][] getUserModels(int maxNum, long leftOff)
    {
        // Prepare for the return
        Vector<String[]> modelList = new Vector<String[]>();
       
        try 
        {
            // Connect to the database
            Connection con = Spkdb.connect(database, hostname, username, password);                 

            // Get user id
            ResultSet userRS = Spkdb.getUser(con, user); 
            userRS.next();
            long userId = userRS.getLong("user_id");
                        
            // Get user models
            ResultSet userModelsRS = Spkdb.userModels(con, userId, maxNum, leftOff);

            // Fill in the list
            while(userModelsRS.next())
            {
                // Get model id
                long modelId = userModelsRS.getLong("model_id"); 
                    
                // Get model archive
                ResultSet modelRS = Spkdb.getModel(con, modelId);
                modelRS.next();
                String modelArchive = modelRS.getString("archive");
//                archive = new Archive("", new ByteArrayInputStream(modelArchive.getBytes()));
                archive = modelArchive;
                
                // Fill in the list 
                String[] model = new String[5];
                model[0] = String.valueOf(modelId); 
                model[1] = userModelsRS.getString("name");
//                model[2] = String.valueOf(archive.getRevisionVersion().last());
//                model[3] = archive.findNode(archive.getRevisionVersion()).getDate().toString();
                model[2] = String.valueOf(Archive.getNumRevision(archive));
                model[3] = Archive.getRevisionDate(archive);
                model[4] = userModelsRS.getString("abstract");
                modelList.add(model);
            }
            
            // Disconnect to the database
            Spkdb.disconnect(con);
        }
        catch(SQLException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "SQL exception",
                                          JOptionPane.ERROR_MESSAGE);
            return null;
        }    
        catch(SpkdbException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Spkdb exception",
                                          JOptionPane.ERROR_MESSAGE); 
            return null;
        }         
//        catch(ParseException e)
//        {
//            JOptionPane.showMessageDialog(null, e,  
//                                          "Parse exception",
//                                          JOptionPane.ERROR_MESSAGE); 
//            return null;
//        }
            
        int nModel = modelList.size();
        if(nModel == 0)
            return null;
        String[][] userModels = new String[nModel][];
        
        for(int i = 0; i < nModel; i++)
            userModels[i] = (String[])modelList.get(i);
        
        return userModels;   
    } 
    
    /** Get a sequence of versions for a given model.
     * @param modelId The id number of the model.
     * @return A String[][] object that contains version number, author name,  
     *         revision time(date format) and log message of the versions.  
     *         The first index of the array is the version sequence in reversed order.  
     *         The second index designates the fields. 
     *         null if failed.         
     */        
    public String[][] getModelVersions(long modelId)    
    {
        // Prepare for the return
        String[][] versionList = null;
        
        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(database, hostname, username, password);                   

            // Get model archive
            ResultSet modelRS = Spkdb.getModel(con, modelId); 
            modelRS.next();
            String modelArchive = modelRS.getString("archive");  
            
            // Disconnect to the database
            Spkdb.disconnect(con);
            
            // Generate version list for the model
//            archive = new Archive("", new ByteArrayInputStream(modelArchive.getBytes())); 
//            int number = archive.getRevisionVersion().last(); 
//            versionList = new String[number][4];
//            for(int i = 0; i < number; i++)
//            {
//                int n = number - i;
//                Node node = archive.findNode(new Version("1." + n));  
//                versionList[i][0] = String.valueOf(n);
//                versionList[i][1] = node.getAuthor().toString();
//                versionList[i][2] = node.getDate().toString();
//                versionList[i][3] = archive.getLog("1." + n);
//            }
            versionList = Archive.getVersionList(modelArchive);
        }
        catch(SQLException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "SQL exception",
                                          JOptionPane.ERROR_MESSAGE); 
            return null;
        }    
        catch(SpkdbException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Spkdb exception",
                                          JOptionPane.ERROR_MESSAGE);
            return null;
        } 
//        catch(ParseException e)
//        {
//            JOptionPane.showMessageDialog(null, e,  
//                                          "Parse exception",
//                                          JOptionPane.ERROR_MESSAGE); 
//            return null;
//        }       
        return versionList;
    }
    
    /** Get archive and log of a given model version
     * @param version Version number.
     * @return A String object containing archive text of the model version.
     *         null if failed.
     */
    public String getModelArchive(String version)    
    {
        // Prepare for the return
        String modelArchive = null;

        try
        {
            modelArchive = Archive.getRevision(archive, perlDir, workingDir, 
                                               "filename", "1." + version);
//            Object[] revision = archive.getRevision("1." + version);
//            modelArchive = ToString.arrayToString(revision, "\n");
        }
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Invalid file format exception",
                                          JOptionPane.ERROR_MESSAGE);  
            return null;
        }    
        catch(InterruptedException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Patch failed exception",
                                          JOptionPane.ERROR_MESSAGE); 
            return null;
        }                                
        return modelArchive;   
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
        // Prepare for the return
        Vector<String[]> datasetList = new Vector<String[]>();
        
        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(database, hostname, username, password);                 

            // Get user id
            ResultSet userRS = Spkdb.getUser(con, user);
            userRS.next();
            long userId = userRS.getLong("user_id");

            // Get user datasets
            ResultSet userDatasetsRS = Spkdb.userDatasets(con, userId, maxNum, leftOff);
            
            // Fill in the list
            while(userDatasetsRS.next())
            {
                // Get dataset id
                long datasetId = userDatasetsRS.getLong("dataset_id"); 
                    
                // Get data archive
                ResultSet datasetRS = Spkdb.getDataset(con, datasetId);                   
                datasetRS.next();
                String datasetArchive = datasetRS.getString("archive");
//                archive = new Archive("", new ByteArrayInputStream(datasetArchive.getBytes()));
                archive = datasetArchive;    
                
                // Fill in the list
                String[] dataset = new String[5];                
                dataset[0] = String.valueOf(userDatasetsRS.getLong("dataset_id"));
                dataset[1] = userDatasetsRS.getString("name");
//                dataset[2] = String.valueOf(archive.getRevisionVersion().last());                
//                dataset[3] = archive.findNode(archive.getRevisionVersion()).getDate().toString();
                dataset[2] = String.valueOf(Archive.getNumRevision(archive));
                dataset[3] = Archive.getRevisionDate(archive);
                dataset[4] = userDatasetsRS.getString("abstract");
                datasetList.add(dataset);
            } 
            
            // Disconnect to the database
            Spkdb.disconnect(con);
        }
        catch(SQLException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "SQL exception",
                                          JOptionPane.ERROR_MESSAGE);  
            return null;
        }    
        catch(SpkdbException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Spkdb exception",
                                          JOptionPane.ERROR_MESSAGE);
            return null;
        } 
//        catch(ParseException e)
//        {
//            JOptionPane.showMessageDialog(null, e,  
//                                          "Parse exception",
//                                          JOptionPane.ERROR_MESSAGE); 
//            return null;
//        }
        int nDataset = datasetList.size();
        if(nDataset == 0)
            return null;
        String[][] userDatasets = new String[nDataset][];        
        for(int i = 0; i < nDataset; i++)
            userDatasets[i] = (String[])datasetList.get(i);

        return userDatasets;   
    } 
    
    /** Get a sequence of versions for a given dataset.
     * @param datasetId The id number of the dataset.
     * @return A String[][] object that contains version number, author name,  
     *         revision time(date format) and log message of the versions.  
     *         The first index of the array is the version sequence in reversed order.  
     *         The second index designates the fields. 
     *         null if failed.        
     */        
    public String[][] getDatasetVersions(long datasetId)    
    {
        // Prepare for the return
        String[][] versionList = null;
        
        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(database, hostname, username, password);                   

            // Get data archive
            ResultSet datasetRS = Spkdb.getDataset(con, datasetId); 
            datasetRS.next(); 
            String datasetArchive = datasetRS.getString("archive");  
            
            // Disconnect to the database
            Spkdb.disconnect(con);
            
            // Generate version list for the model
//            archive = new Archive("", new ByteArrayInputStream(datasetArchive.getBytes())); 
//            int number = archive.getRevisionVersion().last(); 
//            versionList = new String[number][4];
//            for(int i = 0; i < number; i++)
//            {
//                int n = number - i;
//                Node node = archive.findNode(new Version("1." + n));  
//                versionList[i][0] = String.valueOf(n);
//                versionList[i][1] = node.getAuthor().toString();
//                versionList[i][2] = node.getDate().toString();
//                versionList[i][3] = archive.getLog("1." + n);
//            }
            versionList = Archive.getVersionList(datasetArchive);
        }
        catch(SQLException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "SQL exception",
                                          JOptionPane.ERROR_MESSAGE);
            return null;
        }    
        catch(SpkdbException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Spkdb exception",
                                          JOptionPane.ERROR_MESSAGE);
            return null;
        } 
//        catch(ParseException e)
//        {
//            JOptionPane.showMessageDialog(null, e,  
//                                          "Parse exception",
//                                          JOptionPane.ERROR_MESSAGE);            
//        }       
        return versionList;
    }
    
    /** Get archive and log of a given dataset version
     * @param version Version number.
     * @return A String object containing archive text of the dataset version.
     *         null if failed.
     */    
    public String getDatasetArchive(String version)    
    {
        // Prepare for the return
        String datasetArchive = null;       
        try
        {
            datasetArchive = Archive.getRevision(archive, perlDir, workingDir, 
                                                 "filename", "1." + version);
//            Object[] revision = archive.getRevision("1." + version);
//            datasetArchive = ToString.arrayToString(revision, "\n"); 
        }
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "IO exception",
                                          JOptionPane.ERROR_MESSAGE); 
            return null;
        }    
        catch(InterruptedException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Interrupted exception",
                                          JOptionPane.ERROR_MESSAGE);
            return null;
       }                                
        return datasetArchive; 
    }            

    // Database name
    private String database = null;
    
    // Host name
    private String hostname = null; 
    
    // User name of the database
    private String username = null;
    
    // User password of the database
    private String password = null;
    
    // User name of SPK server
    private String user = null;
    
    // Archive
//    private Archive archive = null;
    private String archive = null;
    
    // Directory of the perl script used to run rcs
    private final String perlDir = "/usr/local/bin/";
    
    // Working directory for running rcs
    private final String workingDir = "/tmp/";
}
