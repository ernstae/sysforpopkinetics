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
import java.util.Vector;
import java.util.Properties;
import java.nio.*;
import java.sql.*;
import rfpk.spk.spkdb.*;
import uw.rfpk.beans.UserInfo;
import uw.rfpk.rcs.Archive;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

/** This servlet sends back the job's output including source, report, job, model and dataset.
 * The servlet receives a String array containing thwo String objects from the client.
 * The first String object is the secret code to identify the client.  The second String 
 * is the job_id.  The servlet first checks if the job belongs to the user in the group or to 
 * the library, then calls database 
 * API method, getJob, to get SPK output data that includes source, report, start_time, 
 * event_time, model_name, model_version, model_abstract, dataset_name, dataset_version, 
 * dataset_abstract, job_abstract, parent and method_code.  The servlet puts these data into 
 * a java.util.Properties object.  
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there 
 * is an error or an empty String if there is not any error.  The second object is the 
 * Properties object containing the returned data.
 *
 * @author Jiaji Du
 */
public class GetOutput extends HttpServlet
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
        Statement modelStmt = null;
        Statement datasetStmt = null;
        
        // Prepare output message
        String messageOut = "";
        Properties spkOutput = new Properties();

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
                    // Get job information 
                    long modelId = jobRS.getLong("model_id");
                    long datasetId = jobRS.getLong("dataset_id"); 
                    long startTime = jobRS.getLong("start_time");
                    long eventTime = jobRS.getLong("event_time");
                    String endCode = jobRS.getString("end_code");
                    String modelVersion = jobRS.getString("model_version");
                    String datasetVersion = jobRS.getString("dataset_version");
                    String jobAbstract = jobRS.getString("abstract");
                    long parent = jobRS.getLong("parent");
                    String methodCode = jobRS.getString("method_code");

                    // Get Spk report and source
	            Blob blobReport = jobRS.getBlob("report");
                    if(blobReport != null)
                    {
                        long length = blobReport.length();                       
//	                String report = new String(blobReport.getBytes(1L, (int)length));
                        String report = untar(blobReport.getBytes(1L, (int)length), 
                                              context.getInitParameter("database_name"), jobId); 
	                Blob blobSource = jobRS.getBlob("xml_source");
	                length = blobSource.length(); 
	                String source = new String(blobSource.getBytes(1L, (int)length));
             
                        // Get model information
                        ResultSet modelRS = Spkdb.getModel(con, modelId);
                        modelStmt = modelRS.getStatement();
                        modelRS.next();
                        String modelName = modelRS.getString("name");
                        String modelAbstract = modelRS.getString("abstract");
                        Blob blobModel = modelRS.getBlob("archive");
                        length = blobModel.length();
                        String archive = new String(blobModel.getBytes(1L, (int)length));
                        String modelVersionLog = Archive.getVersionLog(archive, modelVersion);
                         
                        // Get dataset information
                        ResultSet datasetRS = Spkdb.getDataset(con, datasetId);
                        datasetStmt = datasetRS.getStatement();
                        datasetRS.next();
                        String datasetName = datasetRS.getString("name");
                        String datasetAbstract = datasetRS.getString("abstract");           
                        Blob blobDataset = datasetRS.getBlob("archive");
                        length = blobDataset.length();
                        archive = new String(blobDataset.getBytes(1L, (int)length));
                        String datasetVersionLog = Archive.getVersionLog(archive, datasetVersion);
                        
                        // Put returning objects into the Properties
                        spkOutput.setProperty("source", source);
                        spkOutput.setProperty("report", report);
                        spkOutput.setProperty("startTime", String.valueOf(startTime));
                        spkOutput.setProperty("eventTime", String.valueOf(eventTime));
                        spkOutput.setProperty("modelName", modelName);
                        spkOutput.setProperty("modelVersion", modelVersion.substring(2));
                        spkOutput.setProperty("modelAbstract", modelAbstract);
                        spkOutput.setProperty("modelVersionLog", modelVersionLog);
                        spkOutput.setProperty("datasetName", datasetName);
                        spkOutput.setProperty("datasetVersion", datasetVersion.substring(2));
                        spkOutput.setProperty("datasetAbstract", datasetAbstract);
                        spkOutput.setProperty("datasetVersionLog", datasetVersionLog);
                        spkOutput.setProperty("jobAbstract", jobAbstract);
                        spkOutput.setProperty("parent", String.valueOf(parent));
                        spkOutput.setProperty("methodCode", methodCode);
                    }
                    else
                    {
                        messageOut = "No job report was found for the job.";
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
                if(modelStmt != null) modelStmt.close();
                if(datasetStmt != null) datasetStmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){messageOut += "\n" + e.getMessage();}
        }

        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(spkOutput);

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
    
    private String untar(byte[] bytes, String environment, long jobId)
    {
        String workingDir = "/tmp/" + environment + jobId; 
        File directory = new File(workingDir);
        directory.mkdir();
        File file1 = new File(workingDir + "/result.tar.gz");
        File file2 = new File(workingDir + "/result.xml");
        saveFile(bytes, file1);
        String text = null;
        Process process = null;
        String[] command = {"/bin/tar", "xzf", "result.tar.gz"};
        try 
        {
            process = Runtime.getRuntime().exec(command, null, directory);
            process.waitFor();
            text = openFile(file2); 
        }
        catch(IOException e)
        {
        }
        catch(InterruptedException e)
        {
        }
        finally
        {
            if(process != null) process.destroy();
            if(file1.exists()) file1.delete();
            if(file2.exists()) file2.delete();
            if(directory.exists()) directory.delete();
        }
        return text;
    }
    
    private static void saveFile(byte[] bytes, File file)
    {
        FileOutputStream out = null;
        try
        {
            out = new FileOutputStream(file);
        }
        catch(FileNotFoundException e){}         
        ByteBuffer buffer = ByteBuffer.allocate(bytes.length);       
        FileChannel channel = out.getChannel();         
        buffer.put(bytes);     
        buffer.flip();         
        try
        {
            channel.write(buffer);             
            out.close();
        }
        catch(IOException e )
        {
        }
    }
    
    private static String openFile(File file)
    {
        String text = null;
        try
	{
            StringBuffer buffer = new StringBuffer();
            BufferedReader in = new BufferedReader(new FileReader(file));
            String line;
            while((line = in.readLine()) != null)
                buffer.append(line).append("\n");
            in.close();
            text = buffer.toString();
        }
        catch(IOException e)
	{
        }
        return text;
    }
}

