package uw.rfpk.servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
import java.nio.*;
import java.sql.*;
import rfpk.spk.spkdb.*;    
import org.apache.commons.jrcs.rcs.*; 
import org.apache.commons.jrcs.diff.*;
import uw.rfpk.beans.UserInfo;

/** This servlet receives a String array containing ninteen String objects from the client.
 * The first String object is the secret code to identify the client.  The other eighteen
 * Strings are source, dataset, model archive, job_abstract, model_abstract, model_log, 
 * model_name, model_version, model_id, is_new_model, is_new_model_version, dataset_abstract, 
 * dataset_log, dataset_name, dataset_version, dataset_id, is_new_dataset, is_new_dataset_version.
 * If the model is new the servlet calls database API method, newModle, to get model_id.
 * If the model is old but the version is new the servlet calls database API methods, getModel
 * and updateModel, to update the model archive.  The servlet does the same operations for the
 * dataset.  Then the servlet calls database API method, newJob, to add a job into the database.
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there is an error or an empty String if there is not any error.  The second object
 * is also a String that contains a information message to inform the client that the model and
 * the dataset has been added to the database or updated, and that the job has been added to the
 * database if any of them happend.
 *  
 * @author Jiaji Du
 * @version 1.0
 */
public class SubmitJob extends HttpServlet 
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
        String messages = "";
 
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
        
        String which = null;
        try
        {
            // Read the data from the client 
            String[] messageIn = (String[])in.readObject();
            String secret = messageIn[0];
            if(secret.equals((String)req.getSession().getAttribute("SECRET")))             
            {                        
 	        String source = messageIn[1];
 	        String dataset = messageIn[2];
 	        String modelArchive = messageIn[3];
 	        String jobAbstract = messageIn[4];
                String modelDescription = messageIn[5];
                String modelLog = messageIn[6]; 
                String modelName = messageIn[7]; 
                String modelVersion = messageIn[8]; 
                long modelId = Long.parseLong(messageIn[9]);
                String isNewModel = messageIn[10];
                String isNewModelVersion = messageIn[11];
                String datasetDescription = messageIn[12];
                String datasetLog = messageIn[13]; 
                String datasetName = messageIn[14]; 
                String datasetVersion = messageIn[15]; 
                long datasetId = Long.parseLong(messageIn[16]);
                String isNewDataset = messageIn[17];
                String isNewDatasetVersion = messageIn[18];                
                 
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
 
                // Get model archive information
                if(isNewModel.equals("true"))
                {
                    Archive arch = new Archive(modelArchive.split("\n"), ""); 
                    Node node = arch.findNode(new Version("1.1"));
                    node.setAuthor(username);
                    node.setLog(modelLog);
                    which = "model";
                    modelId = Spkdb.newModel(con, 
                                             userId, 
                                             modelName, 
                                             modelDescription, 
                                             arch.toString("\n"));
                    messages += "A new model, " + modelName +
                                ", has been added to the database.\n";
                    modelVersion = "1.1";                    
                }
                else
                {        
                    if(isNewModelVersion.equals("true"))   
                    {               
                        ResultSet modelRS = Spkdb.getModel(con, 
                                                           modelId);
                        modelRS.next();
                        Blob blobArchive = modelRS.getBlob("archive");
                        long length = blobArchive.length();
                        String strAr = new String(blobArchive.getBytes(1L, (int)length));                        
                        Archive arch = new Archive("", new ByteArrayInputStream(strAr.getBytes()));
                        arch.addRevision(modelArchive.split("\n"), modelLog);
                        arch.findNode(arch.getRevisionVersion()).setAuthor(username);
                        Spkdb.updateModel(con, 
                                          modelId, 
                                          new String[]{"archive"}, 
                                          new String[]{arch.toString("\n")}); 
                        messages += "The model, " + modelName +
                                    ", in the database has been updated.\n";                      
                        modelVersion = String.valueOf(arch.getRevisionVersion());                       
                    }
                }
 
                // Get data archive information
                if(isNewDataset.equals("true"))
                {
                    Archive arch = new Archive(dataset.split("\n"), "");
                    Node node = arch.findNode(new Version("1.1"));
                    node.setAuthor(username);
                    node.setLog(datasetLog);
                    which = "dataset";
                    datasetId = Spkdb.newDataset(con, 
                                                 userId, 
                                                 datasetName, 
                                                 datasetDescription, 
                                                 arch.toString("\n"));
                    messages += "A new dataset, " + datasetName +
                                ", has been added to the database.\n";  
                    datasetVersion = "1.1";
                }
                else
                {        
                    if(isNewDatasetVersion.equals("true"))
                    {               
                        ResultSet datasetRS = Spkdb.getDataset(con, 
                                                               datasetId);;
                        datasetRS.next();
                        Blob blobArchive = datasetRS.getBlob("archive");
                        long length = blobArchive.length();
                        String strAr = new String(blobArchive.getBytes(1L, (int)length));                        
                        Archive arch = new Archive("", new ByteArrayInputStream(strAr.getBytes()));
                        arch.addRevision(dataset.split("\n"), datasetLog);
                        arch.findNode(arch.getRevisionVersion()).setAuthor(username);                       
                        Spkdb.updateDataset(con, 
                                            datasetId, 
                                            new String[]{"archive"}, 
                                            new String[]{arch.toString("\n")});  
                        messages += "The dataset, " + datasetName +
                                    ", in the database has been updated.\n";                      
                        datasetVersion = String.valueOf(arch.getRevisionVersion());
                    }
                }            
                                 
                // Add a job
                Spkdb.newJob(con, 
                             userId, 
                             jobAbstract, 
                             datasetId, 
                             datasetVersion, 
                             modelId, 
                             modelVersion, 
                             source);
                messages += "A new job, " + jobAbstract +
                            ", has been added to the database.\n";  
 
                // Disconnect to the database
                Spkdb.disconnect(con);
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
            if(messageOut.indexOf("Duplicate entry") != -1 && which != null)
            {
                int bIndex = messageOut.indexOf("-") + 1;
                int eIndex = messageOut.indexOf("'", bIndex);
                
                messageOut = "The name '" + messageOut.substring(bIndex, eIndex) + 
                             "' has already been used for a " + which +
                             ".\nPlease use another name if it is a different one";
            }
        }        
        catch(SpkdbException e)
        {
            messageOut = e.getMessage();         
        } 
        catch(ParseException e)
        {
            messageOut = e.getMessage();         
        }
        catch(InvalidFileFormatException e)
        {
            messageOut = e.getMessage();         
        }        
        catch(DiffException e)
        {
            messageOut = e.getMessage();         
        } 
        catch(FileNotFoundException e)
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
            out.writeObject(messages);        
         
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
