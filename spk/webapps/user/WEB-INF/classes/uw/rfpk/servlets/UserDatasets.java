package uw.rfpk.servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
import java.nio.*;
import java.sql.*;
import rfpk.spk.spkdb.*;
import java.util.Vector;
import org.apache.commons.jrcs.rcs.*;
import org.apache.commons.jrcs.util.ToString;
import org.apache.commons.jrcs.diff.*;
import uw.rfpk.beans.UserInfo;

/** This servlet receives a String array containing three String objects from the client.
 * The first String object is the secret code to identify the client.  The second String  
 * object is the maximum number of dataset to provide status for.  The third String object is
 * the least dataset_id previously returned.  The servlet calls database API method, 
 * userDatasets, to get dataset status that includes id, name, newest version number, and 
 * abstract of the datasets.  The servlet puts these data into a String[][] object and saves 
 * the user's dataset id list in the Session object, DATASETIDS.
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there is an error or an empty String if there is not any error.  The second 
 * object is the returning data String[][] object.
 *
 * @author Jiaji Du
 * @version 1.0
 */
public class UserDatasets extends HttpServlet
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
        String[][] userDatasets = null;
        
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
        Vector datasetList = new Vector();

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
 
                // Get user datasetss
                ResultSet userDatasetsRS = Spkdb.userDatasets(con, userId, maxNum, leftOff);  
             
                // Fill in the List
                while(userDatasetsRS.next())
                {                  
                    // Get dataset id
                    long datasetId = userDatasetsRS.getLong("dataset_id"); 
                    
                    // Get dataset archive
       	            Blob blobArchive = userDatasetsRS.getBlob("archive");
	            long length = blobArchive.length(); 
	            String datasetArchive = new String(blobArchive.getBytes(1L, (int)length));                    
                    Archive archive = new Archive("", new ByteArrayInputStream(datasetArchive.getBytes()));
                    
                    // Fill in the list 
                    String[] dataset = new String[4];
                    dataset[0] = String.valueOf(datasetId); 
                    dataset[1] = userDatasetsRS.getString("name");
                    dataset[2] = archive.findNode(archive.getRevisionVersion()).getDate().toString();
                    dataset[3] = userDatasetsRS.getString("abstract");
                    datasetList.add(dataset);
                }
                
                // Disconnect to the database
                Spkdb.disconnect(con);
                
                // Put the list in the String[][]
                int nDataset = datasetList.size(); 
                if(nDataset > 0)
                {
                    userDatasets = new String[nDataset][4];
                    Vector datasetIds = (Vector)req.getSession().getAttribute("DATASETIDS"); 
                    if(datasetIds == null)
                        datasetIds = new Vector();                    
                    for(int i = 0; i < nDataset; i++)
                    {
                        userDatasets[i] = (String[])datasetList.get(i);
                        datasetIds.add(userDatasets[i][0]); 
                    }
                    req.getSession().setAttribute("DATASETIDS", datasetIds);                
                }
                else
                    messageOut = "No dataset was found in the database";
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
        catch(ParseException e)
        { 
            messageOut = e.getMessage();
        }
        
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(userDatasets);

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

