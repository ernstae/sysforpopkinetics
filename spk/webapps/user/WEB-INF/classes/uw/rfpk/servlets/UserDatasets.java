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
import java.nio.*;
import java.sql.*;
import rfpk.spk.spkdb.*;
import java.util.Vector;
import org.apache.commons.jrcs.rcs.*;
import org.apache.commons.jrcs.util.ToString;
import org.apache.commons.jrcs.diff.*;
import uw.rfpk.beans.UserInfo;

/** This servlet sends back information about a list of datasets belonging to the user.
 * The servlet receives a String array containing three String objects from the client.
 * The first String object is the secret code to identify the client.  The second String  
 * object is the maximum number of dataset to provide status for.  The third String object is
 * the least dataset_id previously returned.  The fourth String object indicates if it is to 
 * get the dataset list of the dataset library.  The servlet calls database API method, 
 * userDatasets, to get dataset status that includes id, name, newest version number, and 
 * abstract of the datasets.  The servlet puts these data into a String[][] object.
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there is an error or an empty String if there is not any error.  The second 
 * object is the returning data String[][] object.
 *
 * @author Jiaji Du
 */
public class UserDatasets extends HttpServlet
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
                if(messageIn[3].equals("true"))
                    username = "librarian";
                
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
                    String[] dataset = new String[5];
                    dataset[0] = String.valueOf(datasetId); 
                    dataset[1] = userDatasetsRS.getString("name");
                    dataset[2] = String.valueOf(archive.getRevisionVersion().last());
                    dataset[3] = archive.findNode(archive.getRevisionVersion()).getDate().toString();
                    dataset[4] = userDatasetsRS.getString("abstract");
                    datasetList.add(dataset);
                }
                
                // Disconnect to the database
                Spkdb.disconnect(con);
                
                // Put the list in the String[][]
                int nDataset = datasetList.size(); 
                if(nDataset > 0)
                {
                    userDatasets = new String[nDataset][5];
                    for(int i = 0; i < nDataset; i++)
                        userDatasets[i] = (String[])datasetList.get(i);
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

