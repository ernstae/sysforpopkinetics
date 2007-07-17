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

import java.net.*;
import java.nio.*;
import java.io.*;
import java.util.Vector;
import javax.swing.JOptionPane;

/**
 * This class handles the network communications.
 * @author Jiaji Du
 * @version 1.0
 */
public class Network
{
    /**
     * This is the constructor with two arguments creating an url connection
     * to the server with session id.
     * @param url the universal resource locator of the servlet.
     * @param sessionId the session id to identify the session.
     * @exception Exception an exception of some sort.
     */
    public Network(String url, String sessionId) throws Exception
    {
        // Create an url connection
        URL urls = null;
        if(sessionId.equals("")) urls = new URL(url);
        else urls = new URL(url + ";jsessionid=" + sessionId);
        con = urls.openConnection();         
        con.setUseCaches(false);
        con.setDoOutput(true);
        con.setDoInput(true);
    }

    /**
     * This method sends messages to the server.
     * Then, it gets messages from the server.
     * The method displays the returned error message if there is one, then
     * returns the returned object from the server.
     * @param messagesOut an Object containing messages to be sent out.
     * @return Object an Object returned from the server as the result.
     * @exception IOException an io exception of some sort.
     */
    public Object talk(Object messagesOut)
	throws IOException
    {
        // Prepare for the return
        Object messagesIn = null;      
       
        // Data will always be written to a byte array buffer so
        // that we can tell the server the length of the data
        ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
        
        // Create the output stream to be used to write the data
        // to our buffer
        ObjectOutputStream out = new ObjectOutputStream(byteOut);
        
        // write the data
        out.writeObject(messagesOut);
            
        // Flush the data to the buffer
        out.flush();
       
        // Get buffer to be sent
        byte[] buf = byteOut.toByteArray();
        
        // Set the content type that we are sending
        con.setRequestProperty("Content-type", "application/octet-stream");
        
        // Set the length of the data buffer we are sending
        con.setRequestProperty("Content-length", "" + buf.length);
        
        // Get the output stream to the server and send our data buffer
        DataOutputStream dataOut = new DataOutputStream(con.getOutputStream());
        dataOut.write(buf);
       
        // Flush the output stream and close it
        dataOut.flush();                                                        
        dataOut.close();

        // Get the incoming messages
        ObjectInputStream in = new ObjectInputStream(con.getInputStream());

        try
        {
            // Read the data from the server
            String message = (String)in.readObject();
            if(message.equals("") || message.startsWith("count="))
            {
                messagesIn = in.readObject();
                if(!message.equals(""))
                    count = message.substring(6);
            }
            else if(message.equals("Illegal operation on empty result set"))
            {
                count = "0";
            }
            else
            {
                JOptionPane.showMessageDialog(null, message,  
                                              "Message from the server",
                                              JOptionPane.ERROR_MESSAGE);
            }
        }
        catch(ClassNotFoundException e)
        {
            JOptionPane.showMessageDialog(null, e,  
                                          "Class not found exception",
                                          JOptionPane.ERROR_MESSAGE);            
        }
        
        // Close the input stream
        in.close();

        return messagesIn;        
    }

    // URL connection
    private URLConnection con = null;
    
    /** Number of records returned */
    public static String count = "-1";
}
