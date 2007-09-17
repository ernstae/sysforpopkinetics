package uw.rfpk.servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
import java.nio.*;
import javax.swing.JOptionPane;

/**
 * This servlet receives a stream and put it in a byte array.  The byte array
 * is decrypted and the results is contained in another byte array.  The first
 * four-bytes of the resulting byte array is an int value that is the length 
 * of the first message.  The following bytes of the length is the content of
 * the first message.  The next four-bytes is another int value that is the 
 * length of the second message.  The messages are converted into String objects.
 * Currently, there are two messages contained in the incoming stream.
 * The servlet uses the first String object as the filename and the last String
 * object as the content of the file .  The servlet saves the file into the 
 * directory /home/jiaji/User/USER_NAME/ and then sends a message back to the 
 * client to acknowledge the reception.  The servlet encodes this outgoing 
 * message in the same way as the incoming messages before sending it out as a 
 * stream.
 *
 * @author Jiaji Du
 * @version 1.0
 */
public class ReceiveFile extends HttpServlet 
{
    /**
     * Dispatches client requests to the protected service method.
     * 
     * @param req  the HttpServletRequest object that contains the request the client made of the servlet
     * @param resp  the HttpServletResponse object that contains the response the servlet returns to the client
     * @exception ServletException
     * @exception IOException
     */
    public void service(HttpServletRequest req, HttpServletResponse resp) 
	throws ServletException, IOException
    {
        // Get the user name of the session for saving the file
        String user = (String)req.getSession().getAttribute("USER_NAME");
        // Prepare output message
        String message = null;

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
            String secret = (String)in.readObject();
            if(secret.equals((String)req.getSession().getAttribute("SECRET")))             
            {                        
                String fileName = (String)in.readObject();
 	        String fileContent = (String)in.readObject();

                // Save the file in the user's directory
                String pathName = "/home/jiaji/jakarta-tomcat-4.1.24/webapps/spk/user/" + user + "/" + fileName;
                File newFile = new File(pathName);   
                newFile.createNewFile();   
                BufferedWriter fileOut = new BufferedWriter(new FileWriter(newFile));
                fileOut.write(fileContent);
                fileOut.close();

                // Write the outgoing message
                message = "The file " + fileName + " has been received.";
            }
            else
            {
                // Write the outgoing message
                message = "Authentication error or session expired.";    
            }
        }
        catch(ClassNotFoundException e)
        {
        }
            
        // Write the data to our internal buffer
        out.writeObject(message);        
        
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
