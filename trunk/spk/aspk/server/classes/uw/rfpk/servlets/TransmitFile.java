package uw.rfpk.servlets;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
import java.nio.*;
import javax.swing.JOptionPane;

/**
 * This servlet receives a stream and put it in a byte array.  The first
 * four-bytes of the resulting byte array is an int value that is the length 
 * of the first message.  The following bytes of the length is the content of
 * the first message.  The next four-bytes is another int value that is the 
 * length of the second message.  The messages are converted into String objects.
 * Currently, there is only one messages contained in the incoming stream.
 * The servlet uses the String object as the filename to open the requested
 * file in the directory /home/jiaji/jakarta-tomcat-4.1.24/webapps/spk/User/USER_NAME.  
 * The servlet then creats two String objects.  The first String object is the 
 * content of the file and the last String object is the processing result message 
 * to the client.  If the requested file does not exist, the first object will be "".
 * The servlet puts the messages into a byte array in the same way as the incoming 
 * messages and sends it out as a stream.
 *
 * @author Jiaji Du
 * @version 1.0
 */
public class TransmitFile extends HttpServlet
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
        String message1 = null;
        String message2 = null;
        
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
        
                // Open the file in the user's directory
                String pathName = "/home/jiaji/jakarta-tomcat-4.1.24/webapps/spk/user/" + user + "/" + fileName;
                File oldFile = new File(pathName);  
                if(oldFile.exists())
	        {
                    BufferedReader reader = new BufferedReader(new FileReader(pathName));
                    StringBuffer buffer = new StringBuffer();
                    boolean done = false;
                    while(!done)
                    {
                        // Read a line
                        String line = reader.readLine();                            
                        if(line == null) 
                            done = true;
                        else
                            buffer.append(line).append("\n");
	            }                
                    reader.close();

                    // Write the outgoing messages
                    message1 = buffer.toString();
                    message2 = "The file " + fileName + " is transmitted.";
                }
                else
	        {
                    // Write the outgoing messages
                    message1 = "";
                    message2 = "The file " + fileName + " does not exist.";
                }
            }
            else
            {
                // Write the outgoing messages
                message1 = "";
                message2 = "Authentication error.";              
            }
        }
        catch(ClassNotFoundException e)
        {
        }
        
        // Write the data to our internal buffer
        out.writeObject(message1);
        out.writeObject(message2);

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

