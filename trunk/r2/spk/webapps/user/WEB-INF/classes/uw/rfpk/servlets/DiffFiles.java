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
import java.nio.channels.FileChannel;

/** This servlet compares two text files and sends back the differences in unix format.  
 * The servlet receives a String array containing three String objects from the client.
 * The first String object is the secret code to identify the client.  The second String  
 * object is the first text.  The third String object is the second String object.  The 
 * servlet saves the texts in two files and then call Linux command 'diff' to get revision.
 * There is a time limit for running this process.  The time limit is specified in web.xml. 
 * The servlet sends back two objects.  The first object is a String containing the error 
 * message if there is an error, or an empty String if there is not any error.  The second 
 * object is the returning revision as a String object.
 *
 * @author Jiaji Du
 */
public class DiffFiles extends HttpServlet
{
    /**
     * Dispatches client requests to the protected service method.
     * 
     * @param req  the HttpServletRequest object that contains the request the client made of the servlet.
     * @param resp  the HttpServletResponse object that contains the response the servlet returns to the client.
     * @exception ServletException a general exception a servlet can throw when it encounters difficulty
     * @exception IOException an I/O exception of some sort.
     */
    public void service(HttpServletRequest req, HttpServletResponse resp)
	throws ServletException, IOException
    {
        // Prepare output messages
        String messageOut = ""; 
        String revision = "";
        
        // Set time limit
        int timeLimit = Integer.parseInt(getServletContext().getInitParameter("timeLimit"));
        
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
 	        String text1 = messageIn[1]; 
                String text2 = messageIn[2];
                String perlDir = getServletContext().getInitParameter("perlDir");
                String workingDir = "/tmp/";
                
                // Get Runtime object
                Runtime runtime = Runtime.getRuntime();

                // Save passed in Strings into files
                file1 = new File(workingDir + secret + "_1.diff");
                file2 = new File(workingDir + secret + "_2.diff");
                file3 = new File(workingDir + secret + "_3.diff");
                if(file1.exists() || file2.exists())
                {
                    messageOut = "Server is comparing the other pair of files for you.";
                }
                else
                {
                    BufferedWriter out1 = new BufferedWriter(new FileWriter(file1));
                    out1.write(text1);
                    out1.close();        
                    BufferedWriter out2 = new BufferedWriter(new FileWriter(file2));
                    out2.write(text2);
                    out2.close();
                
                    // Start timer              
                    (new Thread(new Timer(timeLimit))).start(); 

                    // Create a subprocess
                    String[] c = new String[]{"perl", perlDir + "diff.pl", workingDir, file1.getPath(), file2.getPath(), file3.getPath()}; 
                    process = runtime.exec(c);
                    process.waitFor();
                    if(process.exitValue() > 1)
                    {
                        file1.delete();
                        file2.delete();
                        file3.delete();
                        messageOut = "Time out.  The time limit is " + timeLimit + " second(s)";
                    }
                    else
                    {
                        revision = openFile(file3);

                        // Destroy the subprocess
                        process.destroy(); 
                    }
                }
            }
            else
            {
                // Write the outgoing messages
                messageOut = "Authentication error.";              
            }                
        }      
        catch(ClassNotFoundException e)
        {
            messageOut += "\n" + e.getMessage();
        }
        catch(InterruptedException e)
        {
            messageOut += "\n" + e.getMessage();
        }        
        finally
        {
            file1.delete();
            file2.delete();
            file3.delete();
            file1 = null;
            file2 = null;
            file3 = null;
            process = null;
        }
        
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(revision);

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
    
    private class Timer implements Runnable
    {
        public Timer(int limit)
        {
            timeLimit = limit;
        }
        public void run() 
        {
            try
            {
                Thread.sleep(timeLimit * 1000);
                process.destroy();
            }
            catch(InterruptedException e)
            {
            }
        }
        // Time limit
        private int timeLimit;
    }
    
    // Declare File objects
    File file1 = null;
    File file2 = null;
    File file3 = null;
    
    // Sub process
    private Process process = null;
}

