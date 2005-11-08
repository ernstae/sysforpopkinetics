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
package uw.rfpk.jobqs;

import java.io.*;
import java.net.*;
import javax.swing.JOptionPane;

/** This class asks the user to wait for the job-queue server to initialize.  It says "Hi"
 * to the server every 10 seconds until it receives "Hi" from the server.  Then it tells
 * the user the initialization hs completed.
 *
 * @author  Jiaji Du
 */
public class Ping 
{
    /** The main method that runs the application.
     * @param args a String array containing the job-queue server host and port.
     */
    public static void main(String[] args)
    {
        Socket socket = null;
        PrintWriter out = null;
        BufferedReader in = null;
        JOptionPane.showMessageDialog(null, "Please wait for job-queue server initialization.");
        while(true)
        {
            try
            {
                socket = new Socket(args[0], Integer.parseInt(args[1]));
                out = new PrintWriter(socket.getOutputStream(), true);            
                in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                out.println("Hi");
                String message = in.readLine();
                if(message.equals("Hi"))
                    JOptionPane.showMessageDialog(null, "Initialization has completed.");
                in.close();
                out.close();
                socket.close();
                System.exit(0);

            }
            catch(IOException e)
            {
            }
            finally
            {
                try
                {
                    if(in != null) in.close();
                    if(out != null) out.close();
                    if(socket != null) socket.close();
                }
                catch(IOException e)
                {
                }
            }
            try
            {
                Thread.sleep(10000);
            }
            catch(InterruptedException e)
            {
            }
        }
    }
}
