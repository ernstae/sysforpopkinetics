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
package uw.rfpk.monitor;

import java.io.*;
import java.net.*;

/**
 * This is the sensor of the monitoring system for SPK daemons. It is a server.
 * @author  Jiaji Du
 */
public class Sensor
{
    /** 
     * The main method of the application.
     * @param args a String array containing the port number of the server.
     */    
    public static void main(String[] args)
    {
        try
        {
            ServerSocket s = new ServerSocket(Integer.parseInt(args[0]));
            while(true)
            {
                Socket incoming = s.accept();
                Thread t = new ThreadedEchoHandler(incoming);
                t.start();
            }     
        }
        catch(IOException e)
        {
        }
    }
}

/**
 * This is the thread to handle a request.
 * @author  Jiaji Du
 */
class ThreadedEchoHandler extends Thread
{
    /**
     * The construcor to initialize the socket. 
     * @param i the socket.
     */
    public ThreadedEchoHandler(Socket i)
    {
        incoming = i;
    }
    
    /**
     * The run method of the thread.
     */
    public void run()
    {
        try
        {
            BufferedReader in = new BufferedReader(new InputStreamReader(incoming.getInputStream()));
            PrintWriter out = new PrintWriter(incoming.getOutputStream(), true);
            
            out.println("Hi, I'm running.");
            String line = in.readLine();
            if(line != null)
                out.println("Echo: " + line);

            incoming.close();
        }
        catch(Exception e)
        {
        }
    }
    
    private Socket incoming;
}
