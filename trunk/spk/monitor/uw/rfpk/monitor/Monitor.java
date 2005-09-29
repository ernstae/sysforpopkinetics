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
import java.nio.*;
import java.net.*;
import javax.net.ssl.*;

/**
 * This is the monitor of the SPK moitoring system.  It is a client.  It calls the compiler daemon.  The
 * run daemon and the SPK web server to check if they are running.  If any of the services is not running,
 * an email message is sent to the specified address.  The running states of the services are recorded
 * in a file.  The monitor will not send email for this service until it is stopped again after running
 * is resumed.
 * @author  Jiaji Du
 */
public class Monitor
{
   /**
    * The main method of the application.
    * @param args a String array containing the host name of the SMTP mail server,
    *                                       the email address of the sender,
    *                                       the email address of the receiver,
    *                                       the host name of the compiler daemon computer,
    *                                       the host name of the run-time daemon computer,
    *                                       the host name of the SPK web server,
    *                                       the port number of the SPK web server,
    *                                       the pathname of the file to record the state.
    */
    public static void main(String[] args)
    {
        char[] state = new char[3];
        try
        {
            BufferedReader in = new BufferedReader(new FileReader(args[7]));
            String line = in.readLine();
            state[0] = line.charAt(0);
            state[1] = line.charAt(1);
            state[2] = line.charAt(2);
            in.close();
        }
        catch(IOException e)
        {
        }
        
        try
        {
            // Get Runtime object
            Runtime runtime = Runtime.getRuntime();
            // Create a subprocess
            String[] c = new String[]{"ssh", args[3], "ps -elf | grep spk"}; 
            Process process = runtime.exec(c);
            process.waitFor();
            if(process.exitValue() < 2)
            {
                // Get stdout of the subprocess
                BufferedInputStream in = new BufferedInputStream(process.getInputStream());
                String stdout = "";
                while(true)
                {
                    int i = in.read();
                    if(i == -1)
                        break;
                    stdout += (char)i;
                }
                in.close();
                if(stdout.indexOf("/usr/bin/perl -w /usr/local/bin/spkprod/spkcmpd.pl spkdb") != -1)
                {
                    state[1] = '1';            
                    System.out.println("The compiler daemon is up.");
                }
                else
                {
                    if(state[1] == '1')
                    {
                        sendEmail(args[0], args[1], args[2], 
                                  "The compiler daemon is down.", "This message was sent by SPK Monitor.");
                        System.out.println("The compiler daemon is down.  A mail was sent.");                
                    }
                    else
                        System.out.println("The compiler daemon is down.");
                    state[1] = '0';
                }
            }
            // Create a subprocess
            c = new String[]{"ssh", args[4], "ps -elf | grep spk"}; 
            process = runtime.exec(c);
            process.waitFor();
            if(process.exitValue() < 2)
            {
                // Get stdout of the subprocess
                BufferedInputStream in = new BufferedInputStream(process.getInputStream());
                String stdout = "";
                while(true)
                {
                    int i = in.read();
                    if(i == -1)
                        break;
                    stdout += (char)i;
                }
                in.close();
                if(stdout.indexOf("/usr/bin/perl -w /usr/local/bin/spkprod/spkrund.pl spkdb") != -1)
                {
                    state[1] = '1';            
                    System.out.println("The run-time daemon is up.");
                }
                else
                {
                    if(state[1] == '1')
                    {
                        sendEmail(args[0], args[1], args[2], 
                                  "The run-time daemon is down.", "This message was sent by SPK Monitor.");
                        System.out.println("The run-time daemon is down.  A mail was sent.");                
                    }
                    else
                        System.out.println("The run-time daemon is down.");
                    state[1] = '0';
                }
            }
        }
        catch(IOException e)
        {
        }
        catch(InterruptedException e)
        {
        }

        TrustManager[] trustAllCerts = new TrustManager[]
        {
            new X509TrustManager() 
            {
                public java.security.cert.X509Certificate[] getAcceptedIssuers() 
                {
                    return null;
                }
                public void checkClientTrusted(java.security.cert.X509Certificate[] certs, String authType) 
                {
                }
                public void checkServerTrusted(java.security.cert.X509Certificate[] certs, String authType) 
                {
                }
            }
        };
    
        try
        {
            // Install the all-trusting trust manager
            SSLContext sc = SSLContext.getInstance("SSL");
            sc.init(null, trustAllCerts, new java.security.SecureRandom());
	    HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
	    HttpsURLConnection.setDefaultHostnameVerifier(new NoHostnameVerifier());

            URL url = new URL("https://" + args[5] + ":" + args[6] + "/user/index.jsp");
            BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
            String message = in.readLine();
            state[2] = '1';
            System.out.println("The SPK web server is up.");
        }
        catch(IOException e)
        {
            if(state[2] == '1')
            {
                sendEmail(args[0], args[1], args[2], 
                         "The SPK web server is down.", "This message was sent by SPK Monitor.");
                System.out.println("The SPK web server is down.  A mail was sent.");
            }
            state[2] = '0';
            System.out.println("The SPK web server is down.");
        }
        catch(java.security.NoSuchAlgorithmException e)
        {            
        }
        catch(java.security.KeyManagementException e)
        {
        }
        
        try
        {
            BufferedWriter out = new BufferedWriter(new FileWriter(args[7]));
            out.write(new String(state));
            out.flush();
        }
        catch(IOException e)
        {
            
        }
    }

    private static class NoHostnameVerifier implements HostnameVerifier
    {
        public boolean verify(String hostname, SSLSession session){return true;}
    }

    
    private static void sendEmail(String mailServer, String from, String to, String subject, String message)
    {
        try
        {
            Socket s = new Socket(mailServer, 25);
            PrintWriter out = new PrintWriter(s.getOutputStream());
            String hostName = InetAddress.getLocalHost().getHostName();
            out.print("HELO " + hostName + "\r\n");out.flush();
            out.print("MAIL FROM: <" + from +">\r\n");out.flush();
            out.print("RCPT TO: <" + to + ">\r\n");out.flush();
            out.print("DATA\r\n");out.flush();
            out.print("To: " + to + "\nSubject: " + subject + "\n" + message +
                      "\r\nThe monitor is on host: " + hostName + ".\r\n");out.flush();
            out.print(".\r\n");out.flush();
            s.close();
        }
        catch(IOException e)
        {
            System.out.println(e);
        }
    }
}
