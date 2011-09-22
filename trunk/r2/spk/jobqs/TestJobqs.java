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
import uw.rfpk.jobqs.*;
import rfpk.spk.spkdb.*;
import java.io.*;
import java.net.*;
import java.sql.*;
import javax.swing.JOptionPane;

/** This class defines unit tests for JobQueue server.
 * @author  Jiaji Du
 */
public class TestJobqs
{
    /** Main method of the unit tests
     * @param args String array containing database host, database name, database user,
     *  database password, and classpath to mysql-connector and to JobQueue class. 
     */
    public static void main(String args[])
    {
        String host = args[0];
        String dbName = args[1];
        String dbUser = args[2];
        String dbPassword = args[3];
        String classpath = args[4];
        
//        String host = "localhost";
//        String dbName = "spktest";
//        String dbUser = "tester";
//        String dbPassword = "tester";
//        String classpath = "/home/jiaji/r2/src/apps/spk/db/api/java/:/home/jiaji/r2/src/apps/spk/jobqs/:" + 
//        "/home/jiaji/mysql-connector-java-3.0.10-stable/mysql-connector-java-3.0.10-stable-bin.jar:.";
        
        final int maxTests = 32;
        final int port = 9001;
	boolean b = true;
	boolean target = true;
        String s = "";
        int i = 1;
        ResultSet rs;
        
        Connection conn;
	try
        {
            // Populate the test database job table
            System.out.println("Connecting and populating the database ...");
	    conn = Spkdb.connect(dbName, host, dbUser, dbPassword);
            long[] jobs = new long[8];
            for(int j = 0; j < 8; j++)
                jobs[j] = Spkdb.newJob(conn, 1L, "", 1L, "", 1L, "", "", "", 0L, false, false, false);

            Spkdb.setStateCode(conn, jobs[1], "cmp");
            Spkdb.setStateCode(conn, jobs[2], "q2r");
            Spkdb.setStateCode(conn, jobs[3], "run");
            Spkdb.setStateCode(conn, jobs[4], "q2ac");
            Spkdb.setStateCode(conn, jobs[5], "acmp");
            Spkdb.setStateCode(conn, jobs[6], "q2ar");
            Spkdb.setStateCode(conn, jobs[7], "arun");
            System.out.println("Connecting and populating the database completed");
        }
        catch(Exception e)
        {
            System.out.println("Database problem has encountered - connect. >> " + e);
            return;
        }
        
        // Start job-queue server and connect to it
        String[] command = {"java", "-cp", classpath, "uw.rfpk.jobqs.JobQueue", 
                            host, dbName, dbUser, dbPassword, String.valueOf(port), "test"};
        Process process = null;
        Socket socket = null;
        PrintWriter out = null;
        BufferedReader in = null;
        try
        {
            s = "starting and connecting to server";
            System.out.println("Job-queue server starting ...");
            // Start the job-queue server
            process = Runtime.getRuntime().exec(command);
            System.out.println("Job-queue server stated");
            // Connect to the job-queue server
            do
            {
                System.out.println("Job-queue server initializing ...");
                Thread.sleep(1000);
	        socket = new Socket("localhost", port);
            } while(socket == null);
            System.out.println("Job-queue server initialization completed");
            out = new PrintWriter(socket.getOutputStream(), true);            
            in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        }
        catch(Exception e)
        {
            s += " >> " + e;
	    b = false;
	    ok(b, i, s);
	    return;            
        }
	ok(b, i, s);
        
        // Test the server
        for (i = 2; i <= maxTests; i++)
        {
            try
            {
		switch(i)
                {
                    case 2:
                        target = true;
                        s = "job 1 status after server initialization";
                        rs = Spkdb.getJob(conn, 1);
                        rs.next();
                        b = rs.getString("state_code").equals("q2c");
                        break;
                    case 3:
                        target = true;
                        s = "job 2 status after server initialization";
                        rs = Spkdb.getJob(conn, 2);
                        rs.next();
                        b = rs.getString("state_code").equals("q2c");
                        break;
                    case 4:
                        target = true;
                        s = "job 3 status after server initialization";
                        rs = Spkdb.getJob(conn, 3);
                        rs.next();
                        b = rs.getString("state_code").equals("q2r");
                        break;
                    case 5:
                        target = true;
                        s = "job 4 status after server initialization";
                        rs = Spkdb.getJob(conn, 4);
                        rs.next();
                        b = rs.getString("state_code").equals("q2r");
                        break;
                    case 6:
                        target = true;
                        s = "job 5 status after server initialization";
                        rs = Spkdb.getJob(conn, 5);
                        rs.next();
                        b = rs.getString("state_code").equals("q2ac");
                        break;
                    case 7:
                        target = true;
                        s = "job 6 status after server initialization";
                        rs = Spkdb.getJob(conn, 6);
                        rs.next();
                        b = rs.getString("state_code").equals("q2ac");
                        break;
                    case 8:
                        target = true;
                        s = "job 7 status after server initialization";
                        rs = Spkdb.getJob(conn, 7);
                        rs.next();
                        b = rs.getString("state_code").equals("q2ar");
                        break;
                    case 9:
                        target = true;
                        s = "job 8 status after server initialization";
                        rs = Spkdb.getJob(conn, 8);
                        rs.next();
                        b = rs.getString("state_code").equals("q2ar");
                        break;
                    case 10:
                        target = true;
                        s = "listing jobs in q2c queue";
                        out.println("list-q2c");                        
                        b = in.readLine().equals("1 - q2c");
                        b = in.readLine().equals("2 - q2c") && b;
                        break;
                    case 11:
                        target = true;
                        s = "listing jobs in q2r queue";
                        out.println("list-q2r");                        
                        b = in.readLine().equals("3 - q2r");
                        b = in.readLine().equals("4 - q2r") && b;
                        break;
                    case 12:
                        target = true;
                        s = "listing jobs in q2ac queue";
                        out.println("list-q2ac");                        
                        b = in.readLine().equals("5 - q2ac");
                        b = in.readLine().equals("6 - q2ac") && b;
                        break;
                    case 13:
                        target = true;
                        s = "listing jobs in q2ar queue";
                        out.println("list-q2ar");                        
                        b = in.readLine().equals("7 - q2ar");
                        b = in.readLine().equals("8 - q2ar") && b;
                        break;
                    case 14:
                        target = true;
                        s = "listing all jobs in jobList";
                        out.println("list-all");                        
                        b = in.readLine().equals("1 - q2c");
                        b = in.readLine().equals("2 - q2c") && b;
                        b = in.readLine().equals("3 - q2r") && b;
                        b = in.readLine().equals("4 - q2r") && b;
                        b = in.readLine().equals("5 - q2ac") && b;
                        b = in.readLine().equals("6 - q2ac") && b;
                        b = in.readLine().equals("3 - q2ar") && b;
                        b = in.readLine().equals("4 - q2ar") && b;
                        b = true;
                        break;
		    case 15: 
                        target = true;
                        s = "getting jobs from q2c queue";
                        out.println("get-q2c");
                        b = in.readLine().equals("1");
                        out.println("get-q2c");
                        b = in.readLine().equals("2") && b;
                        out.println("get-q2c");
                        b = in.readLine().equals("none") && b;
		        break;
		    case 16:
                        target = true;
                        s = "getting jobs from q2r queue";
                        out.println("get-q2r");
                        b = in.readLine().equals("3");
                        out.println("get-q2r");
                        b = in.readLine().equals("4") && b;
                        out.println("get-q2r");
                        b = in.readLine().equals("none") && b;
		        break;
                    case 17: 
                        target = true;
                        s = "getting jobs from q2ac queue";
                        out.println("get-q2ac");
                        b = in.readLine().equals("5");
                        out.println("get-q2ac");
                        b = in.readLine().equals("6") && b;
                        out.println("get-q2ac");
                        b = in.readLine().equals("none") && b;
		        break;
		    case 18:
                        target = true;
                        s = "getting jobs from q2ar queue";
                        out.println("get-q2ar");
                        b = in.readLine().equals("7");
                        out.println("get-q2ar");
                        b = in.readLine().equals("8") && b;
                        out.println("get-q2ar");
                        b = in.readLine().equals("none") && b;
		        break;
                    case 19: 
                        target = true;
                        s = "adding jobs to q2c queue";
                        out.println("add-q2c-1");
                        b = in.readLine().equals("done");
                        out.println("add-q2c-2");
                        b = in.readLine().equals("done") && b;
                        out.println("get-q2c");
                        b = in.readLine().equals("1") && b;
                        out.println("get-q2c");
                        b = in.readLine().equals("2") && b;
                        out.println("get-q2c");
                        b = in.readLine().equals("none") && b;
		        break;
		    case 20:
                        target = true;
                        s = "adding jobs to q2r queue";
                        out.println("add-q2r-3");
                        b = in.readLine().equals("done");                    
                        out.println("add-q2r-4");
                        b = in.readLine().equals("done");
                        out.println("get-q2r");
                        b = in.readLine().equals("3") && b;
                        out.println("get-q2r");
                        b = in.readLine().equals("4") && b;
                        out.println("get-q2r");
                        b = in.readLine().equals("none") && b;
		        break;
                    case 21: 
                        target = true;
                        s = "adding jobs to q2ac queue";
                        out.println("add-q2c-5");
                        b = in.readLine().equals("done");
                        out.println("get-q2c");
                        b = in.readLine().equals("5") && b;
                        out.println("add-q2c-6");
                        b = in.readLine().equals("done") && b;
                        out.println("get-q2c");
                        b = in.readLine().equals("6") && b;
                        out.println("set-abrt-5");
                        b = in.readLine().equals("done") && b;
                        out.println("set-abrt-6");
                        b = in.readLine().equals("done") && b;
                        out.println("get-q2ac");
                        b = in.readLine().equals("5") && b;
                        out.println("get-q2ac");
                        b = in.readLine().equals("6") && b;
                        out.println("get-q2ac");
                        b = in.readLine().equals("none") && b;
		        break;
		    case 22:
                        target = true;
                        s = "adding jobs to q2ar queue";
                        out.println("add-q2r-7");
                        b = in.readLine().equals("done");
                        out.println("add-q2r-8");
                        b = in.readLine().equals("done") && b;
                        out.println("get-q2r");
                        b = in.readLine().equals("7") && b;
                        out.println("get-q2r");
                        b = in.readLine().equals("8") && b;
                        out.println("set-abrt-8");
                        b = in.readLine().equals("done") && b;
                        out.println("set-abrt-7");
                        b = in.readLine().equals("done") && b;
                        out.println("get-q2ar");
                        b = in.readLine().equals("8") && b;                        
                        out.println("get-q2ar");
                        b = in.readLine().equals("7") && b;
                        out.println("get-q2ac");
                        b = in.readLine().equals("none") && b;
		        break;
                    case 23:
                        target = true;
                        s = "setting q2c job to end";
                        out.println("add-q2c-1");
                        b = in.readLine().equals("done");
                        out.println("set-end-1");
                        b = in.readLine().equals("done") && b;
                        out.println("get-q2c");
                        b = in.readLine().equals("none") && b;
                        break;
                    case 24:
                        target = true;
                        s = "setting q2r job to end";
                        out.println("add-q2r-2");
                        b = in.readLine().equals("done");
                        out.println("set-end-2");
                        b = in.readLine().equals("done") && b;
                        out.println("get-q2r");
                        b = in.readLine().equals("none") && b;
                        break;
                    case 25:
                        target = true;
                        s = "setting q2ac job to end";
                        out.println("add-q2c-3");
                        b = in.readLine().equals("done");
                        out.println("get-q2c");
                        b = in.readLine().equals("3") && b;
                        out.println("set-end-3");
                        b = in.readLine().equals("done") && b;
                        out.println("get-q2ac");
                        b = in.readLine().equals("none") && b;                        
                        break;
                    case 26:
                        target = true;
                        s = "setting q2ar job to end";
                        out.println("add-q2r-4");
                        b = in.readLine().equals("done");
                        out.println("get-q2r");
                        b = in.readLine().equals("4") && b;
                        out.println("set-end-4");
                        b = in.readLine().equals("done") && b;
                        out.println("get-q2ar");
                        b = in.readLine().equals("none") && b;                        
                        break;
                    case 27:
                        target = true;
                        s = "init compiler daemon on cmp";
                        out.println("add-q2c-1");
                        in.readLine();                       
                        out.println("get-q2c");
                        String jobId = in.readLine();
                        out.println("init-cmpd");
                        b = in.readLine().equals("done");
                        out.println("get-q2c");
                        b = in.readLine().equals(jobId) && b;
                        break;
                    case 28:
                        target = true;
                        s = "init compiler daemon on acmp";
                        out.println("get-q2ac");
                        jobId = in.readLine();
                        out.println("init-cmpd");
                        b = in.readLine().equals("done");
                        out.println("get-q2ac");
                        b = in.readLine().equals(jobId) && b;
                        break;
                    case 29:
                        target = true;
                        s = "init run-time daemon on run";
                        out.println("add-q2r-3");
                        in.readLine();
                        out.println("get-q2r");
                        jobId = in.readLine();
                        out.println("init-rund");
                        b = in.readLine().equals("done");
                        out.println("get-q2r");
                        b = in.readLine().equals(" " + jobId) && b;
                        break;
                    case 30:
                        target = true;
                        s = "init run-time daemon on arun";
                        out.println("get-q2ar");
                        jobId = in.readLine();
                        out.println("init-rund");
                        b = in.readLine().equals("done");                        
                        out.println("get-q2ar");
                        b = in.readLine().equals(jobId) && b;
                        break;                        
                    case 31:
                        target = true;
                        s = "calling the server Hi";
                        out.println("Hi");
                        b = in.readLine().equals("Hi");
                        break;
                    case 32:
                        target = true;
                        s = "closing the connection";
                        out.println("close");
                        b = in.readLine().equals("closing");
                        break;
		    default:
		        break;
                }
            }
            catch(Exception e)
            {
                target = true;
                s += " >> " + e;
		b = false;
            }
            ok(b == target, i, s);
        }
        
        // Disconnect to the server and stop the server
        try
        {
            s = "disconnecting from and stopping server";
            
            // Disconnect to the server
            in.close();
            out.close();
            socket.close();
            
            // Stop the job-queuej ob-queue server
            process.destroy();
        }
        catch(Exception e)
        {
            s += " >> " + e;
	    b = false;            
        }
	ok(b, i, s);
        
        // Disconnect to the database
        try
        {
            b = Spkdb.disconnect(conn);
	}
        catch(Exception e)
        {
            System.out.println("Database problem has encountered - disconnect. >> " + e);
            return;
	}
        
	System.out.println(okTests + " tests were ok out of a possible " + (maxTests + 1));
    }
    private static void ok(boolean b, int i, String m) {
	String s = b ? "ok:\t" : "not ok:\t";
	s += i + " - " + m;
	System.out.println(s);
	if (b)
	    okTests++;
    }
    private static int okTests = 0;
}
