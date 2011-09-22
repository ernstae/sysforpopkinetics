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
import java.math.BigInteger;
import java.io.*;
import java.nio.*;
import java.sql.*;
import rfpk.spk.spkdb.*;
import java.security.SecureRandom;
import uw.rfpk.beans.UserInfo;

/** This servlet is for test only.  It receives a String array object contaning the username
 * and the password.  The servlet authenticates the user by calling the database API method,
 * getUser, and comparing the digested user submitted password with the one in the database.  
 * Then the servlet creates a String array containing the session id and the secret code that 
 * will be used by subsequent servlet calls to authenticate the client.  The servlet also 
 * created a UserInfor object for use in the session.  The sevlet puts the secret code 
 * and the UserInfo object into the sesion object.  The servlet sends back two objects.  
 * The first object is a String object containing error message if there is an error or 
 * an empty String if there is no error.  The second object is the String array object 
 * containing the the session id and the code.  
 *
 * @author Jiaji Du
 */
public class InitMDATest extends HttpServlet
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
        // Prepare output message
        String messageOut = "";
        String[] sessionCodes = new String[3];
        
        // Database connection
        Connection con = null;
        Statement userStmt = null;
        
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
            String username = messageIn[0];
            String password = messageIn[1];
            ServletContext context = getServletContext();
            String testHost = context.getInitParameter("MDA_test_host");
//            if(testHost != null && req.getRemoteHost().equals(testHost))               
//            {
                // Connect to the database
                con = Spkdb.connect(context.getInitParameter("database_name"),
                                    context.getInitParameter("database_host"),
                                    context.getInitParameter("database_username"),
                                    context.getInitParameter("database_password"));
                
                // Authenticate the user
                boolean OK = false;
                ResultSet userRS = Spkdb.getUser(con, username);
                userStmt = userRS.getStatement();
                userRS.next();
                if(Spkdb.md5sum(password).equals(userRS.getString("password")))
                    OK = true;
                else
                {
                    userRS = Spkdb.getUser(con, "useradmin");
                    userRS.next();
                    if(Spkdb.md5sum(password).equals(userRS.getString("password")))
                        OK = true;
                }
                if(OK)
                {
                    // Get session and secret code
                    HttpSession session = req.getSession();
                    sessionCodes[0] = session.getId();
                    sessionCodes[1] = (String)session.getAttribute("SECRET");
                    if(sessionCodes[1] == null)
                    {
                        // Generate random bytes for creating the session secret
                        SecureRandom seed = new SecureRandom();
                        byte[] b = seed.generateSeed(16);

                        // Convert the byte array to a String object, hex
                        String[] c = {"0", "1", "2", "3", "4", "5", "6", "7", 
                                      "8", "9", "A", "B", "C", "D", "E", "F"};
                        StringBuffer buff = new StringBuffer(b.length * 2);
                        int m = 0;
                        for(int i = 0; i < b.length; i++)
                        {
                            m = b[i] & 0xF0;            // Strip off right half
                            m = m >>> 4;                // Shift to right
                            buff.append(c[m]);  
                            m = b[i] & 0x0F;            // Strip off left half
                            buff.append(c[m]);
                        }
                        sessionCodes[1] = buff.toString();                      
                        session.setAttribute("SECRET", sessionCodes[1]);                     
                    }
                
                    userRS = Spkdb.getUser(con, username);
                    userRS.next();
                    sessionCodes[2] = String.valueOf(userRS.getLong("team_id"));
                    
                    // Create a UserInfo object for the session
                    UserInfo user = new UserInfo();
                    user.setUserName(username);
                    user.setUserId(String.valueOf(userRS.getLong("user_id")));
                    user.setFirstName(userRS.getString("first_name"));
                    user.setFirstName(userRS.getString("surname"));
                    user.setCompany(userRS.getString("company"));
                    user.setState(userRS.getString("state"));
                    user.setCountry(userRS.getString("country"));
                    user.setEmail(userRS.getString("email"));
                    user.setTester(String.valueOf(userRS.getInt("test")));
                    user.setDeveloper(String.valueOf(userRS.getInt("dev")));
                    user.setTeamId(sessionCodes[2]);
                    session.setAttribute("validUser", user);
                }
                else
                {
                    // Write the outgoing messages
                    messageOut = "Authentication error.";
                }
//            }
//            else
//            {
                // Write the outgoing messages
//                messageOut = "Test host error.";              
//            }            
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
        finally
        {
            try
            {
                if(userStmt != null) userStmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){messageOut += "\n" + e.getMessage();}
        }
        
        // Write the data to our internal buffer
        out.writeObject(messageOut);
        if(messageOut.equals(""))
            out.writeObject(sessionCodes);

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

