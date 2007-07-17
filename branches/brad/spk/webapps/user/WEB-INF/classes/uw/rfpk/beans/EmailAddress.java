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
package uw.rfpk.beans;

import java.sql.*;
import rfpk.spk.spkdb.*;

/**
 * Getting user email address list from archive.
 * @author Jiaji Du
 */
public class EmailAddress implements java.io.Serializable
{
    /** Constructor with no argument.
     */     
    public EmailAddress(){}
    
    /** Sets database host.
     * @param dbHost the name of the database host.
     */    
    public void setDbHost(String dbHost)
    {
        this.dbHost = dbHost;
    }
    
    /** Sets database name.
     * @param dbName the name of the database.
     */    
    public void setDbName(String dbName)
    {
        this.dbName = dbName;
    }
    
    /** Sets database username.
     * @param dbUser the username of the database.
     */    
    public void setDbUser(String dbUser)
    {
        this.dbUser = dbUser;
    }    

    /** Sets database password.
     * @param dbPass the password of the database.
     */    
    public void setDbPass(String dbPass)
    {
        this.dbPass = dbPass;
    }    
    
    /** Gets list of user email addresses.
     * @param userType user type: developer, tester or all user.
     * @return the list of user email addresses separated by comma.
     */    
    public String getEmailAddress(String userType)
    {
        // Prepare for the return   
        StringBuffer buf = new StringBuffer();  
        String emailList = null;
        
        // Database connection
        Connection con = null;
        Statement stmt = null;
        
        try
        {
            // Connect to the database
            con = Spkdb.connect(dbName, dbHost, dbUser, dbPass); 
        
            // Get user id
            ResultSet rs = Spkdb.getEmailAddress(con, userType);
            stmt = rs.getStatement();

            // Fill in the List
            while(rs.next())
            {                  
                // Fill in the list 
                buf.append(",");
                buf.append(rs.getString("email"));             
            }
        }      
        catch(SQLException e)
        {
        }    
        catch(SpkdbException e)
        {
        }
        finally
        {
            try
            {
                if(stmt != null) stmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){}
        }
        
        if(buf.length() > 0) emailList = buf.substring(1);
        return emailList;
    }
        
    private String dbHost = null;
    private String dbName = null;
    private String dbUser = null;
    private String dbPass = null;
    private String dbType = null;
}
