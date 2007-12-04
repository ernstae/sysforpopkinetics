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
import java.util.Properties;
import java.text.SimpleDateFormat;
import rfpk.spk.spkdb.*;

/**
 * Converion of state code, end code, and time.
 * @author Jiaji Du
 */
public class Conversion implements java.io.Serializable
{
    /** Constructor with no argument.
     */     
    public Conversion()
    {
    }

    /** Initialize the conversions for state code and end code.
     * @param dbName the name of the database.
     * @param dbHost the name of the database host.
     * @param dbUser the username of the database.
     * @param dbPass the password of the database user.
     */    
    public void initConversion(String dbName, String dbHost, String dbUser, String dbPass)
    {
        // Database connection
        Connection con = null;
        Statement stateStmt = null;
        Statement endStmt = null;
        
        try
        {
            // Connect to the database
            con = Spkdb.connect(dbName, dbHost, dbUser, dbPass); 

            // Set state_code conversion
            ResultSet stateRS = Spkdb.getStateTable(con);
            stateStmt = stateRS.getStatement();
            while(stateRS.next())
            stateConv.setProperty(stateRS.getString(1), stateRS.getString(2));

            // Set end_code conversion
            ResultSet endRS = Spkdb.getEndTable(con);
            endStmt = endRS.getStatement();
            while(endRS.next())
                endConv.setProperty(endRS.getString(1), endRS.getString(2));            
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
                if(stateStmt != null) stateStmt.close();
                if(endStmt != null) endStmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){}
        }
    }
    
    /** Sets time to formated time.
     * @param time a long number to second.
     */    
    public void setTime(String time)
    {
        SimpleDateFormat formater = new SimpleDateFormat("EEE, MMM, d yyyy 'at' HH:mm:ss z");
        this.time = formater.format(new Date(Long.parseLong(time) * 1000));
    }
    
    /** Sets state code to long state code.
     * @param state the short state code.
     */    
    public void setState(String state)
    {
        this.state = stateConv.getProperty(state);
    }    
    
    /** Sets end code to long end code.
     * @param end the short end code.
     */    
    public void setEnd(String end)
    {
        if(end != "")
            this.end = endConv.getProperty(end);  
        else
            this.end = "";
    }
    
    /** Gets formated time.
     * @return the formated time.
     */    
    public String getTime()
    {
        return time;  
    }  
    
    /** Gets long state code.
     * @return the long state code
     */    
    public String getState()
    {
        return state;
    }
    
    /** Gets long end code.
     * @return the long end code.
     */    
    public String getEnd()
    {
        return end;
    }

    private Properties stateConv = new Properties();
    private Properties endConv = new Properties();
    private String state = null;
    private String end = null;
    private String time = null;
}
