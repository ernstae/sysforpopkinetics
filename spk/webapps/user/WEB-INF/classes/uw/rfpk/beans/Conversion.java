package uw.rfpk.beans;

import java.sql.*;
import java.util.Properties;
import java.text.SimpleDateFormat;
import rfpk.spk.spkdb.*;

/**
 * Converion of state code, end code, and time.
 * @author Jiaji Du
 * @version 1.0
 */
public class Conversion implements java.io.Serializable
{
    /** Constructor with no argument
     */     
    public Conversion()
    {
    }

    /** Initialize the conversions for state code and end code.
     * @param dbPass the name of the database.
     * @param dbHost the name of the database host.
     * @param dbUser the username of the database.
     * @param dbPass the password of the database user.
     */    
    public void initConversion(String dbName, String dbHost, String dbUser, String dbPass)
    {
        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(dbName, dbHost, dbUser, dbPass); 

            // Set state_code conversion
            ResultSet stateRS = Spkdb.getStateTable(con);            
            while(stateRS.next())
            stateConv.setProperty(stateRS.getString(1), stateRS.getString(2));

            // Set end_code conversion
            ResultSet endRS = Spkdb.getEndTable(con);                
            while(endRS.next())
                endConv.setProperty(endRS.getString(1), endRS.getString(2));            
            
            // Disconnect to the database
            Spkdb.disconnect(con);
        }
        catch(SQLException e)
        {
        }    
        catch(SpkdbException e)
        {
        }        
    }
    
    /** Sets time to formated time.
     * @param time as a long number to second.
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
     * @return The formated time.
     */    
    public String getTime()
    {
        return time;  
    }  
    
    /** Gets long state code.
     * @return The long state code
     */    
    public String getState()
    {
        return state;
    }
    
    /** Gets long end code.
     * @return The long end code.
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
