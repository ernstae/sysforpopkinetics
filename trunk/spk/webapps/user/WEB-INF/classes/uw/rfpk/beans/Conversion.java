package uw.rfpk.beans;

import java.util.Properties;
import java.sql.Date;
import java.text.SimpleDateFormat;

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
        
        String[] code = {"q2c", "cmp", "q2r", "run", "end", "cerr", "srun"};
        String[] name = {"Queued to compile", "Compiling", "Queued to run", 
                         "Running", "End", "Error found", "Job finished"};
        for(int j = 0; j < 7; j++)
            conversion.setProperty(code[j], name[j]);    
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
        this.state = conversion.getProperty(state);
    }    
    
    /** Sets end code to long end code.
     * @param end the short end code.
     */    
    public void setEnd(String end)
    {
        if(end != "")
            this.end = conversion.getProperty(end);  
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

    private Properties conversion = new Properties();    
    private String state = null;
    private String end = null;
    private String time = null;
}
