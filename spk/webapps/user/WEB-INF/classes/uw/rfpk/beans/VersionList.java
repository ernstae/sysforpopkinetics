package uw.rfpk.beans;

import java.sql.*;
import java.io.ByteArrayInputStream;
import org.apache.commons.jrcs.rcs.*;
import org.apache.commons.jrcs.util.ToString;
import org.apache.commons.jrcs.diff.*;
import rfpk.spk.spkdb.*;

/**
 * Get version list of a model or a dataset.
 * @author Jiaji Du
 * @version 1.0
 */
public class VersionList implements java.io.Serializable
{
    /** Constructor with no argument
     */     
    public VersionList(){}
    
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
    
    /** Sets username of the client.
     * @param username the username of the client.
     */    
    public void setUsername(String username)
    {
        this.username = username;
    }    
    
    /** Gets list of versions.
     * @return The list of versions.
     */    
    public String[][] getVersionList(long id, String type)
    {
        // Prepare for the return   
        String[][] versionList = null;  

        Connection con = null;
        try
        {
            // Connect to the database
            con = Spkdb.connect(dbName, dbHost, dbUser, dbPass); 
        
            // Get model or data archive
            ResultSet archiveRS = null;
            if(type.equals("model"))
                archiveRS = Spkdb.getModel(con, id); 
            if(type.equals("data"))
                archiveRS = Spkdb.getDataset(con, id);
                
            archiveRS.next();
      	    Blob blobArchive = archiveRS.getBlob("archive");
	    long length = blobArchive.length(); 
	    String archive = new String(blobArchive.getBytes(1L, (int)length)); 
            
            // Disconnect to the database
            Spkdb.disconnect(con);          
            
            // Generate version list for the model or the dataset
            Archive arch = new Archive("", new ByteArrayInputStream(archive.getBytes())); 
            int number = arch.getRevisionVersion().last(); 
            versionList = new String[number][4]; 
            for(int i = 0; i < number; i++)
            {
                int n = number - i;
                Node node = arch.findNode(new Version("1." + n));  
                versionList[i][0] = String.valueOf(n);
                versionList[i][1] = node.getAuthor().toString();
                versionList[i][2] = node.getDate().toString();
                versionList[i][3] = arch.getLog("1." + n);
            }            
        }      
        catch(SQLException e)
        {
        }    
        catch(SpkdbException e)
        {
        }
        catch(ParseException e)
        { 
        }
        return versionList;
    }
        
    private String dbHost = null;
    private String dbName = null;
    private String dbUser = null;
    private String dbPass = null;
    private String dbType = null;
    private String username = null;
}
