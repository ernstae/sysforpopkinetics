package uw.rfpk.beans;

import java.sql.*;
import java.util.Vector;
import java.io.ByteArrayInputStream;
import org.apache.commons.jrcs.rcs.*;
import org.apache.commons.jrcs.util.ToString;
import org.apache.commons.jrcs.diff.*;
import rfpk.spk.spkdb.*;

/**
 * Get user dataset list from archive
 * @author Jiaji Du
 * @version 1.0
 */
public class DatasetList implements java.io.Serializable
{
    /** Constructor with no argument
     */     
    public DatasetList(){}
    
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
    
    /** Gets list of user datasets.
     * @param maxNum maximum number of datasets to return.
     * @param leftOff least datasetId previously returned (0 if first call in sequence) 
     * @return The list of user datasets.
     */    
    public Vector getDatasetList(int maxNum, long leftOff)
    {
        // Prepare for the return   
        Vector datasetList = new Vector();  

        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(dbName, dbHost, dbUser, dbPass); 
        
            // Get user id
            ResultSet userRS = Spkdb.getUser(con, username);
            userRS.next();
            long userId = userRS.getLong("user_id");
 
            // Get user models
            ResultSet userDatasetsRS = Spkdb.userDatasets(con, userId, maxNum, leftOff);
        
            // Disconnect to the database
            Spkdb.disconnect(con);
            
            // Fill in the List
            while(userDatasetsRS.next())
            {                  
                // Get dataset id
                long datasetId = userDatasetsRS.getLong("dataset_id"); 
                    
                // Get model archive
	        Blob blobArchive = userDatasetsRS.getBlob("archive");
	        long length = blobArchive.length(); 
	        String dataArchive = new String(blobArchive.getBytes(1L, (int)length));                    
                Archive archive = new Archive("", new ByteArrayInputStream(dataArchive.getBytes()));
                    
                // Fill in the list 
                String[] dataset = new String[5];
                dataset[0] = String.valueOf(datasetId); 
                dataset[1] = userDatasetsRS.getString("name");
                dataset[2] = String.valueOf(archive.getRevisionVersion().last());
                dataset[3] = archive.findNode(archive.getRevisionVersion()).getDate().toString();
                dataset[4] = userDatasetsRS.getString("abstract");
                datasetList.add(dataset);        
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
        return datasetList;
    }
        
    private String dbHost = null;
    private String dbName = null;
    private String dbUser = null;
    private String dbPass = null;
    private String dbType = null;
    private String username = null;
}
