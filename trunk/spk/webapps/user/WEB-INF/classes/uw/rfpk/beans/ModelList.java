package uw.rfpk.beans;

import java.sql.*;
import java.util.Vector;
import java.io.ByteArrayInputStream;
import org.apache.commons.jrcs.rcs.*;
import org.apache.commons.jrcs.util.ToString;
import org.apache.commons.jrcs.diff.*;
import rfpk.spk.spkdb.*;

/**
 * Get user model list from archive
 * @author Jiaji Du
 * @version 1.0
 */
public class ModelList implements java.io.Serializable
{
    /** Constructor with no argument
     */     
    public ModelList(){}
    
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
    
    /** Gets list of user models.
     * @param maxNum maximum number of models to return.
     * @param leftOff least modelId previously returned (0 if first call in sequence)
     * @return The list of user model.
     */    
    public Vector getModelList(int maxNum, long leftOff)
    {
        // Prepare for the return   
        Vector modelList = new Vector();  

        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(dbName, dbHost, dbUser, dbPass); 
        
            // Get user id
            ResultSet userRS = Spkdb.getUser(con, username);
            userRS.next();
            long userId = userRS.getLong("user_id");
 
            // Get user models
            ResultSet userModelsRS = Spkdb.userModels(con, userId, maxNum, leftOff);
        
            // Disconnect to the database
            Spkdb.disconnect(con);
            
            // Fill in the List
            while(userModelsRS.next())
            {                  
                // Get model id
                long modelId = userModelsRS.getLong("model_id"); 
                    
                // Get model archive
	        Blob blobArchive = userModelsRS.getBlob("archive");
	        long length = blobArchive.length(); 
	        String modelArchive = new String(blobArchive.getBytes(1L, (int)length));                    
                Archive archive = new Archive("", new ByteArrayInputStream(modelArchive.getBytes()));
                    
                // Fill in the list 
                String[] model = new String[5];
                model[0] = String.valueOf(modelId); 
                model[1] = userModelsRS.getString("name");
                model[2] = String.valueOf(archive.getRevisionVersion().last());
                model[3] = archive.findNode(archive.getRevisionVersion()).getDate().toString();
                model[4] = userModelsRS.getString("abstract");
                modelList.add(model);        
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
        return modelList;
    }
        
    private String dbHost = null;
    private String dbName = null;
    private String dbUser = null;
    private String dbPass = null;
    private String dbType = null;
    private String username = null;
}
