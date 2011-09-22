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
import java.util.Vector;
import java.io.ByteArrayInputStream;
//import org.apache.commons.jrcs.rcs.*;
//import org.apache.commons.jrcs.util.ToString;
//import org.apache.commons.jrcs.diff.*;
import rfpk.spk.spkdb.*;
import uw.rfpk.rcs.Archive;

/**
 * Getting user model list from archive.
 * @author Jiaji Du
 */
public class ModelList implements java.io.Serializable
{
    /** Constructor with no argument.
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
     * @return the list of user model.
     */    
    public Vector getModelList(int maxNum, long leftOff)
    {
        // Prepare for the return   
        Vector modelList = new Vector();  

        // Database connection
        Connection con = null;
        Statement userStmt = null;
        Statement userModelsStmt = null;
        
        try
        {
            // Connect to the database
            con = Spkdb.connect(dbName, dbHost, dbUser, dbPass); 
        
            // Get user id
            ResultSet userRS = Spkdb.getUser(con, username);
            userStmt = userRS.getStatement();
            userRS.next();
            long userId = userRS.getLong("user_id");
 
            // Get user models
            ResultSet userModelsRS = Spkdb.userModels(con, userId, maxNum, leftOff);
            userModelsStmt = userModelsRS.getStatement();
            
            // Fill in the List
            while(userModelsRS.next())
            {                  
                // Get model id
                long modelId = userModelsRS.getLong("model_id"); 
                    
                // Get model archive
	        Blob blobArchive = userModelsRS.getBlob("archive");
	        long length = blobArchive.length(); 
	        String modelArchive = new String(blobArchive.getBytes(1L, (int)length));                    
//                Archive archive = new Archive("", new ByteArrayInputStream(modelArchive.getBytes()));
                    
                // Fill in the list 
                String[] model = new String[5];
                model[0] = String.valueOf(modelId); 
                model[1] = userModelsRS.getString("name");
//                model[2] = String.valueOf(archive.getRevisionVersion().last());
//                model[3] = archive.findNode(archive.getRevisionVersion()).getDate().toString();
                model[2] = String.valueOf(Archive.getNumRevision(modelArchive));
                model[3] = Archive.getRevisionDate(modelArchive);
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
        finally
        {
            try
            {
                if(userStmt != null) userStmt.close();
                if(userModelsStmt != null) userModelsStmt.close();
                if(con != null) Spkdb.disconnect(con);
            }
            catch(SQLException e){}
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
