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
import java.io.ByteArrayInputStream;
import org.apache.commons.jrcs.rcs.*;
import org.apache.commons.jrcs.util.ToString;
import org.apache.commons.jrcs.diff.*;
import rfpk.spk.spkdb.*;

/**
 * Get version list of a model or a dataset.
 * @author Jiaji
 */
public class VersionList implements java.io.Serializable
{
    /** Constructor with no argument.
     */     
    public VersionList(){}
       
    /** Gets list of versions.
     * @return the list of versions.
     */    
    public String[][] getVersionList(long id, String type, String dbName, String dbHost,
                                     String dbUser, String dbPass)
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
}