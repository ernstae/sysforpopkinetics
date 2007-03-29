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

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * This bean digests password using MD5 algorithm.
 * @author Jiaji Du
 */
public class DigestPassword implements java.io.Serializable
{
    /** Constructor with no argument.
     */
    public DigestPassword(){}

    /** Digests the password using MD5 algorithm.
     * @param password the original password as a character string.
     */
    public void setPassword(String password)
    {
        try
        {
            MessageDigest algorithm = MessageDigest.getInstance("MD5");
            algorithm.reset();
            algorithm.update(password.getBytes());
            byte[] hash = algorithm.digest();
            this.password = "";
            for(int i = 0; i < hash.length; i++)
            {
                int v = hash[i] & 0xFF;
                if(v < 16) this.password += "0";
                this.password += Integer.toString(v, 16).toLowerCase(); 
            }
        }
        catch(NoSuchAlgorithmException e)
        {
        }
    }
        
    /** Retrns the digested password.
     * @return a character string as the fingerprint of the password.
     */
    public String getPassword()
    {
        return password; 
    }
    
    private String password = null;
}
