package uw.rfpk.beans;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * This bean digests password using MD5 algorithm.
 * @author Jiaji Du
 * @version 1.0
 */
public class DigestPassword implements java.io.Serializable
{
    /** Constructor with no argument
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
     * @return A character string as the fingerprint of the password.
     */
    public String getPassword()
    {
        return password; 
    }  
    
    private String password = null;
}
