package uw.rfpk.beans;

/**
 * User bean contains user information
 * @author Jiaji Du
 * @version 1.0
 */
public class UserInfo implements java.io.Serializable
{
    /** Constructor with no argument
     */     
    public UserInfo(){}
    
    /** Sets userId.
     * @param userId the user ID as a character string.
     */
    public void setUserId(String userId)
    {
        this.userId = userId;
    }
    
    /** Sets userName.
     * @param userName the user name as a character string.
     */
    public void setUserName(String userName)
    {
        this.userName = userName;
    }
    
    /** Sets firstName.
     * @param firstName the first name as a character string.
     */    
    public void setFirstName(String firstName)
    {
        this.firstName = firstName;
    }
    
    /** Sets lastName.
     * @param lastName the last name as a character string.
     */    
    public void setLastName(String lastName)
    {
        this.lastName = lastName; 
    }

    /** Gets userId.
     * @return The user ID as a character string.
     */    
    public String getUserId()
    {
        return userId;
    }
       
    /** Gets userName.
     * @return The user name as a character string.
     */    
    public String getUserName()
    {
        return userName;
    }
    
    /** Gets firstName.
     * @return The first name as a character string.
     */    
    public String getFirstName()
    {
        return firstName;
    }

    /** Gets lastName.
     * @return The last name as a character string.
     */    
    public String getLastName()
    {
        return lastName; 
    }  
    
    private String userId = null;
    private String userName = null;
    private String firstName = null;
    private String lastName = null;
}
