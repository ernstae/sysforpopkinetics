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
    
    /** Sets company.
     * @param company the company name as a character string.
     */    
    public void setCompany(String company)
    {
        this.company = company; 
    }
        
    /** Sets country.
     * @param country the country name as a character string.
     */    
    public void setCountry(String country)
    {
        this.country = country; 
    }
        
    /** Sets state.
     * @param state the state name as a character string.
     */    
    public void setState(String state)
    {
        this.state = state; 
    }
        
    /** Sets email.
     * @param state the email address as a character string.
     */    
    public void setEmail(String email)
    {
        this.email = email; 
    }
        
    /** Sets isTester.
     * @param A String "1" if the user is a SPK tester, a String "0" otherwise.
     */    
    public void setTester(String tester)
    {
        this.tester = tester; 
    }
        
    /** Sets developer.
     * @param A String "1" if the user is a SPK developer, a String "0" otherwise.
     */    
    public void setDeveloper(String developer)
    {
        this.developer = developer; 
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
    
    /** Gets company.
     * @return The company name as a character string.
     */    
    public String getCompany()
    {
        return company; 
    }

    /** Gets country.
     * @return The country name as a character string.
     */    
    public String getCountry()
    {
        return country; 
    }    
    
     /** Gets state.
     * @return The state name as a character string.
     */    
    public String getState()
    {
        return state; 
    }   
    
    /** Gets email.
     * @return The email address as a character string.
     */    
    public String getEmail()
    {
        return email; 
    }    
    
    /** Gets tester.
     * @return A String "1" if the user is a SPK tester, a String "0" otherwise.
     */    
    public String getTester()
    {
        return tester; 
    }

    /** Gets developer.
     * @return A String "1" if the user is a SPK developer, a String "0" otherwise.
     */    
    public String getDeveloper()
    {
        return developer; 
    }
    
    private String userId = null;
    private String userName = null;
    private String firstName = null;
    private String lastName = null;
    private String company = null;
    private String country = null;
    private String state = null;
    private String email = null;
    private String tester = null;
    private String developer = null;
}
