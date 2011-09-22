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
import javax.swing.JOptionPane;
/**
 * User bean contains user information.
 * @author Jiaji Du
 */
public class UserInfo implements java.io.Serializable
{
    /** Constructor with no argument.
     */     
    public UserInfo(){}
    
    /** Sets userId.
     * @param userId the user ID as a character string.
     */
    public void setUserId(String userId)
    {
        this.userId = userId;
    }
    
    /** Sets teamId.
     * @param teamId the team ID as a character string.
     */
    public void setTeamId(String teamId)
    {
        this.teamId = teamId;
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
     * @param email the email address as a character string.
     */    
    public void setEmail(String email)
    {
        this.email = email; 
    }
        
    /** Sets isTester.
     * @param tester a String "1" if the user is a SPK tester, a String "0" otherwise.
     */    
    public void setTester(String tester)
    {
        this.tester = tester;
    }
        
    /** Sets developer.
     * @param developer a String "1" if the user is a SPK developer, a String "0" otherwise.
     */    
    public void setDeveloper(String developer)
    {
        this.developer = developer; 
    }
    
    /** Gets userId.
     * @return the user ID as a character string.
     */    
    public String getUserId()
    {
        return userId;
    }

    /** Gets teamId.
     * @return the team ID as a character string.
     */    
    public String getTeamId()
    {
        return teamId;
    }
      
    /** Gets userName.
     * @return the user name as a character string.
     */    
    public String getUserName()
    {
        return userName;
    }
    
    /** Gets firstName.
     * @return the first name as a character string.
     */    
    public String getFirstName()
    {
        return firstName;
    }

    /** Gets lastName.
     * @return the last name as a character string.
     */    
    public String getLastName()
    {
        return lastName; 
    }
    
    /** Gets company.
     * @return the company name as a character string.
     */    
    public String getCompany()
    {
        return company; 
    }

    /** Gets country.
     * @return the country name as a character string.
     */    
    public String getCountry()
    {
        return country; 
    }    
    
     /** Gets state.
     * @return the state name as a character string.
     */    
    public String getState()
    {
        return state; 
    }   
    
    /** Gets email.
     * @return the email address as a character string.
     */    
    public String getEmail()
    {
        return email; 
    }    
    
    /** Gets tester.
     * @return a String "1" if the user is a SPK tester, a String "0" otherwise.
     */    
    public String getTester()
    {
        return tester; 
    }

    /** Gets developer.
     * @return a String "1" if the user is a SPK developer, a String "0" otherwise.
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
    private String teamId = null;
}
