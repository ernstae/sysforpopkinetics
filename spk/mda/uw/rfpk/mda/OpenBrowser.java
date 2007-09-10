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
package uw.rfpk.mda;

import uw.rfpk.mda.nonmem.Utility;

/**
 * This class opens a browser to display an url.
 * @author  Jiaji Du
 */
public class OpenBrowser extends javax.swing.JButton
{
    /** Creates a new instance of OpenBrowser*/
    public OpenBrowser() 
    {
        setBackground(new java.awt.Color(255, 255, 255));
        setForeground(new java.awt.Color(0, 0, 200));
        setBorder(new javax.swing.border.EmptyBorder(0, 0, 10, 0));
        addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) 
            {
                Utility.openURL(url);
                setForeground(new java.awt.Color(200, 0, 0));
            }
        });
    }
    /** Get text on the button.
     * @return the text.
     */
    public String getText()
    {
        return text;
    }
    /** Get URL of the web page.
     * @return the URL.
     */
    public String getUrl()
    {
        return url;
    }
    /** Set text on the button.
     * @param t the text.
     */
    public void setText(String t)
    {
        text = t;
    }
    /** Set the URL of the web page.
     * @param u the URL.
     */
    public void setUrl(String u)
    {
        url = u;
    }
    
    // The text on the button.
    private String text;
    // The URL of the web page.
    private String url;
}
