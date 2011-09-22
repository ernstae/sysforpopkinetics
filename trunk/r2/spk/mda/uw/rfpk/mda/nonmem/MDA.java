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
package uw.rfpk.mda.nonmem;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.IOException;
import javax.net.ssl.*;
import javax.swing.JOptionPane;

/**
 * This is the main class of the Model Design Agent application.
 * @author  Jiaji Du
 */
public class MDA
{
    /**
     * The main method that creates the application object and initialize it.
     * @param args a String array containing the server host name, the server
     * port number, session ID, secret code, 1/0 indicating tester/non-tester,
     * 1/0 indicating developer/non-developer.
     */
    public static void main(String[] args)
    {
        if(!System.getProperty("java.version").startsWith("1.5.") && !System.getProperty("java.version").startsWith("1.6."))
        {
            JOptionPane.showMessageDialog(null, "The version of Java Runtime Environmemt (JRE) on your computer is no longer supported.\n" +
                                          "Please update the JRE version and re-download MDA.", "JRE Version Error", JOptionPane.ERROR_MESSAGE);
            System.exit(0);
        }
        if(args.length != 0)
        {
            // Create a lock file if there is none
            String path = System.getProperty("user.home").trim() + System.getProperty("file.separator") +
                          "." + args[2] + "_lock";

            File lockFile = null;
            try
            {
                lockFile = new File(path);
                if(lockFile.createNewFile())
                    lockFile.deleteOnExit();
                else
                {
                    JOptionPane.showMessageDialog(null, "The MDA is already active.",
                                                  "Starting MDA Error",
                                                  JOptionPane.ERROR_MESSAGE);
                    System.exit(0);
                }
            }
            catch(IOException ioe)
            {
                JOptionPane.showMessageDialog(null, "Error creating lock file",
                                              "File Error",
                                              JOptionPane.ERROR_MESSAGE);
                System.exit(0);
            }
        }
        new MDA().init(args);
    }

    private void init(String[] args)
    {
        MDAFrame window = new MDAFrame("Model Design Agent", args);
        Toolkit theKit = window.getToolkit();           // Get the window toolkit
        Dimension wndSize = theKit.getScreenSize();     // Get screen size

        // Set the position to screen center & height to 3/4 screen height & aspect ratio 0.8
        window.setBounds(wndSize.width/8, wndSize.height/8,        // Position
                         3*wndSize.height*5/16, 3*wndSize.height/4);   // Size
        window.setVisible(true);                        // Display the window
    }
}