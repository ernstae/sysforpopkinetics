package uw.rfpk.mda.nonmem;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.IOException;
import javax.swing.JOptionPane; 

/**
 * This is the main class of the Model Design Agent application. 
 * @author  Jiaji Du
 * @version 1.0
 */
public class MDA
{
    /**
     * The main method that creates the application object and initialize it.
     * @param args A String array containing the server host name, the server
     * port number, session ID and secret code. 
     */
    public static void main(String[] args)
    {
        if(args.length != 0)
        {
            // Determine OS and temporary directory
            String operatingSystem = System.getProperty("os.name");
            String tempDirectory = System.getProperty("java.io.tmpdir");
            
            // Get path for lock file
            String path = null;
            if(operatingSystem.startsWith("Linux") || operatingSystem.startsWith("Unix"))
                path = tempDirectory + "/" + args[0] + ".lock";
            else if(operatingSystem.startsWith("Windows"))
                path = tempDirectory + args[0] + ".lock"; 
            else
                path = args[0] + ".lock";
            
            // Create lock file if there is none
            File lockFile = null;
            try
            {
                lockFile = new File(path);
                if(lockFile.createNewFile())
                {
                    lockFile.deleteOnExit();
                }
                else
                {
                    System.exit(0);
                }
            }
            catch(IOException ioe )
            {
                JOptionPane.showMessageDialog(null, "Error creating lock file", 
                                              "File Error",                     
                                              JOptionPane.ERROR_MESSAGE);
            }
            finally
            {
                lockFile.delete();
            }
        }   
        theApp = new MDA();                       // Create the application object
        theApp.init(args);                        // ...and initialize it
    }

    /** This method initialize the application
     * @param args A String array containing session ID, secret code and user name
     */
    public void init(String[] args)
    {
        window = new MDAFrame("Model Design Agent", args);
        Toolkit theKit = window.getToolkit();           // Get the window toolkit
        Dimension wndSize = theKit.getScreenSize();     // Get screen size

        // Set the position to screen center & size to 3/4 screen size
        window.setBounds(wndSize.width/8, wndSize.height/8,        // Position
                         3*wndSize.width/4, 3*wndSize.height/4);   // Size 
        window.setVisible(true);                        // Display the window
    }

    private static MDAFrame window;                     // The application window
    private static MDA theApp;                          // The application object
}

