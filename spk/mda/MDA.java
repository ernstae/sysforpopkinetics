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
     * @param args It is not used in this program
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
            if(operatingSystem.equals("Linux") || operatingSystem.equals("Unix"))
                path = tempDirectory + "/" + args[0] + ".lock";
            else if(operatingSystem.startsWith("Windows"))
                path = tempDirectory + args[0] + ".lock"; 
            else
                path = args[0] + ".lock";
            // Create lock file if there is none
            try
            {
                File lockFile = new File(path);
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
        }   
        theApp = new MDA();                       // Create the application object
        theApp.init(args);                        // ...and initialize it
    }

    /** This method initialize the application
     * @param args A String array containing the session ID and secret number
     */
    public void init(String[] args)
    {
        window = new MDAFrame("Model Design Agent", args);
        Toolkit theKit = window.getToolkit();           // Get the window toolkit
        Dimension wndSize = theKit.getScreenSize();     // Get screen size

        // Set the position to screen center & size to 2/3 screen size
        window.setBounds(wndSize.width/6, wndSize.height/6,        // Position
                         2*wndSize.width/3, 2*wndSize.height/3);   // Size 
        window.addWindowListener(new WindowHandler());  // Add window listener
        window.setVisible(true);                        // Display the window
    }

    /**
     * Handler class for window events
     */
    class WindowHandler extends WindowAdapter
    {
        // Handler for window closing event
        public void windowClosing(WindowEvent e)
        {
            window.dispose();                           // Release the window resources
            System.exit(0);                             // End the application
        }
    }

    private static MDAFrame window;                     // The application window
    private static MDA theApp;                          // The application object
}

