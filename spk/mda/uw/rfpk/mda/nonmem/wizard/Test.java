/*
 * Test.java
 *
 * Created on August 27, 2003, 1:31 PM
 */

package uw.rfpk.mda.nonmem.wizard;

import java.awt.*;
import java.awt.event.*;
import java.util.Properties;
import java.util.*;
import java.text.SimpleDateFormat;

/**
 *
 * @author  jiaji Du
 */
public class Test{ 
    /** Creates a new instance of Test */
    public Test() {
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        theApp = new Test(); 
        theApp.init();
    }
    public void init(){
        TestFrame window = new TestFrame();
        Toolkit theKit = window.getToolkit();           // Get the window toolkit
        Dimension wndSize = theKit.getScreenSize();     // Get screen size

        // Set the position to screen center and appropriate size
        window.setBounds(wndSize.width/6, wndSize.height/6,        // Position
                         500, 380);                                // Size 

        window.addWindowListener(new WindowHandler());  // Add window listener
        window.setVisible(true);

        GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTimeInMillis(Long.parseLong("1075929317964"));       
        System.out.println("day = " + calendar.get(Calendar.DAY_OF_MONTH));
        System.out.println("month = " + calendar.get(Calendar.MONTH));
        System.out.println("year = " + calendar.get(Calendar.YEAR));        
        System.out.println("hour = " + calendar.get(Calendar.HOUR_OF_DAY));
        System.out.println("minute = " + calendar.get(Calendar.MINUTE));
        System.out.println("second = " + calendar.get(Calendar.SECOND)); 
        System.out.println("time = " + calendar.getTime().getTime());
/*        
        Date date = new Date(Long.parseLong("1075029317964"));
        SimpleDateFormat formatter = new SimpleDateFormat("EEE, MMM, d yyyy 'at' HH:mm:ss z");
        System.out.println("date = " + formatter.format(date));
*/
    }
    class Iterator extends MDAIterator{
        public Iterator(){
            super.setAdvan(10);
            super.setNDataCol(5);
            super.setNTheta(3);
            super.setNEta(7);
            super.setNEps(1);
        }
    }
    class TestFrame extends javax.swing.JFrame{
        public TestFrame(){
            Iterator i = new Iterator();
//            Aesinitial step = new Aesinitial();
//            Aes step = new Aes();
//            Data step = new Data(i);
//            Des step = new Des();
//            Error step = new Error(i);
//            GettingStarted step = new GettingStarted(i);
//            Estimation step = new Estimation();
//            Input step = new Input(i);
//            Model step = new Model(i);
//            PK step = new PK(i);
//            Pred step = new Pred(i);
//            Problem step = new Problem();
//            Omega step = new Omega(i); 
//            Sigma step = new Sigma(i);
//            ScatterPlot step = new ScatterPlot(i); 
//            Simulation step = new Simulation();
//            Subroutines step = new Subroutines(i);
//            Table step = new Table(i);
            Theta step = new Theta(i);
            MDAObject object = new MDAObject();
            Properties records = object.getRecords();
            records.setProperty("Aesinitial",  "");
            records.setProperty("Aes",  "");
            records.setProperty("Error",  "");
            records.setProperty("Des",  "");            
            records.setProperty("Input", "$INPUT ID AMT=DOSE TIME DV=CP WT"); 
            records.setProperty("PK", "$PK \nK\nV=\nS1=1\nS2=1\nF1=1\nF0=1\nALAG1=0"); 
            records.setProperty("Pred", "");
            String[] input = object.getRecords().getProperty("Input").split(" ");
            step.getStepDescription().showingStep(new org.netbeans.ui.wizard.JWizardPane(i, object));
            getContentPane().add(step); 
        }
        /**
         * Handles window events.
         * @param e the WindowEvent object to handle.
         */
        protected void processWindowEvent(WindowEvent e) 
        {
            if (e.getID() == WindowEvent.WINDOW_CLOSING)
            {
                dispose();                    // Release resources
                System.exit(0);               // Exit the program
            }
            super.processWindowEvent(e);      // Pass on the event
        }
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
    
    private static TestFrame window;                     // The application window
    private static Test theApp;       
}
