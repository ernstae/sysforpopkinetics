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
package uw.rfpk.mda.nonmem.wizard;

import java.awt.*;
import java.awt.event.*;
import java.util.Properties;
import java.util.*;
import java.text.SimpleDateFormat;

/** Unit tests of the wizard steps.
 * @author  Jiaji Du
 */
public class Test{ 
    /** Creates a new instance of Test. */
    public Test() {
    }
    
    /** Main method of the class.
     * @param args the command line arguments which is not used here.
     */
    public static void main(String[] args) {
        theApp = new Test(); 
        theApp.init();
    }
    
    /** The initialization method.
     */
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
//        calendar.setTimeInMillis(Long.parseLong("1140633044000"));
        calendar.setTimeInMillis(Long.parseLong("1072994943356"));
//        calendar.setTimeInMillis(Long.parseLong("1086299943000"));
        System.out.println("day = " + calendar.get(Calendar.DAY_OF_MONTH));
        System.out.println("month = " + calendar.get(Calendar.MONTH));
        System.out.println("year = " + calendar.get(Calendar.YEAR));        
        System.out.println("hour = " + calendar.get(Calendar.HOUR_OF_DAY));
        System.out.println("minute = " + calendar.get(Calendar.MINUTE));
        System.out.println("second = " + calendar.get(Calendar.SECOND)); 
        System.out.println("time = " + calendar.getTime().getTime());
        
        calendar = new GregorianCalendar();
        calendar.set(2006, 1, 1);
        System.out.println("time = " + calendar.getTimeInMillis());
/*        
        Date date = new Date(Long.parseLong("1075029317964"));
        SimpleDateFormat formatter = new SimpleDateFormat("EEE, MMM, d yyyy 'at' HH:mm:ss z");
        System.out.println("date = " + formatter.format(date));
*/
    }
    private class Iterator extends MDAIterator{
        public Iterator(){
            super(null, false, null, false, false, new javax.swing.JFileChooser(), 0);
            super.setAdvan(9);
            super.setNDataCol(5);
            super.setNTheta(3);
            super.setNEta(7);
            super.setNEps(1);
        }
    }
    private class TestFrame extends javax.swing.JFrame{
        public TestFrame(){
            Iterator i = new Iterator();
//            Aesinitial step = new Aesinitial(i);
//            Aes step = new Aes(i);
//            Covariance step = new Covariance(i);
//            Data step = new Data(i);
//            Des step = new Des(i);
//            Error step = new Error(i);
            Estimation step = new Estimation(i);
//            GettingStarted step = new GettingStarted(i);          
//            Input step = new Input(i);
//            Model step = new Model(i);
//            Omega step = new Omega(i);
//            PK step = new PK(i);
//            Pred step = new Pred(i);
//            Problem step = new Problem(i);
//            Sigma step = new Sigma(i);
//            ScatterPlot step = new ScatterPlot(i); 
//            Simulation step = new Simulation(i);
//            Subroutines step = new Subroutines(i);
//            Table step = new Table(i);
//            Theta step = new Theta(i);
            MDAObject object = new MDAObject();
            Properties records = object.getRecords();
            object.setData(new Vector<Vector<String[]>>());
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
    private class WindowHandler extends WindowAdapter
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
