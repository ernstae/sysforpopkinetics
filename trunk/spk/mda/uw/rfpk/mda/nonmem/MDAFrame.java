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

import javax.help.*; 
import javax.swing.*;
import javax.swing.text.*;  
import javax.print.*;
import javax.print.attribute.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.URL; 
import java.util.Vector;
import java.util.Properties;
import java.util.HashMap;
import java.awt.print.*;
import java.awt.font.*;
import org.netbeans.ui.wizard.*;
import uw.rfpk.mda.nonmem.wizard.*;
import uw.rfpk.mda.nonmem.display.*;
import javax.swing.table.*;  
import java.text.DecimalFormat;

/**
 * This class creates a window for the model design agent.,  There are eleven buttons, 
 * a menu bar and a text area in the window.
 * @author Jiaji Du
 */                                 
public class MDAFrame extends JFrame
{
    /** This is a constructor creating the applications main window.
     * @param title a String as the title of the window.
     * @param args a String array containing the server host name, the server
     * port number, the session ID, the secret code, 1/0 tester or non-tester, 
     * 1/0 developer/non-developer.
     */
    public MDAFrame(String title, String[] args)
    {
        if(args.length == 0)
        {
            LogInDialog logIn = new LogInDialog(this, true);
            args = logIn.getArgs();
        }
        
        String metal    = "javax.swing.plaf.metal.MetalLookAndFeel";
        String windows  = "com.sun.java.swing.plaf.windows.WindowsLookAndFeel";
    
        // set Look & Feel
        try 
        {
            UIManager.setLookAndFeel(metal);
        } 
        catch (Exception e) 
        {
	
	}   

        setTitle(title);                              
        initComponents();
        cutMenu.addActionListener(new DefaultEditorKit.CutAction());  
        copyMenu.addActionListener(new DefaultEditorKit.CopyAction());  
        pasteMenu.addActionListener(new DefaultEditorKit.PasteAction()); 
        
        // Check server connection and get method table
        if(args.length != 6)
            isOnline = false;
        else
        {
            try
            {
                // Talk to the server to see if it is on line and get method table if it is 
                Network network = new Network("https://" + args[0] + ":" + args[1] +
                                      "/user/servlet/uw.rfpk.servlets.TestConnection",
                                      args[2]);
                methodTable = (HashMap)network.talk(args[3]);
            }
            catch(Exception e)
	    {
                isOnline = false;
            }  
        }

        if(isOnline)
      	{
            server = new Server(args);
            serverName = args[0];
            serverPort = args[1];
            isTester = args[4].equals("1") ? true : false;
            isDeveloper = args[5].equals("1") ? true : false;
        }
        else
	{
            SubmitJobButton.setEnabled(false);
            GetReportButton.setEnabled(false);
            ModelArchiveButton.setEnabled(false);
            DataArchiveButton.setEnabled(false);
            JobExamplesButton.setEnabled(false);
            ModelLibraryButton.setEnabled(false);
            DatasetLibraryButton.setEnabled(false);
            CompareFilesButton.setEnabled(false); 
            jLabel16.setText("Status: Off Line");
        }
    
        ActionListener taskPerformer = new ActionListener() {
            public void actionPerformed(ActionEvent evt) 
            {
                timer.stop();
                showArchiveList(true);
            }
        };
        timer = new Timer(30000, taskPerformer);
        timer.setRepeats(false);
        
        // Setup help
        setupHelp();
    }
    
    private void setupHelp()
    {
        HelpSet hs = null;
        try
        {
            ClassLoader cl = MDAFrame.class.getClassLoader();
            String hsName = "helpfile.hs";
            URL hsURL = HelpSet.findHelpSet(cl, hsName);
            hs = new HelpSet(null, hsURL);
        }
        catch(Exception e)
        {
            System.out.println(e);
            return;
        }
        helpBroker = hs.createHelpBroker();
        helpBroker.enableHelpKey(this.getRootPane(), "Introduction", hs);
        CSH.setHelpIDString(helpButton, "Introduction");
        HelpButton.addActionListener(new CSH.DisplayHelpFromSource(helpBroker));
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        archiveDialog = new javax.swing.JDialog();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jPanel2 = new javax.swing.JPanel();
        jRadioButton1 = new javax.swing.JRadioButton();
        jRadioButton2 = new javax.swing.JRadioButton();
        jRadioButton3 = new javax.swing.JRadioButton();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jTextField2 = new javax.swing.JTextField();
        jTextField3 = new javax.swing.JTextField();
        jPanel3 = new javax.swing.JPanel();
        jRadioButton4 = new javax.swing.JRadioButton();
        jRadioButton5 = new javax.swing.JRadioButton();
        jRadioButton6 = new javax.swing.JRadioButton();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jTextField4 = new javax.swing.JTextField();
        jTextField5 = new javax.swing.JTextField();
        jTextField6 = new javax.swing.JTextField();
        jPanel5 = new javax.swing.JPanel();
        jLabel7 = new javax.swing.JLabel();
        jTextField7 = new javax.swing.JTextField();
        jRadioButton7 = new javax.swing.JRadioButton();
        jRadioButton8 = new javax.swing.JRadioButton();
        jTextField14 = new javax.swing.JTextField();
        jRadioButton9 = new javax.swing.JRadioButton();
        jRadioButton10 = new javax.swing.JRadioButton();
        jRadioButton11 = new javax.swing.JRadioButton();
        jRadioButton12 = new javax.swing.JRadioButton();
        buttonGroup1 = new javax.swing.ButtonGroup();
        errorMessageDialog = new javax.swing.JDialog();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        warningMessageDialog = new javax.swing.JDialog();
        jScrollPane7 = new javax.swing.JScrollPane();
        jTextArea5 = new javax.swing.JTextArea();
        objectiveDialog = new javax.swing.JDialog();
        jTextArea2 = new javax.swing.JTextArea();
        reportDialog = new javax.swing.JDialog();
        jPanel4 = new javax.swing.JPanel();
        previousButton = new javax.swing.JButton();
        nextButton = new javax.swing.JButton();
        jScrollPane3 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        buttonGroup2 = new javax.swing.ButtonGroup();
        versionDialog = new javax.swing.JDialog();
        jScrollPane4 = new javax.swing.JScrollPane();
        jTable2 = new javax.swing.JTable();
        diffDialog = new javax.swing.JDialog();
        jPanel6 = new javax.swing.JPanel();
        jPanel9 = new javax.swing.JPanel();
        modelLButton = new javax.swing.JButton();
        dataLButton = new javax.swing.JButton();
        modelLibLButton = new javax.swing.JButton();
        dataLibLButton = new javax.swing.JButton();
        localLButton = new javax.swing.JButton();
        jInternalFrame2 = new javax.swing.JInternalFrame();
        jScrollPane5 = new javax.swing.JScrollPane();
        jTextArea3 = new javax.swing.JTextArea();
        jPanel11 = new javax.swing.JPanel();
        jLabel14 = new javax.swing.JLabel();
        jTextField8 = new javax.swing.JTextField();
        refreshButton = new javax.swing.JButton();
        compareButton = new javax.swing.JButton();
        jPanel7 = new javax.swing.JPanel();
        jPanel10 = new javax.swing.JPanel();
        modelRButton = new javax.swing.JButton();
        dataRButton = new javax.swing.JButton();
        modelLibRButton = new javax.swing.JButton();
        dataLibRButton = new javax.swing.JButton();
        localRButton = new javax.swing.JButton();
        jInternalFrame3 = new javax.swing.JInternalFrame();
        jScrollPane6 = new javax.swing.JScrollPane();
        jTextArea4 = new javax.swing.JTextArea();
        jPanel12 = new javax.swing.JPanel();
        nextDiffButton = new javax.swing.JButton();
        helpButton = new javax.swing.JButton();
        jLabel15 = new javax.swing.JLabel();
        jTextField9 = new javax.swing.JTextField();
        diffHelpDialog = new javax.swing.JDialog();
        jPanel8 = new javax.swing.JPanel();
        jLabel13 = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        jLabel10 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        buttonGroup3 = new javax.swing.ButtonGroup();
        jPanel1 = new javax.swing.JPanel();
        jTextPane1 = new javax.swing.JTextPane();
        jTextPane2 = new javax.swing.JTextPane();
        jTextPane3 = new javax.swing.JTextPane();
        WriteInputButton = new javax.swing.JButton();
        SubmitJobButton = new javax.swing.JButton();
        GetReportButton = new javax.swing.JButton();
        ReadOutputButton = new javax.swing.JButton();
        JobExamplesButton = new javax.swing.JButton();
        ModelArchiveButton = new javax.swing.JButton();
        DataArchiveButton = new javax.swing.JButton();
        ModelLibraryButton = new javax.swing.JButton();
        DatasetLibraryButton = new javax.swing.JButton();
        CompareFilesButton = new javax.swing.JButton();
        HelpButton = new javax.swing.JButton();
        jInternalFrame1 = new javax.swing.JInternalFrame();
        jScrollPane1 = new javax.swing.JScrollPane();
        textArea = new javax.swing.JTextArea();
        jMenuBar1 = new javax.swing.JMenuBar();
        jMenu6 = new javax.swing.JMenu();
        openMenu = new javax.swing.JMenuItem();
        closeMenu = new javax.swing.JMenuItem();
        jSeparator4 = new javax.swing.JSeparator();
        saveMenu = new javax.swing.JMenuItem();
        savaAsMenu = new javax.swing.JMenuItem();
        jSeparator5 = new javax.swing.JSeparator();
        printMenu = new javax.swing.JMenuItem();
        jSeparator6 = new javax.swing.JSeparator();
        exitMenu = new javax.swing.JMenuItem();
        jMenu7 = new javax.swing.JMenu();
        cutMenu = new javax.swing.JMenuItem();
        copyMenu = new javax.swing.JMenuItem();
        pasteMenu = new javax.swing.JMenuItem();
        findMenu = new javax.swing.JMenuItem();
        jMenu9 = new javax.swing.JMenu();
        errorMenu = new javax.swing.JMenuItem();
        warningMenu = new javax.swing.JMenuItem();
        objectiveMenu = new javax.swing.JMenuItem();
        parameterMenu = new javax.swing.JMenu();
        ThetaMenu = new javax.swing.JMenuItem();
        OmegaMenu = new javax.swing.JMenuItem();
        SigmaMenu = new javax.swing.JMenuItem();
        statisticsMenu = new javax.swing.JMenu();
        stdErrorMenu = new javax.swing.JMenu();
        stdErrThetaMenu = new javax.swing.JMenuItem();
        stdErrOmegaMenu = new javax.swing.JMenuItem();
        stdErrSigmaMenu = new javax.swing.JMenuItem();
        covarianceMenu = new javax.swing.JMenuItem();
        correlationMenu = new javax.swing.JMenuItem();
        invCovarianceMenu = new javax.swing.JMenuItem();
        tableMenu = new javax.swing.JMenuItem();
        scatterPlotMenu = new javax.swing.JMenu();
        dotsMenu = new javax.swing.JMenuItem();
        lineMenu = new javax.swing.JMenuItem();
        bothMenu = new javax.swing.JMenuItem();
        summaryMenu = new javax.swing.JMenuItem();
        traceMenu = new javax.swing.JMenuItem();
        jMenu1 = new javax.swing.JMenu();
        dotsPlotMenu = new javax.swing.JMenuItem();
        linePlotMenu = new javax.swing.JMenuItem();
        bothPlotMenu = new javax.swing.JMenuItem();
        useRMenu = new javax.swing.JMenuItem();
        jLabel16 = new javax.swing.JLabel();

        archiveDialog.getContentPane().setLayout(new java.awt.GridBagLayout());

        archiveDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        archiveDialog.setTitle("Job Submission Dialog");
        archiveDialog.setLocationRelativeTo(this);
        archiveDialog.setModal(true);
        archiveDialog.setResizable(false);
        okButton.setText("OK");
        okButton.setMaximumSize(new java.awt.Dimension(75, 25));
        okButton.setMinimumSize(new java.awt.Dimension(75, 25));
        okButton.setPreferredSize(new java.awt.Dimension(75, 25));
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(12, 45, 12, 14);
        archiveDialog.getContentPane().add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(12, 15, 12, 37);
        archiveDialog.getContentPane().add(cancelButton, gridBagConstraints);

        jTabbedPane1.setMinimumSize(new java.awt.Dimension(280, 240));
        jTabbedPane1.setPreferredSize(new java.awt.Dimension(280, 280));
        jPanel2.setLayout(new java.awt.GridBagLayout());

        jPanel2.setMinimumSize(new java.awt.Dimension(300, 200));
        jPanel2.setPreferredSize(new java.awt.Dimension(300, 300));
        jRadioButton1.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton1.setSelected(true);
        jRadioButton1.setText("New model (NONMEM control file)");
        buttonGroup1.add(jRadioButton1);
        jRadioButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 0, 0, 0);
        jPanel2.add(jRadioButton1, gridBagConstraints);

        jRadioButton2.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton2.setText("New version of an existing model");
        buttonGroup1.add(jRadioButton2);
        jRadioButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 0, 3, 0);
        jPanel2.add(jRadioButton2, gridBagConstraints);

        jRadioButton3.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton3.setText("Existing version of an existing model  ");
        buttonGroup1.add(jRadioButton3);
        jRadioButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton3ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel2.add(jRadioButton3, gridBagConstraints);

        jLabel1.setFont(new java.awt.Font("Default", 0, 12));
        jLabel1.setText("model name (<=20 characters)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(jLabel1, gridBagConstraints);

        jLabel2.setFont(new java.awt.Font("Default", 0, 12));
        jLabel2.setText("short description (<= 100characters)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(jLabel2, gridBagConstraints);

        jLabel3.setFont(new java.awt.Font("Default", 0, 12));
        jLabel3.setText("version");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(jLabel3, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel2.add(jTextField1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel2.add(jTextField2, gridBagConstraints);

        jTextField3.setEditable(false);
        jTextField3.setText("1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel2.add(jTextField3, gridBagConstraints);

        jTabbedPane1.addTab("Model", jPanel2);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel3.setMinimumSize(new java.awt.Dimension(300, 200));
        jPanel3.setPreferredSize(new java.awt.Dimension(300, 300));
        jRadioButton4.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton4.setSelected(true);
        jRadioButton4.setText("New dataset");
        buttonGroup2.add(jRadioButton4);
        jRadioButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton4ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 0, 0, 0);
        jPanel3.add(jRadioButton4, gridBagConstraints);

        jRadioButton5.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton5.setText("New version of an existing dataset");
        buttonGroup2.add(jRadioButton5);
        jRadioButton5.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton5ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 0, 3, 0);
        jPanel3.add(jRadioButton5, gridBagConstraints);

        jRadioButton6.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton6.setText("Existing version of an existing dataset");
        buttonGroup2.add(jRadioButton6);
        jRadioButton6.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton6ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel3.add(jRadioButton6, gridBagConstraints);

        jLabel4.setFont(new java.awt.Font("Default", 0, 12));
        jLabel4.setText("dataset name (<=20 characters )");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jLabel4, gridBagConstraints);

        jLabel5.setFont(new java.awt.Font("Default", 0, 12));
        jLabel5.setText("short description (<=100 characters )");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jLabel5, gridBagConstraints);

        jLabel6.setFont(new java.awt.Font("Default", 0, 12));
        jLabel6.setText("version");
        jLabel6.setToolTipText("new");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jLabel6, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel3.add(jTextField4, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel3.add(jTextField5, gridBagConstraints);

        jTextField6.setEditable(false);
        jTextField6.setText("1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel3.add(jTextField6, gridBagConstraints);

        jTabbedPane1.addTab("Dataset", jPanel3);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jLabel7.setFont(new java.awt.Font("Default", 0, 12));
        jLabel7.setText("short description (<=100 characters)     ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(jLabel7, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel5.add(jTextField7, gridBagConstraints);

        jRadioButton7.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton7.setSelected(true);
        jRadioButton7.setText("Use the method specified in input file");
        buttonGroup3.add(jRadioButton7);
        jRadioButton7.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton7ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jRadioButton7, gridBagConstraints);

        jRadioButton8.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton8.setText("Monte Carlo integration on likelihood ");
        buttonGroup3.add(jRadioButton8);
        jRadioButton8.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton8ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jRadioButton8, gridBagConstraints);

        jTextField14.setEditable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel5.add(jTextField14, gridBagConstraints);

        jRadioButton9.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton9.setText("Use Gibbs Markov Chain Monte Carlo");
        buttonGroup3.add(jRadioButton9);
        jRadioButton9.setEnabled(false);
        jRadioButton9.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton9ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jRadioButton9, gridBagConstraints);

        jRadioButton10.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton10.setText("Grid integration on likelihood");
        buttonGroup3.add(jRadioButton10);
        jRadioButton10.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton10ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jRadioButton10, gridBagConstraints);

        jRadioButton11.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton11.setText("Analytical integration on likelihood");
        buttonGroup3.add(jRadioButton11);
        jRadioButton11.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton11ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jRadioButton11, gridBagConstraints);

        jRadioButton12.setFont(new java.awt.Font("Dialog", 0, 12));
        jRadioButton12.setText("Miser M.C. integration on likelihood");
        buttonGroup3.add(jRadioButton12);
        jRadioButton12.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton12ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jRadioButton12, gridBagConstraints);

        jTabbedPane1.addTab("Job", jPanel5);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 2;
        archiveDialog.getContentPane().add(jTabbedPane1, gridBagConstraints);

        errorMessageDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        errorMessageDialog.setTitle("Error Message");
        errorMessageDialog.setLocationRelativeTo(this);
        jTextArea1.setEditable(false);
        jScrollPane2.setViewportView(jTextArea1);

        errorMessageDialog.getContentPane().add(jScrollPane2, java.awt.BorderLayout.CENTER);

        warningMessageDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        warningMessageDialog.setTitle("Warning Message");
        warningMessageDialog.setLocationRelativeTo(this);
        jTextArea5.setEditable(false);
        jScrollPane7.setViewportView(jTextArea5);

        warningMessageDialog.getContentPane().add(jScrollPane7, java.awt.BorderLayout.CENTER);

        objectiveDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        objectiveDialog.setTitle("Objective");
        objectiveDialog.setLocationRelativeTo(this);
        jTextArea2.setEditable(false);
        objectiveDialog.getContentPane().add(jTextArea2, java.awt.BorderLayout.CENTER);

        reportDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        reportDialog.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent we) {
                timer.stop();
            }
        });
        reportDialog.setTitle("");
        reportDialog.setBackground(java.awt.Color.white);
        reportDialog.setModal(true);
        previousButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("/org/netbeans/ui/wizard/plaf/basic/icons/back.gif")));
        previousButton.setText("Previous " + maxNum);
        previousButton.setMaximumSize(new java.awt.Dimension(127, 26));
        previousButton.setMinimumSize(new java.awt.Dimension(127, 26));
        previousButton.setPreferredSize(new java.awt.Dimension(127, 26));
        previousButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                previousButtonActionPerformed(evt);
            }
        });

        jPanel4.add(previousButton);

        nextButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("/org/netbeans/ui/wizard/plaf/basic/icons/next.gif")));
        nextButton.setText("Next " + maxNum);
        nextButton.setMaximumSize(new java.awt.Dimension(127, 26));
        nextButton.setMinimumSize(new java.awt.Dimension(127, 26));
        nextButton.setPreferredSize(new java.awt.Dimension(127, 26));
        nextButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                nextButtonActionPerformed(evt);
            }
        });

        jPanel4.add(nextButton);

        reportDialog.getContentPane().add(jPanel4, java.awt.BorderLayout.SOUTH);

        jTable1.setFont(new java.awt.Font("Monospaced", 0, 12));
        jTable1.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jTable1.setShowHorizontalLines(false);
        jTable1.setShowVerticalLines(false);
        jTable1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jTable1MouseClicked(evt);
            }
        });

        jScrollPane3.setViewportView(jTable1);

        reportDialog.getContentPane().add(jScrollPane3, java.awt.BorderLayout.CENTER);

        versionDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        versionDialog.setTitle("");
        versionDialog.setLocationRelativeTo(reportDialog);
        versionDialog.setModal(true);
        jTable2.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jTable2.setShowHorizontalLines(false);
        jTable2.setShowVerticalLines(false);
        jTable2.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jTable2MouseClicked(evt);
            }
        });

        jScrollPane4.setViewportView(jTable2);

        versionDialog.getContentPane().add(jScrollPane4, java.awt.BorderLayout.CENTER);

        diffDialog.getContentPane().setLayout(new java.awt.GridLayout(1, 2));

        diffDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        jTextArea3.setText("");
        jTextArea4.setText("");
        diffDialog.setTitle("Comparing Files");
        jPanel6.setLayout(new java.awt.BorderLayout());

        jPanel6.setPreferredSize(new java.awt.Dimension(500, 588));
        modelLButton.setText("My Models");
        modelLButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                modelLButtonActionPerformed(evt);
            }
        });

        jPanel9.add(modelLButton);

        dataLButton.setText("My Data");
        dataLButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dataLButtonActionPerformed(evt);
            }
        });

        jPanel9.add(dataLButton);

        modelLibLButton.setText("Model Lib");
        modelLibLButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                modelLibLButtonActionPerformed(evt);
            }
        });

        jPanel9.add(modelLibLButton);

        dataLibLButton.setText("Data Lib");
        dataLibLButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dataLibLButtonActionPerformed(evt);
            }
        });

        jPanel9.add(dataLibLButton);

        localLButton.setText("Files");
        localLButton.setMaximumSize(new java.awt.Dimension(200, 25));
        localLButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                localLButtonActionPerformed(evt);
            }
        });

        jPanel9.add(localLButton);

        jPanel6.add(jPanel9, java.awt.BorderLayout.NORTH);

        jInternalFrame2.setMinimumSize(new java.awt.Dimension(480, 32));
        jInternalFrame2.setPreferredSize(new java.awt.Dimension(500, 63));
        jInternalFrame2.setVisible(true);
        jScrollPane5.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        jScrollPane5.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        jTextArea3.setFont(new java.awt.Font("Courier", 0, 12));
        jScrollPane5.setViewportView(jTextArea3);

        jInternalFrame2.getContentPane().add(jScrollPane5, java.awt.BorderLayout.CENTER);

        jPanel6.add(jInternalFrame2, java.awt.BorderLayout.CENTER);

        jLabel14.setText("Row:Column");
        jPanel11.add(jLabel14);

        jTextField8.setEditable(false);
        jTextField8.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextField8.setText("0:0");
        jTextField8.setMaximumSize(new java.awt.Dimension(120, 19));
        jTextField8.setMinimumSize(new java.awt.Dimension(120, 19));
        jTextField8.setPreferredSize(new java.awt.Dimension(120, 19));
        jPanel11.add(jTextField8);

        refreshButton.setText("Refresh");
        refreshButton.setMaximumSize(new java.awt.Dimension(110, 25));
        refreshButton.setMinimumSize(new java.awt.Dimension(110, 25));
        refreshButton.setPreferredSize(new java.awt.Dimension(120, 25));
        refreshButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                refreshButtonActionPerformed(evt);
            }
        });

        jPanel11.add(refreshButton);

        compareButton.setText("Compare");
        compareButton.setMaximumSize(new java.awt.Dimension(110, 25));
        compareButton.setMinimumSize(new java.awt.Dimension(110, 25));
        compareButton.setPreferredSize(new java.awt.Dimension(120, 25));
        compareButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                compareButtonActionPerformed(evt);
            }
        });

        jPanel11.add(compareButton);

        jPanel6.add(jPanel11, java.awt.BorderLayout.SOUTH);

        diffDialog.getContentPane().add(jPanel6);

        jPanel7.setLayout(new java.awt.BorderLayout());

        jPanel7.setMinimumSize(new java.awt.Dimension(500, 557));
        modelRButton.setText("My Models");
        modelRButton.setActionCommand("My Model");
        modelRButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                modelRButtonActionPerformed(evt);
            }
        });

        jPanel10.add(modelRButton);

        dataRButton.setText("My Data");
        dataRButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dataRButtonActionPerformed(evt);
            }
        });

        jPanel10.add(dataRButton);

        modelLibRButton.setText("Model Lib");
        modelLibRButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                modelLibRButtonActionPerformed(evt);
            }
        });

        jPanel10.add(modelLibRButton);

        dataLibRButton.setText("Data Lib");
        dataLibRButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dataLibRButtonActionPerformed(evt);
            }
        });

        jPanel10.add(dataLibRButton);

        localRButton.setText("Files");
        localRButton.setMaximumSize(new java.awt.Dimension(200, 10));
        localRButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                localRButtonActionPerformed(evt);
            }
        });

        jPanel10.add(localRButton);

        jPanel7.add(jPanel10, java.awt.BorderLayout.NORTH);

        jInternalFrame3.setMinimumSize(new java.awt.Dimension(480, 32));
        jInternalFrame3.setPreferredSize(new java.awt.Dimension(500, 63));
        jInternalFrame3.setVisible(true);
        jScrollPane6.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        jScrollPane6.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        jTextArea4.setFont(new java.awt.Font("Courier", 0, 12));
        jScrollPane6.setViewportView(jTextArea4);

        jInternalFrame3.getContentPane().add(jScrollPane6, java.awt.BorderLayout.CENTER);

        jPanel7.add(jInternalFrame3, java.awt.BorderLayout.CENTER);

        nextDiffButton.setText("Next Diff");
        nextDiffButton.setMaximumSize(new java.awt.Dimension(110, 25));
        nextDiffButton.setMinimumSize(new java.awt.Dimension(110, 25));
        nextDiffButton.setPreferredSize(new java.awt.Dimension(120, 25));
        nextDiffButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                nextDiffButtonActionPerformed(evt);
            }
        });

        jPanel12.add(nextDiffButton);

        helpButton.setText("Help");
        helpButton.setMaximumSize(new java.awt.Dimension(110, 25));
        helpButton.setMinimumSize(new java.awt.Dimension(110, 25));
        helpButton.setPreferredSize(new java.awt.Dimension(120, 25));
        helpButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                helpButtonActionPerformed(evt);
            }
        });

        jPanel12.add(helpButton);

        jLabel15.setText("Row:Column");
        jPanel12.add(jLabel15);

        jTextField9.setEditable(false);
        jTextField9.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextField9.setText("0:0");
        jTextField9.setMaximumSize(new java.awt.Dimension(120, 19));
        jTextField9.setMinimumSize(new java.awt.Dimension(120, 19));
        jTextField9.setPreferredSize(new java.awt.Dimension(120, 19));
        jPanel12.add(jTextField9);

        jPanel7.add(jPanel12, java.awt.BorderLayout.SOUTH);

        diffDialog.getContentPane().add(jPanel7);

        diffHelpDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        diffHelpDialog.setTitle("Color Code");
        diffHelpDialog.setBackground(java.awt.Color.white);
        diffHelpDialog.setForeground(java.awt.Color.white);
        diffHelpDialog.setLocationRelativeTo(this);
        diffHelpDialog.setResizable(false);
        jPanel8.setLayout(new java.awt.GridBagLayout());

        jPanel8.setBackground(java.awt.Color.white);
        jLabel13.setText("The area has different content.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        jPanel8.add(jLabel13, gridBagConstraints);

        jLabel12.setBackground(java.awt.Color.orange);
        jLabel12.setPreferredSize(new java.awt.Dimension(50, 15));
        jLabel12.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        jPanel8.add(jLabel12, gridBagConstraints);

        jLabel11.setText("This is an inserted empty line.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        jPanel8.add(jLabel11, gridBagConstraints);

        jLabel10.setBackground(java.awt.Color.magenta);
        jLabel10.setPreferredSize(new java.awt.Dimension(50, 15));
        jLabel10.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        jPanel8.add(jLabel10, gridBagConstraints);

        jLabel9.setText("The line only exists in this file.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        jPanel8.add(jLabel9, gridBagConstraints);

        jLabel8.setBackground(java.awt.Color.green);
        jLabel8.setPreferredSize(new java.awt.Dimension(50, 15));
        jLabel8.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        jPanel8.add(jLabel8, gridBagConstraints);

        diffHelpDialog.getContentPane().add(jPanel8, java.awt.BorderLayout.CENTER);

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setBackground(new java.awt.Color(0, 204, 204));
        setLocationRelativeTo(this);
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jPanel1.setBackground(new java.awt.Color(0, 204, 204));
        jPanel1.setMinimumSize(new java.awt.Dimension(751, 546));
        jTextPane1.setBackground(new java.awt.Color(0, 204, 204));
        jTextPane1.setEditable(false);
        jTextPane1.setFont(new java.awt.Font("Default", 0, 24));
        jTextPane1.setText("System For Population Kinetics - model design agent");
        jTextPane1.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 0, 12);
        jPanel1.add(jTextPane1, gridBagConstraints);

        jTextPane2.setBackground(new java.awt.Color(0, 204, 204));
        jTextPane2.setEditable(false);
        jTextPane2.setText("Copyright 2004 Regents of the University of Washington All rights reserved");
        jTextPane2.setFocusable(false);
        jTextPane2.setMinimumSize(new java.awt.Dimension(450, 21));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 0);
        jPanel1.add(jTextPane2, gridBagConstraints);

        jTextPane3.setBackground(new java.awt.Color(0, 204, 204));
        jTextPane3.setEditable(false);
        jTextPane3.setFont(new java.awt.Font("Dialog", 0, 14));
        jTextPane3.setText("NONMEM  Compatible  Version:  0.1");
        jTextPane3.setFocusable(false);
        jTextPane3.setMinimumSize(new java.awt.Dimension(240, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        jPanel1.add(jTextPane3, gridBagConstraints);

        WriteInputButton.setText("Prepare Input");
        WriteInputButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        WriteInputButton.setMaximumSize(new java.awt.Dimension(110, 25));
        WriteInputButton.setMinimumSize(new java.awt.Dimension(110, 25));
        WriteInputButton.setPreferredSize(new java.awt.Dimension(110, 25));
        WriteInputButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                WriteInputButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(16, 12, 6, 12);
        jPanel1.add(WriteInputButton, gridBagConstraints);

        SubmitJobButton.setText("Submit Job");
        SubmitJobButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        SubmitJobButton.setMaximumSize(new java.awt.Dimension(110, 25));
        SubmitJobButton.setMinimumSize(new java.awt.Dimension(110, 25));
        SubmitJobButton.setPreferredSize(new java.awt.Dimension(110, 25));
        SubmitJobButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SubmitJobButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 6, 12);
        jPanel1.add(SubmitJobButton, gridBagConstraints);

        GetReportButton.setText("My Jobs");
        GetReportButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        GetReportButton.setMaximumSize(new java.awt.Dimension(110, 25));
        GetReportButton.setMinimumSize(new java.awt.Dimension(110, 25));
        GetReportButton.setPreferredSize(new java.awt.Dimension(110, 25));
        GetReportButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                GetReportButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(7, 13, 7, 13);
        jPanel1.add(GetReportButton, gridBagConstraints);

        ReadOutputButton.setText("Process Output");
        ReadOutputButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        ReadOutputButton.setMaximumSize(new java.awt.Dimension(110, 25));
        ReadOutputButton.setMinimumSize(new java.awt.Dimension(110, 25));
        ReadOutputButton.setPreferredSize(new java.awt.Dimension(110, 25));
        ReadOutputButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ReadOutputButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 6, 12);
        jPanel1.add(ReadOutputButton, gridBagConstraints);

        JobExamplesButton.setText("Job Examples");
        JobExamplesButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        JobExamplesButton.setMaximumSize(new java.awt.Dimension(110, 25));
        JobExamplesButton.setMinimumSize(new java.awt.Dimension(110, 25));
        JobExamplesButton.setPreferredSize(new java.awt.Dimension(110, 25));
        JobExamplesButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                JobExamplesButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 6, 12);
        jPanel1.add(JobExamplesButton, gridBagConstraints);

        ModelArchiveButton.setText("My Models");
        ModelArchiveButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        ModelArchiveButton.setMaximumSize(new java.awt.Dimension(110, 25));
        ModelArchiveButton.setMinimumSize(new java.awt.Dimension(110, 25));
        ModelArchiveButton.setPreferredSize(new java.awt.Dimension(110, 25));
        ModelArchiveButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ModelArchiveButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 6, 12);
        jPanel1.add(ModelArchiveButton, gridBagConstraints);

        DataArchiveButton.setText("My Datasets");
        DataArchiveButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        DataArchiveButton.setMaximumSize(new java.awt.Dimension(110, 25));
        DataArchiveButton.setMinimumSize(new java.awt.Dimension(110, 25));
        DataArchiveButton.setPreferredSize(new java.awt.Dimension(110, 25));
        DataArchiveButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DataArchiveButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 6, 12);
        jPanel1.add(DataArchiveButton, gridBagConstraints);

        ModelLibraryButton.setText("Model Library");
        ModelLibraryButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        ModelLibraryButton.setMaximumSize(new java.awt.Dimension(110, 25));
        ModelLibraryButton.setMinimumSize(new java.awt.Dimension(110, 25));
        ModelLibraryButton.setPreferredSize(new java.awt.Dimension(110, 25));
        ModelLibraryButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ModelLibraryButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 6, 12);
        jPanel1.add(ModelLibraryButton, gridBagConstraints);

        DatasetLibraryButton.setText("Dataset Library");
        DatasetLibraryButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        DatasetLibraryButton.setMaximumSize(new java.awt.Dimension(110, 25));
        DatasetLibraryButton.setMinimumSize(new java.awt.Dimension(110, 25));
        DatasetLibraryButton.setPreferredSize(new java.awt.Dimension(110, 25));
        DatasetLibraryButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DatasetLibraryButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 6, 12);
        jPanel1.add(DatasetLibraryButton, gridBagConstraints);

        CompareFilesButton.setText("Compare Files");
        CompareFilesButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        CompareFilesButton.setMaximumSize(new java.awt.Dimension(110, 25));
        CompareFilesButton.setMinimumSize(new java.awt.Dimension(110, 25));
        CompareFilesButton.setPreferredSize(new java.awt.Dimension(110, 25));
        CompareFilesButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                CompareFilesButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 6, 12);
        jPanel1.add(CompareFilesButton, gridBagConstraints);

        HelpButton.setText("Help");
        HelpButton.setBorder(new javax.swing.border.BevelBorder(javax.swing.border.BevelBorder.RAISED));
        HelpButton.setMaximumSize(new java.awt.Dimension(110, 25));
        HelpButton.setMinimumSize(new java.awt.Dimension(110, 25));
        HelpButton.setPreferredSize(new java.awt.Dimension(110, 25));
        HelpButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                HelpButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 0, 12);
        jPanel1.add(HelpButton, gridBagConstraints);

        jInternalFrame1.setMinimumSize(new java.awt.Dimension(603, 460));
        jInternalFrame1.setPreferredSize(new java.awt.Dimension(603, 460));
        jInternalFrame1.setVisible(true);
        textArea.setFont(new java.awt.Font("Courier", 0, 12));
        jScrollPane1.setViewportView(textArea);

        jInternalFrame1.getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jMenu6.setText("File");
        openMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, java.awt.event.InputEvent.CTRL_MASK));
        openMenu.setMnemonic('o');
        openMenu.setText("Open");
        openMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                openMenuActionPerformed(evt);
            }
        });

        jMenu6.add(openMenu);

        closeMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C, java.awt.event.InputEvent.CTRL_MASK));
        closeMenu.setMnemonic('c');
        closeMenu.setText("Close");
        closeMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeMenuActionPerformed(evt);
            }
        });

        jMenu6.add(closeMenu);

        jMenu6.add(jSeparator4);

        saveMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.event.InputEvent.CTRL_MASK));
        saveMenu.setMnemonic('s');
        saveMenu.setText("Save");
        saveMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveMenuActionPerformed(evt);
            }
        });

        jMenu6.add(saveMenu);

        savaAsMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, java.awt.event.InputEvent.CTRL_MASK));
        savaAsMenu.setMnemonic('a');
        savaAsMenu.setText("Sava As");
        savaAsMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                savaAsMenuActionPerformed(evt);
            }
        });

        jMenu6.add(savaAsMenu);

        jMenu6.add(jSeparator5);

        printMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P, java.awt.event.InputEvent.CTRL_MASK));
        printMenu.setMnemonic('p');
        printMenu.setText("Print");
        printMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                printMenuActionPerformed(evt);
            }
        });

        jMenu6.add(printMenu);

        jMenu6.add(jSeparator6);

        exitMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_E, java.awt.event.InputEvent.CTRL_MASK));
        exitMenu.setMnemonic('e');
        exitMenu.setText("Exit");
        exitMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exitMenuActionPerformed(evt);
            }
        });

        jMenu6.add(exitMenu);

        jMenuBar1.add(jMenu6);

        jMenu7.setText("Edit");
        cutMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X, java.awt.event.InputEvent.CTRL_MASK));
        cutMenu.setMnemonic('c');
        cutMenu.setText("Cut");
        jMenu7.add(cutMenu);

        copyMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Y, java.awt.event.InputEvent.CTRL_MASK));
        copyMenu.setMnemonic('y');
        copyMenu.setText("Copy");
        jMenu7.add(copyMenu);

        pasteMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T, java.awt.event.InputEvent.CTRL_MASK));
        pasteMenu.setMnemonic('t');
        pasteMenu.setText("Paste");
        jMenu7.add(pasteMenu);

        findMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F, java.awt.event.InputEvent.CTRL_MASK));
        findMenu.setMnemonic('f');
        findMenu.setText("Find");
        findMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findMenuActionPerformed(evt);
            }
        });

        jMenu7.add(findMenu);

        jMenuBar1.add(jMenu7);

        jMenu9.setText("Presentation");
        errorMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R, java.awt.event.InputEvent.CTRL_MASK));
        errorMenu.setMnemonic('r');
        errorMenu.setText("Error Message");
        errorMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                errorMenuActionPerformed(evt);
            }
        });

        jMenu9.add(errorMenu);

        warningMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, java.awt.event.InputEvent.CTRL_MASK));
        warningMenu.setMnemonic('w');
        warningMenu.setText("Warning Message");
        warningMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                warningMenuActionPerformed(evt);
            }
        });

        jMenu9.add(warningMenu);

        objectiveMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_J, java.awt.event.InputEvent.CTRL_MASK));
        objectiveMenu.setMnemonic('j');
        objectiveMenu.setText("Objective");
        objectiveMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                objectiveMenuActionPerformed(evt);
            }
        });

        jMenu9.add(objectiveMenu);

        parameterMenu.setText("Parameters");
        ThetaMenu.setText("THETA");
        ThetaMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ThetaMenuActionPerformed(evt);
            }
        });

        parameterMenu.add(ThetaMenu);

        OmegaMenu.setText("OMEGA");
        OmegaMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                OmegaMenuActionPerformed(evt);
            }
        });

        parameterMenu.add(OmegaMenu);

        SigmaMenu.setText("SIGMA");
        SigmaMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                SigmaMenuActionPerformed(evt);
            }
        });

        parameterMenu.add(SigmaMenu);

        jMenu9.add(parameterMenu);

        statisticsMenu.setText("Statistics");
        stdErrorMenu.setText("StdError");
        stdErrThetaMenu.setText("THETA");
        stdErrThetaMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                stdErrThetaMenuActionPerformed(evt);
            }
        });

        stdErrorMenu.add(stdErrThetaMenu);

        stdErrOmegaMenu.setText("OMEGA");
        stdErrOmegaMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                stdErrOmegaMenuActionPerformed(evt);
            }
        });

        stdErrorMenu.add(stdErrOmegaMenu);

        stdErrSigmaMenu.setText("SIGMA");
        stdErrSigmaMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                stdErrSigmaMenuActionPerformed(evt);
            }
        });

        stdErrorMenu.add(stdErrSigmaMenu);

        statisticsMenu.add(stdErrorMenu);

        covarianceMenu.setText("Covariance");
        covarianceMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                covarianceMenuActionPerformed(evt);
            }
        });

        statisticsMenu.add(covarianceMenu);

        correlationMenu.setText("Correlation");
        correlationMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                correlationMenuActionPerformed(evt);
            }
        });

        statisticsMenu.add(correlationMenu);

        invCovarianceMenu.setText("Inv. Covariance");
        invCovarianceMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                invCovarianceMenuActionPerformed(evt);
            }
        });

        statisticsMenu.add(invCovarianceMenu);

        jMenu9.add(statisticsMenu);

        tableMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_B, java.awt.event.InputEvent.CTRL_MASK));
        tableMenu.setMnemonic('b');
        tableMenu.setText("Tables");
        tableMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                tableMenuActionPerformed(evt);
            }
        });

        jMenu9.add(tableMenu);

        scatterPlotMenu.setText("ScatterPlots");
        dotsMenu.setText("Dots");
        dotsMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dotsMenuActionPerformed(evt);
            }
        });

        scatterPlotMenu.add(dotsMenu);

        lineMenu.setText("Line");
        lineMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                lineMenuActionPerformed(evt);
            }
        });

        scatterPlotMenu.add(lineMenu);

        bothMenu.setText("Both");
        bothMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                bothMenuActionPerformed(evt);
            }
        });

        scatterPlotMenu.add(bothMenu);

        jMenu9.add(scatterPlotMenu);

        summaryMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M, java.awt.event.InputEvent.CTRL_MASK));
        summaryMenu.setMnemonic('m');
        summaryMenu.setText("Summary");
        summaryMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                summaryMenuActionPerformed(evt);
            }
        });

        jMenu9.add(summaryMenu);

        traceMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, java.awt.event.InputEvent.CTRL_MASK));
        traceMenu.setMnemonic('z');
        traceMenu.setText("Optimization Trace");
        traceMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                traceMenuActionPerformed(evt);
            }
        });

        jMenu9.add(traceMenu);

        jMenuBar1.add(jMenu9);

        jMenu1.setText("Plot");
        dotsPlotMenu.setText("Dots");
        dotsPlotMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dotsPlotMenuActionPerformed(evt);
            }
        });

        jMenu1.add(dotsPlotMenu);

        linePlotMenu.setText("Line");
        linePlotMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                linePlotMenuActionPerformed(evt);
            }
        });

        jMenu1.add(linePlotMenu);

        bothPlotMenu.setText("Both");
        bothPlotMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                bothPlotMenuActionPerformed(evt);
            }
        });

        jMenu1.add(bothPlotMenu);

        useRMenu.setText("Use R");
        useRMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                useRMenuActionPerformed(evt);
            }
        });

        jMenu1.add(useRMenu);

        jMenuBar1.add(jMenu1);

        jInternalFrame1.setJMenuBar(jMenuBar1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.gridheight = 12;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 12, 12);
        jPanel1.add(jInternalFrame1, gridBagConstraints);

        jLabel16.setBackground(new java.awt.Color(0, 204, 204));
        jLabel16.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel16.setText("Status:  On Line");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 13;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 16, 16, 12);
        jPanel1.add(jLabel16, gridBagConstraints);

        getContentPane().add(jPanel1, java.awt.BorderLayout.CENTER);

        pack();
    }//GEN-END:initComponents

    private void jRadioButton12ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton12ActionPerformed
        jobMethodCode = "mi";
        jTextField14.setText(((String[])methodTable.get("mi"))[0]);
    }//GEN-LAST:event_jRadioButton12ActionPerformed

    private void jRadioButton11ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton11ActionPerformed
        jobMethodCode = "an";
        jTextField14.setText(((String[])methodTable.get("an"))[0]);
    }//GEN-LAST:event_jRadioButton11ActionPerformed

    private void jRadioButton10ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton10ActionPerformed
        jobMethodCode = "gr";
        jTextField14.setText(((String[])methodTable.get("gr"))[0]);
    }//GEN-LAST:event_jRadioButton10ActionPerformed

    private void warningMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_warningMenuActionPerformed
        if(output != null && output.warning != null)
        {
            String warning = "";
            for(int i = 0; i < output.warning.length; i++)
            {
                warning += "<Warning Message " + (i + 1) + ">\n" + output.warning[i][0] + "\n";
                if(isDeveloper)
                    warning += "This message was issued from file: " + output.warning[i][1] + 
                               " at line: " + output.warning[i][2] + "\n";
                warning += "\n";
            }
            jTextArea5.setText(warning);
            jTextArea5.setCaretPosition(0);            
            warningMessageDialog.setSize(400, 300);
            warningMessageDialog.show();
        }
        else
            JOptionPane.showMessageDialog(null, "The warning message is not available",
                                          "Message Not Found Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_warningMenuActionPerformed

    private void useRMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_useRMenuActionPerformed
        String[] c = null;
        String operatingSystem = System.getProperty("os.name");
        if(operatingSystem.startsWith("Linux") || operatingSystem.startsWith("Unix"))
            c = new String[]{"xterm", "-e", "R"};
        else if(operatingSystem.startsWith("Windows"))
            c = new String[]{"Rgui"};
        try
        {
            Process process = Runtime.getRuntime().exec(c);
        }
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e, "IOException", JOptionPane.ERROR_MESSAGE);
        }
    }//GEN-LAST:event_useRMenuActionPerformed

    private void bothPlotMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_bothPlotMenuActionPerformed
        new PlotTool(textArea.getText(), "both");
    }//GEN-LAST:event_bothPlotMenuActionPerformed

    private void linePlotMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_linePlotMenuActionPerformed
        new PlotTool(textArea.getText(), "line");
    }//GEN-LAST:event_linePlotMenuActionPerformed

    private void dotsPlotMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dotsPlotMenuActionPerformed
        new PlotTool(textArea.getText(), "dots"); 
    }//GEN-LAST:event_dotsPlotMenuActionPerformed

    private void bothMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_bothMenuActionPerformed
        scatterPlot("both");
    }//GEN-LAST:event_bothMenuActionPerformed

    private void lineMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_lineMenuActionPerformed
        scatterPlot("line");
    }//GEN-LAST:event_lineMenuActionPerformed

    private void dotsMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dotsMenuActionPerformed
        scatterPlot("dots");
    }//GEN-LAST:event_dotsMenuActionPerformed

    private void jRadioButton9ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton9ActionPerformed
        jobMethodCode = "mc";
        jTextField14.setText(((String[])methodTable.get("mc"))[0]);
    }//GEN-LAST:event_jRadioButton9ActionPerformed

    private void jRadioButton8ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton8ActionPerformed
        jobMethodCode = "ml";
        jTextField14.setText(((String[])methodTable.get("ml"))[0]);
    }//GEN-LAST:event_jRadioButton8ActionPerformed

    private void jRadioButton7ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton7ActionPerformed
        jobMethodCode = method;
        jTextField14.setText(((String[])methodTable.get(method))[0]);
    }//GEN-LAST:event_jRadioButton7ActionPerformed

    private void traceMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_traceMenuActionPerformed
        if(output != null && output.trace != null)
        {
            saveFile();
            textArea.setText(output.trace);
            textArea.setCaretPosition(0);
            jInternalFrame1.setTitle("Optimization trace");   
            file = null;            
        }
        else
            JOptionPane.showMessageDialog(null, "The optimization trace is not available.", 
                                          "Message Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
        
    }//GEN-LAST:event_traceMenuActionPerformed

    private void dataLibRButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataLibRButtonActionPerformed
        isDiff = true;
        isLeft = false;
        isLibrary = true;
        dataArchive();
    }//GEN-LAST:event_dataLibRButtonActionPerformed

    private void dataLibLButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataLibLButtonActionPerformed
        isDiff = true;
        isLeft = true;
        isLibrary = true;
        dataArchive();
    }//GEN-LAST:event_dataLibLButtonActionPerformed

    private void JobExamplesButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_JobExamplesButtonActionPerformed
        listType = "job";
        isLibrary = true;
        indexList = 0;
        lists = new Vector();
        showArchiveList(false);        
    }//GEN-LAST:event_JobExamplesButtonActionPerformed

    private void DatasetLibraryButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_DatasetLibraryButtonActionPerformed
        isDiff = false;
        isLibrary = true;
        dataArchive();        
    }//GEN-LAST:event_DatasetLibraryButtonActionPerformed

    private void HelpButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_HelpButtonActionPerformed
/*
        if(!isOnline)
            new Help("Instructions for Using Model Design Agent", 
                     GettingStarted.class.getResource("/uw/rfpk/mda/nonmem/help/MDAHelp.html"));
        else
            Utility.openURL("https://" + serverName + ":" + serverPort + "/user/help/MDAHelp.jsp");
*/  
    }//GEN-LAST:event_HelpButtonActionPerformed

    private void modelLibRButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_modelLibRButtonActionPerformed
        isDiff = true;
        isLeft = false;
        isLibrary = true;
        modelArchive();        
    }//GEN-LAST:event_modelLibRButtonActionPerformed

    private void modelLibLButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_modelLibLButtonActionPerformed
        isDiff = true;
        isLeft = true;
        isLibrary = true;
        modelArchive();     
    }//GEN-LAST:event_modelLibLButtonActionPerformed

    private void ModelLibraryButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ModelLibraryButtonActionPerformed
        isDiff = false;
        isLibrary = true;
        modelArchive();        
    }//GEN-LAST:event_ModelLibraryButtonActionPerformed
    
    private void helpButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_helpButtonActionPerformed
        diffHelpDialog.setSize(320, 180);
        diffHelpDialog.show();
    }//GEN-LAST:event_helpButtonActionPerformed

    private void refreshButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_refreshButtonActionPerformed
        if(textL != null)
        {
            jTextArea3.setText(textL);
            jTextArea3.setCaretPosition(0);
        }
        if(textR != null)
        {
            jTextArea4.setText(textR);
            jTextArea4.setCaretPosition(0);
        }
    }//GEN-LAST:event_refreshButtonActionPerformed

    private void findMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_findMenuActionPerformed
        String string = JOptionPane.showInputDialog(null, "Enter a string to find.");
        String text = textArea.getText();
        if(string == null)
            return;
        if(string.equals("") || text.toLowerCase().indexOf(string.toLowerCase()) == -1)
        {
            JOptionPane.showMessageDialog(null, "The string '" + string + "' was not found.", 
                                              "Find Result", JOptionPane.INFORMATION_MESSAGE);
            return;
        }
        removeHighlights(textArea); 
        try 
        {
            Highlighter hilite = textArea.getHighlighter();

            int pos = 0;
    
            // Search for string
            while ((pos = text.toLowerCase().indexOf(string.toLowerCase(), pos)) >= 0) {
             
                hilite.addHighlight(pos, pos + string.length(), 
                                    new DefaultHighlighter.DefaultHighlightPainter(Color.YELLOW));
                pos += string.length();
            }
        } 
        catch (BadLocationException e) 
        {
            JOptionPane.showMessageDialog(null, e, "BadLocationException", JOptionPane.ERROR_MESSAGE);            
        }        
    }//GEN-LAST:event_findMenuActionPerformed

    private void nextDiffButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_nextDiffButtonActionPerformed
        if(deltaLines == null || deltaLines.size() == 0)
            return;
        if(deltaIndex == deltaLines.size())
            deltaIndex = 0;
        Element paragraph = jTextArea3.getDocument().getDefaultRootElement();
        int line = ((Integer)deltaLines.get(deltaIndex++)).intValue();
        jTextArea3.requestFocus();            
        jTextArea3.setCaretPosition(paragraph.getElement(line).getStartOffset()); 
    }//GEN-LAST:event_nextDiffButtonActionPerformed

    private void compareButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_compareButtonActionPerformed
        removeHighlights(jTextArea3);
        removeHighlights(jTextArea3);        
        String text1 = jTextArea3.getText();
        String text2 = jTextArea4.getText();
        if(text1.equals("") && text2.equals(""))
        {
            JOptionPane.showMessageDialog(null, "Both files are empty.", 
                                          "File Comparison Information", 
                                          JOptionPane.INFORMATION_MESSAGE);            
            return;
        }
        if(text1.equals("") && !text2.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The file on the left is empty.", 
                                          "File Comparison Information", 
                                          JOptionPane.INFORMATION_MESSAGE);            
            return;
        }
        if(!text1.equals("") && text2.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The file on the right is empty.", 
                                          "File Comparison Information", 
                                          JOptionPane.INFORMATION_MESSAGE);            
            return;
        }        
        if(text1.equals(text2))
        {
            JOptionPane.showMessageDialog(null, "The files are identical.", 
                                          "File Comparison Information", 
                                          JOptionPane.INFORMATION_MESSAGE);  
            return;
        }
        if(!text1.endsWith("\n"))
            jTextArea3.append("\n"); 
        if(!text2.endsWith("\n"))
            jTextArea4.append("\n");

        String revision = server.diffFiles(jTextArea3.getText(), jTextArea4.getText());
        if(revision == null)
            return;
        if(revision.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The files are identical.", 
                                          "File Comparison Information", 
                                          JOptionPane.INFORMATION_MESSAGE);  
            return;
        }       
        
        String[] deltas = revision.split("\n");
            
        // Mark the tests
        int cL = -1; 
        int cR = -1;
        deltaLines = new Vector(); 
        for(int i = 0; i < deltas.length; i++)
        {
            if(!deltas[i].startsWith("<") && !deltas[i].startsWith(">") && !deltas[i].startsWith("---"))
            {
                int startL, startR, endL, endR;
                if(deltas[i].indexOf("a") != -1)
                {
                    String left = deltas[i].split("a")[0];
                    String right = deltas[i].split("a")[1];
                    if(right.indexOf(",") != -1)
                    {
                        startR = Integer.parseInt(right.split(",")[0]);
                        endR = Integer.parseInt(right.split(",")[1]); 
                    }                    
                    else
                    {
                        startR = Integer.parseInt(right);
                        endR = startR; 
                    }

                    // Get paragraph element
                    Element paragraphL = jTextArea3.getDocument().getDefaultRootElement(); 
                    
                    // Highlight lines on the right
                    highlight(jTextArea4, startR + cR, endR + cR, Color.GREEN); 
                    
                    // Insert empty lines on the left
                    int nLine = endR - startR + 1;
                    int position = Integer.parseInt(left) + cL + 1;                    
                    jTextArea3.insert(newLine(jTextArea3, nLine),   
                                      paragraphL.getElement(position).getStartOffset());  
                    
                    // Highlight the empty lines
                    highlight(jTextArea3, startR + cR, endR + cR, Color.MAGENTA);
                    
                    // Update added line counter                    
                    cL += nLine;   
                    
                    // Save delta position                    
                    deltaLines.add(new Integer(position)); 
                }
                if(deltas[i].indexOf("d") != -1)
                {
                    String left = deltas[i].split("d")[0];
                    String right = deltas[i].split("d")[1];
                    if(left.indexOf(",") != -1)
                    {
                        startL = Integer.parseInt(left.split(",")[0]);
                        endL = Integer.parseInt(left.split(",")[1]); 
                    }                    
                    else
                    {
                        startL = Integer.parseInt(left);
                        endL = startL; 
                    }
                    
                    // Get paragraph element
                    Element paragraphR = jTextArea4.getDocument().getDefaultRootElement();

                    // Highlight lines on the left
                    highlight(jTextArea3, startL + cL, endL + cL, Color.GREEN); 

                    // Insert empty lines on the right
                    int nLine = endL - startL + 1;
                    int position = Integer.parseInt(right) + cR + 1;
                    jTextArea4.insert(newLine(jTextArea4, nLine),   
                                      paragraphR.getElement(position).getStartOffset());
                    
                    // Highlight the empty lines
                    highlight(jTextArea4, startL + cL, endL + cL, Color.MAGENTA);
                    
                    // Update added line counter
                    cR += nLine;   
                    
                    // Save delta position
                    deltaLines.add(new Integer(position)); 
                }
                if(deltas[i].indexOf("c") != -1)
                {
                    String left = deltas[i].split("c")[0];
                    String right = deltas[i].split("c")[1];
                    if(left.indexOf(",") != -1)
                    {
                        startL = Integer.parseInt(left.split(",")[0]);
                        endL = Integer.parseInt(left.split(",")[1]);
                    }
                    else
                    {
                        startL = Integer.parseInt(left);
                        endL = startL;
                    }                        
                    if(right.indexOf(",") != -1)
                    {
                        startR = Integer.parseInt(right.split(",")[0]);
                        endR = Integer.parseInt(right.split(",")[1]);
                    }
                    else
                    {
                        startR = Integer.parseInt(right);
                        endR = startR; 
                    }
                    
                    // Get paragraph element
                    Element paragraphL = jTextArea3.getDocument().getDefaultRootElement();
                    Element paragraphR = jTextArea4.getDocument().getDefaultRootElement(); 
                                   
                    // Highlight lines
                    highlight(jTextArea3, startL + cL, endL + cL, Color.ORANGE);
                    highlight(jTextArea4, startR + cR, endR + cR, Color.ORANGE);
                    
                    // Save delta position
                    deltaLines.add(new Integer(startL + cL));
                    
                    // Handle endL - startL != endR - startR cases
                    if(endL - startL > endR - startR)
                    {
                        // Insert empty lines 
                        int nLine = endL - startL - endR + startR;
                        int position = endR + cR + 1;
                        jTextArea4.insert(newLine(jTextArea4, nLine),   
                                          paragraphR.getElement(position).getStartOffset());
                        
                        // Highlight the empty lines
                        highlight(jTextArea4, endR + cR, endL + cL, Color.MAGENTA);
                        
                        // Update added line counter
                        cR += nLine;
                    }
                    if(endR - startR > endL - startL)
                    {
                        // Insert empty lines 
                        int nLine = endR - startR - endL + startL;
                        int position = endL + cL + 1;
                        jTextArea3.insert(newLine(jTextArea3, nLine),   
                                          paragraphL.getElement(position).getStartOffset());
                        
                        // Highlight the empty lines
                        highlight(jTextArea3, endL + cL, endR + cR, Color.MAGENTA);
                        
                        // Update added line counter
                        cL += nLine;
                    }           
                }
            }
        }
        if(deltaLines.size() > 0)
        {
            deltaIndex = 0;
            Element paragraph = jTextArea3.getDocument().getDefaultRootElement();
            int line = ((Integer)deltaLines.get(deltaIndex++)).intValue();
            jTextArea3.requestFocus();                 
            jTextArea3.setCaretPosition(paragraph.getElement(line).getStartOffset()); 
        }
    }//GEN-LAST:event_compareButtonActionPerformed

    private String newLine(JTextArea textArea, int nLine)
    {
        int nSpace = textArea.getPreferredScrollableViewportSize().width/7;
        String newLine = "";
        for(int i = 0; i < nSpace; i++)
            newLine += " ";
        newLine += "\n";
        String newLines = "";
        for(int i = 0; i < nLine; i++)
            newLines += newLine;
        return newLines;    
    }
    
    private void highlight(JTextArea textArea, int startLine, int endLine, Color color)
    {
        try 
        {
            Highlighter hilite = textArea.getHighlighter();
            Element paragraph = textArea.getDocument().getDefaultRootElement();             
            hilite.addHighlight(paragraph.getElement(startLine).getStartOffset(),
                                paragraph.getElement(endLine).getEndOffset() - 1, 
                                new DefaultHighlighter.DefaultHighlightPainter(color));            
        }
        catch (BadLocationException e) 
        {
            JOptionPane.showMessageDialog(null, e, "BadLocationException", JOptionPane.ERROR_MESSAGE);            
        }
    } 
    
    private void removeHighlights(JTextComponent textComp) 
    {
        Highlighter hilite = textComp.getHighlighter();
        Highlighter.Highlight[] hilites = hilite.getHighlights();
    
        for (int i=0; i<hilites.length; i++) 
            hilite.removeHighlight(hilites[i]);
    }   
    

    private void localRButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_localRButtonActionPerformed
        isDiff = true;
        String[] text = openOperation();
        if(text != null)
        {
            jInternalFrame3.setTitle(text[0]);
            textR = text[1].replaceAll(ls, "\n");
            jTextArea4.setText(textR);
            jTextArea4.setCaretPosition(0);            
        }        
    }//GEN-LAST:event_localRButtonActionPerformed

    private void dataRButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataRButtonActionPerformed
        isDiff = true;
        isLeft = false;
        dataArchive();
    }//GEN-LAST:event_dataRButtonActionPerformed

    private void modelRButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_modelRButtonActionPerformed
        isDiff = true;
        isLeft = false;
        isLibrary = false;
        modelArchive();
    }//GEN-LAST:event_modelRButtonActionPerformed

    private void localLButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_localLButtonActionPerformed
        isDiff = true;
        String[] text = openOperation();
        if(text != null)
        {
            jInternalFrame2.setTitle(text[0]);
            textL = text[1].replaceAll(ls, "\n");
            jTextArea3.setText(textL);
            jTextArea3.setCaretPosition(0);            
        }
    }//GEN-LAST:event_localLButtonActionPerformed

    private void dataLButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataLButtonActionPerformed
        isDiff = true;
        isLeft = true;
        dataArchive();
    }//GEN-LAST:event_dataLButtonActionPerformed

    private void modelLButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_modelLButtonActionPerformed
        isDiff = true;
        isLeft = true;
        isLibrary = false;
        modelArchive();
    }//GEN-LAST:event_modelLButtonActionPerformed

    private void CompareFilesButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_CompareFilesButtonActionPerformed
        jScrollPane5.getVerticalScrollBar().addAdjustmentListener(new ScrollBarListener());
        jScrollPane6.getVerticalScrollBar().addAdjustmentListener(new ScrollBarListener());
        jTextArea3.addCaretListener(new ACaretListener()); 
        jTextArea4.addCaretListener(new ACaretListener());
        jTextArea3.setText("");
        jTextArea4.setText("");
        deltaLines = null;  
        diffDialog.setSize(970, 630);
        diffDialog.show();
    }//GEN-LAST:event_CompareFilesButtonActionPerformed

    private class ACaretListener implements javax.swing.event.CaretListener 
    {
        public void caretUpdate(javax.swing.event.CaretEvent e) 
        {
            if(e.getSource().equals(jTextArea3))
            {
                int caretPositionL = jTextArea3.getCaretPosition();
                int textLengthL = jTextArea3.getText().length(); 
                if(caretPositionL != textLengthL)
                    if(caretPositionL != 0)
                    {
                        Point p = jTextArea3.getCaret().getMagicCaretPosition();  
                        if(p != null)
                            jTextField8.setText(String.valueOf(p.y/13) + ":" + String.valueOf(p.x/7)); 
                    }
                    else
                        jTextField8.setText("0:0");
            }
            if(e.getSource().equals(jTextArea4))
            {
                int caretPositionR = jTextArea4.getCaretPosition();
                int textLengthR = jTextArea4.getText().length(); 
                if(caretPositionR != textLengthR)
                    if(caretPositionR != 0)
                    {
                        Point p = jTextArea4.getCaret().getMagicCaretPosition(); 
                        if(p != null)
                            jTextField9.setText(String.valueOf(p.y/13) + ":" + String.valueOf(p.x/7)); 
                    }
                    else
                        jTextField9.setText("0:0");
            }           
        }
    }
    
    private class ScrollBarListener implements AdjustmentListener
    {  
        /**
         * @param e
         */        
        public void adjustmentValueChanged(AdjustmentEvent e) 
        {
            int value = e.getValue();            
            if(e.getSource().equals(jScrollPane5.getVerticalScrollBar()));
                jScrollPane6.getVerticalScrollBar().setValue(value);
            if(e.getSource().equals(jScrollPane6.getVerticalScrollBar()));
                jScrollPane5.getVerticalScrollBar().setValue(value);
        }        
    }

    private void jRadioButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton3ActionPerformed
        jTextField1.setEnabled(false);
        jTextField2.setEnabled(false);
        jTextField1.setText("");
        jTextField2.setText("");
        jTextField3.setText("");       
        listType = "model";
        isLibrary = false;
        showVersions = true;
        indexList = 0;
        lists = new Vector();
        showArchiveList(false);        
    }//GEN-LAST:event_jRadioButton3ActionPerformed

    private void jRadioButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton2ActionPerformed
        jTextField1.setEnabled(false);
        jTextField2.setEnabled(false);
        jTextField1.setText("");
        jTextField2.setText("");
        jTextField3.setText("");
        listType = "model";
        isLibrary = false;
        showVersions = false;
        indexList = 0;
        lists = new Vector();        
        showArchiveList(false);
    }//GEN-LAST:event_jRadioButton2ActionPerformed

    private void jRadioButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton1ActionPerformed
        jTextField1.setEnabled(true);
        jTextField2.setEnabled(true); 
        jTextField1.setText("");
        jTextField2.setText("");
        jTextField3.setText("1");
    }//GEN-LAST:event_jRadioButton1ActionPerformed

    private void nextButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_nextButtonActionPerformed
        ++indexList;
        showArchiveList(false);
        if(indexList != 0)
            previousButton.setEnabled(true);
    }//GEN-LAST:event_nextButtonActionPerformed

    private void previousButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_previousButtonActionPerformed
        --indexList;
        showArchiveList(false); 
        if(indexList == 0)
            previousButton.setEnabled(false);
        nextButton.setEnabled(true);
    }//GEN-LAST:event_previousButtonActionPerformed

    private void jTable2MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTable2MouseClicked
        if(listType.equals("job"))
            return;
        int index = jTable2.getSelectedRow();
        String version = (String)jTable2.getModel().getValueAt(index,  0);
        String title = archiveName + "." + version;
        String archive = server.getArchive(version);
        if(archive == null)
            return;        
        if(getArchive)
        {
            if(!isDiff)
                saveFile();            
            if(listType.equals("model"))
            {
                if(archive != null)
                {
                    if(isDiff)
                    {
                        if(isLeft)
                        {
                            jInternalFrame2.setTitle(title);
                            textL = archive;
                            jTextArea3.setText(textL);
                            jTextArea3.setCaretPosition(0);
                        }
                        else
                        {
                            jInternalFrame3.setTitle(title);
                            textR = archive;
                            jTextArea4.setText(textR);
                            jTextArea4.setCaretPosition(0);
                        }
                    }
                    else
                    {
                        jInternalFrame1.setTitle(title);
                        textArea.setText(archive);
                        textArea.setCaretPosition(0);                        
                        file = null;
                    }
                }
            }
            if(listType.equals("data"))
            { 
                if(archive != null)
                {
                    if(isDiff)
                    {
                        if(isLeft)
                        {
                            jInternalFrame2.setTitle(title);
                            textL = XMLReader.parseDataXML(archive).replaceAll(ls, "\n");
                            if(textL != null)
                            {
                                jTextArea3.setText(textL);
                                jTextArea3.setCaretPosition(0);
                            }
                        }
                        else
                        {
                            jInternalFrame3.setTitle(title);
                            textR = XMLReader.parseDataXML(archive).replaceAll(ls, "\n");
                            if(textR != null)
                            {
                                jTextArea4.setText(textR);
                                jTextArea4.setCaretPosition(0);
                            }
                        }                            
                    }
                    else
                    {
                        jInternalFrame1.setTitle(title);
                        String text = XMLReader.parseDataXML(archive);
                        if(text != null)
                        {
                            textArea.setText(text);
                            textArea.setCaretPosition(0);                        
                            file = null;
                        }
                    }
                }
            }
        }
        else
        {
            if(listType.equals("model"))
            {
                String spkInput = textArea.getText();
                int index2 = spkInput.indexOf("<spkmodel");        
                String model = XMLReader.getModelArchive(spkInput.substring(index2 - 22)).trim();
                if(model.equals(archive))
                    jTextField3.setText(version);
                else
                {
                    JOptionPane.showMessageDialog(null, "The version of the model you selected" + 
                                                  " is different from the model in the input file.", 
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
            if(listType.equals("data"))
            {
                String spkInput = textArea.getText();
                int index1 = spkInput.indexOf("<spkdata");
                int index2 = spkInput.indexOf("<spkmodel");        
                String dataset = spkInput.substring(index1 - 22, index2 - 22).trim();
                if(dataset.equals(archive))
                {
                    jTextField6.setText(version);
                }
                else
                {

                    JOptionPane.showMessageDialog(null, "The version of the dataset you selected" + 
                                                  " is different from the dataset in the input file.", 
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
                    return;
                }                
            }
        }
        versionDialog.dispose();
        reportDialog.dispose();
    }//GEN-LAST:event_jTable2MouseClicked

    private void DataArchiveButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_DataArchiveButtonActionPerformed
        isDiff = false;
        isLibrary = false; 
        dataArchive();
    }//GEN-LAST:event_DataArchiveButtonActionPerformed

    private void dataArchive()
    {
        listType = "data"; 
        showVersions = true;
        getArchive = true;
        indexList = 0;
        lists = new Vector(); 
        showArchiveList(false);        
    }
    
    private void ModelArchiveButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ModelArchiveButtonActionPerformed
        isDiff = false;
        isLibrary = false;
        modelArchive();
    }//GEN-LAST:event_ModelArchiveButtonActionPerformed

    private void modelArchive()
    {
        listType = "model";
        showVersions = true;
        getArchive = true;
        indexList = 0; 
        lists = new Vector();
        showArchiveList(false);
    }
    
    private void jRadioButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton4ActionPerformed
        jTextField4.setEnabled(true);
        jTextField5.setEnabled(true);
        jTextField4.setText("");
        jTextField5.setText(""); 
        jTextField6.setText("1");
    }//GEN-LAST:event_jRadioButton4ActionPerformed

    private void jRadioButton6ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton6ActionPerformed
        jTextField4.setEnabled(false);
        jTextField5.setEnabled(false);
        jTextField4.setText("");
        jTextField5.setText("");
        jTextField6.setText("");        
        listType = "data";
        isLibrary = false;
        showVersions = true;
        indexList = 0;
        lists = new Vector();
        showArchiveList(false);        
    }//GEN-LAST:event_jRadioButton6ActionPerformed

    private void jRadioButton5ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton5ActionPerformed
        jTextField4.setEnabled(false);
        jTextField5.setEnabled(false);
        jTextField4.setText("");
        jTextField5.setText("");
        jTextField6.setText("");        
        listType = "data";
        isLibrary = false;
        showVersions = false;
        indexList = 0;
        lists = new Vector();        
        showArchiveList(false);
    }//GEN-LAST:event_jRadioButton5ActionPerformed

    private void jTable1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTable1MouseClicked
        int index = jTable1.getSelectedRow();
        if(index == -1)
            return; 
        long id = Long.parseLong(((String[][])lists.get(indexList))[index][0]);
        if(listType.equals("job"))
        {
            new JobInfo(this, id, isLibrary, false);
            timer.stop();
            reportDialog.dispose();        
            return;
        }
        if(getArchive)
        {
            archiveName = ((String[][])lists.get(indexList))[index][1];
            versionDialog.setTitle(archiveName); 
            showVersionList(id);
        }
        else 
        {
            if(listType.equals("model"))
            {
                jTextField1.setText((String)(jTable1.getModel().getValueAt(index, 0)));
                jTextField2.setText((String)(jTable1.getModel().getValueAt(index, 2)));
                modelArchive.id = id; 
                if(showVersions)
                    showVersionList(id);
                else
                {
                    String[][] modelVersions = server.getVersions(id, listType, isLibrary); 
                    if(modelVersions != null)
                    {
                        jTextField3.setText(String.valueOf(modelVersions.length + 1)); 
                        reportDialog.dispose();
                    }
                }
            }
            if(listType.equals("data"))
            {
                jTextField4.setText((String)(jTable1.getModel().getValueAt(index, 0))); 
                jTextField5.setText((String)(jTable1.getModel().getValueAt(index, 2)));
                dataArchive.id = id;                                  
                if(showVersions)
                    showVersionList(id);
                else
                {
                    String[][] datasetVersions = server.getVersions(id, listType, isLibrary);
                    if(datasetVersions != null)
                    {
                        jTextField6.setText(String.valueOf(datasetVersions.length + 1));                     
                        reportDialog.dispose();  
                    }
                }
            }
        }
    }//GEN-LAST:event_jTable1MouseClicked

    private void summaryMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_summaryMenuActionPerformed
        saveFile();
        textArea.setText(Summary.makeSummary(output, isOnline, isDeveloper, jobMethodCode, methodTable));
        textArea.setCaretPosition(0);
        jInternalFrame1.setTitle("");
        file = null;
    }//GEN-LAST:event_summaryMenuActionPerformed

    
    private void ReadOutputButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ReadOutputButtonActionPerformed
        readOutput(textArea.getText());
    }//GEN-LAST:event_ReadOutputButtonActionPerformed

    private void GetReportButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_GetReportButtonActionPerformed
        listType = "job";
        isLibrary = false;
        indexList = 0;
        lists = new Vector();
        showArchiveList(false);
    }//GEN-LAST:event_GetReportButtonActionPerformed

    private void SubmitJobButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_SubmitJobButtonActionPerformed
        String text = textArea.getText();
        if(text.indexOf("<spksource>") == -1 || text.indexOf("<spkdata") == -1 || 
           text.indexOf("<spkmodel>") == -1)
        {
            JOptionPane.showMessageDialog(null, "The text is not a SPK input file", 
                                          "Job Submission Error", 
                                          JOptionPane.ERROR_MESSAGE);            
            return;
        }

        // If the user is going to use a likelihhod evaluation method modify the input file
        jobMethodCode = "fo";
        if(jobId != 0 && text.indexOf("<pop_analysis ") != -1 && text.indexOf(" is_estimation=\"yes\" ") != -1 && 
           JOptionPane.showConfirmDialog(null, "Are you going to use a likelihood evaluation only method?",
                                         "Question", JOptionPane.YES_NO_OPTION) == 0)
        {
            // Get report
            String report = server.getOutput(jobId, isLibrary).getProperty("report");
            text = Likelihood.changeInput(text, report, jobId, isLibrary);
            textArea.setText(text);
            textArea.setCaretPosition(0);
            jobMethodCode = "ml";
        }
                
        // Find $PROBLEM
        int beginIndex = text.indexOf("\n$PROBLEM") + 9; 
        int endIndex = text.indexOf("\n", beginIndex);
        String problem = text.substring(beginIndex, endIndex).trim();
        
        // Find $DATA
        beginIndex = text.indexOf("\n$DATA") + 6; 
        endIndex = text.indexOf("\n", beginIndex);
        String data = text.substring(beginIndex, endIndex).trim();
        
        // Find method
        if(!jobMethodCode.equals("ml"))
        {
            if(text.indexOf("<ind_analysis") != -1)
            {
                beginIndex = text.indexOf("<ind_analysis ");
                if(text.indexOf("is_estimation=\"yes\"", beginIndex) != -1)
                    jobMethodCode = "ia";
                else if(text.indexOf("<simulation ") != -1)
                    jobMethodCode = "so";
                else
                {
                    JOptionPane.showMessageDialog(null, "Neither estimation nor simulation is included in the job",
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
                    return;
                }                
            }
            else if(text.indexOf("<pop_analysis ") != -1)
            {
                beginIndex = text.indexOf("<pop_analysis ");
                String analysis = text.substring(beginIndex, text.indexOf(">", beginIndex));
                if(analysis.indexOf("is_estimation=\"yes\"") != -1)
                {
                    beginIndex = analysis.indexOf(" approximation=") + 16;
                    endIndex = analysis.indexOf("\"", beginIndex);
                    String approximation = analysis.substring(beginIndex, endIndex);
                    method = "";
                    if(approximation.equals("fo")) method = "fo";
                    else if(approximation.equals("foce")) method = "eh";
                    else if(approximation.equals("laplace")) method = "la";
                    if(!method.equals("")) jobMethodCode = method;
                }
                else if(analysis.indexOf("is_estimation=\"no\"") != -1 && text.indexOf("<simulation ") != -1)
                    jobMethodCode = "so";
                else
                {
                    JOptionPane.showMessageDialog(null, "Neither estimation nor simulation is included in the job",
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
            else
            {
                JOptionPane.showMessageDialog(null, "Neither population nor individual analysis is in the job",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return;          
            }            
        }
        
        // Collect archive information
        getArchive = false;
        modelArchive = new ArchiveInfo();
        dataArchive = new ArchiveInfo();
        jRadioButton1.setSelected(true);
        jRadioButton2.setSelected(false);
        jRadioButton3.setSelected(false);
        jRadioButton4.setSelected(true);
        jRadioButton5.setSelected(false);
        jRadioButton6.setSelected(false);
        jRadioButton7.setSelected(false);
        jRadioButton8.setSelected(false);
        jRadioButton9.setSelected(false);
        jRadioButton10.setSelected(false);
        jRadioButton11.setSelected(false);
        jRadioButton12.setSelected(false);
        jRadioButton7.setEnabled(((String[])methodTable.get("fo"))[2].equals("0") ||
                                 ((String[])methodTable.get("eh"))[2].equals("0") ||
                                 ((String[])methodTable.get("la"))[2].equals("0") || isDeveloper);
        jRadioButton8.setEnabled(((String[])methodTable.get("ml"))[2].equals("0") || isDeveloper);
        jRadioButton10.setEnabled(((String[])methodTable.get("gr"))[2].equals("0") || isDeveloper);
        jRadioButton11.setEnabled(((String[])methodTable.get("an"))[2].equals("0") || isDeveloper);
        jRadioButton12.setEnabled(((String[])methodTable.get("mi"))[2].equals("0") || isDeveloper);        
        if(jobMethodCode.equals("ia") || jobMethodCode.equals("so"))
        {
            jRadioButton7.setEnabled(false);
            jRadioButton8.setEnabled(false);
            jRadioButton9.setEnabled(false);
            jRadioButton10.setEnabled(false);
            jRadioButton11.setEnabled(false);
            jRadioButton12.setEnabled(false);
        }
        else if(jobMethodCode.equals("fo") || jobMethodCode.equals("eh") || jobMethodCode.equals("la"))
        {
            jRadioButton8.setEnabled(false);
            jRadioButton10.setEnabled(false);
            jRadioButton11.setEnabled(false);
            jRadioButton12.setEnabled(false);
            jRadioButton7.setSelected(true);
            jRadioButton7.doClick();
        }
        else if(jobMethodCode.equals("ml"))
        {
            jRadioButton7.setEnabled(false);
            jRadioButton9.setEnabled(false);
            jRadioButton8.setSelected(true);
        }
        jTextField14.setText(((String[])methodTable.get(jobMethodCode))[0]);

        if(jobId == 0)
        {
            jRadioButton8.setEnabled(false);
            jRadioButton10.setEnabled(false);
            jRadioButton11.setEnabled(false);
            jRadioButton12.setEnabled(false);            
        }
        jTextField1.setText("");
        jTextField2.setText("");
        jTextField3.setText("1");
        jTextField4.setText(data);
        jTextField5.setText("");
        jTextField6.setText("1");
        jTextField7.setText(problem); 
        jTextField1.setEnabled(true);
        jTextField2.setEnabled(true);
        jTextField4.setEnabled(true);
        jTextField5.setEnabled(true);
        archiveDialog.setSize(300, 320);
        archiveDialog.setVisible(true);
    }//GEN-LAST:event_SubmitJobButtonActionPerformed

    private void WriteInputButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_WriteInputButtonActionPerformed
        MDAIterator iterator = new MDAIterator(serverName, serverPort, isOnline, this, isTester, isDeveloper);
        writeInput(iterator);
    }//GEN-LAST:event_WriteInputButtonActionPerformed

    private void scatterPlot(String type)
    {
        if(output != null && output.scatterplot != null)
        {
            if(output.dataAll != null && output.dataItems != null && output.dataLabelMap != null)
            {
                new PlotShow(output.scatterplot, output.dataAll, output.dataItems,  
                             output.dataLabelMap, type);
            }
            else
            {
                JOptionPane.showMessageDialog(null, "The data is not available", 
                                              "Data not Found Error",               
                                              JOptionPane.ERROR_MESSAGE);
            }
        }
        else
            JOptionPane.showMessageDialog(null, "No scatterplot is available", 
                                          "Data not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);        
    }
    
    private void tableMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_tableMenuActionPerformed
        if(tableShow != null)
            tableShow.showTableList();
        else
            JOptionPane.showMessageDialog(null, "No table is available", 
                                          "Data not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_tableMenuActionPerformed

    private void invCovarianceMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_invCovarianceMenuActionPerformed
        if(output != null && output.invCovariance != null)
        {
            int length = output.invCovariance.length;
            String[] header = new String[length + 1];
            for(int i = 0; i < length; i++)
                header[i + 1] = output.invCovariance[i][0];           
            new MatrixShow(output.invCovariance, header, "Inv. Covariance of Estimate", 
                           "Inv.Covariance Matrix of Estimate",
                           width(length), height(length), 0, 0, false);    
        }
        else
            JOptionPane.showMessageDialog(null, "The INV. COVARIANCE is not available", 
                                          "Data Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_invCovarianceMenuActionPerformed

    private void correlationMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_correlationMenuActionPerformed
        if(output != null && output.correlation != null)
        {
            int length = output.correlation.length;
            String[] header = new String[length + 1];
            for(int i = 0; i < length; i++)
                header[i + 1] = output.correlation[i][0];            
            new MatrixShow(output.correlation, header, "Correlation of Estimate", 
                           "Correlation Matrix of Estimate",
                           width(length), height(length), 0, 0, false);    
        }
        else
            JOptionPane.showMessageDialog(null, "The CORRELATION is not available", 
                                          "Data Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_correlationMenuActionPerformed

    private void covarianceMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_covarianceMenuActionPerformed
        if(output != null && output.covariance != null)
        {
            int length = output.covariance.length;
            String[] header = new String[length + 1];
            for(int i = 0; i < length; i++)
                header[i + 1] = output.covariance[i][0];            
            new MatrixShow(output.covariance, header, "Covariance of Estimate", 
                           "Covariance Matrix of Estimate",
                           width(length), height(length), 0, 0, false);    
        }
        else
            JOptionPane.showMessageDialog(null, "The COVARIANCE is not available", 
                                          "Data Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_covarianceMenuActionPerformed

    private void stdErrSigmaMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_stdErrSigmaMenuActionPerformed
        if(output != null && output.stdErrSigma != null)
        {
            for(int l = 0; l < output.stdErrSigma.length; l++)
            {
                int length = output.stdErrSigma[l].length;
                String[] header = new String[length + 1];
                for(int i = 0; i < length; i++)
                    header[i + 1] = output.stdErrSigma[l][i][0];     
                new MatrixShow(output.stdErrSigma[l], header, "Standard Error of SIGMA Estimate", 
                               "SIGMA - Cov Matrix for Residuals - ETAs  Block " + (l+1),
                               width(length), height(length), l*40, l*30, output.sigmaStruct.equals("diagonal"));
            }
        }
        else
            JOptionPane.showMessageDialog(null, "The standard error of SIGMA is not available", 
                                          "Data Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_stdErrSigmaMenuActionPerformed

    private void stdErrOmegaMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_stdErrOmegaMenuActionPerformed
        if(output != null && output.stdErrOmega != null)
        {
            for(int l = 0; l < output.stdErrOmega.length; l++)
            {
                int length = output.stdErrOmega[l].length;
                String[] header = new String[length + 1];
                for(int i = 0; i < length; i++)
                    header[i + 1] = output.stdErrOmega[l][i][0]; 
                new MatrixShow(output.stdErrOmega[l], header, "Standard Error of OMEGA Estimate", 
                               "OMEGA - Cov Matrix for Random Effects - ETAs  Block " + (l+1),
                               width(length), height(length), l*40, l*30, output.omegaStruct.equals("diagonal"));
            }
        }
        else
            JOptionPane.showMessageDialog(null, "The standard error of OMEGA is not available", 
                                          "Data Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_stdErrOmegaMenuActionPerformed

    private void stdErrThetaMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_stdErrThetaMenuActionPerformed
        if(output != null && output.stdErrTheta != null)
        {
            int length = output.stdErrTheta.length;
            String[] header = new String[length];
            for(int i = 0; i < length; i++)
                header[i] = "TH " + (i + 1);        
            new VectorShow(output.stdErrTheta, header, "Standard Error of Estimate", 
                           "THETA - Vector of Fixed Effects Parameters",
                           width(length - 1)); 
        }
        else
            JOptionPane.showMessageDialog(null, "The standard error of THETA is not available", 
                                          "Data Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_stdErrThetaMenuActionPerformed

    private void SigmaMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_SigmaMenuActionPerformed
        if(output != null && output.sigma != null)
        {
            for(int l = 0; l < output.sigma.length; l++)
            {
                int length = output.sigma[l].length;
                String[] header = new String[length + 1];
                for(int i = 0; i < length; i++)
                    header[i + 1] = output.sigma[l][i][0];            
                new MatrixShow(output.sigma[l], header, "SIGMA Parameter Estimate",
                               "SIGMA - Cov Matrix for Residuals - EPSILONs  Block " + (l+1),
                               width(length), height(length), l*40, l*30, false);
            }
        }
        else
            JOptionPane.showMessageDialog(null, "The SIGMA is not available", 
                                          "Data Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_SigmaMenuActionPerformed

    private void OmegaMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_OmegaMenuActionPerformed
        if(output != null && output.omega != null)
        {
            for(int l =0; l < output.omega.length; l++)
            {
                int length = output.omega[l].length;
                String[] header = new String[length + 1];
                for(int i = 0; i < length; i++)
                    header[i + 1] = output.omega[l][i][0];
                new MatrixShow(output.omega[l], header, "OMEGA Parameter Estimate", 
                               "OMEGA - Cov Matrix for Random Effects - ETAs  Block" + (l+1),
                               width(length), height(length), l*40, l*30, false);
            }
        }
        else
            JOptionPane.showMessageDialog(null, "The OMEGA is not available", 
                                          "Data Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_OmegaMenuActionPerformed

    private void ThetaMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ThetaMenuActionPerformed
        if(output != null && output.theta != null)
        {
            int length = output.theta.length;
            String[] header = new String[length];
            for(int i = 0; i < length; i++)
                header[i] = "TH " + (i + 1);        
            new VectorShow(output.theta, header, "THETA Parameter Estimate", 
                           "THETA - Vector of Fixed Effects Parameters", 
                           width(length - 1)); 
        }
        else
            JOptionPane.showMessageDialog(null, "The THETA is not available",
                                          "Data Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_ThetaMenuActionPerformed

    private void objectiveMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_objectiveMenuActionPerformed
        if(output != null && output.objective != null)
        {
            String objective = "Minimum Value of the Objective Function:\n" + output.objective + "\n";
            String objStdErr = "";
            String jobMethod = "";
            if(output.objStdErr != null)
                objStdErr = "\nStandard Error of the Objective Function Value:\n" + output.objStdErr + "\n";
            if(output.methodCode != null)
                jobMethod = "\nMethod Used in the Analysis:\n" + 
                            ((String[])methodTable.get(output.methodCode))[0] + "\n";
            jTextArea2.setText(objective + objStdErr + jobMethod);
            objectiveDialog.setSize(300, 200);
            objectiveDialog.show();
        }
        else
            JOptionPane.showMessageDialog(null, "The objective is not available", 
                                          "Data Not Found Error",               
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_objectiveMenuActionPerformed

    private void errorMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_errorMenuActionPerformed
        if(output != null && output.error != null)
        {
            String error = "";
            for(int i = 0; i < output.error.length; i++)
            {
                error += "<Error Message " + (i + 1) + ">\n" + output.error[i][0] + "\n";
                if(isDeveloper)
                    error += "This message was issued from file: " + output.error[i][1] + 
                               " at line: " + output.error[i][2] + "\n";
                error += "\n";
            }
            jTextArea1.setText(error);
            jTextArea1.setCaretPosition(0);            
            errorMessageDialog.setSize(400, 300);
            errorMessageDialog.show();
        }
        else
            JOptionPane.showMessageDialog(null, "The error message is not available",
                                          "Message Not Found Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_errorMenuActionPerformed
    
    private void exitMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exitMenuActionPerformed
        server.endSession();
    }//GEN-LAST:event_exitMenuActionPerformed

    private void printMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_printMenuActionPerformed
        String text = textArea.getText();
        if(!text.endsWith("\n"))
            text += "\n";
        Printer printable = new Printer(text);
			
	// Get a PrinterJob object
        PrinterJob printerJob = PrinterJob.getPrinterJob();

        // Display print dialog,if user return OK, setPrintable and print
        PrintRequestAttributeSet attributes = new HashPrintRequestAttributeSet(); 
//        PageFormat pageFormat = printerJob.pageDialog(attributes);
//        if(pageFormat != null)
        if(printerJob.printDialog(attributes))
        {
            printerJob.setPrintable(printable);
//            Book book = new Book();
//            book.append(printable, pageFormat, printable.getPageCount());   
//            printerJob.setPageable(book);
            try
	    {
                printerJob.print(attributes);
            }
            catch(PrinterException pe)
	    {
                JOptionPane.showMessageDialog(null, "Error printing " + pe,  // Display printing 
                                              "Printer Error",               // error message
                                              JOptionPane.ERROR_MESSAGE);
            }
        }
    }//GEN-LAST:event_printMenuActionPerformed

    private void savaAsMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_savaAsMenuActionPerformed
        files.setDialogTitle("Save File");
        int result = files.showSaveDialog(null);
        if(result == files.APPROVE_OPTION)
	{
            file = files.getSelectedFile();
            saveOperation(textArea.getText());
            jInternalFrame1.setTitle(file.getName());
        }
    }//GEN-LAST:event_savaAsMenuActionPerformed

    private void saveMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveMenuActionPerformed
        if(file != null)
	{
            saveOperation(textArea.getText());
        }
        else
	{
            files.setDialogTitle("Save File");
            int result = files.showSaveDialog(null);
            if(result == files.APPROVE_OPTION)
	    {
                file = files.getSelectedFile();
                saveOperation(textArea.getText());
            }
        }
    }//GEN-LAST:event_saveMenuActionPerformed

    private void closeMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeMenuActionPerformed
        textArea.setText("");
        jInternalFrame1.setTitle("");
        file = null;
    }//GEN-LAST:event_closeMenuActionPerformed

    private void openMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_openMenuActionPerformed
        // Ask the user whether to save the text to a file
        saveFile();
        isDiff = false;
        String[] titleAndText = openOperation();
        if(titleAndText != null && titleAndText[1] != null)
        {
            textArea.setText(titleAndText[1]);
            textArea.setCaretPosition(0);
            jInternalFrame1.setTitle(file.getName());
        }
    }//GEN-LAST:event_openMenuActionPerformed

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
        String jobAbstract = jTextField7.getText();
        
        // Collect model archive information 
        modelArchive.name = jTextField1.getText();
        if(modelArchive.name.length() > 20)
            modelArchive.name = modelArchive.name.substring(0, 20);
        modelArchive.description = jTextField2.getText(); 
        if(modelArchive.description.length() > 100)
            modelArchive.description = modelArchive.description.substring(0, 100);        
        modelArchive.version = "1." + jTextField3.getText();
        modelArchive.isNewArchive = jRadioButton1.isSelected(); 
        modelArchive.isNewVersion = jRadioButton2.isSelected();
        if(!modelArchive.isNewVersion || modelArchive.isNewArchive)
            modelArchive.log = "";
        
        // Check input errors
        if(modelArchive.name.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The model name is requried",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);                 
            return; 
        }            
        if(modelArchive.version.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The model version is requried",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);                 
            return; 
        }        

        // Collect data archive information 
        dataArchive.name = jTextField4.getText();
        if(dataArchive.name.length() > 20)
            dataArchive.name = dataArchive.name.substring(0, 20);        
        dataArchive.description = jTextField5.getText(); 
        if(dataArchive.description.length() > 100)
            dataArchive.description = dataArchive.description.substring(0, 100);        
        dataArchive.version = "1." + jTextField6.getText();
        dataArchive.isNewArchive = jRadioButton4.isSelected(); 
        dataArchive.isNewVersion = jRadioButton5.isSelected();
        if(!dataArchive.isNewVersion || dataArchive.isNewArchive) 
            dataArchive.log = "";
        
        // Check input errors  
        if(dataArchive.name.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The dataset name is requried",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);
            return; 
        }            
        if(dataArchive.version.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The dataset version is requried",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);                 
            return; 
        }

        // Get the XML documents as String objects
        String spkInput = textArea.getText();
        int index1 = spkInput.indexOf("<spkdata");
        int index2 = spkInput.indexOf("<spkmodel");
        String source = spkInput.substring(0, index1 - 22);
        String dataset = spkInput.substring(index1 - 22, index2 - 22);
        String model = XMLReader.getModelArchive(spkInput.substring(index2 - 22));
        
        // Collect version logs
        if((modelArchive.isNewArchive || modelArchive.isNewVersion) && (jRadioButton7.isSelected() || jRadioButton9.isSelected()))
        {
            modelArchive.log = JOptionPane.showInputDialog("Enter log for the new version of the model (<=100 characters).  ");
            if(modelArchive.log == null)
                modelArchive.log = "";
        }
        if((dataArchive.isNewArchive || dataArchive.isNewVersion) && (jRadioButton7.isSelected() || jRadioButton9.isSelected()))
        {
            dataArchive.log = JOptionPane.showInputDialog("Enter log for the new version of the dataset (<=100 characters).");
            if(dataArchive.log == null)
                dataArchive.log = "";
        }
        
        // Remove number of objective function evaluations
        int indexMC = source.indexOf("<monte_carlo ");
        if(indexMC != -1)
            source = source.substring(0, indexMC - 3) + source.substring(source.indexOf("</nonmem>"));
        
        // Get job method class
        String jobMethodClass = ((String[])methodTable.get(jobMethodCode))[1];
        
        // Add number of objective function evaluations
        if(jobMethodClass.equals("le"))
        {
            if(Likelihood.insertLeElement(source, jobMethodCode) == null)
                return;
        }
        
        // Get job parent
        long jobParent = 0;
        if(jobId != 0)
        {
            jobParent = jobId;
            jobId = 0;
        }

        // Submit the job
        server.submitJob(source, dataset, model, jobAbstract, modelArchive, dataArchive,
                         jobMethodCode, jobParent);    
        
        // Close the dialog
        archiveDialog.dispose();
    }//GEN-LAST:event_okButtonActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        archiveDialog.dispose();
    }//GEN-LAST:event_cancelButtonActionPerformed
    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
        server.endSession();       
    }//GEN-LAST:event_exitForm

    // This method performs open file operation.
    private String[] openOperation()
    {
        // Prepare for the return
        String[] text = null; 
        files.setDialogTitle("Open File");
        int result = files.showOpenDialog(null);
        if(result == files.APPROVE_OPTION) 
	{
            if(!isDiff)
                file = files.getSelectedFile();
            try
	    {
                BufferedReader in = new BufferedReader(new FileReader(files.getSelectedFile()));
                StringBuffer buffer = new StringBuffer();
                boolean done = false;
                while(!done)
                {
                    // Read a line
                    String line = in.readLine();                            
                    if(line == null) 
                        done = true;
                    else
                        buffer.append(line).append("\n");
	        }
                in.close();
                text = new String[2];
                text[0] = files.getSelectedFile().getPath();
                text[1] = buffer.toString();
            }
            catch(IOException ioe )
	    {
                System.err.println(ioe);
                JOptionPane.showMessageDialog(null, "Error opening file",  // Display opening file 
                                              "File Error",                // error message
                                              JOptionPane.ERROR_MESSAGE);
            }
        }
        return text;
    }
    
    // This method performs save file operation.
    private void saveOperation(String text)
    {
        try
        {
            BufferedWriter out = new BufferedWriter(new FileWriter(file));
            out.write(text.replaceAll("\n", ls));
            out.close();
        }
        catch(IOException e )
        {
            JOptionPane.showMessageDialog(null, e, "IOException", JOptionPane.ERROR_MESSAGE);
        }
    }

    /** Write input.
     * @param iterator a MDAIterator object for the wizard.
     */    
    protected void writeInput(MDAIterator iterator)
    {
        wp = new JWizardPane(iterator, object); 
        wp.getContentPanel().setBackground(new Color(240, 245, 255));   
//        wp.setContentImage((new javax.swing.ImageIcon(getClass().getResource("/uw/rfpk/mda/nonmem/wizard/nonmem-spk.gif"))).getImage()); 
        wp.createDialog(this, "Model Design agent Input File Generation Tool").show();
        WriteInputButton.setEnabled(false);
    }
    
    /** Enable Prepare Input button.
     *
     */
    public void enablePrepareInput()
    {
        WriteInputButton.setEnabled(true);   
    }
    
    /** Process the input information to write a NONMEM control file and a SPK input file.
     *  
     */
    public void processInput()
    {
        if(wp.getCustomizedObject() == null)
            return;
        object = (MDAObject)wp.getCustomizedObject();
        Properties records = object.getRecords();
        String[] names = {"Problem", "Data", "Input", "Pred", "Subroutines", "Aes", 
                          "Aesinitial", "Model", "PK", "Theta", "Omega", "Des", 
                          "Error", "Sigma", "Simulation", "TableSim", "ScatterPlotSim",
                          "Estimation", "Covariance", "TableEst", "ScatterPlotEst"};        
        control = "";
        for(int i = 0; i < 21; i++)
        {
            if(!records.getProperty(names[i]).equals("")) 
                control = control + records.getProperty(names[i]) + "\n";
        }
        if(JOptionPane.showConfirmDialog(null, 
                                         "Do you want to save the NONMEM control file?",   
                                         "Question Dialog",
                                         JOptionPane.YES_NO_OPTION,
                                         JOptionPane.QUESTION_MESSAGE) == 0)
        {
            files.setSelectedFile(new File("control.txt"));            
            int result = files.showSaveDialog(null);
            if(result == files.APPROVE_OPTION)
	    {
                file = files.getSelectedFile();
                saveOperation(control);
            }   
        }
        file = null;
        files.setSelectedFile(new File(""));
        
        // Set restart option
        if(!records.getProperty("Estimation").equals("") &&
           JOptionPane.showConfirmDialog(null, "Do you want SPK to continue estimation when the maximum\n " +
                                         "allowable number of evaluations you specified is reached?",
                                         "Question Dialog",
                                         JOptionPane.YES_NO_OPTION,
                                         JOptionPane.QUESTION_MESSAGE) == 0)
            object.getSource().isRestart = true;
        else
            object.getSource().isRestart = false;
        
        // Write SPK input file in XML format
        if(object.getSource() == null || object.getData() == null || control == null)
        {
            JOptionPane.showMessageDialog(null, "Input information is not complete.", 
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
        }
        else
        {
            XMLWriter writer = new XMLWriter(control, object);
            textArea.setText(writer.getDocument()); 
            textArea.setCaretPosition(0);        
            jInternalFrame1.setTitle("SPK input");
        }
        WriteInputButton.setEnabled(true);
    }
   
    /** Read report
     * @param text a String containing the SPK output.
     */    
    protected void readOutput(String text)
    { 
        if(text.indexOf("<spkreport>") == -1 || text.indexOf("<spksource>") == -1)
        {
            JOptionPane.showMessageDialog(null, "SPK output file is not loaded",  
                                          "File Error",            
                                          JOptionPane.ERROR_MESSAGE);            
            return;   
        }
        output = new Output();
        XMLReader reader = new XMLReader(text, output);
        if(!output.ok)
            return;
        
        // Promote user to save tables into files
        if(output.table != null)
        {        
            if(output.dataAll != null && output.dataItems != null && output.dataLabelMap != null)
            {
                tableShow = new TableShow(output.table, output.dataAll, output.dataItems,  
                                          output.dataLabelMap);
                for(int i = 0; i < output.table.length; i++)
                {
                    String[][] tableI = output.table[i];
                    if(tableI[0][0] != null && 
                       JOptionPane.showConfirmDialog(null, 
                                                     "Do you want to save the table file: " + tableI[0][0],   
                                                     "Question Dialog",
                                                     JOptionPane.YES_NO_OPTION,
                                                     JOptionPane.QUESTION_MESSAGE) == 0)
                    {
                        // Fill the table
                        String[][] data = new String[output.dataAll.length][tableI[1].length + 1]; 
                        String[] header = new String[tableI[1].length + 1];
                        String path = System.getProperty("user.home") + System.getProperty("file.separator");
                        for(int j = 0; j < tableI[1].length; j++)
                        {
                             // For item "DV" replace it by the alias
                             if(tableI[1][j].equals("DV"))
                                 tableI[1][j] = output.dataLabelMap.getProperty("DV");
                        }                        
                        
                        tableShow.fillTable(tableI, data, header);
                        files.setDialogTitle("Save table File");
                        files.setSelectedFile(new File(path + tableI[0][0])); 
                        int result = files.showSaveDialog(null);
                        if(result == files.APPROVE_OPTION)
	                {
                            file = files.getSelectedFile();
                            try
                            {
                                BufferedWriter out = new BufferedWriter(new FileWriter(file));
                                int nColumns = data[0].length;
                                int nRows = data.length;
                                
                                // Format and write header and data
                                DecimalFormat f = new DecimalFormat("0.0000E00");
                                int start = 0;
                                int k = 0;
                                while(k < nRows)
                                {    
                                    // Write headers
                                    if(tableI[0][1].equals("every") ||
                                       (tableI[0][1].equals("one") && k == 0))
                                    {
                                        out.write("TABLE NO.  " + (i + 1) + ls);
                                        for(int j = 1; j < nColumns; j++)
                                        {                                            
                                            out.write(getSpace(12 - header[j].length()));
                                            out.write(header[j]);
                                        }
                                        out.write(ls);
                                    }

                                    // Format and write data
                                    for(k = start; k < nRows && k < start + 900; k++) 
                                    {
                                        // Format and write data
                                        for(int l = 1; l < nColumns; l++)
                                            out.write(" " + Utility.formatData(8, f.format(Double.parseDouble(data[k][l]))));
                                        out.write(ls);
                                    }
                                    start = k;
                                }
                                file = null;
                                out.close();
                            }
                            catch(IOException e )
                            {
                                JOptionPane.showMessageDialog(null, e,  
                                                              "File Error",              
                                                              JOptionPane.ERROR_MESSAGE);
                                return;
                            }
                        }
                        else
                        {
                            files.setDialogTitle("");
                            files.setSelectedFile(new File(""));    
                        }
                    }
                }
            }
            else
            {
                JOptionPane.showMessageDialog(null, "The data is not available", 
                                              "Data Not Found Error",               
                                              JOptionPane.ERROR_MESSAGE);                
            }
        }

        // Promote user to save presentation data
        if(output.dataAll != null )
        {
            if(JOptionPane.showConfirmDialog(null, 
                                             "Do you want to save the SPK report data file?",   
                                             "Question Dialog",
                                             JOptionPane.YES_NO_OPTION,
                                             JOptionPane.QUESTION_MESSAGE) == 0)
            {
                files.setDialogTitle("Save SPK report data File");
                files.setSelectedFile(new File("data.txt")); 
                int result = files.showSaveDialog(null);
                if(result == files.APPROVE_OPTION)
	        {
                    file = files.getSelectedFile();
                    try
                    {
                        BufferedWriter out = new BufferedWriter(new FileWriter(file));
                        int nColumns = output.dataItems.size();
                        int nRows = output.dataAll.length;

                        for(int i = 0; i < nColumns; i++)
                        {
                            String label = (String)output.dataItems.get(i); 
                            out.write(getSpace(12 - label.length()));
                            out.write(label);                            
                        }
                        out.write(ls);
                        DecimalFormat f = new DecimalFormat("0.0000E00");
                        for(int j = 0; j < nRows; j++)
                        {
                            for(int i = 0; i < nColumns; i++)
                                out.write(" " + Utility.formatData(8, f.format(output.dataAll[j][i]))); 
                            out.write(ls);
                        }
                        file = null;
                        out.close();
                    }
                    catch(IOException e )
                    {
                        JOptionPane.showMessageDialog(null, e,  
                                                      "File Error",               
                                                      JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                }
                else
                {
                    files.setDialogTitle("");
                    files.setSelectedFile(new File(""));                   
                }
            }
        }
        JOptionPane.showMessageDialog(null, "Output processing is finished. Result is ready for presentation.",  
                                      "MDA Status Information",             
                                      JOptionPane.INFORMATION_MESSAGE);
        saveFile();
        textArea.setText(Summary.makeSummary(output, isOnline, isDeveloper, jobMethodCode, methodTable));
        textArea.setCaretPosition(0);
        jInternalFrame1.setTitle("");
        file = null;
    }
    
    // This function return spaces
    private String getSpace(int n)
    {
        String s = "";
        for(int i = 0; i < n; i++)
            s += " ";
        return s;  
    }

    // This function returns matrix width
    private int width(int dimension) 
    {
        if(dimension < 3)
            dimension = 3;
        int width = (dimension + 1) * 80 + 60; 
        if(width > 800)
            width = 800;
        return width;
    }
    
    // This function returns matrix height
    private int height(int dimension)
    {
        int height = (dimension + 1) * 20 + 120;
        if(height > 600)
            height = 600;
        return height;
    }  
    
    /** This function asks the user wether to save the text in the editor text area to a file.
     */
    protected void saveFile()
    {
        if(!textArea.getText().equals(""))
        {
            String pathName = "untitled";
            if(file != null)
                pathName = file.getPath();
            if(JOptionPane.showConfirmDialog(null, 
                                             "Do you want to save the file " + pathName + "?",   
                                             "Question Dialog",
                                             JOptionPane.YES_NO_OPTION,
                                             JOptionPane.QUESTION_MESSAGE) == 0)
            {
                if(pathName.equals("untitled")) 
                {
                    int result = files.showSaveDialog(null);
                    if(result == files.APPROVE_OPTION)
                    {
                        file = files.getSelectedFile();
                        saveOperation(textArea.getText());
                    }
                }
                else
                {
                    file = new File(pathName);
                    saveOperation(textArea.getText());
                }
            }  
        }
    }
    
    // Display a list of jobs or models or versions that belongs to the user
    private void showArchiveList(boolean isRepeatCall)
    {
        String[] header = null;
        String title = "";
        String[][] archiveList = null;
        switch(listType.charAt(0))
        {            
            case 'j':
            {
                timer.start();
                title = "Job List"; 
                header = new String[]{"Job ID", "Submission Time", "Status Code", "Model.Version", "Dataset.Version", "Job Description"};
                break;
            }
            case 'm':
            {
                title = "Model List"; 
                header = new String[]{"Model Name", "No. of Versions", "Last Revised Time", "Description"};
                break;
            }
            case 'd':
            {
                title = "Dataset List";
                header = new String[]{"Dataset Name", "No. of Versions", "Last Revised Time", "Description"};
                break;
            }            
            default: return;
        }
        
        if(!isRepeatCall && indexList < lists.size())
        {
            archiveList = (String[][])lists.get(indexList);
            if(archiveList.length <= maxNum)
                nextButton.setEnabled(false);
        }
        else
        {
            long leftOff = 0L;
            if(indexList != 0)
                leftOff = Long.parseLong(((String[][])lists.get(indexList - 1))[maxNum - 1][0]);

            switch(listType.charAt(0))
            {            
                case 'j':
                {
                    archiveList = server.getUserJobs(maxNum + 1, leftOff, isLibrary);
                    break;
                }
                case 'm':
                {
                    archiveList = server.getUserModels(maxNum + 1, leftOff, isLibrary);
                    break;
                }
                case 'd':
                {
                    archiveList = server.getUserDatasets(maxNum + 1, leftOff, isLibrary); 
                    break;
                }
                default: return;             
            }
            
            if(archiveList == null)
            {
                JOptionPane.showMessageDialog(null, "No " + listType + " was returned from the server.",
                                                  "Server Information",
                                                  JOptionPane.INFORMATION_MESSAGE);
                if(listType.equals("job"))
                    timer.stop();
                return;
            }

            // Add the list to the collection or use the list to update the collection
            if(!isRepeatCall)
                lists.add(archiveList);
            else
                lists.set(indexList, archiveList);
            if(archiveList.length <= maxNum)
                // Turn off the next button
                nextButton.setEnabled(false); 
            else
                // Turn on the next button
                nextButton.setEnabled(true);            

            if(indexList == 0)
                previousButton.setEnabled(false);
        }
        
        // Put the list in a table and then show the dialog containing the table
        if(archiveList.length < 0)
            return;
        int start = listType.equals("job") ? 0:1;
        DisplayTableModel reportModel = new DisplayTableModel(archiveList, header, start);
        jTable1.setModel(reportModel);
        TableColumnModel columnModel = jTable1.getColumnModel();
        if(listType.equals("job"))
        {
            columnModel.getColumn(0).setPreferredWidth(100);
            columnModel.getColumn(1).setPreferredWidth(200);
            columnModel.getColumn(2).setPreferredWidth(120);
            columnModel.getColumn(3).setPreferredWidth(150);
            columnModel.getColumn(4).setPreferredWidth(150);
            columnModel.getColumn(0).setCellRenderer(new CellRenderer()); 
        }
        else
        {
            columnModel.getColumn(0).setPreferredWidth(200);
            columnModel.getColumn(1).setPreferredWidth(120);
            columnModel.getColumn(2).setPreferredWidth(200);
            columnModel.getColumn(1).setCellRenderer(new CellRenderer());            
        }

        columnModel.getColumn(header.length - 1).setPreferredWidth(300);
        int length = archiveList.length;
        if(length > maxNum)
            length--;        
        reportDialog.setSize(1000, 16 * length + 90);  
        reportDialog.setTitle(title);
        reportDialog.setFocusableWindowState(false);
        reportDialog.setVisible(true);
    }
    
    // Display a list of jobs or models or versions that belongs to the user
    private void showVersionList(long id)
    {
        String[] header = {"Revision", "Author", "Revised Time", "Log Message"};
        String[][] versionList = server.getVersions(id, listType, isLibrary);             
        if(versionList != null)
        {
            DisplayTableModel versionModel = new DisplayTableModel(versionList, header, 0); 
            jTable2.setModel(versionModel);
            TableColumnModel columnModel = jTable2.getColumnModel();
            columnModel.getColumn(3).setPreferredWidth(500);
            versionDialog.setTitle("Version List");
            versionDialog.setLocation(200, 200);
            versionDialog.setSize(800, 16 * versionList.length + 60); 
            versionDialog.show();
        }
    }    

    private class DisplayTableModel extends AbstractTableModel 
    {
        public DisplayTableModel(String[][] data, String[] header, int start)
        {
            this.data = data;
            this.header = header;
            this.start = start;
        }
        
        public String getColumnName(int c) 
        {
            return header[c];
        }
        public Class getColumnClass(int c) 
        {
            return "".getClass();
        }
        public int getColumnCount() 
        {
            return header.length; 
        }
        public int getRowCount()
        {
            int length = data.length;
            if(length > maxNum)
                length--;
            return length;
        }
        public Object getValueAt(int r, int c)
        {
            return data[r][c + start];
        }

        // Table data array
        String[][] data = null;
        
        // Table header array
        String[] header = null;
        
        // Starting column
        int start = 0;
    }    

    private class CellRenderer extends DefaultTableCellRenderer 
    {
        public Component getTableCellRendererComponent(JTable table,
            Object value,boolean isSelected, boolean hasFocus, int row,int col) 
        {
            super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,col);
            setHorizontalAlignment(SwingConstants.CENTER);
            return this;
	}
    }    
    
    /** Returns the text in the MDA editor.
     * @return a String containing the text in the MDA editor.
     */
    public String getEditorText()
    {
        return textArea.getText();
    }
    
    /** Set text to the editor.
     * @param text a string to display in the editor.
     */    
    protected void setEditorText(String text)
    {
        textArea.setText(text);   
    }
    
    /** Set editor's caret position.
     * @param position an int, the position for the caret.
     */    
    protected void setEditorCaretPosition(int position)
    {
        textArea.setCaretPosition(position);
    }
    
    /** Set editor title.
     * @param title a string for the editor title.
     */    
    protected void setEditorTitle(String title)
    {
        jInternalFrame1.setTitle(title);   
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton CompareFilesButton;
    private javax.swing.JButton DataArchiveButton;
    private javax.swing.JButton DatasetLibraryButton;
    private javax.swing.JButton GetReportButton;
    private javax.swing.JButton HelpButton;
    private javax.swing.JButton JobExamplesButton;
    private javax.swing.JButton ModelArchiveButton;
    private javax.swing.JButton ModelLibraryButton;
    private javax.swing.JMenuItem OmegaMenu;
    private javax.swing.JButton ReadOutputButton;
    private javax.swing.JMenuItem SigmaMenu;
    private javax.swing.JButton SubmitJobButton;
    private javax.swing.JMenuItem ThetaMenu;
    private javax.swing.JButton WriteInputButton;
    private javax.swing.JDialog archiveDialog;
    private javax.swing.JMenuItem bothMenu;
    private javax.swing.JMenuItem bothPlotMenu;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.ButtonGroup buttonGroup3;
    private javax.swing.JButton cancelButton;
    private javax.swing.JMenuItem closeMenu;
    private javax.swing.JButton compareButton;
    private javax.swing.JMenuItem copyMenu;
    private javax.swing.JMenuItem correlationMenu;
    private javax.swing.JMenuItem covarianceMenu;
    private javax.swing.JMenuItem cutMenu;
    private javax.swing.JButton dataLButton;
    private javax.swing.JButton dataLibLButton;
    private javax.swing.JButton dataLibRButton;
    private javax.swing.JButton dataRButton;
    private javax.swing.JDialog diffDialog;
    private javax.swing.JDialog diffHelpDialog;
    private javax.swing.JMenuItem dotsMenu;
    private javax.swing.JMenuItem dotsPlotMenu;
    private javax.swing.JMenuItem errorMenu;
    private javax.swing.JDialog errorMessageDialog;
    private javax.swing.JMenuItem exitMenu;
    private javax.swing.JMenuItem findMenu;
    private javax.swing.JButton helpButton;
    private javax.swing.JMenuItem invCovarianceMenu;
    private javax.swing.JInternalFrame jInternalFrame1;
    private javax.swing.JInternalFrame jInternalFrame2;
    private javax.swing.JInternalFrame jInternalFrame3;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel15;
    private javax.swing.JLabel jLabel16;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu6;
    private javax.swing.JMenu jMenu7;
    private javax.swing.JMenu jMenu9;
    private javax.swing.JMenuBar jMenuBar1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel12;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JRadioButton jRadioButton1;
    private javax.swing.JRadioButton jRadioButton10;
    private javax.swing.JRadioButton jRadioButton11;
    private javax.swing.JRadioButton jRadioButton12;
    private javax.swing.JRadioButton jRadioButton2;
    private javax.swing.JRadioButton jRadioButton3;
    private javax.swing.JRadioButton jRadioButton4;
    private javax.swing.JRadioButton jRadioButton5;
    private javax.swing.JRadioButton jRadioButton6;
    private javax.swing.JRadioButton jRadioButton7;
    private javax.swing.JRadioButton jRadioButton8;
    private javax.swing.JRadioButton jRadioButton9;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JScrollPane jScrollPane5;
    private javax.swing.JScrollPane jScrollPane6;
    private javax.swing.JScrollPane jScrollPane7;
    private javax.swing.JSeparator jSeparator4;
    private javax.swing.JSeparator jSeparator5;
    private javax.swing.JSeparator jSeparator6;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JTable jTable1;
    private javax.swing.JTable jTable2;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextArea jTextArea2;
    private javax.swing.JTextArea jTextArea3;
    private javax.swing.JTextArea jTextArea4;
    private javax.swing.JTextArea jTextArea5;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField14;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextField jTextField4;
    private javax.swing.JTextField jTextField5;
    private javax.swing.JTextField jTextField6;
    private javax.swing.JTextField jTextField7;
    private javax.swing.JTextField jTextField8;
    private javax.swing.JTextField jTextField9;
    private javax.swing.JTextPane jTextPane1;
    private javax.swing.JTextPane jTextPane2;
    private javax.swing.JTextPane jTextPane3;
    private javax.swing.JMenuItem lineMenu;
    private javax.swing.JMenuItem linePlotMenu;
    private javax.swing.JButton localLButton;
    private javax.swing.JButton localRButton;
    private javax.swing.JButton modelLButton;
    private javax.swing.JButton modelLibLButton;
    private javax.swing.JButton modelLibRButton;
    private javax.swing.JButton modelRButton;
    private javax.swing.JButton nextButton;
    private javax.swing.JButton nextDiffButton;
    private javax.swing.JDialog objectiveDialog;
    private javax.swing.JMenuItem objectiveMenu;
    private javax.swing.JButton okButton;
    private javax.swing.JMenuItem openMenu;
    private javax.swing.JMenu parameterMenu;
    private javax.swing.JMenuItem pasteMenu;
    private javax.swing.JButton previousButton;
    private javax.swing.JMenuItem printMenu;
    private javax.swing.JButton refreshButton;
    private javax.swing.JDialog reportDialog;
    private javax.swing.JMenuItem savaAsMenu;
    private javax.swing.JMenuItem saveMenu;
    private javax.swing.JMenu scatterPlotMenu;
    private javax.swing.JMenu statisticsMenu;
    private javax.swing.JMenuItem stdErrOmegaMenu;
    private javax.swing.JMenuItem stdErrSigmaMenu;
    private javax.swing.JMenuItem stdErrThetaMenu;
    private javax.swing.JMenu stdErrorMenu;
    private javax.swing.JMenuItem summaryMenu;
    private javax.swing.JMenuItem tableMenu;
    private javax.swing.JTextArea textArea;
    private javax.swing.JMenuItem traceMenu;
    private javax.swing.JMenuItem useRMenu;
    private javax.swing.JDialog versionDialog;
    private javax.swing.JMenuItem warningMenu;
    private javax.swing.JDialog warningMessageDialog;
    // End of variables declaration//GEN-END:variables

    /** The HelpBroker. */
    protected HelpBroker helpBroker = null;
    
    /** The method table for the database. */
    protected HashMap methodTable = null;
    
    /** The flag for the user being a tester. */
    protected boolean isTester = false;
    
    /** The flag for the user being a developer. */
    protected boolean isDeveloper = false;    
    
    /** The current job id. */
    protected long jobId = 0; 

    // Job method code
    private String jobMethodCode = null;
    
    // Analytical approximation method
    private String method = null;
    
    /** The Server name. */
    protected String serverName = null;
    
    /** The flag for online status. */
    protected boolean isOnline = true;
    
    /** The server port. */
    protected String serverPort = null;

    // File chooser
    private JFileChooser files = new JFileChooser();

    /** The current file. */
    protected File file = null;

    // The model archive information
    private ArchiveInfo modelArchive = null;
    
    // The data archive information
    private ArchiveInfo dataArchive = null; 
    
    // The model (Nonmem Control file) text 
    private String control = null;
    
    // MDA object
    private MDAObject object = new MDAObject();
    
    // Spk output
    private Output output = null;
    
    // TableShow object
    private TableShow tableShow = null;

    // JWizardPane
    private JWizardPane wp = null;
    
    /** The Server object. */
    protected Server server = null;

    // List content
    private String listType = null;
    
    // Is library
    private boolean isLibrary = true;
    
    // Show version list
    private boolean showVersions = true;
    
    // Get archive
    private boolean getArchive = true; 
    
    // Is diff
    private boolean isDiff = true;
    
    // The text on the left to diff
    private String textL = null;
    
    // The text on the right to diff
    private String textR = null;    
    
    // Line number of delta 
    private Vector deltaLines = null;
    
    // Delta index
    private int deltaIndex = 0;
    
    // Is left
    private boolean isLeft = true;
    
    // List collection
    private Vector lists = null;
    
    // Index of the list in list collection
    private int indexList = 0;
    
    // Selected archive name
    private String archiveName = null;
    
    // line separator
    private static final String ls = System.getProperty("line.separator");
    
    // Maximum number of items
    private static final int maxNum = 12;
    
    // Timer for refreshing the job list dialog
    private Timer timer = null;
}