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

import uw.rfpk.mda.*;
import javax.help.*; 
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.undo.*;
import javax.swing.text.*;
import javax.print.*;
import javax.print.attribute.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.nio.*;
import java.nio.channels.FileChannel;
import java.net.URL;
import java.util.Calendar;
import java.util.Vector;
import java.util.ArrayList;
import java.util.Properties;
import java.util.HashMap;
import java.util.GregorianCalendar;
import java.awt.print.*;
import java.awt.font.*;
import org.netbeans.ui.wizard.*;
import uw.rfpk.mda.nonmem.wizard.*;
import uw.rfpk.mda.nonmem.display.*;
import javax.swing.table.*;  
import java.text.DecimalFormat;
import java.text.NumberFormat;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

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
        jMenu9.remove(identifiabilityTraceMenu);   // Uncomment it to remove identifiability
        textArea.getDocument().addDocumentListener(new MyDocumentListener());
        cutMenu.addActionListener(new DefaultEditorKit.CutAction());  
        copyMenu.addActionListener(new DefaultEditorKit.CopyAction());  
        pasteMenu.addActionListener(new DefaultEditorKit.PasteAction()); 
        
        // Check server connection and get method table
        if(args.length != 8)
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
            String tester = args[4];
            isTester = args[4].equals("1") ? true : false;
            isDeveloper = args[5].equals("1") ? true : false;
            setTitle("Model Design Agent for " + args[6]);
            myName = args[6];
            username = myName;
            groupID = args[7];
            if(!args[7].equals("0"))
            {
                Vector group = server.getGoupUsers();
                groupLabel.setEnabled(true);
                groupComboBox.setEnabled(true);
                for(int i = 0; i < group.size(); i++)
                    groupComboBox.addItem(group.get(i));
                groupComboBox.setSelectedItem(myName);
                isInit = false;
            }
            else
            {
                groupLabel.setEnabled(false);
                groupComboBox.setEnabled(false);
            }
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
                showArchiveList(true, false);
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
            String hsName = "spkhelp.hs";
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
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
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
        jTextField3 = new javax.swing.JTextField();
        jScrollPane8 = new javax.swing.JScrollPane();
        jTextArea8 = new javax.swing.JTextArea();
        jLabel27 = new javax.swing.JLabel();
        jScrollPane12 = new javax.swing.JScrollPane();
        jTextArea6 = new javax.swing.JTextArea();
        jPanel3 = new javax.swing.JPanel();
        jRadioButton4 = new javax.swing.JRadioButton();
        jRadioButton5 = new javax.swing.JRadioButton();
        jRadioButton6 = new javax.swing.JRadioButton();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jTextField4 = new javax.swing.JTextField();
        jTextField6 = new javax.swing.JTextField();
        jScrollPane9 = new javax.swing.JScrollPane();
        jTextArea9 = new javax.swing.JTextArea();
        jLabel28 = new javax.swing.JLabel();
        jScrollPane13 = new javax.swing.JScrollPane();
        jTextArea7 = new javax.swing.JTextArea();
        jPanel5 = new javax.swing.JPanel();
        jLabel7 = new javax.swing.JLabel();
        jRadioButton7 = new javax.swing.JRadioButton();
        jRadioButton8 = new javax.swing.JRadioButton();
        jRadioButton9 = new javax.swing.JRadioButton();
        jRadioButton10 = new javax.swing.JRadioButton();
        jRadioButton11 = new javax.swing.JRadioButton();
        jRadioButton12 = new javax.swing.JRadioButton();
        jScrollPane10 = new javax.swing.JScrollPane();
        jTextArea10 = new javax.swing.JTextArea();
        jCheckBox2 = new javax.swing.JCheckBox();
        jCheckBox1 = new javax.swing.JCheckBox();
        buttonGroup1 = new javax.swing.ButtonGroup();
        errorMessageDialog = new javax.swing.JDialog();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        warningMessageDialog = new javax.swing.JDialog();
        jScrollPane7 = new javax.swing.JScrollPane();
        jTextArea5 = new javax.swing.JTextArea();
        objectiveDialog = new javax.swing.JDialog();
        jTextArea2 = new javax.swing.JTextArea();
        jButton3 = new javax.swing.JButton();
        reportDialog = new javax.swing.JDialog();
        jPanel4 = new javax.swing.JPanel();
        findJobButton = new javax.swing.JButton();
        previousButton = new javax.swing.JButton();
        nextButton = new javax.swing.JButton();
        jPanel17 = new javax.swing.JPanel();
        jLabel26 = new javax.swing.JLabel();
        versionRadioButton = new javax.swing.JRadioButton();
        jobRadioButton = new javax.swing.JRadioButton();
        groupLabel = new javax.swing.JLabel();
        groupComboBox = new javax.swing.JComboBox();
        countLabel = new javax.swing.JLabel();
        countTextField = new javax.swing.JTextField();
        jScrollPane3 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        buttonGroup2 = new javax.swing.ButtonGroup();
        findJobDialog = new javax.swing.JDialog();
        jLabel20 = new javax.swing.JLabel();
        jLabel21 = new javax.swing.JLabel();
        keyWordTextField = new javax.swing.JTextField();
        jPanel14 = new javax.swing.JPanel();
        findJobOKButton = new javax.swing.JButton();
        findJobCancelButton = new javax.swing.JButton();
        jPanel15 = new javax.swing.JPanel();
        jLabel22 = new javax.swing.JLabel();
        mComboBox = new javax.swing.JComboBox();
        jLabel23 = new javax.swing.JLabel();
        dComboBox = new javax.swing.JComboBox();
        jLabel24 = new javax.swing.JLabel();
        yComboBox = new javax.swing.JComboBox();
        jPanel16 = new javax.swing.JPanel();
        jLabel19 = new javax.swing.JLabel();
        jobIDTextField = new javax.swing.JTextField();
        jLabel25 = new javax.swing.JLabel();
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
        replaceDialog = new javax.swing.JDialog();
        jLabel17 = new javax.swing.JLabel();
        jLabel18 = new javax.swing.JLabel();
        jTextField2 = new javax.swing.JTextField();
        jTextField5 = new javax.swing.JTextField();
        jPanel13 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        indIDDialog = new javax.swing.JDialog();
        jScrollPane11 = new javax.swing.JScrollPane();
        jTable3 = new javax.swing.JTable();
        buttonGroup4 = new javax.swing.ButtonGroup();
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
        jLabel16 = new javax.swing.JLabel();
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
        // Listen for undo and redo events
        textArea.getDocument().addUndoableEditListener(new UndoableEditListener() {
            public void undoableEditHappened(UndoableEditEvent evt) {
                undo.addEdit(evt.getEdit());
            }
        });

        // Create an undo action and add it to the text component
        textArea.getActionMap().put("Undo", new AbstractAction("Undo") {
            public void actionPerformed(ActionEvent evt) {
                try {
                    if (undo.canUndo()) {
                        undo.undo();
                    }
                } catch (CannotUndoException e) {
                }
            }
        });

        // Create an undo action and add it to the text component
        textArea.getActionMap().put("Redo", new AbstractAction("Redo") {
            public void actionPerformed(ActionEvent evt) {
                try {
                    if (undo.canRedo()) {
                        undo.redo();
                    }
                } catch (CannotUndoException e) {
                }
            }
        });

        JMenuItem undo = jMenu7.add(textArea.getActionMap().get("Undo"));
        JMenuItem redo = jMenu7.add(textArea.getActionMap().get("Redo"));

        undo.setAccelerator(KeyStroke.getKeyStroke("control N"));
        redo.setAccelerator(KeyStroke.getKeyStroke("control D"));
        undo.setMnemonic('n');
        redo.setMnemonic('d');
        jSeparator7 = new javax.swing.JSeparator();
        cutMenu = new javax.swing.JMenuItem();
        copyMenu = new javax.swing.JMenuItem();
        pasteMenu = new javax.swing.JMenuItem();
        deleteMenu = new javax.swing.JMenuItem();
        jSeparator8 = new javax.swing.JSeparator();
        findMenu = new javax.swing.JMenuItem();
        replaceMenu = new javax.swing.JMenuItem();
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
        scatterPlotMenu = new javax.swing.JMenuItem();
        summaryMenu = new javax.swing.JMenuItem();
        traceMenu = new javax.swing.JMenuItem();
        identifiabilityTraceMenu = new javax.swing.JMenuItem();
        indIDMenu = new javax.swing.JMenuItem();
        nonparamMenu = new javax.swing.JMenu();
        paramOutMenu = new javax.swing.JMenuItem();
        densityMenu = new javax.swing.JMenuItem();
        meanMenu = new javax.swing.JMenuItem();
        paramInMenu = new javax.swing.JMenuItem();
        jMenu4 = new javax.swing.JMenu();
        jMenu2 = new javax.swing.JMenu();
        jMenuItem10 = new javax.swing.JMenuItem();
        jMenuItem11 = new javax.swing.JMenuItem();
        jMenuItem12 = new javax.swing.JMenuItem();
        jMenuItem13 = new javax.swing.JMenuItem();
        FirstMenu = new javax.swing.JMenuItem();
        jMenuItem9 = new javax.swing.JMenuItem();
        jMenu1 = new javax.swing.JMenu();
        jMenu3 = new javax.swing.JMenu();
        jMenuItem1 = new javax.swing.JMenuItem();
        jMenuItem2 = new javax.swing.JMenuItem();
        jMenuItem5 = new javax.swing.JMenuItem();
        jMenuItem6 = new javax.swing.JMenuItem();
        jMenuItem3 = new javax.swing.JMenuItem();
        jMenuItem4 = new javax.swing.JMenuItem();
        jMenuItem7 = new javax.swing.JMenuItem();
        jMenuItem8 = new javax.swing.JMenuItem();
        dataMenu = new javax.swing.JMenuItem();
        jSeparator1 = new javax.swing.JSeparator();
        useMDAMenu = new javax.swing.JMenuItem();
        useRMenu = new javax.swing.JMenuItem();

        archiveDialog.getContentPane().setLayout(new java.awt.GridBagLayout());

        archiveDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        archiveDialog.setTitle("Job Submission Dialog");
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

        jTabbedPane1.setMaximumSize(new java.awt.Dimension(290, 300));
        jTabbedPane1.setMinimumSize(new java.awt.Dimension(290, 300));
        jTabbedPane1.setPreferredSize(new java.awt.Dimension(290, 300));
        jPanel2.setLayout(new java.awt.GridBagLayout());

        jPanel2.setMaximumSize(new java.awt.Dimension(290, 250));
        jPanel2.setMinimumSize(new java.awt.Dimension(290, 250));
        jPanel2.setPreferredSize(new java.awt.Dimension(290, 250));
        buttonGroup1.add(jRadioButton1);
        jRadioButton1.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton1.setSelected(true);
        jRadioButton1.setText("New model (NONMEM control file)");
        jRadioButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 0, 0, 0);
        jPanel2.add(jRadioButton1, gridBagConstraints);

        buttonGroup1.add(jRadioButton2);
        jRadioButton2.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton2.setText("New version of an existing model");
        jRadioButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(jRadioButton2, gridBagConstraints);

        buttonGroup1.add(jRadioButton3);
        jRadioButton3.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton3.setText("Existing version of an existing model");
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
        jLabel2.setText("model description (<= 100characters)");
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

        jTextField1.setColumns(20);
        jTextField1.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextField1KeyTyped(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel2.add(jTextField1, gridBagConstraints);

        jTextField3.setEditable(false);
        jTextField3.setText("1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel2.add(jTextField3, gridBagConstraints);

        jScrollPane8.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        jScrollPane8.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
        jScrollPane8.setMaximumSize(new java.awt.Dimension(260, 48));
        jScrollPane8.setMinimumSize(new java.awt.Dimension(260, 48));
        jScrollPane8.setPreferredSize(new java.awt.Dimension(260, 48));
        jTextArea8.setLineWrap(true);
        jTextArea8.setRows(3);
        jTextArea8.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextArea8KeyTyped(evt);
            }
        });

        jScrollPane8.setViewportView(jTextArea8);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel2.add(jScrollPane8, gridBagConstraints);

        jLabel27.setFont(new java.awt.Font("Default", 0, 12));
        jLabel27.setText("version log");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(jLabel27, gridBagConstraints);

        jTextArea6.setLineWrap(true);
        jTextArea6.setRows(2);
        jScrollPane12.setViewportView(jTextArea6);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel2.add(jScrollPane12, gridBagConstraints);

        jTabbedPane1.addTab("Model", jPanel2);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel3.setMaximumSize(new java.awt.Dimension(280, 250));
        jPanel3.setMinimumSize(new java.awt.Dimension(280, 250));
        jPanel3.setPreferredSize(new java.awt.Dimension(280, 250));
        buttonGroup2.add(jRadioButton4);
        jRadioButton4.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton4.setSelected(true);
        jRadioButton4.setText("New dataset");
        jRadioButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton4ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 0, 0, 0);
        jPanel3.add(jRadioButton4, gridBagConstraints);

        buttonGroup2.add(jRadioButton5);
        jRadioButton5.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton5.setText("New version of an existing dataset");
        jRadioButton5.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton5ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jRadioButton5, gridBagConstraints);

        buttonGroup2.add(jRadioButton6);
        jRadioButton6.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton6.setText("Existing version of an existing dataset");
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
        jLabel5.setText("dataset description (<=100 characters )");
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

        jTextField4.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextField4KeyTyped(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel3.add(jTextField4, gridBagConstraints);

        jTextField6.setEditable(false);
        jTextField6.setText("1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel3.add(jTextField6, gridBagConstraints);

        jScrollPane9.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        jScrollPane9.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
        jScrollPane9.setMaximumSize(new java.awt.Dimension(260, 48));
        jScrollPane9.setMinimumSize(new java.awt.Dimension(260, 48));
        jScrollPane9.setPreferredSize(new java.awt.Dimension(260, 48));
        jTextArea9.setLineWrap(true);
        jTextArea9.setRows(3);
        jTextArea9.setBorder(null);
        jTextArea9.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextArea9KeyTyped(evt);
            }
        });

        jScrollPane9.setViewportView(jTextArea9);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel3.add(jScrollPane9, gridBagConstraints);

        jLabel28.setFont(new java.awt.Font("Default", 0, 12));
        jLabel28.setText("version log");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jLabel28, gridBagConstraints);

        jTextArea7.setLineWrap(true);
        jTextArea7.setRows(2);
        jScrollPane13.setViewportView(jTextArea7);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        jPanel3.add(jScrollPane13, gridBagConstraints);

        jTabbedPane1.addTab("Dataset", jPanel3);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jPanel5.setFocusable(false);
        jPanel5.setMaximumSize(new java.awt.Dimension(280, 250));
        jPanel5.setMinimumSize(new java.awt.Dimension(280, 250));
        jPanel5.setPreferredSize(new java.awt.Dimension(280, 250));
        jLabel7.setFont(new java.awt.Font("Default", 0, 12));
        jLabel7.setText("job abstract (<=100 characters)     ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(jLabel7, gridBagConstraints);

        buttonGroup3.add(jRadioButton7);
        jRadioButton7.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton7.setText("Use the method specified in input file");
        jRadioButton7.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton7ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jRadioButton7, gridBagConstraints);

        buttonGroup3.add(jRadioButton8);
        jRadioButton8.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton8.setText("Monte Carlo integration on likelihood ");
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

        buttonGroup3.add(jRadioButton9);
        jRadioButton9.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton9.setText("Vegas M.C. integration on likelihood");
        jRadioButton9.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton9ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jRadioButton9, gridBagConstraints);

        buttonGroup3.add(jRadioButton10);
        jRadioButton10.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton10.setText("Grid integration on likelihood");
        jRadioButton10.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton10ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jRadioButton10, gridBagConstraints);

        buttonGroup3.add(jRadioButton11);
        jRadioButton11.setFont(new java.awt.Font("Default", 0, 12));
        jRadioButton11.setText("Adapt integration of likelihood");
        jRadioButton11.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton11ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jRadioButton11, gridBagConstraints);

        buttonGroup3.add(jRadioButton12);
        jRadioButton12.setFont(new java.awt.Font("Dialog", 0, 12));
        jRadioButton12.setText("Miser M.C. integration on likelihood");
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

        jScrollPane10.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        jScrollPane10.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER);
        jScrollPane10.setMaximumSize(new java.awt.Dimension(260, 48));
        jScrollPane10.setMinimumSize(new java.awt.Dimension(260, 48));
        jScrollPane10.setPreferredSize(new java.awt.Dimension(260, 48));
        jTextArea10.setLineWrap(true);
        jTextArea10.setRows(3);
        jTextArea10.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextArea10KeyTyped(evt);
            }
        });

        jScrollPane10.setViewportView(jTextArea10);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel5.add(jScrollPane10, gridBagConstraints);

        jCheckBox2.setFont(new java.awt.Font("Dialog", 0, 12));
        jCheckBox2.setText("Run the job in parallel computation mode");
        jCheckBox2.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        jCheckBox2.setEnabled(false);
        jCheckBox2.setMargin(new java.awt.Insets(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        jPanel5.add(jCheckBox2, gridBagConstraints);

        jCheckBox1.setFont(new java.awt.Font("Dialog", 0, 12));
        jCheckBox1.setText("Email me when the job has finished");
        jCheckBox1.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        jCheckBox1.setMargin(new java.awt.Insets(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        jPanel5.add(jCheckBox1, gridBagConstraints);

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

        jButton3.setText("Show Estimated Likelihood");
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        objectiveDialog.getContentPane().add(jButton3, java.awt.BorderLayout.SOUTH);

        reportDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        reportDialog.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent we) {
                timer.stop();
            }
        });
        reportDialog.setTitle("");
        reportDialog.setBackground(java.awt.Color.white);
        reportDialog.setModal(true);
        reportDialog.addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                reportDialogWindowClosing(evt);
            }
        });

        jPanel4.setLayout(new java.awt.GridBagLayout());

        findJobButton.setText("Search Jobs");
        findJobButton.setMaximumSize(new java.awt.Dimension(127, 26));
        findJobButton.setMinimumSize(new java.awt.Dimension(127, 26));
        findJobButton.setPreferredSize(new java.awt.Dimension(127, 26));
        findJobButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findJobButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 100);
        jPanel4.add(findJobButton, gridBagConstraints);

        previousButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("/org/netbeans/ui/wizard/plaf/basic/icons/back.gif")));
        previousButton.setText("Previous Page");
        previousButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        previousButton.setMaximumSize(new java.awt.Dimension(127, 26));
        previousButton.setMinimumSize(new java.awt.Dimension(127, 26));
        previousButton.setPreferredSize(new java.awt.Dimension(127, 26));
        previousButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                previousButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 6;
        gridBagConstraints.gridy = 0;
        jPanel4.add(previousButton, gridBagConstraints);

        nextButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("/org/netbeans/ui/wizard/plaf/basic/icons/next.gif")));
        nextButton.setText("Next Page");
        nextButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        nextButton.setMaximumSize(new java.awt.Dimension(127, 26));
        nextButton.setMinimumSize(new java.awt.Dimension(127, 26));
        nextButton.setPreferredSize(new java.awt.Dimension(127, 26));
        nextButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                nextButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 7;
        gridBagConstraints.gridy = 0;
        jPanel4.add(nextButton, gridBagConstraints);

        jPanel17.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 5, 0));

        jLabel26.setText("List");
        jLabel26.setFocusable(false);
        jPanel17.add(jLabel26);

        buttonGroup4.add(versionRadioButton);
        versionRadioButton.setSelected(true);
        versionRadioButton.setText("Versions");
        jPanel17.add(versionRadioButton);

        buttonGroup4.add(jobRadioButton);
        jobRadioButton.setText("Jobs");
        jPanel17.add(jobRadioButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 50);
        jPanel4.add(jPanel17, gridBagConstraints);

        groupLabel.setFont(new java.awt.Font("Dialog", 0, 12));
        groupLabel.setText("Group member  ");
        groupLabel.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        jPanel4.add(groupLabel, gridBagConstraints);

        groupComboBox.setMaximumSize(new java.awt.Dimension(160, 22));
        groupComboBox.setMinimumSize(new java.awt.Dimension(160, 22));
        groupComboBox.setPreferredSize(new java.awt.Dimension(160, 22));
        groupComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                groupComboBoxActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 50);
        jPanel4.add(groupComboBox, gridBagConstraints);

        countLabel.setFont(new java.awt.Font("Dialog", 0, 12));
        countLabel.setText("Total found  ");
        countLabel.setFocusable(false);
        jPanel4.add(countLabel, new java.awt.GridBagConstraints());

        countTextField.setEditable(false);
        countTextField.setFont(new java.awt.Font("Dialog", 1, 12));
        countTextField.setHorizontalAlignment(javax.swing.JTextField.TRAILING);
        countTextField.setFocusable(false);
        countTextField.setMaximumSize(new java.awt.Dimension(60, 19));
        countTextField.setMinimumSize(new java.awt.Dimension(60, 19));
        countTextField.setPreferredSize(new java.awt.Dimension(60, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 50);
        jPanel4.add(countTextField, gridBagConstraints);

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

        findJobDialog.getContentPane().setLayout(new java.awt.GridBagLayout());

        findJobDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        findJobDialog.setTitle("Job Search Dialog");
        findJobDialog.setLocationRelativeTo(reportDialog);
        jLabel20.setText("Search only jobs submitted on or before");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(7, 12, 0, 13);
        findJobDialog.getContentPane().add(jLabel20, gridBagConstraints);

        jLabel21.setText("Search for strings either in Model Name, in");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(7, 12, 2, 12);
        findJobDialog.getContentPane().add(jLabel21, gridBagConstraints);

        keyWordTextField.setPreferredSize(new java.awt.Dimension(264, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 12, 0, 12);
        findJobDialog.getContentPane().add(keyWordTextField, gridBagConstraints);

        findJobOKButton.setText("OK");
        findJobOKButton.setPreferredSize(new java.awt.Dimension(75, 25));
        findJobOKButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findJobOKButtonActionPerformed(evt);
            }
        });

        jPanel14.add(findJobOKButton);

        findJobCancelButton.setText("Cancel");
        findJobCancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findJobCancelButtonActionPerformed(evt);
            }
        });

        jPanel14.add(findJobCancelButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.insets = new java.awt.Insets(10, 12, 10, 12);
        findJobDialog.getContentPane().add(jPanel14, gridBagConstraints);

        jLabel22.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel22.setText("Month");
        jPanel15.add(jLabel22);

        mComboBox.setMaximumRowCount(12);
        mComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12" }));
        jPanel15.add(mComboBox);

        jLabel23.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel23.setText("Day");
        jLabel23.setToolTipText("");
        jPanel15.add(jLabel23);

        dComboBox.setMaximumRowCount(12);
        dComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31" }));
        jPanel15.add(dComboBox);

        jLabel24.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel24.setText("Year");
        jPanel15.add(jLabel24);

        yComboBox.setMaximumRowCount(12);
        jPanel15.add(yComboBox);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        findJobDialog.getContentPane().add(jPanel15, gridBagConstraints);

        jLabel19.setText("Maximum Job ID number");
        jPanel16.add(jLabel19);

        jobIDTextField.setPreferredSize(new java.awt.Dimension(100, 19));
        jPanel16.add(jobIDTextField);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(9, 9, 0, 9);
        findJobDialog.getContentPane().add(jPanel16, gridBagConstraints);

        jLabel25.setText("Dataset Name or in Job Abstract");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        findJobDialog.getContentPane().add(jLabel25, gridBagConstraints);

        versionDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        versionDialog.setTitle("");
        versionDialog.setLocationRelativeTo(reportDialog);
        versionDialog.setModal(true);
        versionDialog.addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                versionDialogWindowClosing(evt);
            }
        });

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
        jScrollPane5.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
        jScrollPane5.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
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
        jScrollPane6.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
        jScrollPane6.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
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
        jLabel8.setFocusable(false);
        jLabel8.setPreferredSize(new java.awt.Dimension(50, 15));
        jLabel8.setOpaque(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        jPanel8.add(jLabel8, gridBagConstraints);

        diffHelpDialog.getContentPane().add(jPanel8, java.awt.BorderLayout.CENTER);

        replaceDialog.getContentPane().setLayout(new java.awt.GridBagLayout());

        replaceDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        replaceDialog.setTitle("Replacing String");
        replaceDialog.setLocationRelativeTo(this);
        replaceDialog.setModal(true);
        replaceDialog.setResizable(false);
        jLabel17.setText("Replace");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 6, 0);
        replaceDialog.getContentPane().add(jLabel17, gridBagConstraints);

        jLabel18.setText("By");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        replaceDialog.getContentPane().add(jLabel18, gridBagConstraints);

        jTextField2.setPreferredSize(new java.awt.Dimension(120, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 6, 12);
        replaceDialog.getContentPane().add(jTextField2, gridBagConstraints);

        jTextField5.setPreferredSize(new java.awt.Dimension(120, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        replaceDialog.getContentPane().add(jTextField5, gridBagConstraints);

        jButton1.setText("OK");
        jButton1.setMaximumSize(new java.awt.Dimension(75, 25));
        jButton1.setMinimumSize(new java.awt.Dimension(75, 25));
        jButton1.setPreferredSize(new java.awt.Dimension(75, 25));
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jPanel13.add(jButton1);

        jButton2.setText("Cancel");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        jPanel13.add(jButton2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        replaceDialog.getContentPane().add(jPanel13, gridBagConstraints);

        indIDDialog.setTitle("Individual IDs");
        indIDDialog.setLocationRelativeTo(this);
        jTable3.setModel(new javax.swing.table.DefaultTableModel(
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
        jScrollPane11.setViewportView(jTable3);

        indIDDialog.getContentPane().add(jScrollPane11, java.awt.BorderLayout.CENTER);

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setBackground(new java.awt.Color(0, 204, 204));
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
        WriteInputButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
        SubmitJobButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
        GetReportButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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

        ReadOutputButton.setText("Parse Report");
        ReadOutputButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
        JobExamplesButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
        ModelArchiveButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
        DataArchiveButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
        ModelLibraryButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
        DatasetLibraryButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
        CompareFilesButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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
        HelpButton.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
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

        jLabel16.setBackground(new java.awt.Color(0, 204, 204));
        jLabel16.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel16.setText("Status:  On Line");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 12, 14, 12);
        jPanel1.add(jLabel16, gridBagConstraints);

        jInternalFrame1.setBackground(new java.awt.Color(255, 255, 255));
        jInternalFrame1.setFocusable(false);
        jInternalFrame1.setMinimumSize(new java.awt.Dimension(603, 460));
        jInternalFrame1.setPreferredSize(new java.awt.Dimension(603, 460));
        jInternalFrame1.setRequestFocusEnabled(false);
        jInternalFrame1.setVisible(true);
        textArea.setFont(new java.awt.Font("Courier", 0, 12));
        textArea.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                textAreaKeyPressed(evt);
            }
        });
        textArea.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                textAreaMouseClicked(evt);
            }
        });

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

        closeMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_K, java.awt.event.InputEvent.CTRL_MASK));
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
        savaAsMenu.setText("Save As");
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
        jMenu7.add(jSeparator7);

        cutMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_U, java.awt.event.InputEvent.CTRL_MASK));
        cutMenu.setMnemonic('u');
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

        deleteMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X, java.awt.event.InputEvent.CTRL_MASK));
        deleteMenu.setMnemonic('e');
        deleteMenu.setText("Delete");
        deleteMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteMenuActionPerformed(evt);
            }
        });

        jMenu7.add(deleteMenu);

        jMenu7.add(jSeparator8);

        findMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F, java.awt.event.InputEvent.CTRL_MASK));
        findMenu.setMnemonic('f');
        findMenu.setText("Find");
        findMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                findMenuActionPerformed(evt);
            }
        });

        jMenu7.add(findMenu);

        replaceMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Q, java.awt.event.InputEvent.CTRL_MASK));
        replaceMenu.setMnemonic('p');
        replaceMenu.setText("Replace");
        replaceMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                replaceMenuActionPerformed(evt);
            }
        });

        jMenu7.add(replaceMenu);

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
        objectiveMenu.setText("Objective/Likelihood");
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

        scatterPlotMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C, java.awt.event.InputEvent.CTRL_MASK));
        scatterPlotMenu.setMnemonic('c');
        scatterPlotMenu.setText("Scatterplots");
        scatterPlotMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                scatterPlotMenuActionPerformed(evt);
            }
        });

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

        identifiabilityTraceMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, java.awt.event.InputEvent.CTRL_MASK));
        identifiabilityTraceMenu.setMnemonic('d');
        identifiabilityTraceMenu.setText("Identifiability Trace");
        identifiabilityTraceMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                identifiabilityTraceMenuActionPerformed(evt);
            }
        });

        jMenu9.add(identifiabilityTraceMenu);

        indIDMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_I, java.awt.event.InputEvent.CTRL_MASK));
        indIDMenu.setMnemonic('i');
        indIDMenu.setText("Individual IDs");
        indIDMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                indIDMenuActionPerformed(evt);
            }
        });

        jMenu9.add(indIDMenu);

        nonparamMenu.setText("Nopnparametric Results");
        paramOutMenu.setText("Output Parameters");
        paramOutMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                paramOutMenuActionPerformed(evt);
            }
        });

        nonparamMenu.add(paramOutMenu);

        densityMenu.setText("Prob. of Measurement");
        densityMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                densityMenuActionPerformed(evt);
            }
        });

        nonparamMenu.add(densityMenu);

        meanMenu.setText("Posterior Mean");
        meanMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                meanMenuActionPerformed(evt);
            }
        });

        nonparamMenu.add(meanMenu);

        paramInMenu.setText("Initial Parameters");
        paramInMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                paramInMenuActionPerformed(evt);
            }
        });

        nonparamMenu.add(paramInMenu);

        jMenu9.add(nonparamMenu);

        jMenuBar1.add(jMenu9);

        jMenu4.setText("Table");
        jMenu2.setText("Default Table");
        jMenuItem10.setText("ID TIME DV PRED (Free format)");
        jMenuItem10.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem10ActionPerformed(evt);
            }
        });

        jMenu2.add(jMenuItem10);

        jMenuItem11.setText("ID TIME DV PRED (Exp. format)");
        jMenuItem11.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem11ActionPerformed(evt);
            }
        });

        jMenu2.add(jMenuItem11);

        jMenuItem12.setText("ID TIME DV IPRED (Free format)");
        jMenuItem12.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem12ActionPerformed(evt);
            }
        });

        jMenu2.add(jMenuItem12);

        jMenuItem13.setText("ID TIME DV IPRED (Exp. format)");
        jMenuItem13.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem13ActionPerformed(evt);
            }
        });

        jMenu2.add(jMenuItem13);

        jMenu4.add(jMenu2);

        FirstMenu.setText("First Row Only");
        FirstMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                FirstMenuActionPerformed(evt);
            }
        });

        jMenu4.add(FirstMenu);

        jMenuItem9.setText("Selected Dataset");
        jMenuItem9.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem9ActionPerformed(evt);
            }
        });

        jMenu4.add(jMenuItem9);

        jMenuBar1.add(jMenu4);

        jMenu1.setText("Plot");
        jMenu3.setText("Default Plots");
        jMenuItem1.setText("DV vs PRED");
        jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem1ActionPerformed(evt);
            }
        });

        jMenu3.add(jMenuItem1);

        jMenuItem2.setText("DV vs PRED by ID");
        jMenuItem2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem2ActionPerformed(evt);
            }
        });

        jMenu3.add(jMenuItem2);

        jMenuItem5.setText("PRED DV vs TIME");
        jMenuItem5.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem5ActionPerformed(evt);
            }
        });

        jMenu3.add(jMenuItem5);

        jMenuItem6.setText("PRED DV vs TIME by ID");
        jMenuItem6.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem6ActionPerformed(evt);
            }
        });

        jMenu3.add(jMenuItem6);

        jMenuItem3.setText("DV vs IPRED");
        jMenuItem3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem3ActionPerformed(evt);
            }
        });

        jMenu3.add(jMenuItem3);

        jMenuItem4.setText("DV vs IPRED by ID");
        jMenuItem4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem4ActionPerformed(evt);
            }
        });

        jMenu3.add(jMenuItem4);

        jMenuItem7.setText("IPRED DV vs TIME");
        jMenuItem7.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem7ActionPerformed(evt);
            }
        });

        jMenu3.add(jMenuItem7);

        jMenuItem8.setText("IPRED DV vs TIME by ID");
        jMenuItem8.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem8ActionPerformed(evt);
            }
        });

        jMenu3.add(jMenuItem8);

        jMenu1.add(jMenu3);

        dataMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_H, java.awt.event.InputEvent.CTRL_MASK));
        dataMenu.setMnemonic('d');
        dataMenu.setText("Entire Report Data");
        dataMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dataMenuActionPerformed(evt);
            }
        });

        jMenu1.add(dataMenu);

        jMenu1.add(jSeparator1);

        useMDAMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L, java.awt.event.InputEvent.CTRL_MASK));
        useMDAMenu.setMnemonic('l');
        useMDAMenu.setText("Use MDA Plotter");
        useMDAMenu.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                useMDAMenuActionPerformed(evt);
            }
        });

        jMenu1.add(useMDAMenu);

        useRMenu.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_G, java.awt.event.InputEvent.CTRL_MASK));
        useRMenu.setMnemonic('g');
        useRMenu.setText("Use R Language");
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
        gridBagConstraints.gridheight = 13;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 12, 12);
        jPanel1.add(jInternalFrame1, gridBagConstraints);

        getContentPane().add(jPanel1, java.awt.BorderLayout.CENTER);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jTextArea10KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextArea10KeyTyped
        if(jTextArea10.getText().length() == 100)
        {
            evt.consume();
            Toolkit.getDefaultToolkit().beep();
        }
    }//GEN-LAST:event_jTextArea10KeyTyped

    private void jTextArea9KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextArea9KeyTyped
        if(jTextArea9.getText().length() == 100)
        {
            evt.consume();
            Toolkit.getDefaultToolkit().beep();
        }       
    }//GEN-LAST:event_jTextArea9KeyTyped

    private void jTextArea8KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextArea8KeyTyped
        if(jTextArea8.getText().length() == 100)
        {
            evt.consume();
            Toolkit.getDefaultToolkit().beep();
        }  
    }//GEN-LAST:event_jTextArea8KeyTyped

    private void jTextField4KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextField4KeyTyped
        if(jTextField4.getText().length() == 20)
        {
            evt.consume();
            Toolkit.getDefaultToolkit().beep();
        }    
    }//GEN-LAST:event_jTextField4KeyTyped

    private void jTextField1KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextField1KeyTyped
        if(jTextField1.getText().length() == 20)
        {
            evt.consume();
            Toolkit.getDefaultToolkit().beep();
        }    
    }//GEN-LAST:event_jTextField1KeyTyped
    
    private void paramInMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_paramInMenuActionPerformed
        if(output != null && output.nonparamInTheta != null && output.nonparamInOmega != null)
            NonparamShow.initTable(this, output);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_paramInMenuActionPerformed

    private void meanMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_meanMenuActionPerformed
        if(output != null && output.nonparamMeanTheta != null && output.nonparamMeanOmega != null)
            NonparamShow.meanTable(this, output);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_meanMenuActionPerformed

    private void densityMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_densityMenuActionPerformed
        if(output != null && output.nonparamDensity != null)
            NonparamShow.densityTable(this, output);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_densityMenuActionPerformed

    private void paramOutMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_paramOutMenuActionPerformed
        if(output != null && output.nonparamOutTheta != null && output.nonparamOutOmega != null)
            new NonparamShow(this, output).setVisible(true);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_paramOutMenuActionPerformed

    private void identifiabilityTraceMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_identifiabilityTraceMenuActionPerformed
        if(output != null && output.methodCode.equals("id") &&  output.trace != null)
        {
            textArea.setText(output.trace);
            textArea.setCaretPosition(0);
            if(jobInfo != null)
                jInternalFrame1.setTitle("Identifiability Trace: Job-" + jobInfo.id);
            else
                jInternalFrame1.setTitle("Identifiability Trace");
            file = null;
            isChanged = false;
        }
        else
            JOptionPane.showMessageDialog(null, "The Identifiability trace is not available.",
                                          "Message Not Found Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_identifiabilityTraceMenuActionPerformed
    private class MyDocumentListener implements DocumentListener {
        public void insertUpdate(DocumentEvent e) {isChanged = true;}
        public void removeUpdate(DocumentEvent e) {isChanged = true;}
        public void changedUpdate(DocumentEvent e) {isChanged = true;}
    }
    
    private void jMenuItem13ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem13ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
        {
            String[] labels = {"ID", "TIME", "DV", "IPRED"};
            uw.rfpk.mda.Tabler.defaultTable(this, output.dataItems, output.dataAll, output.indIDs, labels, true);
        }
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem13ActionPerformed

    private void jMenuItem12ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem12ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
        {
            String[] labels = {"ID", "TIME", "DV", "IPRED"};
            uw.rfpk.mda.Tabler.defaultTable(this, output.dataItems, output.dataAll, output.indIDs, labels, false);
        }
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem12ActionPerformed

    private void jMenuItem11ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem11ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
        {
            String[] labels = {"ID", "TIME", "DV", "PRED"};
            uw.rfpk.mda.Tabler.defaultTable(this, output.dataItems, output.dataAll, output.indIDs, labels, true);
        }
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem11ActionPerformed

    private void jMenuItem10ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem10ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
        {
            String[] labels = {"ID", "TIME", "DV", "PRED"};
            uw.rfpk.mda.Tabler.defaultTable(this, output.dataItems, output.dataAll, output.indIDs, labels, false);
        }
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem10ActionPerformed

    private void jMenuItem9ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem9ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
            new uw.rfpk.mda.Tabler(this, output.dataItems, output.dataAll, output.indIDs);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem9ActionPerformed

    private void groupComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_groupComboBoxActionPerformed
        if(isInit) return;
        username = (String)groupComboBox.getSelectedItem();
        startID = null;
        startTime = null;
        keyWords = null;
        indexList = 0;
        lists = new Vector<String[][]>();
        showArchiveList(false, true);
    }//GEN-LAST:event_groupComboBoxActionPerformed

    private void findJobCancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_findJobCancelButtonActionPerformed
        findJobDialog.setVisible(false);
    }//GEN-LAST:event_findJobCancelButtonActionPerformed

    private void findJobOKButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_findJobOKButtonActionPerformed
        startID = jobIDTextField.getText();
        if(!startID.equals("") && !Utility.isPosIntNumber(startID))
        {
            JOptionPane.showMessageDialog(null, "Job ID must be a positive integer",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            startID = null;
            return;
        }
        if(startID.equals("")) startID = null;
        GregorianCalendar calendar = new GregorianCalendar();
        calendar.set(Integer.parseInt((String)yComboBox.getSelectedItem()),
                     Integer.parseInt((String)mComboBox.getSelectedItem()) - 1,
                     Integer.parseInt((String)dComboBox.getSelectedItem()));
        startTime = String.valueOf(calendar.getTimeInMillis() / 1000);
        keyWords = keyWordTextField.getText().trim().replaceAll(",", " ");
        if(keyWords.equals("")) keyWords = null;
        findJobDialog.setVisible(false);
        indexList = 0;
        lists = new Vector<String[][]>();
        showArchiveList(false, true);
    }//GEN-LAST:event_findJobOKButtonActionPerformed

    private void findJobButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_findJobButtonActionPerformed
        reportDialog.setVisible(false);
        timer.stop();
        jobIDTextField.setText("");
        GregorianCalendar calendar = new GregorianCalendar();
        int month = calendar.get(Calendar.MONTH);
        int day = calendar.get(Calendar.DATE);
        int year = calendar.get(Calendar.YEAR);
        String[] years = new String[year - 2003];
        for(int i = 0; i < years.length; i++)
            years[i] = String.valueOf(year - i);
        mComboBox.setSelectedIndex(month);
        dComboBox.setSelectedIndex(day - 1);
        yComboBox.setModel(new javax.swing.DefaultComboBoxModel(years));
        keyWordTextField.setText("");
        findJobDialog.setSize(310, 252);
        findJobDialog.setVisible(true);
    }//GEN-LAST:event_findJobButtonActionPerformed

    private void jMenuItem8ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem8ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
            DefaultPlot.plotPREDDVvsTIME(output.dataItems, output.dataAll, output.indIDs, "IPRED", true);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem8ActionPerformed

    private void jMenuItem7ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem7ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
            DefaultPlot.plotPREDDVvsTIME(output.dataItems, output.dataAll, output.indIDs, "IPRED", false);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem7ActionPerformed

    private void jMenuItem6ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem6ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
            DefaultPlot.plotPREDDVvsTIME(output.dataItems, output.dataAll, output.indIDs, "PRED", true);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem6ActionPerformed

    private void jMenuItem5ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem5ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
            DefaultPlot.plotPREDDVvsTIME(output.dataItems, output.dataAll, output.indIDs, "PRED", false);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem5ActionPerformed

    private void jMenuItem4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem4ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
            DefaultPlot.plotDVvsPRED(output.dataItems, output.dataAll, output.indIDs, "IPRED", true);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem4ActionPerformed

    private void jMenuItem3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem3ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
            DefaultPlot.plotDVvsPRED(output.dataItems, output.dataAll, output.indIDs, "IPRED", false);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem3ActionPerformed

    private void jMenuItem2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem2ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
            DefaultPlot.plotDVvsPRED(output.dataItems, output.dataAll, output.indIDs, "PRED", true);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem2ActionPerformed

    private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem1ActionPerformed
        if(output != null && output.dataItems != null && output.dataAll != null && output.indIDs != null)
            DefaultPlot.plotDVvsPRED(output.dataItems, output.dataAll, output.indIDs, "PRED", false);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_jMenuItem1ActionPerformed

    private void FirstMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_FirstMenuActionPerformed
        if(dataFirst != null)
        {
            textArea.setText(dataFirst);
            textArea.setCaretPosition(0);
            String title = "Report Data(First row only): Job-" + output.jobId;
            jInternalFrame1.setTitle(title);
            isChanged = false;
        }
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_FirstMenuActionPerformed

    private void indIDMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_indIDMenuActionPerformed
        if(output.indIDs == null)
        {
            JOptionPane.showMessageDialog(null, "Individual IDs are not available.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return;
        }
        Vector<String> indIDs = new Vector<String>();
        indIDs.add(output.indIDs[0]);
        int n = output.indIDs.length;
        for(int i = 1; i < n; i++)
            if(!output.indIDs[i].equals(output.indIDs[i - 1]))
                indIDs.add(output.indIDs[i]);
        n = indIDs.size();
        String[][] indID = new String[n][2];
        for(int i = 0; i < n; i++)
        {
            indID[i][0] = String.valueOf(i + 1);
            indID[i][1] = (String)indIDs.get(i);
        }
        jTable3.setModel(new DefaultTableModel(indID, new String[]{"Order", "ID"}));
        indIDDialog.setSize(200, 200);
        indIDDialog.setVisible(true);
    }//GEN-LAST:event_indIDMenuActionPerformed

    private void dataMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataMenuActionPerformed
        if(dataBlock != null && output != null)
        {
            textArea.setText(dataBlock);
            textArea.setCaretPosition(0);
            String title = "Report Data: Job-" + output.jobId;
            jInternalFrame1.setTitle(title);
            isChanged = false;
        }
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_dataMenuActionPerformed

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        replaceDialog.dispose();
    }//GEN-LAST:event_jButton2ActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        String text = textArea.getText();
        String string1 = jTextField2.getText().trim();
        String string2 = jTextField5.getText().trim();
        if(string1.equals("") || text.toLowerCase().indexOf(string1.toLowerCase()) == -1)
        {
            JOptionPane.showMessageDialog(null, "The string '" + string1 + "' was not found in the text.", 
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return;
        }
        if(!string1.equals(""))
        {
            int pos = 0;
            Vector<Integer> positions = new Vector<Integer>();            
            while ((pos = text.toLowerCase().indexOf(string1.toLowerCase(), pos)) >= 0)
            {
                positions.add(new Integer(pos));
                pos += string1.length();
            }
            for(int i = 0; i < positions.size(); i++)
            {
                int start = ((Integer)positions.get(i)).intValue() + 
                             i * (string2.length() - string1.length());
                textArea.select(start, start + string1.length());
                textArea.replaceSelection(string2);
            }
        }
        replaceDialog.dispose();
    }//GEN-LAST:event_jButton1ActionPerformed

    private void replaceMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_replaceMenuActionPerformed
        replaceDialog.setSize(220, 150);
        replaceDialog.setVisible(true);
    }//GEN-LAST:event_replaceMenuActionPerformed

    private void deleteMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteMenuActionPerformed
        textArea.replaceSelection("");
    }//GEN-LAST:event_deleteMenuActionPerformed

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        new LikelihoodShow(this, false, output).setVisible(true);
    }//GEN-LAST:event_jButton3ActionPerformed

    private void versionDialogWindowClosing(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_versionDialogWindowClosing
        if(!isVersionListOn) return;
        if(listType.equals("model"))
        {
            if(modelRadioButtonState == 1)
                jRadioButton1.doClick();
            if(modelRadioButtonState == 3)
                jRadioButton3.setSelected(true);
        }
        if(listType.equals("data"))
        {
            if(datasetRadioButtonState == 1)
                jRadioButton4.doClick();
            if(datasetRadioButtonState == 3)
                jRadioButton6.setSelected(true);
        }
        isVersionListOn = false;
    }//GEN-LAST:event_versionDialogWindowClosing

    private void reportDialogWindowClosing(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_reportDialogWindowClosing
        if(isVersionListOn || isDiff) return;
        if(listType.equals("model"))
        {
            if(modelRadioButtonState == 1)
                jRadioButton1.setSelected(true);
            if(modelRadioButtonState == 3)
                jRadioButton3.setSelected(true);
            versionRadioButton.setSelected(true);
            modelID = null;
        }
        if(listType.equals("data"))
        {
            if(datasetRadioButtonState == 1)
                jRadioButton4.setSelected(true);
            if(datasetRadioButtonState == 3)
                jRadioButton6.setSelected(true);
            versionRadioButton.setSelected(true);
            datasetID = null;
        }
        if(listType.equals("job"))
        {
            startID = null;
            startTime = null;
            keyWords = null;
        }
        username = myName;
        isInit = true;
        groupComboBox.setSelectedItem(myName);
        isInit = false;
        reportDialog.setFocusableWindowState(true);
    }//GEN-LAST:event_reportDialogWindowClosing

    private void scatterPlotMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_scatterPlotMenuActionPerformed
        scatterPlot();
    }//GEN-LAST:event_scatterPlotMenuActionPerformed

    private void textAreaMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_textAreaMouseClicked
        removeHighlights(textArea);       
    }//GEN-LAST:event_textAreaMouseClicked

    private void textAreaKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_textAreaKeyPressed
        if(evt.getKeyCode() == 114)
        {
            if(indexPosition >= positions.size() - 1)
                indexPosition -= positions.size();
            textArea.setCaretPosition(((Integer)positions.get(++indexPosition)).intValue());
        }
    }//GEN-LAST:event_textAreaKeyPressed

    private void jRadioButton12ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton12ActionPerformed
        jobMethodCode = "mi";
    }//GEN-LAST:event_jRadioButton12ActionPerformed

    private void jRadioButton11ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton11ActionPerformed
        jobMethodCode = "ad";
    }//GEN-LAST:event_jRadioButton11ActionPerformed

    private void jRadioButton10ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton10ActionPerformed
        jobMethodCode = "gr";
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
            warningMessageDialog.setVisible(true);
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
            JOptionPane.showMessageDialog(null, "MDA could not start R.\n See instructions on SPK Login web page.", "IOException", JOptionPane.ERROR_MESSAGE);
        }
    }//GEN-LAST:event_useRMenuActionPerformed

    private void useMDAMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_useMDAMenuActionPerformed
        String dataText = textArea.getText();
        if(!dataText.trim().equals(""))
            new PlotTool(dataText, false);
        else
            JOptionPane.showMessageDialog(null, "Report data were not found.", "Input Error", 
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_useMDAMenuActionPerformed

    private void jRadioButton9ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton9ActionPerformed
        jobMethodCode = "vl";
    }//GEN-LAST:event_jRadioButton9ActionPerformed

    private void jRadioButton8ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton8ActionPerformed
        jobMethodCode = "ml";
    }//GEN-LAST:event_jRadioButton8ActionPerformed

    private void jRadioButton7ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton7ActionPerformed
        jobMethodCode = method;
    }//GEN-LAST:event_jRadioButton7ActionPerformed

    private void traceMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_traceMenuActionPerformed
        if(output != null && !output.methodCode.equals("id") &&  output.trace != null)
        {
            textArea.setText(output.trace);
            textArea.setCaretPosition(0);
            if(jobInfo != null)
                jInternalFrame1.setTitle("Optimization Trace: Job-" + jobInfo.id);
            else
                jInternalFrame1.setTitle("Optimization Trace");
            file = null;
            isChanged = false;
        }
        else
            JOptionPane.showMessageDialog(null, "The optimization trace is not available.",
                                          "Message Not Found Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_traceMenuActionPerformed

    private void dataLibRButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataLibRButtonActionPerformed
        isDiff = true;
        isLeft = false;
        isWizard = false;
        isLibrary = true;
        username = "librarian";
        dataArchive();
    }//GEN-LAST:event_dataLibRButtonActionPerformed

    private void dataLibLButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataLibLButtonActionPerformed
        isDiff = true;
        isLeft = true;
        isWizard = false;        
        isLibrary = true;
        username = "librarian";
        dataArchive();
    }//GEN-LAST:event_dataLibLButtonActionPerformed

    private void JobExamplesButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_JobExamplesButtonActionPerformed
        username = "librarian";
        listType = "job";
        isLibrary = true;
        indexList = 0;
        lists = new Vector<String[][]>();
        showArchiveList(false, true);        
    }//GEN-LAST:event_JobExamplesButtonActionPerformed

    private void DatasetLibraryButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_DatasetLibraryButtonActionPerformed
        username = "librarian";
        jobRadioButton.setEnabled(true);
        isDiff = false;
        isWizard = false;        
        isLibrary = true;
        dataArchive();        
    }//GEN-LAST:event_DatasetLibraryButtonActionPerformed

    private void HelpButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_HelpButtonActionPerformed
/*
        if(!isOnline)
            new Help("Instructions for Using Model Design Agent", 
                     GettingStarted.class.getResource("/uw/rfpk/mda/nonmem/help/MDAHelp.html"));
        else
            Utility.openURL("https://" + server.getHost() + ":" + server.getPort() + "/user/help/MDAHelp.jsp");
*/  
    }//GEN-LAST:event_HelpButtonActionPerformed

    private void modelLibRButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_modelLibRButtonActionPerformed
        isDiff = true;
        isLeft = false;
        isWizard = false;        
        isLibrary = true;
        username = "librarian";
        modelArchive();        
    }//GEN-LAST:event_modelLibRButtonActionPerformed

    private void modelLibLButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_modelLibLButtonActionPerformed
        isDiff = true;
        isLeft = true;
        isWizard = false;        
        isLibrary = true;
        username = "librarian";
        modelArchive();     
    }//GEN-LAST:event_modelLibLButtonActionPerformed

    private void ModelLibraryButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ModelLibraryButtonActionPerformed
        username = "librarian";
        jobRadioButton.setEnabled(true);
        isDiff = false;
        isWizard = false;        
        isLibrary = true;
        modelArchive();        
    }//GEN-LAST:event_ModelLibraryButtonActionPerformed
    
    private void helpButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_helpButtonActionPerformed
        diffHelpDialog.setSize(320, 180);
        diffHelpDialog.setVisible(true);
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
        if(string == null || string.trim().equals(""))
            return;
        String text = textArea.getText();
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
            positions = new Vector<Integer>();
            
            // Search for string
            while ((pos = text.toLowerCase().indexOf(string.toLowerCase(), pos)) >= 0) {
             
                hilite.addHighlight(pos, pos + string.length(), 
                                    new DefaultHighlighter.DefaultHighlightPainter(Color.YELLOW));
                positions.add(new Integer(pos));
                pos += string.length();
            }
            textArea.setCaretPosition(((Integer)positions.get(0)).intValue());
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
        deltaLines = new Vector<Integer>(); 
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
        versionRadioButton.setSelected(true);
        jobRadioButton.setEnabled(false);
        isDiff = true;
        isLeft = false;
        isWizard = false;
        isLibrary = false;
        dataArchive();
    }//GEN-LAST:event_dataRButtonActionPerformed

    private void modelRButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_modelRButtonActionPerformed
        versionRadioButton.setSelected(true);
        jobRadioButton.setEnabled(false);
        isDiff = true;
        isLeft = false;
        isWizard = false;        
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
        versionRadioButton.setSelected(true);
        jobRadioButton.setEnabled(false);
        isDiff = true;
        isLeft = true;
        isWizard = false;
        isLibrary = false;
        dataArchive();
    }//GEN-LAST:event_dataLButtonActionPerformed

    private void modelLButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_modelLButtonActionPerformed
        versionRadioButton.setSelected(true);
        jobRadioButton.setEnabled(false);
        isDiff = true;
        isLeft = true;
        isWizard = false;        
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
        diffDialog.setVisible(true);
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
        if(recentModel.name != null && recentModel.text.trim().equals(modelArchive.text.trim()))
        {
            modelRadioButtonState = 3;
            jTextField1.setText(recentModel.name);
            jTextArea8.setText(recentModel.description);
            jTextField3.setText(recentModel.version);
            jTextArea6.setText(recentModel.log);
            jTextArea6.setEditable(false);
            modelArchive.id = recentModel.id;
            return;
        }
        jTextField1.setEditable(false);      
        listType = "model";
        isLibrary = false;
        showVersions = true;
        indexList = 0;
        lists = new Vector<String[][]>();
        username = myName;
        jobRadioButton.setEnabled(false);
        showArchiveList(false, true);
    }//GEN-LAST:event_jRadioButton3ActionPerformed

    private void jRadioButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton2ActionPerformed
        int option = 1;
        if(recentModel.name != null)
            option = JOptionPane.showConfirmDialog(null, "Is this a new version of this recently downloaded model?" +
                                                   "\nModel Name:     " + recentModel.name +
                                                   "\nModel Version:  " + recentModel.version,
                                                   "Question", JOptionPane.YES_NO_CANCEL_OPTION);
        if(option == 0)
        {
            jTextField1.setEditable(false);
            jTextField1.setText(recentModel.name);
            jTextArea8.setText(recentModel.description);
            jTextField3.setText(String.valueOf(Integer.parseInt(server.getLastVersion(recentModel.id, "model")) + 1));
            modelArchive.id = recentModel.id;
            jTextArea6.setText("");
            jTextArea6.setEditable(true);
            modelRadioButtonState = 2;
        }
        else if(option == 1)
        {
            jTextField1.setEditable(false);
            listType = "model";
            isLibrary = false;
            showVersions = false;
            indexList = 0;
            lists = new Vector<String[][]>();
            username = myName;
            jobRadioButton.setEnabled(false);
            showArchiveList(false, true);      
        }
        else
        {
            if(modelRadioButtonState == 1)
                jRadioButton1.doClick();
            if(modelRadioButtonState == 3)
                jRadioButton3.setSelected(true);            
        }
    }//GEN-LAST:event_jRadioButton2ActionPerformed

    private void jRadioButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton1ActionPerformed
        jTextField1.setEditable(true); 
        jTextField1.setText("");
        jTextArea8.setText("");
        jTextField3.setText("1");
        jTextArea6.setText("");
        jTextArea6.setEditable(true);
        modelRadioButtonState = 1;
    }//GEN-LAST:event_jRadioButton1ActionPerformed

    private void nextButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_nextButtonActionPerformed
        ++indexList;
        showArchiveList(false, false);
        if(indexList != 0)
            previousButton.setEnabled(true);
    }//GEN-LAST:event_nextButtonActionPerformed

    private void previousButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_previousButtonActionPerformed
        --indexList;
        showArchiveList(false, false); 
        if(indexList == 0)
            previousButton.setEnabled(false);
        nextButton.setEnabled(true);
    }//GEN-LAST:event_previousButtonActionPerformed

    private void jTable2MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTable2MouseClicked
        if(listType.equals("job"))
            return;
        int index = jTable2.getSelectedRow();
        String version = (String)jTable2.getModel().getValueAt(index,  0);
        String log = (String)jTable2.getModel().getValueAt(index,  3);
        String title = archiveName + "." + version;
        String archive = server.getArchive(version);
        if(archive == null)
            return;        
        if(getArchive)
        {
//            if(!isDiff && !isWizard)
//                saveFile();            
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
                        if(!isLibrary && username.equals(myName))
                        {
                            recentModel.id = archiveId;
                            recentModel.name = archiveName;
                            recentModel.text = archive;
                            recentModel.version = version;
                            recentModel.description = archiveDescription;
                            recentModel.log = log;
                        }
                        jInternalFrame1.setTitle(title);
                        textArea.setText(archive);
                        textArea.setCaretPosition(0);                        
                        file = null;
                        isChanged = false;
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
                            textL = XMLReader.parseDataXML(archive, true).replaceAll(ls, "\n");
                            if(textL != null)
                            {
                                jTextArea3.setText(textL);
                                jTextArea3.setCaretPosition(0);
                            }
                        }
                        else
                        {
                            jInternalFrame3.setTitle(title);
                            textR = XMLReader.parseDataXML(archive, true).replaceAll(ls, "\n");
                            if(textR != null)
                            {
                                jTextArea4.setText(textR);
                                jTextArea4.setCaretPosition(0);
                            }
                        }
                    }
                    else
                    {
                        if(isWizard)
                        {
                            if((iterator.analysis.equals("population") ||  iterator.analysis.equals("two-stage") ||
                                iterator.analysis.equals("nonparametric")) && archive.indexOf(">ID</value>") == -1)
                            {
                                JOptionPane.showMessageDialog(null, "Data item 'ID' was not found in the dataset." +
                                                              "\nIt is required for the population analysis.",
                                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                                iterator.setIsNewData(false);
                                versionDialog.setVisible(false);
                                reportDialog.setVisible(false);
                                return;
                            }                            
                            iterator.setDataXML(archive, 1);
                            iterator.setDatasetName(archiveName, 1);
                            iterator.setIsNewData(true);
                        }
                        else
                        {
                            String text = XMLReader.parseDataXML(archive,true);
                            if(text != null)
                            {
                                textArea.setText(text);
                                textArea.setCaretPosition(0);
                                jInternalFrame1.setTitle(title);
                                file = null;
                                isChanged = false;
                            }
                        }
                        if(!isLibrary && username.equals(myName))
                        {
                            recentDataset.id = archiveId;
                            recentDataset.name = archiveName;
                            recentDataset.text = archive;                    
                            recentDataset.version = version;
                            recentDataset.description = archiveDescription;
                            recentDataset.log = log;
                        }
                    }
                }
            }
        }
        else
        {
            isVersionListOn = false;
            if(listType.equals("model"))
            {
                String spkInput = textArea.getText();
                int indexModel = spkInput.lastIndexOf("<?xml ", spkInput.indexOf("<spkmodel"));
                String model = XMLReader.getModelArchive(spkInput.substring(indexModel)).trim();
//                int index2 = spkInput.indexOf("<spkmodel");        
//                String model = XMLReader.getModelArchive(spkInput.substring(index2 - 22)).trim();
                if(model.equals(archive))
                {
                    jTextField1.setText(modelArchive.name);
                    jTextArea8.setText(modelArchive.description);
                    jTextField3.setText(version);
                    jTextArea6.setText(log);
                    jTextArea6.setEditable(true);
                    modelRadioButtonState = 3;
                }
                else
                {
                    JOptionPane.showMessageDialog(null, "The version of the model you selected" + 
                                                  " is different from the model in the input file.", 
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
//                    return;   // Windows frozen
                }
            }
            if(listType.equals("data"))
            {
                String spkInput = textArea.getText();
                int indexData = spkInput.lastIndexOf("<?xml ", spkInput.indexOf("<spkdata"));
                int indexModel = spkInput.lastIndexOf("<?xml ", spkInput.indexOf("<spkmodel"));
                String dataset = spkInput.substring(indexData, indexModel).trim();
//                int index1 = spkInput.indexOf("<spkdata");
//                int index2 = spkInput.indexOf("<spkmodel");        
//                String dataset = spkInput.substring(index1 - 22, index2 - 22).trim();
                if(dataset.equals(archive))
                {
                    jTextField4.setText(dataArchive.name);
                    jTextArea9.setText(dataArchive.description);                    
                    jTextField6.setText(version);
                    jTextArea7.setText(log);
                    jTextArea7.setEditable(true);
                    datasetRadioButtonState = 3;
                }
                else
                {
                    JOptionPane.showMessageDialog(null, "The version of the dataset you selected" + 
                                                  " is different from the dataset in the input file.", 
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
//                    return;   // Windows frozen
                }                
            }
        }
        versionDialog.setVisible(false);
        reportDialog.setVisible(false);
    }//GEN-LAST:event_jTable2MouseClicked

    private void DataArchiveButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_DataArchiveButtonActionPerformed
        username = myName;
        isInit = true;
        groupComboBox.setSelectedItem(myName);
        versionRadioButton.setSelected(true);
        isInit = false;
        isDiff = false;
        isWizard = false;
        isLibrary = false; 
        dataArchive();
    }//GEN-LAST:event_DataArchiveButtonActionPerformed

    /** Reload data from the database.
     * @param isLib a flag to specify if reloading data from the library.
     */
    public void reloadData(boolean isLib)
    {
        isWizard = true;
        isDiff = false;
        isLibrary = isLib;
        dataArchive();
    }
        
    private void dataArchive()
    {
        listType = "data"; 
        showVersions = true;
        getArchive = true;
        indexList = 0;
        lists = new Vector<String[][]>();
        isInit = true;
        versionRadioButton.setSelected(true);
        groupComboBox.setSelectedItem(myName);
        isInit = false;
        showArchiveList(false, true);        
    }
    
    private void ModelArchiveButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ModelArchiveButtonActionPerformed
        username = myName;
        isInit = true;
        groupComboBox.setSelectedItem(myName);
        versionRadioButton.setSelected(true);
        isInit = false;
        isDiff = false;
        isWizard = false;        
        isLibrary = false;
        modelArchive();
    }//GEN-LAST:event_ModelArchiveButtonActionPerformed

    private void modelArchive()
    {
        listType = "model";
        showVersions = true;
        getArchive = true;
        indexList = 0; 
        lists = new Vector<String[][]>();
        isInit = true;
        versionRadioButton.setSelected(true);
        groupComboBox.setSelectedItem(myName);
        isInit = false;
        showArchiveList(false, true);
    }
    
    private void jRadioButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton4ActionPerformed
        jTextField4.setEditable(true);
        jTextField4.setText("");
        jTextArea9.setText(""); 
        jTextField6.setText("1");
        jTextArea7.setText("");
        jTextArea7.setEditable(true);
        datasetRadioButtonState = 1;
    }//GEN-LAST:event_jRadioButton4ActionPerformed

    private void jRadioButton6ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton6ActionPerformed
        jTextField4.setEditable(false);
        if(recentDataset.name != null &&
            recentDataset.text.substring(recentDataset.text.indexOf("<spkdata "))
           .equals(dataArchive.text.substring(dataArchive.text.indexOf("<spkdata "))))
//           XMLReader.parseDataXML(recentDataset.text.substring(recentDataset.text.indexOf("<spkdata ")), true)
//           .equals(XMLReader.parseDataXML(dataArchive.text.substring(dataArchive.text.indexOf("<spkdata ")), true)))
        {
            datasetRadioButtonState = 3;
            jTextField4.setText(recentDataset.name);
            jTextArea9.setText(recentDataset.description);
            jTextField6.setText(recentDataset.version);
            jTextArea7.setText(recentDataset.log);
            jTextArea7.setEditable(false);
            dataArchive.id = recentDataset.id;
            return;   
        }
        listType = "data";
        isLibrary = false;
        showVersions = true;
        indexList = 0;
        lists = new Vector<String[][]>();
        username = myName;
        jobRadioButton.setEnabled(false);
        showArchiveList(false, true);
    }//GEN-LAST:event_jRadioButton6ActionPerformed

    private void jRadioButton5ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton5ActionPerformed
        int option = 1;
        if(recentDataset.name != null)
            option = JOptionPane.showConfirmDialog(null, "Is this a new version of this recently downloaded dataset?" +
                                                   "\nDataset Name:     " + recentDataset.name +
                                                   "\nDataset Version:  " + recentDataset.version,
                                                   "Question", JOptionPane.YES_NO_CANCEL_OPTION);
        if(option == 0)
        {
            jTextField4.setEditable(false);            
            jTextField4.setText(recentDataset.name);
            jTextArea9.setText(recentDataset.description);
            jTextField6.setText(String.valueOf(Integer.parseInt(server.getLastVersion(recentDataset.id, "data")) + 1));
            dataArchive.id = recentDataset.id;
            jTextArea7.setText("");
            jTextArea7.setEditable(true);
            datasetRadioButtonState = 2;            
        }
        else if(option == 1)
        {
            jTextField4.setEditable(false);      
            listType = "data";
            isLibrary = false;
            showVersions = false;
            indexList = 0;
            lists = new Vector<String[][]>();
            username = myName;
            jobRadioButton.setEnabled(false);
            showArchiveList(false, true);           
        }
        else
        {
            if(datasetRadioButtonState == 1)
                jRadioButton4.doClick();
            if(datasetRadioButtonState == 3)
                jRadioButton6.setSelected(true);            
        }
    }//GEN-LAST:event_jRadioButton5ActionPerformed

    private void jTable1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTable1MouseClicked
        int index = jTable1.getSelectedRow();
        if(index == -1)
            return; 
        long id = Long.parseLong(((String[][])lists.get(indexList))[index][0].replaceFirst("s", ""));
        if(listType.equals("job") || modelID != null || datasetID != null)
        {
            timer.stop();
            startID = null;
            startTime = null;
            keyWords = null;
            jobInfo = new JobInfo(this, id, isLibrary, false);
            reportDialog.setVisible(false);
            return;
        }
        else if(getArchive)
        {
            if(jobRadioButton.isSelected())
            {
                if(listType.equals("model")) modelID = String.valueOf(id);
                if(listType.equals("data")) datasetID = String.valueOf(id);
                indexList = 0;
                lists = new Vector<String[][]>();
                showArchiveList(false, true);
            }
            else
            {
                modelID = null;
                datasetID = null;
                archiveName = ((String[][])lists.get(indexList))[index][1];
                archiveId = id;
                archiveDescription = ((String[][])lists.get(indexList))[index][4];
                versionDialog.setTitle(archiveName);
                showVersionList(id);
            }
        }
        else 
        {
            if(listType.equals("model"))
            {
                modelArchive.name = (String)(jTable1.getModel().getValueAt(index, 1));
                modelArchive.description = (String)(jTable1.getModel().getValueAt(index, 4));
                modelArchive.id = id; 
                if(showVersions)
                {
                    isVersionListOn = true;
                    showVersionList(id);
                }
                else
                {
                    String[][] modelVersions = server.getVersions(id, listType); 
                    if(modelVersions != null)
                    {
                        jTextField1.setText(modelArchive.name);
                        jTextArea8.setText(modelArchive.description);                        
                        jTextField3.setText(String.valueOf(modelVersions.length + 1));
                    }
                    modelRadioButtonState = 2;
                    jTextArea6.setText("");
                    jTextArea6.setEditable(true);
                    reportDialog.setVisible(false);
                }
            }
            if(listType.equals("data"))
            {
                dataArchive.name = (String)(jTable1.getModel().getValueAt(index, 1));
                dataArchive.description = (String)(jTable1.getModel().getValueAt(index, 4));
                dataArchive.id = id;                                  
                if(showVersions)
                {
                    isVersionListOn = true;
                    showVersionList(id);
                }
                else
                {
                    String[][] datasetVersions = server.getVersions(id, listType);
                    if(datasetVersions != null)
                    {
                        jTextField4.setText(dataArchive.name); 
                        jTextArea9.setText(dataArchive.description);                        
                        jTextField6.setText(String.valueOf(datasetVersions.length + 1));
                    }
                    jTextArea7.setText("");
                    jTextArea7.setEditable(true);
                    datasetRadioButtonState = 2;    
                    reportDialog.setVisible(false);  
                }
            }
        }
    }//GEN-LAST:event_jTable1MouseClicked

    private void summaryMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_summaryMenuActionPerformed
//        saveFile();
        String summary = Summary.makeSummary(output, isOnline, isDeveloper, jobMethodCode, methodTable);
        if(summary != null)
        {
            textArea.setText(summary);
            textArea.setCaretPosition(0);
            jInternalFrame1.setTitle("Summary Report: Job-" + jobInfo.id);
            file = null;
            isChanged = false;
        }
    }//GEN-LAST:event_summaryMenuActionPerformed
    
    private void ReadOutputButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ReadOutputButtonActionPerformed
        if(jobInfo != null && jobInfo.subReport != null && jobInfo.subReport.equals("Parameter-All"))
        {
            String text = textArea.getText();
            int index = text.lastIndexOf("<?xml ");
            dataBlock = ParameterAll.getParameterAll(text.substring(index), text.substring(0, index));
            dataMenu.doClick();
        }
        else
            readOutput(textArea.getText());
    }//GEN-LAST:event_ReadOutputButtonActionPerformed

    private void GetReportButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_GetReportButtonActionPerformed
        username = myName;
        isInit = true;
        getArchive = true; 
        groupComboBox.setSelectedItem(myName);
        versionRadioButton.setSelected(true);
        isInit = false;
        listType = "job";
        isLibrary = false;
        isDiff = false;
        indexList = 0;
        lists = new Vector<String[][]>();
        showArchiveList(false, true);
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

        if(text.indexOf("is_restart=\"yes\"", text.indexOf("<pop_analysis ") + 1) != -1 ||
           text.indexOf("is_restart=\"yes\"", text.indexOf("<ind_analysis ") + 1) != -1)
        {
            JOptionPane.showMessageDialog(null, "This is a warm start job.  It must start with a parent job." +
                                          "\nIf you want to submit a warm start job, click 'My Jobs'" +
                                          "\nbutton, then find the parent job of the warm start job.",
                                          "Job Submission Error", 
                                          JOptionPane.ERROR_MESSAGE);
            return;            
        }
        jobMethodClass = "";
        submitJob(text);
    }//GEN-LAST:event_SubmitJobButtonActionPerformed

   /** submit a likelihood evaluation job.
    * @param text SPK input.
    */
    protected void likelihoodJob(String text)
    {
        jobMethodClass = "";        

        // Get report
        String[] reports = server.getOutput(jobId).getProperty("report").split("<spkreport");
        
        // Use the last report
        if(reports.length > 2)
            JOptionPane.showMessageDialog(null, "The parent job has multiple reports.\n" +
                                          "The final estimates of the parameters from\n" +
                                          "the last report will be used to initialze this job.");
        String report = "<?xml version=\"1.0\">\n<spkreport" + reports[reports.length - 1];
        if(report.indexOf("<error_message>") != -1)
        {
            JOptionPane.showMessageDialog(null, "The parent job, Job ID = " + jobId + ", has error.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return;
        }
        hasSimulation = text.indexOf("<simulation ") != -1;
        text = Likelihood.changeInput(text, report, jobId, isLibrary);
        textArea.setText(text);
        textArea.setCaretPosition(0);
        isChanged = false;
        jobMethodClass = "le";

        submitJob(text);        
    }
        
   /** submit a non-likelihood evaluation job.
    * @param text SPK input.
    */
    protected void populationJob(String text)
    {
        textArea.setText(text);
        textArea.setCaretPosition(0);
        isChanged = false;
        jobMethodClass = "ts";

        submitJob(text);                
    }
    
    private void submitJob(String text)
    {     
        // Find method
        jobMethodCode = "fo";
        if(!jobMethodClass.equals("le"))
        {
            int beginIndex, endIndex;
            if(text.indexOf("<ind_analysis") != -1)
            {
                beginIndex = text.indexOf("<ind_analysis ");
                if(text.indexOf("is_estimation=\"yes\"", beginIndex) != -1)
                    jobMethodCode = "ia";
                else if(text.indexOf("is_identifiability=\"yes\"") != -1)
                {
                    jobMethodCode = "id";
                    method = "id";
                }
                else if(text.indexOf("<simulation ") != -1)
                    jobMethodCode = "so";
                else
                {
                    JOptionPane.showMessageDialog(null, "The text is not a SPK input file",
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
                    else if(approximation.equals("std_two_stage")) method = "s2";
                    else if(approximation.equals("iterative_two_stage")) method = "i2";
                    else if(approximation.equals("global_two_stage")) method = "g2";
                    else if(approximation.equals("map_std_two_stage")) method = "sm";
                    else if(approximation.equals("map_iterative_two_stage")) method = "im";
                    else if(approximation.equals("map_global_two_stage")) method = "gm";
                    else if(approximation.equals("nonparametric"))
                    {
                        int index = text.indexOf("measure_points_in");
                        String nonparam = text.substring(index, text.indexOf("/>", index));
                        index = nonparam.indexOf("auto_generate_method=") + 22;
                        String methodString = nonparam.substring(index, nonparam.indexOf("\"", index));
                        if(methodString.equals("random_uniform")) method = "un";
                        if(methodString.equals("grid")) method = "gn";
                    }
                    if(!method.equals("")) jobMethodCode = method;
                }
                else if(analysis.indexOf("is_estimation=\"no\"") != -1 && text.indexOf("<simulation ") != -1)
                    jobMethodCode = "so";   
                else
                {
                    JOptionPane.showMessageDialog(null, "Neither estimation nor simulation is included in this job." +
                                                  "\nIf you want to submit a likelihood evaluation only job, click" +
                                                  "\n'My Jobs' button, then select a job to use as the parent job.",
                                                  "Job Submission Error", JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }
            else
            {
                JOptionPane.showMessageDialog(null, "Neither population nor individual analysis is included in this job.",
                                              "Job Submission Error", JOptionPane.ERROR_MESSAGE);
                return;          
            }            
        }

        // Collect archive information
        getArchive = false;
        int indexData = text.lastIndexOf("<?xml ", text.indexOf("<spkdata"));
        int indexModel = text.lastIndexOf("<?xml ", text.indexOf("<spkmodel"));
        String dataset = text.substring(indexData, indexModel).trim();
        String model = XMLReader.getModelArchive(text.substring(indexModel));
//        int index1 = text.indexOf("<spkdata");
//        int index2 = text.indexOf("<spkmodel");
//        String dataset = text.substring(index1 - 22, index2 - 22);
//        String model = XMLReader.getModelArchive(text.substring(index2 - 22));         
        modelArchive = new ArchiveInfo();
        dataArchive = new ArchiveInfo();
        modelArchive.text = model;
        dataArchive.text = dataset;
        jTabbedPane1.removeAll(); 
        if(jobMethodClass == "le")
        {
            jRadioButton3.setSelected(true);
            modelRadioButtonState = 3;
            jTextField1.setText(jobInfo.modelName);
            jTextArea8.setText(jobInfo.modelAbstract);
            jTextField3.setText(jobInfo.modelVersion);
            modelArchive.id = jobInfo.modelId;
            jobMethodCode = "ml";
            if(hasSimulation)
            {
                jRadioButton4.doClick();
                jTabbedPane1.add("Dataset", jPanel3);
            }
            else
            {
                jRadioButton6.setSelected(true);
                datasetRadioButtonState = 3;
                jTextField4.setText(jobInfo.datasetName);
                jTextArea9.setText(jobInfo.datasetAbstract);
                jTextField6.setText(jobInfo.datasetVersion);
                dataArchive.id = jobInfo.datasetId;                               
            }            
        }
        else if(jobMethodClass == "ts")
        {
            jRadioButton1.doClick();
            jTabbedPane1.add("Model", jPanel2);
            jRadioButton6.setSelected(true);
            datasetRadioButtonState = 3;
            jTextField4.setText(jobInfo.datasetName);
            jTextArea9.setText(jobInfo.datasetAbstract);
            jTextField6.setText(jobInfo.datasetVersion);
            dataArchive.id = jobInfo.datasetId;
        }
        else
        {
            if(recentModel.description != null) jTextArea8.setText(recentModel.description);
            if(recentDataset.description != null) jTextArea9.setText(recentDataset.description);
            if(recentModel.name != null && recentModel.text.trim().equals(model.trim()))
                jRadioButton3.doClick();
            else
                jRadioButton1.doClick();
            if(recentDataset.name != null &&
               recentDataset.text.substring(recentDataset.text.indexOf("<spkdata "))
               .equals(dataset.substring(dataset.indexOf("<spkdata "))))
//               XMLReader.parseDataXML(recentDataset.text.substring(recentDataset.text.indexOf("<spkdata ")), true)
//               .equals(XMLReader.parseDataXML(dataset.substring(dataset.indexOf("<spkdata ")), true)))
                jRadioButton6.doClick();
            else
                jRadioButton4.doClick();
            jTabbedPane1.add("Model", jPanel2);
            jTabbedPane1.add("Dataset", jPanel3);            
        }
        jTabbedPane1.add("Job", jPanel5);
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
        jRadioButton9.setEnabled(((String[])methodTable.get("vl"))[2].equals("0") || isDeveloper);
        jRadioButton10.setEnabled(((String[])methodTable.get("gr"))[2].equals("0") || isDeveloper);
        jRadioButton11.setEnabled(((String[])methodTable.get("ad"))[2].equals("0") || isDeveloper);
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
        else if(jobMethodCode.equals("fo") || jobMethodCode.equals("eh") || jobMethodCode.equals("la") ||
                jobMethodCode.equals("s2") || jobMethodCode.equals("i2") || jobMethodCode.equals("g2") ||
                jobMethodCode.equals("sm") || jobMethodCode.equals("im") || jobMethodCode.equals("gm") ||
                jobMethodCode.equals("id") || jobMethodCode.equals("un") || jobMethodCode.equals("gn"))
        {
            jRadioButton7.setText(((String[])methodTable.get(jobMethodCode))[0]);
            jRadioButton7.doClick();
            jRadioButton8.setEnabled(false);
            jRadioButton9.setEnabled(false);
            jRadioButton10.setEnabled(false);
            jRadioButton11.setEnabled(false);
            jRadioButton12.setEnabled(false);
        }
        else if(jobMethodCode.equals("ml"))
        {
            jRadioButton7.setEnabled(false);
            
            if(Likelihood.findNEta(text) == 1)
            {
                jRadioButton9.setSelected(true);
                jobMethodCode = "vl";
                jRadioButton11.setEnabled(false);
            }
            else
            {
                jRadioButton11.setSelected(true);
                jobMethodCode = "ad";
            }
        }
        
        if(jobId != 0) jTextArea10.setText(jobInfo.jobAbstract); 
        jCheckBox1.setSelected(false);
        jCheckBox2.setSelected(false);
        jCheckBox2.setEnabled(jobMethodCode.equals("eh") || jobMethodCode.equals("la") || jobMethodClass.equals("le"));
        if(!isTester) jCheckBox2.setEnabled(false);
        jRadioButton12.setEnabled(false);
        archiveDialog.setSize(300, 380);
        archiveDialog.setVisible(true);    
    }
    
    private void WriteInputButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_WriteInputButtonActionPerformed
        jobId = 0;
        iterator = new MDAIterator(server, isOnline, this, isTester, isDeveloper, files, jobId);
        writeInput(iterator);
    }//GEN-LAST:event_WriteInputButtonActionPerformed

    private void scatterPlot()
    {
        if(output != null && output.scatterplot != null)
        {
            if(output.dataAll != null && output.dataItems != null && output.dataLabelMap != null)
            {
                new PlotShow(output.scatterplot, output.dataAll, output.dataItems,
                             output.dataLabelMap); 
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
                               width(length), height(length), l*40, l*30, output.sigmaStruct[l].equals("diagonal"));
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
                               width(length), height(length), l*40, l*30, output.omegaStruct[l].equals("diagonal"));
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
        if(output != null && (output.objective != null || output.likelihood != null) && output.methodCode != null)
        {
            String objective = null;
            String objStdErr = "";
            String nEvaluation = "";
            String computeTime = "";
            if(!((String[])methodTable.get(output.methodCode))[1].equals("le"))
            {
                objective = "Minimum Value of Objective Function:\n" + output.objective;
                jButton3.setEnabled(false);
                objectiveDialog.setTitle("Objective");
                objectiveDialog.setSize(300, 200);                
            }
            else
            {
                objective = "Estimate for Likelihood Function:\n" + output.likelihood[0][1];
                if(output.likelihood_std != null)
                    objStdErr = "\n\nStandard Error in Likelihood Function:\n" + output.likelihood_std[0][1];          
                jButton3.setEnabled(true);
                objectiveDialog.setTitle("Likelihood");
                nEvaluation = "\n\nNumber of Evaluations:\n" + output.nEvaluation;
                computeTime = "\n\nComputing Time (second):\n" + output.computingTimes[0];
                objectiveDialog.setSize(300, 280);
            }
            String jobMethod = "\n\nMethod Used in the Analysis:\n" + 
                               ((String[])methodTable.get(output.methodCode))[0];
            
            jTextArea2.setText(objective + objStdErr + jobMethod + nEvaluation + computeTime);
            objectiveDialog.setVisible(true);
        }
        else
            JOptionPane.showMessageDialog(null, "The objective or the method code is not available", 
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
            errorMessageDialog.setSize(600, 400);
            errorMessageDialog.setVisible(true);
        }
        else
            JOptionPane.showMessageDialog(null, "The error message is not available",
                                          "Message Not Found Error",
                                          JOptionPane.ERROR_MESSAGE);
    }//GEN-LAST:event_errorMenuActionPerformed
    
    private void exitMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exitMenuActionPerformed
        if(JOptionPane.showConfirmDialog(null, 
                                         "Are you sure you want to close the MDA?",   
                                         "Question Dialog",
                                         JOptionPane.YES_NO_OPTION,
                                         JOptionPane.QUESTION_MESSAGE) == 0)
        {
            if(isOnline) server.endSession();
            System.exit(0);
        }
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
            saveOperation(textArea.getText(), file);
            jInternalFrame1.setTitle(file.getName());
        }
    }//GEN-LAST:event_savaAsMenuActionPerformed

    private void saveMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveMenuActionPerformed
        if(file != null)
	{
            saveOperation(textArea.getText(), file);
        }
        else
	{
            files.setDialogTitle("Save File");
            int result = files.showSaveDialog(null);
            if(result == files.APPROVE_OPTION)
	    {
                file = files.getSelectedFile();
                saveOperation(textArea.getText(), file);
            }
        }
    }//GEN-LAST:event_saveMenuActionPerformed

    private void closeMenuActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeMenuActionPerformed
        textArea.setText("");
        jInternalFrame1.setTitle("");
        file = null;
        isChanged = false;
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
            isChanged = false;
        }
    }//GEN-LAST:event_openMenuActionPerformed

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
        String jobAbstract = jTextArea10.getText();
                
        // Collect model archive information 
        modelArchive.name = jTextField1.getText();
        modelArchive.description = jTextArea8.getText();        
        modelArchive.version = "1." + jTextField3.getText();
        modelArchive.isNewArchive = jRadioButton1.isSelected(); 
        modelArchive.isNewVersion = jRadioButton2.isSelected();
        if(!modelArchive.isNewVersion || modelArchive.isNewArchive)
            modelArchive.log = "";
        
        // Check input errors
        if(modelArchive.name.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The model name is requried.",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);                 
            return; 
        }
        if(modelArchive.name.length() > 20)
        {
            JOptionPane.showMessageDialog(null, "The model name is too long.",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);                 
            return; 
        }  
        if(modelArchive.description.length() > 100)
        {
            JOptionPane.showMessageDialog(null, "The model description is too long.",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);                 
            return;
        }
        if(modelArchive.version.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The model version is requried.",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);                 
            return; 
        }        

        // Collect data archive information 
        dataArchive.name = jTextField4.getText();    
        dataArchive.description = jTextArea9.getText();     
        dataArchive.version = "1." + jTextField6.getText();
        dataArchive.isNewArchive = jRadioButton4.isSelected(); 
        dataArchive.isNewVersion = jRadioButton5.isSelected();
        if(!dataArchive.isNewVersion || dataArchive.isNewArchive) 
            dataArchive.log = "";
        
        // Check input errors  
        if(dataArchive.name.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The dataset name is requried.",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }
        if(dataArchive.name.length() > 20)
        {
            JOptionPane.showMessageDialog(null, "The dataset name is too long.",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);                 
            return; 
        }  
        if(dataArchive.description.length() > 100)
        {
            JOptionPane.showMessageDialog(null, "The dataset description is too long.",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);                 
            return;
        }
        if(dataArchive.version.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The dataset version is requried.",  
                                          "Input Error",            
                                          JOptionPane.ERROR_MESSAGE);                 
            return;
        }

        // Get the XML documents as String objects
        String spkInput = textArea.getText();
        int indexData = spkInput.lastIndexOf("<?xml ", spkInput.indexOf("<spkdata"));
        String source = spkInput.substring(0, indexData);
//        int index = spkInput.indexOf("<spkdata");
//        String source = spkInput.substring(0, index - 22);

        // Collect version logs
        if((modelArchive.isNewArchive || modelArchive.isNewVersion))
        {
            modelArchive.log = jTextArea6.getText();
            if(!Utility.checkCharacter(modelArchive.log, "model log"))
                return;
        }

        if((dataArchive.isNewArchive || dataArchive.isNewVersion))
        {
            dataArchive.log = jTextArea7.getText();
            if(!Utility.checkCharacter(dataArchive.log, "dataset log"))
                return;
        }
        
        // Remove number of objective function evaluations
        int indexMC = source.indexOf("<monte_carlo ");
        if(indexMC != -1)
            source = source.substring(0, indexMC - 3) + source.substring(source.indexOf("</nonmem>"));
   
        // Add number of objective function evaluations
        if(jobMethodClass.equals("le"))
        {
            source = Likelihood.insertLeElement(source, jobMethodCode);
            if(source == null)
                return;
        }
        
        // Get job parent
        long jobParent = 0;
        if(jobId != 0)
        {
            jobParent = jobId;
            jobId = 0;
        }

        int nTasks = 0;
        if(jCheckBox2.isSelected())
        {
            nTasks = Utility.findNTasks(source, jobMethodCode, jobMethodClass);
        }
        
        // Submit the job
        boolean ok = server.submitJob(source, jobAbstract, modelArchive, dataArchive, jobMethodCode,
                                      jobParent, false, jCheckBox1.isSelected(), nTasks);
        
        // Close the dialog
        if(ok) archiveDialog.dispose();
    }//GEN-LAST:event_okButtonActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        archiveDialog.dispose();
    }//GEN-LAST:event_cancelButtonActionPerformed
    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
        if(JOptionPane.showConfirmDialog(null, 
                                         "Are you sure you want to close the MDA?",   
                                         "Question Dialog",
                                         JOptionPane.YES_NO_OPTION,
                                         JOptionPane.QUESTION_MESSAGE) == 0)
        {
            if(isOnline) server.endSession();
            System.exit(0);
        }
    }//GEN-LAST:event_exitForm

    /** This method performs open file operation.
     * @return a String[] of two elements.  The first element is the file path and the last element
     * is the file content.
     */
    public String[] openOperation()
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
                StringBuffer buffer = new StringBuffer();
                BufferedReader in = new BufferedReader(new FileReader(files.getSelectedFile()));
                String line;
                while((line = in.readLine()) != null)
                    buffer.append(line).append("\n");
                in.close();
                text = new String[2];
                text[0] = files.getSelectedFile().getPath();
                text[1] = buffer.toString();
            }
            catch(IOException e)
	    {
                System.err.println(e);
                JOptionPane.showMessageDialog(null, "Error opening file",  // Display opening file 
                                              "File Error",                // error message
                                              JOptionPane.ERROR_MESSAGE);
            }
        }
        return text;
    }
    
    /** This method performs save file operation.
     * @param text the content of the file to be saved.
     * @param file the File object for the file to be saved
     */
    public void saveOperation(String text, File file)
    {
        text = text.replaceAll("\n", ls);
        FileOutputStream out = null;
        try
        {
            out = new FileOutputStream(file);
        }
        catch(FileNotFoundException e){}
        ByteBuffer buffer = ByteBuffer.allocate(text.length());
        FileChannel channel = out.getChannel();
        buffer.put(text.getBytes());
        buffer.flip();
        try
        {
            channel.write(buffer);
//            BufferedWriter out = new BufferedWriter(new FileWriter(file));
//            out.write(text.replaceAll("\n", ls));
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
        wp.createDialog(this, "Model Design Agent Input File Generation Tool").setVisible(true);
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
                          "Error", "Sigma", "Simulation", "Table", "ScatterPlot",
                          "Estimation", "Covariance", "Table", "ScatterPlot"};        
        control = "";
        for(int i = 0; i < 19; i++)
        {
            if(!records.getProperty(names[i]).equals("")) 
                control = control + records.getProperty(names[i]) + "\n";
        }
/**        
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
                saveOperation(control, file);
            }   
        }
        file = null;
        files.setSelectedFile(new File(""));
*/
        // Propote user to enter seed for identifiability analysis
        
        
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
            isChanged = false;
        }
        WriteInputButton.setEnabled(true);
    }
   
    /** Read report
     * @param text a String containing the SPK output.
     */    
    protected void readOutput(String text)
    { 
        if(text.indexOf("<spkreport") == -1 || text.indexOf("<spksource") == -1)
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
//                        String path = System.getProperty("user.home") + System.getProperty("file.separator");
                        for(int j = 0; j < tableI[1].length; j++)
                        {
                             // For item "DV" replace it by the alias
                             if(tableI[1][j].equals("DV"))
                                 tableI[1][j] = output.dataLabelMap.getProperty("DV");
                        }                        
                        
                        tableShow.fillTable(tableI, data, header);
                        files.setDialogTitle("Save table File");
//                        files.setSelectedFile(new File(path + tableI[0][0]));
                        files.setSelectedFile(new File(tableI[0][0]));
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
                                DecimalFormat f = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
                                f.applyPattern("0.0000E00");
                                int start = 0;
                                int k = 0;
                                String element;
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
                                        {
                                            element = data[k][l];
                                            if(element.equals("NaN") || element.endsWith("Infinity"))
                                                out.write(getSpace(12 - element.length()) + element);
                                            else
                                                out.write(" " + Utility.formatData(8, f.format(Double.parseDouble(element))));
                                        }
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
                if(!((String[])methodTable.get(output.methodCode))[1].equals("le"))
                    JOptionPane.showMessageDialog(null, "The data for user specified tables are unavailable", 
                                                  "Data Not Found Error",               
                                                  JOptionPane.ERROR_MESSAGE);                
            }
        }

        // Create a presentation data block and a first row only data block
        if(output.dataItems != null && output.dataAll != null)
        {
            int nColumns = output.dataItems.size();
            int nRows = output.dataAll.length;
            StringBuffer sb = new StringBuffer();
            StringBuffer fr = new StringBuffer();
            for(int i = 0; i < nColumns; i++)
            {
                String label = (String)output.dataItems.get(i); 
                sb.append(getSpace(12 - label.length()));
                sb.append(label);
                fr.append(getSpace(12 - label.length()));
                fr.append(label);
            }
            sb.append("\n");
            fr.append("\n");
            DecimalFormat f = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
            f.applyPattern("0.0000E00");
            double value;
            for(int j = 0; j < nRows; j++)
            {
                String ID = output.indIDs[j];
                sb.append(getSpace(12 - ID.length()));
                sb.append(ID);
                for(int i = 1; i < nColumns; i++)
                {
                    value = output.dataAll[j][i];
                    if(String.valueOf(value).equals("NaN"))
                        sb.append("         NaN");
                    else if(String.valueOf(value).equals("-Infinity"))
                        sb.append("   -Infinity");
                    else if(String.valueOf(value).equals("Infinity") || String.valueOf(value).equals("+Infinity"))
                        sb.append("   +Infinity");
                    else
                        sb.append(" " + Utility.formatData(8, f.format(value)));
                }
                sb.append("\n");
            }
            String ID = "";
            for(int j = 0; j < nRows; j++)
            {
                if(!output.indIDs[j].equals(ID))
                {
                    ID = output.indIDs[j];
                    fr.append(getSpace(12 - ID.length()));
                    fr.append(ID);
                    for(int i = 1; i < nColumns; i++)
                    {
                        value = output.dataAll[j][i];
                        if(String.valueOf(value).equals("NaN"))
                            fr.append("         NaN");
                        else if(String.valueOf(value).equals("-Infinity"))
                            fr.append("   -Infinity");
                        else if(String.valueOf(value).equals("Infinity") || String.valueOf(value).equals("+Infinity"))
                            fr.append("   +Infinity");
                        else
                            fr.append(" " + Utility.formatData(8, f.format(value)));
                    }
                    fr.append("\n");
                }
            }
            dataBlock = sb.toString();
            dataFirst = fr.toString();
        }

        JOptionPane.showMessageDialog(null, "Report parsing is finished. Result is ready for presentation.",  
                                      "MDA Status Information",             
                                      JOptionPane.INFORMATION_MESSAGE);
//        saveFile();
        textArea.setText(Summary.makeSummary(output, isOnline, isDeveloper, jobMethodCode, methodTable));
        textArea.setCaretPosition(0);
        jInternalFrame1.setTitle("Summary Report: Job-" + output.jobId);
        file = null;
        isChanged = false;
    }
    
    // This method returns spaces
    private String getSpace(int n)
    {
        StringBuffer sb = new StringBuffer();
        for(int i = 0; i < n; i++)
            sb.append(" ");
        return sb.toString();  
    }

    // This method returns matrix width
    private int width(int dimension) 
    {
        if(dimension < 3)
            dimension = 3;
        int width = (dimension + 1) * 80 + 60; 
        if(width > 800)
            width = 800;
        return width;
    }
    
    // This method returns matrix height
    private int height(int dimension)
    {
        int height = (dimension + 1) * 20 + 120;
        if(height > 600)
            height = 600;
        return height;
    }  
    
    /** This method asks the user wether to save the text in the editor text area to a file.
     */
    protected void saveFile()
    {
        if(!textArea.getText().equals(""))
        {
            String pathName = "untitled";
            if(file != null)
                pathName = file.getPath();
            if(isChanged)
                if(JOptionPane.showConfirmDialog(null, 
                                             "Do you want to save the file " + pathName + "?",
                                             "Question Dialog",
                                             JOptionPane.YES_NO_OPTION,
                                             JOptionPane.QUESTION_MESSAGE) == 0)
                {
                    if(pathName.equals("untitled"))
                    {
                        files.setDialogTitle("Save File");
                        int result = files.showSaveDialog(null);
                        if(result == files.APPROVE_OPTION)
                        {
                            file = files.getSelectedFile();
                            saveOperation(textArea.getText(), file);
                        }
                    }
                    else
                    {
                        file = new File(pathName);
                        saveOperation(textArea.getText(), file);
                    }
                }  
        }
    }
    
    // Display a list of jobs or models or versions that belongs to the user
    private void showArchiveList(boolean isRepeatCall, boolean isCountChange)
    {
        String[] header = null;
        String title = "";
        String[][] archiveList = null;
        String name = isLibrary ? "librarian" : username;
                    
        if(listType.equals("job") || modelID != null || datasetID != null)
        {
            timer.start();
            title = "Job List - " + name; 
            header = new String[]{"Job ID", "Submission Time", "Status Code", "Model.Version", "Dataset.Version", "Job Abstract"};
            jPanel17.setVisible(false);
            findJobButton.setVisible(true);
            findJobButton.setEnabled(true);
        }
        else if(listType.equals("model"))
        {
            title = "Model List - " + name; 
            header = new String[]{"Model ID", "Model Name", "No. of Versions", "Last Revised Time", "Description"};
            findJobButton.setVisible(false);
            jPanel17.setVisible(true);
            versionRadioButton.setEnabled(true);
        }
        else if(listType.equals("data"))
        {
            title = "Dataset List - " + name;
            header = new String[]{"Dataset ID", "Dataset Name", "No. of Versions", "Last Revised Time", "Description"};
            findJobButton.setVisible(false);
            jPanel17.setVisible(true);
            versionRadioButton.setEnabled(true);
        }
        else
            return;
        
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
                leftOff = Long.parseLong(((String[][])lists.get(indexList - 1))[maxNum - 1][0].replaceFirst("s", ""));

            if(listType.equals("job") || modelID != null || datasetID != null)        
                archiveList = server.getUserJobs(maxNum + 1, leftOff, name, startID,
                                                 startTime, keyWords, modelID, datasetID);             
            else if(listType.equals("model"))
                archiveList = server.getUserModels(maxNum + 1, leftOff, name);
            else if(listType.equals("data"))      
                archiveList = server.getUserDatasets(maxNum + 1, leftOff, name);
            else
                return;
            
            versionRadioButton.setEnabled(true);
            jobRadioButton.setEnabled(true);
            
            if(archiveList == null)
            {
                if(listType.equals("job") || modelID != null || datasetID != null)
                {
                    archiveList = new String[1][6];
                    for(int i = 0; i < 6; i++) archiveList[0][i] = "";
                }
                else
                {
                    archiveList = new String[1][5];
                    for(int i = 0; i < 5; i++) archiveList[0][i] = "";
                    versionRadioButton.setEnabled(false);
                    jobRadioButton.setEnabled(false);
                }
                timer.stop();
                startID = startTime = keyWords = modelID = datasetID = null;
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
    //    int start = listType.equals("job") ? 0:1;
        DisplayTableModel reportModel = new DisplayTableModel(archiveList, header);
        jTable1.setModel(reportModel);
        TableColumnModel columnModel = jTable1.getColumnModel();
        if(listType.equals("job") || modelID != null || datasetID != null)
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
            columnModel.getColumn(0).setPreferredWidth(100);            
            columnModel.getColumn(1).setPreferredWidth(200);
            columnModel.getColumn(2).setPreferredWidth(120);
            columnModel.getColumn(3).setPreferredWidth(200);
            columnModel.getColumn(0).setCellRenderer(new CellRenderer());
            columnModel.getColumn(2).setCellRenderer(new CellRenderer());
        }

        columnModel.getColumn(header.length - 1).setPreferredWidth(300);
        groupLabel.setEnabled(!groupID.equals("0") && !isLibrary && getArchive && !isDiff);
        groupComboBox.setEnabled(!groupID.equals("0") && !isLibrary && getArchive && !isDiff);
        if(jobRadioButton.isSelected()) groupComboBox.setEnabled(false);
        
        if(isCountChange) countTextField.setText(Network.count);
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
        String[][] versionList = server.getVersions(id, listType);             
        if(versionList != null)
        {
            jTable2.setModel(new DefaultTableModel(versionList, header));
            TableColumnModel columnModel = jTable2.getColumnModel();
            columnModel.getColumn(0).setPreferredWidth(80);
            columnModel.getColumn(2).setPreferredWidth(200);
            columnModel.getColumn(3).setPreferredWidth(500);
            String title;
            if(listType.equals("model"))
                title = "Version List - Model ID: " + id;
            else if(listType.equals("data"))
                title = "Version List - Dataset ID: " + id;
            else
                return;
            versionDialog.setTitle(title);
            versionDialog.setLocation(200, 200);
            versionDialog.setSize(800, 16 * versionList.length + 56); 
            versionDialog.setVisible(true);
        }
    }    

    private class DisplayTableModel extends AbstractTableModel
    {
        public DisplayTableModel(String[][] data, String[] header)
        {
            this.data = data;
            this.header = header;
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
            return data[r][c];
        }

        // Table data array
        String[][] data = null;
        
        // Table header array
        String[] header = null;
    }    

    private class CellRenderer extends DefaultTableCellRenderer 
    {
        public Component getTableCellRendererComponent(JTable table,
            Object value,boolean isSelected, boolean hasFocus, int row,int col) 
        {
            super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,col);
            
            if(((String)value).endsWith("s"))
                setForeground(Color.red);
            else if(((String)value).startsWith("s"))
                setForeground(Color.blue);
            else
                setForeground(Color.black);
            setText(((String)value).replaceFirst("s", ""));
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
    public void setEditorText(String text)
    {
        textArea.setText(text);
        isChanged = false;
    }
    
    /** Set editor's caret position.
     * @param position an int, the position for the caret.
     */    
    public void setEditorCaretPosition(int position)
    {
        textArea.setCaretPosition(position);
    }
    
    /** Set editor title.
     * @param title a string for the editor title.
     */    
    public void setEditorTitle(String title)
    {
        jInternalFrame1.setTitle(title);   
    }

    /** Get the helpBroker.
     * @return the helpBroker.
     */
    public static HelpBroker getHelpBroker()
    {
        return helpBroker;   
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton CompareFilesButton;
    private javax.swing.JButton DataArchiveButton;
    private javax.swing.JButton DatasetLibraryButton;
    private javax.swing.JMenuItem FirstMenu;
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
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.ButtonGroup buttonGroup3;
    private javax.swing.ButtonGroup buttonGroup4;
    private javax.swing.JButton cancelButton;
    private javax.swing.JMenuItem closeMenu;
    private javax.swing.JButton compareButton;
    private javax.swing.JMenuItem copyMenu;
    private javax.swing.JMenuItem correlationMenu;
    private javax.swing.JLabel countLabel;
    private javax.swing.JTextField countTextField;
    private javax.swing.JMenuItem covarianceMenu;
    private javax.swing.JMenuItem cutMenu;
    private javax.swing.JComboBox dComboBox;
    private javax.swing.JButton dataLButton;
    private javax.swing.JButton dataLibLButton;
    private javax.swing.JButton dataLibRButton;
    private javax.swing.JMenuItem dataMenu;
    private javax.swing.JButton dataRButton;
    private javax.swing.JMenuItem deleteMenu;
    private javax.swing.JMenuItem densityMenu;
    private javax.swing.JDialog diffDialog;
    private javax.swing.JDialog diffHelpDialog;
    private javax.swing.JMenuItem errorMenu;
    private javax.swing.JDialog errorMessageDialog;
    private javax.swing.JMenuItem exitMenu;
    private javax.swing.JButton findJobButton;
    private javax.swing.JButton findJobCancelButton;
    private javax.swing.JDialog findJobDialog;
    private javax.swing.JButton findJobOKButton;
    private javax.swing.JMenuItem findMenu;
    private javax.swing.JComboBox groupComboBox;
    private javax.swing.JLabel groupLabel;
    private javax.swing.JButton helpButton;
    private javax.swing.JMenuItem identifiabilityTraceMenu;
    private javax.swing.JDialog indIDDialog;
    private javax.swing.JMenuItem indIDMenu;
    private javax.swing.JMenuItem invCovarianceMenu;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JCheckBox jCheckBox2;
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
    private javax.swing.JLabel jLabel17;
    private javax.swing.JLabel jLabel18;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel20;
    private javax.swing.JLabel jLabel21;
    private javax.swing.JLabel jLabel22;
    private javax.swing.JLabel jLabel23;
    private javax.swing.JLabel jLabel24;
    private javax.swing.JLabel jLabel25;
    private javax.swing.JLabel jLabel26;
    private javax.swing.JLabel jLabel27;
    private javax.swing.JLabel jLabel28;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu2;
    private javax.swing.JMenu jMenu3;
    private javax.swing.JMenu jMenu4;
    private javax.swing.JMenu jMenu6;
    private javax.swing.JMenu jMenu7;
    private javax.swing.JMenu jMenu9;
    private javax.swing.JMenuBar jMenuBar1;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItem10;
    private javax.swing.JMenuItem jMenuItem11;
    private javax.swing.JMenuItem jMenuItem12;
    private javax.swing.JMenuItem jMenuItem13;
    private javax.swing.JMenuItem jMenuItem2;
    private javax.swing.JMenuItem jMenuItem3;
    private javax.swing.JMenuItem jMenuItem4;
    private javax.swing.JMenuItem jMenuItem5;
    private javax.swing.JMenuItem jMenuItem6;
    private javax.swing.JMenuItem jMenuItem7;
    private javax.swing.JMenuItem jMenuItem8;
    private javax.swing.JMenuItem jMenuItem9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel12;
    private javax.swing.JPanel jPanel13;
    private javax.swing.JPanel jPanel14;
    private javax.swing.JPanel jPanel15;
    private javax.swing.JPanel jPanel16;
    private javax.swing.JPanel jPanel17;
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
    private javax.swing.JScrollPane jScrollPane10;
    private javax.swing.JScrollPane jScrollPane11;
    private javax.swing.JScrollPane jScrollPane12;
    private javax.swing.JScrollPane jScrollPane13;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JScrollPane jScrollPane5;
    private javax.swing.JScrollPane jScrollPane6;
    private javax.swing.JScrollPane jScrollPane7;
    private javax.swing.JScrollPane jScrollPane8;
    private javax.swing.JScrollPane jScrollPane9;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator4;
    private javax.swing.JSeparator jSeparator5;
    private javax.swing.JSeparator jSeparator6;
    private javax.swing.JSeparator jSeparator7;
    private javax.swing.JSeparator jSeparator8;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JTable jTable1;
    private javax.swing.JTable jTable2;
    private javax.swing.JTable jTable3;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextArea jTextArea10;
    private javax.swing.JTextArea jTextArea2;
    private javax.swing.JTextArea jTextArea3;
    private javax.swing.JTextArea jTextArea4;
    private javax.swing.JTextArea jTextArea5;
    private javax.swing.JTextArea jTextArea6;
    private javax.swing.JTextArea jTextArea7;
    private javax.swing.JTextArea jTextArea8;
    private javax.swing.JTextArea jTextArea9;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextField jTextField4;
    private javax.swing.JTextField jTextField5;
    private javax.swing.JTextField jTextField6;
    private javax.swing.JTextField jTextField8;
    private javax.swing.JTextField jTextField9;
    private javax.swing.JTextPane jTextPane1;
    private javax.swing.JTextPane jTextPane2;
    private javax.swing.JTextPane jTextPane3;
    private javax.swing.JTextField jobIDTextField;
    private javax.swing.JRadioButton jobRadioButton;
    private javax.swing.JTextField keyWordTextField;
    private javax.swing.JButton localLButton;
    private javax.swing.JButton localRButton;
    private javax.swing.JComboBox mComboBox;
    private javax.swing.JMenuItem meanMenu;
    private javax.swing.JButton modelLButton;
    private javax.swing.JButton modelLibLButton;
    private javax.swing.JButton modelLibRButton;
    private javax.swing.JButton modelRButton;
    private javax.swing.JButton nextButton;
    private javax.swing.JButton nextDiffButton;
    private javax.swing.JMenu nonparamMenu;
    private javax.swing.JDialog objectiveDialog;
    private javax.swing.JMenuItem objectiveMenu;
    private javax.swing.JButton okButton;
    private javax.swing.JMenuItem openMenu;
    private javax.swing.JMenuItem paramInMenu;
    private javax.swing.JMenuItem paramOutMenu;
    private javax.swing.JMenu parameterMenu;
    private javax.swing.JMenuItem pasteMenu;
    private javax.swing.JButton previousButton;
    private javax.swing.JMenuItem printMenu;
    private javax.swing.JButton refreshButton;
    private javax.swing.JDialog replaceDialog;
    private javax.swing.JMenuItem replaceMenu;
    protected javax.swing.JDialog reportDialog;
    private javax.swing.JMenuItem savaAsMenu;
    private javax.swing.JMenuItem saveMenu;
    private javax.swing.JMenuItem scatterPlotMenu;
    private javax.swing.JMenu statisticsMenu;
    private javax.swing.JMenuItem stdErrOmegaMenu;
    private javax.swing.JMenuItem stdErrSigmaMenu;
    private javax.swing.JMenuItem stdErrThetaMenu;
    private javax.swing.JMenu stdErrorMenu;
    private javax.swing.JMenuItem summaryMenu;
    private javax.swing.JMenuItem tableMenu;
    private javax.swing.JTextArea textArea;
    private javax.swing.JMenuItem traceMenu;
    private javax.swing.JMenuItem useMDAMenu;
    private javax.swing.JMenuItem useRMenu;
    private javax.swing.JDialog versionDialog;
    private javax.swing.JRadioButton versionRadioButton;
    private javax.swing.JMenuItem warningMenu;
    private javax.swing.JDialog warningMessageDialog;
    private javax.swing.JComboBox yComboBox;
    // End of variables declaration//GEN-END:variables

    /** The username of the MDA. */
    protected String myName = null;
    
    /** The username of the group member. */
    protected String username = null;
    
    private static HelpBroker helpBroker = null;
    
    /** The method table for the database. */
    protected HashMap methodTable = null;
    
    /** The Server object. */
    protected Server server = null;
    
    /** The flag for the user being a tester. */
    protected boolean isTester = false;
    
    /** The flag for the user being a developer. */
    protected boolean isDeveloper = false;    
    
    /** The current job id. */
    protected long jobId = 0;
    
    // Job method class
    private String jobMethodClass = null;
    
    // Job method code
    private String jobMethodCode = null;
    
    // Analytical approximation method
    private String method = null;
    
    /** The flag for online status. */
    protected boolean isOnline = true;

    /** The file chooser. */
    protected JFileChooser files = new JFileChooser();

    /** The current file. */
    protected File file = null;

    // The model archive information
    private ArchiveInfo modelArchive = null;
    
    // The data archive information
    private ArchiveInfo dataArchive = null;
    
    /** The information of recently downloaded model */
    protected ArchiveInfo recentModel = new ArchiveInfo();
    
    /** The information of recently downloaded dataset */
    protected ArchiveInfo recentDataset = new ArchiveInfo();
    
    // The model (Nonmem Control file) text 
    private String control = null;
    
    // MDA object
    private MDAObject object = new MDAObject();
    
    /** SPK output */
    protected Output output = null;
    
    // TableShow object
    private TableShow tableShow = null;

    // JWizardPane
    private JWizardPane wp = null;

    // List content
    private String listType = null;
    
    // Is library
    private boolean isLibrary = true;
    
    // Show version list
    private boolean showVersions = true;
    
    // Get archive
    private boolean getArchive = true; 
    
    // Is diff
    private boolean isDiff = false;
    
    // Is diff
    private boolean isWizard = true;
    
    // The text on the left to diff
    private String textL = null;
    
    // The text on the right to diff
    private String textR = null;    
    
    // Line number of delta 
    private Vector<Integer> deltaLines = null;
    
    // Delta index
    private int deltaIndex = 0;
    
    // Is left
    private boolean isLeft = true;
    
    // List collection
    private Vector<String[][]> lists = null;
    
    // Index of the list in list collection
    private int indexList = 0;
    
    // Selected archive name
    private String archiveName = null;
    
    // Selected archive id
    private long archiveId = 0;
    
    // Selected archive description
    private String archiveDescription = null;
    
    // line separator
    private static final String ls = System.getProperty("line.separator");
    
    // Maximum number of items
    private static final int maxNum = 12;
    
    // Timer for refreshing the job list dialog
    private Timer timer = null;
    
    // Job infomation
    private JobInfo jobInfo = null;
    
    // Has simulation in the source
    private boolean hasSimulation = false;
    
    // Positions of string found
    private Vector<Integer> positions = null;
    
    // Index of positions of string found
    private int indexPosition = 0;
    
    // Model radioButton State
    private int modelRadioButtonState = 1;
    
    // Dataset radioButton State
    private int datasetRadioButtonState = 1;
    
    // Is the version list dialog showing.
    private boolean isVersionListOn = false;
    
    /** MDA iterator */
    protected MDAIterator iterator = null;
    
    // Undo Manager
    private UndoManager undo = new UndoManager();
    
    /** Presentation data block */
    protected String dataBlock = null;
    
    // First row only data block
    private String dataFirst = null;
    
    // Start ID
    private String startID = null;
    
    // Start time
    private String startTime = null;
    
    // Key words
    private String keyWords = null;
    
    // Model ID to find job
    private String modelID = null;
    
    // Dataset ID to find job
    private String datasetID = null;
    
    // Group ID of the user
    private String groupID = null;
    
    // Is initializing
    private boolean isInit = true;
    
    // Is text changed
    private boolean isChanged = false;
    
    // Shared job list
    private ArrayList sharedJobs = null;
}
