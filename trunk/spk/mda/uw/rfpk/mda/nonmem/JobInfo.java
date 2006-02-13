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
import java.util.Properties;
import java.awt.Cursor;
import java.io.File;
import javax.swing.JTextArea;
import javax.swing.JInternalFrame;
import javax.swing.JOptionPane;
import uw.rfpk.mda.nonmem.wizard.MDAIterator;
import javax.swing.table.*;

/** Job information dialog.
 * 
 * @author  Jiaji Du
 */
public class JobInfo extends javax.swing.JFrame {
    
    /** Creates new form JobInfo.
     * @param frame reference to MDAFrame object.
     * @param jobId job id.
     * @param isLibrary true if the job belongs to the library, false otherwise.
     * @param isParent true if it is a parent job, false otherwise.
     */
    public JobInfo(MDAFrame frame, long jobId, boolean isLibrary, boolean isParent)
    {
        id = jobId;
        this.isLibrary = isLibrary;
        this.frame = frame;
        isFirst = true;
        initComponents();
        Properties jobInfo = frame.server.getJobInfo(id, isLibrary);
        if(jobInfo == null)
            return;
        jobAbstract = jobInfo.getProperty("jobAbstract");
        jobOwner = jobInfo.getProperty("jobOwner");
        modelName = jobInfo.getProperty("modelName");
        datasetName = jobInfo.getProperty("datasetName");
        modelVersion = jobInfo.getProperty("modelVersion");
        datasetVersion = jobInfo.getProperty("datasetVersion");
        modelId = Long.parseLong(jobInfo.getProperty("modelId"));
        datasetId = Long.parseLong(jobInfo.getProperty("datasetId"));
        modelAbstract = jobInfo.getProperty("modelAbstract");
        datasetAbstract = jobInfo.getProperty("datasetAbstract");
        methodCode = jobInfo.getProperty("methodCode");
        stateCode = jobInfo.getProperty("stateCode");
        endCode = jobInfo.getProperty("endCode");
        jTextField1.setText(modelName);
        jTextField3.setText(datasetName);
        jTextField2.setText(modelVersion);
        jTextField4.setText(datasetVersion);
        String[] row = (String[])frame.methodTable.get(methodCode);
        if(row == null)
            row = new String[]{"Not Available", ""};
        jTextField5.setText(row[0]);
        jTextField6.setText(String.valueOf(jobId));
        jTextField7.setText(jobOwner);
        jobParent = Long.parseLong(jobInfo.getProperty("parent"));
        jCheckBox2.setSelected(true);
        boolean ok = !row[1].equals("le");
        jCheckBox1.setSelected(ok);
        jCheckBox3.setSelected(ok);
        jCheckBox1.setEnabled(ok);
        jCheckBox3.setEnabled(ok);
        jTextArea8.setEnabled(jobParent != 0);
        jButton6.setEnabled(jobParent != 0);
        ok = (methodCode.equals("fo") || methodCode.equals("eh") || methodCode.equals("la") ||
             methodCode.equals("ia")) && (endCode.equals("opte") || endCode.equals("abrt"));
        jTextArea6.setEnabled(ok);
        jButton7.setEnabled(ok);
        if(isParent)
        {
            java.awt.Point point = getLocation();
            point.translate(40, 30);
            setLocation(point);
        }
        jButton8.setEnabled(!stateCode.equals("end") && !stateCode.equals("q2ac") &&
                            !stateCode.equals("acmp") && !stateCode.equals("q2ar") &&
                            !stateCode.equals("arun") && jobOwner.equals(System.getProperty("user.name")));
        ok = (methodCode.equals("fo") || methodCode.equals("eh")|| methodCode.equals("la") ||
              methodCode.equals("s2") || methodCode.equals("i2")|| methodCode.equals("g2") ||
              methodCode.equals("sm") || methodCode.equals("im")|| methodCode.equals("gm")) 
             && (endCode.equals("srun") || endCode.equals("staf"));
        jButton9.setEnabled(ok);
        jTextArea5.setEnabled(ok);
        setTitle("Job Information - " + frame.username);
        setVisible(true);
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        historyDialog = new javax.swing.JDialog();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        warmStartDialog = new javax.swing.JDialog();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jTextField8 = new javax.swing.JTextField();
        jLabel10 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        OKButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        jComboBox1 = new javax.swing.JComboBox();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTextArea7 = new javax.swing.JTextArea();
        jTextArea1 = new javax.swing.JTextArea();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jTextField2 = new javax.swing.JTextField();
        jTextField3 = new javax.swing.JTextField();
        jTextField4 = new javax.swing.JTextField();
        jSeparator1 = new javax.swing.JSeparator();
        jTextArea2 = new javax.swing.JTextArea();
        jPanel1 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();
        jButton4 = new javax.swing.JButton();
        jCheckBox1 = new javax.swing.JCheckBox();
        jCheckBox2 = new javax.swing.JCheckBox();
        jSeparator2 = new javax.swing.JSeparator();
        jTextArea3 = new javax.swing.JTextArea();
        jButton5 = new javax.swing.JButton();
        jButton6 = new javax.swing.JButton();
        jTextField5 = new javax.swing.JTextField();
        jLabel6 = new javax.swing.JLabel();
        jCheckBox3 = new javax.swing.JCheckBox();
        jTextField6 = new javax.swing.JTextField();
        jTextArea5 = new javax.swing.JTextArea();
        jTextArea6 = new javax.swing.JTextArea();
        jButton7 = new javax.swing.JButton();
        jTextArea8 = new javax.swing.JTextArea();
        jButton8 = new javax.swing.JButton();
        jButton9 = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        jLabel5 = new javax.swing.JLabel();
        jTextField7 = new javax.swing.JTextField();
        jLabel7 = new javax.swing.JLabel();

        historyDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
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
        jScrollPane1.setViewportView(jTable1);

        historyDialog.getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        warmStartDialog.getContentPane().setLayout(new java.awt.GridBagLayout());

        warmStartDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        warmStartDialog.setTitle("Warm Start Job Settings");
        warmStartDialog.setLocationRelativeTo(this);
        warmStartDialog.setModal(true);
        jLabel8.setFont(new java.awt.Font("Default", 0, 12));
        jLabel8.setText("\nJob short description (<=100 characters)   ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        warmStartDialog.getContentPane().add(jLabel8, gridBagConstraints);

        jLabel9.setFont(new java.awt.Font("Default", 0, 12));
        jLabel9.setText("Maximum number of iterations");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 0, 0);
        warmStartDialog.getContentPane().add(jLabel9, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 6, 0, 12);
        warmStartDialog.getContentPane().add(jTextField8, gridBagConstraints);

        jLabel10.setFont(new java.awt.Font("Default", 0, 12));
        jLabel10.setText("Please enter the following information.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        warmStartDialog.getContentPane().add(jLabel10, gridBagConstraints);

        jLabel11.setFont(new java.awt.Font("Default", 0, 12));
        jLabel11.setText("Number of significant digits ");
        jLabel11.setEnabled(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(5, 12, 4, 0);
        warmStartDialog.getContentPane().add(jLabel11, gridBagConstraints);

        OKButton.setText("OK");
        OKButton.setMaximumSize(new java.awt.Dimension(75, 25));
        OKButton.setMinimumSize(new java.awt.Dimension(75, 25));
        OKButton.setPreferredSize(new java.awt.Dimension(75, 25));
        OKButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                OKButtonActionPerformed(evt);
            }
        });

        jPanel2.add(OKButton);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });

        jPanel2.add(cancelButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        warmStartDialog.getContentPane().add(jPanel2, gridBagConstraints);

        jComboBox1.setBackground(new java.awt.Color(255, 255, 255));
        jComboBox1.setFont(new java.awt.Font("Dialog", 0, 12));
        jComboBox1.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "1", "2", "3", "4", "5", "6", "7", "8" }));
        jComboBox1.setMaximumSize(new java.awt.Dimension(32767, 20));
        jComboBox1.setMinimumSize(new java.awt.Dimension(36, 20));
        jComboBox1.setPreferredSize(new java.awt.Dimension(36, 20));
        jComboBox1.setEnabled(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 6, 0, 12);
        warmStartDialog.getContentPane().add(jComboBox1, gridBagConstraints);

        jScrollPane2.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        jScrollPane2.setVerticalScrollBarPolicy(javax.swing.JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        jScrollPane2.setMaximumSize(new java.awt.Dimension(32767, 48));
        jScrollPane2.setMinimumSize(new java.awt.Dimension(7, 48));
        jTextArea7.setLineWrap(true);
        jTextArea7.setRows(3);
        jTextArea7.setBorder(new javax.swing.border.CompoundBorder());
        jTextArea7.setMaximumSize(new java.awt.Dimension(2147483647, 45));
        jTextArea7.setMinimumSize(new java.awt.Dimension(0, 45));
        jScrollPane2.setViewportView(jTextArea7);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        warmStartDialog.getContentPane().add(jScrollPane2, gridBagConstraints);

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setResizable(false);
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        jTextArea1.setBackground(new java.awt.Color(204, 204, 204));
        jTextArea1.setEditable(false);
        jTextArea1.setText("This job uses the following model and dataset:");
        jTextArea1.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 12, 6, 12);
        getContentPane().add(jTextArea1, gridBagConstraints);

        jLabel1.setText("Name");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel1, gridBagConstraints);

        jLabel2.setText("Version");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jLabel2, gridBagConstraints);

        jLabel3.setText("Model");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 2, 12);
        getContentPane().add(jLabel3, gridBagConstraints);

        jLabel4.setText("Dataset");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 12, 7, 12);
        getContentPane().add(jLabel4, gridBagConstraints);

        jTextField1.setEditable(false);
        jTextField1.setFont(new java.awt.Font("Monospaced", 0, 12));
        jTextField1.setFocusable(false);
        jTextField1.setMaximumSize(new java.awt.Dimension(160, 19));
        jTextField1.setMinimumSize(new java.awt.Dimension(160, 19));
        jTextField1.setPreferredSize(new java.awt.Dimension(160, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(6, 0, 2, 0);
        getContentPane().add(jTextField1, gridBagConstraints);

        jTextField2.setEditable(false);
        jTextField2.setFont(new java.awt.Font("Monospaced", 0, 12));
        jTextField2.setFocusable(false);
        jTextField2.setMaximumSize(new java.awt.Dimension(60, 19));
        jTextField2.setMinimumSize(new java.awt.Dimension(60, 19));
        jTextField2.setPreferredSize(new java.awt.Dimension(60, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 2, 12);
        getContentPane().add(jTextField2, gridBagConstraints);

        jTextField3.setEditable(false);
        jTextField3.setFont(new java.awt.Font("Monospaced", 0, 12));
        jTextField3.setFocusable(false);
        jTextField3.setMaximumSize(new java.awt.Dimension(160, 19));
        jTextField3.setMinimumSize(new java.awt.Dimension(160, 19));
        jTextField3.setPreferredSize(new java.awt.Dimension(160, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 7, 0);
        getContentPane().add(jTextField3, gridBagConstraints);

        jTextField4.setEditable(false);
        jTextField4.setFont(new java.awt.Font("Monospaced", 0, 12));
        jTextField4.setFocusable(false);
        jTextField4.setMaximumSize(new java.awt.Dimension(60, 19));
        jTextField4.setMinimumSize(new java.awt.Dimension(60, 19));
        jTextField4.setPreferredSize(new java.awt.Dimension(60, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 12, 7, 12);
        getContentPane().add(jTextField4, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jSeparator1, gridBagConstraints);

        jTextArea2.setBackground(new java.awt.Color(204, 204, 204));
        jTextArea2.setEditable(false);
        jTextArea2.setText("You may get the model, the dataset, the job input XML file or \nthe job output XML file by clicking the button below:");
        jTextArea2.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(7, 12, 0, 12);
        getContentPane().add(jTextArea2, gridBagConstraints);

        jButton1.setText("Model");
        jButton1.setMaximumSize(new java.awt.Dimension(82, 25));
        jButton1.setMinimumSize(new java.awt.Dimension(82, 25));
        jButton1.setPreferredSize(new java.awt.Dimension(82, 25));
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton1);

        jButton2.setText("Dataset");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton2);

        jButton3.setText("Input");
        jButton3.setMaximumSize(new java.awt.Dimension(82, 25));
        jButton3.setMinimumSize(new java.awt.Dimension(82, 25));
        jButton3.setPreferredSize(new java.awt.Dimension(82, 25));
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton3);

        jButton4.setText("Output");
        jButton4.setMaximumSize(new java.awt.Dimension(82, 25));
        jButton4.setMinimumSize(new java.awt.Dimension(82, 25));
        jButton4.setPreferredSize(new java.awt.Dimension(82, 25));
        jButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton4ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton4);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(5, 12, 3, 12);
        getContentPane().add(jPanel1, gridBagConstraints);

        jCheckBox1.setFont(new java.awt.Font("Dialog", 0, 12));
        jCheckBox1.setSelected(true);
        jCheckBox1.setText("Start input preparation tool when the Input button is clicked");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jCheckBox1, gridBagConstraints);

        jCheckBox2.setFont(new java.awt.Font("Dialog", 0, 12));
        jCheckBox2.setSelected(true);
        jCheckBox2.setText("Parse the output report when the Output button is clicked");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jCheckBox2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jSeparator2, gridBagConstraints);

        jTextArea3.setBackground(new java.awt.Color(204, 204, 204));
        jTextArea3.setEditable(false);
        jTextArea3.setText("You may get this job's processing history.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 13;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(10, 12, 8, 12);
        getContentPane().add(jTextArea3, gridBagConstraints);

        jButton5.setText("Job History");
        jButton5.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton5ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 13;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(7, 12, 5, 12);
        getContentPane().add(jButton5, gridBagConstraints);

        jButton6.setText("Job Parent");
        jButton6.setMaximumSize(new java.awt.Dimension(104, 25));
        jButton6.setMinimumSize(new java.awt.Dimension(104, 25));
        jButton6.setPreferredSize(new java.awt.Dimension(104, 25));
        jButton6.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton6ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 5, 12);
        getContentPane().add(jButton6, gridBagConstraints);

        jTextField5.setEditable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(10, 0, 4, 0);
        getContentPane().add(jTextField5, gridBagConstraints);

        jLabel6.setText("Method");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(10, 12, 4, 12);
        getContentPane().add(jLabel6, gridBagConstraints);

        jCheckBox3.setFont(new java.awt.Font("Dialog", 0, 12));
        jCheckBox3.setSelected(true);
        jCheckBox3.setText("Set the job as parent of a job created from this job's input");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 6, 12);
        getContentPane().add(jCheckBox3, gridBagConstraints);

        jTextField6.setEditable(false);
        jTextField6.setFont(new java.awt.Font("Monospaced", 0, 12));
        jTextField6.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
        jTextField6.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 0, 12);
        getContentPane().add(jTextField6, gridBagConstraints);

        jTextArea5.setBackground(new java.awt.Color(204, 204, 204));
        jTextArea5.setText("You may submit a likelihood evaluation job.");
        jTextArea5.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 16;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 12, 10, 12);
        getContentPane().add(jTextArea5, gridBagConstraints);

        jTextArea6.setBackground(new java.awt.Color(204, 204, 204));
        jTextArea6.setEditable(false);
        jTextArea6.setText("You may continue this job's optimization.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 12, 9, 0);
        getContentPane().add(jTextArea6, gridBagConstraints);

        jButton7.setText("Warm Start");
        jButton7.setMaximumSize(new java.awt.Dimension(104, 25));
        jButton7.setMinimumSize(new java.awt.Dimension(104, 25));
        jButton7.setPreferredSize(new java.awt.Dimension(104, 25));
        jButton7.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton7ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 5, 12);
        getContentPane().add(jButton7, gridBagConstraints);

        jTextArea8.setBackground(new java.awt.Color(204, 204, 204));
        jTextArea8.setText("You may also get this job's parent job.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(3, 12, 8, 12);
        getContentPane().add(jTextArea8, gridBagConstraints);

        jButton8.setText("Abort  Job");
        jButton8.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton8ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 12, 0, 12);
        getContentPane().add(jButton8, gridBagConstraints);

        jButton9.setText("Likelihood");
        jButton9.setMaximumSize(new java.awt.Dimension(104, 25));
        jButton9.setMinimumSize(new java.awt.Dimension(104, 25));
        jButton9.setPreferredSize(new java.awt.Dimension(104, 25));
        jButton9.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton9ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 16;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 8, 12);
        getContentPane().add(jButton9, gridBagConstraints);

        jPanel3.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        jLabel5.setText("Job Owner   ");
        jPanel3.add(jLabel5);

        jTextField7.setBackground(new java.awt.Color(204, 204, 204));
        jTextField7.setEditable(false);
        jTextField7.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
        jTextField7.setFocusable(false);
        jTextField7.setPreferredSize(new java.awt.Dimension(120, 19));
        jPanel3.add(jTextField7);

        jLabel7.setText("   Job ID");
        jPanel3.add(jLabel7);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 0, 0);
        getContentPane().add(jPanel3, gridBagConstraints);

        pack();
    }//GEN-END:initComponents

    private void jButton9ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton9ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Properties spkInput = frame.server.getInput(id, isLibrary);
        frame.jobId = id;
        frame.likelihoodJob(spkInput.getProperty("source") + spkInput.getProperty("dataset") +
                            "\n" + XMLWriter.setModel(spkInput.getProperty("model")));
        setCursor(null);
    }//GEN-LAST:event_jButton9ActionPerformed

    private void jButton8ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton8ActionPerformed
        if(frame.server.abortJob(id))
            JOptionPane.showMessageDialog(null, "Job " + id + " has been aborted");
        else
            JOptionPane.showMessageDialog(null, "Job " + id + " is not abortable");
        jButton8.setEnabled(false);
    }//GEN-LAST:event_jButton8ActionPerformed

    private void OKButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_OKButtonActionPerformed
        // Handle user's input
        jobAbstract = jTextArea7.getText();
        if(jobAbstract.length() > 100)
            jobAbstract = jobAbstract.substring(0, 100);
        String maxIter = jTextField8.getText().trim();
        if(!Utility.isPosIntNumber(maxIter))
        {
            JOptionPane.showMessageDialog(null, "The Maximum number of iterations must be a positive integer.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return;
        }
        String sigDig = (String)jComboBox1.getSelectedItem();
        
        // Create model and data archives
        ArchiveInfo modelArchive = new ArchiveInfo();
        ArchiveInfo dataArchive = new ArchiveInfo();
        modelArchive.isNewArchive = false;
        modelArchive.id = modelId;
        dataArchive.isNewArchive = false;
        dataArchive.id = datasetId;
        dataArchive.isNewVersion = false;
        dataArchive.version = "1." + datasetVersion;
        int beginIndex, endIndex;
        String analysis = methodCode.equals("ia") ? "<ind_analysis " : "<pop_analysis ";
        if(maxIter.equals(maxIteration) && sigDig.equals(sigDigit))
        {
            modelArchive.isNewVersion = false;
            modelArchive.version = "1." + modelVersion;
        }
        else
        {
            modelArchive.name = modelName;
            modelArchive.isNewVersion = true;
            modelArchive.description = modelAbstract;
            modelArchive.log = JOptionPane.showInputDialog("Enter log for the new version of the model (<=100 characters).  ");
            if(modelArchive.log == null)
                modelArchive.log = "";
            
            // Change source and model
            if(!maxIter.equals(maxIteration))
            {
                beginIndex = model.indexOf("MAXEVALS=", model.indexOf("$ESTIMATION")) + 9;
                endIndex = Math.min(model.indexOf(" ", beginIndex), model.indexOf("\n", beginIndex));
                model = model.substring(0, beginIndex) + maxIter + model.substring(endIndex);
                beginIndex = source.indexOf("mitr=", source.indexOf(analysis)) + 6;
                endIndex = Math.min(source.indexOf(" ", beginIndex), source.indexOf(">", beginIndex)) - 1;
                source = source.substring(0, beginIndex) + maxIter + source.substring(endIndex);
            }
            if(!sigDig.equals(sigDigit))
            {
                beginIndex = model.indexOf("SIGDIGITS=", model.indexOf("$ESTIMATION")) + 10;
                endIndex = Math.min(model.indexOf(" ", beginIndex), model.indexOf("\n", beginIndex));
                model = model.substring(0, beginIndex) + sigDig + model.substring(endIndex);
                beginIndex = source.indexOf("sig_digits=", source.indexOf(analysis)) + 12;
                endIndex = Math.min(source.indexOf(" ", beginIndex), source.indexOf(">", beginIndex)) - 1;
                source = source.substring(0, beginIndex) + sigDig + source.substring(endIndex);                
            }
            modelArchive.text = model;
        }
        
        // Set is_restart="yes" in source
        beginIndex= source.indexOf("is_restart=", source.indexOf(analysis));
        if(beginIndex != -1)
        {
            endIndex = Math.min(source.indexOf(" ", beginIndex), source.indexOf(">", beginIndex));
            source = source.substring(0, beginIndex) + source.substring(endIndex);
        }
        source = source.replaceFirst(analysis, analysis + "is_restart=\"yes\" ");
        
        // Ask the user if an end-job email notice is requested
        boolean isMailNotice = false;
        if(JOptionPane.showConfirmDialog(null, "Do you want to receive an email notice when the job has finished?",
                                         "Question", JOptionPane.YES_NO_OPTION) == 0)
            isMailNotice = true;
        
        // submit the warm start job
        frame.server.submitJob(source, jobAbstract, modelArchive, dataArchive, methodCode, id, true, isMailNotice);

        // Close the dialog
        warmStartDialog.dispose();
    }//GEN-LAST:event_OKButtonActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        warmStartDialog.dispose();
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void jButton7ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton7ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Properties spkInput = frame.server.getInput(id, isLibrary);
        frame.jobId = id;
        if(spkInput != null)
        {
            // Get SPK input file
            source = spkInput.getProperty("source");
            model = spkInput.getProperty("model");
            dataset = spkInput.getProperty("dataset");
            jTextArea7.setText(jobAbstract);
            int beginIndex = model.indexOf("MAXEVALS=", model.indexOf("$ESTIMATION")) + 9;
            int endIndex = Math.min(model.indexOf(" ", beginIndex), (model + "\n").indexOf("\n", beginIndex));
            maxIteration = model.substring(beginIndex, endIndex);
            jTextField8.setText(maxIteration);
            beginIndex = model.indexOf("SIGDIGITS=", model.indexOf("$ESTIMATION")) + 10;
            endIndex = Math.min(model.indexOf(" ", beginIndex), (model + "\n").indexOf("\n", beginIndex));
            sigDigit = model.substring(beginIndex, endIndex);
            jComboBox1.setSelectedItem(sigDigit);
            warmStartDialog.setSize(290, 230);
            warmStartDialog.show();
        }
        setCursor(null);
    }//GEN-LAST:event_jButton7ActionPerformed

    private void jButton6ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton6ActionPerformed
        if(jobParent != 0)
            new JobInfo(frame, jobParent, isLibrary, true);
        else
            JOptionPane.showMessageDialog(null, "This job has no parent.",
                                          "Information Message", JOptionPane.INFORMATION_MESSAGE);
    }//GEN-LAST:event_jButton6ActionPerformed

    private void jButton5ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton5ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        String[] header = {"Event Time", "State Code", "Host"};
        String[][] history = frame.server.getHistory(id, isLibrary);
        if(history != null)
        { 
            jTable1.setModel(new DefaultTableModel(history, header));
            TableColumnModel columnModel = jTable1.getColumnModel();
            columnModel.getColumn(0).setPreferredWidth(250);
            columnModel.getColumn(1).setPreferredWidth(150);
            columnModel.getColumn(2).setPreferredWidth(250);
            historyDialog.setTitle("Job History - Job ID: " + id);
            historyDialog.setLocation(200, 200);
            historyDialog.setSize(650, 16 * history.length + 60);
            historyDialog.show();
        }
        setCursor(null);
    }//GEN-LAST:event_jButton5ActionPerformed

    private void jButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton4ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        if(isFirst)
        {
            spkOutput = frame.server.getOutput(id, isLibrary);
            if(spkOutput != null)
            {
                // Get SPK output file
                spkOutput.setProperty("jobId", String.valueOf(id));
                isFirst = false;
                reports = spkOutput.getProperty("report").split("<spkreport");
            }
        }
        if(reports != null && !reports.equals(""))
        {
            String text = "";
            if(reports.length > 2)
            {
                Object[] subProblems = new String[reports.length];
                subProblems[0] = "Parameter-All";
                for(int i = 1; i < reports.length; i++)
                    subProblems[i] = "Sub-report " + i;
                subReport = (String)JOptionPane.showInputDialog(null, "Select a report:",
                                                      "Sub-report Selection",
                                                      JOptionPane.INFORMATION_MESSAGE, 
                                                      null, subProblems, subProblems[0]);
                if(subReport == null)
                {
                    setCursor(null);
                    return;   
                }
                if(subReport.equals("Parameter-All"))
                {
                    text = ParameterAll.integrateReports(reports, 1);
                }
                else
                {
                    int subId = Integer.parseInt(subReport.substring(11));
                    String report = "<?xml version=\"1.0\"?>\n<spkreport" + reports[subId];
                    if(subId < reports.length - 1)
                        report = report.substring(0, report.lastIndexOf("<?xml "));
                    spkOutput.setProperty("report", report);
                    text = XMLWriter.setOutput(spkOutput);
                }
            }
            else
                text = XMLWriter.setOutput(spkOutput);
   
            // Handle the output file
            if(jCheckBox2.isSelected())
            {
                if(subReport.equals("Parameter-All"))
                {
                    frame.dataBlock = ParameterAll.getParameterAll(spkOutput.getProperty("source"), text);                
                    frame.saveFile();
                    frame.setEditorTitle("Report Data: Job-" + id);
                    frame.setEditorText(frame.dataBlock);
                    frame.setEditorCaretPosition(0);
                }
                else
                    frame.readOutput(text);
            }
            if(!jCheckBox2.isSelected()) 
            {
                frame.saveFile();
            
                if(subReport.equals("Parameter-All"))
                {
                    text += spkOutput.getProperty("source");
                    frame.setEditorTitle("Report Data: Job-" + id);
                }
                else
                    frame.setEditorTitle("Output");
                frame.setEditorText(text);            
                frame.setEditorCaretPosition(0);        
                frame.file = null;
            }
        }
        else
            JOptionPane.showMessageDialog(null, "Report was not found."); 
        setCursor(null);        
    }//GEN-LAST:event_jButton4ActionPerformed

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Properties spkInput = frame.server.getInput(id, isLibrary);
        frame.jobId = jCheckBox3.isSelected() ? id : 0;
        if(spkInput != null)
        {
            // Get SPK input file
            source = spkInput.getProperty("source");
            model = spkInput.getProperty("model");
            dataset = spkInput.getProperty("dataset");
            frame.recentModel.id = modelId;
            frame.recentModel.name = modelName;
            frame.recentModel.text = model;
            frame.recentModel.version = modelVersion;
            frame.recentModel.description = modelAbstract;
            frame.recentDataset.id = datasetId;
            frame.recentDataset.name = datasetName;
            frame.recentDataset.text = dataset;
            frame.recentDataset.version = datasetVersion;
            frame.recentDataset.description = datasetAbstract;
            
            // Handle the input file
            if(jCheckBox1.isSelected())
            {
                // Start preparing input wizard
                frame.iterator = new MDAIterator(frame.server,
                                                 frame.isOnline, frame, frame.isTester,
                                                 frame.isDeveloper, frame.files, id);
                frame.iterator.setIsDataXML(true);
                frame.iterator.setIsReload(true);
                if(id != 0 && endCode.equals("srun") && source.indexOf("<pop_analysis ") != -1 && source.indexOf(" is_estimation=\"yes\" ") != -1 && 
                   JOptionPane.showConfirmDialog(null, "Do you want to set parameter initial values to the estimates from the parent job?",
                                                 "Question", JOptionPane.YES_NO_OPTION) == 0)
                {
                    // Get report
                    Properties spkOutput = frame.server.getOutput(id, false);
                    String[] reports = spkOutput.getProperty("report").split("<spkreport");
                    // Use the last report
                    if(reports.length > 2)
                        JOptionPane.showMessageDialog(null, "The parent job has multiple reports.\n" +
                                                      "The final estimates of the parameters from\n" +
                                                      "the last report will be used to initialze this job.");
                    String report = "<?xml version=\"1.0\">\n<spkreport" + reports[reports.length - 1];
                    if(report.indexOf("<error_message>") != -1)
                    {
                        JOptionPane.showMessageDialog(null, "The parent job, Job ID = " + id + ", has error.",
                                                      "Input Error", JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                    model = Utility.replaceModelParameters(model, report);           

                    if(model.indexOf("$SIMULATION") != -1 &&
                       JOptionPane.showConfirmDialog(null, "Do you want to use the simulated data from the parent job?",
                                                     "Question", JOptionPane.YES_NO_OPTION) == 0)
                        Utility.replaceDataDVbySimDV(report, source, dataset);
                }
                String method = null;
                String covTheta = null;
                if(methodCode.equals("s2") || methodCode.equals("i2")|| methodCode.equals("g2") ||
                   methodCode.equals("sm") || methodCode.equals("im")|| methodCode.equals("gm"))
                {
                    frame.iterator.setIsTwoStage(true);
                    frame.iterator.setIsInd(false);
                    if(methodCode.equals("s2")) method = "std_two_stage";
                    if(methodCode.equals("i2")) method = "iterative_two_stage";
                    if(methodCode.equals("g2")) method = "global_two_stage";
                    if(methodCode.equals("sm")) method = "map_bayes_std_two_stage";
                    if(methodCode.equals("im")) method = "map_bayes_iterative_two_stage";
                    if(methodCode.equals("gm")) method = "map_bayes_global_two_stage";
                    if(method.startsWith("map"))
                    {
                        covTheta = Utility.getOmegaValues(source);
                        if(covTheta == null)
                        {
                            JOptionPane.showMessageDialog(null, "Omega is not found in source.",
                                                          "Input Error", JOptionPane.ERROR_MESSAGE);
                            return;
                        }
                    }
               }
                else
                {
                    frame.iterator.setIsTwoStage(false);
                }
                frame.iterator.setDataXML(dataset, 0);
                frame.iterator.parseControl(model, method, covTheta);
                frame.writeInput(frame.iterator);
                frame.iterator.getGettingStarted().setOptions();
            }
            else
            {
                // Save the text in the editor
                frame.saveFile();
                
                // Display the file content and name
                frame.setEditorText(spkInput.getProperty("source") + dataset +
                                    "\n" + XMLWriter.setModel(model));
                frame.setEditorCaretPosition(0);
                frame.setEditorTitle("Input");
                frame.file = null;
            }
        }
        setCursor(null);
    }//GEN-LAST:event_jButton3ActionPerformed

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Properties archive = frame.server.getJobArchive(id, "data", isLibrary);
        if(archive != null)
        {
            // Save the text in the editor
            frame.saveFile();
                
            // Display the file content and name
            String text = archive.getProperty("text");
            frame.setEditorText(XMLReader.parseDataXML(text, true)); 
            frame.setEditorCaretPosition(0);
            frame.setEditorTitle(archive.getProperty("name") + "." +
                                 archive.getProperty("version"));   
            frame.file = null;
            frame.recentDataset.id = datasetId;
            frame.recentDataset.name = datasetName;
            frame.recentDataset.text = text;            
            frame.recentDataset.version = datasetVersion;
            frame.recentDataset.description = datasetAbstract;
        }
        setCursor(null);
    }//GEN-LAST:event_jButton2ActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Properties archive = frame.server.getJobArchive(id, "model", isLibrary);
        if(archive != null)
        {
            // Save the text in the editor
            frame.saveFile();
                
            // Display the file content and name
            String text = archive.getProperty("text");
            frame.setEditorText(text); 
            frame.setEditorCaretPosition(0);
            frame.setEditorTitle(archive.getProperty("name") + "." +
                                 archive.getProperty("version"));   
            frame.file = null;
            frame.recentModel.id = modelId;
            frame.recentModel.name = modelName;
            frame.recentModel.text = text;
            frame.recentModel.version = modelVersion;
            frame.recentModel.description = modelAbstract;            
        }
        setCursor(null);        
    }//GEN-LAST:event_jButton1ActionPerformed

    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
//        System.exit(0);
    }//GEN-LAST:event_exitForm

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton OKButton;
    private javax.swing.JButton cancelButton;
    private javax.swing.JDialog historyDialog;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JButton jButton4;
    private javax.swing.JButton jButton5;
    private javax.swing.JButton jButton6;
    private javax.swing.JButton jButton7;
    private javax.swing.JButton jButton8;
    private javax.swing.JButton jButton9;
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JCheckBox jCheckBox2;
    private javax.swing.JCheckBox jCheckBox3;
    private javax.swing.JComboBox jComboBox1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextArea jTextArea2;
    private javax.swing.JTextArea jTextArea3;
    private javax.swing.JTextArea jTextArea5;
    private javax.swing.JTextArea jTextArea6;
    private javax.swing.JTextArea jTextArea7;
    private javax.swing.JTextArea jTextArea8;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextField jTextField4;
    private javax.swing.JTextField jTextField5;
    private javax.swing.JTextField jTextField6;
    private javax.swing.JTextField jTextField7;
    private javax.swing.JTextField jTextField8;
    private javax.swing.JDialog warmStartDialog;
    // End of variables declaration//GEN-END:variables
    
    /** Job ID. */
    protected long id = 0;
    
    // Job source
    private String source;
    
    // Job model
    private String model;
    
    // Job dataset
    private String dataset;
    
    /** Job method code */
    protected String methodCode;
    
    // State code
    private String stateCode = "";
    
    /** End code */
    protected String endCode = "";
    
    /** Model ID */
    protected long modelId = 0;    
    
    /** Model name */
    protected String modelName;
    
    /** Model abstract */
    protected String modelAbstract;  
    
    /** Model version */
    protected String modelVersion;
    
    /** Dataset ID */
    protected long datasetId = 0; 
    
    /** Dataset name */
    protected String datasetName;
    
    /** Dataset abstract */
    protected String datasetAbstract;
    
    /** Dataset version */
    protected String datasetVersion;
        
    // Is library
    private boolean isLibrary = false;
    
    // MDA frame reference
    private MDAFrame frame;
    
    // Pareant job ID
    private long jobParent = 0;
        
    // Job abstract
    private String jobAbstract;
    
    // Job owner
    private String jobOwner;    
    
    // Maximum number of iterations
    private String maxIteration;
    
    // Number of significant digit
    private String sigDigit;
    
    // Reports of the job
    private String[] reports;
    
    // Is first time click the Output button
    private boolean isFirst = true;
    
    // Spk output
    private Properties spkOutput;
    
    /** Sub-report */
    protected String subReport = "";
}
