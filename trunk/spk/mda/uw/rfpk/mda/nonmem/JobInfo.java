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
        initComponents();
        Properties jobInfo = frame.server.getJobInfo(id, isLibrary);
        if(jobInfo == null)
            return;
        modelName = jobInfo.getProperty("modelName");
        datasetName = jobInfo.getProperty("datasetName");
        modelVersion = jobInfo.getProperty("modelVersion");
        datasetVersion = jobInfo.getProperty("datasetVersion");
        modelId = jobInfo.getProperty("modelId");
        datasetId = jobInfo.getProperty("datasetId");
        modelAbstract = jobInfo.getProperty("modelAbstract");
        datasetAbstract = jobInfo.getProperty("datasetAbstract");
        methodCode = jobInfo.getProperty("methodCode");
        jTextField1.setText(modelName);
        jTextField3.setText(datasetName);
        jTextField2.setText(modelVersion);        
        jTextField4.setText(datasetVersion);
        String[] row = (String[])frame.methodTable.get(methodCode);
        if(row == null)
            row = new String[]{"Not Available", ""};
        jTextField5.setText(row[0]); 
        jTextField6.setText(String.valueOf(jobId));
        jobParent = Long.parseLong(jobInfo.getProperty("parent"));
        jCheckBox1.setSelected(true);
        jCheckBox2.setSelected(true);
        jCheckBox3.setSelected(!row[1].equals("le"));
        jCheckBox3.setEnabled(!row[1].equals("le"));        
        if(isParent)
        {
            java.awt.Point point = getLocation();
            point.translate(40, 30);
            setLocation(point);            
        }
        show();
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
        jLabel5 = new javax.swing.JLabel();
        jButton6 = new javax.swing.JButton();
        jTextField5 = new javax.swing.JTextField();
        jLabel6 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jCheckBox3 = new javax.swing.JCheckBox();
        jTextArea4 = new javax.swing.JTextArea();
        jTextField6 = new javax.swing.JTextField();
        jTextArea5 = new javax.swing.JTextArea();

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

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Job Information");
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
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 6, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jTextArea1, gridBagConstraints);

        jLabel1.setText("Name");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel1, gridBagConstraints);

        jLabel2.setText("Version");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jLabel2, gridBagConstraints);

        jLabel3.setText("Model");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 2, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel3, gridBagConstraints);

        jLabel4.setText("Dataset");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.insets = new java.awt.Insets(4, 12, 7, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel4, gridBagConstraints);

        jTextField1.setEditable(false);
        jTextField1.setFont(new java.awt.Font("Monospaced", 0, 12));
        jTextField1.setFocusable(false);
        jTextField1.setMaximumSize(new java.awt.Dimension(160, 19));
        jTextField1.setMinimumSize(new java.awt.Dimension(160, 19));
        jTextField1.setPreferredSize(new java.awt.Dimension(160, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
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
        gridBagConstraints.gridy = 2;
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
        gridBagConstraints.gridy = 3;
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
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 12, 7, 12);
        getContentPane().add(jTextField4, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
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
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(7, 12, 0, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
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
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(7, 12, 6, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jPanel1, gridBagConstraints);

        jCheckBox1.setFont(new java.awt.Font("Dialog", 0, 12));
        jCheckBox1.setSelected(true);
        jCheckBox1.setText("Start input preparation tool when Input button is clicked");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jCheckBox1, gridBagConstraints);

        jCheckBox2.setFont(new java.awt.Font("Dialog", 0, 12));
        jCheckBox2.setSelected(true);
        jCheckBox2.setText("Start output data processing when Output button is clicked");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jCheckBox2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jSeparator2, gridBagConstraints);

        jTextArea3.setBackground(new java.awt.Color(204, 204, 204));
        jTextArea3.setText("You may get the job's processing history.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 13;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(10, 12, 8, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
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

        jLabel5.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel5.setText("You may also get the job's parent job.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(3, 12, 8, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel5, gridBagConstraints);

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
        gridBagConstraints.gridy = 12;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jTextField5, gridBagConstraints);

        jLabel6.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel6.setText("Method");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.insets = new java.awt.Insets(8, 12, 0, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel6, gridBagConstraints);

        jLabel7.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel7.setText("is used by the job.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.insets = new java.awt.Insets(8, 12, 0, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel7, gridBagConstraints);

        jCheckBox3.setFont(new java.awt.Font("Dialog", 0, 12));
        jCheckBox3.setSelected(true);
        jCheckBox3.setText("Set the job as parent of a job created from this job's input");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 6, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jCheckBox3, gridBagConstraints);

        jTextArea4.setBackground(new java.awt.Color(204, 204, 204));
        jTextArea4.setText("The identification number of the job is:");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 12, 12, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jTextArea4, gridBagConstraints);

        jTextField6.setEditable(false);
        jTextField6.setFont(new java.awt.Font("Monospaced", 0, 12));
        jTextField6.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
        jTextField6.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 14, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jTextField6, gridBagConstraints);

        jTextArea5.setBackground(new java.awt.Color(204, 204, 204));
        jTextArea5.setText("To create input for a likelihood evaluation only job uncheck the\nfirst check box and make sure the third check box is checked.");
        jTextArea5.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 2, 12);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jTextArea5, gridBagConstraints);

        pack();
    }//GEN-END:initComponents

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
            historyDialog.setTitle("Job History");
            historyDialog.setLocation(200, 200);
            historyDialog.setSize(650, 16 * history.length + 60);
            historyDialog.show();
        }
        setCursor(null);
    }//GEN-LAST:event_jButton5ActionPerformed

    private void jButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton4ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Properties spkOutput = frame.server.getOutput(id, isLibrary);
        if(spkOutput != null) 
        {
            // Get SPK output file
            spkOutput.setProperty("jobId", String.valueOf(id));
            String text = XMLWriter.setOutput(spkOutput);
            
            // Handle the output file
            if(jCheckBox2.isSelected())
            {
                frame.readOutput(text);
            }
            else
            {
                // Save the text in the editor
                frame.saveFile();
                
                // Display the file content and name
                frame.setEditorText(text); 
                frame.setEditorCaretPosition(0);
                frame.setEditorTitle("Output");   
                frame.file = null;
            }
        }
        setCursor(null);        
    }//GEN-LAST:event_jButton4ActionPerformed

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Properties spkInput = frame.server.getInput(id, isLibrary);
        frame.jobId = jCheckBox3.isSelected() ? id : 0;
        if(spkInput != null)
        {
            // Get SPK input file
            String source = spkInput.getProperty("source");
            String model = spkInput.getProperty("model");
            String dataset = spkInput.getProperty("dataset");            
            
            // Handle the input file
            if(jCheckBox1.isSelected())
            {
                // Start preparing input wizard
                MDAIterator iterator = new MDAIterator(frame.server, 
                                                       frame.isOnline, frame, frame.isTester, 
                                                       frame.isDeveloper, frame.files, id);
                iterator.setIsDataXML(true);
                iterator.setIsReload(true);
                if(id != 0 && source.indexOf("<pop_analysis ") != -1 && source.indexOf(" is_estimation=\"yes\" ") != -1 && 
                   JOptionPane.showConfirmDialog(null, "Do you want to set parameter initial values to the estimates from the parent job?",
                                                 "Question", JOptionPane.YES_NO_OPTION) == 0)
                {
                    // Get report
                    Properties spkOutput = frame.server.getOutput(id, false);
                    String report = spkOutput.getProperty("report");
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
                iterator.setDataXML(dataset);
                iterator.parseControl(model);
                frame.writeInput(iterator);
                iterator.getGettingStarted().setOptions();
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
            frame.setEditorText(XMLReader.parseDataXML(archive.getProperty("text"))); 
            frame.setEditorCaretPosition(0);
            frame.setEditorTitle(archive.getProperty("name") + "." +
                                   archive.getProperty("version"));   
            frame.file = null;            
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
            frame.setEditorText(archive.getProperty("text")); 
            frame.setEditorCaretPosition(0);
            frame.setEditorTitle(archive.getProperty("name") + "." +
                                 archive.getProperty("version"));   
            frame.file = null;            
        }
        setCursor(null);        
    }//GEN-LAST:event_jButton1ActionPerformed

    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
//        System.exit(0);
    }//GEN-LAST:event_exitForm

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JDialog historyDialog;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JButton jButton4;
    private javax.swing.JButton jButton5;
    private javax.swing.JButton jButton6;
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JCheckBox jCheckBox2;
    private javax.swing.JCheckBox jCheckBox3;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextArea jTextArea2;
    private javax.swing.JTextArea jTextArea3;
    private javax.swing.JTextArea jTextArea4;
    private javax.swing.JTextArea jTextArea5;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextField jTextField4;
    private javax.swing.JTextField jTextField5;
    private javax.swing.JTextField jTextField6;
    // End of variables declaration//GEN-END:variables
    
    // Job ID
    private long id = 0;
    
    // Job method code
    protected String methodCode = null;
    
    /** Model ID */
    protected String modelId = null;    
    
    /** Model name */
    protected String modelName = null;
    
    /** Model abstract */
    protected String modelAbstract = null;  
    
    /** Model version */
    protected String modelVersion = null;
    
    /** Dataset ID */
    protected String datasetId = null; 
    
    /** Dataset name */
    protected String datasetName = null;
    
    /** Dataset abstract */
    protected String datasetAbstract = null;
    
    /** Dataset version */
    protected String datasetVersion = null;
        
    // Is library
    private boolean isLibrary = false;
    
    // MDA frame reference
    private MDAFrame frame = null;
    
    // Pareant job ID
    private long jobParent = 0;
}
