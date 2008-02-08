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
package uw.rfpk.mda.nonmem.compartment;

import uw.rfpk.mda.nonmem.Utility;
import java.awt.Dimension;
import java.awt.Cursor;
import java.util.regex.*;
import javax.swing.JOptionPane;
import javax.help.*;
import uw.rfpk.mda.nonmem.MDAFrame;

/** This class defines mixed effects model dialog.
 *
 * @author  Jiaji Du
 */
public class MixedModelDialog extends javax.swing.JDialog {
    
    /** Creates new form ModelModelDialog.     
     * @param parent parent of this dialog.
     * @param parameter a parameter object containing model name and value.
     * @param dataLabels a String array containing data labels of the dataset.
     * @param isOK true if the OK button is clicked, false otherwise.
     */
    public MixedModelDialog(java.awt.Frame parent, Parameter parameter, String[] dataLabels, int[] isOK)
    {
        super(parent, true);
        this.parameter = parameter;
        this.checkError = checkError;
        this.isOK = isOK;
        initComponents();
        helpButton.addActionListener(new CSH.DisplayHelpFromSource(MDAFrame.getHelpBroker()));
        CSH.setHelpIDString(helpButton, "Mixed_Effects_Model_Graphical_Editor_");
        nameLabel.setText("Name: " + parameter.name);
        for(int i = 0; i < dataLabels.length; i++)
        {
            String dataName = dataLabels[i].split("=")[0];
            if(!Utility.isStdItem(dataName) && !dataName.equals("ID"))
            {
                dataNameComboBox.addItem(dataName);
                dataNameComboBox1.addItem(dataName);
            }
        }
        if(parameter.name.length() == 0)
            userDefinedTextArea.setText(parameter.name + "=");
        else
            userDefinedTextArea.setText(parameter.value);
        userDefinedRadioButton.setSelected(true);
//        helpButton.addActionListener(new CSH.DisplayHelpFromSource(MDAFrame.getHelpBroker()));
//        CSH.setHelpIDString(helpButton, "Prepare_Input_Model_Parameters");
        Dimension wndSize = getToolkit().getScreenSize();
        userDefinedTextArea.setEditable(parameter.value.indexOf("IF(") == -1);
        setLocation(wndSize.width/2, wndSize.height/3);
        setSize(410, 520);
        setVisible(true);
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        buttonGroup1 = new javax.swing.ButtonGroup();
        jDialog1 = new javax.swing.JDialog();
        jLabel14 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jLabel15 = new javax.swing.JLabel();
        jLabel16 = new javax.swing.JLabel();
        jLabel17 = new javax.swing.JLabel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTextArea2 = new javax.swing.JTextArea();
        jPanel2 = new javax.swing.JPanel();
        jButton4 = new javax.swing.JButton();
        jButton5 = new javax.swing.JButton();
        jLabel18 = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        jLabel20 = new javax.swing.JLabel();
        jTextField2 = new javax.swing.JTextField();
        jPanel4 = new javax.swing.JPanel();
        jLabel19 = new javax.swing.JLabel();
        jTextField3 = new javax.swing.JTextField();
        dataNameComboBox1 = new javax.swing.JComboBox();
        jLabel1 = new javax.swing.JLabel();
        buttonGroup2 = new javax.swing.ButtonGroup();
        nameLabel = new javax.swing.JLabel();
        jTextArea1 = new javax.swing.JTextArea();
        jLabel2 = new javax.swing.JLabel();
        additiveRadioButton = new javax.swing.JRadioButton();
        proportionalRadioButton = new javax.swing.JRadioButton();
        exponentialRadioButton = new javax.swing.JRadioButton();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        userDefinedRadioButton = new javax.swing.JRadioButton();
        jLabel6 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jLabel10 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        jLabel13 = new javax.swing.JLabel();
        jPanel1 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        helpButton = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        userDefinedTextArea = new javax.swing.JTextArea();
        jLabel21 = new javax.swing.JLabel();
        jRadioButton1 = new javax.swing.JRadioButton();
        jRadioButton2 = new javax.swing.JRadioButton();
        jPanel7 = new javax.swing.JPanel();
        jLabel25 = new javax.swing.JLabel();
        dataNameComboBox = new javax.swing.JComboBox();

        jDialog1.getContentPane().setLayout(new java.awt.GridBagLayout());

        jDialog1.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        jDialog1.setTitle("Conditional Parameter Model");
        jDialog1.setModal(true);
        jLabel14.setText("IF ( ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 5, 0);
        jDialog1.getContentPane().add(jLabel14, gridBagConstraints);

        jTextField1.setPreferredSize(new java.awt.Dimension(230, 22));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 6, 2);
        jDialog1.getContentPane().add(jTextField1, gridBagConstraints);

        jLabel15.setText(") THEN");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 5, 12);
        jDialog1.getContentPane().add(jLabel15, gridBagConstraints);

        jLabel16.setText("ELSE");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 0);
        jDialog1.getContentPane().add(jLabel16, gridBagConstraints);

        jLabel17.setText("Helper Equations:");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 3, 12);
        jDialog1.getContentPane().add(jLabel17, gridBagConstraints);

        jScrollPane2.setPreferredSize(new java.awt.Dimension(353, 83));
        jTextArea2.setColumns(20);
        jTextArea2.setRows(5);
        jTextArea2.setMaximumSize(new java.awt.Dimension(350, 80));
        jTextArea2.setMinimumSize(new java.awt.Dimension(350, 80));
        jTextArea2.setPreferredSize(new java.awt.Dimension(350, 80));
        jScrollPane2.setViewportView(jTextArea2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 11, 9, 11);
        jDialog1.getContentPane().add(jScrollPane2, gridBagConstraints);

        jButton4.setText("OK");
        jButton4.setPreferredSize(new java.awt.Dimension(75, 25));
        jButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton4ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton4);

        jButton5.setText("Cancel");
        jButton5.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton5ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton5);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        jDialog1.getContentPane().add(jPanel2, gridBagConstraints);

        jLabel18.setText("ENDIF");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 11, 0, 0);
        jDialog1.getContentPane().add(jLabel18, gridBagConstraints);

        jPanel3.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 5));

        jLabel20.setText("NAME=");
        jPanel3.add(jLabel20);

        jTextField2.setPreferredSize(new java.awt.Dimension(310, 22));
        jPanel3.add(jTextField2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        jDialog1.getContentPane().add(jPanel3, gridBagConstraints);

        jPanel4.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 5));

        jLabel19.setText("NAME=");
        jPanel4.add(jLabel19);

        jTextField3.setPreferredSize(new java.awt.Dimension(310, 22));
        jPanel4.add(jTextField3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        jDialog1.getContentPane().add(jPanel4, gridBagConstraints);

        dataNameComboBox1.setMinimumSize(new java.awt.Dimension(32, 20));
        dataNameComboBox1.setPreferredSize(new java.awt.Dimension(100, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 12);
        jDialog1.getContentPane().add(dataNameComboBox1, gridBagConstraints);

        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("Select a data item for adding to the model");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 0);
        jDialog1.getContentPane().add(jLabel1, gridBagConstraints);

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Mixed Effects Model");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                closeDialog(evt);
            }
        });

        nameLabel.setText("Name:");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        getContentPane().add(nameLabel, gridBagConstraints);

        jTextArea1.setBackground(new java.awt.Color(238, 238, 238));
        jTextArea1.setText("Select a mixed-effects model:\n  - Model must contain fixed effect parameter THETA\n  - Model may contain random effect parameter ETA\n  - Enter appropriate number in ( ) folowing parameter (e.g.\n     THETA(1), ETA(2) etc.).");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 8, 12);
        getContentPane().add(jTextArea1, gridBagConstraints);

        jLabel2.setText("Model Name");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 0);
        getContentPane().add(jLabel2, gridBagConstraints);

        buttonGroup1.add(additiveRadioButton);
        additiveRadioButton.setText("Additive");
        additiveRadioButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                additiveRadioButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 0);
        getContentPane().add(additiveRadioButton, gridBagConstraints);

        buttonGroup1.add(proportionalRadioButton);
        proportionalRadioButton.setText("Proportional");
        proportionalRadioButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                proportionalRadioButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 0);
        getContentPane().add(proportionalRadioButton, gridBagConstraints);

        buttonGroup1.add(exponentialRadioButton);
        exponentialRadioButton.setText("Exponential");
        exponentialRadioButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                exponentialRadioButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 0);
        getContentPane().add(exponentialRadioButton, gridBagConstraints);

        jLabel3.setText("Expression");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel3, gridBagConstraints);

        jLabel4.setText("Distribution");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel4, gridBagConstraints);

        jLabel5.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel5.setText("THETA + ETA");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel5, gridBagConstraints);

        buttonGroup1.add(userDefinedRadioButton);
        userDefinedRadioButton.setText("User Defined");
        userDefinedRadioButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                userDefinedRadioButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 12, 4, 4);
        getContentPane().add(userDefinedRadioButton, gridBagConstraints);

        jLabel6.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel6.setText("Normal\n");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel6, gridBagConstraints);

        jLabel7.setIcon(new javax.swing.ImageIcon(getClass().getResource("/uw/rfpk/mda/nonmem/compartment/normal.jpg")));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 12);
        getContentPane().add(jLabel7, gridBagConstraints);

        jLabel8.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel8.setText("THETA * (1 + ETA)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 11);
        getContentPane().add(jLabel8, gridBagConstraints);

        jLabel9.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel9.setText("Normal");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel9, gridBagConstraints);

        jLabel10.setIcon(new javax.swing.ImageIcon(getClass().getResource("/uw/rfpk/mda/nonmem/compartment/normal.jpg")));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 12);
        getContentPane().add(jLabel10, gridBagConstraints);

        jLabel11.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel11.setText("THETA * EXP(ETA)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel11, gridBagConstraints);

        jLabel12.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel12.setText("Lognormal");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 5);
        getContentPane().add(jLabel12, gridBagConstraints);

        jLabel13.setIcon(new javax.swing.ImageIcon(getClass().getResource("/uw/rfpk/mda/nonmem/compartment/lognormal.jpg")));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 12);
        getContentPane().add(jLabel13, gridBagConstraints);

        jButton1.setText("OK");
        jButton1.setPreferredSize(new java.awt.Dimension(75, 25));
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton1);

        jButton2.setText("Cancel");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton2);

        helpButton.setText("Help");
        helpButton.setPreferredSize(new java.awt.Dimension(75, 25));
        jPanel1.add(helpButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        getContentPane().add(jPanel1, gridBagConstraints);

        userDefinedTextArea.setMaximumSize(new java.awt.Dimension(350, 80));
        userDefinedTextArea.setMinimumSize(new java.awt.Dimension(350, 80));
        userDefinedTextArea.setPreferredSize(new java.awt.Dimension(350, 80));
        jScrollPane1.setViewportView(userDefinedTextArea);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 3, 12);
        getContentPane().add(jScrollPane1, gridBagConstraints);

        jLabel21.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel21.setText("Edit the model:  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 14);
        getContentPane().add(jLabel21, gridBagConstraints);

        buttonGroup2.add(jRadioButton1);
        jRadioButton1.setText("Conditional");
        jRadioButton1.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        jRadioButton1.setMargin(new java.awt.Insets(0, 0, 0, 0));
        jRadioButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 12);
        getContentPane().add(jRadioButton1, gridBagConstraints);

        buttonGroup2.add(jRadioButton2);
        jRadioButton2.setText("None conditional");
        jRadioButton2.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        jRadioButton2.setMargin(new java.awt.Insets(0, 0, 0, 0));
        jRadioButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jRadioButton2, gridBagConstraints);

        jLabel25.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel25.setText("Select a data item to add to the model");
        jPanel7.add(jLabel25);

        dataNameComboBox.setMinimumSize(new java.awt.Dimension(32, 20));
        dataNameComboBox.setPreferredSize(new java.awt.Dimension(100, 20));
        dataNameComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                dataNameComboBoxActionPerformed(evt);
            }
        });

        jPanel7.add(dataNameComboBox);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 4;
        getContentPane().add(jPanel7, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jRadioButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton2ActionPerformed
        userDefinedTextArea.setEditable(true);
    }//GEN-LAST:event_jRadioButton2ActionPerformed

    private void jRadioButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton1ActionPerformed
        String value = userDefinedTextArea.getText().trim();
        int indexIF = value.indexOf("IF(");
        if(indexIF == -1)
        {
            if(value.indexOf("\n") == -1)
                jTextArea2.setText("");
            else
                jTextArea2.setText(value.substring(0, value.lastIndexOf("\n")));
            jTextField1.setText("");
            jTextField2.setText(value.substring(value.lastIndexOf("=") + 1));
            jTextField3.setText("");
        }
        else
        {
            jTextArea2.setText(value.substring(0, indexIF));
            String[] lines = value.substring(indexIF).split("\n");
            jTextField1.setText(lines[0].substring(lines[0].indexOf("IF(") + 3, lines[0].indexOf(") THEN")));
            jTextField2.setText(lines[1].substring(lines[1].indexOf("=") + 1));
            jTextField3.setText(lines[3].substring(lines[3].indexOf("=") + 1));
        }
        jLabel19.setText(parameter.name + "=");
        jLabel20.setText(parameter.name + "=");
        jDialog1.setSize(410, 380);
        jDialog1.setLocation(getX() + 100, getY() + 100);
        jDialog1.setVisible(true);
    }//GEN-LAST:event_jRadioButton1ActionPerformed

    private void jButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton4ActionPerformed
        String condition = jTextField1.getText().trim();
        if(condition.equals(""))
        {
            JOptionPane.showMessageDialog(null, "Condition statement is missing.");
            return;
        }
        String thenValue = jTextField2.getText().trim();
        if(thenValue.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The THEN statement is missing.");
            return;
        }
        String elseValue = jTextField3.getText().trim();
        if(elseValue.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The ELSE statement is missing.");
            return;
        }
        StringBuffer buffer = new StringBuffer();
        buffer.append(jTextArea2.getText().trim());
        buffer.append("\nIF(");
        buffer.append(condition);
        buffer.append(") THEN");
        buffer.append("\n");
        buffer.append(parameter.name);
        buffer.append("=");
        buffer.append(thenValue);
        buffer.append("\nELSE");
        buffer.append("\n");
        buffer.append(parameter.name);
        buffer.append("=");
        buffer.append(elseValue);
        buffer.append("\nENDIF");
        userDefinedTextArea.setText(buffer.toString().trim());
        userDefinedTextArea.setEditable(false);
        jDialog1.setVisible(false);
    }//GEN-LAST:event_jButton4ActionPerformed

    private void jButton5ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton5ActionPerformed
        jDialog1.setVisible(false);
    }//GEN-LAST:event_jButton5ActionPerformed

    private void dataNameComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_dataNameComboBoxActionPerformed
        String data = (String)dataNameComboBox.getSelectedItem();
        userDefinedTextArea.insert(data, userDefinedTextArea.getCaretPosition());
    }//GEN-LAST:event_dataNameComboBoxActionPerformed

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        isOK[0] = 0;
        setVisible(false);
    }//GEN-LAST:event_jButton2ActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        String text = userDefinedTextArea.getText().trim().toUpperCase();
        if(userDefinedTextArea.isEditable())
        {
            if(Pattern.compile("\\bIF\\b", Pattern.UNIX_LINES).matcher(text).find())
            {
                JOptionPane.showMessageDialog(null, "You must select the Conditional button to enter conditional model.",
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
                setCursor(null);
                return;
            }
            text = "\n" + text;
            if(!text.substring(text.lastIndexOf("\n")).trim().replaceAll(" ", "").startsWith(parameter.name + "="))
            {
                JOptionPane.showMessageDialog(null, "\"" + parameter.name + "=\" is missing or other error is in the model.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                setCursor(null);
                return;
            }
        } 
        if(Pattern.compile("\\bTHETA\\(\\)", Pattern.UNIX_LINES).matcher(text).find())
        {
            JOptionPane.showMessageDialog(null, "THETA Number is missing.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            setCursor(null);
            return;
        }
        if(Pattern.compile("\\bETA\\(\\)", Pattern.UNIX_LINES).matcher(text).find())
        {
            JOptionPane.showMessageDialog(null, "ETA Number is missing.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            setCursor(null);
            return;
        }
        
        parameter.value = text.trim();
        isOK[0] = 1;
        setVisible(false);
        setCursor(null);
    }//GEN-LAST:event_jButton1ActionPerformed

    private void userDefinedRadioButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_userDefinedRadioButtonActionPerformed
        userDefinedTextArea.setText("\n\n\n" + parameter.name + "=");
    }//GEN-LAST:event_userDefinedRadioButtonActionPerformed

    private void exponentialRadioButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_exponentialRadioButtonActionPerformed
        userDefinedTextArea.setText("\n\n\n" + parameter.name + "=THETA()*EXP(ETA())");
    }//GEN-LAST:event_exponentialRadioButtonActionPerformed

    private void proportionalRadioButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_proportionalRadioButtonActionPerformed
        userDefinedTextArea.setText("\n\n\n" + parameter.name + "=THETA()+THETA()*ETA()");
    }//GEN-LAST:event_proportionalRadioButtonActionPerformed

    private void additiveRadioButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_additiveRadioButtonActionPerformed
        userDefinedTextArea.setText("\n\n\n" + parameter.name + "=THETA()+ETA()");
    }//GEN-LAST:event_additiveRadioButtonActionPerformed
    
    /** Closes the dialog */
    private void closeDialog(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_closeDialog
        setVisible(false);
        dispose();
    }//GEN-LAST:event_closeDialog
    
    /** The main function to test the dialog.
     * @param args the command line arguments which are not being used.
     */
    public static void main(String args[]) {
        Parameter parameter = new Parameter("Name", "Name=Expression");
        String[] dataLabels = {"ID", "TIME", "DV", "AMT", "WT", "DT"};
        int[] isOK = {1};
        new MixedModelDialog(new DesignTool(), parameter, dataLabels, isOK);
        System.out.println(isOK[0]);
        System.out.println(parameter.name + ": " + parameter.value);
    }
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JRadioButton additiveRadioButton;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.JComboBox dataNameComboBox;
    private javax.swing.JComboBox dataNameComboBox1;
    private javax.swing.JRadioButton exponentialRadioButton;
    private javax.swing.JButton helpButton;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton4;
    private javax.swing.JButton jButton5;
    private javax.swing.JDialog jDialog1;
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
    private javax.swing.JLabel jLabel25;
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
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JRadioButton jRadioButton1;
    private javax.swing.JRadioButton jRadioButton2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextArea jTextArea2;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JLabel nameLabel;
    private javax.swing.JRadioButton proportionalRadioButton;
    private javax.swing.JRadioButton userDefinedRadioButton;
    private javax.swing.JTextArea userDefinedTextArea;
    // End of variables declaration//GEN-END:variables
    
    private Parameter parameter;
    private boolean checkError;
    private int[] isOK;
}
