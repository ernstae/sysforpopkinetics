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

import uw.rfpk.mda.nonmem.Utility;
import org.netbeans.ui.wizard.*;
import java.util.Properties;
import javax.swing.JComponent;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.io.*; 

/**
 * This class defines a step to collect general information.
 * @author  Jiaji Du
 */
public class GettingStarted extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private MDAIterator iterator = null;
    private JWizardPane wizardPane = null;
    private String subroutine = null;
    private boolean isValid = false;

    /** Creates new form GettingStarted.
     * @param iter a MDAIterator object to initialize the field iterator.
     */
    public GettingStarted(MDAIterator iter) {
        iterator = iter;
        initComponents();
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        buttonGroup1 = new javax.swing.ButtonGroup();
        buttonGroup2 = new javax.swing.ButtonGroup();
        jPopupMenu1 = new javax.swing.JPopupMenu();
        jMenuItem1 = new javax.swing.JMenuItem();
        jMenuItem2 = new javax.swing.JMenuItem();
        jMenuItem3 = new javax.swing.JMenuItem();
        jMenuItem4 = new javax.swing.JMenuItem();
        jMenuItem5 = new javax.swing.JMenuItem();
        jMenuItem6 = new javax.swing.JMenuItem();
        jMenuItem7 = new javax.swing.JMenuItem();
        jMenuItem8 = new javax.swing.JMenuItem();
        jMenuItem9 = new javax.swing.JMenuItem();
        jMenuItem10 = new javax.swing.JMenuItem();
        jMenuItem11 = new javax.swing.JMenuItem();
        jMenuItem12 = new javax.swing.JMenuItem();
        jTextPane1 = new javax.swing.JTextPane();
        jRadioButton1 = new javax.swing.JRadioButton();
        jRadioButton2 = new javax.swing.JRadioButton();
        jRadioButton3 = new javax.swing.JRadioButton();
        jRadioButton4 = new javax.swing.JRadioButton();
        jRadioButton5 = new javax.swing.JRadioButton();
        jTextPane2 = new javax.swing.JTextPane();
        jCheckBox1 = new javax.swing.JCheckBox();
        jCheckBox2 = new javax.swing.JCheckBox();
        jSeparator1 = new javax.swing.JSeparator();
        jCheckBox3 = new javax.swing.JCheckBox();
        jCheckBox4 = new javax.swing.JCheckBox();
        jCheckBox5 = new javax.swing.JCheckBox();
        jCheckBox6 = new javax.swing.JCheckBox();
        jSeparator2 = new javax.swing.JSeparator();
        jSeparator3 = new javax.swing.JSeparator();
        jTextPane3 = new javax.swing.JTextPane();
        jButton1 = new javax.swing.JButton();
        jSeparator4 = new javax.swing.JSeparator();
        jSeparator5 = new javax.swing.JSeparator();
        jLabel1 = new javax.swing.JLabel();

        jPopupMenu1.setBackground(new java.awt.Color(255, 255, 255));
        jPopupMenu1.setAutoscrolls(true);
        jPopupMenu1.addPopupMenuListener(new javax.swing.event.PopupMenuListener() {
            public void popupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {
                jPopupMenu1PopupMenuCanceled(evt);
            }
            public void popupMenuWillBecomeInvisible(javax.swing.event.PopupMenuEvent evt) {
            }
            public void popupMenuWillBecomeVisible(javax.swing.event.PopupMenuEvent evt) {
            }
        });

        jMenuItem1.setText("SUBROUTINE ADVAN1");
        jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem1ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem1);

        jMenuItem2.setText("SUBROUTINE ADVAN2");
        jMenuItem2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem2ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem2);

        jMenuItem3.setText("SUBROUTINE ADVAN3");
        jMenuItem3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem3ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem3);

        jMenuItem4.setText("SUBROUTINE ADVAN4");
        jMenuItem4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem4ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem4);

        jMenuItem5.setText("SUBROUTINE ADVAN5");
        jMenuItem5.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem5ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem5);

        jMenuItem6.setText("SUBROUTINE ADVAN6");
        jMenuItem6.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem6ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem6);

        jMenuItem7.setText("SUBROUTINE ADVAN7");
        jMenuItem7.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem7ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem7);

        jMenuItem8.setText("SUBROUTINE ADAVN8");
        jMenuItem8.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem8ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem8);

        jMenuItem9.setText("SUBROUTINE ADVAN9");
        jMenuItem9.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem9ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem9);

        jMenuItem10.setText("SUBROUTINE ANVAN10");
        jMenuItem10.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem10ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem10);

        jMenuItem11.setText("SUBROUTINE ADVAN11");
        jMenuItem11.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem11ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem11);

        jMenuItem12.setText("SUBROUTINE ADVAN12");
        jMenuItem12.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem12ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem12);

        setLayout(new java.awt.GridBagLayout());

        jTextPane1.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane1.setEditable(false);
        jTextPane1.setText("This tool is guiding you in the creation of a structural - statistical mixed \neffect model.  The tool converts the model in a NONMEM control file and a\nSPK input file .  Select the following items and then click the \"Next\" button.\n(Note:  This tool is not for preparing a likelihood evaluation only job.)");
        jTextPane1.setFocusCycleRoot(false);
        jTextPane1.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 24, 0, 24);
        add(jTextPane1, gridBagConstraints);

        jRadioButton1.setText("Performing individual analysis");
        buttonGroup1.add(jRadioButton1);
        jRadioButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(6, 20, 0, 6);
        add(jRadioButton1, gridBagConstraints);

        jRadioButton2.setText("Performing population analysis");
        buttonGroup1.add(jRadioButton2);
        jRadioButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 20, 0, 6);
        add(jRadioButton2, gridBagConstraints);

        jRadioButton3.setText("Defining a new model");
        buttonGroup2.add(jRadioButton3);
        jRadioButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton3ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(0, 20, 0, 6);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jRadioButton3, gridBagConstraints);

        jRadioButton4.setText("Using the RFPK model library");
        buttonGroup2.add(jRadioButton4);
        jRadioButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton4ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(0, 20, 0, 6);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jRadioButton4, gridBagConstraints);

        jRadioButton5.setText("Computing statistics of estimates");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(0, 20, 11, 6);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        add(jRadioButton5, gridBagConstraints);

        jTextPane2.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane2.setEditable(false);
        jTextPane2.setText("First, one of the check boxes on the top must be selected. Then, one of the first and the second radio buttons on the left as well as one of the third and the fourth radio buttons on the left must also be selected.");
        jTextPane2.setFocusCycleRoot(false);
        jTextPane2.setFocusable(false);
        jTextPane2.setMinimumSize(new java.awt.Dimension(160, 140));
        jTextPane2.setPreferredSize(new java.awt.Dimension(160, 140));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridheight = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 6, 0, 24);
        gridBagConstraints.weightx = 1.0;
        add(jTextPane2, gridBagConstraints);

        jCheckBox1.setSelected(true);
        jCheckBox1.setText("Parameter Estimation");
        jCheckBox1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBox1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(9, 36, 4, 24);
        add(jCheckBox1, gridBagConstraints);

        jCheckBox2.setText("Data Simulation");
        jCheckBox2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBox2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(9, 9, 3, 23);
        add(jCheckBox2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 21, 0, 24);
        add(jSeparator1, gridBagConstraints);

        jCheckBox3.setText("Table output");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 47, 0, 62);
        add(jCheckBox3, gridBagConstraints);

        jCheckBox4.setText("plot output");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 47, 8, 21);
        add(jCheckBox4, gridBagConstraints);

        jCheckBox5.setText("Table output");
        jCheckBox5.setEnabled(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 20, 0, 61);
        add(jCheckBox5, gridBagConstraints);

        jCheckBox6.setText("Plot output");
        jCheckBox6.setEnabled(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 20, 8, 61);
        add(jCheckBox6, gridBagConstraints);

        jSeparator2.setOrientation(javax.swing.SwingConstants.VERTICAL);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridheight = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        add(jSeparator2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 13;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 20, 0, 24);
        add(jSeparator3, gridBagConstraints);

        jTextPane3.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane3.setEditable(false);
        jTextPane3.setText("You may load back a model that was \ncreated by this tool or a SPK input file.");
        jTextPane3.setFocusCycleRoot(false);
        jTextPane3.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(8, 20, 12, 12);
        add(jTextPane3, gridBagConstraints);

        jButton1.setText("Load Model or Input");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.insets = new java.awt.Insets(7, 8, 11, 24);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jButton1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 21, 0, 0);
        add(jSeparator4, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 21, 0, 0);
        add(jSeparator5, gridBagConstraints);

        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("(This library is now in construction.)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.insets = new java.awt.Insets(0, 42, 0, 0);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel1, gridBagConstraints);

    }//GEN-END:initComponents

    private void jPopupMenu1PopupMenuCanceled(javax.swing.event.PopupMenuEvent evt) {//GEN-FIRST:event_jPopupMenu1PopupMenuCanceled
        jRadioButton3.setSelected(true);
    }//GEN-LAST:event_jPopupMenu1PopupMenuCanceled

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        if(iterator.reloadInput() == 0)
            setOptions();
    }//GEN-LAST:event_jButton1ActionPerformed

    /** Set the options on the first step. */
    public void setOptions()
    {
        Properties reload = iterator.getReload();
        if(reload.getProperty("ESTIMATION") != null)
            jCheckBox1.setSelected(true);
        else
            jCheckBox1.setSelected(false);
        if(reload.getProperty("SIMULATION") != null)
            jCheckBox2.setSelected(true);
        else
            jCheckBox2.setSelected(false);
        refresh();
        if(reload.getProperty("TABLEEST") != null)
            jCheckBox3.setSelected(true);
        else
            jCheckBox3.setSelected(false);
        if(reload.getProperty("SCATTERPLOTEST") != null)
            jCheckBox4.setSelected(true);
        else
            jCheckBox4.setSelected(false);
        if(reload.getProperty("TABLESIM") != null)
            jCheckBox5.setSelected(true);
        else
            jCheckBox5.setSelected(false);
        if(reload.getProperty("SCATTERPLOTSIM") != null)
            jCheckBox6.setSelected(true);
        else
            jCheckBox6.setSelected(false);
        if(reload.getProperty("SIGMA") == null)
            jRadioButton1.setSelected(true);
        else
            jRadioButton2.setSelected(true);
        if(reload.getProperty("PRED") != null)
            jRadioButton3.setSelected(true);
        else
        {
            jRadioButton4.setSelected(true);
            String subroutines = reload.getProperty("SUBROUTINES").trim().concat(" ");
            int beginIndex = subroutines.indexOf("ADVAN");
            int endIndex = subroutines.indexOf(" ", beginIndex);
            subroutine = "SUBROUTINE " + subroutines.substring(beginIndex, endIndex);
            jRadioButton4.setText("Using RFPK " + subroutine);            
        }
        if(reload.getProperty("COVARIANCE") != null)
            jRadioButton5.setSelected(true);
        setLeftOptions();        
    }
    
    private void jCheckBox2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBox2ActionPerformed
        refresh();
    }//GEN-LAST:event_jCheckBox2ActionPerformed

    private void refresh()
    {
        boolean isCheckBox1 = jCheckBox1.isSelected();
        boolean isCheckBox2 = jCheckBox2.isSelected();
        boolean isSelected = isCheckBox1 || isCheckBox2;
        jRadioButton1.setEnabled(isSelected);
        jRadioButton2.setEnabled(isSelected);
        jRadioButton3.setEnabled(isSelected);
        if(iterator.getIsDeveloper()) jRadioButton4.setEnabled(isSelected);
        jRadioButton5.setEnabled(isCheckBox1);
        if(!isCheckBox1) jRadioButton5.setSelected(false);         
        jCheckBox3.setEnabled(isCheckBox1);
        jCheckBox4.setEnabled(isCheckBox1);
        jCheckBox5.setEnabled(isCheckBox2);
        jCheckBox6.setEnabled(isCheckBox2); 
        setLeftOptions();        
    }
    
    private void jCheckBox1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBox1ActionPerformed
        refresh();
    }//GEN-LAST:event_jCheckBox1ActionPerformed

    private void jMenuItem12ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem12ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem12ActionPerformed

    private void jMenuItem11ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem11ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem11ActionPerformed

    private void jMenuItem10ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem10ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem10ActionPerformed

    private void jMenuItem9ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem9ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem9ActionPerformed

    private void jMenuItem7ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem7ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem7ActionPerformed

    private void jMenuItem6ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem6ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem6ActionPerformed

    private void jMenuItem5ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem5ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem5ActionPerformed

    private void jMenuItem4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem4ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem4ActionPerformed

    private void jMenuItem3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem3ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem3ActionPerformed

    private void jRadioButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton3ActionPerformed
        jRadioButton4.setText("Using RFPK model library");
        setLeftOptions();
    }//GEN-LAST:event_jRadioButton3ActionPerformed

    private void jRadioButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton2ActionPerformed
        setLeftOptions();
    }//GEN-LAST:event_jRadioButton2ActionPerformed

    private void jMenuItem2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem2ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem2ActionPerformed

    private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem1ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem1ActionPerformed

    private void jRadioButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton1ActionPerformed
        setLeftOptions();
    }//GEN-LAST:event_jRadioButton1ActionPerformed

    private void jMenuItem8ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem8ActionPerformed
        addSubroutine(evt);
    }//GEN-LAST:event_jMenuItem8ActionPerformed

    private void jRadioButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton4ActionPerformed
        jPopupMenu1.show(this, 300, 200); 
        setLeftOptions();
    }//GEN-LAST:event_jRadioButton4ActionPerformed
    
    private void addSubroutine(ActionEvent evt)
    {
        subroutine = ((javax.swing.JMenuItem)evt.getSource()).getText();
        jRadioButton4.setText("Using RFPK " + subroutine);
        jRadioButton4.setSelected(true);
    }
    
    private void setLeftOptions()
    {       
        if((jCheckBox1.isSelected() || jCheckBox2.isSelected()) && 
           (jRadioButton1.isSelected() || jRadioButton2.isSelected()) &&
           (jRadioButton3.isSelected() || jRadioButton4.isSelected())) 
            isValid = true;
        else
            isValid = false;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());                  
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.JButton jButton1;
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JCheckBox jCheckBox2;
    private javax.swing.JCheckBox jCheckBox3;
    private javax.swing.JCheckBox jCheckBox4;
    private javax.swing.JCheckBox jCheckBox5;
    private javax.swing.JCheckBox jCheckBox6;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItem10;
    private javax.swing.JMenuItem jMenuItem11;
    private javax.swing.JMenuItem jMenuItem12;
    private javax.swing.JMenuItem jMenuItem2;
    private javax.swing.JMenuItem jMenuItem3;
    private javax.swing.JMenuItem jMenuItem4;
    private javax.swing.JMenuItem jMenuItem5;
    private javax.swing.JMenuItem jMenuItem6;
    private javax.swing.JMenuItem jMenuItem7;
    private javax.swing.JMenuItem jMenuItem8;
    private javax.swing.JMenuItem jMenuItem9;
    private javax.swing.JPopupMenu jPopupMenu1;
    private javax.swing.JRadioButton jRadioButton1;
    private javax.swing.JRadioButton jRadioButton2;
    private javax.swing.JRadioButton jRadioButton3;
    private javax.swing.JRadioButton jRadioButton4;
    private javax.swing.JRadioButton jRadioButton5;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JSeparator jSeparator3;
    private javax.swing.JSeparator jSeparator4;
    private javax.swing.JSeparator jSeparator5;
    private javax.swing.JTextPane jTextPane1;
    private javax.swing.JTextPane jTextPane2;
    private javax.swing.JTextPane jTextPane3;
    // End of variables declaration//GEN-END:variables

    /**
     * This method is to return the StepDescriptor object.
     * @return a StepDescriptor object.
     */    
    public StepDescriptor getStepDescription(){
	return sd;
    }

    private class MyStepDescriptor extends StepDescriptor{ 

	public Component getComponent(){
	    return panel;
	}
       
  	public String getContentItem(){
  	    return "Analysis Selection";
  	}

	public String getStepTitle(){
	    return "Analysis Selection";
	}

	public void showingStep(JWizardPane wizard){
            wizardPane = wizard;
            MDAObject object = (MDAObject)wizard.getCustomizedObject();
            Properties records = object.getRecords();
            Source control = object.getSource(); 
            String[] names = {"Problem", "Data", "Input", "Pred", "Subroutines", "Aes", 
                              "Aesinitial", "Model", "PK", "Theta", "Omega", "Des", 
                              "Error", "Sigma", "Estimation", "Covariance", "TableEst", 
                              "ScatterPlotEst", "Simulation", "TableSim", "ScatterPlotSim"}; 
            for(int i = 0; i < 21; i++)
                records.setProperty(names[i], "");   
            control.aes = null;
            control.aesinitial = null;
            control.analysis = null;
            control.covariance = null;
//            control.data = null;
            control.des = null;
            control.error = null;
            control.pk = null;
            control.pred = null;
            control.problem = null;
            control.simulation = null;
            control.estimation = null;
            control.input = null;
            control.subroutines = null;
            control.model = null;
            control.omega = null;
            control.sigma = null;
            control.theta = null;
            control.splotEst = null;
            control.splotSim = null;
            control.tableEst = null;
            control.tableSim = null;
            jRadioButton4.setEnabled(iterator.getIsDeveloper());
	}

	public void hidingStep(JWizardPane wizard){
            iterator.setIsEstimation(jCheckBox1.isSelected()); 
            iterator.setIsSimulation(jCheckBox2.isSelected());
            iterator.setIsEstTable(jCheckBox3.isSelected());
            iterator.setIsEstPlot(jCheckBox4.isSelected());
            iterator.setIsSimTable(jCheckBox5.isSelected());
            iterator.setIsSimPlot(jCheckBox6.isSelected());
            iterator.setIsCov(jRadioButton5.isSelected());
            iterator.setIsPred(jRadioButton3.isSelected());
            iterator.setIsInd(jRadioButton1.isSelected());
            if(jRadioButton4.isSelected())
                iterator.setAdvan(Integer.parseInt(subroutine.substring(16)));
            if(jRadioButton2.isSelected())
                ((MDAObject)wizard.getCustomizedObject()).getSource().analysis = "population";
            if(jRadioButton1.isSelected())
                ((MDAObject)wizard.getCustomizedObject()).getSource().analysis = "individual"; 
	}

	public boolean isValid(){
	    return isValid;
	}

	public ActionListener getHelpAction(){
	    return new ActionListener(){
                public void actionPerformed(ActionEvent e){
                    if(!iterator.getIsOnline()) 
                        new Help("Help for Getting Started", 
                                 GettingStarted.class.getResource("/uw/rfpk/mda/nonmem/help/GettingStarted.html"));                        
                    else
                        Utility.openURL("https://" + iterator.getServerName() + 
                                        ":" + iterator.getServerPort() + "/user/help/GettingStarted.html");  
                }
            };
	}
        
        public String getHelpID() {
            return "Prepare_Input__Getting_Started";
        }
        
    }
}
