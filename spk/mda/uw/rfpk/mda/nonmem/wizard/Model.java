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
import javax.swing.JComponent;
import javax.swing.DefaultListModel;
import javax.swing.JOptionPane; 
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * This class defines a step to create the $MODEL record.
 * @author  Jiaji Du
 */
public class Model extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this; 
    private MDAIterator iterator = null;
    private DefaultListModel model = null; 
    private String attributes = "";
    private JWizardPane wizardPane = null;
    private boolean isValid = false;
    private int index = -1;

    /** Creates new form Model.
     * @param iter a MDAIterator object to initialize the field iterator.
     */
    public Model(MDAIterator iter) { 
        initComponents();
        iterator = iter; 
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        jDialog1 = new javax.swing.JDialog();
        jCheckBox1 = new javax.swing.JCheckBox();
        jCheckBox2 = new javax.swing.JCheckBox();
        jCheckBox3 = new javax.swing.JCheckBox();
        jCheckBox4 = new javax.swing.JCheckBox();
        jCheckBox5 = new javax.swing.JCheckBox();
        jButton2 = new javax.swing.JButton();
        jTextPane3 = new javax.swing.JTextPane();
        jCheckBox6 = new javax.swing.JCheckBox();
        jCheckBox7 = new javax.swing.JCheckBox();
        addButton = new javax.swing.JButton();
        upButton = new javax.swing.JButton();
        downButton = new javax.swing.JButton();
        jTextPane1 = new javax.swing.JTextPane();
        jTextPane2 = new javax.swing.JTextPane();
        changeButton = new javax.swing.JButton();
        deleteButton = new javax.swing.JButton();
        jSeparator1 = new javax.swing.JSeparator();
        jSeparator2 = new javax.swing.JSeparator();
        jScrollPane1 = new javax.swing.JScrollPane();
        jList1 = new javax.swing.JList();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jButton1 = new javax.swing.JButton();
        jTextField1 = new javax.swing.JTextField();
        jTextField2 = new javax.swing.JTextField();
        jTextField3 = new javax.swing.JTextField();
        jTextField4 = new javax.swing.JTextField();
        jLabel4 = new javax.swing.JLabel();
        jSeparator3 = new javax.swing.JSeparator();

        jDialog1.getContentPane().setLayout(new java.awt.GridBagLayout());

        jDialog1.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        jDialog1.setTitle("Attributes");
        jDialog1.setBackground(java.awt.Color.white);
        jDialog1.setLocationRelativeTo(jButton1);
        jDialog1.setModal(true);
        jDialog1.setResizable(false);
        jCheckBox1.setText("INITIALOFF");
        jCheckBox1.setToolTipText("Compartment is initially off");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 24, 1, 12);
        jDialog1.getContentPane().add(jCheckBox1, gridBagConstraints);

        jCheckBox2.setText("NOOFF");
        jCheckBox2.setToolTipText("Compartment may not be turned on or off.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 24, 1, 12);
        jDialog1.getContentPane().add(jCheckBox2, gridBagConstraints);

        jCheckBox3.setText("NODOSE");
        jCheckBox3.setToolTipText("Compartment may not receive a dose.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 24, 1, 12);
        jDialog1.getContentPane().add(jCheckBox3, gridBagConstraints);

        jCheckBox4.setText("EQUILIBRIUM");
        jCheckBox4.setToolTipText("Compartment is an equilibrium compartment (implies NODOSE).");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 24, 1, 12);
        jDialog1.getContentPane().add(jCheckBox4, gridBagConstraints);

        jCheckBox5.setText("EXCLUDE");
        jCheckBox5.setToolTipText("Compartment amount is excluded from the amount of the output compartment.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 24, 1, 12);
        jDialog1.getContentPane().add(jCheckBox5, gridBagConstraints);

        jButton2.setText("OK");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.insets = new java.awt.Insets(4, 54, 4, 54);
        jDialog1.getContentPane().add(jButton2, gridBagConstraints);

        jTextPane3.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane3.setText("Multiple attributes may be selected.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(10, 12, 6, 12);
        jDialog1.getContentPane().add(jTextPane3, gridBagConstraints);

        jCheckBox6.setText("DEFOBSERVATION");
        jCheckBox6.setToolTipText("Compartment is the default observation compartment.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 24, 1, 12);
        jDialog1.getContentPane().add(jCheckBox6, gridBagConstraints);

        jCheckBox7.setText("DEFDOSE");
        jCheckBox7.setToolTipText("Compartment is the default dose compartment.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(1, 24, 1, 12);
        jDialog1.getContentPane().add(jCheckBox7, gridBagConstraints);

        setLayout(new java.awt.GridBagLayout());

        addButton.setText("Add");
        addButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 6;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 16, 7, 16);
        add(addButton, gridBagConstraints);

        upButton.setText("Up");
        upButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                upButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 6;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(16, 16, 7, 16);
        add(upButton, gridBagConstraints);

        downButton.setText("Down");
        downButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 6;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(5, 16, 56, 16);
        add(downButton, gridBagConstraints);

        jTextPane1.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane1.setEditable(false);
        jTextPane1.setText("Enter total number of compartments other than the output compartment, \nnumber of equilibrium compartments, number of basic PK parameters.  \nThen enter the definition of each compartments: its name and attributes.\n(Note:  These items are all optional in order to create a compartment.)");
        jTextPane1.setFocusable(false);
        jTextPane1.setMargin(new java.awt.Insets(0, 0, 0, 0));
        jTextPane1.setMinimumSize(new java.awt.Dimension(500, 60));
        jTextPane1.setPreferredSize(new java.awt.Dimension(400, 60));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 7;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 8);
        add(jTextPane1, gridBagConstraints);

        jTextPane2.setBackground(new java.awt.Color(204, 204, 204));
        jTextPane2.setEditable(false);
        jTextPane2.setText("List of the\ncompart-\nments\nyou have\nentered in\nNONMEM\nsyntax");
        jTextPane2.setFocusable(false);
        jTextPane2.setMaximumSize(new java.awt.Dimension(70, 81));
        jTextPane2.setMinimumSize(new java.awt.Dimension(70, 81));
        jTextPane2.setPreferredSize(new java.awt.Dimension(70, 81));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(11, 12, 12, 5);
        add(jTextPane2, gridBagConstraints);

        changeButton.setText("Change");
        changeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                changeButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 6;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 16, 6, 16);
        add(changeButton, gridBagConstraints);

        deleteButton.setText("Delete");
        deleteButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteButtonActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 6;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(7, 16, 14, 16);
        add(deleteButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 7;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 16);
        add(jSeparator1, gridBagConstraints);

        jSeparator2.setOrientation(javax.swing.SwingConstants.VERTICAL);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        add(jSeparator2, gridBagConstraints);

        jScrollPane1.setMaximumSize(new java.awt.Dimension(180, 100));
        jScrollPane1.setPreferredSize(new java.awt.Dimension(180, 100));
        model = new DefaultListModel();
        jList1 = new javax.swing.JList(model);
        jList1.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        jList1.setFixedCellHeight(15);
        jList1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jList1MouseClicked(evt);
            }
        });

        jScrollPane1.setViewportView(jList1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(16, 0, 12, 12);
        add(jScrollPane1, gridBagConstraints);

        jLabel1.setText("NCOMPARTMENTS");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(13, 12, 12, 0);
        add(jLabel1, gridBagConstraints);

        jLabel2.setText("NEQUILIBRIUM");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(9, 12, 9, 0);
        add(jLabel2, gridBagConstraints);

        jLabel3.setText("NPARAMETERS");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(9, 12, 18, 2);
        add(jLabel3, gridBagConstraints);

        jButton1.setText("Attributes");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(7, 12, 14, 12);
        add(jButton1, gridBagConstraints);

        jTextField1.setPreferredSize(new java.awt.Dimension(4, 25));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 50;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(9, 12, 9, 12);
        add(jTextField1, gridBagConstraints);

        jTextField2.setPreferredSize(new java.awt.Dimension(4, 25));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        add(jTextField2, gridBagConstraints);

        jTextField3.setPreferredSize(new java.awt.Dimension(8, 25));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(10, 12, 17, 12);
        add(jTextField3, gridBagConstraints);

        jTextField4.setMaximumSize(new java.awt.Dimension(80, 25));
        jTextField4.setMinimumSize(new java.awt.Dimension(80, 25));
        jTextField4.setPreferredSize(new java.awt.Dimension(80, 25));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 30;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        add(jTextField4, gridBagConstraints);

        jLabel4.setText("Name:");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(11, 12, 5, 12);
        add(jLabel4, gridBagConstraints);

        jSeparator3.setOrientation(javax.swing.SwingConstants.VERTICAL);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        add(jSeparator3, gridBagConstraints);

    }//GEN-END:initComponents

    private void jList1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jList1MouseClicked
        index = jList1.getSelectedIndex();
        
        // Reload selected value
        String selectedValue = (String)jList1.getSelectedValue();        
        int beginIndex = selectedValue.indexOf("(") + 1;
        int endIndex = selectedValue.indexOf(")");
        selectedValue = selectedValue.substring(beginIndex, endIndex);
        jTextField4.setText(selectedValue.split(" ")[0]);        
        int spaceIndex = selectedValue.indexOf(" ");
        if(spaceIndex != -1)
        {            
            String attributes = selectedValue.substring(spaceIndex);
            jCheckBox1.setSelected(selectedValue.indexOf(" INITIALOFF") != -1);
            jCheckBox2.setSelected(selectedValue.indexOf(" NOOFF") != -1);
            jCheckBox3.setSelected(selectedValue.indexOf(" NODOSE") != -1);
            if(iterator.getAdvan() == 9)
            {   
                jCheckBox4.setEnabled(true);
                jCheckBox5.setEnabled(true);
                jCheckBox4.setSelected(selectedValue.indexOf(" EQUILIBRIUM") != -1);
                jCheckBox5.setSelected(selectedValue.indexOf(" EXCLUDE") != -1);
            }
            else
            {
                jCheckBox4.setEnabled(false);
                jCheckBox5.setEnabled(false);                
            }
            jCheckBox6.setSelected(selectedValue.indexOf(" DEFOBSERVATION") != -1);
            jCheckBox7.setSelected(selectedValue.indexOf(" DEFDOSE") != -1);            
        }
        
        changeButton.setEnabled(true);
        deleteButton.setEnabled(true);        
        Utility.setUpDownButton(index, model, upButton, downButton);
    }//GEN-LAST:event_jList1MouseClicked

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        attributes = "";
        if(jCheckBox1.isSelected())
            attributes = attributes + " INITIALOFF";
        if(jCheckBox2.isSelected())
            attributes = attributes + " NOOFF";
        if(jCheckBox3.isSelected())
            attributes = attributes + " NODOSE";
        if(iterator.getAdvan() == 9 && jCheckBox4.isSelected())
            attributes = attributes + " EQUILIBRIUM";
        if(iterator.getAdvan() == 9 && jCheckBox5.isSelected())
            attributes = attributes + " EXCLUDE";
        if(jCheckBox6.isSelected())
            attributes = attributes + " DEFOBSERVATION";
        if(jCheckBox7.isSelected())
            attributes = attributes + " DEFDOSE";
        jDialog1.setVisible(false);
        jDialog1.dispose();
    }//GEN-LAST:event_jButton2ActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        jCheckBox4.setEnabled(iterator.getAdvan() == 9);
        jCheckBox5.setEnabled(iterator.getAdvan() == 9);
        if(iterator.getAdvan() != 9) 
        {
            jCheckBox4.setSelected(false);
            jCheckBox5.setSelected(false);            
        }
        jDialog1.setSize(250,280);            
        jDialog1.setVisible(true);
    }//GEN-LAST:event_jButton1ActionPerformed

    private void deleteButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteButtonActionPerformed
        if(index == -1) return;
        // Update the name of the followed compartments
        for(int i = index + 1; i < model.getSize(); i++)
        {
            String comp = (String)model.get(i);
            if(comp.startsWith("COMP=(COMP " + (i + 1)))
                model.setElementAt("COMP=(COMP " + i + comp.substring(12), i);
        }
        // Remove element
        model.removeElement(jList1.getSelectedValue());
        jList1.setSelectedIndex(--index);
        
        // Set add button
        addButton.setEnabled(true);
        
        // Set left options
        if(model.getSize() == 0)
        {
            isValid = false;
            wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray()); 
        }
        
        // Set up and down buttons
        Utility.setUpDownButton(index, model, upButton, downButton);
    }//GEN-LAST:event_deleteButtonActionPerformed

    private void changeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_changeButtonActionPerformed
        // Construct element
        String name = jTextField2.getText().trim();
        if(name.equals(""))
            name = "COMP" + String.valueOf(index + 1);
        String element = "COMP=(" + name + attributes + ")";
        // Check if changeable
        for(int i = 0; i < model.getSize(); i++)
            if(i != index && 
               ((String)model.get(i)).split(" ")[0].equals(element.split(" ")[0]))
            {
                JOptionPane.showMessageDialog(null, "The name '" + name + "' is already used.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);                
                return;
            }
        model.setElementAt(element, index);     
        jList1.setSelectedIndex(index);
    }//GEN-LAST:event_changeButtonActionPerformed

    private void downButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downButtonActionPerformed
        jList1.setSelectedIndex(++index);
        if(index == 0)
        {
            changeButton.setEnabled(true);
            deleteButton.setEnabled(true);
        }
                
        // Set up and down buttons
        Utility.setUpDownButton(index, model, upButton, downButton);
    }//GEN-LAST:event_downButtonActionPerformed

    private void addButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addButtonActionPerformed
        // Construct element
        String name = jTextField4.getText().trim();
        if(name.equals(""))
            name = "COMP" + String.valueOf(index + 2);
        String element = "COMP=(" + name + attributes + ")";
        // Check if addable
        for(int i = 0; i < model.getSize(); i++)
        {
            if(element.split(" ")[0].equals(((String)model.get(i)).split(" ")[0]))
            {
                JOptionPane.showMessageDialog(null, "The name '" + name + "' is already used.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        // Update the name of the followed compartments
        for(int i = index + 1; i < model.getSize(); i++)
        {
            String comp = (String)model.get(i);
            if(comp.startsWith("COMP=(COMP " + (i + 1)))
                model.setElementAt("COMP=(COMP " + (i + 2) + comp.substring(12), i);
        }
        // Add element
        model.add(++index, element);     
        jList1.setSelectedIndex(index);
        
        // Check the limit
        if(model.getSize() == 9)
            addButton.setEnabled(false);
        
        // Set up and down buttons
        Utility.setUpDownButton(index, model, upButton, downButton);
        
        // Set left options
        String n1 = jTextField1.getText().trim();
        String n2 = jTextField2.getText().trim();
        String n3 = jTextField3.getText().trim();        
        if(!Utility.isPosIntNumber(n1))
        {
            JOptionPane.showMessageDialog(null, 
                                          "The number of compartments other than the output compartment is " +
                                          "not a positive integer",
                                          "Input Error",  
                                          JOptionPane.ERROR_MESSAGE); 
            jTextField1.setText("");
        }
        else if(!Utility.isPosIntNumber(n2) && !n2.equals("0"))
        {
            JOptionPane.showMessageDialog(null, 
                                          "The number of equilibrium equations is " +
                                          "not a positive integer or zero.",
                                          "Input Error",  
                                          JOptionPane.ERROR_MESSAGE); 
            jTextField2.setText("");
        }
        else if(!Utility.isPosIntNumber(n3) && !n3.equals("0"))
        {
            JOptionPane.showMessageDialog(null, 
                                          "The number of basic PK parameters is " +
                                          "not a positive integer or zero.",
                                          "Input Error",  
                                          JOptionPane.ERROR_MESSAGE); 
            jTextField3.setText("");
        }        
        else
        {
            isValid = true;
            wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());            
        }        
    }//GEN-LAST:event_addButtonActionPerformed

    private void upButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_upButtonActionPerformed
        jList1.setSelectedIndex(--index);
        
        // Set up and down buttons
        Utility.setUpDownButton(index, model, upButton, downButton);
    }//GEN-LAST:event_upButtonActionPerformed
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton addButton;
    private javax.swing.JButton changeButton;
    private javax.swing.JButton deleteButton;
    private javax.swing.JButton downButton;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JCheckBox jCheckBox2;
    private javax.swing.JCheckBox jCheckBox3;
    private javax.swing.JCheckBox jCheckBox4;
    private javax.swing.JCheckBox jCheckBox5;
    private javax.swing.JCheckBox jCheckBox6;
    private javax.swing.JCheckBox jCheckBox7;
    private javax.swing.JDialog jDialog1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JList jList1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JSeparator jSeparator3;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextField jTextField4;
    private javax.swing.JTextPane jTextPane1;
    private javax.swing.JTextPane jTextPane2;
    private javax.swing.JTextPane jTextPane3;
    private javax.swing.JButton upButton;
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
  	    return "Model Specification";
  	}

	public String getStepTitle(){
	    return "Model Specification";
	}

	public void showingStep(JWizardPane wizard){
            wizardPane = wizard;
            if(iterator.getIsReload())
            {
                String text = iterator.getReload().getProperty("MODEL");
                if(text != null)
                {
                    text = text.trim().concat(" ");
                    iterator.getReload().remove("MODEL");
                    int beginIndex = text.indexOf("NCOMPARTMENTS=") + 14;
                    int endIndex = 0;
                    if(beginIndex != -1)
                    {
                        endIndex = text.indexOf(" ", beginIndex); 
                        jTextField1.setText(text.substring(beginIndex, endIndex));
                    }
                    beginIndex = text.indexOf("NEQUILIBRIUM=") + 13;
                    if(beginIndex != -1)
                    {
                        endIndex = text.indexOf(" ", beginIndex); 
                        jTextField2.setText(text.substring(beginIndex, endIndex));
                    }
                    beginIndex = text.indexOf("NPARAMETERS=") + 12;
                    if(beginIndex != -1)
                    {
                        endIndex = text.indexOf(" ", beginIndex); 
                        jTextField3.setText(text.substring(beginIndex, endIndex));
                    }
                    model.removeAllElements();
                    beginIndex = text.indexOf("COMP=(");
                    while(beginIndex != -1)
                    {
                        endIndex = text.indexOf(")", beginIndex);
                        model.addElement(checkAttribute(text.substring(beginIndex, endIndex + 1)));
                        beginIndex = text.indexOf("COMP=(", endIndex);
                    }
                    index = model.size() - 1;
                    jList1.setSelectedIndex(index);
                    isValid = true;
                    wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());                    
                }
                else
                    for(int i = 0; i < model.size(); i++)
                        model.set(i, checkAttribute((String)model.get(i)));
            }
            else
                for(int i = 0; i < model.size(); i++)
                    model.set(i, checkAttribute((String)model.get(i)));
	}

        private String checkAttribute(String element)
        {
            String cName = element.substring(6, element.indexOf(" "));
            String attrs = element.substring(element.indexOf(" "), element.length() - 1);
            if(iterator.getAdvan() != 9 && attrs.indexOf(" EQUILIBRIUM") != -1)
            {
                element = element.replaceAll(" EQUILIBRIUM", "");
                JOptionPane.showMessageDialog(null, "The attribute 'EQUILIBRIUM' has been removed from compartment '" + cName + "'\n" +
                                              "because it is only for ADVAN9.", 
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
            }
            if(iterator.getAdvan() != 9 && attrs.indexOf(" EXCLUDE") != -1)
            {
                element =element.replaceAll(" EXCLUDE", "");
                JOptionPane.showMessageDialog(null, "The attribute 'EXCLUDE' has been removed from compartment '" + cName + "'\n" +
                                              "because it is only for ADVAN9.", 
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
            }
            return element;
        }
        
	public void hidingStep(JWizardPane wizard){
            if(iterator.getIsBack())
            {
                iterator.setIsBack(false);
                return;
            }            
            int size = model.getSize();
            if(size == 0)
                return;
            MDAObject object = (MDAObject)wizard.getCustomizedObject();
            String record = "";
            String n1 = jTextField1.getText().trim();
            if(!n1.equals(""))
                record += " NCOMPARTMENTS=" + n1;
            String n2 = jTextField2.getText().trim();
            if(!n2.equals(""))
                record += " NEQUILIBRIUM=" + n2;
            String n3 = jTextField3.getText().trim();
            if(!n3.equals(""))
                record += " NPARAMETERS=" + n3 + " ";
            
            for(int i = 0; i < size; i++)
                record += "\n" + ((String)model.get(i)).replaceAll("\r", "");
            object.getRecords().setProperty("Model", "$MODEL" + record);
            
            // Set source
            String[][] compartments = new String[size + 1][];
            compartments[0] = new String[3]; 
            if(!n1.equals(""))
                compartments[0][0] = n1;
            else
                compartments[0][0] = String.valueOf(size);
            if(!n2.equals(""))
                compartments[0][1] = n2;
            else
                compartments[0][1] = "0";
            if(!n3.equals(""))
                compartments[0][2] = n3;
            else
                compartments[0][2] = null;
            for(int i = 1; i <= size; i++)
            {
                String compartment = (String)model.get(i - 1);
                String name = compartment.substring(6, compartment.indexOf(" ", 6));              
                compartments[i] = new String[8];
                if(name.startsWith("\"") || name.startsWith("'"))
                {
                    name = name.substring(1, name.length() - 1);                    
                }
                compartments[i][0] = name; 
                if(compartment.indexOf(" INITIALOFF") != -1)
                    compartments[i][1] = "yes";
                else
                    compartments[i][1] = "no";
                
                if(compartment.indexOf(" NOOFF") != -1)
                    compartments[i][2] = "yes";
                else
                    compartments[i][2] = "no";
                if(compartment.indexOf(" NODOSE") != -1)
                    compartments[i][3] = "yes";
                else
                    compartments[i][3] = "no";
                if(compartment.indexOf(" EQUILIBRIUM") != -1)
                    compartments[i][4] = "yes";
                else
                    compartments[i][4] = "no";
                if(compartment.indexOf(" EXCLUDE") != -1)
                    compartments[i][5] = "yes";
                else
                    compartments[i][5] = "no";
                if(compartment.indexOf(" DEFOBSERVATION") != -1)
                    compartments[i][6] = "yes";
                else
                    compartments[i][6] = "no";
                if(compartment.indexOf(" DEFDOSE") != -1)
                    compartments[i][7] = "yes";
                else
                    compartments[i][7] = "no";
            }
            object.getSource().model = compartments;
	}

	public boolean isValid(){
            return isValid;
	}

	public ActionListener getHelpAction(){
	    return new ActionListener(){
                public void actionPerformed(ActionEvent e){ 
                    if(!iterator.getIsOnline()) 
                        new Help("Help for $MODEL Record", 
                                 Model.class.getResource("/uw/rfpk/mda/nonmem/help/Model.html"));
                    else
                        Utility.openURL("https://" + iterator.getServerName() + 
                                        ":" + iterator.getServerPort() + "/user/help/Model.html");  
                }
            };
	}
    }
}
