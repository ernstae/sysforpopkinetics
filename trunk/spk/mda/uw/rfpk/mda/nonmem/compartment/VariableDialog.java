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

import javax.swing.DefaultListModel;
import java.util.*;
import javax.swing.JList;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

/** This class defines variable dialog.
 *
 * @author  Jiaji Du
 */
public class VariableDialog extends javax.swing.JDialog {
    
    /** Creates new form VariableDialog.
     * @param parent DesignTool object.
     */
    public VariableDialog(DesignTool parent) {
        super(parent, false);
        initComponents();
        frame = parent;
        setVariableList();
        setSize(300, 300);
    }
    
    /** Set variable list */
    protected void setVariableList()
    {
        Properties variables = Model.variables;
        List keySet = new Vector(variables.keySet());
        Iterator keyIter = keySet.iterator();
        String key, value;
        listModel1.removeAllElements();
        while(keyIter.hasNext())
        {
            key = (String)keyIter.next();
            value = variables.getProperty(key);
            if(value != null)
                listModel1.addElement(key + "=" + value);
        }
        jList1.setModel(listModel1);
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        jScrollPane1 = new javax.swing.JScrollPane();
        jList1 = new javax.swing.JList();
        jLabel1 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Variable List");
        setLocationRelativeTo(this);
        setResizable(false);
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                closeDialog(evt);
            }
        });

        jScrollPane1.setPreferredSize(new java.awt.Dimension(259, 200));
        jList1.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        jList1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jList1MouseClicked(evt);
            }
        });

        jScrollPane1.setViewportView(jList1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        getContentPane().add(jScrollPane1, gridBagConstraints);

        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("Select one to modify");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel1, gridBagConstraints);

        jLabel6.setText("User defined mixed effects variables");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel6, gridBagConstraints);

        pack();
    }//GEN-END:initComponents

    private void jList1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jList1MouseClicked
        index = jList1.getSelectedIndex();
        String selectedItem = (String)jList1.getSelectedValue();
        variable = selectedItem.substring(0, selectedItem.indexOf("=")).trim();
        modelExpression = selectedItem.substring(selectedItem.indexOf("=") + 1).trim();
        String[] model = new String[1];
        model[0] = modelExpression;
        ModelDialog modelDialog = new ModelDialog(new JFrame(), model, variable);
        modelExpression = model[0];
        if(modelExpression.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The variable's mixed effect model has not been defined.",
                                          "Error Message", JOptionPane.ERROR_MESSAGE);
            return;
        }
        Model.variables.setProperty(variable, modelExpression);
        listModel1.setElementAt(variable + "=" + modelExpression, index);
        setVariableList();
        if(frame.isModelApplied)
            frame.clickFinishButton();
    }//GEN-LAST:event_jList1MouseClicked
    
    private static String getKey(Properties property, String value)
    {
        String key;
        Enumeration keys = property.keys();
        while(keys.hasMoreElements())
        {
            key = (String)keys.nextElement();
            if(property.getProperty(key).equals(value)) return key;
        }
        return null;
    }
    
    /** Closes the dialog */
    private void closeDialog(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_closeDialog
        setVisible(false);
        dispose();
    }//GEN-LAST:event_closeDialog
    
    /** Main method.
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        new VariableDialog(new DesignTool()).setVisible(true);
    }
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JList jList1;
    private javax.swing.JScrollPane jScrollPane1;
    // End of variables declaration//GEN-END:variables

    protected static DefaultListModel listModel1 = new DefaultListModel();
    private String variable, modelExpression;
    private int index = -1;
    private DesignTool frame;
}