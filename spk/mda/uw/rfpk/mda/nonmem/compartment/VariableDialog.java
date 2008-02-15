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

import uw.rfpk.mda.nonmem.MDAFrame;
import uw.rfpk.mda.nonmem.Utility;
import javax.swing.DefaultListModel;
import java.awt.Cursor;
import java.util.*;
import javax.swing.JList;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Color;
import java.awt.Dimension;
import javax.help.*;

/** This class defines variable dialog.
 *
 * @author  Jiaji Du
 */

public class VariableDialog extends javax.swing.JDialog {
    
    /**
     * Creates new form VariableDialog.
     * 
     * @param parent DesignTool object.
     */
    public VariableDialog(DesignTool parent) {
        super(parent, false);
        tool = parent;
        initComponents();
        helpButton.addActionListener(new CSH.DisplayHelpFromSource(MDAFrame.getHelpBroker()));
        CSH.setHelpIDString(helpButton, "Parameters");
        jList1.setModel(listModel);
        copyParameterList();
        setVariableList();
        jList1.setCellRenderer(new MyCellRenderer());
        setSize(400, 350);
        setLocationRelativeTo(tool.parameterButton);
        setVisible(true);
    }
    
    private void copyParameterList()
    {
        for(int i = 0; i < Model.parameterList.size(); i++)
        {
            Parameter modelParameter = Model.parameterList.get(i);
            if(modelParameter instanceof Variable)
            {
                Variable variable = new Variable(modelParameter.name, modelParameter.value);
                variable.refCount = ((Variable)modelParameter).refCount;
                parameterList.add(variable);
            }
            else
            {
                Parameter parameter = new Parameter(modelParameter.name, modelParameter.value);
                parameterList.add(parameter);
            }
        }
        for(int i = 0; i < parameterList.size(); i++)
        {
            Parameter parameter = parameterList.get(i);
            Parameter modelParameter = Model.parameterList.get(i);
            ArrayList<Variable> modelDependVariables = modelParameter.dependVariables;
            for(int j = 0; j < modelDependVariables.size(); j++)
            {
                int index = Model.parameterList.indexOf(modelDependVariables.get(j));
                Variable variable = (Variable)parameterList.get(index);
                parameter.dependVariables.add(variable);
            }
        }
    }
    
    private void setVariableList()
    {
        listModel.removeAllElements();
        for(Parameter parameter : parameterList)
            listModel.addElement(parameter.value);
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jLabel1 = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jList1 = new javax.swing.JList();
        jPanel2 = new javax.swing.JPanel();
        upButton = new javax.swing.JButton();
        downButton = new javax.swing.JButton();
        modelButton = new javax.swing.JButton();
        addButton = new javax.swing.JButton();
        deleteButton = new javax.swing.JButton();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        helpButton = new javax.swing.JButton();

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Parameter List");
        setModal(true);
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                closeDialog(evt);
            }
        });

        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("Select an item to modify or to change order");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 2, 6);
        getContentPane().add(jLabel1, gridBagConstraints);

        jScrollPane1.setMaximumSize(new java.awt.Dimension(263, 263));
        jScrollPane1.setMinimumSize(new java.awt.Dimension(263, 263));
        jScrollPane1.setPreferredSize(new java.awt.Dimension(263, 263));
        jList1.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        jScrollPane1.setViewportView(jList1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 12, 6);
        getContentPane().add(jScrollPane1, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridLayout(0, 1, 0, 4));

        upButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("/uw/rfpk/mda/nonmem/wizard/icons/up.png")));
        upButton.setPreferredSize(new java.awt.Dimension(50, 25));
        upButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                upButtonActionPerformed(evt);
            }
        });

        jPanel2.add(upButton);

        downButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("/uw/rfpk/mda/nonmem/wizard/icons/down.png")));
        downButton.setPreferredSize(new java.awt.Dimension(50, 25));
        downButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                downButtonActionPerformed(evt);
            }
        });

        jPanel2.add(downButton);

        modelButton.setText("Model");
        modelButton.setPreferredSize(new java.awt.Dimension(75, 25));
        modelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                modelButtonActionPerformed(evt);
            }
        });

        jPanel2.add(modelButton);

        addButton.setText("Add");
        addButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addButtonActionPerformed(evt);
            }
        });

        jPanel2.add(addButton);

        deleteButton.setText("Delete");
        deleteButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteButtonActionPerformed(evt);
            }
        });

        jPanel2.add(deleteButton);

        okButton.setText("OK");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });

        jPanel2.add(okButton);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });

        jPanel2.add(cancelButton);

        helpButton.setText("Help");
        helpButton.setPreferredSize(new java.awt.Dimension(75, 25));
        jPanel2.add(helpButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 12, 12);
        getContentPane().add(jPanel2, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void deleteButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        int index = jList1.getSelectedIndex();
        if(index >= 0)
        {
            Parameter parameter = parameterList.get(index);
            if(tool.checkVariableName(parameter.name))
            {
                parameterList.remove(index);
                tool.updateParameterList(parameterList, parameter, true);
                setVariableList();
            }
            else
                JOptionPane.showMessageDialog(null, "The parameter is not deletable.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
        }
        setCursor(null);
    }//GEN-LAST:event_deleteButtonActionPerformed

    private void addButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        String name = JOptionPane.showInputDialog("Enter parameter name.");
        if(name != null && name.length() != 0)
        {
            name = name.toUpperCase();
            if(tool.checkVariableName(name))
            {
                Parameter parameter = new Parameter(name, name + "=");
                parameterList.add(parameter);
                int[] isOK = {1};
                if(tool.iterator.analysis.equals("population"))
                    new MixedModelDialog(null, parameter, tool.object.getDataLabels(), isOK);
                else
                    new IndModelDialog(null, parameter, tool.object.getDataLabels(), isOK);
                if(isOK[0] == 0)
                {
                    setCursor(null);
                    return;
                }
                tool.updateParameterList(parameterList, parameter, false);
                setVariableList();
            }
            else
                JOptionPane.showMessageDialog(null, "The parameter name is invalid.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
        }
        setCursor(null);
    }//GEN-LAST:event_addButtonActionPerformed

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Model.parameterList = parameterList;
        tool.setRecords();
        setVisible(false);
        setCursor(null);
    }//GEN-LAST:event_okButtonActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        setVisible(false);
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void modelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_modelButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        int index = jList1.getSelectedIndex();
        if(index < 0)
        {
            setCursor(null);
            return;
        }
        Parameter parameter = parameterList.get(index);
        if(parameter.name.matches("FF\\d+"))
        {
            parameter.value = JOptionPane.showInputDialog(null, "Enter forcing function:                        ", 
                                                          parameter.value);
        }
        else
        {
            int[] isOK = {1};
            if(tool.iterator.analysis.equals("population"))
                new MixedModelDialog(null, parameter, tool.object.getDataLabels(), isOK);
            else
                new IndModelDialog(null, parameter, tool.object.getDataLabels(), isOK);
            if(isOK[0] == 0)
            {
                setCursor(null);
                return;
            }
        }
        if(parameter.value.equals("") || parameter.value.endsWith("="))
        {
            JOptionPane.showMessageDialog(null, "The parameter's model has not been defined.",
                                          "Warning Message", JOptionPane.WARNING_MESSAGE);
            setCursor(null);
            return;
        }
        tool.updateParameterList(parameterList, parameter, false);
        setVariableList();
        setCursor(null);
    }//GEN-LAST:event_modelButtonActionPerformed

    private void downButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        int index = jList1.getSelectedIndex();
        if(index >= 0 && index < listModel.size() - 1)
        {
            Parameter parameter = parameterList.get(index);
            parameterList.set(index, parameterList.get(index + 1));
            parameterList.set(index + 1, parameter);
            setVariableList();
            jList1.setSelectedIndex(index + 1);
        }
        setCursor(null);
    }//GEN-LAST:event_downButtonActionPerformed

    private void upButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_upButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        int index = jList1.getSelectedIndex();
        if(index > 0)
        {
            Parameter parameter = parameterList.get(index);
            parameterList.set(index, parameterList.get(index - 1));
            parameterList.set(index - 1, parameter);
            setVariableList();
            jList1.setSelectedIndex(index - 1);
        }
        setCursor(null);
    }//GEN-LAST:event_upButtonActionPerformed

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
    private javax.swing.JButton addButton;
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton deleteButton;
    private javax.swing.JButton downButton;
    private javax.swing.JButton helpButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JList jList1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JButton modelButton;
    private javax.swing.JButton okButton;
    private javax.swing.JButton upButton;
    // End of variables declaration//GEN-END:variables

    private DefaultListModel listModel = new DefaultListModel();
    private DesignTool tool;
    private ArrayList<Parameter> parameterList = new ArrayList<Parameter>();
}

class MyCellRenderer extends JPanel implements ListCellRenderer
{
    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus)
    {
        string = (String)value;
        background = isSelected ? list.getSelectionBackground() : list.getBackground();
        foreground = isSelected ? list.getSelectionForeground() : list.getForeground();
        return this;
    }
    
    public void paintComponent(Graphics g)
    {
        g.setColor(background);
        g.fillRect(0, 0, getWidth(), getHeight());
        g.setColor(foreground);
        String[] lines = string.split("\n");
        for(int i = 0; i < lines.length; i++)
            g.drawString(lines[i], 0, 10 * (i + 1) + 4);
    }
    
    public Dimension getPreferredSize()
    {
        String[] lines = string.split("\n");
//        int length = lines[0].length();
//        if(lines.length > 1)
//            for(int i = 1; i < lines.length; i++)
//                length = Math.max(lines[i].length(), length);
        return new Dimension(0, lines.length * 10 + 8);
    }
    
    private String string;
    private Color background;
    private Color foreground;
}
