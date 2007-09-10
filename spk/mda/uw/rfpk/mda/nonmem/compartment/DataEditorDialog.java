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
import javax.swing.JTable;
import javax.swing.table.*;
import javax.swing.JOptionPane;
import javax.swing.SwingConstants;
import java.awt.Cursor;
import java.awt.Color;
import java.awt.Dimension;
import java.util.Vector;

/** This class defines data editor dialog.
 *
 * @author  Jiaji Du
 */

public class DataEditorDialog extends javax.swing.JDialog {
    
    /**
     * Creates new form DataEditorDialog.
     * 
     * @param parent DesignTool object.
     * @param input an Element.Input object.
     */
    public DataEditorDialog(DesignTool parent, Element.Input input) {
        
        super(parent, false);
        tool = parent;
        this.input = input;
        int idGroup = tool.selectedModel.id;
        String compartment = String.valueOf(((Element.Compartment)input.compartments.get(0)).number);
        header = tool.object.getDataLabels();
        data = new Vector<String[]>();
        copy = copyData(tool.object.getData());
        Vector<String[]> indData;
        indexCMT = indexOf(header, "CMT");
        indexID = indexOf(header, "ID");
        if(indexCMT == -1)
        {
            if(indexID == 0)
            {
                for(int i = 0; i < copy.size(); i++)
                {
                    indData = copy.get(i);
                    for(String[] record : indData)
                        if(tool.subjectModel.getProperty(record[0]).equals(String.valueOf(idGroup)))
                            data.add(record);
                }
            }
            else if(indexID == -1)
            {
                for(int i = 0; i < copy.size(); i++)
                {
                    indData = copy.get(i);
                    for(String[] record : indData)
                        data.add(record);
                }
            }
            else
            {
                JOptionPane.showMessageDialog(null, "ID must be in the first column.", "Input Error", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        else
        {
            if(indexID == 0)
            {
                for(int i = 0; i < copy.size(); i++)
                {
                    indData = copy.get(i);
                    for(String[] record : indData)
                        if(tool.subjectModel.getProperty(record[0]).equals(String.valueOf(idGroup)) &&
                           record[indexCMT].equals(compartment))
                            data.add(record);
                }
            }
            else if(indexID == -1)
            {
                for(int i = 0; i < copy.size(); i++)
                {
                    indData = copy.get(i);
                    for(String[] record : indData)
                        if(record[indexCMT].equals(compartment))
                            data.add(record);
                }
            }
            else
            {
                JOptionPane.showMessageDialog(null, "ID must be in the first column.", "Input Error", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        initComponents();
        jTable1.putClientProperty("terminateEditOnFocusLost", Boolean.TRUE);
        jTable1.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        jTextField1.setText(input.name);
        ATableModel model = new ATableModel();
        jTable1.setModel(model);
        TableColumnModel columnModel = jTable1.getColumnModel();
        for(int i = 0; i < header.length; i++)
            columnModel.getColumn(i).setCellRenderer(new CellRenderer());
        setSize(500, 500);
        Dimension wndSize = getToolkit().getScreenSize();
        setLocation(wndSize.width/2, wndSize.height/3);
        setVisible(true);
    }
    
    private int indexOf(String[] array, String string)
    {
        for(int i = 0; i < array.length; i++)
            if(array[i].equals(string)) return i;
        return -1;
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jPanel1 = new javax.swing.JPanel();
        OKButton = new javax.swing.JButton();
        helpButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Dose Attributes");
        setModal(true);
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                closeDialog(evt);
            }
        });

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
        jTable1.setCellSelectionEnabled(true);
        jTable1.setGridColor(new java.awt.Color(255, 255, 255));
        jScrollPane1.setViewportView(jTable1);

        getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jPanel1.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 5, 12));

        OKButton.setText("OK");
        OKButton.setPreferredSize(new java.awt.Dimension(75, 25));
        OKButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                OKButtonActionPerformed(evt);
            }
        });

        jPanel1.add(OKButton);

        helpButton.setText("Help");
        helpButton.setPreferredSize(new java.awt.Dimension(75, 25));
        helpButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                helpButtonActionPerformed(evt);
            }
        });

        jPanel1.add(helpButton);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });

        jPanel1.add(cancelButton);

        getContentPane().add(jPanel1, java.awt.BorderLayout.SOUTH);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText("Dose name  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 12, 0);
        jPanel2.add(jLabel2, gridBagConstraints);

        jTextField1.setPreferredSize(new java.awt.Dimension(100, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(11, 0, 11, 12);
        jPanel2.add(jTextField1, gridBagConstraints);

        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("Data records for the compartment and the ID group ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 12);
        jPanel2.add(jLabel1, gridBagConstraints);

        getContentPane().add(jPanel2, java.awt.BorderLayout.NORTH);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void helpButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_helpButtonActionPerformed
        JOptionPane.showMessageDialog(null, "Help is not currently available for this topic.");
    }//GEN-LAST:event_helpButtonActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        copy = null;
        setVisible(false);
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void OKButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_OKButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        tool.object.setData(copy);
        input.name = jTextField1.getText();
        setVisible(false);
        setCursor(null);
    }//GEN-LAST:event_OKButtonActionPerformed

    /** Closes the dialog */
    private void closeDialog(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_closeDialog
        setVisible(false);
        dispose();
    }//GEN-LAST:event_closeDialog
   
    private class ATableModel extends AbstractTableModel 
    {
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
            return data.size();
        }
        public Object getValueAt(int r, int c)
        {
            return data.get(r)[c];
        }
        public boolean isCellEditable(int r, int c)
        {
            return c != indexID && c != indexCMT;
        }
        public void setValueAt(Object value,int r, int c)
        {
            if(Utility.isFloatNumber((String)value))
                data.get(r)[c] = (String)value;
            else
                JOptionPane.showMessageDialog(null, "The data entry must be a number.", "Input Error", JOptionPane.ERROR_MESSAGE);
        }
    }
    
    private static Vector<Vector<String[]>> copyData(Vector<Vector<String[]>> data)
    {
        Vector<Vector<String[]>> copy = new Vector<Vector<String[]>>();
        for(Vector<String[]> ind : data)
        {
            Vector<String[]> cop = new Vector<String[]>();
            for(String[] row : ind)
            {
                String[] clo = row.clone();
                cop.add(clo);
            }
            copy.add(cop);
        }
        return copy;
    }
    
    private class CellRenderer extends DefaultTableCellRenderer 
    {
        public java.awt.Component getTableCellRendererComponent(JTable table,
            Object value,boolean isSelected, boolean hasFocus, int row,int col) 
        {
            super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,col);
            setHorizontalAlignment(SwingConstants.CENTER);
            if(col == indexID || col == indexCMT)
                setBackground(Color.LIGHT_GRAY);
            return this;
	}
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton OKButton;
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton helpButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextField jTextField1;
    // End of variables declaration//GEN-END:variables

    private DesignTool tool;
    private Element.Input input;
    private Vector<String[]> data;
    private Vector<Vector<String[]>> copy;
    private String[] header;
    private int indexCMT;
    private int indexID;
}

