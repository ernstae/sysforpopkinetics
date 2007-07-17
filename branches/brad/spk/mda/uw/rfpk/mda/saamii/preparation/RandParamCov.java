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
package uw.rfpk.mda.saamii.preparation;

import javax.swing.table.*;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.SwingConstants;
import javax.swing.JOptionPane;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Color;

/**
 *
 * @author  Jiaji Du
 */
public class RandParamCov extends javax.swing.JDialog {
    
    /** Creates new form RandParamCov */
    public RandParamCov(java.awt.Frame parent, boolean modal) {
        super(parent, modal);
        initComponents();
        int dimension = 3;
        data = new Object[dimension][dimension + 1];
        for(int i = 0; i < dimension; i++)
        {
            data[i][0] = "R" + String.valueOf(i + 1);
            for(int j = 1; j <= dimension; j++) 
                if(j - i != 1) data[i][j] = "           0";
                else data[i][j] = "";
        }
        
        // Create a column model for the main table.  
        TableColumnModel cm = new DefaultTableColumnModel() {
            boolean first = true;
            public void addColumn(TableColumn tc) {
                // Drop the first column that will be the row header.
                if(first) {first = false; return; }
                tc.setMinWidth(100);
                super.addColumn(tc);
            }
        };
        // Create a column model that will serve the row header table.
        TableColumnModel rowHeaderModel = new DefaultTableColumnModel() {
            boolean first = true; 
            public void addColumn(TableColumn tc) {
                if(first) {
                    tc.setMaxWidth(100);
                    super.addColumn(tc);
                    first = false;
                }
                // Drop the rest of the columns.
            }
        };
                
        jTable1.setModel(tableModel);
        jTable1.setColumnModel(cm);

        // Set up the header column and get it hooked up to everything.
        JTable headerColumn = new JTable(tableModel, rowHeaderModel); 
        jTable1.createDefaultColumnsFromModel();
        headerColumn.createDefaultColumnsFromModel();
        jTable1.setSelectionModel(headerColumn.getSelectionModel());
        jTable1.setRowSelectionAllowed(false);
        rowHeaderModel.getColumn(0).setCellRenderer(new HeaderCellRenderer()); 
        
        // Make the header column look pretty.
        headerColumn.setMaximumSize(new Dimension(100,  dimension));
        headerColumn.setBackground(new Color(204, 204, 204));
        headerColumn.setColumnSelectionAllowed(false);
        headerColumn.setCellSelectionEnabled(false);

        // Put it in a viewport.
        JViewport jv = new JViewport();
        jv.setView(headerColumn);
        jv.setPreferredSize(headerColumn.getMaximumSize());

        // Shut off autoResizeMode.
        jTable1.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

        // Attach the row headers.
        jScrollPane1.setRowHeader(jv);
        
        setVisible(true);
    }
    
    private class ATableModel extends AbstractTableModel {
        public String getColumnName(int c) {
            String name = null;
            if(c == 0) 
                name = "Index";
            else
                name = "R" + String.valueOf(c);
            return name;
        }
        public Class getColumnClass(int c) {
            return data[0][c].getClass();
        }
        public int getColumnCount() {
            return data.length + 1; 
        }
        public int getRowCount() {
            return data.length;
        }
        public Object getValueAt(int r, int c) {
            return data[r][c];
        }
        public boolean isCellEditable(int r, int c) {
            if(jRadioButton1.isSelected()) return c - r == 1;
            else return !(c == 0 || c - r > 1);
        }
        public void setValueAt(Object value, int r, int c) {
            data[r][c] = value;
        }
    }

    private class HeaderCellRenderer extends DefaultTableCellRenderer 
    {
        public Component getTableCellRendererComponent(JTable table,
            Object value, boolean isSelected, boolean hasFocus, int row, int col) 
        {
            super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,col);
            setHorizontalAlignment(SwingConstants.CENTER);
            return this;
	}
    }    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        buttonGroup1 = new javax.swing.ButtonGroup();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jTextArea1 = new javax.swing.JTextArea();
        jPanel1 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jRadioButton1 = new javax.swing.JRadioButton();
        jRadioButton2 = new javax.swing.JRadioButton();

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Random Effect Parameters Covariance");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                closeDialog(evt);
            }
        });

        jScrollPane1.setPreferredSize(new java.awt.Dimension(415, 100));
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
        jTable1.setRowSelectionAllowed(false);
        jTable1.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                jTable1KeyPressed(evt);
            }
        });

        jScrollPane1.setViewportView(jTable1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jScrollPane1, gridBagConstraints);

        jTextArea1.setBackground(new java.awt.Color(204, 204, 204));
        jTextArea1.setEditable(false);
        jTextArea1.setText("Enter initial estimates of the covariance matrix of the random effect \nparameters.  Append an \"F\" to the number if the parameter is fixed.\nNote: You must press the \"Enter\" key after typing in the last element.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 0, 12);
        getContentPane().add(jTextArea1, gridBagConstraints);

        jButton1.setText("OK");
        jButton1.setPreferredSize(new java.awt.Dimension(75, 25));
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton1);

        jButton2.setText("Cancel");
        jPanel1.add(jButton2);

        jButton3.setText("Help");
        jButton3.setPreferredSize(new java.awt.Dimension(75, 25));
        jPanel1.add(jButton3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.insets = new java.awt.Insets(12, 12, 12, 12);
        getContentPane().add(jPanel1, gridBagConstraints);

        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("Select a matrix form    ");
        jPanel2.add(jLabel1);

        jRadioButton1.setSelected(true);
        jRadioButton1.setText("Diagonal Matrix");
        buttonGroup1.add(jRadioButton1);
        jRadioButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton1ActionPerformed(evt);
            }
        });

        jPanel2.add(jRadioButton1);

        jRadioButton2.setText("Full Matrix");
        buttonGroup1.add(jRadioButton2);
        jRadioButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton2ActionPerformed(evt);
            }
        });

        jPanel2.add(jRadioButton2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        getContentPane().add(jPanel2, gridBagConstraints);

        pack();
    }//GEN-END:initComponents

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        if(jRadioButton1.isSelected())
        {
            for(int i = 0; i < data.length; i++)
            {
                String string = ((String)tableModel.getValueAt(i, i + 1)).trim();
                if(string.endsWith("F"))
                    string = string.substring(0, string.length() - 1);
                double value;
                try
                {
                    value = Double.parseDouble(string);
                }
                catch(NumberFormatException e)
                {
                    JOptionPane.showMessageDialog(null, 
                                          "The element [" + (i + 1) + "," + (i + 1) + 
                                          "] is not a floating point number.",
                                          "Input Error",    
                                          JOptionPane.ERROR_MESSAGE);                         
                    return;
                }
                if(value <= 0)
                {
                    JOptionPane.showMessageDialog(null, 
                                          "The element [" + (i + 1) + "," + (i + 1) + 
                                          "] is not a positive number.",
                                          "Input Error",    
                                          JOptionPane.ERROR_MESSAGE);                       
                    return;
                }
            }
        }
        else
        {
            for(int i = 0; i < data.length; i++)
            {
                for(int j = 1; j < i + 2; j++)
                {
                    String string = ((String)tableModel.getValueAt(i, j)).trim();
                    if(string.endsWith("F"))
                        string = string.substring(0, string.length() - 1);
                    double value;
                    try
                    {
                        value = Double.parseDouble(string);
                    }
                    catch(NumberFormatException e)
                    {
                        JOptionPane.showMessageDialog(null, 
                                          "The element [" + (i + 1) + "," + j + 
                                          "] is not a floating point number.",
                                          "Input Error",    
                                          JOptionPane.ERROR_MESSAGE);                          
                        return; 
                    }
                    if(j == i + 1 && value <= 0)
                    {
                        JOptionPane.showMessageDialog(null, 
                                          "The element [" + j + "," + j + 
                                          "] is not a positive number.",
                                          "Input Error",    
                                          JOptionPane.ERROR_MESSAGE);                       
                        return;
                    }
                }
            }
        }
                
        // Put the user entered data into source
        
        
        // Close the dialog
        dispose();
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jTable1KeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTable1KeyPressed
        jButton1.setEnabled(evt.getKeyCode() == 10);
    }//GEN-LAST:event_jTable1KeyPressed

    private void jRadioButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton2ActionPerformed
        int dimension = data.length;
        for(int i = 0; i < dimension; i++)
        {
            data[i][0] = String.valueOf(i + 1);
            for(int j = 1; j <= dimension; j++) 
                if(j - i > 1) data[i][j] = "         ---";
                else data[i][j] = "";
        }
        jTable1.repaint();
    }//GEN-LAST:event_jRadioButton2ActionPerformed

    private void jRadioButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton1ActionPerformed
        int dimension = data.length;
        for(int i = 0; i < dimension; i++)
        {
            data[i][0] = String.valueOf(i + 1);
            for(int j = 1; j <= dimension; j++) 
                if(j - i != 1) data[i][j] = "           0";
                else data[i][j] = "";
        }
        jTable1.repaint();
    }//GEN-LAST:event_jRadioButton1ActionPerformed
    
    /** Closes the dialog */
    private void closeDialog(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_closeDialog
        setVisible(false);
        dispose();
    }//GEN-LAST:event_closeDialog
    
    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        new RandParamCov(new javax.swing.JFrame(), true);
    }
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JRadioButton jRadioButton1;
    private javax.swing.JRadioButton jRadioButton2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextArea jTextArea1;
    // End of variables declaration//GEN-END:variables
    
    private Object[][] data;
    private TableModel tableModel = new ATableModel();
}
