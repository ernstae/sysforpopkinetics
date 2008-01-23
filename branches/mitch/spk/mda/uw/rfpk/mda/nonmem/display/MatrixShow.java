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
package uw.rfpk.mda.nonmem.display;

import uw.rfpk.mda.nonmem.Utility;
import javax.swing.table.*;
import javax.swing.JTable;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.awt.*;
import javax.swing.JViewport;
import javax.swing.SwingConstants;

/** This class displays a matrix show on the screen.
 *
 * @author  Jiaji Du
 */
public class MatrixShow extends javax.swing.JFrame {
    
    /** Creates new form MatrixShow.
     * @param data a String[][] object containing the matrix of data values.
     * @param header a String[] object containing the headers.
     * @param title a String object containing the title of the window.   
     * @param text a String[] object containing the text to display in the window.
     * @param width the width of the window.
     * @param height the height of the window.
     * @param x horizontal location.
     * @param y vertical location.
     * @param isDiagonal true if the matrix is diagonal, false otherwise.
     */
    public MatrixShow(String[][] data, String[] header, String title, String text,
                      int width, int height, int x, int y, boolean isDiagonal)
    {
        initComponents();
        setTitle(title);
        setSize(width, height);
        Point point = getLocation();
        point.translate(x,  y);
        setLocation(point);
        jTextPane1.setText(text);
//        jScrollPane1.setPreferredSize(new java.awt.Dimension(width - 20, height - 100));
//        jTable1.setPreferredSize(new java.awt.Dimension(width - 100, height - 100));
        
        // Format the data
        int dimension = data.length;
        DecimalFormat f = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
        f.applyPattern("0.00E00");
        if(isDiagonal)
            for(int i = 0; i < dimension; i++)
            {
                if(!data[i][i + 1].equalsIgnoreCase("nan") && !data[i][i + 1].endsWith("inf") && !data[i][i + 1].endsWith("Infinity"))
                    data[i][i + 1] = Utility.formatData(6, f.format(Double.parseDouble(data[i][i + 1])));
                else
                    data[i][i + 1] = "   N/A";
            }
        else
            for(int i = 0; i < dimension; i++)
                for(int j = 1; j < data[i].length; j++)
                {
                    if(!data[i][j].equalsIgnoreCase("nan") && !data[i][j].endsWith("inf") && !data[i][j].endsWith("Infinity"))
                        data[i][j] = Utility.formatData(6, f.format(Double.parseDouble(data[i][j])));
                    else
                        data[i][j] = "   N/A";
                }

        // Set table model
        DisplayTableModel tableModel = new DisplayTableModel(data, header);
        jTable1.setModel(tableModel);
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        setVisible(true);    
        
        // Create a column model for the main table.  
        TableColumnModel cm = new DefaultTableColumnModel() {
            boolean first = true;
            public void addColumn(TableColumn tc) {
                // Drop the first column that will be the row header.
                if(first) {first = false; return; }
                tc.setMinWidth(80);
                super.addColumn(tc);
            }
        };
        // Create a column model that will serve the row header table.
        TableColumnModel rowHeaderModel = new DefaultTableColumnModel() {
            boolean first = true; 
            public void addColumn(TableColumn tc) {
                if(first) {
                    tc.setMaxWidth(80);
                    super.addColumn(tc);
                    first = false;
                }
                // Drop the rest of the col;umns.
            }
        };
        
        jTable1.setColumnModel(cm);
        
        // Set up the header column and get it hooked up to everything.
        JTable headerColumn = new JTable(tableModel, rowHeaderModel); 
        jTable1.createDefaultColumnsFromModel();
        headerColumn.createDefaultColumnsFromModel();
        jTable1.setSelectionModel(headerColumn.getSelectionModel());
        jTable1.setRowSelectionAllowed(true);
        jTable1.setCellSelectionEnabled(true);
        jTable1.setColumnSelectionAllowed(true);
        rowHeaderModel.getColumn(0).setCellRenderer(new HeaderCellRenderer());
        
        // Make the header column look pretty.
        headerColumn.setRowHeight(20);
        headerColumn.setMaximumSize(new Dimension(80, dimension));
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
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jTextPane1 = new javax.swing.JTextPane();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setLocationRelativeTo(jTextPane1);
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        jScrollPane1.setFont(new java.awt.Font("Dialog", 0, 14));
        jTable1.setFont(new java.awt.Font("Monospaced", 0, 12));
        jTable1.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ) {
            boolean[] canEdit = new boolean [] {
                false, false, false, false
            };

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
        jTable1.setCellSelectionEnabled(true);
        jTable1.setName("");
        jTable1.setRowHeight(20);
        jScrollPane1.setViewportView(jTable1);

        getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jTextPane1.setEditable(false);
        jTextPane1.setPreferredSize(null);
        getContentPane().add(jTextPane1, java.awt.BorderLayout.NORTH);

        pack();
    }//GEN-END:initComponents
    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
//        System.exit(0);
    }//GEN-LAST:event_exitForm

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
        public int getRowCount() {
            return data.length;
        }
        public Object getValueAt(int r, int c) {
            String value = "";
            if(c < data[r].length)
                value = data[r][c];
            return value;
        }

        // Table data array
        String[][] data = null;
        
        // Table header array
        String[] header = null;
    }
    
    private class HeaderCellRenderer extends DefaultTableCellRenderer 
    {
        public Component getTableCellRendererComponent(JTable table,
            Object value,boolean isSelected, boolean hasFocus, int row,int col) 
        {
            super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,col);
            setHorizontalAlignment(SwingConstants.CENTER);
            return this;
	}
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextPane jTextPane1;
    // End of variables declaration//GEN-END:variables

}
