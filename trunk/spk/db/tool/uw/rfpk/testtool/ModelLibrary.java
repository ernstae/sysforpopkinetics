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
package uw.rfpk.testtool;

import java.io.*;
import java.sql.*;
import java.util.Vector;
import javax.swing.*;
import rfpk.spk.spkdb.*;
import javax.swing.JOptionPane;
import uw.rfpk.mda.nonmem.*;
import javax.swing.table.*;
import org.apache.commons.jrcs.rcs.*;
import org.apache.commons.jrcs.util.ToString;
import org.apache.commons.jrcs.diff.*;
import java.text.SimpleDateFormat;

/**
 * This is the main class of model library management tool.
 * @author  Jiaji
 */
public class ModelLibrary extends javax.swing.JFrame {
    
    /** Creates new form Report */
    public ModelLibrary() {
        initComponents();
        setSize(700, 500);
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        reportDialog = new javax.swing.JDialog();
        jPanel4 = new javax.swing.JPanel();
        previousButton = new javax.swing.JButton();
        nextButton = new javax.swing.JButton();
        jScrollPane3 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        buttonGroup1 = new javax.swing.ButtonGroup();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jPanel2 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jRadioButton1 = new javax.swing.JRadioButton();
        jRadioButton2 = new javax.swing.JRadioButton();
        jLabel4 = new javax.swing.JLabel();
        jTextField2 = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        jTextField3 = new javax.swing.JTextField();
        jButton3 = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        jTextField4 = new javax.swing.JTextField();
        jLabel5 = new javax.swing.JLabel();
        jTextField5 = new javax.swing.JTextField();
        jLabel6 = new javax.swing.JLabel();
        jTextField6 = new javax.swing.JTextField();

        reportDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        reportDialog.setTitle("");
        reportDialog.setBackground(java.awt.Color.white);
        reportDialog.setModal(true);
        previousButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("/org/netbeans/ui/wizard/plaf/basic/icons/back.gif")));
        previousButton.setText("Previous");
        previousButton.setMaximumSize(new java.awt.Dimension(127, 26));
        previousButton.setMinimumSize(new java.awt.Dimension(127, 26));
        previousButton.setPreferredSize(new java.awt.Dimension(127, 26));
        previousButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                previousButtonActionPerformed(evt);
            }
        });

        jPanel4.add(previousButton);

        nextButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("/org/netbeans/ui/wizard/plaf/basic/icons/next.gif")));
        nextButton.setText("Next");
        nextButton.setMaximumSize(new java.awt.Dimension(127, 26));
        nextButton.setMinimumSize(new java.awt.Dimension(127, 26));
        nextButton.setPreferredSize(new java.awt.Dimension(127, 26));
        nextButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                nextButtonActionPerformed(evt);
            }
        });

        jPanel4.add(nextButton);

        reportDialog.getContentPane().add(jPanel4, java.awt.BorderLayout.SOUTH);

        jTable1.setFont(new java.awt.Font("Monospaced", 0, 12));
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
        jTable1.setShowHorizontalLines(false);
        jTable1.setShowVerticalLines(false);
        jTable1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jTable1MouseClicked(evt);
            }
        });

        jScrollPane3.setViewportView(jTable1);

        reportDialog.getContentPane().add(jScrollPane3, java.awt.BorderLayout.CENTER);

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Model Library Manager");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        jScrollPane1.setViewportView(jTextArea1);

        getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jButton1.setText("Open File");
        jButton1.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jButton1.setPreferredSize(new java.awt.Dimension(80, 19));
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton1);

        jRadioButton1.setText("New Model");
        buttonGroup1.add(jRadioButton1);
        jRadioButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton1ActionPerformed(evt);
            }
        });

        jPanel2.add(jRadioButton1);

        jRadioButton2.setText("New Version");
        buttonGroup1.add(jRadioButton2);
        jRadioButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton2ActionPerformed(evt);
            }
        });

        jPanel2.add(jRadioButton2);

        jLabel4.setText("  Name");
        jPanel2.add(jLabel4);

        jTextField2.setPreferredSize(new java.awt.Dimension(60, 19));
        jTextField2.setEnabled(false);
        jPanel2.add(jTextField2);

        jLabel1.setText("Description");
        jPanel2.add(jLabel1);

        jTextField3.setPreferredSize(new java.awt.Dimension(100, 19));
        jTextField3.setEnabled(false);
        jPanel2.add(jTextField3);

        jButton3.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jButton3.setPreferredSize(new java.awt.Dimension(80, 19));
        jButton3.setEnabled(false);
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton3);

        getContentPane().add(jPanel2, java.awt.BorderLayout.NORTH);

        jLabel2.setText("Database Host");
        jPanel1.add(jLabel2);

        jTextField1.setText("rose.rfpk.washington.edu");
        jPanel1.add(jTextField1);

        jLabel3.setText("Name");
        jPanel1.add(jLabel3);

        jTextField4.setText("spktest");
        jTextField4.setPreferredSize(new java.awt.Dimension(80, 19));
        jPanel1.add(jTextField4);

        jLabel5.setText("Username");
        jPanel1.add(jLabel5);

        jTextField5.setText("tester");
        jTextField5.setPreferredSize(new java.awt.Dimension(80, 19));
        jPanel1.add(jTextField5);

        jLabel6.setText("Password");
        jPanel1.add(jLabel6);

        jTextField6.setText("tester");
        jTextField6.setPreferredSize(new java.awt.Dimension(80, 19));
        jPanel1.add(jTextField6);

        getContentPane().add(jPanel1, java.awt.BorderLayout.SOUTH);

        pack();
    }//GEN-END:initComponents

    private void jRadioButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton2ActionPerformed
        modelStatus = "old";
        jTextField2.setEnabled(false);
        jTextField3.setEnabled(false);
        jButton3.setText("Update");
        jButton3.setEnabled(true);        
        indexList = 0;
        lists = new Vector();
        showArchiveList(0);
    }//GEN-LAST:event_jRadioButton2ActionPerformed

    private void jRadioButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton1ActionPerformed
        modelStatus = "new";
        jTextField2.setEnabled(true);
        jTextField3.setEnabled(true);
        jButton3.setText("Add");
        jButton3.setEnabled(true);
    }//GEN-LAST:event_jRadioButton1ActionPerformed

    private void jTable1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTable1MouseClicked
        int index = jTable1.getSelectedRow();
        if(index == -1)
            return; 
        modelId = Long.parseLong(((String[][])lists.get(indexList))[index][0]); 
        versionLog = JOptionPane.showInputDialog("Enter log for the new version (<=100 characters)");
        if(versionLog == null)
            versionLog = "";
        reportDialog.dispose();        
    }//GEN-LAST:event_jTable1MouseClicked

    private void nextButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_nextButtonActionPerformed
        showArchiveList(++indexList);
        if(indexList != 0)
            previousButton.setEnabled(true);
    }//GEN-LAST:event_nextButtonActionPerformed

    private void previousButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_previousButtonActionPerformed
        showArchiveList(--indexList); 
        if(indexList == 0)
            previousButton.setEnabled(false);
        nextButton.setEnabled(true);        
    }//GEN-LAST:event_previousButtonActionPerformed

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        String model = jTextArea1.getText();
        if(model.indexOf("$PRO") == -1)
        {
            JOptionPane.showMessageDialog(null, "The model file is not loaded.",  
                                          "Input Error",
                                          JOptionPane.ERROR_MESSAGE);   
            return;
        }
        if(jTextField2.equals(""))
        {
            JOptionPane.showMessageDialog(null, "The model must have a name.",  
                                          "Input Error",
                                          JOptionPane.ERROR_MESSAGE);   
            return;
        }
        try
        {
            Connection con = Spkdb.connect(jTextField4.getText(), 
                                           jTextField1.getText(), 
                                           jTextField5.getText(), 
                                           jTextField6.getText());
            
            String author = JOptionPane.showInputDialog(null, "Enter author's name.");
            if(author == null || author.equals(""))          
                author = "unknown";
            if(modelStatus.equals("new"))
            {
                Archive arch = new Archive(model.split("\n"), ""); 
                arch.findNode(new Version("1.1")).setAuthor(author); 
                ResultSet userRS = Spkdb.getUser(con, "librarian");
                userRS.next();
                long userId = userRS.getLong("user_id"); 
                Spkdb.newModel(con, 
                               userId, 
                               jTextField2.getText(), 
                               jTextField3.getText(),
                               arch.toString("\n"));
                JOptionPane.showMessageDialog(null, "A new model, " + jTextField2.getText() +
                                              ", has been added to the database.",  
                                              "Model Archive Information",
                                              JOptionPane.INFORMATION_MESSAGE);
            }
            else
            {         
                ResultSet modelRS = Spkdb.getModel(con, 
                                                   modelId);
                modelRS.next();
                String strAr = modelRS.getString("archive");
                Archive arch = new Archive("", new ByteArrayInputStream(strAr.getBytes()));
                arch.addRevision(jTextArea1.getText().split("\n"), versionLog);
                int number = arch.getRevisionVersion().last();
                arch.findNode(new Version("1." + number)).setAuthor(author); 
                Spkdb.updateModel(con, 
                                  modelId, 
                                  new String[]{"archive"}, 
                                  new String[]{arch.toString("\n")});                                   
                JOptionPane.showMessageDialog(null, "The model, " + jTextField2.getText() +
                                                  ", in the database has been updated.",  
                                                  "Model Archive Information",
                                                  JOptionPane.INFORMATION_MESSAGE);                    
            }
                 
            // Disconnect to the database
            Spkdb.disconnect(con);           
        }
        catch(SQLException e)
        {
        }
        catch(SpkdbException e)
        {
        }  
        catch(ParseException e)
        { 
        }   
        catch(InvalidFileFormatException e)
        { 
        }
        catch(DiffException e)
        { 
        }
    }//GEN-LAST:event_jButton3ActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        JFileChooser files = new JFileChooser();
        int result = files.showOpenDialog(null);
        if(result != files.APPROVE_OPTION) 
	    return;
        File file = files.getSelectedFile();
        StringBuffer buffer = new StringBuffer();        
        try
        {
            BufferedReader reader = new BufferedReader(new FileReader(file));
            boolean done = false;
            while(!done)
            {
                // Read a line
                String line = reader.readLine();                            
                if(line == null) 
                    done = true;
                else
                    buffer.append(line).append("\n");
            }	    
            reader.close(); 
            
            // Display the file text
            jTextArea1.setText(buffer.toString());
        }
        catch(IOException e){}         
    }//GEN-LAST:event_jButton1ActionPerformed

    // Display a list of library models
    private void showArchiveList(int indexList)
    {
        String[] header = new String[]{"Model Name", "No. of Versions", "Last Revised Time", "Description"};
        String title = "Model List";
        String[][] archiveList = null;

        if(indexList < lists.size())
        {
            archiveList = (String[][])lists.get(indexList);
            if(archiveList.length <= maxNum)
                nextButton.setEnabled(false);
        }
        else
        {
            long leftOff = 0L;
            if(indexList != 0)
                leftOff = Long.parseLong(((String[][])lists.get(indexList - 1))[maxNum - 1][0]);
       
            Vector modelList = new Vector();
            try
            {
                Connection con = Spkdb.connect(jTextField4.getText(), 
                                               jTextField1.getText(), 
                                               jTextField5.getText(), 
                                               jTextField6.getText());  
                ResultSet userRS = Spkdb.getUser(con, "librarian");
                userRS.next();
                long userId = userRS.getLong("user_id"); 
                ResultSet userModelsRS = Spkdb.userModels(con, 
                                                          userId, 
                                                          maxNum + 1, 
                                                          leftOff);
                // Fill in the List
                SimpleDateFormat formater = new SimpleDateFormat("EEE, MMM, d yyyy 'at' HH:mm:ss z");
                while(userModelsRS.next())
                {                  
                    // Get model id
                    long modelId = userModelsRS.getLong("model_id"); 
                    
                    // Get model archive
                    ResultSet modelRS = Spkdb.getModel(con, modelId);
                    modelRS.next();
                    String modelArchive = modelRS.getString("archive");
                    Archive archive = new Archive("", new ByteArrayInputStream(modelArchive.getBytes()));
                    
                    // Fill in the list 
                    String[] model = new String[5];
                    model[0] = String.valueOf(modelId); 
                    model[1] = userModelsRS.getString("name");
                    model[2] = String.valueOf(archive.getRevisionVersion().last());
                    model[3] = archive.findNode(archive.getRevisionVersion()).getDate().toString();
                    model[4] = userModelsRS.getString("abstract");
                    modelList.add(model);
                }
                
                // Disconnect to the database
                Spkdb.disconnect(con);
                
                // Put the list in the String[][]
                int nModel = modelList.size(); 
                if(nModel > 0)
                {
                    archiveList = new String[nModel][5]; 
                    for(int i = 0; i < nModel; i++)
                        archiveList[i] = (String[])modelList.get(i);
                }
            }
            catch(SQLException e)
            {
            }
            catch(SpkdbException e)
            {
            }
            catch(ParseException e)
            { 
            }            
            if(archiveList == null)
            {
                JOptionPane.showMessageDialog(null, "No library model was found in the database.",
                                                  "Database Information",
                                                  JOptionPane.INFORMATION_MESSAGE);               
                return;
            }
            
            // Add the list to the collection
            lists.add(archiveList);
            if(archiveList.length <= maxNum)
                // Turn off the next button
                nextButton.setEnabled(false); 
            else
                // Turn on the next button
                nextButton.setEnabled(true);            

            if(indexList == 0)
                previousButton.setEnabled(false);
        }
        
        // Put the list in a table and then show the dialog containing the table
        if(archiveList.length < 0)
            return;

        DisplayTableModel reportModel = new DisplayTableModel(archiveList, header, 1);  
        jTable1.setModel(reportModel); 
        TableColumnModel columnModel = jTable1.getColumnModel();
        columnModel.getColumn(1).setCellRenderer(new CellRenderer());        
        columnModel.getColumn(header.length - 1).setPreferredWidth(500);
        int length = archiveList.length;
        if(length > maxNum)
            length--;
        reportDialog.setSize(800, 16 * length + 90);  
        reportDialog.setTitle(title);
        reportDialog.show();
    } 
    
    private class DisplayTableModel extends AbstractTableModel 
    {
        public DisplayTableModel(String[][] data, String[] header, int start)
        {
            this.data = data;
            this.header = header;
            this.start = start;
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
        public int getRowCount()
        {
            int length = data.length;
            if(length > maxNum)
                length--;
            return length;
        }
        public Object getValueAt(int r, int c)
        {
            return data[r][c + start];
        }

        // Table data array
        String[][] data = null;
        
        // Table header array
        String[] header = null;
        
        // Starting column
        int start = 0;
    }

    private class CellRenderer extends DefaultTableCellRenderer 
    {
        public java.awt.Component getTableCellRendererComponent(JTable table,
            Object value,boolean isSelected, boolean hasFocus, int row,int col) 
        {
            super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,col);
            setHorizontalAlignment(SwingConstants.CENTER);
            return this;
	}
    }
    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
        System.exit(0);
    }//GEN-LAST:event_exitForm
    
    /** The main method that creates the application object and displays it.
     * @param args the command line arguments, not being used.
     */
    public static void main(String args[]) {
        new ModelLibrary().show();
    }
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton3;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JRadioButton jRadioButton1;
    private javax.swing.JRadioButton jRadioButton2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextField jTextField4;
    private javax.swing.JTextField jTextField5;
    private javax.swing.JTextField jTextField6;
    private javax.swing.JButton nextButton;
    private javax.swing.JButton previousButton;
    private javax.swing.JDialog reportDialog;
    // End of variables declaration//GEN-END:variables
    
    // Model status
    private String modelStatus = "old";
    
    // Model id
    private long modelId = 0L;
    
    // Version log
    private String versionLog = "";
    
    // List collection
    private Vector lists = null;
    
    // Index of the list in list collection
    private int indexList = 0; 
    
    // Maximum number of items
    private static final int maxNum = 25; 
}
