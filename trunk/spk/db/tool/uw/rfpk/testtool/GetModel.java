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
 * This is the main class of model archive management tool.
 * @author  Jiaji Du
 */
public class GetModel extends javax.swing.JFrame {
    
    /** Creates new form Report */
    public GetModel() {
        initComponents();
        jTextField1.setText(dbHost);
        jTextField4.setText(dbName);
        jTextField5.setText(userName);
        jTextField6.setText(password);        
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
        jLabel4 = new javax.swing.JLabel();
        jTextField3 = new javax.swing.JTextField();
        jButton3 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        jButton4 = new javax.swing.JButton();
        jButton5 = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
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
        setTitle("Get Model Archive");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        jScrollPane1.setViewportView(jTextArea1);

        getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jLabel4.setText("User Name");
        jPanel2.add(jLabel4);

        jTextField3.setPreferredSize(new java.awt.Dimension(100, 19));
        jPanel2.add(jTextField3);

        jButton3.setText("Get Archive");
        jButton3.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jButton3.setPreferredSize(new java.awt.Dimension(100, 19));
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton3);

        jButton2.setText("Update");
        jButton2.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jButton2.setPreferredSize(new java.awt.Dimension(90, 19));
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton2);

        jButton4.setText("Get Model");
        jButton4.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jButton4.setPreferredSize(new java.awt.Dimension(90, 19));
        jButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton4ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton4);

        jButton5.setText("New Version");
        jButton5.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jButton5.setPreferredSize(new java.awt.Dimension(97, 19));
        jButton5.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton5ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton5);

        jButton1.setText("Save File");
        jButton1.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jButton1.setPreferredSize(new java.awt.Dimension(90, 19));
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton1);

        getContentPane().add(jPanel2, java.awt.BorderLayout.NORTH);

        jLabel2.setText("Database Host");
        jPanel1.add(jLabel2);

        jTextField1.setPreferredSize(new java.awt.Dimension(140, 19));
        jPanel1.add(jTextField1);

        jLabel3.setText("Name");
        jPanel1.add(jLabel3);

        jTextField4.setPreferredSize(new java.awt.Dimension(80, 19));
        jPanel1.add(jTextField4);

        jLabel5.setText("Username");
        jPanel1.add(jLabel5);

        jTextField5.setPreferredSize(new java.awt.Dimension(80, 19));
        jPanel1.add(jTextField5);

        jLabel6.setText("Password");
        jPanel1.add(jLabel6);

        jTextField6.setPreferredSize(new java.awt.Dimension(80, 19));
        jPanel1.add(jTextField6);

        getContentPane().add(jPanel1, java.awt.BorderLayout.SOUTH);

        pack();
    }//GEN-END:initComponents

    private void jButton5ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton5ActionPerformed
        if(jTextField3.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null, "User Name is required.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }
        action = "send";
        indexList = 0;
        lists = new Vector();
        showArchiveList(0);        
    }//GEN-LAST:event_jButton5ActionPerformed

    private void jButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton4ActionPerformed
        if(jTextField3.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null, "User Name is required.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }
        action = "get model";
        indexList = 0;
        lists = new Vector();
        showArchiveList(0);
    }//GEN-LAST:event_jButton4ActionPerformed

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        if(jTextField3.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null, "User Name is required.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }
        if(jTextArea1.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null, "There is no text for the source.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }
        action = "update";
        indexList = 0;
        lists = new Vector();
        showArchiveList(0);           
    }//GEN-LAST:event_jButton2ActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        if(jTextArea1.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null, "There is no text to save.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }
        JFileChooser files = new JFileChooser();
        int result = files.showSaveDialog(null);
        if(result == files.APPROVE_OPTION)
	{
            File file = files.getSelectedFile();
            try
            {
                BufferedWriter out = new BufferedWriter(new FileWriter(file));
                out.write(jTextArea1.getText());
                out.close();
            }
            catch(IOException e )
            {
                JOptionPane.showMessageDialog(null, e, "IOException", JOptionPane.ERROR_MESSAGE);
            }
        }
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jTable1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTable1MouseClicked
        int index = jTable1.getSelectedRow();
        if(index == -1)
            return; 
        long modelId = Long.parseLong(((String[][])lists.get(indexList))[index][0]); 
        String name = ((String[][])lists.get(indexList))[index][1];
        reportDialog.dispose(); 
        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(jTextField4.getText(),  
                                           jTextField1.getText(), 
                                           jTextField5.getText(), 
                                           jTextField6.getText());
            
            if(action.startsWith("get"))
            {
                ResultSet modelRS = Spkdb.getModel(con, modelId);
            
                // Get model and display it
                modelRS.next();
                Blob blobArchive = modelRS.getBlob("archive");
                long length = blobArchive.length();
                String archive = new String(blobArchive.getBytes(1L, (int)length));
                if(action.endsWith("archive"))
                    jTextArea1.setText(archive);
                if(action.endsWith("model"))
                {
                    Archive arch = new Archive("", new ByteArrayInputStream(archive.getBytes()));
                    jTextArea1.setText(ToString.arrayToString(arch.getRevision(), "\n"));
                }
                jTextArea1.setCaretPosition(0);
            }
            if(action.equals("update"))
            {
                String sql = "update model set archive='" + jTextArea1.getText() + 
                             "' where model_id='" + modelId + "'";
                Statement stmt = con.createStatement();
	        stmt.executeUpdate(sql);
                JOptionPane.showMessageDialog(null, 
                                              "Model '" + name + "' archive was updated.",  
                                              "Information",
                                              JOptionPane.INFORMATION_MESSAGE);                
            }
            if(action.equals("send"))
            {
                // Get librarian user id
                ResultSet userRS = Spkdb.getUser(con, "librarian");
                userRS.next();
                long userId = userRS.getLong("user_id");
                
                // get model and send it to library
                ResultSet modelRS = Spkdb.getModel(con, modelId);
                modelRS.next();
                Blob blobArchive = modelRS.getBlob("archive");
                long length = blobArchive.length();
                String strAr = new String(blobArchive.getBytes(1L, (int)length));
                
                String versionLog = JOptionPane.showInputDialog("Enter log for the new version (<=100 characters)");
                if(versionLog == null)
                    versionLog = "";
                
                Archive arch = new Archive("", new ByteArrayInputStream(strAr.getBytes()));
                arch.addRevision(jTextArea1.getText().split("\n"), versionLog);
                arch.findNode(arch.getRevisionVersion()).setAuthor(jTextField3.getText());
                if(Spkdb.updateModel(con, 
                                     modelId, 
                                     new String[]{"archive"}, 
                                     new String[]{arch.toString("\n")})) 
                JOptionPane.showMessageDialog(null, 
                                              "The Model '" + name + "' was updated to version " + 
                                              String.valueOf(arch.getRevisionVersion().last()) + ".", 
                                              "Information",
                                              JOptionPane.INFORMATION_MESSAGE);                    
            }
            
            // Disconnect to the database
            Spkdb.disconnect(con);
          
        }
        catch(SpkdbException e) 
        {
            JOptionPane.showMessageDialog(null, e, "SpkdbException", JOptionPane.ERROR_MESSAGE); 
        }
        catch(SQLException e) 
        {
            JOptionPane.showMessageDialog(null, e, "SQLException", JOptionPane.ERROR_MESSAGE);        
        }
        catch(InvalidFileFormatException e)
        { 
            JOptionPane.showMessageDialog(null, e, "InvalidFileFormatException", JOptionPane.ERROR_MESSAGE);
        }
        catch(ParseException e)
        { 
            JOptionPane.showMessageDialog(null, e, "ParseException", JOptionPane.ERROR_MESSAGE);
        }
        catch(PatchFailedException e)
        {
            JOptionPane.showMessageDialog(null, e, "PatchFailedException", JOptionPane.ERROR_MESSAGE);
        }
        catch(DiffException e)
        {
            JOptionPane.showMessageDialog(null, e, "DiffException", JOptionPane.ERROR_MESSAGE);         
        }        
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
        if(jTextField3.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null, "User Name is required.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }
        action = "get archive";
        indexList = 0;
        lists = new Vector();
        showArchiveList(0);
    }//GEN-LAST:event_jButton3ActionPerformed

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
            
            Database database = new Database(jTextField4.getText(),  
                                             jTextField1.getText(), 
                                             jTextField5.getText(), 
                                             jTextField6.getText(),
                                             jTextField3.getText());
            
            archiveList = database.getUserModels(maxNum + 1, leftOff);

            if(archiveList == null)
            {
                JOptionPane.showMessageDialog(null, "No model was found in the database.",
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
    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
        System.exit(0);
    }//GEN-LAST:event_exitForm
    
    /** The main method that creates the application object and displays it.
     * @param args the command line arguments, not being used.
     */
    public static void main(String args[]) {
        new GetModel().show();
    }
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JButton jButton4;
    private javax.swing.JButton jButton5;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextField jTextField4;
    private javax.swing.JTextField jTextField5;
    private javax.swing.JTextField jTextField6;
    private javax.swing.JButton nextButton;
    private javax.swing.JButton previousButton;
    private javax.swing.JDialog reportDialog;
    // End of variables declaration//GEN-END:variables

    // List collection
    private Vector lists = null;
    
    // Index of the list in list collection
    private int indexList = 0; 
    
    // Type of action
    private String action = null;
    
    // Maximum number of items
    private static final int maxNum = 12;
    
    // Database host
    private static final String  dbHost = "192.168.1.2";
    
    // Database name
    private static final String  dbName = "spkdb";
    
    // Database username
    private static final String  userName = "daemon";
    
    // Database password
    private static final String  password = "daemon";    
}
