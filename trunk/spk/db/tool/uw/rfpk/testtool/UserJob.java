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
import javax.swing.table.*;
import rfpk.spk.spkdb.*;
import javax.swing.JOptionPane;
import uw.rfpk.mda.nonmem.*;

/**
 * This is the main class of job management tool.
 * @author  Jiaji Du
 */
public class UserJob extends javax.swing.JFrame {
    
    /** Creates new form Report */
    public UserJob() {
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
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jPanel2 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        jComboBox1 = new javax.swing.JComboBox();
        jButton2 = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jTextField2 = new javax.swing.JTextField();
        jButton3 = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jLabel4 = new javax.swing.JLabel();
        jTextField3 = new javax.swing.JTextField();
        jLabel5 = new javax.swing.JLabel();
        jTextField4 = new javax.swing.JTextField();
        jLabel6 = new javax.swing.JLabel();
        jTextField5 = new javax.swing.JTextField();
        jLabel7 = new javax.swing.JLabel();
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

        setTitle("User Job Manager");
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

        jLabel2.setText("Username");
        jPanel2.add(jLabel2);

        jTextField1.setText("scientist");
        jTextField1.setPreferredSize(new java.awt.Dimension(80, 19));
        jPanel2.add(jTextField1);

        jLabel3.setText("End Code");
        jPanel2.add(jLabel3);

        jComboBox1.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "cerr", "srun" }));
        jComboBox1.setSelectedIndex(1);
        jComboBox1.setPreferredSize(new java.awt.Dimension(60, 19));
        jPanel2.add(jComboBox1);

        jButton2.setText("Add Report");
        jButton2.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jButton2.setPreferredSize(new java.awt.Dimension(80, 19));
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton2);

        jLabel1.setText("No. of Jobs");
        jPanel2.add(jLabel1);

        jTextField2.setPreferredSize(new java.awt.Dimension(60, 19));
        jPanel2.add(jTextField2);

        jButton3.setText("Add Job");
        jButton3.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jButton3.setPreferredSize(new java.awt.Dimension(80, 19));
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        jPanel2.add(jButton3);

        getContentPane().add(jPanel2, java.awt.BorderLayout.NORTH);

        jLabel4.setText("Database Host");
        jPanel1.add(jLabel4);

        jTextField3.setText("rose.rfpk.washington.edu");
        jPanel1.add(jTextField3);

        jLabel5.setText("Name");
        jPanel1.add(jLabel5);

        jTextField4.setText("spktest");
        jTextField4.setPreferredSize(new java.awt.Dimension(80, 19));
        jPanel1.add(jTextField4);

        jLabel6.setText("Username");
        jPanel1.add(jLabel6);

        jTextField5.setText("tester");
        jTextField5.setPreferredSize(new java.awt.Dimension(80, 19));
        jPanel1.add(jTextField5);

        jLabel7.setText("Password");
        jPanel1.add(jLabel7);

        jTextField6.setText("tester");
        jTextField6.setPreferredSize(new java.awt.Dimension(80, 19));
        jPanel1.add(jTextField6);

        getContentPane().add(jPanel1, java.awt.BorderLayout.SOUTH);

        pack();
    }//GEN-END:initComponents

    private void jTable1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTable1MouseClicked
        int index = jTable1.getSelectedRow();
        if(index == -1)
            return; 
        long jobId = Long.parseLong(((String[][])lists.get(indexList))[index][0]);       
        reportDialog.dispose(); 
        
        try
        {
            // Connect to the database
            Connection con = Spkdb.connect(jTextField4.getText(),  
                                           jTextField3.getText(), 
                                           jTextField5.getText(), 
                                           jTextField6.getText());             

            Spkdb.endJob(con, 
                         jobId,
                         (String)jComboBox1.getSelectedItem(),  
                         jTextArea1.getText()); 
            
            // Disconnect to the database
            Spkdb.disconnect(con);  
            
            JOptionPane.showMessageDialog(null, "The job report was put in the database.",
                                          "Database Information", JOptionPane.INFORMATION_MESSAGE);
        }
        catch(SpkdbException e) 
        {
            JOptionPane.showMessageDialog(null, e, "SpkdbException", JOptionPane.ERROR_MESSAGE);        
        }
        catch(SQLException e) 
        {
            JOptionPane.showMessageDialog(null, e, "SQLException", JOptionPane.ERROR_MESSAGE);        
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
        if(jTextField1.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null, "User Name is required.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }
        String spkInput = jTextArea1.getText();
        if(spkInput.indexOf("<spksource>") == -1 || spkInput.indexOf("<spkdata") == -1 || 
           spkInput.indexOf("<spkmodel>") == -1)
        {
            JOptionPane.showMessageDialog(null, "The text is not a SPK input file", 
                                          "Job Submission Error", 
                                          JOptionPane.ERROR_MESSAGE);          
            return;
        }
        String n = jTextField2.getText();

        if(!Utility.isPosIntNumber(n))
        {
            JOptionPane.showMessageDialog(null, "The No. of Jobs must be a positive integer.", 
                                          "Job Submission Error", 
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }        
        int m = Integer.parseInt(n);
        
        String s = JOptionPane.showInputDialog(null, "Enter a starting number for job, model and dataset names.");

        if(s == null || s.equals(""))
        {
            JOptionPane.showMessageDialog(null, "A starting number is required.", 
                                          "Input Error", 
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }
        if(!Utility.isPosIntNumber(s))
        {
            JOptionPane.showMessageDialog(null, "The starting number must be a positive integer.", 
                                          "Input Error", 
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }        
        long l = Integer.parseInt(s);        

        ArchiveInfo modelArchive = new ArchiveInfo();
        ArchiveInfo dataArchive = new ArchiveInfo();
        for(int i = 0; i < m; i++) 
        {
            String jobAbstract = "Job description " + (i + l);
        
            // Collect model archive information 
            modelArchive.name = "Model " + (i + l);
            modelArchive.description = "Test model " + (i + l); 
            modelArchive.version = "1";
            modelArchive.isNewArchive = true; 
            modelArchive.isNewVersion = false;
            modelArchive.log = "";
  
            // Collect data archive information 
            dataArchive.name = "Dataset " + (i + l);
            dataArchive.description = "Test dataset " + (i + l); 
            dataArchive.version = "1";
            dataArchive.isNewArchive = true; 
            dataArchive.isNewVersion = false;
            dataArchive.log = "";

            // Get the XML documents as String objects
            int index1 = spkInput.indexOf("<spkdata");
            int index2 = spkInput.indexOf("<spkmodel");        
            String source = spkInput.substring(0, index1 - 22);
            String dataset = spkInput.substring(index1 - 22, index2 - 22);
            String model = XMLReader.getModelArchive(spkInput.substring(index2 - 22));

            // Submit the job
            Database database = new Database(jTextField4.getText(),  
                                             jTextField3.getText(), 
                                             jTextField5.getText(), 
                                             jTextField6.getText(),
                                             jTextField1.getText());
            database.submitJob(source, dataset, model, jobAbstract, modelArchive, dataArchive); 
        }
    }//GEN-LAST:event_jButton3ActionPerformed

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        if(jTextField1.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null, "User Name is required.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }
        if(jTextArea1.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null, "Job report is not loaded.", "Input Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }        
        indexList = 0;
        lists = new Vector();
        showArchiveList(0);
    }//GEN-LAST:event_jButton2ActionPerformed

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
            jTextArea1.setCaretPosition(0);
        }
        catch(IOException e)
        {
            JOptionPane.showMessageDialog(null, e, "IOException", JOptionPane.ERROR_MESSAGE);        
        }         
    }//GEN-LAST:event_jButton1ActionPerformed

    // Display a list of library models
    private void showArchiveList(int indexList)
    {
        String[] header = new String[]{"Submission Time", "State Code", "End Code", "Description"};
        String title = "Job List";
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
                                             jTextField3.getText(), 
                                             jTextField5.getText(), 
                                             jTextField6.getText(),
                                             jTextField1.getText());
            
            archiveList = database.getUserJobs(maxNum + 1, leftOff);

            if(archiveList == null)
            {
                JOptionPane.showMessageDialog(null, "No job was found in the database.",
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
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        new UserJob().show();
    }
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JComboBox jComboBox1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel4;
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

    // List collection
    private Vector lists = null;
    
    // Index of the list in list collection
    private int indexList = 0; 

    // Maximum number of items
    private static final int maxNum = 12;     
}
