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
import uw.rfpk.mda.nonmem.XMLReader;
import org.netbeans.ui.wizard.*;
import java.util.Vector;
import java.io.File;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter;
import javax.swing.text.BadLocationException;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * This class defines a step to create the $DATA record.
 * @author  Jiaji Du
 */
public class Data extends javax.swing.JPanel implements WizardStep {
    
    private StepDescriptor sd = new MyStepDescriptor(); 
    private JComponent panel = this;
    private MDAIterator iterator = null;
    private JWizardPane wizardPane = null;
    private boolean isValid = false;
    private boolean isHighlight = false;    
    private DefaultHighlighter highlighter = new DefaultHighlighter();
    private DefaultHighlighter.DefaultHighlightPainter highlight_painter =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(200,200,250));
    private String filename = null;
    private JRadioButton[] radioButtons = new JRadioButton[4];
    private int selectedRadioButton = 0;
    
    /** Creates new form Data.
     *  @param iter a MDAIterator object to initialize the field iterator.
     */
    public Data(MDAIterator iter) {
        iterator = iter;
        initComponents();
        radioButtons[0] = jRadioButton1;
        radioButtons[1] = jRadioButton3;
        radioButtons[2] = jRadioButton4;
        radioButtons[3] = jRadioButton2;
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        buttonGroup1 = new javax.swing.ButtonGroup();
        jLabel1 = new javax.swing.JLabel();
        jButton2 = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jRadioButton1 = new javax.swing.JRadioButton();
        jRadioButton2 = new javax.swing.JRadioButton();
        jRadioButton3 = new javax.swing.JRadioButton();
        jRadioButton4 = new javax.swing.JRadioButton();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jTextArea2 = new javax.swing.JTextArea();
        jTextArea3 = new javax.swing.JTextArea();
        jButton3 = new javax.swing.JButton();
        jButton4 = new javax.swing.JButton();
        jTextArea4 = new javax.swing.JTextArea();
        jButton5 = new javax.swing.JButton();
        jLabel6 = new javax.swing.JLabel();
        jTextArea1 = new javax.swing.JTextArea();
        jButton1 = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("Enter the data file name (including the full path) or browse.");
        jLabel1.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.ipadx = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 30, 0, 12);
        add(jLabel1, gridBagConstraints);

        jButton2.setText("Load File");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.insets = new java.awt.Insets(2, 6, 2, 12);
        add(jButton2, gridBagConstraints);

        jLabel2.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel2.setText("           ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 13;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 30, 12, 0);
        add(jLabel2, gridBagConstraints);

        jLabel3.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel3.setText("Click the \"Load File\" button to proceed.");
        jLabel3.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 30, 0, 0);
        add(jLabel3, gridBagConstraints);

        jRadioButton1.setText("Use the data file already loaded from the parent job.");
        buttonGroup1.add(jRadioButton1);
        jRadioButton1.setEnabled(false);
        jRadioButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 8, 0, 0);
        add(jRadioButton1, gridBagConstraints);

        jRadioButton2.setSelected(true);
        jRadioButton2.setText("Load in a data file from the local file system");
        buttonGroup1.add(jRadioButton2);
        jRadioButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton2ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 8, 0, 0);
        add(jRadioButton2, gridBagConstraints);

        jRadioButton3.setText("Load in a dataset from the My Datasets");
        buttonGroup1.add(jRadioButton3);
        jRadioButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton3ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 8, 0, 0);
        add(jRadioButton3, gridBagConstraints);

        jRadioButton4.setText("Load in a dataset from the Dataset Library");
        buttonGroup1.add(jRadioButton4);
        jRadioButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jRadioButton4ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 8, 0, 0);
        add(jRadioButton4, gridBagConstraints);

        jLabel4.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel4.setText("Enter file name for the dataset.  You may save the file locally.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 29, 0, 12);
        add(jLabel4, gridBagConstraints);

        jLabel5.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel5.setText("Enter file name for the dataset.  You may save the file locally.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 30, 0, 12);
        add(jLabel5, gridBagConstraints);

        jTextArea2.setMinimumSize(new java.awt.Dimension(300, 15));
        jTextArea2.setPreferredSize(new java.awt.Dimension(300, 15));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 30, 0, 0);
        add(jTextArea2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 30, 0, 0);
        add(jTextArea3, gridBagConstraints);

        jButton3.setText("Save File");
        jButton3.setMaximumSize(new java.awt.Dimension(90, 25));
        jButton3.setMinimumSize(new java.awt.Dimension(90, 25));
        jButton3.setPreferredSize(new java.awt.Dimension(90, 25));
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.insets = new java.awt.Insets(2, 6, 2, 12);
        add(jButton3, gridBagConstraints);

        jButton4.setText("Save File");
        jButton4.setMaximumSize(new java.awt.Dimension(90, 25));
        jButton4.setMinimumSize(new java.awt.Dimension(90, 25));
        jButton4.setPreferredSize(new java.awt.Dimension(90, 25));
        jButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton4ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.insets = new java.awt.Insets(2, 6, 2, 12);
        add(jButton4, gridBagConstraints);

        jTextArea4.setEditable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 30, 0, 0);
        add(jTextArea4, gridBagConstraints);

        jButton5.setText("Save File");
        jButton5.setMaximumSize(new java.awt.Dimension(90, 25));
        jButton5.setMinimumSize(new java.awt.Dimension(90, 25));
        jButton5.setPreferredSize(new java.awt.Dimension(90, 25));
        jButton5.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton5ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(2, 6, 2, 12);
        add(jButton5, gridBagConstraints);

        jLabel6.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel6.setText("This is the data file name.  You may save the file locally.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 30, 0, 12);
        add(jLabel6, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 30, 0, 0);
        add(jTextArea1, gridBagConstraints);

        jButton1.setText("Browse");
        jButton1.setMaximumSize(new java.awt.Dimension(90, 25));
        jButton1.setMinimumSize(new java.awt.Dimension(90, 25));
        jButton1.setPreferredSize(new java.awt.Dimension(90, 25));
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.insets = new java.awt.Insets(2, 6, 2, 12);
        add(jButton1, gridBagConstraints);

    }//GEN-END:initComponents

    private void jButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton4ActionPerformed
        saveFile(jTextArea3.getText(), 1);
    }//GEN-LAST:event_jButton4ActionPerformed

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        saveFile(jTextArea2.getText(), 1);
    }//GEN-LAST:event_jButton3ActionPerformed

    private void jButton5ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton5ActionPerformed
        saveFile(jTextArea4.getText(), 0);
    }//GEN-LAST:event_jButton5ActionPerformed

    private void saveFile(String filename, int index)
    {
        JFileChooser files = iterator.getFileChooser();
        files.setDialogTitle("Save File");
        files.setSelectedFile(new File(filename));
        int result = files.showSaveDialog(null);
        if(result == files.APPROVE_OPTION)
            iterator.frame.saveOperation(XMLReader.parseDataXML(iterator.getDataXML(index)),
                                         files.getSelectedFile());        
    }
    
    private void jRadioButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton4ActionPerformed
        // Initialize GUI components
        initialize(2);
        iterator.frame.reloadData(true);
        if(!iterator.getIsNewData())
        {
            iterator.setIsNewData(true);
            radioButtons[selectedRadioButton].setSelected(true);
            initialize(selectedRadioButton);
            return;
        }
        Vector data = new Vector(); 
        String dataXML = iterator.getDataXML(1);
        int nDataCol = Utility.parseDataXML(dataXML, data, iterator.getIsInd());
        ((MDAObject)wizardPane.getCustomizedObject()).setData(data);
        jButton4.setEnabled(true);
        iterator.setNDataCol(nDataCol);
        iterator.setIsDataXML(false);
        filename = iterator.getDatasetName(1);
        jTextArea3.setText(filename);
        jLabel2.setText("There are " + nDataCol + " columns in the data file.");
        selectedRadioButton = 2;
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
        jTextArea3.requestFocusInWindow();
    }//GEN-LAST:event_jRadioButton4ActionPerformed

    private void initialize(int radioButton)
    {
        jLabel6.setEnabled(radioButton == 0);
        jTextArea4.setEnabled(radioButton == 0);
        jButton5.setEnabled(false);
        jLabel4.setEnabled(radioButton == 1);
        jTextArea2.setEnabled(radioButton == 1);
        jButton3.setEnabled(false);
        jLabel5.setEnabled(radioButton == 2);
        jTextArea3.setEnabled(radioButton == 2);
        jButton4.setEnabled(false);
        jLabel1.setEnabled(radioButton == 3);
        jTextArea1.setEnabled(radioButton == 3);
        jButton1.setEnabled(radioButton == 3);
        jLabel3.setEnabled(radioButton == 3);
        jButton2.setEnabled(radioButton == 3);
    }
    
    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        JFileChooser files = iterator.getFileChooser();
        int result = files.showOpenDialog(null);
        if(result == files.APPROVE_OPTION)
	{
            File file = files.getSelectedFile(); 
            jTextArea1.setText(file.getPath());
        }        
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jRadioButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton3ActionPerformed
        // Initialize GUI components
        initialize(1);
        iterator.frame.reloadData(false);
        if(!iterator.getIsNewData())
        {
            iterator.setIsNewData(true);
            radioButtons[selectedRadioButton].setSelected(true);
            initialize(selectedRadioButton);
            if(selectedRadioButton == 0) iterator.setIsDataXML(true);
            return;
        }
        Vector data = new Vector();
        String dataXML = iterator.getDataXML(1);
        int nDataCol = Utility.parseDataXML(dataXML, data, iterator.getIsInd());
        ((MDAObject)wizardPane.getCustomizedObject()).setData(data);
        jButton3.setEnabled(true);
        iterator.setNDataCol(nDataCol);
        filename = iterator.getDatasetName(1);
        jTextArea2.setText(filename);
        jLabel2.setText("There are " + nDataCol + " columns in the data file.");
        selectedRadioButton = 1;
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
        jTextArea2.requestFocusInWindow();
    }//GEN-LAST:event_jRadioButton3ActionPerformed

    private void jRadioButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton1ActionPerformed
        // Initialize GUI components
        initialize(0);
        // Get the data object as an Vector and the number of columns in the data file
        Vector data = new Vector(); 
        int nDataCol = Utility.parseDataXML(iterator.getDataXML(0), data, iterator.getIsInd());                    
        ((MDAObject)wizardPane.getCustomizedObject()).setData(data);
        jButton5.setEnabled(true);
        iterator.setIsNewData(true);
        iterator.setNDataCol(nDataCol);
        jLabel2.setText("There are " + nDataCol + " columns in the data file.");
        selectedRadioButton = 0;
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
    }//GEN-LAST:event_jRadioButton1ActionPerformed

    private void jRadioButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton2ActionPerformed
        // Initialize GUI components
        initialize(3);
        jButton2.setEnabled(true);
        jLabel2.setText(" ");
        selectedRadioButton = 3;
        isValid = false;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
        jTextArea1.requestFocusInWindow();
    }//GEN-LAST:event_jRadioButton2ActionPerformed

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        String path = jTextArea1.getText().trim();
        Vector data = new Vector();
        int nDataCol = Utility.parseDataFile(path, data, iterator.getIsInd());
        iterator.setIsNewData(true);
        if(nDataCol <= 0)
        {
            jTextArea1.setText("");
            radioButtons[selectedRadioButton].setSelected(true);
            initialize(selectedRadioButton);
            if(selectedRadioButton == 0) iterator.setIsDataXML(true);
            JOptionPane.showMessageDialog(null, "Error in data file.",
                                          "File Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }       
        ((MDAObject)wizardPane.getCustomizedObject()).setData(data);
        iterator.setNDataCol(nDataCol);
        jLabel2.setText("There are " + nDataCol + " columns in the data file.");
        iterator.setIsDataXML(false);
        filename = path;
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
    }//GEN-LAST:event_jButton2ActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JButton jButton4;
    private javax.swing.JButton jButton5;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JRadioButton jRadioButton1;
    private javax.swing.JRadioButton jRadioButton2;
    private javax.swing.JRadioButton jRadioButton3;
    private javax.swing.JRadioButton jRadioButton4;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextArea jTextArea2;
    private javax.swing.JTextArea jTextArea3;
    private javax.swing.JTextArea jTextArea4;
    // End of variables declaration//GEN-END:variables

    /**
     * This method is to return the StepDescriptor object.
     * @return  a StepDescriptor object.
     */    
    public StepDescriptor getStepDescription(){
	return sd;
    }

    private class MyStepDescriptor extends StepDescriptor{

	public Component getComponent(){
	    return panel;
	}
       
  	public String getContentItem(){
  	    return "Data File Selection";
  	}

	public String getStepTitle(){
	    return "Data File Selection";
	}

	public void showingStep(JWizardPane wizard){
            wizardPane = wizard;
            MDAObject object = (MDAObject)wizard.getCustomizedObject();
            if(iterator.getIsReload())
            {
                // Get the DATA record ontaining the data filename
                String text = iterator.getReload().getProperty("DATA");
                if(text != null)         // The first time enter this step after reloading.
                {
                    // Remove the DATA property from reload object.
                    iterator.getReload().remove("DATA");
                    
                    if(iterator.getIsDataXML())  // Dataset is reloaded.
                    {
                        // Enable jRadioButton1 and click it.
                        jRadioButton1.setEnabled(true);
                        jRadioButton1.doClick();
                        selectedRadioButton = 0;
                        // Initialize the data filename.
                        filename = text.substring(5).trim();
                        // Show the data filename in jTextArea4.
                        jTextArea4.setText(filename);
                        // Save the filename in iterator.datasetName
                        iterator.setDatasetName(filename, 0);
                    }
                    else
                    {
                        // Disable jRadioButton1 and click jRadioButton2.
                        jRadioButton1.setEnabled(false);
                        jRadioButton2.doClick();
                        selectedRadioButton = 3;
                    }
                }
            }
            else
            {
                if(filename == null)
                {
                    jRadioButton2.doClick();
                    selectedRadioButton = 3;
                }
            }
            if(!iterator.getIsOnline())
            {
//                initialize(3);
                jRadioButton3.setEnabled(false);
                jRadioButton4.setEnabled(false);
            }
	}

	public void hidingStep(JWizardPane wizard){
            if(iterator.getIsBack())
            {
                iterator.setIsBack(false);
                return;
            }            
            String filePath = filename.trim();
            if(filePath == "")
                return;
            String[] path = filePath.replace('\\', '/').split("/");            
            String fileName = path[path.length - 1]; 
            MDAObject object = (MDAObject)wizard.getCustomizedObject();
            String record = "$DATA " + fileName.replaceAll("\r", "");
            object.getRecords().setProperty("Data", record); 
            object.getSource().data = record.substring(6);
            iterator.setIsDataXML(selectedRadioButton == 0);                
	}

	public boolean isValid(){
	    return isValid; 
	}

	public ActionListener getHelpAction(){
	    return new ActionListener(){
                public void actionPerformed(ActionEvent e){ 
                    if(!iterator.getIsOnline()) 
                        new Help("Help for $DATA record", 
                                 Data.class.getResource("/uw/rfpk/mda/nonmem/help/Data.html"));
                    else
                        Utility.openURL("https://" + iterator.getServerName() + 
                                        ":" + iterator.getServerPort() + "/user/help/Data.html");  
                }
            };
	}
        
        public String getHelpID() {
            return "Prepare_Input_Data_File_Selection";
        }
        
    }
}
