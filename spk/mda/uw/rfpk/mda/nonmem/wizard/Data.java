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
    private boolean first = true;
    private boolean isInd;
    
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
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
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
        jButton3 = new javax.swing.JButton();
        jButton4 = new javax.swing.JButton();
        jButton5 = new javax.swing.JButton();
        jLabel6 = new javax.swing.JLabel();
        jButton1 = new javax.swing.JButton();
        jTextField1 = new javax.swing.JTextField();
        jTextField2 = new javax.swing.JTextField();
        jTextField3 = new javax.swing.JTextField();
        jTextField4 = new javax.swing.JTextField();

        setLayout(new java.awt.GridBagLayout());

        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("Enter data file name (including full path) or browse.");
        jLabel1.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.ipadx = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 30, 0, 12);
        add(jLabel1, gridBagConstraints);

        jButton2.setText("Read File");
        jButton2.setMargin(new java.awt.Insets(2, 10, 2, 10));
        jButton2.setMaximumSize(new java.awt.Dimension(90, 25));
        jButton2.setMinimumSize(new java.awt.Dimension(90, 25));
        jButton2.setPreferredSize(new java.awt.Dimension(90, 25));
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
        jLabel3.setForeground(new java.awt.Color(255, 0, 0));
        jLabel3.setText("Click \"Read File\" button to load data file.");
        jLabel3.setFocusable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 30, 0, 0);
        add(jLabel3, gridBagConstraints);

        buttonGroup1.add(jRadioButton1);
        jRadioButton1.setText("Use data file previously loaded from parent job.");
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

        buttonGroup1.add(jRadioButton2);
        jRadioButton2.setSelected(true);
        jRadioButton2.setText("Load in data file from local file system");
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

        buttonGroup1.add(jRadioButton3);
        jRadioButton3.setText("Load in dataset from My Datasets");
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

        buttonGroup1.add(jRadioButton4);
        jRadioButton4.setText("Load in dataset from Dataset Library");
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
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 29, 0, 12);
        add(jLabel4, gridBagConstraints);

        jLabel5.setFont(new java.awt.Font("Dialog", 0, 12));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 30, 0, 12);
        add(jLabel5, gridBagConstraints);

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
        jLabel6.setText("Data file name:");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 30, 0, 12);
        add(jLabel6, gridBagConstraints);

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

        jTextField1.setMaximumSize(new java.awt.Dimension(300, 19));
        jTextField1.setMinimumSize(new java.awt.Dimension(300, 19));
        jTextField1.setPreferredSize(new java.awt.Dimension(300, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 30, 0, 0);
        add(jTextField1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 30, 0, 0);
        add(jTextField2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 30, 0, 0);
        add(jTextField3, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 30, 0, 0);
        add(jTextField4, gridBagConstraints);

    }// </editor-fold>//GEN-END:initComponents

    private void jButton4ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton4ActionPerformed
        saveFile(jTextField3.getText(), 1);
    }//GEN-LAST:event_jButton4ActionPerformed

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        saveFile(jTextField2.getText(), 1);
    }//GEN-LAST:event_jButton3ActionPerformed

    private void jButton5ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton5ActionPerformed
        saveFile(jTextField4.getText(), 0);
    }//GEN-LAST:event_jButton5ActionPerformed

    private void saveFile(String filename, int index)
    {
        JFileChooser files = iterator.getFileChooser();
        files.setDialogTitle("Save File");
        files.setSelectedFile(new File(filename));
        int result = files.showSaveDialog(null);
        if(result == files.APPROVE_OPTION)
            iterator.frame.saveOperation(XMLReader.parseDataXML(iterator.getDataXML(index), false),
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
        Vector<Vector<String[]>> data = new Vector<Vector<String[]>>(); 
        String dataXML = iterator.getDataXML(1);
        if(dataXML == null) return;
        String[] labels = Utility.parseDataXML(dataXML, data);
        int nDataCol = labels.length;
        ((MDAObject)wizardPane.getCustomizedObject()).setData(data);
        ((MDAObject)wizardPane.getCustomizedObject()).setDataLabels(labels);
        jButton4.setEnabled(true);
        iterator.setNDataCol(nDataCol);
        iterator.setIsDataXML(false);
        filename = iterator.getDatasetName(1);
        jTextField3.setText(filename);
        jLabel2.setText("There are " + nDataCol + " columns in data file.");
        selectedRadioButton = 2;
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
        jTextField3.requestFocusInWindow();
    }//GEN-LAST:event_jRadioButton4ActionPerformed

    private void initialize(int radioButton)
    {
        jLabel6.setEnabled(radioButton == 0);
        jTextField4.setEnabled(radioButton == 0);
        jButton5.setEnabled(false);
        jLabel4.setEnabled(radioButton == 1);
        jTextField2.setEnabled(radioButton == 1);
        jButton3.setEnabled(false);
        jLabel5.setEnabled(radioButton == 2);
        jTextField3.setEnabled(radioButton == 2);
        jButton4.setEnabled(false);
        jLabel1.setEnabled(radioButton == 3);
        jTextField1.setEnabled(radioButton == 3);
        jButton1.setEnabled(radioButton == 3);
        jLabel3.setEnabled(radioButton == 3);
        jButton2.setEnabled(radioButton == 3);
    }
    
    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        JFileChooser files = iterator.getFileChooser();
        files.setDialogTitle("Open File");
        int result = files.showOpenDialog(null);
        if(result == files.APPROVE_OPTION)
	{
            File file = files.getSelectedFile(); 
            jTextField1.setText(file.getPath());
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
        Vector<Vector<String[]>> data = new Vector<Vector<String[]>>();
        String dataXML = iterator.getDataXML(1);
        if(dataXML == null) return;
        String[] labels = Utility.parseDataXML(dataXML, data);
        int nDataCol = labels.length;
        ((MDAObject)wizardPane.getCustomizedObject()).setData(data);
        ((MDAObject)wizardPane.getCustomizedObject()).setDataLabels(labels);
        jButton3.setEnabled(true);
        iterator.setNDataCol(nDataCol);
        filename = iterator.getDatasetName(1);
        jTextField2.setText(filename);
        jLabel2.setText("There are " + nDataCol + " columns in the data file.");
        selectedRadioButton = 1;
        isValid = true;
        wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
        jTextField2.requestFocusInWindow();
    }//GEN-LAST:event_jRadioButton3ActionPerformed

    private void jRadioButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jRadioButton1ActionPerformed
        // Initialize GUI components
        initialize(0);
        // Get the data object as an Vector and the number of columns in the data file
        Vector<Vector<String[]>> data = new Vector<Vector<String[]>>();
        String dataXML = iterator.getDataXML(0);
        if(dataXML == null) return;
        String[] labels = Utility.parseDataXML(dataXML, data);
        int nDataCol = labels.length;
        ((MDAObject)wizardPane.getCustomizedObject()).setData(data);
        ((MDAObject)wizardPane.getCustomizedObject()).setDataLabels(labels);
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
        jTextField1.requestFocusInWindow();
    }//GEN-LAST:event_jRadioButton2ActionPerformed

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        String path = jTextField1.getText().trim();
        Vector<Vector<String[]>> data = new Vector<Vector<String[]>>();
        String[] labels = Utility.parseDataFile(path, data, isInd);
        if(labels == null) return;
        int nDataCol = labels.length;
        iterator.setIsNewData(true);
        if(nDataCol <= 0)
        {
            jTextField1.setText("");
            radioButtons[selectedRadioButton].setSelected(true);
            initialize(selectedRadioButton);
            if(selectedRadioButton == 0) iterator.setIsDataXML(true);
            JOptionPane.showMessageDialog(null, "Error in data file.",
                                          "File Error",
                                          JOptionPane.ERROR_MESSAGE);
            return;
        }       
        ((MDAObject)wizardPane.getCustomizedObject()).setData(data);
        ((MDAObject)wizardPane.getCustomizedObject()).setDataLabels(labels);
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
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextField jTextField4;
    // End of variables declaration//GEN-END:variables

    /**
     * This method is to return the StepDescriptor object.
     * @return  a StepDescriptor object.
     */    
    public StepDescriptor getStepDescription(){
	return sd;
    }

    private class MyStepDescriptor extends StepDescriptor{

        private String fileName;
        
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
            if(iterator.getIsBack())
            {
                iterator.setIsBack(false);
                return;
            }
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
                        jTextField4.setText(filename);
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
            if(first)
            {
                first = false;
                isInd = iterator.analysis.equals("individual") || iterator.analysis.equals("identifiability");
            }
            else
            {
                if(isInd != iterator.analysis.equals("individual") || iterator.analysis.equals("identifiability"))
                {
                    isInd = iterator.analysis.equals("individual") || iterator.analysis.equals("identifiability");
                    jTextField1.setText("");
                    jTextField2.setText("");
                    jTextField3.setText("");
                    jTextField4.setText("");
                    jRadioButton1.setEnabled(false);
                    jRadioButton2.doClick();
                    selectedRadioButton = 3;
                    isValid = false;
                    wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());                
                    jLabel2.setText("You need to load in a dataset since you changed the analysis type.");
                }                
            }
            
            // Check dataset type for population-level
            if(!isInd && ((MDAObject)wizard.getCustomizedObject()).getDataLabels() != null &&
               !((MDAObject)wizard.getCustomizedObject()).getDataLabels()[0].equals("ID"))
            {
                isValid = false;
                wizardPane.setLeftOptions(wizardPane.getUpdatedLeftOptions().toArray());
                jLabel2.setText("You need to load in a population dataset for this job.");
            }
	}

        public boolean checkingStep(JWizardPane wizard){
            String filePath = filename.trim();
            if(filePath == "")
            {
                JOptionPane.showMessageDialog(null, "Dataset name was missing.", "Input Error", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            String[] path = filePath.replace('\\', '/').split("/");            
            fileName = path[path.length - 1];
            if(iterator.analysis.equals("individual") || iterator.analysis.equals("identifiability"))
            {
                int nInd = ((MDAObject)wizard.getCustomizedObject()).getData().size();            
                if(nInd > 1)
                {
                    String[] possibleValues = new String[nInd];
                    java.util.ArrayList<String> idList = new java.util.ArrayList<String>(nInd);
                    Vector<Vector<String[]>> popData = ((MDAObject)wizard.getCustomizedObject()).getData();
                    int i = 0;
                    for(Object indData : popData)
                    {
                        possibleValues[i] = ((String[])((Vector)indData).get(0))[0];
                        idList.add(possibleValues[i]);
                        i++;
                    }
                    String id = (String)JOptionPane.showInputDialog(null, "Population dataset cannot be used in individual analysis.\nPlease choose an individual from the dataset",
                                                                    "Selecting Dataset", JOptionPane.INFORMATION_MESSAGE, null,
                                                                    possibleValues, 
                                                                    possibleValues[0]);
                    if(id != null)
                    {
                        Vector<String[]> selectedData = popData.get(idList.indexOf(id));
                        popData.removeAllElements();
                        popData.add(selectedData);
                    }
                    else
                    {
                        JOptionPane.showMessageDialog(null, "Please load another dataset.");
                        return false;
                    }
                }
            }
            return true;
        }
        
	public void hidingStep(JWizardPane wizard){
            if(iterator.getIsBack()) return;
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
                }
            };
	}
        
        public String getHelpID() {
            return "Prepare_Input_Data_File_Selection";
        }
        
    }
}
