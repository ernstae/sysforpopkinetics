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
package uw.rfpk.mda.nonmem;

import uw.rfpk.mda.nonmem.Utility;
import uw.rfpk.mda.nonmem.display.Plotter;
import java.awt.Component;
import java.awt.Color;
import java.util.Vector;
import java.util.Properties;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import java.text.DecimalFormat;
import java.util.StringTokenizer;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.DefaultHighlighter.DefaultHighlightPainter;
import javax.swing.text.BadLocationException;
import javax.swing.JLabel;
import javax.swing.ListCellRenderer;
import javax.swing.DefaultListModel;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.*;
import java.awt.Polygon;
import java.awt.BasicStroke;
import java.text.DecimalFormat;

/** This class's instance reads data names and data values, displays a dialog to
 * collect user's selections for the plot and calls a plotter to plot the data.
 *
 * @author  Jiaji Du
 */
public class PlotTool extends JFrame {
    
    /** Creates new form PlotTool.
     * @param text a String containing the data to be plotted as a text in columns.
     */
    public PlotTool(String text)
    {
        this.text = text;
        for(int i = 0; i < intArray1.length; i++)
            intArray1[i] = new Integer(i);
        for(int i = 0; i < intArray2.length; i++)
            intArray2[i] = new Integer(i);        
        initComponents();
        ComboBoxRenderer renderer = new ComboBoxRenderer("color");
        c1ComboBox.setRenderer(renderer);
        c2ComboBox.setRenderer(renderer);
        c3ComboBox.setRenderer(renderer);
        xlComboBox.setRenderer(renderer);
        ylComboBox.setRenderer(renderer);
        ulComboBox.setRenderer(renderer);
        renderer = new ComboBoxRenderer("symbol");
        s1ComboBox.setRenderer(renderer);
        s2ComboBox.setRenderer(renderer);
        s3ComboBox.setRenderer(renderer);        
        
        // Put the text in an array of lines
        String[] lines = text.split("\n");
        
        // Read the first line of the text
        StringTokenizer tokenizer = new StringTokenizer(lines[0].trim(), " ", false);
        int nTokens = tokenizer.countTokens();
        if(nTokens < 2)
        {
            JOptionPane.showMessageDialog(null, "Data are not availible.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return; 
        }
        
        // Initialize the combo boxes
        y1ComboBox.removeAllItems();
        y2ComboBox.removeAllItems();
        y3ComboBox.removeAllItems();
        xComboBox.removeAllItems();
        y1ComboBox.addItem("none");
        y2ComboBox.addItem("none");
        y3ComboBox.addItem("none");
        xComboBox.addItem("none");
        String token = null;
        for(int i = 0; i < nTokens; i++)
        {
            token = tokenizer.nextToken();
            y1ComboBox.addItem(token);
            y2ComboBox.addItem(token);
            y3ComboBox.addItem(token);
            xComboBox.addItem(token);            
        }
        
        // Read the data into a double array.
        dataAll = new double[nTokens][lines.length - 1];
        for(int i = 1; i < lines.length; i++)
        {
            tokenizer = new StringTokenizer(lines[i].trim(), " ", false);
            if(tokenizer.countTokens() != nTokens)
            {
                JOptionPane.showMessageDialog(null, "Data are not availible or erroneous.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);                
                return;   
            }
            for(int j = 0; j < nTokens; j++)
                dataAll[j][i - 1] = Double.parseDouble(tokenizer.nextToken());
        }
                
        // Initialize the text field
        jTextField1.setText("");
        
        // Initialize the check boxes
        jCheckBox1.setSelected(false);
        jCheckBox2.setSelected(false);
        jCheckBox3.setSelected(false);
        jCheckBox4.setSelected(false);
        
        // Display the window
        OKButton.setEnabled(false);
        advancedButton.setEnabled(false);
        setVisible(true);
    }

    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        java.awt.GridBagConstraints gridBagConstraints;

        curveListDialog = new javax.swing.JDialog();
        jScrollPane1 = new javax.swing.JScrollPane();
        curveList = new javax.swing.JList(model);
        jTextField2 = new javax.swing.JTextField();
        displayButton = new javax.swing.JButton();
        advancedDialog = new javax.swing.JDialog();
        jPanel3 = new javax.swing.JPanel();
        ipRadioButton = new javax.swing.JRadioButton();
        otRadioButton = new javax.swing.JRadioButton();
        oRRadioButton = new javax.swing.JRadioButton();
        jPanel5 = new javax.swing.JPanel();
        hgCheckBox = new javax.swing.JCheckBox();
        vgCheckBox = new javax.swing.JCheckBox();
        jPanel2 = new javax.swing.JPanel();
        txCheckBox = new javax.swing.JCheckBox();
        tyCheckBox = new javax.swing.JCheckBox();
        jPanel7 = new javax.swing.JPanel();
        jLabel9 = new javax.swing.JLabel();
        wTextField = new javax.swing.JTextField();
        jLabel10 = new javax.swing.JLabel();
        hTextField = new javax.swing.JTextField();
        jPanel4 = new javax.swing.JPanel();
        jLabel13 = new javax.swing.JLabel();
        minYTextField = new javax.swing.JTextField();
        jLabel14 = new javax.swing.JLabel();
        maxYTextField = new javax.swing.JTextField();
        jLabel11 = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        minXTextField = new javax.swing.JTextField();
        maxXTextField = new javax.swing.JTextField();
        jPanel6 = new javax.swing.JPanel();
        applyButton = new javax.swing.JButton();
        resetButton = new javax.swing.JButton();
        cancelAdvancedButton = new javax.swing.JButton();
        buttonGroup1 = new javax.swing.ButtonGroup();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        y3ComboBox = new javax.swing.JComboBox();
        xComboBox = new javax.swing.JComboBox();
        jLabel3 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jPanel1 = new javax.swing.JPanel();
        OKButton = new javax.swing.JButton();
        advancedButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        jCheckBox1 = new javax.swing.JCheckBox();
        jCheckBox2 = new javax.swing.JCheckBox();
        jCheckBox3 = new javax.swing.JCheckBox();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        y1ComboBox = new javax.swing.JComboBox();
        y2ComboBox = new javax.swing.JComboBox();
        jLabel6 = new javax.swing.JLabel();
        s1ComboBox = new javax.swing.JComboBox(intArray2);
        s2ComboBox = new javax.swing.JComboBox(intArray2);
        s3ComboBox = new javax.swing.JComboBox(intArray2);
        jLabel19 = new javax.swing.JLabel();
        jLabel20 = new javax.swing.JLabel();
        c1ComboBox = new javax.swing.JComboBox(intArray1);
        c2ComboBox = new javax.swing.JComboBox(intArray1);
        c3ComboBox = new javax.swing.JComboBox(intArray1);
        jCheckBox4 = new javax.swing.JCheckBox();
        xlComboBox = new javax.swing.JComboBox(intArray1);
        ylComboBox = new javax.swing.JComboBox(intArray1);
        ulComboBox = new javax.swing.JComboBox(intArray1);
        jLabel7 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jTextField3 = new javax.swing.JTextField();
        jTextField4 = new javax.swing.JTextField();

        curveListDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        curveListDialog.setTitle("Plot List");
        curveListDialog.setLocationRelativeTo(this);
        jScrollPane1.setViewportView(curveList);

        curveListDialog.getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jTextField2.setEditable(false);
        jTextField2.setText("Please select.");
        jTextField2.setFocusable(false);
        curveListDialog.getContentPane().add(jTextField2, java.awt.BorderLayout.NORTH);

        displayButton.setText("Display");
        displayButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                displayButtonActionPerformed(evt);
            }
        });

        curveListDialog.getContentPane().add(displayButton, java.awt.BorderLayout.SOUTH);

        advancedDialog.getContentPane().setLayout(new java.awt.GridBagLayout());

        advancedDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        advancedDialog.setTitle("Advanced Settings");
        advancedDialog.setLocationRelativeTo(this);
        advancedDialog.setModal(true);
        advancedDialog.setResizable(false);
        jPanel3.setBorder(new javax.swing.border.TitledBorder("Legend Position"));
        ipRadioButton.setSelected(true);
        ipRadioButton.setText("Inside Plot");
        buttonGroup1.add(ipRadioButton);
        jPanel3.add(ipRadioButton);

        otRadioButton.setText("On Top");
        buttonGroup1.add(otRadioButton);
        jPanel3.add(otRadioButton);

        oRRadioButton.setText("On Right");
        buttonGroup1.add(oRRadioButton);
        jPanel3.add(oRRadioButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        advancedDialog.getContentPane().add(jPanel3, gridBagConstraints);

        jPanel5.setBorder(new javax.swing.border.TitledBorder("Grid Lines"));
        hgCheckBox.setSelected(true);
        hgCheckBox.setText(" Horizontal Lines");
        jPanel5.add(hgCheckBox);

        vgCheckBox.setSelected(true);
        vgCheckBox.setText("Vertical Lines");
        jPanel5.add(vgCheckBox);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        advancedDialog.getContentPane().add(jPanel5, gridBagConstraints);

        jPanel2.setBorder(new javax.swing.border.TitledBorder("ticks"));
        txCheckBox.setSelected(true);
        txCheckBox.setText("Along X Axis");
        jPanel2.add(txCheckBox);

        tyCheckBox.setSelected(true);
        tyCheckBox.setText("Along Y Axis");
        jPanel2.add(tyCheckBox);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        advancedDialog.getContentPane().add(jPanel2, gridBagConstraints);

        jPanel7.setBorder(new javax.swing.border.TitledBorder("Window Size"));
        jLabel9.setText("Width");
        jPanel7.add(jLabel9);

        wTextField.setText("500");
        wTextField.setPreferredSize(new java.awt.Dimension(75, 19));
        jPanel7.add(wTextField);

        jLabel10.setText("Height");
        jPanel7.add(jLabel10);

        hTextField.setText("400");
        hTextField.setPreferredSize(new java.awt.Dimension(75, 19));
        jPanel7.add(hTextField);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        advancedDialog.getContentPane().add(jPanel7, gridBagConstraints);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        jPanel4.setBorder(new javax.swing.border.TitledBorder("Plotting Range"));
        jLabel13.setText("Min. Y");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 10, 0, 5);
        jPanel4.add(jLabel13, gridBagConstraints);

        minYTextField.setPreferredSize(new java.awt.Dimension(75, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 10);
        jPanel4.add(minYTextField, gridBagConstraints);

        jLabel14.setText("Max. Y");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 5);
        jPanel4.add(jLabel14, gridBagConstraints);

        maxYTextField.setPreferredSize(new java.awt.Dimension(75, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 10);
        jPanel4.add(maxYTextField, gridBagConstraints);

        jLabel11.setText("Min. X");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(0, 10, 0, 5);
        jPanel4.add(jLabel11, gridBagConstraints);

        jLabel12.setText("Max. X");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 5);
        jPanel4.add(jLabel12, gridBagConstraints);

        minXTextField.setPreferredSize(new java.awt.Dimension(75, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 10);
        jPanel4.add(minXTextField, gridBagConstraints);

        maxXTextField.setPreferredSize(new java.awt.Dimension(75, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 10);
        jPanel4.add(maxXTextField, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        advancedDialog.getContentPane().add(jPanel4, gridBagConstraints);

        applyButton.setText("Apply");
        applyButton.setMaximumSize(new java.awt.Dimension(80, 25));
        applyButton.setMinimumSize(new java.awt.Dimension(80, 25));
        applyButton.setPreferredSize(new java.awt.Dimension(80, 25));
        applyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                applyButtonActionPerformed(evt);
            }
        });

        jPanel6.add(applyButton);

        resetButton.setText("Reset");
        resetButton.setMaximumSize(new java.awt.Dimension(80, 25));
        resetButton.setMinimumSize(new java.awt.Dimension(80, 25));
        resetButton.setPreferredSize(new java.awt.Dimension(80, 25));
        resetButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                resetButtonActionPerformed(evt);
            }
        });

        jPanel6.add(resetButton);

        cancelAdvancedButton.setText("Cancel");
        cancelAdvancedButton.setMaximumSize(new java.awt.Dimension(80, 25));
        cancelAdvancedButton.setMinimumSize(new java.awt.Dimension(80, 25));
        cancelAdvancedButton.setPreferredSize(new java.awt.Dimension(80, 25));
        cancelAdvancedButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelAdvancedButtonActionPerformed(evt);
            }
        });

        jPanel6.add(cancelAdvancedButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        advancedDialog.getContentPane().add(jPanel6, gridBagConstraints);

        getContentPane().setLayout(new java.awt.GridBagLayout());

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Plot Settings");
        setLocationRelativeTo(this);
        setResizable(false);
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        jLabel1.setLabelFor(y3ComboBox);
        jLabel1.setText("Y axis 3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 0);
        getContentPane().add(jLabel1, gridBagConstraints);

        jLabel2.setLabelFor(xComboBox);
        jLabel2.setText("X axis");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 11, 0, 0);
        getContentPane().add(jLabel2, gridBagConstraints);

        y3ComboBox.setMaximumSize(new java.awt.Dimension(120, 20));
        y3ComboBox.setMinimumSize(new java.awt.Dimension(120, 20));
        y3ComboBox.setPreferredSize(new java.awt.Dimension(120, 20));
        y3ComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                y3ComboBoxActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 12);
        getContentPane().add(y3ComboBox, gridBagConstraints);

        xComboBox.setMaximumSize(new java.awt.Dimension(120, 20));
        xComboBox.setMinimumSize(new java.awt.Dimension(120, 20));
        xComboBox.setPreferredSize(new java.awt.Dimension(120, 20));
        xComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                xComboBoxActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.insets = new java.awt.Insets(3, 0, 1, 12);
        getContentPane().add(xComboBox, gridBagConstraints);

        jLabel3.setLabelFor(jTextField1);
        jLabel3.setText("Plot Title");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 0, 0);
        getContentPane().add(jLabel3, gridBagConstraints);

        jTextField1.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextField1KeyTyped(evt);
            }
        });
        jTextField1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jTextField1MouseClicked(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 0, 12);
        getContentPane().add(jTextField1, gridBagConstraints);

        OKButton.setText("OK");
        OKButton.setMaximumSize(new java.awt.Dimension(94, 25));
        OKButton.setMinimumSize(new java.awt.Dimension(94, 25));
        OKButton.setPreferredSize(new java.awt.Dimension(94, 25));
        OKButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                OKButtonActionPerformed(evt);
            }
        });

        jPanel1.add(OKButton);

        advancedButton.setText("Advanced");
        advancedButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                advancedButtonActionPerformed(evt);
            }
        });

        jPanel1.add(advancedButton);

        cancelButton.setText("Cancel");
        cancelButton.setMaximumSize(new java.awt.Dimension(94, 25));
        cancelButton.setMinimumSize(new java.awt.Dimension(94, 25));
        cancelButton.setPreferredSize(new java.awt.Dimension(94, 25));
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });

        jPanel1.add(cancelButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.insets = new java.awt.Insets(10, 12, 10, 12);
        getContentPane().add(jPanel1, gridBagConstraints);

        jCheckBox1.setText("Add a vertical line, X=0, to the plot");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jCheckBox1, gridBagConstraints);

        jCheckBox2.setText("Add a horizontal line, Y=0, to the plot");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jCheckBox2, gridBagConstraints);

        jCheckBox3.setText("Add a unit slope line, X=Y, to the plot");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jCheckBox3, gridBagConstraints);

        jLabel4.setText("Y axis 1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 12, 0, 0);
        getContentPane().add(jLabel4, gridBagConstraints);

        jLabel5.setText("Y axis 2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 0);
        getContentPane().add(jLabel5, gridBagConstraints);

        y1ComboBox.setMaximumSize(new java.awt.Dimension(120, 20));
        y1ComboBox.setMinimumSize(new java.awt.Dimension(120, 20));
        y1ComboBox.setPreferredSize(new java.awt.Dimension(120, 20));
        y1ComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                y1ComboBoxActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(5, 0, 1, 11);
        getContentPane().add(y1ComboBox, gridBagConstraints);

        y2ComboBox.setMaximumSize(new java.awt.Dimension(120, 20));
        y2ComboBox.setMinimumSize(new java.awt.Dimension(120, 20));
        y2ComboBox.setPreferredSize(new java.awt.Dimension(120, 20));
        y2ComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                y2ComboBoxActionPerformed(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 12);
        getContentPane().add(y2ComboBox, gridBagConstraints);

        jLabel6.setText("Select data columns");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 0, 0);
        getContentPane().add(jLabel6, gridBagConstraints);

        s1ComboBox.setMaximumRowCount(13);
        s1ComboBox.setPreferredSize(new java.awt.Dimension(80, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(5, 0, 1, 6);
        getContentPane().add(s1ComboBox, gridBagConstraints);

        s2ComboBox.setMaximumRowCount(13);
        s2ComboBox.setPreferredSize(new java.awt.Dimension(80, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 6);
        getContentPane().add(s2ComboBox, gridBagConstraints);

        s3ComboBox.setMaximumRowCount(13);
        s3ComboBox.setPreferredSize(new java.awt.Dimension(80, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 6);
        getContentPane().add(s3ComboBox, gridBagConstraints);

        jLabel19.setText("Symbol");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 0, 0);
        getContentPane().add(jLabel19, gridBagConstraints);

        jLabel20.setText("Color");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(12, 2, 0, 12);
        getContentPane().add(jLabel20, gridBagConstraints);

        c1ComboBox.setMaximumRowCount(10);
        c1ComboBox.setPreferredSize(new java.awt.Dimension(60, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(5, 0, 1, 12);
        getContentPane().add(c1ComboBox, gridBagConstraints);

        c2ComboBox.setMaximumRowCount(10);
        c2ComboBox.setPreferredSize(new java.awt.Dimension(60, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 12);
        getContentPane().add(c2ComboBox, gridBagConstraints);

        c3ComboBox.setMaximumRowCount(10);
        c3ComboBox.setPreferredSize(new java.awt.Dimension(60, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 12);
        getContentPane().add(c3ComboBox, gridBagConstraints);

        jCheckBox4.setText("Split the data by ID");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 1, 12);
        getContentPane().add(jCheckBox4, gridBagConstraints);

        xlComboBox.setMaximumRowCount(10);
        xlComboBox.setPreferredSize(new java.awt.Dimension(60, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 12);
        getContentPane().add(xlComboBox, gridBagConstraints);

        ylComboBox.setMaximumRowCount(10);
        ylComboBox.setPreferredSize(new java.awt.Dimension(60, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 12);
        getContentPane().add(ylComboBox, gridBagConstraints);

        ulComboBox.setMaximumRowCount(10);
        ulComboBox.setPreferredSize(new java.awt.Dimension(60, 20));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 12);
        getContentPane().add(ulComboBox, gridBagConstraints);

        jLabel7.setText("X Lable");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 12, 2, 0);
        getContentPane().add(jLabel7, gridBagConstraints);

        jLabel8.setText("Y Label");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 0);
        getContentPane().add(jLabel8, gridBagConstraints);

        jTextField3.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextField3KeyTyped(evt);
            }
        });
        jTextField3.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jTextField3MouseClicked(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(3, 12, 3, 12);
        getContentPane().add(jTextField3, gridBagConstraints);

        jTextField4.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyTyped(java.awt.event.KeyEvent evt) {
                jTextField4KeyTyped(evt);
            }
        });
        jTextField4.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jTextField4MouseClicked(evt);
            }
        });

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 0, 12);
        getContentPane().add(jTextField4, gridBagConstraints);

        pack();
    }//GEN-END:initComponents

    private void cancelAdvancedButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelAdvancedButtonActionPerformed
        advancedDialog.dispose();
    }//GEN-LAST:event_cancelAdvancedButtonActionPerformed

    private void resetButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_resetButtonActionPerformed
        ipRadioButton.setSelected(true);
        hgCheckBox.setSelected(true);
        vgCheckBox.setSelected(true);
        txCheckBox.setSelected(true);
        tyCheckBox.setSelected(true);
        wTextField.setText("500");
        hTextField.setText("400");
        DecimalFormat f = new DecimalFormat("0.00E00");
        minXTextField.setText(String.valueOf(Utility.formatData(6, f.format(minX))));
        maxXTextField.setText(String.valueOf(Utility.formatData(6, f.format(maxX))));        
        minYTextField.setText(String.valueOf(Utility.formatData(6, f.format(minY))));
        maxYTextField.setText(String.valueOf(Utility.formatData(6, f.format(maxY))));
    }//GEN-LAST:event_resetButtonActionPerformed

    private void applyButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_applyButtonActionPerformed
        int tempWidth, tempHeight;
        double tempMinX, tempMaxX, tempMinY, tempMaxY;
        try
        {
            tempWidth = Integer.parseInt(wTextField.getText());
            tempHeight = Integer.parseInt(hTextField.getText());
            tempMinX = Double.parseDouble(minXTextField.getText());
            tempMaxX = Double.parseDouble(maxXTextField.getText());           
            tempMinY = Double.parseDouble(minYTextField.getText());
            tempMaxY = Double.parseDouble(maxYTextField.getText());
            if(tempWidth < 500 )
            {
                JOptionPane.showMessageDialog(null, "The width must be >= than 500.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return;
            }
            if(tempHeight < 400)
            {
                JOptionPane.showMessageDialog(null, "The height must be >= than 400.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return;
            }         
            if(tempMinY > tempMaxY)
            {
                JOptionPane.showMessageDialog(null, "The entered min. data value > max. data value.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return;
            }      
        }
        catch(NumberFormatException e)
        {
            JOptionPane.showMessageDialog(null, "The entered data range has format error.",
                                          "Input Error", JOptionPane.ERROR_MESSAGE);
            return;
        }
        if(ipRadioButton.isSelected())
            legendLocation = "Inside";
        else if(otRadioButton.isSelected())
            legendLocation = "Top";
        else
            legendLocation = "Right";
       
        showHorizontalGrid = hgCheckBox.isSelected();
        showVerticalGrid = vgCheckBox.isSelected();
        showTicksOnX = txCheckBox.isSelected();
        showTicksOnY = tyCheckBox.isSelected();
        width = tempWidth;
        height = tempHeight;
        selectedMinX = tempMinX;
        selectedMaxX = tempMaxX;        
        selectedMinY = tempMinY;
        selectedMaxY = tempMaxY;
        
        advancedDialog.dispose();
    }//GEN-LAST:event_applyButtonActionPerformed

    private void advancedButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_advancedButtonActionPerformed
        // determine the data range
        if(first)
        {
            int nCurve = selection.size();   
            double[][] dataX = new double[nCurve][];
            double[][] dataY = new double[nCurve][];
            for(int i = 0; i < nCurve; i++)
            {
                dataX[i] = dataAll[xComboBox.getSelectedIndex() - 1];
                dataY[i] = dataAll[((Integer)selection.get(i)).intValue() - 1];
            }

            double[] range = new double[2];
            range = Plotter.getDefaultRange(dataX);
            minX = range[0];
            maxX = range[1];
            range = Plotter.getDefaultRange(dataY);
            minY = range[0];
            maxY = range[1];
            range = null;            
        
            // Set default values to Advanced Settings dialog
            DecimalFormat f = new DecimalFormat("0.00E00");
            minXTextField.setText(String.valueOf(Utility.formatData(6, f.format(minX))));
            maxXTextField.setText(String.valueOf(Utility.formatData(6, f.format(maxX))));
            minYTextField.setText(String.valueOf(Utility.formatData(6, f.format(minY))));
            maxYTextField.setText(String.valueOf(Utility.formatData(6, f.format(maxY))));
            first = false;
        }
        
        advancedDialog.setSize(300, 370);
        advancedDialog.show();
    }//GEN-LAST:event_advancedButtonActionPerformed

    private void jTextField4MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTextField4MouseClicked
        highlighter3.removeAllHighlights();
        isHighlight3 = false;
    }//GEN-LAST:event_jTextField4MouseClicked

    private void jTextField3MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTextField3MouseClicked
        highlighter2.removeAllHighlights();
        isHighlight2 = false;
    }//GEN-LAST:event_jTextField3MouseClicked

    private void jTextField4KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextField4KeyTyped
        if(isHighlight3)
        {
            jTextField4.setText("");
            highlighter3.removeAllHighlights();
            isHighlight3 = false;
        }
    }//GEN-LAST:event_jTextField4KeyTyped

    private void jTextField3KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextField3KeyTyped
        if(isHighlight2)
        {
            jTextField3.setText("");
            highlighter2.removeAllHighlights();
            isHighlight2 = false;
        }
    }//GEN-LAST:event_jTextField3KeyTyped

    private void displayButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_displayButtonActionPerformed
        int[] selectedIndex = curveList.getSelectedIndices();
        if(selectedIndex.length == 1 && selectedIndex[0] < 0)
            return;
        
        // Get data and other information
        Object[] elements = curveList.getSelectedValues();
        int nCurve = selection.size();
        double[][] dataX, dataY;        
        for(int j = 0; j < elements.length; j++)
        {
            int startRow = ((Integer)startIndex.get(selectedIndex[j])).intValue();
            int endRow = ((Integer)startIndex.get(selectedIndex[j] + 1)).intValue();
            int nRow = endRow - startRow;
            dataX = new double[nCurve][nRow];   
            dataY = new double[nCurve][nRow];
            for(int i = 0; i < nCurve; i++)
            {
                Vector tempX = new Vector();
                Vector tempY = new Vector();
                for(int k = 0; k < nRow; k++)
                {
                    int l = startRow + k;
                    dataY[i][k] = dataAll[((Integer)selection.get(i)).intValue() - 1][l];    
                    dataX[i][k] = dataAll[xComboBox.getSelectedIndex() - 1][l];
                }
            }
            if(selectedMinX != minX || selectedMaxX != maxX || selectedMinY != minY || selectedMaxY != maxY)
            {
                if(plottingRange(dataX, dataY) == 0)
                {
                    JOptionPane.showMessageDialog(null, "No data were found in the specified range." +
                                                  "\n" + (String)elements[j]);                    
                    continue;   
                }
            }
 
            int l = j - j / 10 * 10;
            plot(dataX, dataY, (String)elements[j], l * 50, l * 40);            
        }
    }//GEN-LAST:event_displayButtonActionPerformed

    private void y2ComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_y2ComboBoxActionPerformed
        setTitle();
    }//GEN-LAST:event_y2ComboBoxActionPerformed

    private void y1ComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_y1ComboBoxActionPerformed
        setTitle();
    }//GEN-LAST:event_y1ComboBoxActionPerformed

    private void jTextField1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTextField1MouseClicked
        highlighter1.removeAllHighlights();
        isHighlight1 = false;
    }//GEN-LAST:event_jTextField1MouseClicked

    private void jTextField1KeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jTextField1KeyTyped
        if(isHighlight1)
        {
            jTextField1.setText("");
            highlighter1.removeAllHighlights();
            isHighlight1 = false;
        }
    }//GEN-LAST:event_jTextField1KeyTyped

    private void y3ComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_y3ComboBoxActionPerformed
        setTitle();
    }//GEN-LAST:event_y3ComboBoxActionPerformed

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        advancedDialog.dispose();
        dispose();
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void xComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_xComboBoxActionPerformed
        setTitle();
    }//GEN-LAST:event_xComboBoxActionPerformed

    private void OKButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_OKButtonActionPerformed
        // Put data in double arrays, only one curve in the plot
        if(selection == null) 
            return;
        if(jCheckBox4.isSelected())
        {
            if(!y1ComboBox.getItemAt(1).equals("ID"))
            {
                JOptionPane.showMessageDialog(null, "ID was not found.", "Input Error", 
                                              JOptionPane.ERROR_MESSAGE);   
                return;
            }
            else
            {
                startIndex = new Vector();
                model.removeAllElements();
                int id = (int)dataAll[0][0];
                model.addElement("ID = " + id);                
                startIndex.addElement(new Integer(0));
                for(int i = 1; i < dataAll[0].length; i++)
                {
                    if(id != (int)dataAll[0][i])
                    {
                        id = (int)dataAll[0][i];
                        startIndex.addElement(new Integer(i));
                        model.addElement("ID = " + id);
                    }
                }
                startIndex.addElement(new Integer(dataAll[0].length));
                curveListDialog.setSize(150, 250);
                curveListDialog.setVisible(true);
            }
        }
        else
        {
            int nCurve = selection.size();
            double[][] dataX = new double[nCurve][];   
            double[][] dataY = new double[nCurve][];
            for(int i = 0; i < nCurve; i++)
            {
                dataY[i] = dataAll[((Integer)selection.get(i)).intValue() - 1];
                dataX[i] = dataAll[xComboBox.getSelectedIndex() - 1];
            }
            if(selectedMinX != minX || selectedMaxX != maxX || selectedMinY != minY || selectedMaxY != maxY)
                if(plottingRange(dataX, dataY) == 0)
                {
                    JOptionPane.showMessageDialog(null, "No data were found in the specified range.");
                    return;
                }
            plot(dataX, dataY, null, 0, 0);
        }
    }
    
    // Select data in user specified plotting range.
    private int plottingRange(double[][] dataX, double[][] dataY)
    {
        for(int i = 0; i < dataX.length; i++)
        {
            Vector tempX = new Vector();
            Vector tempY = new Vector();
            for(int j = 0; j < dataY[i].length; j++)
            {
                if(dataX[i][j] >= minX && dataX[i][j] <= maxX &&
                   dataY[i][j] >= minY && dataY[i][j] <= maxY)
                {
                    tempY.add(new Double(dataY[i][j]));
                    tempX.add(new Double(dataX[i][j]));
                }
            }
            int size = tempX.size();
            if(size == 0)
                return 0;
            dataX[i] = new double[size];
            dataY[i] = new double[size];
            for(int j = 0; j <size ; j++)
            {
                dataX[i][j] = ((Double)tempX.get(j)).doubleValue();
                dataY[i][j] = ((Double)tempY.get(j)).doubleValue();
            }
        }
        return 1;
    }
    
    private void plot(double[][] dataX, double[][] dataY, String idTitle, int x, int y)
    {
        int nCurve = selection.size();
        String[] name = new String[nCurve];
        int[] symbol = new int[nCurve];
        Color[] color = new Color[nCurve];
        for(int i = 0; i < nCurve; i++)
        {
            if(((String)curveName.get(i)).equals("y1"))
            {
                name[i] = (String)y1ComboBox.getSelectedItem();
                symbol[i] = s1ComboBox.getSelectedIndex();
                color[i] = colorList[c1ComboBox.getSelectedIndex()];
            }
            if(((String)curveName.get(i)).equals("y2"))
            {
                name[i] = (String)y2ComboBox.getSelectedItem();                
                symbol[i] = s2ComboBox.getSelectedIndex();
                color[i] = colorList[c2ComboBox.getSelectedIndex()];
            }
            if(((String)curveName.get(i)).equals("y3"))
            {
                name[i] = (String)y3ComboBox.getSelectedItem();                
                symbol[i] = s3ComboBox.getSelectedIndex();
                color[i] = colorList[c3ComboBox.getSelectedIndex()];
            }
        }
                             
        Color[] addedLineColor = new Color[]{colorList[xlComboBox.getSelectedIndex()],
                                             colorList[ylComboBox.getSelectedIndex()],
                                             colorList[ulComboBox.getSelectedIndex()]};
        String title = jTextField1.getText();
        if(idTitle != null)
            title += " for " + idTitle;

        // Display the plot
        Plotter plotter = new Plotter(dataX,
                                      dataY,
                                      title,
                                      jTextField3.getText(),
                                      jTextField4.getText(),
                                      name,
                                      symbol,
                                      color,
                                      jCheckBox1.isSelected(),
                                      jCheckBox2.isSelected(),
                                      jCheckBox3.isSelected(),
                                      addedLineColor,
                                      legendLocation,
                                      showHorizontalGrid, 
                                      showVerticalGrid, 
                                      showTicksOnX,
                                      showTicksOnY,
                                      selectedMaxX,
                                      selectedMinX,
                                      selectedMaxY, 
                                      selectedMinY);
        plotter.setToolTipText("");
        JFrame frame = new JFrame();
        frame.getContentPane().add(plotter);
        frame.setLocation(x, y);
	frame.setSize(width, height);
	frame.setTitle("Model Design Agent Data Plot");	
	frame.setVisible(true);
    }//GEN-LAST:event_OKButtonActionPerformed

    private void setTitle()
    {
        String x = (String)xComboBox.getSelectedItem();
        String y1 = (String)y1ComboBox.getSelectedItem();
        String y2 = (String)y2ComboBox.getSelectedItem();
        String y3 = (String)y3ComboBox.getSelectedItem();
        String y = "";
        selection = new Vector();
        curveName = new Vector();
        if(y1 != null && !y1.equals("none"))
        {
            y += y1 + " ";
            curveName.addElement("y1");
            selection.addElement(new Integer(y1ComboBox.getSelectedIndex()));
        }
        if(y2 != null && !y2.equals("none"))
        {
            y += y2 + " ";
            curveName.addElement("y2");
            selection.addElement(new Integer(y2ComboBox.getSelectedIndex()));
        }
        if(y3 != null && !y3.equals("none"))
        {
            y += y3 + " ";
            curveName.addElement("y3");
            selection.addElement(new Integer(y3ComboBox.getSelectedIndex()));
        }
        if(x != null && !x.equals("none") && !y.equals("")) 
        {
            String title = y + "Versus " + x;
            jTextField1.setText(title);
            jTextField3.setText(x);
            jTextField4.setText(y.trim());
            jTextField1.setCaretPosition(title.length());
            try
            {
                jTextField1.setHighlighter(highlighter1);
                highlighter1.addHighlight(0, title.length(), highlight_painter);
                isHighlight1 = true;
                jTextField3.setHighlighter(highlighter2);                
                highlighter2.addHighlight(0, x.length(), highlight_painter);
                isHighlight2 = true;                
                jTextField4.setHighlighter(highlighter3);
                highlighter3.addHighlight(0, y.length() - 1, highlight_painter);
                isHighlight3 = true;                
            }
            catch(BadLocationException e) 
            {
                JOptionPane.showMessageDialog(null, e, "BadLocationException", JOptionPane.ERROR_MESSAGE);
            }
            jTextField1.requestFocusInWindow();
            OKButton.setEnabled(true);
            advancedButton.setEnabled(true);
        }
        else
        {
            jTextField1.setText("");
            jTextField3.setText("");
            jTextField4.setText("");
            OKButton.setEnabled(false);
            advancedButton.setEnabled(false);
        }
    }

    private class ComboBoxRenderer extends javax.swing.JPanel implements ListCellRenderer
    {
        public ComboBoxRenderer(String type)
        {
            this.type = type;
            label.setPreferredSize(new java.awt.Dimension(8, 8));
            label.setOpaque(true);
            label.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
            label.setVerticalAlignment(javax.swing.SwingConstants.CENTER);
            if(type.equals("color"))
                add(label);
        }
        public Component getListCellRendererComponent(javax.swing.JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) 
        {
            intValue = ((Integer)value).intValue();
            if(type.equals("color"))
                label.setBackground(colorList[intValue]);
            repaint();
            return this;
        }

        public void paintComponent(Graphics gc) 
        {
            super.paintComponent(gc);
            setBackground(Color.white);
            if(type.equals("color"))
                return;
            Graphics2D gc2D = ((Graphics2D)gc);
            gc2D.setColor(Color.black);
        
	    // determine the center of the panel
            int x = getSize().width / 2;            
            int y = getSize().height / 2;
            
            // Draw symbols
            new Plotter().drawSymbol(gc2D, intValue, x, y);
        }            

            
        int intValue = 0;
        String type = null;
        JLabel label = new JLabel();
    }
    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
//        System.exit(0);
        advancedDialog.dispose();
        dispose();
    }//GEN-LAST:event_exitForm
 
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton OKButton;
    private javax.swing.JButton advancedButton;
    private javax.swing.JDialog advancedDialog;
    private javax.swing.JButton applyButton;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JComboBox c1ComboBox;
    private javax.swing.JComboBox c2ComboBox;
    private javax.swing.JComboBox c3ComboBox;
    private javax.swing.JButton cancelAdvancedButton;
    private javax.swing.JButton cancelButton;
    private javax.swing.JList curveList;
    private javax.swing.JDialog curveListDialog;
    private javax.swing.JButton displayButton;
    private javax.swing.JTextField hTextField;
    private javax.swing.JCheckBox hgCheckBox;
    private javax.swing.JRadioButton ipRadioButton;
    private javax.swing.JCheckBox jCheckBox1;
    private javax.swing.JCheckBox jCheckBox2;
    private javax.swing.JCheckBox jCheckBox3;
    private javax.swing.JCheckBox jCheckBox4;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel20;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextField jTextField4;
    private javax.swing.JTextField maxXTextField;
    private javax.swing.JTextField maxYTextField;
    private javax.swing.JTextField minXTextField;
    private javax.swing.JTextField minYTextField;
    private javax.swing.JRadioButton oRRadioButton;
    private javax.swing.JRadioButton otRadioButton;
    private javax.swing.JButton resetButton;
    private javax.swing.JComboBox s1ComboBox;
    private javax.swing.JComboBox s2ComboBox;
    private javax.swing.JComboBox s3ComboBox;
    private javax.swing.JCheckBox txCheckBox;
    private javax.swing.JCheckBox tyCheckBox;
    private javax.swing.JComboBox ulComboBox;
    private javax.swing.JCheckBox vgCheckBox;
    private javax.swing.JTextField wTextField;
    private javax.swing.JComboBox xComboBox;
    private javax.swing.JComboBox xlComboBox;
    private javax.swing.JComboBox y1ComboBox;
    private javax.swing.JComboBox y2ComboBox;
    private javax.swing.JComboBox y3ComboBox;
    private javax.swing.JComboBox ylComboBox;
    // End of variables declaration//GEN-END:variables

    private String text = null;
    private double[][] dataAll = null;
    private Integer[] intArray1 = new Integer[10];
    private Integer[] intArray2 = new Integer[13];
    private Color[] colorList = new Color[]{Color.black, Color.gray, Color.red, Color.pink, 
                                            Color.orange, Color.yellow, Color.green, 
                                            Color.magenta, Color.cyan, Color.blue};    
    private Vector selection = null;
    private Vector curveName = null;
    private Vector startIndex = null;
    private boolean isHighlight1 = false;
    private boolean isHighlight2 = false;
    private boolean isHighlight3 = false;
    private DefaultHighlighter highlighter1 = new DefaultHighlighter();
    private DefaultHighlighter highlighter2 = new DefaultHighlighter();
    private DefaultHighlighter highlighter3 = new DefaultHighlighter();    
    private DefaultHighlighter.DefaultHighlightPainter highlight_painter =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(200,200,250));
    private DefaultListModel model = new DefaultListModel();
    private String legendLocation = "Inside";
    private boolean showHorizontalGrid = true;
    private boolean showVerticalGrid = true;
    private boolean showTicksOnX = true;
    private boolean showTicksOnY = true;
    private double minX, maxX, selectedMinX, selectedMaxX, minY, maxY, selectedMinY, selectedMaxY;
    private int width = 500;
    private int height = 400;
    private boolean first = true;
    
    public static void main(String[] args)
    {
        String text = null; 
        javax.swing.JFileChooser files = new javax.swing.JFileChooser();
        files.setDialogTitle("Open File");
        int result = files.showOpenDialog(null);
        if(result == files.APPROVE_OPTION) 
	{
            try
	    {
                java.io.BufferedReader in = new java.io.BufferedReader(new java.io.FileReader(files.getSelectedFile()));
                StringBuffer buffer = new StringBuffer();
                boolean done = false;
                while(!done)
                {
                    // Read a line
                    String line = in.readLine();                            
                    if(line == null) 
                        done = true;
                    else
                        buffer.append(line).append("\n");
	        }
                in.close();
                text = buffer.toString();
            }
            catch(java.io.IOException ioe )
	    {
            }            
        }
        new PlotTool(text);
    }
}
