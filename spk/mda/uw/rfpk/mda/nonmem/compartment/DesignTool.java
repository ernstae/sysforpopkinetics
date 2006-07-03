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

import uw.rfpk.mda.nonmem.wizard.MDAObject;
import uw.rfpk.mda.nonmem.wizard.MDAIterator;
import uw.rfpk.mda.nonmem.wizard.Subroutines;
import javax.swing.JPanel;
import javax.swing.JButton;
import java.awt.Color;
import java.awt.Insets;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.Graphics;
import java.awt.Graphics2D;
import javax.swing.table.*;
import java.awt.Polygon;
import java.awt.FlowLayout;
import java.awt.BorderLayout;
import javax.swing.BorderFactory;
import java.util.Vector;
import java.util.Properties;
import javax.swing.JToggleButton;
import javax.swing.ButtonGroup;
import java.awt.Cursor;
import java.awt.Toolkit;
import java.awt.Image;
import java.awt.Point;
import javax.swing.ImageIcon;
import java.awt.BasicStroke;
import javax.swing.JOptionPane;
import javax.swing.DefaultListModel;

/** This class defines graphical model editor window.
 *
 * @author  Jiaji Du
 */
public class DesignTool extends javax.swing.JFrame {
    
    /** Creates a new instance of DesignTool without argument */
    public DesignTool(){}
    
    /** Create a new instance of DesignTool with arguments.
     * @param subjects subject ID list.
     * @param imageDir directory where image files are located.
     * @param object MDAObject object.
     * @param iterator MDAIterator object.
     * @param panel panel of the wizard step to start this design tool.
     */
    public DesignTool(String[] subjects, String imageDir, MDAObject object, MDAIterator iterator, JPanel panel)
    {
        this.object = object;
        this.iterator = iterator;
        this.panel = panel;
        initComponents();
        jTextField1.setText("0");
        jTextField2.setText(String.valueOf(subjects.length));
        compButton.add(new ButtonPanel("Compartment"));
        delayButton.add(new ButtonPanel("Delay"));
        fluxButton.add(new ButtonPanel("Flux"));
        inputButton.add(new ButtonPanel("Input"));
        sampleButton.add(new ButtonPanel("Sample"));
        changeButton.add(new ButtonPanel("Change"));
        diagram = new Diagram(this);
        jScrollPane3.setViewportView(diagram);
        this.subjects = subjects;
        jList1.setModel(list);
        for(int i = 0; i < subjects.length; i++)
        {
            subjectModel.setProperty(subjects[i], "");
            list.addElement("Subject " + subjects[i]);
        }
                               
        // Create customer cursors
        Toolkit tk = Toolkit.getDefaultToolkit();
        Image img1 = new ImageIcon(getClass().getResource(imageDir + "comp.png")).getImage();
        Image img2 = new ImageIcon(getClass().getResource(imageDir + "delay.png")).getImage();
        Image img3 = new ImageIcon(getClass().getResource(imageDir + "flux.png")).getImage();
        Image img4 = new ImageIcon(getClass().getResource(imageDir + "input.png")).getImage();
        Image img5 = new ImageIcon(getClass().getResource(imageDir + "sample.png")).getImage();
        Point p = new Point(0,0); // This is the hot-spot in the cursor
        try
        {
	    compCursor = tk.createCustomCursor(img1, p, "Compartment");
            delayCursor = tk.createCustomCursor(img2, p, "Delay");
            fluxCursor = tk.createCustomCursor(img3, p, "Flux");
            inputCursor = tk.createCustomCursor(img4, p, "Input");
            sampleCursor = tk.createCustomCursor(img5, p, "Sample");
        }
        catch(IndexOutOfBoundsException e)
        {
	    System.out.println(e);
        }
        setVisible(true);
    }
    
    private class ButtonPanel extends JPanel
    {
        public ButtonPanel(String symbol)
        {
            this.symbol = symbol;  
        }
        
        public void paintComponent(Graphics gc) 
        {
            super.paintComponent(gc);
            Graphics2D gc2D = ((Graphics2D)gc);

            // Draw the symbol
            if(symbol.equals("Compartment"))
            {
                gc2D.setColor(Color.black);
                gc2D.drawOval(0, 0, 28, 28); 
            }
            if(symbol.equals("Delay"))
            {
                gc2D.setColor(Color.black);
                gc2D.drawRect(0, 4, 27, 19);
                gc2D.drawLine(0, 14, 3, 14);
                gc2D.drawOval(3, 11, 6, 6); 
                gc2D.drawLine(9, 14, 11, 14);
                gc2D.drawOval(11, 11, 6, 6); 
                gc2D.drawLine(17, 14, 19, 14);
                gc2D.drawOval(19, 11, 6, 6); 
                gc2D.drawLine(25, 14, 28, 14);
            }
            if(symbol.equals("Flux"))
            {
                gc2D.setColor(Color.black);
                gc2D.drawLine(0, 14, 24, 14);
                Polygon triangle = new Polygon(new int[]{24, 24, 28}, new int[]{17, 11, 14}, 3);
                gc2D.draw(triangle);
                gc2D.fill(triangle);
            }
            if(symbol.equals("Input"))
            {
                gc2D.setColor(Color.BLUE);
                gc2D.drawLine(1, 24, 4, 27);
                gc2D.drawLine(2, 25, 8, 19);
                gc2D.drawLine(3, 26, 9, 20);                
                gc2D.drawLine(5, 16, 12, 23);
                gc2D.drawLine(7, 18, 19, 6);
                gc2D.drawLine(10, 21, 22, 9);
                gc2D.drawLine(19, 6, 22, 9);                 
                gc2D.drawLine(20, 7, 21, 7);
                gc2D.drawLine(21, 7, 21, 8);
                gc2D.drawLine(22, 6, 28, 0);
                gc2D.fillPolygon(new int[]{11, 19, 22, 14}, new int[]{14, 6, 9, 17}, 4);
            }
            if(symbol.equals("Sample"))
            {
                gc2D.setColor(Color.red);
                gc2D.drawLine(2, 26, 19, 9);
                gc2D.drawOval(18, 2, 8, 8);
            }
            if(symbol.equals("Change"))
            {
                gc2D.setColor(Color.black);
                gc2D.drawLine(4, 24, 16, 24);
                gc2D.drawLine(16, 24, 16, 4);
                gc2D.drawLine(16, 4, 28, 4);
            }
        }
        
        private String symbol;
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        subjectDialog = new javax.swing.JDialog();
        jTextArea3 = new javax.swing.JTextArea();
        jScrollPane4 = new javax.swing.JScrollPane();
        jList1 = new javax.swing.JList();
        jPanel4 = new javax.swing.JPanel();
        jButton2 = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        leftButton = new javax.swing.JButton();
        rightButton = new javax.swing.JButton();
        jPopupMenu1 = new javax.swing.JPopupMenu();
        jMenuItem2 = new javax.swing.JMenuItem();
        jMenuItem1 = new javax.swing.JMenuItem();
        buttonGroup1 = new javax.swing.ButtonGroup();
        jSplitPane1 = new javax.swing.JSplitPane();
        jSplitPane2 = new javax.swing.JSplitPane();
        jSplitPane3 = new javax.swing.JSplitPane();
        jInternalFrame1 = new javax.swing.JInternalFrame();
        jScrollPane1 = new javax.swing.JScrollPane();
        modelTextArea = new javax.swing.JTextArea();
        jInternalFrame3 = new javax.swing.JInternalFrame();
        jScrollPane5 = new javax.swing.JScrollPane();
        pkTextArea = new javax.swing.JTextArea();
        jSplitPane4 = new javax.swing.JSplitPane();
        jInternalFrame2 = new javax.swing.JInternalFrame();
        jScrollPane2 = new javax.swing.JScrollPane();
        desTextArea = new javax.swing.JTextArea();
        jInternalFrame4 = new javax.swing.JInternalFrame();
        jScrollPane6 = new javax.swing.JScrollPane();
        errorTextArea = new javax.swing.JTextArea();
        jPanel1 = new javax.swing.JPanel();
        jToolBar2 = new javax.swing.JToolBar();
        compButton = new javax.swing.JButton();
        delayButton = new javax.swing.JButton();
        fluxButton = new javax.swing.JButton();
        inputButton = new javax.swing.JButton();
        sampleButton = new javax.swing.JButton();
        changeButton = new javax.swing.JButton();
        jToggleButton1 = new javax.swing.JToggleButton();
        jPanel2 = new javax.swing.JPanel();
        clearButton = new javax.swing.JButton();
        saveButton = new javax.swing.JButton();
        addButton = new javax.swing.JButton();
        applyButton = new javax.swing.JButton();
        variableButton = new javax.swing.JButton();
        jScrollPane3 = new javax.swing.JScrollPane();
        jPanel3 = new javax.swing.JPanel();
        statusBarL = new javax.swing.JPanel();
        statusBarR = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jLabel2 = new javax.swing.JLabel();
        jTextField2 = new javax.swing.JTextField();
        jSlider1 = new javax.swing.JSlider();
        jPanel5 = new javax.swing.JPanel();
        jButton3 = new javax.swing.JButton();
        jButton4 = new javax.swing.JButton();
        jButton5 = new javax.swing.JButton();
        jButton6 = new javax.swing.JButton();
        jButton7 = new javax.swing.JButton();
        jButton8 = new javax.swing.JButton();

        subjectDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        subjectDialog.setTitle("Subject - Model");
        subjectDialog.setResizable(false);
        jTextArea3.setText("Subject ID: Model Name");
        subjectDialog.getContentPane().add(jTextArea3, java.awt.BorderLayout.NORTH);

        jScrollPane4.setViewportView(jList1);

        subjectDialog.getContentPane().add(jScrollPane4, java.awt.BorderLayout.CENTER);

        jButton2.setText("Apply");
        jButton2.setToolTipText("Apply the selected model to the selected subject(s).");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        jPanel4.add(jButton2);

        jButton1.setText("Finish");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jPanel4.add(jButton1);

        subjectDialog.getContentPane().add(jPanel4, java.awt.BorderLayout.SOUTH);

        leftButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("/org/netbeans/ui/wizard/plaf/basic/icons/back.gif")));
        leftButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
        leftButton.setMaximumSize(new java.awt.Dimension(20, 20));
        leftButton.setMinimumSize(new java.awt.Dimension(20, 20));
        leftButton.setPreferredSize(new java.awt.Dimension(20, 20));
        leftButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                leftButtonActionPerformed(evt);
            }
        });

        rightButton.setIcon(new javax.swing.ImageIcon(getClass().getResource("/org/netbeans/ui/wizard/plaf/basic/icons/next.gif")));
        rightButton.setMargin(new java.awt.Insets(0, 0, 0, 0));
        rightButton.setMaximumSize(new java.awt.Dimension(20, 20));
        rightButton.setMinimumSize(new java.awt.Dimension(20, 20));
        rightButton.setPreferredSize(new java.awt.Dimension(20, 20));
        rightButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                rightButtonActionPerformed(evt);
            }
        });

        jMenuItem2.setText("Rename");
        jMenuItem2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem2ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem2);

        jMenuItem1.setText("Delete");
        jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItem1ActionPerformed(evt);
            }
        });

        jPopupMenu1.add(jMenuItem1);

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Compartment Model Design Tool");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        jSplitPane1.setBackground(new java.awt.Color(255, 255, 255));
        jSplitPane1.setBorder(new javax.swing.border.EtchedBorder());
        jSplitPane1.setDividerLocation(280);
        jSplitPane1.setDividerSize(2);
        jSplitPane1.setPreferredSize(new java.awt.Dimension(800, 600));
        jSplitPane2.setDividerLocation(300);
        jSplitPane2.setDividerSize(1);
        jSplitPane2.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane2.setResizeWeight(0.5);
        jSplitPane2.setPreferredSize(new java.awt.Dimension(60, 92));
        jSplitPane3.setDividerLocation(150);
        jSplitPane3.setDividerSize(2);
        jSplitPane3.setForeground(new java.awt.Color(0, 0, 0));
        jSplitPane3.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jInternalFrame1.setTitle("Model Specification");
        jInternalFrame1.setVisible(true);
        modelTextArea.setEditable(false);
        jScrollPane1.setViewportView(modelTextArea);

        jInternalFrame1.getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jSplitPane3.setTopComponent(jInternalFrame1);

        jInternalFrame3.setTitle("Mixed Effect Model");
        jInternalFrame3.setVisible(true);
        pkTextArea.setEditable(false);
        jScrollPane5.setViewportView(pkTextArea);

        jInternalFrame3.getContentPane().add(jScrollPane5, java.awt.BorderLayout.CENTER);

        jSplitPane3.setBottomComponent(jInternalFrame3);

        jSplitPane2.setTopComponent(jSplitPane3);

        jSplitPane4.setDividerLocation(150);
        jSplitPane4.setDividerSize(2);
        jSplitPane4.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jInternalFrame2.setTitle("Differential Equation Structure");
        jInternalFrame2.setVisible(true);
        desTextArea.setEditable(false);
        jScrollPane2.setViewportView(desTextArea);

        jInternalFrame2.getContentPane().add(jScrollPane2, java.awt.BorderLayout.CENTER);

        jSplitPane4.setTopComponent(jInternalFrame2);

        jInternalFrame4.setTitle("Residual Unknown Variability Model");
        jInternalFrame4.setVisible(true);
        errorTextArea.setEditable(false);
        jScrollPane6.setViewportView(errorTextArea);

        jInternalFrame4.getContentPane().add(jScrollPane6, java.awt.BorderLayout.CENTER);

        jSplitPane4.setBottomComponent(jInternalFrame4);

        jSplitPane2.setBottomComponent(jSplitPane4);

        jSplitPane1.setLeftComponent(jSplitPane2);

        jPanel1.setLayout(new java.awt.BorderLayout());

        jPanel1.setBackground(new java.awt.Color(255, 255, 255));
        compButton.setToolTipText("Compartment");
        compButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        compButton.setMaximumSize(new java.awt.Dimension(40, 40));
        compButton.setPreferredSize(new java.awt.Dimension(40, 40));
        compButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                compButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(compButton);

        delayButton.setToolTipText("Delay");
        delayButton.setMaximumSize(new java.awt.Dimension(40, 40));
        delayButton.setPreferredSize(new java.awt.Dimension(40, 40));
        delayButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                delayButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(delayButton);

        fluxButton.setToolTipText("Flux");
        fluxButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        fluxButton.setMaximumSize(new java.awt.Dimension(40, 40));
        fluxButton.setPreferredSize(new java.awt.Dimension(40, 40));
        fluxButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                fluxButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(fluxButton);

        inputButton.setToolTipText("Input");
        inputButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        inputButton.setMaximumSize(new java.awt.Dimension(40, 40));
        inputButton.setPreferredSize(new java.awt.Dimension(40, 40));
        inputButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                inputButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(inputButton);

        sampleButton.setToolTipText("Sample");
        sampleButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        sampleButton.setMaximumSize(new java.awt.Dimension(40, 40));
        sampleButton.setMinimumSize(new java.awt.Dimension(40, 40));
        sampleButton.setPreferredSize(new java.awt.Dimension(40, 40));
        sampleButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sampleButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(sampleButton);

        changeButton.setToolTipText("Change Condition");
        changeButton.setMargin(new java.awt.Insets(2, 0, 2, 0));
        changeButton.setMaximumSize(new java.awt.Dimension(40, 40));
        changeButton.setMinimumSize(new java.awt.Dimension(40, 40));
        changeButton.setPreferredSize(new java.awt.Dimension(40, 40));
        jToolBar2.add(changeButton);

        jToggleButton1.setText("^");
        jToggleButton1.setToolTipText("Show name or number");
        jToggleButton1.setMargin(new java.awt.Insets(2, 0, 2, 0));
        jToggleButton1.setMaximumSize(new java.awt.Dimension(14, 40));
        jToggleButton1.setMinimumSize(new java.awt.Dimension(14, 40));
        jToggleButton1.setPreferredSize(new java.awt.Dimension(14, 40));
        jToggleButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButton1ActionPerformed(evt);
            }
        });

        jToolBar2.add(jToggleButton1);

        jToolBar2.add(jPanel2);

        clearButton.setText("Clear");
        clearButton.setToolTipText("Clear the drawing panel and remove all models.");
        clearButton.setEnabled(false);
        clearButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                clearButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(clearButton);

        saveButton.setText("Save");
        saveButton.setToolTipText("Save the current experimrnt to the selected model.");
        saveButton.setEnabled(false);
        saveButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(saveButton);

        addButton.setText("Add");
        addButton.setToolTipText("Add the current model to model list.");
        addButton.setEnabled(false);
        addButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(addButton);

        applyButton.setText("Apply");
        applyButton.setToolTipText("Apply the selected model to subject(s).");
        applyButton.setEnabled(false);
        applyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                applyButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(applyButton);

        variableButton.setText("Variables");
        variableButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                variableButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(variableButton);

        jPanel1.add(jToolBar2, java.awt.BorderLayout.NORTH);

        jPanel1.add(jScrollPane3, java.awt.BorderLayout.CENTER);

        jSplitPane1.setRightComponent(jPanel1);

        getContentPane().add(jSplitPane1, java.awt.BorderLayout.CENTER);

        jPanel3.setLayout(new java.awt.GridLayout(1, 2));

        statusBarL.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 1, 0));

        statusBarL.setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY));
        statusBarL.setPreferredSize(new java.awt.Dimension(400, 20));
        jPanel3.add(statusBarL);

        statusBarR.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.RIGHT, 3, 0));

        statusBarR.setBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0)));
        statusBarR.setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY));
        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("No. of models");
        statusBarR.add(jLabel1);

        jTextField1.setEditable(false);
        jTextField1.setHorizontalAlignment(javax.swing.JTextField.TRAILING);
        jTextField1.setPreferredSize(new java.awt.Dimension(50, 19));
        statusBarR.add(jTextField1);

        jLabel2.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel2.setText("No. of subjects");
        statusBarR.add(jLabel2);

        jTextField2.setEditable(false);
        jTextField2.setHorizontalAlignment(javax.swing.JTextField.TRAILING);
        jTextField2.setPreferredSize(new java.awt.Dimension(50, 19));
        statusBarR.add(jTextField2);

        jSlider1.setMajorTickSpacing(1);
        jSlider1.setMaximum(10);
        jSlider1.setSnapToTicks(true);
        jSlider1.setToolTipText("Drawing Scale");
        jSlider1.setValue(5);
        jSlider1.setMaximumSize(new java.awt.Dimension(120, 40));
        jSlider1.setMinimumSize(new java.awt.Dimension(120, 40));
        jSlider1.setPreferredSize(new java.awt.Dimension(120, 25));
        jSlider1.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                jSlider1StateChanged(evt);
            }
        });

        statusBarR.add(jSlider1);

        jPanel3.add(statusBarR);

        getContentPane().add(jPanel3, java.awt.BorderLayout.SOUTH);

        jPanel5.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 0, 0));

        jButton3.setText("Finish");
        jButton3.setMargin(new java.awt.Insets(2, 2, 2, 2));
        jButton3.setPreferredSize(new java.awt.Dimension(60, 25));
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        jPanel5.add(jButton3);

        jButton4.setText("Back");
        jButton4.setMargin(new java.awt.Insets(2, 2, 2, 2));
        jButton4.setPreferredSize(new java.awt.Dimension(60, 25));
        jPanel5.add(jButton4);

        jButton5.setText("Save");
        jButton5.setMargin(new java.awt.Insets(2, 2, 2, 2));
        jButton5.setPreferredSize(new java.awt.Dimension(60, 25));
        jPanel5.add(jButton5);

        jButton6.setText("Print");
        jButton6.setMargin(new java.awt.Insets(2, 2, 2, 2));
        jButton6.setPreferredSize(new java.awt.Dimension(60, 25));
        jPanel5.add(jButton6);

        jButton7.setText("Cancel");
        jButton7.setMargin(new java.awt.Insets(2, 2, 2, 2));
        jButton7.setPreferredSize(new java.awt.Dimension(60, 25));
        jPanel5.add(jButton7);

        jButton8.setText("Help");
        jButton8.setMargin(new java.awt.Insets(2, 2, 2, 2));
        jButton8.setPreferredSize(new java.awt.Dimension(60, 25));
        jPanel5.add(jButton8);

        getContentPane().add(jPanel5, java.awt.BorderLayout.NORTH);

        pack();
    }//GEN-END:initComponents

    private void jToggleButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jToggleButton1ActionPerformed
        showName = !showName;
        repaint();
    }//GEN-LAST:event_jToggleButton1ActionPerformed

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        boolean ok = record.setModel(object);
        ok = ok && record.setPK(object, iterator);
        ok = ok && record.setDes(object);
        ok = ok && record.setError(object);
        if(ok)
        {
            ((Subroutines)panel).setValid();
            setVisible(false);
        }
    }//GEN-LAST:event_jButton3ActionPerformed

    private void variableButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_variableButtonActionPerformed
        variableDialog.setVariableList();
        variableDialog.setVisible(true);
    }//GEN-LAST:event_variableButtonActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        for(int i = 0; i < subjectModel.size(); i++)
            if(subjectModel.getProperty(subjects[i]).equals(""))
            {
                JOptionPane.showMessageDialog(null, "No model is applied to Subject " + subjects[i] + ".",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                return;
            }
        isModelApplied = true;
        subjectDialog.dispose();
        setRecords();
    }//GEN-LAST:event_jButton1ActionPerformed
    
    /** Display records */
    protected void setRecords()
    {
        modelTextArea.setText(record.getModel());
        modelTextArea.setCaretPosition(0);
        pkTextArea.setText(record.getPK());
        pkTextArea.setCaretPosition(0);
        desTextArea.setText(record.getDes());
        desTextArea.setCaretPosition(0);
        errorTextArea.setText(record.getError());
        errorTextArea.setCaretPosition(0);
    }
    
    private void jMenuItem2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem2ActionPerformed
        String name = JOptionPane.showInputDialog(null, "Enter new name.", selectedModel.name);
        if(name != null && !name.trim().equals(""))
        {
            selectedModel.name = name;
            resetStatusBar();
        }
    }//GEN-LAST:event_jMenuItem2ActionPerformed

    private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem1ActionPerformed
        models.remove(selectedModel);
        selectedModel = null;
        if(index > 0) index--;
        resetStatusBar();
        jTextField1.setText(String.valueOf(models.size()));
        saveButton.setEnabled(false);
        applyButton.setEnabled(false);
    }//GEN-LAST:event_jMenuItem1ActionPerformed

    private void saveButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveButtonActionPerformed
        int i = models.indexOf(selectedModel);
        int id = selectedModel.id;
        String name = selectedModel.name;
        diagram.model.isCopyToDiagram = false;
        selectedModel = (Model)diagram.model.clone();
        selectedModel.id = id;
        selectedModel.name = name;
        models.remove(i);
        models.insertElementAt(selectedModel, i);
        if(isModelApplied)
            setRecords();
    }//GEN-LAST:event_saveButtonActionPerformed

    private void rightButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_rightButtonActionPerformed
        index++;
        resetStatusBar();
    }//GEN-LAST:event_rightButtonActionPerformed

    private void leftButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_leftButtonActionPerformed
        index--;
        resetStatusBar();
    }//GEN-LAST:event_leftButtonActionPerformed

    private void addButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addButtonActionPerformed
        String modelName = JOptionPane.showInputDialog(null, "Enter model name.");
        if(modelName == null)
            return;
        if(selectedModel != null) selectedModel.isSelected = false;
        diagram.model.isCopyToDiagram = false;
        selectedModel = (Model)diagram.model.clone();
        selectedModel.id = ++modelId;
        if(modelName.equals(""))
            modelName = "Model " + modelId;
        selectedModel.name = modelName;
        selectedModel.isSelected = true;
        models.add(selectedModel);
        if(models.size() - index > 5)
            index = models.size() - 5;
        resetStatusBar();
        saveButton.setEnabled(true);
        applyButton.setEnabled(true);
        jTextField1.setText(String.valueOf(models.size()));
    }//GEN-LAST:event_addButtonActionPerformed

    private void resetStatusBar()
    {
        statusBarL.removeAll();
        int nModelShown = models.size() - index;
        boolean scroll = false;
        if(nModelShown > 5) 
        {
            nModelShown = 5;
            scroll = true;
        }
        if(index > 0)
            statusBarL.add(leftButton);
        buttonGroup = new ButtonGroup();
        for(int i = index; i < nModelShown + index; i++)
        {
            Model model = (Model)models.get(i);
            ModelButton modelButton = new ModelButton(model.id);
            modelButton.setText(model.name);
            statusBarL.add(modelButton);
            buttonGroup.add(modelButton);
            modelButton.setSelected(model.isSelected);
            if(model.isSelected) selectedButton = modelButton;
        }
        if(scroll)
            statusBarL.add(rightButton);

        statusBarL.revalidate();
        statusBarL.repaint();
    }
    
    private void clearButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_clearButtonActionPerformed
        isModelApplied = false;
        list.removeAllElements();
        Model.variables.clear();
        for(int i = 0; i < subjects.length; i++)
        {
            subjectModel.setProperty(subjects[i], "");
            list.addElement("Subject " + subjects[i]);
        }
        modelTextArea.setText("");
        pkTextArea.setText("");
        desTextArea.setText("");
        clearButton.setEnabled(false);
        saveButton.setEnabled(false);
        addButton.setEnabled(false);
        applyButton.setEnabled(false);
        models.removeAllElements();
        modelId = 0;
        resetStatusBar();
/*
        if(selectedButton != null)
        {
            buttonGroup.remove(selectedButton);
            selectedButton.setSelected(false);
            buttonGroup.add(selectedButton);
        }
*/
        selectedModel = null;
        diagram.model.clear();
        diagram.startElement = null;
        diagram.endElement = null;
        diagram.tempElement = null;
        diagram.repaint();
    }//GEN-LAST:event_clearButtonActionPerformed

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        int[] selectedIndices = jList1.getSelectedIndices();
        for(int i = 0; i < selectedIndices.length; i++)
        {
            int in = selectedIndices[i];
            subjectModel.setProperty(subjects[in], String.valueOf(selectedModel.id));
            list.setElementAt("Subject " + subjects[in] + ":  " + selectedModel.name, in);
        }
    }//GEN-LAST:event_jButton2ActionPerformed

    private void jSlider1StateChanged(javax.swing.event.ChangeEvent evt) {//GEN-FIRST:event_jSlider1StateChanged
        scale = (double)(jSlider1.getValue() + 5) / 10;
        if(isFirst)
        {
            diagramBounds = diagram.getBounds();
            isFirst = false;
        }
        int width = (int)(diagramBounds.width * scale);
        int height = (int)(diagramBounds.height * scale);
        diagram.scrollRectToVisible(new Rectangle(width, height));
        area.width = width;
        area.height = height;
        diagram.setPreferredSize(area);
        diagram.revalidate();
        diagram.repaint();
    }//GEN-LAST:event_jSlider1StateChanged

    private void delayButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_delayButtonActionPerformed
        pickedElement = "delay";
        diagram.setCursor(delayCursor);
    }//GEN-LAST:event_delayButtonActionPerformed

    private void sampleButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sampleButtonActionPerformed
        pickedElement = "sample";
        diagram.setCursor(sampleCursor);
    }//GEN-LAST:event_sampleButtonActionPerformed

    private void inputButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_inputButtonActionPerformed
        pickedElement = "input";
        diagram.setCursor(inputCursor);
    }//GEN-LAST:event_inputButtonActionPerformed

    private void fluxButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_fluxButtonActionPerformed
        pickedElement = "flux";
        diagram.setCursor(fluxCursor);
    }//GEN-LAST:event_fluxButtonActionPerformed

    private void compButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_compButtonActionPerformed
        pickedElement = "compartment";
        diagram.setCursor(compCursor);
    }//GEN-LAST:event_compButtonActionPerformed

    private void applyButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_applyButtonActionPerformed
        subjectDialog.setSize(180, 210);
        Point p = applyButton.getLocation();
        subjectDialog.setLocation(p.x + 200, p.y + 100);
        subjectDialog.show();
    }//GEN-LAST:event_applyButtonActionPerformed

    class ModelButton extends JToggleButton
    {
        public ModelButton(int id)
        {
            super("Model " + id);
            this.id = id;
            setMargin(new Insets(1, 1, 1, 1));
            setPreferredSize(new Dimension(70, 20));
            addActionListener(new java.awt.event.ActionListener(){
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    modelButtonActionPerformed(evt);
                }
            });
            addMouseListener(new java.awt.event.MouseAdapter() {
                public void mousePressed(java.awt.event.MouseEvent evt) {
                    modelButtonMousePressed(evt);
                }
            });
        }
        
        private void modelButtonMousePressed(java.awt.event.MouseEvent evt) 
        {
            if(evt.isPopupTrigger())
                jPopupMenu1.show(evt.getComponent(), evt.getX(), evt.getY());
        }
        
        private void modelButtonActionPerformed(java.awt.event.ActionEvent evt) 
        {
            selectedButton = (ModelButton)evt.getSource();
            if(selectedModel != null) selectedModel.isSelected = false;
            for(int i = 0; i < models.size(); i++)
            {
                Model model = (Model)models.get(i);
                if(model.id == selectedButton.id)
                {
                    selectedModel = model;
                    selectedModel.isSelected = true;
                    selectedModel.isCopyToDiagram = true;
                    diagram.model = (Model)selectedModel.clone();
                    diagram.repaint();
                    break;
                }
            }
            clearButton.setEnabled(true);
            saveButton.setEnabled(true);
            addButton.setEnabled(true);
            applyButton.setEnabled(true);
        }
        private int id;
    }

    private String[] getIndIDs(Vector dataObject, boolean isInd)
    {
        String[] indIDs;
        if(isInd)
        {
            indIDs = new String[1];
            indIDs[0] = "1";
        }
        else
        {
            int nInd = dataObject.size();
            indIDs = new String[nInd];
            for(int i = 0; i < nInd; i++)
                indIDs[i] = ((String[])((Vector)dataObject.get(i)).get(0))[0];
        }        
        return indIDs;
    }
    
    /** Enable Add button */
    protected void enableAddButton()
    {
        addButton.setEnabled(true);
    }
    
    /** Enable Clear button */
    protected void enableClearButton()
    {
        clearButton.setEnabled(true);
    }
    
    /** Click Finish button */
    protected void clickFinishButton()
    {
        jButton1.doClick();
    }
    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
        setVisible(false);
    }//GEN-LAST:event_exitForm
    
    /** Main method of the application
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        String[] subjects = new String[10];
        for(int i = 0; i < 10; i++)
            subjects[i] = String.valueOf(i + 1);
        MDAObject object = new MDAObject();
        String[] dataLabels = {"ID", "TIME", "DV", "AMT"};
        object.setDataLabels(dataLabels);
        new DesignTool(subjects, "", object, null, null);
    }
    
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton addButton;
    private javax.swing.JButton applyButton;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton changeButton;
    private javax.swing.JButton clearButton;
    private javax.swing.JButton compButton;
    private javax.swing.JButton delayButton;
    private javax.swing.JTextArea desTextArea;
    private javax.swing.JTextArea errorTextArea;
    private javax.swing.JButton fluxButton;
    private javax.swing.JButton inputButton;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JButton jButton4;
    private javax.swing.JButton jButton5;
    private javax.swing.JButton jButton6;
    private javax.swing.JButton jButton7;
    private javax.swing.JButton jButton8;
    private javax.swing.JInternalFrame jInternalFrame1;
    private javax.swing.JInternalFrame jInternalFrame2;
    private javax.swing.JInternalFrame jInternalFrame3;
    private javax.swing.JInternalFrame jInternalFrame4;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JList jList1;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItem2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPopupMenu jPopupMenu1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JScrollPane jScrollPane5;
    private javax.swing.JScrollPane jScrollPane6;
    private javax.swing.JSlider jSlider1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JSplitPane jSplitPane2;
    private javax.swing.JSplitPane jSplitPane3;
    private javax.swing.JSplitPane jSplitPane4;
    private javax.swing.JTextArea jTextArea3;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JToggleButton jToggleButton1;
    private javax.swing.JToolBar jToolBar2;
    private javax.swing.JButton leftButton;
    private javax.swing.JTextArea modelTextArea;
    private javax.swing.JTextArea pkTextArea;
    private javax.swing.JButton rightButton;
    private javax.swing.JButton sampleButton;
    private javax.swing.JButton saveButton;
    private javax.swing.JPanel statusBarL;
    private javax.swing.JPanel statusBarR;
    private javax.swing.JDialog subjectDialog;
    private javax.swing.JButton variableButton;
    // End of variables declaration//GEN-END:variables
    
    /** MDAObject */
    protected MDAObject object;
    /** Mapping subject to model */
    protected Properties subjectModel = new Properties();
    /** Picked element */
    protected String pickedElement = "";
    /** Goemetrical scaling factor */
    protected double scale = 1.0;
    /** All models */
    protected Vector models = new Vector();
    private Diagram diagram;
    private Dimension area = new Dimension(0, 0);
    private Rectangle diagramBounds;
    private boolean isFirst = true;
    private int index = 0;
    private Model selectedModel = null;
    private ModelButton selectedButton = null;
    private int modelId = 0;
    private ButtonGroup buttonGroup;
    private Cursor compCursor, delayCursor, fluxCursor, inputCursor, sampleCursor;
    private DefaultListModel list = new DefaultListModel();
    private String[] subjects;
    private Record record = new Record(models, subjectModel);
    /** Is model applied */
    protected boolean isModelApplied;
    /** Show name*/
    protected static boolean showName = false;
    /** Variable dialog object */
    protected VariableDialog variableDialog = new VariableDialog(this);
    private MDAIterator iterator;
    private JPanel panel;
}

