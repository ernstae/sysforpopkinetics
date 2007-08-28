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

import uw.rfpk.mda.nonmem.MDAFrame;
import uw.rfpk.mda.nonmem.Utility;
import uw.rfpk.mda.nonmem.wizard.MDAObject;
import uw.rfpk.mda.nonmem.wizard.MDAIterator;
import uw.rfpk.mda.nonmem.wizard.Subroutines;
import uw.rfpk.mda.Plotter;
import uw.rfpk.mda.PrintPreview;
import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JTextArea;
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
import java.util.ArrayList;
import java.util.Properties;
import java.util.Enumeration;
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
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;
import java.awt.print.*;
import java.awt.image.*;
import java.io.File;
import java.io.IOException;
import javax.print.attribute.*;
import javax.imageio.ImageIO;
import javax.help.*;
import java.net.URL;

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
        helpButton.addActionListener(new CSH.DisplayHelpFromSource(MDAFrame.getHelpBroker()));
        CSH.setHelpIDString(helpButton, "Compartment_Model_Design_Tool");
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
            subjectModel.setProperty(subjects[i], "1");
            list.addElement("Subject " + subjects[i] + ":  Group-1");
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
        addButton.doClick();
        Reload reload = new Reload(this);
        isInit = false;
        if(iterator.analysis.equals("individual") || iterator.analysis.equals("two-stage") || iterator.analysis.equals("nonparametric"))
        {
            addButton.setEnabled(false);
            applyButton.setEnabled(false);
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
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        subjectDialog = new javax.swing.JDialog();
        jScrollPane4 = new javax.swing.JScrollPane();
        jList1 = new javax.swing.JList();
        jPanel4 = new javax.swing.JPanel();
        assignButton = new javax.swing.JButton();
        doneButton = new javax.swing.JButton();
        jLabel5 = new javax.swing.JLabel();
        subjectComboBox = new javax.swing.JComboBox();
        jLabel6 = new javax.swing.JLabel();
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
        jToggleButton2 = new javax.swing.JToggleButton();
        compButton = new javax.swing.JButton();
        delayButton = new javax.swing.JButton();
        fluxButton = new javax.swing.JButton();
        inputButton = new javax.swing.JButton();
        sampleButton = new javax.swing.JButton();
        changeButton = new javax.swing.JButton();
        jToggleButton1 = new javax.swing.JToggleButton();
        jPanel2 = new javax.swing.JPanel();
        parameterButton = new javax.swing.JButton();
        defaultsButton = new javax.swing.JButton();
        finishButton = new javax.swing.JButton();
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
        jPanel6 = new javax.swing.JPanel();
        saveButton = new javax.swing.JButton();
        printButton = new javax.swing.JButton();
        clearButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        helpButton = new javax.swing.JButton();
        jPanel7 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        addButton = new javax.swing.JButton();
        applyButton = new javax.swing.JButton();

        subjectDialog.getContentPane().setLayout(new java.awt.GridBagLayout());

        subjectDialog.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        subjectDialog.setTitle("ID Group");
        subjectDialog.setModal(true);
        subjectDialog.addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                subjectDialogWindowClosing(evt);
            }
        });

        jScrollPane4.setViewportView(jList1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 11, 11, 11);
        subjectDialog.getContentPane().add(jScrollPane4, gridBagConstraints);

        assignButton.setText("Assign");
        assignButton.setToolTipText("Apply the selected model to the selected subject(s).");
        assignButton.setMargin(new java.awt.Insets(2, 12, 2, 12));
        assignButton.setPreferredSize(new java.awt.Dimension(75, 25));
        assignButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                assignButtonActionPerformed(evt);
            }
        });

        jPanel4.add(assignButton);

        doneButton.setText("Done");
        doneButton.setPreferredSize(new java.awt.Dimension(75, 25));
        doneButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                doneButtonActionPerformed(evt);
            }
        });

        jPanel4.add(doneButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        subjectDialog.getContentPane().add(jPanel4, gridBagConstraints);

        jLabel5.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel5.setText("Select a group: ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(13, 11, 4, 0);
        subjectDialog.getContentPane().add(jLabel5, gridBagConstraints);

        subjectComboBox.setPreferredSize(new java.awt.Dimension(100, 24));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 12);
        subjectDialog.getContentPane().add(subjectComboBox, gridBagConstraints);

        jLabel6.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel6.setText("Subject ID:  Group name");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(5, 12, 1, 12);
        subjectDialog.getContentPane().add(jLabel6, gridBagConstraints);

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

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle("Compartment Model Design Tool");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        jSplitPane1.setBackground(new java.awt.Color(255, 255, 255));
        jSplitPane1.setBorder(javax.swing.BorderFactory.createEtchedBorder());
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

        jInternalFrame3.setTitle("Model Parameters");
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
        jToggleButton2.setIcon(new javax.swing.ImageIcon(getClass().getResource("/uw/rfpk/mda/nonmem/compartment/unlock.png")));
        jToggleButton2.setToolTipText("Toggle compartment, delay and transfer lock/unlock");
        jToggleButton2.setMargin(new java.awt.Insets(0, 0, 0, 0));
        jToggleButton2.setMaximumSize(new java.awt.Dimension(15, 40));
        jToggleButton2.setMinimumSize(new java.awt.Dimension(15, 40));
        jToggleButton2.setPreferredSize(new java.awt.Dimension(15, 40));
        jToggleButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButton2ActionPerformed(evt);
            }
        });

        jToolBar2.add(jToggleButton2);

        compButton.setToolTipText("Compartment: click canvas");
        compButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        compButton.setMaximumSize(new java.awt.Dimension(40, 40));
        compButton.setPreferredSize(new java.awt.Dimension(40, 40));
        compButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                compButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(compButton);

        delayButton.setToolTipText("Delay: click canvas");
        delayButton.setMaximumSize(new java.awt.Dimension(40, 40));
        delayButton.setPreferredSize(new java.awt.Dimension(40, 40));
        delayButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                delayButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(delayButton);

        fluxButton.setToolTipText("Flux: click \"from\" then \"to\" compartment/delay");
        fluxButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        fluxButton.setMaximumSize(new java.awt.Dimension(40, 40));
        fluxButton.setPreferredSize(new java.awt.Dimension(40, 40));
        fluxButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                fluxButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(fluxButton);

        inputButton.setToolTipText("Input: click compartment/delay then click canvas");
        inputButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        inputButton.setMaximumSize(new java.awt.Dimension(40, 40));
        inputButton.setPreferredSize(new java.awt.Dimension(40, 40));
        inputButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                inputButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(inputButton);

        sampleButton.setToolTipText("Observation:  click compartment/delay then click canvas");
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

        changeButton.setToolTipText("Change condition in Differential Equation Structure");
        changeButton.setMargin(new java.awt.Insets(2, 0, 2, 0));
        changeButton.setMaximumSize(new java.awt.Dimension(40, 40));
        changeButton.setMinimumSize(new java.awt.Dimension(40, 40));
        changeButton.setPreferredSize(new java.awt.Dimension(40, 40));
        changeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                changeButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(changeButton);

        jToggleButton1.setText("1");
        jToggleButton1.setToolTipText("Toggle compartment and delay number/name");
        jToggleButton1.setMargin(new java.awt.Insets(2, 0, 2, 0));
        jToggleButton1.setMaximumSize(new java.awt.Dimension(15, 40));
        jToggleButton1.setMinimumSize(new java.awt.Dimension(15, 40));
        jToggleButton1.setPreferredSize(new java.awt.Dimension(15, 40));
        jToggleButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jToggleButton1ActionPerformed(evt);
            }
        });

        jToolBar2.add(jToggleButton1);

        jPanel2.setMinimumSize(new java.awt.Dimension(0, 10));
        jPanel2.setPreferredSize(new java.awt.Dimension(0, 10));
        jToolBar2.add(jPanel2);

        parameterButton.setText("Parameter");
        parameterButton.setToolTipText("Show user defined variables");
        parameterButton.setMaximumSize(new java.awt.Dimension(74, 27));
        parameterButton.setMinimumSize(new java.awt.Dimension(74, 27));
        parameterButton.setPreferredSize(new java.awt.Dimension(74, 27));
        parameterButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                parameterButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(parameterButton);

        defaultsButton.setText("Defaults");
        defaultsButton.setToolTipText("Showing user defined equations");
        defaultsButton.setMaximumSize(new java.awt.Dimension(74, 27));
        defaultsButton.setMinimumSize(new java.awt.Dimension(74, 27));
        defaultsButton.setPreferredSize(new java.awt.Dimension(74, 27));
        defaultsButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                defaultsButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(defaultsButton);

        finishButton.setText("Finish");
        finishButton.setToolTipText("Finish using this tool");
        finishButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        finishButton.setMaximumSize(new java.awt.Dimension(74, 27));
        finishButton.setMinimumSize(new java.awt.Dimension(74, 27));
        finishButton.setPreferredSize(new java.awt.Dimension(74, 27));
        finishButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                finishButtonActionPerformed(evt);
            }
        });

        jToolBar2.add(finishButton);

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

        statusBarR.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));
        statusBarR.setBorder(BorderFactory.createLineBorder(Color.DARK_GRAY));
        jLabel1.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel1.setText("No. of groups");
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

        jPanel5.setLayout(new javax.swing.BoxLayout(jPanel5, javax.swing.BoxLayout.X_AXIS));

        jPanel6.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.LEFT, 2, 5));

        saveButton.setText("Save");
        saveButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        saveButton.setPreferredSize(new java.awt.Dimension(60, 25));
        saveButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveButtonActionPerformed(evt);
            }
        });

        jPanel6.add(saveButton);

        printButton.setText("Print");
        printButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        printButton.setPreferredSize(new java.awt.Dimension(60, 25));
        printButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                printButtonActionPerformed(evt);
            }
        });

        jPanel6.add(printButton);

        clearButton.setText("Clear");
        clearButton.setToolTipText("");
        clearButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        clearButton.setPreferredSize(new java.awt.Dimension(60, 25));
        clearButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                clearButtonActionPerformed(evt);
            }
        });

        jPanel6.add(clearButton);

        cancelButton.setText("Cancel");
        cancelButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        cancelButton.setPreferredSize(new java.awt.Dimension(60, 25));
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });

        jPanel6.add(cancelButton);

        helpButton.setText("Help");
        helpButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        helpButton.setPreferredSize(new java.awt.Dimension(60, 25));
        jPanel6.add(helpButton);

        jPanel5.add(jPanel6);

        jPanel7.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.RIGHT, 2, 5));

        jLabel3.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel3.setText("Add additional ID group: ");
        jPanel7.add(jLabel3);

        addButton.setText("New");
        addButton.setToolTipText("Add a new ID group");
        addButton.setMaximumSize(new java.awt.Dimension(74, 25));
        addButton.setMinimumSize(new java.awt.Dimension(74, 25));
        addButton.setPreferredSize(new java.awt.Dimension(74, 25));
        addButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addButtonActionPerformed(evt);
            }
        });

        jPanel7.add(addButton);

        applyButton.setText("Assign");
        applyButton.setToolTipText("Assign IDs to groups");
        applyButton.setMargin(new java.awt.Insets(2, 2, 2, 2));
        applyButton.setMaximumSize(new java.awt.Dimension(74, 25));
        applyButton.setMinimumSize(new java.awt.Dimension(74, 25));
        applyButton.setPreferredSize(new java.awt.Dimension(74, 25));
        applyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                applyButtonActionPerformed(evt);
            }
        });

        jPanel7.add(applyButton);

        jPanel5.add(jPanel7);

        getContentPane().add(jPanel5, java.awt.BorderLayout.NORTH);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void changeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_changeButtonActionPerformed
        new ChangeCondition(this);
    }//GEN-LAST:event_changeButtonActionPerformed

    private void jToggleButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jToggleButton2ActionPerformed
        lock = !lock;
        if(lock)
            jToggleButton2.setIcon(new javax.swing.ImageIcon(getClass().getResource("/uw/rfpk/mda/nonmem/compartment/lock.png")));
        else
            jToggleButton2.setIcon(new javax.swing.ImageIcon(getClass().getResource("/uw/rfpk/mda/nonmem/compartment/unlock.png")));
        pickedElement = "";
        diagram.setCursor(null);
    }//GEN-LAST:event_jToggleButton2ActionPerformed

    private void subjectDialogWindowClosing(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_subjectDialogWindowClosing
        subjectDialog.toFront();
        doneButton.doClick();
    }//GEN-LAST:event_subjectDialogWindowClosing

    private void defaultsButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_defaultsButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        new DefaultModelDialog(this);
        setCursor(null);
    }//GEN-LAST:event_defaultsButtonActionPerformed

    private void printButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_printButtonActionPerformed
        // Get a PrinterJob object and a PageFormat object.
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        printerJob = PrinterJob.getPrinterJob();
        attributes = new HashPrintRequestAttributeSet();         
        pageFormat = printerJob.pageDialog(attributes);

        // Show print setting dialog if pageFormat != null.
        if(pageFormat != null)
        {
            // Set what to print.
            printerJob.setPrintable(new Printer());            
            try
	    {
                printerJob.print(attributes);
            }
            catch(PrinterException pe)
	    {
                JOptionPane.showMessageDialog(null, "Error printing " + pe,  // Display printing 
                                              "Printer Error",               // error message
                                              JOptionPane.ERROR_MESSAGE);
            }
        }
        setCursor(null);
    }//GEN-LAST:event_printButtonActionPerformed

    private void saveButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_saveButtonActionPerformed
        // Create an image to save
        // Create a buffered image in which to draw
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        int plotWidth = diagram.getSize().width;
        int plotHeight = diagram.getSize().height;
        BufferedImage bufferedImage = new BufferedImage(plotWidth, plotHeight, 
                                                        BufferedImage.TYPE_INT_RGB);
    
        // Create a graphics contents on the buffered image
        Graphics2D g2d = bufferedImage.createGraphics();
        g2d.setColor(Color.white);
        g2d.fillRect(0, 0, plotWidth, plotHeight);
        g2d.setColor(Color.black);
        PageFormat format = new PageFormat();
        format.setOrientation(PageFormat.PORTRAIT);
        Paper paper = new Paper();
        paper.setImageableArea(0, 0, plotWidth, plotHeight);
        format.setPaper(paper);
        Printable printer = new Printer();
        try
        {
            printer.print(g2d, format, 0);
            JFileChooser fileChooser = new JFileChooser();
            fileChooser.setDialogTitle("Save Image File");
            fileChooser.removeChoosableFileFilter(fileChooser.getFileFilter());
            fileChooser.addChoosableFileFilter(new MyFilter("jpg"));
            fileChooser.addChoosableFileFilter(new MyFilter("png"));
            int result = fileChooser.showSaveDialog(null);
            if(result == fileChooser.APPROVE_OPTION)
	    {
                FileFilter filter = fileChooser.getFileFilter();
                File file = fileChooser.getSelectedFile();
                String pathname = file.getPath();
                String type = filter.getDescription();
                if(pathname.indexOf(".") == -1)
                    pathname += "." + type;
                else
                    pathname = pathname.substring(0, pathname.indexOf(".")) + "." + type;
                ImageIO.write(bufferedImage, type, new File(pathname));
            }
        }
        catch(PrinterException e)
        {
            JOptionPane.showMessageDialog(null, e, "Printing Error", JOptionPane.ERROR_MESSAGE);
        }
        catch (IOException e) 
        {
            JOptionPane.showMessageDialog(null, e, "File Error", JOptionPane.ERROR_MESSAGE);
        }
        setCursor(null);
    }//GEN-LAST:event_saveButtonActionPerformed

    private class MyFilter extends FileFilter 
    {
        public MyFilter(String type)
        {
            this.type = type;    
        }
        
        public boolean accept(File file) 
        {
            if(file.isDirectory()) return true;
            String filename = file.getName();
            return filename.endsWith(type);
        }
        public String getDescription() {
            return type;
        }
        
        String type;
    }
    
    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        if(JOptionPane.showConfirmDialog(null, 
                                         "Are you sure you want to close the graphical model editor?",   
                                         "Question Dialog",
                                         JOptionPane.YES_NO_OPTION,
                                         JOptionPane.QUESTION_MESSAGE) == 0)
        {
            setVisible(false);
        }
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void jToggleButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jToggleButton1ActionPerformed
        showName = !showName;
        if(showName)
            jToggleButton1.setText("A");
        else
            jToggleButton1.setText("1");
        repaint();
    }//GEN-LAST:event_jToggleButton1ActionPerformed

    private void finishButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_finishButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        boolean ok = record.setModel(object);
        ok = ok && record.setPK(object, iterator);
        ok = ok && record.setDes(object);
        ok = ok && record.setError(object, iterator);
        if(ok)
        {
            if(iterator.getReload() != null)
            {
                iterator.getReload().remove("MODEL");
                iterator.getReload().remove("PK");
                iterator.getReload().remove("DES");
                iterator.getReload().remove("ERROR");
            }
            if(iterator.initAdvan != null)
            {
                iterator.initAdvan.remove("model");
                iterator.initAdvan.remove("pk");
                iterator.initAdvan.remove("des");
            }
            ((Subroutines)panel).setValid();
            setVisible(false);
        }
        setCursor(null);
    }//GEN-LAST:event_finishButtonActionPerformed

    private void parameterButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_parameterButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        VariableDialog variableDialog = new VariableDialog(this);
        setCursor(null);
    }//GEN-LAST:event_parameterButtonActionPerformed

    private void doneButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_doneButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        for(int i = 0; i < subjectModel.size(); i++)
            if(subjectModel.getProperty(subjects[i]).equals(""))
            {
                JOptionPane.showMessageDialog(null, "Subject " + subjects[i] + " is not assigned to group.",
                                              "Input Error", JOptionPane.ERROR_MESSAGE);
                setCursor(null);
                return;
            }
        for(int i = 0; i < models.size(); i++)
            if(!subjectModel.contains(String.valueOf(((Model)models.get(i)).id)))
            {
                if(JOptionPane.showConfirmDialog(null, ((Model)models.get(i)).name + " has no member.\n" +
                                                 "Do you want to delete " + ((Model)models.get(i)).name + "?",   
                                                 "Question Dialog",
                                                 JOptionPane.YES_NO_OPTION,
                                                 JOptionPane.QUESTION_MESSAGE) == 0)
                {
                    deleteModel();
                }
                else
                {
                    setCursor(null);
                    return;
                }
            }
        subjectDialog.setVisible(false);
        setRecords();
        setCursor(null);
    }//GEN-LAST:event_doneButtonActionPerformed
    
    /** Display records */
    protected void setRecords()
    {
        pkTextArea.setText(record.getPK());          // must call getPK before call getModel
        pkTextArea.setCaretPosition(0);
        modelTextArea.setText(record.getModel());
        modelTextArea.setCaretPosition(0);
        desTextArea.setText(record.getDes());
        desTextArea.setCaretPosition(0);
        errorTextArea.setText(record.getError());
        errorTextArea.setCaretPosition(0);
        record.checkCode("Model Parameters", pkTextArea, 1);
        record.checkCode("Differential Equation Structure", desTextArea, 2);
        record.checkCode("Residual Unknown Variability Model", errorTextArea, 3);
    }
    
    private void jMenuItem2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem2ActionPerformed
        String name = JOptionPane.showInputDialog(null, "Enter new name.", selectedModel.name);
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        if(name != null && !name.trim().equals(""))
        {
            selectedModel.name = name;
            setRecords();
            resetStatusBar();
        }
        setCursor(null);
    }//GEN-LAST:event_jMenuItem2ActionPerformed

    private void jMenuItem1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItem1ActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        if(iterator.analysis.equals("individual") || iterator.analysis.equals("two-stage") || iterator.analysis.equals("nonparametric"))
        {
            clearButton.doClick();
            setCursor(null);
            return;
        }
        deleteModel();
        if(models.size() > 0)
        {
            applyButton.doClick();
            JOptionPane.showMessageDialog(null, "To continue, choose an existing ID group,\n" + 
                                          "or create a new ID group and re-assign IDs.");
        }
        else
        {
            JOptionPane.showMessageDialog(null, "To continue, create a new ID group and assign IDs.");
        }
        setRecords();
        setCursor(null);
    }//GEN-LAST:event_jMenuItem1ActionPerformed

    private void deleteModel()
    {
        diagram.isDrawable = false;
        diagram.repaint();
        models.remove(selectedModel);
        selectedModel = null;
        if(index > 0) index--;
        resetStatusBar();
        jTextField1.setText(String.valueOf(models.size()));
    }
    
    private void rightButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_rightButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        index++;
        resetStatusBar();
        setCursor(null);
    }//GEN-LAST:event_rightButtonActionPerformed

    private void leftButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_leftButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        index--;
        resetStatusBar();
        setCursor(null);
    }//GEN-LAST:event_leftButtonActionPerformed

    private void addButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        if(selectedModel != null) selectedModel.isSelected = false;
        diagram.model.isCopyToDiagram = false;
        diagram.model.inputs.clear();
        diagram.model.samples.clear();        
        diagram.isDrawable = true;
        diagram.repaint();
        selectedModel = (Model)diagram.model.clone();
        selectedModel.id = ++modelId;
        selectedModel.name = "Group-" + modelId;
        selectedModel.isSelected = true;
        models.add(selectedModel);
        if(models.size() - index > 5)
            index = models.size() - 5;
        resetStatusBar();
        applyButton.setEnabled(true);
        if(!isInit) 
            applyButton.doClick();      
        jTextField1.setText(String.valueOf(models.size()));
        setCursor(null);
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
        if(Model.elements.size() > 0 &&
           JOptionPane.showConfirmDialog(null, "Are you sure you want to remove all models?",
                                         "Confirmation Question",
                                         JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == 0)
            clear();
    }//GEN-LAST:event_clearButtonActionPerformed
    
    /* Remove all models and if initial add a group. **/
    protected void clear()
    {
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        list.removeAllElements();
        Model.parameterList.clear();
        Model.desEqns = "";
        Model.errorEqns = "";
        models.removeAllElements();
        modelId = 0;
        resetStatusBar();
        selectedModel = null;
        diagram.model.clear();
        diagram.startElement = null;
        diagram.endElement = null;
        diagram.tempElement = null;       
        diagram.repaint();
        for(int i = 0; i < subjects.length; i++)
        {
            subjectModel.setProperty(subjects[i], "1");
            list.addElement("Subject " + subjects[i] + ":  Group-1");
        }
        isInit = true;
        addButton.setEnabled(true);
        addButton.doClick();
        isInit = false;
        if(iterator.analysis.equals("individual") || iterator.analysis.equals("two-stage") || iterator.analysis.equals("nonparametric"))
            addButton.setEnabled(false);       
        setRecords();
        setCursor(null);   
    }
        
    private void assignButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_assignButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        Model group = ((Model)models.get(subjectComboBox.getSelectedIndex()));
        int[] selectedIndices = jList1.getSelectedIndices();
        for(int i = 0; i < selectedIndices.length; i++)
        {
            int in = selectedIndices[i];
            subjectModel.setProperty(subjects[in], String.valueOf(group.id));
            list.setElementAt("Subject " + subjects[in] + ":  " + group.name, in);
        }
        setCursor(null);
    }//GEN-LAST:event_assignButtonActionPerformed

    private boolean removeModel(Model model)
    {
        boolean isRemoved = false;
        Enumeration keys = subjectModel.keys();
        String key;
        String[] parts;
        while(keys.hasMoreElements())
        {
            key = (String)keys.nextElement();
            if(subjectModel.getProperty(key).equals(String.valueOf(model.id)))
            {
                subjectModel.setProperty(key, "");
                isRemoved = true;
            }
        }
        return isRemoved;
    }
    
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
        if(lock) jToggleButton2.doClick();
        pickedElement = "sample";
        diagram.setCursor(sampleCursor);
    }//GEN-LAST:event_sampleButtonActionPerformed

    private void inputButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_inputButtonActionPerformed
        if(lock) jToggleButton2.doClick();
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
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        subjectComboBox.removeAllItems();
        for(int i = 0; i < models.size(); i++)
            subjectComboBox.addItem(((Model)models.get(i)).name);
        list.clear();
        for(int i = 0; i < subjects.length; i++)
        {
            String modelName = "";
            for(int j = 0; j < models.size(); j++)
            {
                Model model = (Model)models.get(j);
                if(subjectModel.getProperty(subjects[i]).equals(String.valueOf(model.id)))
                    modelName = model.name;
            }
            list.addElement("Subject " + subjects[i] + ":  " + modelName);
        }
        subjectDialog.setSize(240, 300);
        Point p = applyButton.getLocation();
        subjectDialog.setLocation(p.x + 200, p.y + 100);
        subjectDialog.setVisible(true);
        setCursor(null);
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
            if(evt.isPopupTrigger() && selectedModel != null)
                jPopupMenu1.show(evt.getComponent(), evt.getX(), evt.getY());
        }
        
        private void modelButtonActionPerformed(java.awt.event.ActionEvent evt) 
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            selectedButton = (ModelButton)evt.getSource();
            diagram.tempElement = null;
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
                    diagram.isDrawable = true;
                    diagram.repaint();
                    break;
                }
            }
            setCursor(null);
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
    
    /** Click Add button */
    protected void clickAddButton()
    {
        addButton.doClick();
    }
     
    /** Save the selected model */
    protected void saveModel()
    {
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        int i = models.indexOf(selectedModel);
        int id = selectedModel.id;
        String name = selectedModel.name;
        diagram.model.isCopyToDiagram = false;
        selectedModel = (Model)diagram.model.clone();
        selectedModel.id = id;
        selectedModel.name = name;
        models.remove(i);
        models.insertElementAt(selectedModel, i);
        setRecords();
        setCursor(null);
    }
    
    /** Click Finish button */
    protected void clickFinishButton()
    {
        doneButton.doClick();
    }

    private class Printer implements Printable
    {   
        public int print(Graphics gc, PageFormat pageFormat, int pageIndex) throws PrinterException
        {
            if(pageIndex != 0)
                return NO_SUCH_PAGE; 
            Graphics2D gc2D = ((Graphics2D)gc);
            gc2D.scale(scale, scale);
            
            // Get upper-left corner coordinates
            int lineInsetX  = (int)(pageFormat.getImageableX() / scale);
            int lineInsetY  = (int)(pageFormat.getImageableY() / scale);
            diagram.model.draw(gc2D, lineInsetX, lineInsetY);
            return PAGE_EXISTS;
        }
    }

    /** Update parameter list following updating depending variable list of the passed-in parameter.
     * @param parameter parameter that has been changed in value or removed.
     * @param isRemove true for parameter has been removed, false otherwise.
     * return true if new variable is added to the parameter list, otherwise false.
     */
    protected boolean updateParameterList(Parameter parameter, boolean isRemove)
    {
        // update parameter list after removing a parameter
        if(isRemove)
        {
            for(Variable variable : parameter.dependVariables)
            {
                variable.refCount--;
                if(variable.refCount == 0)
                    Model.parameterList.remove(variable);
            }
            return false;
        }
        
        // Tokenize the parameter value
        String[] lines = parameter.value.trim().split("\n");
        ArrayList<String> symbols = new ArrayList<String>();
        ArrayList<String> variables = new ArrayList<String>();
        for(int i = 0; i < lines.length; i++) 
        {
            if(lines[i].startsWith("IF("))
            {
                String[] tokens = lines[i].toUpperCase().replaceAll(".AND.", " ").replaceAll(".OR.", " ")
                                  .replaceAll(".EQ.", " ").replaceAll(".NE.", " ").replaceAll(".GT.", " ")
                                  .replaceAll(".LT.", " ").replaceAll(".GE.", " ").replaceAll(".LE.", " ")
                                  .split("[ |+|\\-|*|/|(|)|>|<|=]");
                for(String token : tokens)
                    if(checkVariableName(token) && symbols.indexOf(token) == -1)
                        if(variables.indexOf(token) == -1)
                            variables.add(token);
            }
            else if(lines[i].equals("ELSE") || lines[i].equals("ENDIF"))
            {}
            else
            {
                String[] sides = lines[i].replaceAll(" ", "").split("=");
                if(sides.length == 2)
                {
                    String[] tokens = sides[1].toUpperCase().split("[+|\\-|*|/|(|)]");
                    for(String token : tokens)
                        if(checkVariableName(token) && symbols.indexOf(token) == -1)
                            if(variables.indexOf(token) == -1)
                                variables.add(token);
                    symbols.add(sides[0].toUpperCase());
                }
                else if(sides.length == 1)
                {
                    symbols.add(sides[0].toUpperCase());
                }
                else
                {
                    JOptionPane.showMessageDialog(null, "Error was found in parameter model.",
                                                  "Input Error", JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
        }
        
        // Comparing new and old depending variable lists and update parameter list
        int size = parameter.dependVariables.size();
        for(int i = 0; i < size;  i++)
        {
            Variable var = parameter.dependVariables.get(i);
            if(variables.indexOf(var.name) == -1)    // the depending variable is removed
            {
                --var.refCount;
                if(var.refCount == 0)
                {
                    parameter.dependVariables.remove(var);
                    Model.parameterList.remove(var);
                }
            }
        }
        boolean isNewVariableAdded = false;
        for(String name : variables)
        {
            boolean isNew = true;
            for(Variable var : parameter.dependVariables)
            {
                if(name.equals(var.name))
                {
                    isNew = false;
                    break;
                }
            }
            if(isNew)
            {
                Variable variable = checkVariableExist(name);
                if(variable == null)
                {
                    variable = new Variable(name, name + "=");
                    Model.parameterList.add(Model.parameterList.indexOf(parameter), variable);
                    isNewVariableAdded = true;
                }
                parameter.dependVariables.add(variable);
            }
        }
        return isNewVariableAdded;
    }
    
    private Variable checkVariableExist(String name)
    {
        for(Parameter parameter : Model.parameterList)
        {
            if(parameter instanceof Variable)
            {
                if(parameter.name.equals(name))
                {
                    ((Variable)parameter).refCount++;
                    return (Variable)parameter;
                }
            }
        }
        return null;
    }
    
/*    private boolean checkEquationExist(String name)
    {
        for(Parameter parameter : Model.equationList)
            if(parameter.name.equals(name))
                return true;
        return false;
    }*/
    
    private boolean checkVariableName(String name)
    {
        name = name.toUpperCase();
        String[] functions = {"", "ABS", "COS", "SIN", "ACOS", "ASIN", "ATAN", "ATAN2", "COSH",
                              "MAX", "MIN", "MOD", "SINH", "TAN", "TANH", "EXP", "THETA", "ETA",
                              "IF", "THEN", "ELSE", "ENDIF"};
        for(String function : functions)
            if(name.equals(function))
                return false;
        if(name.matches("TLAG\\d+") || name.matches("[S|F|R|D]\\d+") || name.matches("ALAG\\d+") ||
           name.matches("K\\d+") || name.matches("K\\d+T\\d+") || Utility.isFloatNumber(name))
            return false;
        for(String dataLabel : object.getDataLabels())
        {
            String[] sides = dataLabel.split("=");
            if(name.equals(sides[0]) || (sides.length == 2 && name.equals(sides[1])))
                return false;
        }
        return true;
    }
    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
        if(JOptionPane.showConfirmDialog(null, 
                                         "Are you sure you want to close the graphical model editor?",   
                                         "Question Dialog",
                                         JOptionPane.YES_NO_OPTION,
                                         JOptionPane.QUESTION_MESSAGE) == 0)
        {
            setVisible(false);
        }
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
    
    /** Get pk JTextArea.
     * @return pk JTextArea.
     */
    protected JTextArea getPKTextArea() {return pkTextArea;};
    
    /** Get des JTextArea.
     * @return des JTextArea.
     */
    protected JTextArea getDesTextArea() {return desTextArea;};
    
    /** Get error JTextArea.
     * @return error JTextArea.
     */
    protected JTextArea getErrorTextArea() {return errorTextArea;};
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton addButton;
    private javax.swing.JButton applyButton;
    private javax.swing.JButton assignButton;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton cancelButton;
    protected javax.swing.JButton changeButton;
    private javax.swing.JButton clearButton;
    private javax.swing.JButton compButton;
    protected javax.swing.JButton defaultsButton;
    private javax.swing.JButton delayButton;
    private javax.swing.JTextArea desTextArea;
    private javax.swing.JButton doneButton;
    private javax.swing.JTextArea errorTextArea;
    private javax.swing.JButton finishButton;
    private javax.swing.JButton fluxButton;
    private javax.swing.JButton helpButton;
    private javax.swing.JButton inputButton;
    private javax.swing.JInternalFrame jInternalFrame1;
    private javax.swing.JInternalFrame jInternalFrame2;
    private javax.swing.JInternalFrame jInternalFrame3;
    private javax.swing.JInternalFrame jInternalFrame4;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JList jList1;
    private javax.swing.JMenuItem jMenuItem1;
    private javax.swing.JMenuItem jMenuItem2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
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
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JToggleButton jToggleButton1;
    private javax.swing.JToggleButton jToggleButton2;
    private javax.swing.JToolBar jToolBar2;
    private javax.swing.JButton leftButton;
    private javax.swing.JTextArea modelTextArea;
    protected javax.swing.JButton parameterButton;
    private javax.swing.JTextArea pkTextArea;
    private javax.swing.JButton printButton;
    private javax.swing.JButton rightButton;
    private javax.swing.JButton sampleButton;
    private javax.swing.JButton saveButton;
    private javax.swing.JPanel statusBarL;
    private javax.swing.JPanel statusBarR;
    private javax.swing.JComboBox subjectComboBox;
    private javax.swing.JDialog subjectDialog;
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
    protected Vector<Model> models = new Vector<Model>();
    /** Drawing diagram */
    protected Diagram diagram;
    private Dimension area = new Dimension(0, 0);
    private Rectangle diagramBounds;
    private boolean isFirst = true;
    private int index = 0;
    /** Selected model */
    protected Model selectedModel = null;
    private ModelButton selectedButton = null;
    private int modelId = 0;
    private ButtonGroup buttonGroup;
    private Cursor compCursor, delayCursor, inputCursor, sampleCursor;
    /** Cursor for transfer */
    protected Cursor fluxCursor;
    private DefaultListModel list = new DefaultListModel();
    private DefaultListModel transferList = new DefaultListModel();
    /** Subject IDs */
    protected String[] subjects;
    private Record record = new Record(this);
    /** Show name*/
    protected static boolean showName = false;
    /** MDAIterator object */
    protected MDAIterator iterator;
    private JPanel panel;
    private PrinterJob printerJob;
    private PageFormat pageFormat;
    private PrintRequestAttributeSet attributes;
    /** Is reload */
    protected boolean isInit = true;
    /** Is locked */
    protected boolean lock = false;
}

