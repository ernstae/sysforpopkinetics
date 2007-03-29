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

import uw.rfpk.mda.Plotter;
import uw.rfpk.mda.nonmem.MDAFrame;
import uw.rfpk.mda.nonmem.Output;
import uw.rfpk.mda.nonmem.Utility;
import java.util.ArrayList;
import java.awt.Cursor;
import java.awt.Color;
import java.awt.Font;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.DefaultListModel;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import javax.help.*;

/** This class displays results of a nonparametric job.
 *
 * @author  Jiaji
 */
public class NonparamShow extends javax.swing.JDialog {
    
    /** Creates new form NonparamShow.
     * @param frame the MDA main frame window.
     * @param output The output object.
     */
    public NonparamShow(MDAFrame frame, Output output) {
        super(frame, false);
        this.output = output;
        model = new DefaultListModel();
        initComponents();
        jButton1.addActionListener(new CSH.DisplayHelpFromSource(((MDAFrame)frame).getHelpBroker()));
//        CSH.setHelpIDString(jButton1, "");
        
        // Generate table
        StringBuffer buffer = new StringBuffer();
        buffer.append(formatString("Point"));
        buffer.append(formatString("Weight"));
        
        // Generate data array for plotting
        int nPoint = output.nonparamOutTheta.length;
        int nTheta = output.nonparamOutTheta[0].length;
        int nBlock = output.nonparamOutOmega.length;
        double[][] theta = new double[nTheta][nPoint];
        for(int i = 0; i < nTheta; i++)
            for(int j = 0; j < nPoint; j++)
                theta[i][j] = Double.parseDouble(output.nonparamOutTheta[j][i]);
        double[][][] omega = new double[nBlock][][];
        for(int i = 0; i < nBlock; i++)
        {
            int nOmega = output.nonparamOutOmega[i][0].length;
            omega[i] = new double[nOmega][nPoint];
            for(int j = 0; j < nOmega; j++)
                for(int k = 0; k < nPoint; k++)
                    omega[i][j][k] = Double.parseDouble(output.nonparamOutOmega[i][k][j]);
        }
        
        // Add to the list model and parameter list
        parameterList = new ArrayList<double[]>();
        String item;
        for(int i = 0; i < nTheta; i++)
        {
            item = "THETA" + (i + 1);
            model.addElement("Weight versus " + item);
            parameterList.add(theta[i]);
            buffer.append(formatString(item));
        }
        for(int i = 0; i < nBlock; i++)
        {
            if(output.omegaStruct[i].equals("diagonal"))
            {
                for(int j = 0; j < omega[i].length; j++)
                {
                    if(nBlock == 1)
                        item = "OMEGA" + (j + 1) + "," + (j + 1);
                    else
                        item = "OMEGA[" + (i + 1) + "]" + (j + 1) + "," + (j + 1);
                    model.addElement("Weight versus " + item);
                    parameterList.add(omega[i][j]);
                    buffer.append(formatString(item));
                }
            }
            else
            {
                int k = 1;
                int l = 1;
                for(int j = 0; j < omega[i].length; j++)
                {
                    if(nBlock == 1)
                        item = "OMEGA" + k + "," + l;
                    else
                        item = "OMEGA[" + (i + 1) + "]" + k + "," + l;
                    model.addElement("Weight versus " + item);
                    if(l < k) 
                        l++;
                    else
                    {
                        k++;
                        l = 1;
                    }
                    parameterList.add(omega[i][j]);
                    buffer.append(formatString(item));
                }
            }
        }
        for(int i = 0; i < nPoint; i++)
        {
            buffer.append("\n");
            buffer.append(formatString(String.valueOf(i + 1)));
            buffer.append(" ");
            buffer.append(formatNumeric(output.nonparamWeight[i]));
            for(int j = 0; j < nTheta; j++)
            {
                buffer.append(" ");
                buffer.append(formatNumeric(output.nonparamOutTheta[i][j]));
            }
            for(int j = 0; j < nBlock; j++)
                for(int k = 0; k < output.nonparamOutOmega[j][i].length; k++)
                {
                    buffer.append(" ");
                    buffer.append(formatNumeric(output.nonparamOutOmega[j][i][k]));
                }
        }
        frame.setEditorText(buffer.toString());
        frame.setEditorCaretPosition(0);
        frame.setEditorTitle("Output Weight and Parameters");
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Generated Code ">//GEN-BEGIN:initComponents
    private void initComponents() {
        jTextField1 = new javax.swing.JTextField();
        jScrollPane1 = new javax.swing.JScrollPane();
        plotList = new javax.swing.JList(model);
        jPanel1 = new javax.swing.JPanel();
        displayButton = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Plot List");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                closeDialog(evt);
            }
        });

        jTextField1.setEditable(false);
        jTextField1.setText("Please select.");
        jTextField1.setFocusable(false);
        getContentPane().add(jTextField1, java.awt.BorderLayout.NORTH);

        jScrollPane1.setViewportView(plotList);

        getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        displayButton.setText("Display");
        displayButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                displayButtonActionPerformed(evt);
            }
        });

        jPanel1.add(displayButton);

        jButton1.setText("Help");
        jButton1.setMaximumSize(new java.awt.Dimension(81, 25));
        jButton1.setMinimumSize(new java.awt.Dimension(81, 25));
        jButton1.setPreferredSize(new java.awt.Dimension(81, 25));
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        jPanel1.add(jButton1);

        getContentPane().add(jPanel1, java.awt.BorderLayout.SOUTH);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        JOptionPane.showMessageDialog(null, "Help is not currently available for this topic.");
    }//GEN-LAST:event_jButton1ActionPerformed

    private void displayButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_displayButtonActionPerformed
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        int[] selectedIndex = plotList.getSelectedIndices();
        int nSelectedIndex = selectedIndex.length;
        if(nSelectedIndex == 1 && selectedIndex[0] < 0)
            return;
        
        double[][][] dataX = new double[nSelectedIndex][1][];
        for(int i = 0; i < nSelectedIndex; i++)
            dataX[i][0] = parameterList.get(selectedIndex[i]);
        double[][] dataY = new double[1][];
        dataY[0] = new double[output.nonparamWeight.length];
        for(int i = 0; i < output.nonparamWeight.length; i++)
            dataY[0][i] = Double.parseDouble(output.nonparamWeight[i]);
        maxY = dataY[0][0];
        for(int i = 0; i < dataY.length; i++)
            for(int j = 0; j < dataY[i].length; j++)
                maxY = Math.max(maxY, dataY[i][j]);
        double[] range = Plotter.optDivisions(6, 0, maxY);
        minY = range[0];
        maxY = range[1];
        nVDivi = (int)(range[2] + 0.1);
        for(int i = 0; i < selectedIndex.length; i++)
        {
            JFrame frame = new JFrame(); 
            Plotter plotter = new Plotter(dataX[i],
                                          dataY,
                                          "Weight Versus Parameter",
                                          ((String)model.get(selectedIndex[i])).substring(14),
                                          "Weight",
                                          name, symbol, color,
                                          false, false, false, false, false, true, true,
                                          0, 0,
                                          null, null, null,
                                          null,
                                          5, nVDivi, 6, 6, 4, 4, 4, 4,                                          
                                          0, 0, maxY, minY,
                                          new Font("SansSerif", Font.BOLD, 14),
                                          new Font("SansSerif", Font.BOLD, 12),
                                          new Font("SansSerif", Font.BOLD, 11),
                                          new Font("SansSerif", Font.BOLD, 10),
                                          0, 0, 0, 0, true, true, false, false,
                                          false, 0, false, null,
                                          2, 2, frame, null);

            plotter.setToolTipText("");
            frame.getContentPane().add(plotter);
            frame.setLocation(50 * i, 40 * i);
	    frame.setSize(500, 440);
	    frame.setTitle("Model Design Agent Data Plot");	
	    frame.setVisible(true);
        }
        setCursor(null);
    }//GEN-LAST:event_displayButtonActionPerformed

    /** Display probability of measurements in a table.
     * @param frame the MDA main frame window.
     * @param output The output object.
     */
    public static void densityTable(MDAFrame frame, Output output)
    {
        StringBuffer buffer = new StringBuffer();
        int nPoint = output.nonparamDensity.length;
        int nInd = output.nonparamDensity[0].length;
        buffer.append(formatString("Point"));
        for(int i = 0; i < nInd; i++)
            buffer.append(formatString("ID" + (i + 1)));
        for(int i = 0; i < nPoint; i++)
        {
            buffer.append("\n");
            buffer.append(formatString(String.valueOf(i + 1)));
            for(int j = 0; j < nInd; j++)
            {
                buffer.append(" ");
                buffer.append(formatNumeric(output.nonparamDensity[i][j]));
            }
        }
        frame.setEditorText(buffer.toString());
        frame.setEditorCaretPosition(0);
        frame.setEditorTitle("Individual Conditional Atomic Density");
    }
    
    /** Display initial value of parameters in a table.
     * @param frame the MDA main frame window.
     * @param output The output object.
     */
    public static void initTable(MDAFrame frame, Output output)
    {
        // Generate table
        StringBuffer buffer = new StringBuffer();
        int nBlock = output.nonparamInOmega.length;
         int nInd = output.nonparamInTheta.length;
        int nTheta = output.nonparamInTheta[0].length;
        buffer.append(formatString("Point"));
                for(int i = 0; i < nTheta; i++)
            buffer.append(formatString("THETA" + (i + 1)));
        for(int i = 0; i < nBlock; i++)
        {
            if(output.omegaStruct[i].equals("diagonal"))
            {
                for(int j = 0; j < output.nonparamMeanOmega[i][0].length; j++)
                    if(nBlock == 1)
                        buffer.append(formatString("OMEGA" + (j + 1) + "," + (j + 1)));
                    else
                        buffer.append(formatString("OMEGA[" + (i + 1) + "]" + (j + 1) + "," + (j + 1)));
            }
            else
            {
                int k = 1;
                int l = 1;
                for(int j = 0; j < output.nonparamMeanOmega[i][0].length; j++)
                {
                    if(nBlock == 1)
                        buffer.append(formatString("OMEGA" + k + "," + l));
                    else
                        buffer.append(formatString("OMEGA[" + (i + 1) + "]" + k + "," + l));
                    if(l < k) 
                        l++;
                    else
                    {
                        k++;
                        l = 1;
                    }
                }
            }
        }
        for(int i = 0; i < nInd; i++)
        {
            buffer.append("\n");
            buffer.append(formatString(String.valueOf(i + 1)));
            for(int j = 0; j < nTheta; j++)
            {
                buffer.append(" ");
                buffer.append(formatNumeric(output.nonparamInTheta[i][j]));
            }
            for(int j = 0; j < nBlock; j++)
                for(int k = 0; k < output.nonparamInOmega[j][i].length; k++)
                {
                    buffer.append(" ");
                    buffer.append(formatNumeric(output.nonparamInOmega[j][i][k]));
                }
        }
        frame.setEditorText(buffer.toString());
        frame.setEditorCaretPosition(0);
        frame.setEditorTitle("Initial Parameters");
    }
    
    /** Display posterior mean of parameters in a table.
     * @param frame the MDA main frame window.
     * @param output The output object.
     */
    public static void meanTable(MDAFrame frame, Output output)
    {
        StringBuffer buffer = new StringBuffer();
        int nBlock = output.nonparamMeanOmega.length;
        int nInd = output.nonparamMeanTheta.length;
        int nTheta = output.nonparamMeanTheta[0].length;
        buffer.append(formatString("ID"));
        for(int i = 0; i < nTheta; i++)
            buffer.append(formatString("THETA" + (i + 1)));
        for(int i = 0; i < nBlock; i++)
        {
            if(output.omegaStruct[i].equals("diagonal"))
            {
                for(int j = 0; j < output.nonparamMeanOmega[i][0].length; j++)
                    if(nBlock == 1)
                        buffer.append(formatString("OMEGA" + (j + 1) + "," + (j + 1)));
                    else
                        buffer.append(formatString("OMEGA[" + (i + 1) + "]" + (j + 1) + "," + (j + 1)));
            }
            else
            {
                int k = 1;
                int l = 1;
                for(int j = 0; j < output.nonparamMeanOmega[i][0].length; j++)
                {
                    if(nBlock == 1)
                        buffer.append(formatString("OMEGA" + k + "," + l));
                    else
                        buffer.append(formatString("OMEGA[" + (i + 1) + "]" + k + "," + l));
                    if(l < k) 
                        l++;
                    else
                    {
                        k++;
                        l = 1;
                    }
                }
            }
        }
        for(int i = 0; i < nInd; i++)
        {
            buffer.append("\n");
            buffer.append(formatString(String.valueOf(i + 1)));
            for(int j = 0; j < nTheta; j++)
            {
                buffer.append(" ");
                buffer.append(formatNumeric(output.nonparamMeanTheta[i][j]));
            }
            for(int j = 0; j < nBlock; j++)
                for(int k = 0; k < output.nonparamMeanOmega[j][i].length; k++)
                {
                    buffer.append(" ");
                    buffer.append(formatNumeric(output.nonparamMeanOmega[j][i][k]));
                }
        }
        frame.setEditorText(buffer.toString());
        frame.setEditorCaretPosition(0);
        frame.setEditorTitle("Posterior Mean of Parameters");
    }
    
    // Format the data of type string
    private static String formatString(String number)
    {
        StringBuffer stringBuffer = new StringBuffer();
        for(int i = 0; i < 12 - number.length(); i++)
            stringBuffer.append(" ");
        return stringBuffer.append(number).toString();
    }
    
    // Format the data of type numeric 
    private static String formatNumeric(String number)
    {
        if(number.equals("."))
            number += "0";
        DecimalFormat f = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
        f.applyPattern("0.0000E00");
        return Utility.formatData(8, f.format(Double.parseDouble(number)));  
    }
    
    /** Closes the dialog */
    private void closeDialog(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_closeDialog
        setVisible(false);
        dispose();
    }//GEN-LAST:event_closeDialog
   
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton displayButton;
    private javax.swing.JButton jButton1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JList plotList;
    // End of variables declaration//GEN-END:variables
    private Output output;
    private DefaultListModel model;
    private double minY;
    private double maxY;
    private int nVDivi;
    private String[] name = null;
    private int[] symbol = {14};
    private Color[] color = {Color.black};
    private ArrayList<double[]> parameterList;
}
