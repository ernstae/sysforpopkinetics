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
import uw.rfpk.mda.nonmem.Utility;
import java.awt.Component;
import java.awt.geom.*;
import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;
import java.util.Properties;
import java.util.Comparator;
import java.util.Arrays;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.DefaultListModel;
import java.text.DecimalFormat;
import java.text.NumberFormat;

/** This class processes scatterplot information and displays scatterplots.
 *
 * @author  Jiaji Du
 */
public class PlotShow extends JFrame {
    
    /** Creates new form PlotShow.
     * @param plotAll a String[][][] containing information for all the scatterplots.
     * @param dataAll a double[][] containing all pesentation data.
     * @param labelAll an Arraylist object containing the data labels.
     * @param dataLabelMap a Properties object containing the data label-alias mapping.
     */
    public PlotShow(String[][][] plotAll, double[][] dataAll, ArrayList labelAll,
                    Properties dataLabelMap)
    {
        this.plotAll = plotAll;
        this.dataLabelMap = dataLabelMap;
        
        // Display the window
        setSize(500, 500);
        initComponents();
        setVisible(true);
        
        // Replace the label name by the alias if exist
        ArrayList<String> aliasAll = new ArrayList<String>();
        for(int i = 0; i < labelAll.size(); i++)
        {
            String alias = dataLabelMap.getProperty((String)labelAll.get(i));
            if(alias == null)
                alias = (String)labelAll.get(i);  
            aliasAll.add(i, alias);
        } 

        // Process the data then show scatterplot list
        DefaultListModel model = new DefaultListModel();
        jList1.setModel(model);
        dataList = new double[plotAll.length][][];
        DecimalFormat f = (DecimalFormat)NumberFormat.getInstance(java.util.Locale.ENGLISH);
        f.applyPattern("0.00E00");
        for(int i = 0; i < plotAll.length; i++)
        {
            // Get data for the plots from the presentation data            
            String[][] plotI = plotAll[i];
            int l1 = plotI[1].length;
            int l2 = plotI[2].length;
            int l3 = 0;
            if(plotI[3] != null)
                l3 = plotI[3].length;
            int from = Integer.parseInt(plotI[0][0]) - 1;
            int to = Integer.parseInt(plotI[0][1]) - 1; 
            if(to > dataAll.length - 1)
                to = dataAll.length - 1;
            int length = to - from + 1;
            double[][] data = new double[length][l1 + l2 + l3];
            ArrayList<String> alias = new ArrayList<String>();
            for(int j = 0; j < l3; j++)
            {
                int index = aliasAll.indexOf(plotI[3][j]); 
                for(int k = 0; k < length; k++)
                    data[k][j] = dataAll[k + from][index];
                alias.add(plotI[3][j]);
            }            
            for(int j = 0; j < l1; j++)
            {
                int index = aliasAll.indexOf(plotI[1][j]); 
                for(int k = 0; k < length; k++)
                    data[k][j + l3] = dataAll[k + from][index];
                alias.add(plotI[1][j]);                
            }
            for(int j = 0; j < l2; j++)
            {
                int index = aliasAll.indexOf(plotI[2][j]); 
                for(int k = 0; k < length; k++)
                    data[k][j + l1 + l3] = dataAll[k + from][index];
                alias.add(plotI[2][j]);                
            }
     
            // Sort the data if splitting is required
            ArrayList<Portion> split = new ArrayList<Portion>();
            if(l3 != 0)
            {
                Arrays.sort(data, new CompareRows(l3));
                Portion portion = new Portion();
                portion.index1 = 0; 
                portion.value1 = data[0][0];
                if(l3 > 1)
                    portion.value2 = data[0][1];
                split.add(portion); 
                for(int j = 1; j < dataAll.length; j++)
                {
                    if(l3 == 1)
                    {
                        if(data[j][0] != data[j - 1][0])
                        {
                            portion.index2 = j;
                            portion = new Portion();
                            portion.index1 = j; 
                            portion.value1 = data[j][0];
                            split.add(portion);
                        }
                    }
                    else
                    {
                        if(data[j][0] != data[j - 1][0] || data[j][1] != data[j - 1][1])
                        {
                            portion.index2 = j;
                            portion = new Portion();
                            portion.index1 = j; 
                            portion.value1 = data[j][0];
                            portion.value2 = data[j][1];
                            split.add(portion);
                        }                        
                    }
                }
                portion.index2 = dataAll.length;
            }
            splitList.add(split);
            dataList[i] = data;
            aliasList.add(alias);
            
            // Fill the list
            String element = "Scatterplot " + (i + 1) + "_";
            int n = 0;
            for(int j = 0; j < l1; j++)
            {
                for(int k = 0; k < l2; k++)
                {
                    String element1 = plotI[1][j] + " versus " + plotI[2][k];
                    n++;
                    if(l3 != 0)
                    {
                        for(int m = 0; m < split.size(); m++)
                        {
                            String element2 = element1 + " " + plotI[3][0] + "=" + 
                                              Utility.formatData(6, f.format(((Portion)split.get(m)).value1)).trim();
                            if(l3 > 1)
                            {
                                element2 += " " + plotI[3][1] + "=" + 
                                            Utility.formatData(6, f.format(((Portion)split.get(m)).value2)).trim();
                                model.addElement(element + n + "_" + (m + 1) + ": " + element2);
                            }
                            else
                                model.addElement(element + n + "_" + (m + 1) + ": " + element2); 
                        }
                    }
                    else
                        model.addElement(element + n + ": " + element1);
                }
            }            
        }
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents
        jScrollPane1 = new javax.swing.JScrollPane();
        jList1 = new javax.swing.JList();
        jButton1 = new javax.swing.JButton();
        jTextPane1 = new javax.swing.JTextPane();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Scatterplot List");
        setLocationRelativeTo(this);
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        jScrollPane1.setPreferredSize(new java.awt.Dimension(400, 200));
        jList1.setFixedCellHeight(15);
        jScrollPane1.setViewportView(jList1);

        getContentPane().add(jScrollPane1, java.awt.BorderLayout.CENTER);

        jButton1.setText("DISPLAY");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        getContentPane().add(jButton1, java.awt.BorderLayout.SOUTH);

        jTextPane1.setEditable(false);
        jTextPane1.setText("Select a scatterplot or scatterplots to display");
        jTextPane1.setFocusable(false);
        getContentPane().add(jTextPane1, java.awt.BorderLayout.NORTH);

        pack();
    }//GEN-END:initComponents

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        int[] selectedIndex = jList1.getSelectedIndices();
        if(selectedIndex.length == 1 && selectedIndex[0] < 0)
            return;
        
        // Get data and other information for the scatterplot record
        Object[] elements = jList1.getSelectedValues();
        for(int j = 0; j < elements.length; j++)
        {
            String[] tokens = ((String)elements[j]).split(" ");
            int p = Integer.parseInt(tokens[1].split("_")[0]) - 1;
            double[][] data = dataList[p]; 
            String[][] plot = plotAll[p];
            ArrayList alias = (ArrayList)aliasList.get(p);
            ArrayList split = (ArrayList)splitList.get(p);
        
            // Put data in double arrays, only one curve in the plot
            String title = "";
            int columnX = alias.indexOf(tokens[4]);
            int columnY = alias.indexOf(tokens[2]);
            double[] dataXAll, dataYAll; 
            
            if(plot[3] == null)
            {
                dataXAll = new double[data.length];
                dataYAll = new double[data.length];
                for(int i = 0; i < data.length; i++)
                {
                    dataXAll[i] = data[i][columnX];
                    dataYAll[i] = data[i][columnY];
                }
            }
            else
            {
                String s = tokens[1].split("_")[2];
                int n = Integer.parseInt(s.substring(0, s.length() - 1)) - 1;
                Portion portion = (Portion)split.get(n); 
                int length = portion.index2 - portion.index1;
                dataXAll = new double[length];
                dataYAll = new double[length];
                for(int i = 0; i < length; i++) 
                {
                    dataXAll[i] = data[portion.index1 + i][columnX];
                    dataYAll[i] = data[portion.index1 + i][columnY];                
                }      
                title = "This plot is for " + tokens[5];
                if(plot[3].length > 1)
                    title += " " + tokens[6];
            }

            // Check for missing data values
            double[][] dataOut = Utility.removeMissingValue(dataXAll, dataYAll, 0, dataXAll.length - 1, false, false);
            if(dataOut == null)
            {
                JOptionPane.showMessageDialog(null, "Data missing for the plot.");
                return;
            }
            double[][] dataX = new double[1][];    
            double[][] dataY = new double[1][];
            dataX[0] = dataOut[0];
            dataY[0] = dataOut[1];
            
            // Display the plot
            JFrame frame = new JFrame(); 
            Plotter plotter = new Plotter(dataX, dataY, title, tokens[4], tokens[2],
                                          new String[]{tokens[2]},
                                          new int[]{0},
                                          new Color[]{Color.red, Color.green, Color.green, Color.green},
                                          plot[0][2].equals("show"),
                                          plot[0][3].equals("show"),
                                          plot[0][4].equals("show"),
                                          false, false, true, true, 0, 0, null, null,
                                          new Color[]{Color.green, Color.green, Color.green, null, null}, 
                                          null,
                                          5, 5, 6, 6, 4, 4, 4, 4, 0, 0, 0, 0,
                                          new Font("SansSerif", Font.BOLD, 14),
                                          new Font("SansSerif", Font.BOLD, 12),
                                          new Font("SansSerif", Font.BOLD, 11),
                                          new Font("SansSerif", Font.BOLD, 10),
                                          0, 0, 0, 0, true, true, false, false,
                                          false, 0, false, null, 2, 2, frame, null);                                          
            plotter.setToolTipText("");
            frame.getContentPane().add(plotter);
            frame.setLocation(50 * j, 40 * j);
            frame.setSize(500, 400);
            frame.setTitle("Model Design Agent Scaterplot Display");	
            frame.setVisible(true);
        }
    }//GEN-LAST:event_jButton1ActionPerformed

    // Class to implements Comparator interface
    private class CompareRows implements Comparator<Object>
    {
        // Constructor
        public CompareRows(int nCompare)
        {
            this.nCompare = nCompare;
        }
        
        // Compare the two rows
        public int compare(Object obj, Object obj1) {
            double[] d1 = (double[])obj;
            double[] d2 = (double[])obj1;
            for(int i = 0; i < nCompare; i++) 
            {
                if(d1[i] < d2[i])
                    return -1;
                if(d1[i] > d2[i])
                    return 1;
            } 
            return 0;
        } 
        
        // Number of the first columns to compare
        private int nCompare;
    }
    
    // Portion of the data after splitting
    private class Portion
    {
         public int index1;       // beginning index included
         public int index2;       // ending index excluded
         public double value1;    // value of the first splitting item
         public double value2;    // value of the second splitting item
    }
    
    /** Exit the Application */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
//        System.exit(0);
    }//GEN-LAST:event_exitForm
 
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JList jList1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextPane jTextPane1;
    // End of variables declaration//GEN-END:variables

    private String[][][] plotAll = null;
    private double[][][] dataList = null; 
    private ArrayList<ArrayList> aliasList = new ArrayList<ArrayList>();  
    private ArrayList<ArrayList> splitList = new ArrayList<ArrayList>();
    private Properties dataLabelMap = null;
}
